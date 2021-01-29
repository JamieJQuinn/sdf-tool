module plain_variable

  use sdf
  USE shared_data

  use shared_data

  implicit none

  private
  public :: PlainVariable, load_var, save_var, resize_var, reset_var

  type PlainVariable
    REAL(num), DIMENSION(:,:,:), ALLOCATABLE :: var_data
    INTEGER, DIMENSION(4) :: dims
    CHARACTER(LEN=c_id_length) :: varname, units, mesh_id, block_id
    INTEGER :: stagger, distribution, subarray
  end type PlainVariable

contains
  subroutine load_var(this, sdf_handle)
    type(PlainVariable), intent(inout) :: this
    TYPE(sdf_file_handle), intent(in) :: sdf_handle

    ! Grab variable info from header & block
    CALL sdf_read_block_header(sdf_handle, this%block_id, this%varname)
    CALL sdf_read_plain_variable_info(sdf_handle, this%dims, this%units, this%mesh_id, this%stagger)
    CALL set_subarray(this)

    ! Actually read in the data
    ALLOCATE(this%var_data(-1:this%dims(1)+2, -1:this%dims(2)+2, -1:this%dims(3)+2))
    CALL sdf_read_plain_variable(sdf_handle, this%var_data, this%distribution, this%subarray)
  end subroutine load_var

  subroutine reset_var(this)
    type(PlainVariable), intent(inout) :: this
    deallocate(this%var_data)
  end subroutine

  subroutine set_subarray(this)
    type(PlainVariable), intent(inout) :: this

    ! Calculate distribution and subarray from stagger value
    select case(this%stagger)
    case(c_stagger_cell_centre)
      this%distribution = cell_distribution
      this%subarray = cell_subarray
    case(c_stagger_vertex)
      this%distribution = node_distribution
      this%subarray = node_subarray
    case(c_stagger_bx)
      this%distribution = bx_distribution
      this%subarray = bx_subarray
    case(c_stagger_by)
      this%distribution = by_distribution
      this%subarray = by_subarray
    case(c_stagger_bz)
      this%distribution = bz_distribution
      this%subarray = bz_subarray
    end select
  end subroutine set_subarray

  subroutine resize_var(this, slices)
    type(PlainVariable), intent(inout) :: this
    ! Assume indices are indexing centred quantities
    INTEGER, dimension(6), intent(in) :: slices
    INTEGER :: ix_min, ix_max, iy_min, iy_max, iz_min, iz_max
    INTEGER :: nx, ny, nz
    REAL(num), DIMENSION(:,:,:), ALLOCATABLE :: new_var_data

    integer :: ix, iy, iz

    ix_min = slices(1)
    ix_max = slices(2)
    iy_min = slices(3)
    iy_max = slices(4)
    iz_min = slices(5)
    iz_max = slices(6)

    ! Fix indices to include vertex variables
    select case(this%stagger)
    case(c_stagger_cell_centre)
      ! Do nothing, indices correct
    case(c_stagger_vertex)
      ix_max = ix_max + 1
      iy_max = iy_max + 1
      iz_max = iz_max + 1
    case(c_stagger_bx)
      ix_max = ix_max + 1
    case(c_stagger_by)
      iy_max = iy_max + 1
    case(c_stagger_bz)
      iz_max = iz_max + 1
    end select

    nx = ix_max - ix_min + 1
    ny = iy_max - iy_min + 1
    nz = iz_max - iz_min + 1

    allocate(new_var_data(-1:nx+2, -1:ny+2, -1:nz+2))
    do iz = 1, nz
      do iy = 1, ny
        do ix = 1, nx
          new_var_data(ix,iy,iz) = this%var_data(ix_min + ix - 1,iy_min + iy - 1, iz_min + iz - 1)
        enddo
      enddo
    enddo

    !do iz = 1, nz
      !new_var_data(:,:,iz) = iz
    !enddo

    call move_alloc(new_var_data, this%var_data)
    !deallocate(this%var_data)
    !allocate(this%var_data(-1:nx+2, -1:ny+2, -1:nz+2))
    !this%var_data(:,:,:) = new_var_data(:,:,:)
    !deallocate(new_var_data)

    ! reset MPI subarrays
    call set_subarray(this)

    this%dims(1) = nx
    this%dims(2) = ny
    this%dims(3) = nz
  end subroutine resize_var

  subroutine save_var(this, sdf_handle)
    type(PlainVariable), intent(in) :: this
    TYPE(sdf_file_handle), intent(in) :: sdf_handle

    CALL sdf_write_plain_variable(sdf_handle, TRIM(this%block_id), &
        TRIM(this%varname), TRIM(this%units), this%dims, &
        this%stagger, 'grid', this%var_data, &
        this%distribution, this%subarray)
  end subroutine save_var
end module plain_variable
