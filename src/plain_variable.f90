module plain_variable

  use sdf
  USE shared_data

  use shared_data

  implicit none

  private
  public :: PlainVariable, load_var, save_var

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

    ! Actually read in the data
    ALLOCATE(this%var_data(-1:this%dims(1)+2, -1:this%dims(2)+2, -1:this%dims(3)+2))
    CALL sdf_read_plain_variable(sdf_handle, this%var_data, this%distribution, this%subarray)
  end subroutine load_var

  subroutine save_var(this, sdf_handle)
    type(PlainVariable), intent(in) :: this
    TYPE(sdf_file_handle), intent(in) :: sdf_handle

    CALL sdf_write_plain_variable(sdf_handle, TRIM(this%block_id), &
        TRIM(this%varname), TRIM(this%units), this%dims, &
        this%stagger, 'grid', this%var_data, &
        this%distribution, this%subarray)
  end subroutine save_var
end module plain_variable
