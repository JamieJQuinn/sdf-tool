MODULE sdf_io

  USE sdf
  USE sdf_job_info
  USE sdf_input_ru
  USE sdf_output_ru

  USE shared_data
  USE mpi_routines
  use plain_variable

  implicit none

  private
  public :: process_sdf

  !! Header vars
  INTEGER :: step, code_io_version, string_len
  REAL(num) :: time
  CHARACTER(LEN=c_id_length) :: c_code_name
  LOGICAL :: restart_flag
  TYPE(jobid_type) :: jobid

  !! Run Information
  INTEGER(i4) :: c_version, c_revision, c_minor_rev
  CHARACTER(LEN=c_max_string_length) :: c_commit_id, sha1sum
  CHARACTER(LEN=c_max_string_length) :: c_compile_machine, c_compile_flags
  INTEGER(i8) :: defines
  INTEGER(i4) :: c_compile_date, run_date, io_date

  !! CPU Info
  INTEGER, DIMENSION(:), ALLOCATABLE :: cell_nx_maxs, cell_ny_maxs, cell_nz_maxs
  INTEGER, DIMENSION(c_ndims) :: cpu_dims
  INTEGER :: cpu_geometry

  !! Constants Blocks
  REAL(num) :: dt_from_restart, time_prev, total_visc_heating

  !! Grid
  INTEGER :: geometry
  INTEGER :: nx_global, ny_global, nz_global
  INTEGER, DIMENSION(4) :: dims
  REAL(num), DIMENSION(2*c_ndims) :: extents
  REAL(num), DIMENSION(:), ALLOCATABLE :: xb_global, yb_global, zb_global

  type(PlainVariable) :: variable

CONTAINS
  !! This function taken directly from Lare3d
  FUNCTION str_cmp(str_in, str_test)

    CHARACTER(*), INTENT(IN) :: str_in, str_test
    CHARACTER(30) :: str_trim
    LOGICAL :: str_cmp
    INTEGER :: l

    str_trim = TRIM(ADJUSTL(str_in))
    l = LEN(str_test)

    IF (l > LEN(str_in)) THEN
      str_cmp = .FALSE.
      RETURN
    END IF

    IF (str_trim(l+1:l+1) /= ' ') THEN
      str_cmp = .FALSE.
      RETURN
    END IF

    str_cmp = str_trim(1:l) == str_test

  END FUNCTION str_cmp

  subroutine process_sdf(in_filename, out_filename, output_variables, save_all, slices)

    CHARACTER(LEN=c_id_length) :: block_id
    CHARACTER(LEN=c_id_length) :: mesh_id, str1
    CHARACTER(LEN=c_max_string_length) :: name
    !CHARACTER(LEN=22) :: in_filename_fmt
    CHARACTER(LEN=*), INTENT(IN) :: in_filename
    CHARACTER(LEN=6+data_dir_max_length+n_zeros+c_id_length) :: full_in_filename
    INTEGER :: blocktype, datatype
    INTEGER :: iblock, nblocks, ndims
    INTEGER :: comm = 0
    TYPE(sdf_file_handle) :: in_sdf_handle

    CHARACTER(LEN=*), INTENT(IN) :: out_filename
    CHARACTER(LEN=6+data_dir_max_length+n_zeros+c_id_length) :: full_out_filename
    integer, dimension(6), intent(in) :: slices
    LOGICAL :: convert = .FALSE.
    TYPE(sdf_file_handle) :: out_sdf_handle
    TYPE(sdf_block_type), POINTER :: b
    character(c_id_length), INTENT(IN) :: output_variables(:)
    logical, intent(in) :: save_all
    integer :: nx, ny, nz

    full_in_filename = TRIM(in_filename)
    full_out_filename = TRIM(out_filename)

    !PRINT*,'Attempting to read from file: ', full_in_filename

    CALL sdf_open(in_sdf_handle, full_in_filename, comm, c_sdf_read)

    CALL sdf_read_header(in_sdf_handle, step, time, c_code_name, code_io_version, &
        string_len, restart_flag)

    nblocks = sdf_read_nblocks(in_sdf_handle)
    jobid = sdf_read_jobid(in_sdf_handle)

    !PRINT*, 'READING HEADER'
    !PRINT*, 'step', step
    !PRINT*, 'time', time
    !PRINT*, 'c_code_name', c_code_name
    !PRINT*, 'code_io_version', code_io_version
    !PRINT*, 'string_len', string_len
    !PRINT*, 'restart_flag', restart_flag
    !PRINT*, 'nblocks', nblocks
    !PRINT*, 'jobid', jobid
    !PRINT*, 'DONE READING HEADER'

    ! Open output sdf and print header info
    CALL sdf_open(out_sdf_handle, full_out_filename, comm, c_sdf_write)
    CALL sdf_set_string_length(out_sdf_handle, c_max_string_length)
    CALL sdf_write_header(out_sdf_handle, TRIM(c_code_name), 1, step, time, &
        restart_flag, jobid)

    ! Start reading in blocks
    CALL sdf_read_blocklist(in_sdf_handle)
    CALL sdf_seek_start(in_sdf_handle)

    ! Read in each block
    DO iblock = 1, nblocks
      CALL sdf_read_next_block_header(in_sdf_handle, block_id, name, blocktype, &
          ndims, datatype)
        !print *, block_id
      SELECT CASE(blocktype)
      CASE(c_blocktype_run_info)
        ! Read in
        CALL sdf_read_run_info(in_sdf_handle, c_version, c_revision, &
          c_minor_rev, c_commit_id, sha1sum, c_compile_machine, &
          c_compile_flags, defines, c_compile_date, run_date, io_date)

        ! Print out
        CALL sdf_write_run_info(out_sdf_handle, c_version, c_revision, c_minor_rev, &
            c_commit_id, '', c_compile_machine, c_compile_flags, 0_8, &
            c_compile_date, run_date)
        ! fix io date
        b => out_sdf_handle%current_block
        b%run%io_date = io_date
      CASE(c_blocktype_cpu_split)
        CALL sdf_read_cpu_split_info(in_sdf_handle, cpu_dims, cpu_geometry)
        ALLOCATE(cell_nx_maxs(1:cpu_dims(1)+1))
        ALLOCATE(cell_ny_maxs(1:cpu_dims(2)+1))
        ALLOCATE(cell_nz_maxs(1:cpu_dims(3)+1))
        CALL sdf_read_srl_cpu_split(in_sdf_handle, &
          cell_nx_maxs, cell_ny_maxs, cell_nz_maxs)

        CALL sdf_write_cpu_split(out_sdf_handle, 'cpu_rank', 'CPUs/Original rank', &
            cell_nx_maxs, cell_ny_maxs, cell_nz_maxs)

      CASE(c_blocktype_constant)
        IF (str_cmp(block_id, 'dt')) THEN
          CALL sdf_read_srl(in_sdf_handle, dt_from_restart)
          CALL sdf_write_srl(out_sdf_handle, 'dt', 'Time increment', dt_from_restart)
        ELSE IF (str_cmp(block_id, 'time_prev')) THEN
          CALL sdf_read_srl(in_sdf_handle, time_prev)
          CALL sdf_write_srl(out_sdf_handle, 'time_prev', 'Last dump time requested', &
              time_prev)
        ELSE IF (str_cmp(block_id, 'visc_heating')) THEN
          CALL sdf_read_srl(in_sdf_handle, total_visc_heating)
          CALL sdf_write_srl(out_sdf_handle, 'visc_heating', 'Viscous heating total', &
              total_visc_heating)
        END IF

      CASE(c_blocktype_plain_mesh)
        IF (ndims /= c_ndims .OR. datatype /= sdf_num &
            .OR. .NOT.str_cmp(block_id, 'grid')) CYCLE

        CALL sdf_read_plain_mesh_info(in_sdf_handle, geometry, dims, extents)

        nx_global = dims(1) - 1
        ny_global = dims(2) - 1
        nz_global = dims(3) - 1

        !PRINT*, 'READING GRID'
        !PRINT*, 'dims', dims
        !PRINT*, 'extents', extents

        CALL mpi_create_types(nx_global, ny_global, nz_global)

        ALLOCATE(xb_global(1:dims(1)))
        ALLOCATE(yb_global(1:dims(2)))
        ALLOCATE(zb_global(1:dims(3)))

        CALL sdf_read_srl_plain_mesh(in_sdf_handle, xb_global, yb_global, zb_global)

        !PRINT*, 'DONE READING GRID'

        if (slices(1) .ne. 0) then
          nx = slices(2) - slices(1) + 1
          ny = slices(4) - slices(3) + 1
          nz = slices(6) - slices(5) + 1
          CALL resize(slices)
        end if

        CALL sdf_write_srl_plain_mesh(out_sdf_handle, 'grid', 'Grid/Grid', &
            xb_global, yb_global, zb_global, convert)

      CASE(c_blocktype_plain_variable)
        IF (ndims /= c_ndims .OR. datatype /= sdf_num) CYCLE

        CALL sdf_read_plain_variable_info(in_sdf_handle, dims, str1, mesh_id)

        IF (.NOT.str_cmp(mesh_id, 'grid')) CYCLE

        if (any(output_variables==block_id) .or. save_all) then
          CALL mpi_create_types(nx_global, ny_global, nz_global)
          call load_var(variable, in_sdf_handle)
          if (slices(1) .ne. 0) then
            CALL mpi_create_types(nx, ny, nz)
            call resize_var(variable, slices)
          end if
          call save_var(variable, out_sdf_handle)
          call reset_var(variable)
        end if

      END SELECT
    END DO

    CALL sdf_close(in_sdf_handle)
    CALL sdf_close(out_sdf_handle)

  end subroutine

  subroutine resize(slices)
    integer, dimension(6), intent(in) :: slices
    INTEGER :: ix_min, ix_max, iy_min, iy_max, iz_min, iz_max
    INTEGER :: nx, ny, nz
    REAL(num), DIMENSION(:), ALLOCATABLE :: new_xb_global, new_yb_global, new_zb_global

    ix_min = slices(1)
    ix_max = slices(2)
    iy_min = slices(3)
    iy_max = slices(4)
    iz_min = slices(5)
    iz_max = slices(6)

    ! Resize MPI subarrays
    nx = ix_max - ix_min + 1
    ny = iy_max - iy_min + 1
    nz = iz_max - iz_min + 1
    CALL mpi_create_types(nx, ny, nz)

    print *, "Resizing to (", nx, ny, nz, ")"

    ! Resize grid
    ix_max = ix_max + 1
    iy_max = iy_max + 1
    iz_max = iz_max + 1

    nx = ix_max - ix_min + 1
    ny = iy_max - iy_min + 1
    nz = iz_max - iz_min + 1

    ALLOCATE(new_xb_global(1:nx))
    ALLOCATE(new_yb_global(1:ny))
    ALLOCATE(new_zb_global(1:nz))

    new_xb_global(:) = xb_global(ix_min:ix_max)
    new_yb_global(:) = yb_global(iy_min:iy_max)
    new_zb_global(:) = zb_global(iz_min:iz_max)

    deallocate(xb_global)
    deallocate(yb_global)
    deallocate(zb_global)

    ALLOCATE(xb_global(1:nx))
    ALLOCATE(yb_global(1:ny))
    ALLOCATE(zb_global(1:nz))

    xb_global(:) = new_xb_global(:)
    yb_global(:) = new_yb_global(:)
    zb_global(:) = new_zb_global(:)

    deallocate(new_xb_global)
    deallocate(new_yb_global)
    deallocate(new_zb_global)

    ! make sure we can't restart from this
    restart_flag = .FALSE.
  end subroutine resize
END MODULE sdf_io
