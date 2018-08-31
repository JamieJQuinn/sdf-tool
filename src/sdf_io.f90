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
  public :: load_sdf, save_sdf

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
  INTEGER, DIMENSION(c_ndims) :: global_dims
  REAL(num), DIMENSION(2*c_ndims) :: extents
  REAL(num), DIMENSION(:), ALLOCATABLE :: xb_global, yb_global, zb_global

  !! Simulation Variables
  integer :: nsimvars
  type(PlainVariable), dimension(:), allocatable :: variables

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

  SUBROUTINE load_sdf(filename)

    CHARACTER(LEN=c_id_length) :: block_id
    CHARACTER(LEN=c_id_length) :: mesh_id, str1
    CHARACTER(LEN=c_max_string_length) :: name
    !CHARACTER(LEN=22) :: filename_fmt
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=6+data_dir_max_length+n_zeros+c_id_length) :: full_filename
    INTEGER :: blocktype, datatype
    INTEGER :: ierr, iblock, nblocks, ndims
    INTEGER :: isimvar = 1
    INTEGER :: comm = 0

    TYPE(sdf_file_handle) :: sdf_handle

    full_filename = TRIM(filename)

    PRINT*,'Attempting to read from file: ', full_filename

    CALL sdf_open(sdf_handle, full_filename, comm, c_sdf_read)

    CALL sdf_read_header(sdf_handle, step, time, c_code_name, code_io_version, &
        string_len, restart_flag)

    nblocks = sdf_read_nblocks(sdf_handle)
    jobid = sdf_read_jobid(sdf_handle)

    PRINT*, 'READING HEADER'
    PRINT*, 'step', step
    PRINT*, 'time', time
    PRINT*, 'c_code_name', c_code_name
    PRINT*, 'code_io_version', code_io_version
    PRINT*, 'string_len', string_len
    PRINT*, 'restart_flag', restart_flag
    PRINT*, 'nblocks', nblocks
    PRINT*, 'jobid', jobid
    PRINT*, 'DONE READING HEADER'

    CALL sdf_read_blocklist(sdf_handle)
    CALL sdf_seek_start(sdf_handle)

    nsimvars = nblocks - 6
    allocate(variables(nsimvars))

    DO iblock = 1, nblocks
      CALL sdf_read_next_block_header(sdf_handle, block_id, name, blocktype, &
          ndims, datatype)
      SELECT CASE(blocktype)
      CASE(c_blocktype_run_info)
        CALL sdf_read_run_info(sdf_handle, c_version, c_revision, &
          c_minor_rev, c_commit_id, sha1sum, c_compile_machine, &
          c_compile_flags, defines, c_compile_date, run_date, io_date)
      CASE(c_blocktype_cpu_split)
        CALL sdf_read_cpu_split_info(sdf_handle, cpu_dims, cpu_geometry)
        ALLOCATE(cell_nx_maxs(1:cpu_dims(1)+1))
        ALLOCATE(cell_ny_maxs(1:cpu_dims(2)+1))
        ALLOCATE(cell_nz_maxs(1:cpu_dims(3)+1))
        CALL sdf_read_srl_cpu_split(sdf_handle, &
          cell_nx_maxs, cell_ny_maxs, cell_nz_maxs)

      CASE(c_blocktype_constant)
        IF (str_cmp(block_id, 'dt')) THEN
          CALL sdf_read_srl(sdf_handle, dt_from_restart)
        ELSE IF (str_cmp(block_id, 'time_prev')) THEN
          CALL sdf_read_srl(sdf_handle, time_prev)
        ELSE IF (str_cmp(block_id, 'visc_heating')) THEN
          CALL sdf_read_srl(sdf_handle, total_visc_heating)
        END IF

      CASE(c_blocktype_plain_mesh)
        IF (ndims /= c_ndims .OR. datatype /= sdf_num &
            .OR. .NOT.str_cmp(block_id, 'grid')) CYCLE

        CALL sdf_read_plain_mesh_info(sdf_handle, geometry, dims, extents)

        nx_global = dims(1) - 1
        ny_global = dims(2) - 1
        nz_global = dims(3) - 1

        PRINT*, 'READING GRID'
        PRINT*, 'dims', dims
        PRINT*, 'extents', extents

        CALL mpi_create_types(nx_global, ny_global, nz_global)

        ALLOCATE(xb_global(1:dims(1)))
        ALLOCATE(yb_global(1:dims(2)))
        ALLOCATE(zb_global(1:dims(3)))

        CALL sdf_read_srl_plain_mesh(sdf_handle, xb_global, yb_global, zb_global)

        PRINT*, 'DONE READING GRID'

      CASE(c_blocktype_plain_variable)
        IF (ndims /= c_ndims .OR. datatype /= sdf_num) CYCLE

        CALL sdf_read_plain_variable_info(sdf_handle, dims, str1, mesh_id)

        IF (.NOT.str_cmp(mesh_id, 'grid')) CYCLE

        call load_var(variables(isimvar), sdf_handle)
        isimvar = isimvar + 1

      END SELECT
    END DO

    CALL sdf_close(sdf_handle)
  END SUBROUTINE load_sdf

  SUBROUTINE save_sdf(filename)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=6+data_dir_max_length+n_zeros+c_id_length) :: full_filename
    CHARACTER(LEN=c_id_length) :: varname, units
    INTEGER, DIMENSION(c_ndims) :: global_dims, dims
    INTEGER :: comm = 0
    INTEGER :: isimvar = 1
    LOGICAL :: convert = .FALSE.
    TYPE(sdf_file_handle) :: sdf_handle
    TYPE(sdf_block_type), POINTER :: b

    full_filename = TRIM(filename)

    CALL sdf_open(sdf_handle, full_filename, comm, c_sdf_write)
    CALL sdf_set_string_length(sdf_handle, c_max_string_length)
    CALL sdf_write_header(sdf_handle, TRIM(c_code_name), 1, step, time, &
        restart_flag, jobid)
    CALL sdf_write_run_info(sdf_handle, c_version, c_revision, c_minor_rev, &
        c_commit_id, '', c_compile_machine, c_compile_flags, 0_8, &
        c_compile_date, run_date)
    ! fix io date
    b => sdf_handle%current_block
    b%run%io_date = io_date
    CALL write_run_info_meta(sdf_handle, 'run_info', 'Run_info')

    CALL sdf_write_cpu_split(sdf_handle, 'cpu_rank', 'CPUs/Original rank', &
        cell_nx_maxs, cell_ny_maxs, cell_nz_maxs)
    CALL sdf_write_srl(sdf_handle, 'dt', 'Time increment', dt_from_restart)
    CALL sdf_write_srl(sdf_handle, 'time_prev', 'Last dump time requested', &
        time_prev)
    CALL sdf_write_srl(sdf_handle, 'visc_heating', 'Viscous heating total', &
        total_visc_heating)

    CALL sdf_write_srl_plain_mesh(sdf_handle, 'grid', 'Grid/Grid', &
        xb_global, yb_global, zb_global, convert)

    do isimvar = 1, nsimvars
      call save_var(variables(isimvar), sdf_handle)
    end do

    CALL sdf_close(sdf_handle)
  END SUBROUTINE save_sdf
END MODULE sdf_io
