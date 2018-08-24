MODULE sdf_io

  USE sdf
  USE sdf_job_info

  implicit none

  private
  public :: load_sdf, save_sdf

  INTEGER, PARAMETER :: num = KIND(1.D0)
  INTEGER, PARAMETER :: num_sz = 8
  INTEGER, PARAMETER :: c_max_string_length = 128

  INTEGER, PARAMETER :: data_dir_max_length = 64
  CHARACTER(LEN = data_dir_max_length) :: data_dir

  INTEGER, PARAMETER :: n_zeros = 4
  INTEGER, PARAMETER :: c_ndims = 3

  !! Header vars
  INTEGER :: step, code_io_version, string_len
  REAL(num) :: time
  CHARACTER(LEN=c_id_length) :: c_code_name
  LOGICAL :: restart_flag
  TYPE(jobid_type) :: jobid

CONTAINS
  SUBROUTINE load_sdf(filename)

    !CHARACTER(LEN=c_id_length) :: block_id, mesh_id, str1
    !CHARACTER(LEN=c_max_string_length) :: name
    !CHARACTER(LEN=22) :: filename_fmt
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=6+data_dir_max_length+n_zeros+c_id_length) :: full_filename
    !INTEGER :: blocktype, datatype
    !INTEGER :: ierr, iblock, nblocks, ndims, geometry
    !INTEGER, DIMENSION(4) :: dims
    !INTEGER, DIMENSION(c_ndims) :: global_dims
    INTEGER :: comm = 0
    !REAL(num), DIMENSION(2*c_ndims) :: extents

    TYPE(sdf_file_handle) :: sdf_handle

    full_filename = TRIM(filename)

    PRINT*,'Attempting to read from file: ', full_filename

    CALL sdf_open(sdf_handle, full_filename, comm, c_sdf_read)

    CALL sdf_read_header(sdf_handle, step, time, c_code_name, code_io_version, &
        string_len, restart_flag)

    !nblocks = sdf_read_nblocks(sdf_handle)
    jobid = sdf_read_jobid(sdf_handle)

    PRINT*, 'step', step
    PRINT*, 'time', time
    PRINT*, 'c_code_name', c_code_name
    PRINT*, 'code_io_version', code_io_version
    PRINT*, 'string_len', string_len
    PRINT*, 'restart_flag', restart_flag

    !PRINT*, 'Input file contains', nblocks, 'blocks'

    !CALL sdf_read_blocklist(sdf_handle)

    !CALL sdf_seek_start(sdf_handle)

    !global_dims = (/ nx_global+1, ny_global+1, nz_global+1 /)

    !DO iblock = 1, nblocks
      !CALL sdf_read_next_block_header(sdf_handle, block_id, name, blocktype, &
          !ndims, datatype)

      !SELECT CASE(blocktype)
      !CASE(c_blocktype_constant)
        !IF (str_cmp(block_id, 'dt')) THEN
          !CALL sdf_read_srl(sdf_handle, dt_from_restart)
        !ELSE IF (str_cmp(block_id, 'time_prev')) THEN
          !CALL sdf_read_srl(sdf_handle, time_prev)
        !ELSE IF (str_cmp(block_id, 'visc_heating')) THEN
          !CALL sdf_read_srl(sdf_handle, total_visc_heating)
          !IF (rank /= 0) total_visc_heating = 0
        !END IF
      !CASE(c_blocktype_plain_mesh)
        !IF (ndims /= c_ndims .OR. datatype /= sdf_num &
            !.OR. .NOT.str_cmp(block_id, 'grid')) CYCLE

        !CALL sdf_read_plain_mesh_info(sdf_handle, geometry, dims, extents)

        !IF (geometry /= c_geometry_cartesian &
            !.OR. ALL(dims(1:c_ndims) /= global_dims(1:c_ndims))) CYCLE

        !! Should read the grid from file at this point?
        !x_min = extents(1)
        !x_max = extents(c_ndims+1)
        !y_min = extents(2)
        !y_max = extents(c_ndims+2)
        !z_min = extents(3)
        !z_max = extents(c_ndims+3)

      !CASE(c_blocktype_plain_variable)
        !IF (ndims /= c_ndims .OR. datatype /= sdf_num) CYCLE

        !CALL sdf_read_plain_variable_info(sdf_handle, dims, str1, mesh_id)

        !IF (.NOT.str_cmp(mesh_id, 'grid')) CYCLE

        !IF (str_cmp(block_id, 'Rho')) THEN
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, rho, &
              !cell_distribution, cell_subarray)

        !ELSE IF (str_cmp(block_id, 'Energy')) THEN
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, energy, &
              !cell_distribution, cell_subarray)

        !ELSE IF (str_cmp(block_id, 'Vx')) THEN
          !dims = dims - 1
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, vx, &
              !node_distribution, node_subarray)

        !ELSE IF (str_cmp(block_id, 'Vy')) THEN
          !dims = dims - 1
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, vy, &
              !node_distribution, node_subarray)

        !ELSE IF (str_cmp(block_id, 'Vz')) THEN
          !dims = dims - 1
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, vz, &
              !node_distribution, node_subarray)

        !ELSE IF (str_cmp(block_id, 'Bx')) THEN
          !dims(1) = dims(1) - 1
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, bx, &
              !bx_distribution, bx_subarray)

        !ELSE IF (str_cmp(block_id, 'By')) THEN
          !IF (c_ndims >= 2) dims(2) = dims(2) - 1
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, by, &
              !by_distribution, by_subarray)

        !ELSE IF (str_cmp(block_id, 'Bz')) THEN
          !IF (c_ndims >= 3) dims(3) = dims(3) - 1
          !CALL check_dims(dims)
          !CALL sdf_read_plain_variable(sdf_handle, bz, &
              !bz_distribution, bz_subarray)

        !END IF

      !END SELECT
    !END DO

    CALL sdf_close(sdf_handle)
  END SUBROUTINE load_sdf

  SUBROUTINE save_sdf(filename)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=6+data_dir_max_length+n_zeros+c_id_length) :: full_filename
    INTEGER :: comm = 0
    TYPE(sdf_file_handle) :: sdf_handle

    full_filename = TRIM(filename)

    CALL sdf_open(sdf_handle, full_filename, comm, c_sdf_write)
    CALL sdf_set_string_length(sdf_handle, c_max_string_length)
    CALL sdf_write_header(sdf_handle, TRIM(c_code_name), 1, step, time, &
        restart_flag, jobid)
    !CALL sdf_write_run_info(sdf_handle, c_version, c_revision, c_minor_rev, &
        !c_commit_id, '', c_compile_machine, c_compile_flags, 0_8, &
        !c_compile_date, run_date)
    !CALL sdf_write_cpu_split(sdf_handle, 'cpu_rank', 'CPUs/Original rank', &
        !cell_nx_maxs, cell_ny_maxs, cell_nz_maxs)
    !CALL sdf_write_srl(sdf_handle, 'dt', 'Time increment', dt)
    !CALL sdf_write_srl(sdf_handle, 'time_prev', 'Last dump time requested', &
        !time_prev)
    !CALL sdf_write_srl(sdf_handle, 'visc_heating', 'Viscous heating total', &
        !visc_heating)

    !CALL sdf_write_srl_plain_mesh(sdf_handle, 'grid', 'Grid/Grid', &
        !xb_global, yb_global, zb_global, convert)
  END SUBROUTINE save_sdf
END MODULE sdf_io
