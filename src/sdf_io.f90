MODULE sdf_io

  USE sdf
  USE sdf_job_info

  implicit none

  private
  public :: load_sdf

  INTEGER, PARAMETER :: num = KIND(1.D0)
  INTEGER, PARAMETER :: num_sz = 8

  INTEGER, PARAMETER :: c_max_string_length = 128

  INTEGER, PARAMETER :: data_dir_max_length = 64
  CHARACTER(LEN = data_dir_max_length) :: data_dir

  INTEGER, PARAMETER :: n_zeros = 4

  INTEGER, PARAMETER :: c_ndims = 3

  TYPE(jobid_type) :: jobid

CONTAINS
  SUBROUTINE load_sdf

    CHARACTER(LEN=c_id_length) :: code_name, block_id, mesh_id, str1
    CHARACTER(LEN=c_max_string_length) :: name
    CHARACTER(LEN=22) :: filename_fmt
    CHARACTER(LEN=5+n_zeros+c_id_length) :: filename
    CHARACTER(LEN=6+data_dir_max_length+n_zeros+c_id_length) :: full_filename
    INTEGER :: blocktype, datatype, code_io_version, string_len
    INTEGER :: ierr, iblock, nblocks, ndims, geometry
    INTEGER, DIMENSION(4) :: dims
    INTEGER, DIMENSION(c_ndims) :: global_dims
    REAL(num), DIMENSION(2*c_ndims) :: extents
    LOGICAL :: restart_flag
    TYPE(sdf_file_handle) :: sdf_handle

    INTEGER :: step, file_number
    REAL(num) :: time

    step = -1
    filename = "test/test.sdf"

    full_filename = TRIM(filename)

    PRINT*,'Attempting to restart from file: ',TRIM(full_filename)

    CALL sdf_open(sdf_handle, full_filename, 0, c_sdf_read)

    CALL sdf_read_header(sdf_handle, step, time, code_name, code_io_version, &
        string_len, restart_flag)

    nblocks = sdf_read_nblocks(sdf_handle)
    jobid = sdf_read_jobid(sdf_handle)

    PRINT*, 'Loading snapshot for time', time

    PRINT*, 'Input file contains', nblocks, 'blocks'

    CALL sdf_read_blocklist(sdf_handle)

    CALL sdf_seek_start(sdf_handle)

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
END MODULE sdf_io
