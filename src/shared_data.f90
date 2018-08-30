MODULE shared_data

  USE sdf

  implicit none

  include 'mpif.h'

  INTEGER, PARAMETER :: num = KIND(1.D0)
  INTEGER, PARAMETER :: num_sz = 8
  !INTEGER, PARAMETER :: c_max_string_length = 128

  INTEGER, PARAMETER :: data_dir_max_length = 64

  INTEGER, PARAMETER :: n_zeros = 4
  INTEGER, PARAMETER :: c_ndims = 3
  !INTEGER, PARAMETER :: i4  = SELECTED_INT_KIND(9)  ! 4-byte 2^31 ~ 10^9
  !INTEGER, PARAMETER :: i8  = SELECTED_INT_KIND(0_8)  ! 4-byte 2^31 ~ 10^9

  !! MPI variables
  INTEGER, PARAMETER :: mpireal = MPI_DOUBLE_PRECISION
  INTEGER, PARAMETER :: sdf_num = c_datatype_real8

  INTEGER :: cell_subarray, cellng_subarray, cell_distribution
  INTEGER :: node_subarray, nodeng_subarray, node_distribution
  INTEGER :: bx_subarray, bx_distribution
  INTEGER :: by_subarray, by_distribution
  INTEGER :: bz_subarray, bz_distribution
  INTEGER :: cell_xface, node_xface, node_xface1
  INTEGER :: cell_yface, node_yface, node_yface1
  INTEGER :: cell_zface, node_zface, node_zface1
  INTEGER :: bx_xface, by_xface, bz_xface, bx_xface1
  INTEGER :: bx_yface, by_yface, bz_yface, by_yface1
  INTEGER :: bx_zface, by_zface, bz_zface, bz_zface1

END MODULE shared_data
