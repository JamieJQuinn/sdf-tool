MODULE mpi_routines

  USE shared_data

  implicit none

  !include 'mpif.h'

  PRIVATE
  PUBLIC :: mpi_start, mpi_finish
CONTAINS

  SUBROUTINE mpi_start
    INTEGER :: errcode
    CALL MPI_INIT(errcode)
  END SUBROUTINE mpi_start

  SUBROUTINE mpi_finish
    INTEGER :: errcode
    CALL MPI_FINALIZE(errcode)
  END SUBROUTINE mpi_finish

  !! This function taken directly from Lare3d and modified slightly
  SUBROUTINE mpi_create_types(nx_global, ny_global, nz_global)

    INTEGER, INTENT(IN) :: nx_global, ny_global, nz_global

    INTEGER :: sizes(c_ndims), subsizes(c_ndims), starts(c_ndims)
    INTEGER :: local_dims(c_ndims), global_dims(c_ndims)
    INTEGER :: idir, vdir, mpitype, errcode
    INTEGER, PARAMETER :: ng = 2 ! Number of ghost cells

    local_dims = (/nx_global, ny_global, nz_global/)
    global_dims = (/nx_global, ny_global, nz_global/)

    ! File view for cell-centred variables (excluding the ghost cells)
    sizes = global_dims
    subsizes = local_dims
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    cell_distribution = mpitype

    ! Subarray for cell-centred variable which has no ghost cells
    sizes = subsizes
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    cellng_subarray = mpitype

    ! Cell-centred array dimensions
    sizes = subsizes + 2 * ng

    ! Subarray for cell-centred variable which excludes the ghost cells
    starts = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    cell_subarray = mpitype

    ! MPI subtypes for communication of cell-centred variables

    ! ng cells, 1d slice of cell-centred variable

    idir = 1
    subsizes = sizes
    subsizes(idir) = ng
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    cell_xface = mpitype

    idir = 2
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    cell_yface = mpitype

    idir = 3
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    cell_zface = mpitype

    ! File view for node-centred variables (excluding the ghost cells)
    sizes = global_dims + 1
    subsizes = local_dims + 1
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_distribution = mpitype

    ! Subarray for node-centred variable which has no ghost cells
    sizes = subsizes
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    nodeng_subarray = mpitype

    ! Node-centred array dimensions
    sizes = subsizes + 2 * ng

    ! Subarray for node-centred variable which excludes the ghost cells
    starts = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_subarray = mpitype

    ! MPI subtypes for communication of node-centred variables

    ! ng cells, 1d slice of node-centred variable

    idir = 1
    subsizes = sizes
    subsizes(idir) = ng
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_xface = mpitype

    idir = 2
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_yface = mpitype

    idir = 3
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_zface = mpitype

    ! ng+1 cells, 1d slice of node-centred variable

    idir = 1
    subsizes = sizes
    subsizes(idir) = ng + 1

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_xface1 = mpitype

    idir = 2
    subsizes = sizes
    subsizes(idir) = ng + 1

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_yface1 = mpitype

    idir = 3
    subsizes = sizes
    subsizes(idir) = ng + 1

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    node_zface1 = mpitype

    ! Array sizes for Bx-sized variables
    vdir = 1
    sizes = global_dims
    sizes(vdir) = sizes(vdir) + 1

    ! File view for Bx-sized variables (excluding the ghost cells)
    subsizes = local_dims
    subsizes(vdir) = subsizes(vdir) + 1
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bx_distribution = mpitype

    ! Bx-sized array dimensions
    sizes = subsizes + 2 * ng

    ! Subarray for Bx-sized variable which excludes the ghost cells
    starts = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bx_subarray = mpitype

    ! MPI subtypes for communication of Bx-sized variables

    ! ng cells, 1d slice of Bx-sized variable

    idir = 1
    subsizes = sizes
    subsizes(idir) = ng
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bx_xface = mpitype

    idir = 2
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bx_yface = mpitype

    idir = 3
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bx_zface = mpitype

    ! ng+1 cells, 1d slice of Bx-sized variable

    idir = vdir
    subsizes = sizes
    subsizes(idir) = ng + 1

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bx_xface1 = mpitype

    ! Array sizes for By-sized variables
    vdir = 2
    sizes = global_dims
    sizes(vdir) = sizes(vdir) + 1

    ! File view for By-sized variables (excluding the ghost cells)
    subsizes = local_dims
    subsizes(vdir) = subsizes(vdir) + 1
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    by_distribution = mpitype

    ! By-sized array dimensions
    sizes = subsizes + 2 * ng

    ! Subarray for By-sized variable which excludes the ghost cells
    starts = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    by_subarray = mpitype

    ! MPI subtypes for communication of By-sized variables

    ! ng cells, 1d slice of By-sized variable

    idir = 1
    subsizes = sizes
    subsizes(idir) = ng
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    by_xface = mpitype

    idir = 2
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    by_yface = mpitype

    idir = 3
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    by_zface = mpitype

    ! ng+1 cells, 1d slice of By-sized variable

    idir = vdir
    subsizes = sizes
    subsizes(idir) = ng + 1

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    by_yface1 = mpitype

    ! Array sizes for Bz-sized variables
    vdir = 3
    sizes = global_dims
    sizes(vdir) = sizes(vdir) + 1

    ! File view for Bz-sized variables (excluding the ghost cells)
    subsizes = local_dims
    subsizes(vdir) = subsizes(vdir) + 1
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bz_distribution = mpitype

    ! Bz-sized array dimensions
    sizes = subsizes + 2 * ng

    ! Subarray for Bz-sized variable which excludes the ghost cells
    starts = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bz_subarray = mpitype

    ! MPI subtypes for communication of Bz-sized variables

    ! ng cells, 1d slice of Bz-sized variable

    idir = 1
    subsizes = sizes
    subsizes(idir) = ng
    starts = 0

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bz_xface = mpitype

    idir = 2
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bz_yface = mpitype

    idir = 3
    subsizes = sizes
    subsizes(idir) = ng

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bz_zface = mpitype

    ! ng+1 cells, 1d slice of Bz-sized variable

    idir = vdir
    subsizes = sizes
    subsizes(idir) = ng + 1

    mpitype = MPI_DATATYPE_NULL
    CALL MPI_TYPE_CREATE_SUBARRAY(c_ndims, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, mpireal, mpitype, errcode)
    CALL MPI_TYPE_COMMIT(mpitype, errcode)

    bz_zface1 = mpitype

  END SUBROUTINE mpi_create_types

END MODULE
