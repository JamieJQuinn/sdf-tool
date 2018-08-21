MODULE mpi_routines

  include 'mpif.h'

  PRIVATE
  PUBLIC :: mpi_minimal_init, mpi_close

CONTAINS

  SUBROUTINE mpi_minimal_init
    INTEGER :: rank, errcode, nproc
    CALL MPI_INIT(errcode)
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, errcode)
  END SUBROUTINE mpi_minimal_init

  SUBROUTINE mpi_close
    INTEGER :: errcode
    CALL MPI_FINALIZE(errcode)
  END SUBROUTINE mpi_close

END MODULE
