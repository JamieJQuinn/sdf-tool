MODULE mpi_routines

  include 'mpif.h'

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

END MODULE
