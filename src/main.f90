program sdf_tool
  use sdf_io
  use mpi_routines

  print *, "Welcome to sdf-tool!"

  CALL mpi_start

  CALL load_sdf

  CALL mpi_finish

end program sdf_tool
