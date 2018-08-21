program sdf_tool
  use sdf_io
  use mpi_routines

  print *, "Welcome to sdf-tool!"

  CALL mpi_minimal_init

  CALL load_sdf

  CALL mpi_close

end program sdf_tool
