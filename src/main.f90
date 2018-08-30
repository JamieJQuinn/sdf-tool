program sdf_tool
  use sdf_io
  use mpi_routines

  print *, "Welcome to sdf-tool!"

  CALL mpi_start

  CALL load_sdf("test/test.sdf")
  CALL save_sdf("test/test_out.sdf")

  CALL mpi_finish

end program sdf_tool
