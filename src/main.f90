program sdf_tool
  use sdf_io
  use mpi_routines

  INTEGER :: i, narg
  CHARACTER(len=32) :: arg

  narg = iargc()

  i = 1
  DO while (i < narg)
    CALL get_command_argument(i, arg)
    print *, arg
    select case(arg)
    case("--input")
      CALL get_command_argument(i+1, arg)
      i = i+1
      print *, "Input: ", arg
    case("--output")
      CALL get_command_argument(i+1, arg)
      i = i+1
      print *, "Output: ", arg
    end select
    i = i+1
  end do

  print *, "Welcome to sdf-tool!"

  CALL mpi_start

  !CALL load_sdf("test/test.sdf")
  !CALL save_sdf("test/test_out.sdf")

  CALL mpi_finish

end program sdf_tool
