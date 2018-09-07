program sdf_tool
  use sdf_io
  use mpi_routines
  use flap, only : command_line_interface

  type(command_line_interface) :: cli
  character(99)                :: input, output
  integer                      :: error

  print *, "Welcome to sdf-tool!"

  call cli%init(description = 'SDF Manipulation Tool')
  call cli%add(switch='--input', &
      switch_ab='-i',    &
      help='input file',   &
      required=.true.,   &
      act='store',       &
      error=error)
  call cli%add(switch='--output', &
      switch_ab='-o',    &
      help='output file',   &
      required=.true.,   &
      act='store',       &
      error=error)
  call cli%get(switch='--input', val=input, error=error)
  call cli%get(switch='--output', val=output, error=error)

  print *, "Output: ", output
  print *, "Input: ", input

  CALL mpi_start

  CALL load_sdf(input)
  CALL save_sdf(output)

  CALL mpi_finish

end program sdf_tool
