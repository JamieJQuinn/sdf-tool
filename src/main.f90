program sdf_tool
  use sdf_io
  use mpi_routines
  use shared_data
  use flap, only : command_line_interface

  type(command_line_interface) :: cli
  character(99)                :: input, output
  integer                      :: error
  logical :: output_all
  character(c_id_length), allocatable :: output_variables(:)

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
  call cli%add(switch='--output-all', &
    help='output all variables',   &
    act='store_true',       &
    def='.false.',       &
    error=error)
  call cli%add(switch='--output-variables',&
    help='output select variables',required=.false.,act='store',&
    nargs='*',def='',error=error)
  call cli%get(switch='--input', val=input, error=error)
  call cli%get(switch='--output', val=output, error=error)
  call cli%get(switch='--output-all', val=output_all, error=error)
  call cli%get_varying(switch='--output-variables', val=output_variables, error=error)

  CALL mpi_start

  CALL load_sdf(input)
  CALL save_sdf(output, output_variables, output_all)

  CALL mpi_finish

end program sdf_tool
