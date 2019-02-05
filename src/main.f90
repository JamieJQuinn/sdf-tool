!sdf-tool: simple SDF file manipulation tool
!Copyright (C) 2018 Jamie Quinn

!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.

!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.

!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  integer, dimension(6) :: slices

  print *, "Welcome to sdf-tool!"

  call cli%init(progname = 'sdf-tool',&
                authors = 'Jamie Quinn',&
                license = 'GPLv3',&
                description = 'SDF Manipulation Tool',&
                examples = ['sdf-tool -i in.sdf -o out.sdf --output-variables Energy Vx --slice 1 5 1 5 100 200'])
  call cli%add(switch='--input', &
    switch_ab='-i',    &
    help='input file',   &
    required=.true.,   &
    act='store',       &
    error=error)
  if (error/=0) stop
  call cli%add(switch='--output', &
    switch_ab='-o',    &
    help='output file',   &
    required=.true.,   &
    act='store',       &
    error=error)
  if (error/=0) stop
  call cli%add(switch='--output-all-variables', &
    help='output all variables',   &
    act='store_true',       &
    def='.false.',       &
    error=error)
  if (error/=0) stop
  call cli%add(switch='--output-variables',&
    help='output select variables',required=.false.,act='store',&
    nargs='*',def='',error=error)
  if (error/=0) stop
  call cli%add(switch='--slice',&
    help='ix_min ix_max iy_min iy_max iz_min iz_max INCLUSIVE',required=.false.,act='store',&
    nargs='6',def='0 0 0 0 0 0',error=error)
  if (error/=0) stop

  call cli%get(switch='--input', val=input, error=error)
  if (error/=0) stop
  call cli%get(switch='--output', val=output, error=error)
  if (error/=0) stop
  call cli%get(switch='--output-all-variables', val=output_all, error=error)
  if (error/=0) stop
  call cli%get_varying(switch='--output-variables', val=output_variables, error=error)
  if (error/=0) stop
  call cli%get(switch='--slice', val=slices, error=error)
  if (error/=0) stop

  CALL mpi_start

  CALL load_sdf(input)
  CALL save_sdf(output, output_variables, output_all, slices)

  CALL mpi_finish

end program sdf_tool
