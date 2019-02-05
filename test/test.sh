#!/usr/bin/env bash

SDFFILTER=~/prog/sdf-tool/SDF/utilities/sdffilter
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}"  )" >/dev/null && pwd  )"

function do_headers_match ()
{
  diff <($SDFFILTER -v 0 test.sdf) <($SDFFILTER -v 0 test_out.sdf)
  diff <($SDFFILTER -v run_info test.sdf) <($SDFFILTER -v run_info test_out.sdf)
  diff <($SDFFILTER -v cpu_rank test.sdf) <($SDFFILTER -v cpu_rank test_out.sdf)
}

function do_serial_blocks_match ()
{
  diff <($SDFFILTER -v dt test.sdf) <($SDFFILTER -v dt test_out.sdf)
  diff <($SDFFILTER -v time_prev test.sdf) <($SDFFILTER -v time_prev test_out.sdf)
  diff <($SDFFILTER -v visc_heating test.sdf) <($SDFFILTER -v visc_heating test_out.sdf)
}

function do_grids_match ()
{
  diff <($SDFFILTER -c -v grid test.sdf) <($SDFFILTER -c -v grid test_out.sdf)
}

function do_variables_match ()
{
  diff <($SDFFILTER -v Rho test.sdf) <($SDFFILTER -v Rho test_out.sdf)
  diff <($SDFFILTER -v Energy test.sdf) <($SDFFILTER -v Energy test_out.sdf)
  diff <($SDFFILTER -v Vx test.sdf) <($SDFFILTER -v Vx test_out.sdf)
  diff <($SDFFILTER -v Vy test.sdf) <($SDFFILTER -v Vy test_out.sdf)
  diff <($SDFFILTER -v Vz test.sdf) <($SDFFILTER -v Vz test_out.sdf)
  diff <($SDFFILTER -v Bx test.sdf) <($SDFFILTER -v Bx test_out.sdf)
  diff <($SDFFILTER -v By test.sdf) <($SDFFILTER -v By test_out.sdf)
  diff <($SDFFILTER -v Bz test.sdf) <($SDFFILTER -v Bz test_out.sdf)
  diff <($SDFFILTER -v eta test.sdf) <($SDFFILTER -v eta test_out.sdf)
  diff <($SDFFILTER -v iso test.sdf) <($SDFFILTER -v iso test_out.sdf)
  diff <($SDFFILTER -v aniso test.sdf) <($SDFFILTER -v aniso test_out.sdf)
}

function does_block_selection_work ()
{
  if [ "$($SDFFILTER -v 0 test_out.sdf | awk '/nblocks_file/ {print $2}')" -eq "16" ]
  then
    true
  else
    echo "Block Selection Failed!"
    echo "number of blocks: $($SDFFILTER -v 0 test_out.sdf | awk '/nblocks_file/ {print $2}')" "should have been 16"
    false
  fi
}

cd $DIR
rm test_out.sdf
../bin/sdf-tool --input test.sdf --output test_out.sdf --output-all-variables

do_headers_match
do_serial_blocks_match
do_grids_match
do_variables_match

rm test_out.sdf
../bin/sdf-tool --input test.sdf --output test_out.sdf --output-variables Energy Rho Vx Vy Vz Bx By Bz eta Isotropic_Viscous_Heating

does_block_selection_work
