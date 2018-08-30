#!/usr/bin/env bash

SDFFILTER=~/prog/sdf-tool/SDF/utilities/sdffilter

function do_headers_match ()
{
  diff <($SDFFILTER -v 0 test.sdf | sed '7,9d') <($SDFFILTER -v 0 test_out.sdf | sed '7,9d')
}

function do_serial_blocks_match ()
{
  diff <($SDFFILTER -v dt test.sdf) <($SDFFILTER -v dt test_out.sdf)
  diff <($SDFFILTER -v time_prev test.sdf) <($SDFFILTER -v time_prev test_out.sdf)
  diff <($SDFFILTER -v visc_heating test.sdf) <($SDFFILTER -v visc_heating test_out.sdf)
}

function do_grids_match ()
{
  diff <($SDFFILTER -v grid test.sdf) <($SDFFILTER -v grid test_out.sdf)
}

do_headers_match
do_serial_blocks_match
do_grids_match
