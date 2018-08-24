#!/usr/bin/env bash

SDFFILTER=~/prog/sdf-tool/SDF/utilities/sdffilter

function do_headers_match ()
{
  diff <($SDFFILTER -v 0 test.sdf) <($SDFFILTER -v 0 test_out.sdf)
}

do_headers_match
