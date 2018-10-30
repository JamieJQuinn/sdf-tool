# SDF-tool

## What is SDF?

SDF (self-describing format) is a file format developed at Warwick designed to contain simulation data.

## What is SDF-tool?

`sdf-tool` is a simple tool for manipulating SDF files, particularly those outputted from the Lare3D code.

## Features

- [x] Reading/writing of SDF files
- [x] Removing whole variables
- [x] Taking 3D slices
- [ ] Calculating new variables

## Prerequisites 

In order to compile and run `sdf-tool` you must have a local version of MPI linked to gfortran. I haven't yet tested intel's fortran compilers + MPI. The use of MPI is integral to the SDF library. 

## Installation

Installing should be as simple as cloning the git repository:
```
git clone --recursive https://github.com/JamieJQuinn/sdf-tool.git
```
The use of recursive should automatically clone the SDF library.

A simple `make` will compile the tool and it can be run with `bin/sdf-tool`.

## Usage

*Input/Output*

`sdf-tool -i in.sdf -o out.sdf`

*Specifying variables*

Variables are selected by their block names

`sdf-tool --output-variables Energy Vx`

*Slicing*

The slice is inclusive and in index space, so `1 100` slices from index number 1 to 100, inclusive.

`sdf-tool --slice 1 100 1 100 25 75`
