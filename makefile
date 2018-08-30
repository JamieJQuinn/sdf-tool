SRCFILES=main.f90 sdf_io.f90 mpi_routines.f90 shared_data.f90
OBJFILES := $(SRCFILES:.f90=.o)

SRCDIR=src
OBJDIR=obj
BINDIR=bin

TARGET=main

SDF := SDF/FORTRAN
SDFMOD = $(SDF)/include/sdf.mod

FULLTARGET = $(BINDIR)/$(TARGET)

export MPIF90=mpif90
export COMPILER=gfortran
export MODE=debug
MODULEFLAG = -I/usr/local/include -I$(OBJDIR) -J$(OBJDIR)
FFLAGS = $(MODULEFLAG) -I$(SDF)/include -g
LDFLAGS = $(FFLAGS) -L$(SDF)/lib -lsdf

FC=$(MPIF90)

VPATH = $(SRCDIR):$(SRCDIR)/core:$(SDF)/src:$(OBJDIR)

all: $(FULLTARGET)

$(FULLTARGET): $(SDFMOD) $(OBJFILES)
	@mkdir -p $(BINDIR)
	$(FC) -o $@ $(addprefix $(OBJDIR)/,$(OBJFILES)) $(LDFLAGS)

$(SDFMOD):
	$(MAKE) -C $(SDF)

%.o: %.f90
	$(FC) -c $(FFLAGS) -o $(OBJDIR)/$@ $<

$(FULLTARGET): | $(BINDIR)

$(BINDIR):
	@mkdir -p $(BINDIR)

$(OBJFILES): | $(OBJDIR)

$(OBJDIR):
	@mkdir -p $(OBJDIR)

clean:
	rm -rf $(OBJDIR) $(FULLTARGET)

main.o: main.f90 sdf_io.o mpi_routines.o shared_data.o
sdf_io.o: sdf_io.f90 mpi_routines.o shared_data.o
mpi_routines.o: mpi_routines.f90 shared_data.o
