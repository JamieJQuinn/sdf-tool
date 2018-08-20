SRCFILES=main.f90 sdf_io.f90
OBJFILES := $(SRCFILES:.f90=.o)

SRCDIR=src
OBJDIR=obj
BINDIR=bin

TARGET=sdf-tool

SDF := SDF/FORTRAN
SDFMOD = $(SDF)/include/sdf.mod

FULLTARGET = $(BINDIR)/$(TARGET)

export MPIF90=mpif90
export COMPILER=gfortran
MODULEFLAG = -I/usr/local/include -I$(OBJDIR) -J$(OBJDIR)
FFLAGS = $(MODULEFLAG) -I$(SDF)/include
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

$(OBJFILES): | $(OBJDIR)

$(OBJDIR):
	@mkdir -p $(OBJDIR)

main.o: main.f90 sdf_io.o
