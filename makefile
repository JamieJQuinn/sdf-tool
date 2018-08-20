SOURCE_FILES=main.f90
SOURCE_FOLDER=src
SRC=$(addprefix ${SOURCE_FOLDER}/,${SOURCE_FILES})

OUTPUT=bin/sdf-tool

all: ${SRC}
	gfortran -o ${OUTPUT} ${SRC}
