# Run with 'make -f ../Makefile' from the daily directories

all: run

run: fortran.bin
	./fortran.bin

fortran.bin: main.f90 ../aoc.f90
	gfortran -g $(FFLAGS) ../aoc.f90 main.f90 -o $@

.PHONY: run
