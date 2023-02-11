############################ -*- Mode: Makefile -*- ###########################
## Makefile --- MPI : TP4 : derived datatypes (type_transpose)
##
## Author          : Dimitri LECAS (CNRS/IDRIS) <dimitri.lecas@idris.fr>
##
###############################################################################

# Compilers, options for compilation and link
include ../../arch/make_inc

OBJS = transpose.o

# Implicit rule of compilation
.SUFFIXES: .o .c
.c.o:
	$(CC) -c $(CFLAGS_TP4) $<

all : transpose

transpose: $(OBJS)
	$(CC) -o $@ $(LDFLAGS_TP4) $(OBJS)

.PHONY: clean exe

exe : transpose
	mpiexec -n 2 ./transpose
#SBATCH	sbatch transpose.slurm

clean :
	rm -f $(OBJS) transpose core
