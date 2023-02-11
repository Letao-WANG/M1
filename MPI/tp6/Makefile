############################ -*- Mode: Makefile -*- ###########################
## Makefile --- MPI : TP6 : subdividing a 2D cartesian topology
##
## Author          : Dimitri Lecas (CNRS/IDRIS)
##
###############################################################################

# Compilers, options for compilation and link
include ../arch/make_inc

OBJS = commsplit.o

# Implicit rule for compilation
.SUFFIXES: .o .c
.c.o:
	$(CC) -c $(CFLAGS_TP6) $<

all : commsplit

commsplit: $(OBJS)
	$(CC) -o $@ $(LDFLAGS_TP6) $(OBJS)

.PHONY: clean exe

exe: commsplit
	mpiexec -n 8 ./commsplit
#SBATCH	sbatch commsplit.slurm

clean :
	rm -f $(OBJS) commsplit core
