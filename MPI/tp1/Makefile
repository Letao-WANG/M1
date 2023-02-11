############################ -*- Mode: Makefile -*- ###########################
## Makefile --- MPI : TP1 : Environment
##
## Author          : Dimitri Lecas (CNRS/IDRIS) <dimitri.lecas@idris.fr>
###############################################################################

# Compiler options and link
include ../arch/make_gnu

OBJS = even_odd.o

# Implicit compil rule
.SUFFIXES: .o .c
.c.o:
	$(CC) -c $(CFLAGS_TP1) $<

all: even_odd

even_odd:$(OBJS)
	$(CC) -o $@ $(LDFLAGS_TP1) $(OBJS)

.PHONY: clean exe

exe : even_odd
	mpiexec -n 4 ./even_odd
#SBATCH	sbatch even_odd.slurm

clean:
	rm -f $(OBJS) even_odd core
