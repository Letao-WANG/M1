############################ -*- Mode: Makefile -*- ###########################
## Makefile ---  MPI : TP2 : point-to-point communications  - ping-pong
##
## Author          : Dimitri Lecas (CNRS/IDRIS)
##
###############################################################################

# Compilers, options for compilation and link
include ../arch/make_gnu

OBJS1 = ping_pong_1.o
OBJS2 = ping_pong_2.o
OBJS3 = ping_pong_3.o

# Implicit rule of compilation
.SUFFIXES: .o .c
.c.o:
	$(CC) -c $(CFLAGS_TP2) $<

ping_pong_1: $(OBJS1)
	$(CC) -o $@ $(LDFLAGS_TP2) $(OBJS1)

ping_pong_2: $(OBJS2)
	$(CC) -o $@ $(LDFLAGS_TP2) $(OBJS2)

ping_pong_3: $(OBJS3)
	$(CC) -o $@ $(LDFLAGS_TP2) $(OBJS3)

.PHONY: clean exe1 exe2 exe3

exe1 : ping_pong_1
	mpiexec -n 2 ./ping_pong_1
#SBATCH	sbatch ping_pong_1.slurm

exe2 : ping_pong_2
	mpiexec -n 2 ./ping_pong_2
#SBATCH	sbatch ping_pong_2.slurm

exe3 : ping_pong_3
	mpiexec -n 2 ./ping_pong_3
#SBATCH	sbatch ping_pong_3.slurm

clean:
	rm -f $(OBJS1) $(OBJS2) $(OBJS3) ping_pong_1 ping_pong_2 ping_pong_3 core
