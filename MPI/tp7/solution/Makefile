############################ -*- Mode: Makefile -*- ###########################
## Makefile --- TP7
##
## Author          : Dimitri Lecas (CNRS/IDRIS - France)
##
###############################################################################

# Compilateur et options de compilation et de link
include ../../arch/make_inc

OBJS    = read_file.o

# Implicite rule for compilation
.SUFFIXES:
.SUFFIXES: .o .c
.c.o:
	$(CC) -c $(CFLAGS_TP7) $<

read_file:data.dat $(OBJS)
	$(CC) $(LDFLAGS_TP7) -o $@ $(OBJS)

data.dat:
	cd creation;make clean;make

verification: verification.c file_dei0.dat
	$(CC) verification.c -o verification
	$(EXEC_TP7_VERIF) ./verification

.PHONY : clean cleanall exe

exe: read_file
	mpiexec -n 4 ./read_file
#SBATCH	sbatch read_file.slurm

clean:
	rm -f *.o read_file fichier*.dat verification core*

cleanall:
	make clean
	rm -f data.dat
