/* who_am_i */
#include <mpi.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  int nb_procs,rank;

  MPI_Init (&argc,&argv);

  MPI_Comm_size (MPI_COMM_WORLD,&nb_procs);
  MPI_Comm_rank (MPI_COMM_WORLD,&rank);

  printf("I am the process %d among %d\n",rank,nb_procs);

  MPI_Finalize ();
}