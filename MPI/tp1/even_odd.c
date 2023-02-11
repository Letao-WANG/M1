/* even odd */
#include <mpi.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  int nb_procs,rank;

  MPI_Init (&argc,&argv);

  MPI_Comm_size (MPI_COMM_WORLD,&nb_procs);
  MPI_Comm_rank (MPI_COMM_WORLD,&rank);

  if (rank % 2 == 0){
    printf("I am even, and my rank is: %d\n", rank);
  } else {
    printf("I am odd, and my rank is: %d\n", rank);
  }
  // printf("I am the process %d among %d\n",rank,nb_procs);

  MPI_Finalize ();
}