#include <mpi.h>
#include <stdio.h>

int power(int n, int m) {
    int result = 1;
    for (int i = 0; i < m; i++) {
        result *= n;
    }
    return result;
}

double wallis(int n){
  double numerator = 4*power(n, 2);
  double denominator = 4*power(n, 2)-1;
  return numerator/denominator;
}

int main(int argc,char *argv[]) {
  int rank,nb_procs,i;
  double value;
  double product=1;

  MPI_Init (&argc,&argv);
  MPI_Comm_size (MPI_COMM_WORLD,&nb_procs);
  MPI_Comm_rank (MPI_COMM_WORLD,&rank);

  if (rank == 0)
    value = 1;
  else{
    value = wallis(rank);
    // printf("Process %d, has the value: %f\n",rank,value);
  }

  MPI_Reduce (&value,&product,1,MPI_DOUBLE,MPI_PROD, 0, MPI_COMM_WORLD);
  if (rank == 0)
    printf("I, process %d, received the value of the global product %.2f\n",rank,2*product);

  MPI_Finalize ();
}