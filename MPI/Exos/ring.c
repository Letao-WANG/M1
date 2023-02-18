#include <stdio.h>
#include <mpi.h>

int main(int argc, char **argv)
{
  int rank, size;
  int value = 1000;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if (rank == 0)
  {
    MPI_Send(&value, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
    MPI_Recv(&value, 1, MPI_INT, size - 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    printf("value: %d\n", value);
  }
  else
  {
    MPI_Recv(&value, 1, MPI_INT, rank - 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    value++;
    MPI_Send(&value, 1, MPI_INT, (rank + 1) % size, 0, MPI_COMM_WORLD);
  }
  MPI_Finalize();
  return 0;
}
