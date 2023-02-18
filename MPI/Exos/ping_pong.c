#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>


int main(int argc, char *argv[]) {
  int rank,iter,iter2, bufsize, overhead, typesize;
  int nb_tests=10;
  int nb_values[nb_tests];
  int nb_values_max=7000000;
  int tag=99;
  double *values;
  double * buffer;
  MPI_Status status;
  double time_begin,time_end;

  MPI_Init( &argc, &argv);

  nb_values[0]=0,nb_values[1]=0,nb_values[2]=1,nb_values[3]=10;
  nb_values[4]=100,nb_values[5]=1000,nb_values[6]=10000;
  nb_values[7]=100000,nb_values[8]=1000000,nb_values[9]=7000000;

  MPI_Comm_rank( MPI_COMM_WORLD, &rank);
  MPI_Type_size(MPI_DOUBLE, &typesize);

  values = (double *) malloc(nb_values_max*sizeof(double));

  for(iter=0; iter<nb_tests; iter++) {

    // overhead = (double) (1+(MPI_BSEND_OVERHEAD*1.)/typesize);
    // buffer = (double *) malloc(nb_values_max*sizeof(double));
    // bufsize = typesize*nb_values_max;
    MPI_Buffer_attach(values, nb_values[iter]);

    if (rank == 0) {
      for (iter2 = 0; iter2<nb_values_max; iter2++)
        values[iter2] = rand() / (RAND_MAX + 1.);
      time_begin=MPI_Wtime();
      MPI_Bsend(values,nb_values[iter],MPI_DOUBLE,1,tag,MPI_COMM_WORLD);
      MPI_Recv(values,nb_values[iter],MPI_DOUBLE,1,tag,MPI_COMM_WORLD,&status);
      time_end=MPI_Wtime();
      if (nb_values[iter] != 0) {
        printf("Me, process 0, sent and received %8d values"
               "(last = %4.2f) from process 1 in %8.6f seconds, bandwidth "
               "%7.2f MB/s.\n",
               nb_values[iter], values[nb_values[iter]-1],
               time_end-time_begin,
               2.*nb_values[iter]*8/1000000./(time_end-time_begin));
      } else
        printf("Me, process 0, sent and received %8d values in %8.6f "
               "seconds, bandwidth %7.2f MB/s.\n",
               nb_values[iter], time_end-time_begin,
               2.*nb_values[iter]*8/1000000./(time_end-time_begin));
    } else if(rank == 1) {
      MPI_Recv(values,nb_values[iter],MPI_DOUBLE,0,tag,
               MPI_COMM_WORLD,&status);
      MPI_Bsend(values,nb_values[iter],MPI_DOUBLE,0,tag,MPI_COMM_WORLD);
    }
  }

  MPI_Finalize();
  return 0;
}
