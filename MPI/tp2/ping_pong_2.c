/* block */
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc,char *argv[]) {
  int rank,i,j, size_real;
  int nb_lines=5,nb_columns=4, tag=100;
  int nb_lines_block=5,nb_columns_block=1;
  double a[nb_lines][nb_columns];
  double res[nb_columns][nb_lines];
  MPI_Datatype type_block, type_trans;
  MPI_Aint size_displacement;
  MPI_Status statut;

  MPI_Init (&argc,&argv);
  MPI_Comm_rank (MPI_COMM_WORLD,&rank);

  /* Initialization of the matrix on each process */
  for(i=0;i<nb_lines;i++)
    for(j=0;j<nb_columns;j++)
      a[i][j]=i*nb_columns+j;

  MPI_Type_vector (nb_lines_block,nb_columns_block,nb_columns,MPI_DOUBLE,&type_block);
  MPI_Type_size(MPI_DOUBLE,&size_real);
  size_displacement = size_real;
  MPI_Type_create_hvector(nb_lines,1,size_displacement,type_block,&type_trans);
  MPI_Type_commit(&type_trans);
  
  if (rank == 0) {
    printf("-------- Before sending: ------------ \n");
    for (int iterc=0; iterc<nb_lines; iterc++) {
      for (int iterl=0; iterl<nb_columns;iterl++) {
        printf("%4.f ", a[iterc][iterl]);
      }
      printf("\n");
    }
    MPI_Send (a,nb_lines*nb_columns,MPI_DOUBLE,1,tag,MPI_COMM_WORLD);
  } else {
    MPI_Recv (res,1,type_trans,0,tag,MPI_COMM_WORLD,&statut); 
    printf("-------- After recived: ------------ \n");
    for (int iterc=0; iterc<nb_lines; iterc++) {
      for (int iterl=0; iterl<nb_columns;iterl++) {
        printf("%4.f ", res[iterc][iterl]);
      }
      printf("\n");
    }
  }

  /* Freeing of the datatype type_block */
  MPI_Type_free (&type_block);
  MPI_Finalize ();
}