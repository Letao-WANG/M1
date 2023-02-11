/*
 * transpose.f90  --- derived datatypes  (type_transpose)
 *                    to transpose matrix
 *
 *
 * Author          : Dimitri LECAS (CNRS/IDRIS - France)
 *                   <dimitri.lecas@idris.fr>
 *
 */

#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  int nb_lines=5;
  int nb_columns=4;
  int tag=1000;
  double a[nb_lines][nb_columns];
  double at[nb_columns][nb_lines];
  int rank,iterl,iterc,size_real;
  MPI_Datatype type_column, type_transpose;
  MPI_Aint new_extent,new_lbound=0;
  MPI_Status status;

  /* MPI Init */
  MPI_Init( &argc, &argv);

  /* Who I am */
  MPI_Comm_rank( MPI_COMM_WORLD, &rank);

  /* type_column for at */
  MPI_Type_vector(nb_columns,1,nb_lines,MPI_DOUBLE,&type_column);
  /* Type_transpose for at */
  MPI_Type_size(MPI_DOUBLE,&size_real);
  new_extent = size_real;
  MPI_Type_create_resized(type_column,new_lbound,new_extent,
                          &type_transpose);
  /* Validation of the derived datatypes type_transpose */
  MPI_Type_commit(&type_transpose);

  if (rank == 0) {
    /* Initialisation of A */
    for (iterl=0; iterl<nb_lines; iterl++)
      for (iterc=0; iterc<nb_columns; iterc++)
        a[iterl][iterc] = 1+iterc*nb_lines+iterl;

    printf("Matrix a\n");
    for (iterl=0; iterl<nb_lines;iterl++) {
      for (iterc=0; iterc<nb_columns; iterc++) {
        printf("%4.f ", a[iterl][iterc]);
      }
      printf("\n");
    }

    /* Send matrix A to process 1 */
    MPI_Send(a,nb_columns*nb_lines,MPI_DOUBLE,1,tag,MPI_COMM_WORLD);
  } else {
    /* Receive in matrix AT with type_transpose */
    MPI_Recv(at,nb_lines,type_transpose,0,tag,
             MPI_COMM_WORLD,&status);

    printf("Matrice transposee at\n");
    for (iterc=0; iterc<nb_columns; iterc++) {
      for (iterl=0; iterl<nb_lines;iterl++) {
        printf("%4.f ", at[iterc][iterl]);
      }
      printf("\n");
    }
  }

  /* Clean MPI types */

  /* Finalize MPI */
  MPI_Finalize();
  return 0;
}
