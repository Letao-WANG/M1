//
// Created by letao on 2/12/23.
//

#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#ifdef USE_MKL
#include <mkl.h>
#endif

/* Allocate matrix and init to zero */
void matrix_allocate(double **mat, int linenbr, int colnbr) {
    int iter;
    (*mat) = (double *) malloc(linenbr*colnbr*sizeof(double));
    for (iter=0; iter<linenbr*colnbr; iter++) {
        (*mat)[iter] = 0.; }
}

/* Init matrix */
void random_number(double *mat,int line, int col) {
    int iterl,iterc;
    for(iterl=0; iterl<line; iterl++)
        for(iterc=0;iterc<col; iterc++)
            mat[iterl*line+iterc] =  iterl*col+iterc;
}

int main(int argc, char *argv[]) {
    int col = 4;
    int line = 5;
    double *A;
    double *res;
    int tag=1000;
    int rank;
    MPI_Datatype type_col;
    MPI_Status status;

    MPI_Init( &argc, &argv);
    MPI_Comm_rank( MPI_COMM_WORLD, &rank);

    if (rank == 0) {
        matrix_allocate(&A, line, col);
        random_number(A, line, col);
        for (int iterc=0; iterc<line; iterc++) {
          for (int iterl=0; iterl<col;iterl++) {
            printf("%4.f ", A[iterc*line+iterl]);
          }
          printf("\n");
        }
        MPI_Type_vector (line,1, col, MPI_DOUBLE, &type_col);
        MPI_Type_commit(&type_col);
        MPI_Send(A, 1, type_col, 1, tag, MPI_COMM_WORLD);
    } else {
        res = (double *) malloc(line*col*sizeof(double));
        MPI_Recv(&res[0], 1, type_col, 0,tag,MPI_COMM_WORLD,&status);
        printf("------------------The result is : %2.f--------------\n", res[0]);
        for (int iterl=0; iterl<line*col;iterl++) {
          printf("%2.f ", res[iterl]);
        }
        printf("\n");
    }
    MPI_Finalize ();
    return 0;
}