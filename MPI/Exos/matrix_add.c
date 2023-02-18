#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

int tag=1000;

void print_matrix(double* a, int nb_lines, int nb_columns){
  printf("Matrix : \n");
    for (int iterl=0; iterl<nb_lines;iterl++) {
      for (int iterc=0; iterc<nb_columns; iterc++) {
        printf("%4.f ", a[iterl*nb_columns+iterc]);
      }
      printf("\n");
    }
}

/* Allocate matrix and init to zero */
void matrix_allocate(double **mat, int linenbr, int colnbr) {
  int iter;
  (*mat) = (double *) malloc(linenbr*colnbr*sizeof(double));
  for (iter=0; iter<linenbr*colnbr; iter++) {
    (*mat)[iter] = 0.; }
}

/* Init matrix with random number */
void random_number(double *mat,int n) {
  int iterl,iterc;
  for(iterl=0; iterl<n; iterl++)
    for(iterc=0;iterc<n; iterc++)
      mat[iterl*n+iterc] = iterl*n+iterc;
}

/* Matrix product C = A*B */
void matrix_add(double *A, double *B, double *C, int nl, int nc) {
  int iterl,iterc;
  double sum;
  for(iterl=0;iterl<nl;iterl++) {
    for(iterc=0;iterc<nc;iterc++) {
      C[iterl*nc+iterc] = A[iterl*nc+iterc] + B[iterl*nc+iterc];
    }
  }
}

int main(int argc, char *argv[]) {
  int rank,Nprocs,N,NL;
  double *A,*B,*C,*CC;
  double *AL,*BL,*CL,*TEMP;
  MPI_Datatype type_temp,type_slice;
  int double_size;
  MPI_Aint lbound,displacement;
  int iter;
  MPI_Status status;
  int previous_rank,following_rank;

  /* Init MPI */
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &Nprocs);

  N = 4;
  /* Process 0 broadcast N */
  MPI_Bcast(&N,1,MPI_INTEGER,0,MPI_COMM_WORLD);

  /* N need to be divisible by Nprocs */
  if ( (N % Nprocs) == 0)
    NL = N/Nprocs;
  else {
    fprintf(stderr, "N is not divisible by Nprocs\n");
    /* We stop the program */
    MPI_Abort(MPI_COMM_WORLD,1);
  }

  /* Process 0 init matrices A and B */
  if (rank ==0) {
    /* Dynamic allocation of matrices A, B & C */
    matrix_allocate(&A,N,N);matrix_allocate(&B,N,N);
    matrix_allocate(&C,N,N);matrix_allocate(&CC,N,N);

    /* Init A & B */
    random_number(A,N);
    random_number(B,N);

    /* Sequential computation of A*B */
    matrix_add(A,B,CC,N,N);

    // print_matrix(A, N, N);
    // print_matrix(B, N, N);
    // print_matrix(CC, N, N);
  }

  /* Dynamoc allocation of local matrices */
  matrix_allocate(&AL,NL,N);
  matrix_allocate(&BL,NL,N);
  matrix_allocate(&CL,NL,N);;

  /* Processus 0 distribute in AL the horizontal slices of A */
  MPI_Scatter(A, NL*N, MPI_DOUBLE, AL, NL*N, MPI_DOUBLE,0, MPI_COMM_WORLD);
  MPI_Scatter(B, NL*N, MPI_DOUBLE, BL, NL*N, MPI_DOUBLE,0, MPI_COMM_WORLD);

  // print_matrix(AL, NL, N);
  // print_matrix(BL, NL, N);

  matrix_add(AL,BL,CL, NL,N);

  /* The process 0 gather all CL slices from each processes to form the C matrix */
  MPI_Gather(CL,NL*N,MPI_DOUBLE,C,NL*N,MPI_DOUBLE,0,MPI_COMM_WORLD);

  if(rank == 0)
    print_matrix(C, N, N);

  /* Deallocate locals arrays */
  free(AL); free(BL); free(CL);
  MPI_Finalize();
  return 0;
}
