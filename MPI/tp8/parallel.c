#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <mpi.h>

#include "parallel.h"
#include "params.h"
#include "compute.h"

#define ndims 2 /* grid dimension */
#define NB_NEIGHBOURS 4
#define N 0
#define E 1
#define S 2
#define W 3

/* Local Sub-Domain rank */
int rank;
/* Number of processes */
static int size;
/* Number of processes in each dimension for the Cartesian topology */
static int dims[ndims];
/* Communicator of the Cartesian topology */
static MPI_Comm comm2d;
/* Array storing the rank of neighbours */
static int neighbour[NB_NEIGHBOURS];
/* Derived datatypes */
static MPI_Datatype type_column, type_line;
int ntx, nty;
int sx, ex, sy, ey;

/*
 * Initialization of the MPI environnement
 */
void env_init(int argc, char* argv[]) {
  /* MPI initialization */

  /* Who I am */

  /* Total number of processes */

}

/*
 * Creation of the Cartesian topology
 */
void topology_init() {
  FILE *file;
  int periods[ndims];
  const int reorganisation = false;

  /* Read ntx and nty in the file poisson.data */
  file = fopen("poisson.data", "r");
  fscanf(file, "%d", &ntx);
  fscanf(file, "%d", &nty);
  fclose(file);

  /* Number of processes on each dimension (depends on the total number of processes) */


  /* Creation of the 2D Cartesian topology (no periodicity) */

  if (rank == 0) {
    printf("Execution poisson with %d MPI processes\n"
           "Size of the domain: ntx=%d nty=%d\n"
           "Dimension for the topology: %d along x, %d along y\n"
           "-----------------------------------------\n", 
           size, ntx, nty, dims[0], dims[1]);
  }
}

/*
 * Computation of the local grid boundary coordinates (global indexes)
 */
void domain_boundaries() {
  int coords[ndims];
  /* What is my coordinates in the topology */

  /* X-axis limits */
  sx = (coords[0]*ntx)/dims[0]+1;
  ex = ((coords[0]+1)*ntx)/dims[0];

  /* Y-axis limits */
  sy = (coords[1]*nty)/dims[1]+1;
  ey = ((coords[1]+1)*nty)/dims[1];

  printf("Rank in the topology: %d Local Grid Index: %d to %d along x, "
         "%d to %d along y\n", rank, sx, ex, sy, ey);
}

/*
 * Neighbours
 */
void domain_neighbours() {
  /* Get my northern and southern neighbours */

  /* Get my western and eastern neighbours */

  printf("Process %d neighbour: N %d E %d S %d W %d\n", 
         rank, neighbour[N], neighbour[E], neighbour[S], neighbour[W]);
}

/*
 * Creation of the derived datatypes needed to exchange points with neighbours
 */
void derived_datatypes() {
  /* Creation of the type_line derived datatype to exchange points
     with northern to southern neighbours */


  /* Creation of the type_column derived datatype to exchange points
     with western to eastern neighbours */

}

/*
 * IDX(i, j) : indice de l'element i, j dans le tableau u
 * sx-1 <= i <= ex+1
 * sy-1 <= j <= ey+1
 */
#define IDX(i, j) ( ((i)-(sx-1))*(ey-sy+3) + (j)-(sy-1) )
/*
 * Exchange the points at the interface
 */
void communications(double *u) {
  const int tag = 100;
  MPI_Status status;

  /* Send to neighbour N and receive from neighbour S */

  /* Send to neighbour S and receive from neighbour N */

  /* Send to neighbour W and receive from neighbour E */

  /* Send to neighbour E  and receive from neighbour W */

}

/*
 * Compute the global error (maximum of the locals errors)
 */
double global_error(double *u, double *u_new) {
  double local_error, diffnorm;
  int iterx, itery;

  local_error = 0;
  for (iterx=sx; iterx<ex+1; iterx++) {
    for (itery=sy; itery<ey+1; itery++) {
      double temp = fabs( u[IDX(iterx, itery)] - u_new[IDX(iterx, itery)] );
      if (local_error < temp) local_error = temp;
    }
  }

  /* Computation of global error */

  return diffnorm;
}

/*
 * Write array u inside a domain for each process in the data.dat file
 */
void mpi_write(double *u) {
  int code;
  MPI_File fh;
  int shape_array[ndims], shape_subarray[ndims], start_coord[ndims];
  int shape_array_view[ndims], shape_subarray_view[ndims], start_coord_view[ndims];
  MPI_Datatype type_subarray, type_subarray_view;
  MPI_Offset initial_displacement;
  MPI_Status status;

  /*
   * Open file "data.dat" in write mode
   */

  /* Error checking */
  if (code != MPI_SUCCESS) {
    fprintf(stderr, "Error opening the file");
    MPI_Abort(comm2d, 2);
  }

  /*
   * Change the file view
   */


  /*
   * Creation of the derived datatype type_subarray corresponding to the matrix u without ghost cells
   */


  /*
   * Write u for each process with the view *
   */


  /*
   * Close file
   */

  /*
  * Clean MPI types
  */

}

/*
 * Terminates MPI execution environment
 */
void env_finalize() {
  /* Clean MPI objects */

  /* Terminates MPI execution environment */

}
