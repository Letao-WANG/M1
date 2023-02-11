/*
 * read_file.c --- TP7 MPI
 *
 * Author          : Dimitri LECAS (CNRS/IDRIS - France) <dimitri.lecas@idris.fr>
 *
 */

#include <mpi.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char * argv[]) {
  const int nb_values=121;
  int values[nb_values];
  int rank,iter;
  MPI_File fh;
  int nb_bytes_integer;
  MPI_Offset offset;
  MPI_Status status;
  char name_file[256];
  FILE * file;

  MPI_Init( &argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);

  /* Open donnees.dat in read mode */




  /* Read via explicit offsets, in individual mode */
  for (iter=0;iter<nb_values; iter++) values[iter]=0;


  sprintf(name_file,"file_dei%1d.dat",rank);
  file = fopen(name_file,"w");
  for (iter=0; iter<nb_values; iter++)
    fprintf(file,"%3d\n",values[iter]);
  fclose(file);


  /* Read via shared ifle pointers, in collective mode */
  for (iter=0;iter<nb_values; iter++) values[iter]=0;


  sprintf(name_file,"file_ppc%1d.dat",rank);
  file = fopen(name_file,"w");
  for (iter=0; iter<nb_values; iter++)
    fprintf(file,"%3d\n",values[iter]);
  fclose(file);


 /* Read via individual file pointer, in individual mode */
  for (iter=0;iter<nb_values; iter++) values[iter]=0;


  sprintf(name_file,"file_pii%1d.dat",rank);
  file = fopen(name_file,"w");
  for (iter=0; iter<nb_values; iter++)
    fprintf(file,"%3d\n",values[iter]);
  fclose(file);


  /* Read via shared file pointers, in individual mode */
  for (iter=0;iter<nb_values; iter++) values[iter]=0;


  sprintf(name_file,"file_ppi%1d.dat",rank);
  file = fopen(name_file,"w");
  for (iter=0; iter<nb_values; iter++)
    fprintf(file,"%3d\n",values[iter]);
  fclose(file);

  /* Close the file */


  MPI_Finalize();
  return 0;
}
