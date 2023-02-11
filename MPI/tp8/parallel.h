#ifndef PARALLEL_H
#define PARALLEL_H

extern int rank;

void env_init( int, char **);
void topology_init();
void domain_boundaries();
void domain_neighbours();
void derived_datatypes();
void communications(double *);
double global_error(double *, double *);
void mpi_write(double *);
void env_finalize();

#endif
