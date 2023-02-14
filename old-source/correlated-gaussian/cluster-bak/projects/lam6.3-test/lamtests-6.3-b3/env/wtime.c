/*
 *	LAM
 *	Copyright 1999 University of Notre Dame
 *	GDB
 *
 *	$Id: wtime.c,v 1.3 1999/08/31 21:34:39 jsquyres Exp $
 *
 *	Function:	- tests MPI_Wtime() and MPI_Wtick()
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <mpi.h>

int
main(argc, argv)

int			argc;
char			**argv;

{
	double		time, delta, min;
	double		tick1, tick2;
	int		i, rank;

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	if (rank == 0)  {
		sleep(1);

		tick1 = MPI_Wtick();

		for (i = 0; i < 100; ++i) {
			tick2 = MPI_Wtick();
			if ((tick2 - tick1) > 1e-06) {
				printf("wtick variation: %10.10f, %10.10f\n",
					tick1, tick2);
				break;
			}
		}

		min = -1;

		for (i = 0; i < 100; ++i) {
			time = MPI_Wtime();
			while ((delta = MPI_Wtime() - time) <= 0) ;

			if ((min < 0) || (min > delta)) min = delta;
		}

		printf("resolution = %10.10f, wtick = %10.10f\n",
			min, MPI_Wtick());
	}

	MPI_Finalize();
	return(0);
}
