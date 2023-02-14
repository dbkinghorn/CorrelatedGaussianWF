/*
 *	LAM
 *	Copyright 1999 University of Notre Dame
 *	GDB
 *
 *	$Id: ssend.c,v 1.1 1999/06/14 22:07:07 jsquyres Exp $
 *
 *	Function:	- tests synchonicity of MPI_Ssend between two ranks
 */

#include <stdio.h>
#include <unistd.h>
#include <mpi.h>

#define WAIT_SECONDS	10			/* # seconds wait-time */


int
main(argc, argv)

int			argc;
char			**argv;

{
	MPI_Status	stat;
	int		rank;
	int		flag, junk;
	double		time;

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	if (rank == 0) {
		MPI_Recv(&junk, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &stat);
		MPI_Ssend(&rank, 1, MPI_INT, 1, 1, MPI_COMM_WORLD);
		MPI_Send(&rank, 1, MPI_INT, 1, 2, MPI_COMM_WORLD);
	}
	else if (rank == 1) {
		MPI_Send(&rank, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);

		time = MPI_Wtime();
		do {
			MPI_Iprobe(0, 2, MPI_COMM_WORLD, &flag, &stat);
			if (flag) break;
			sleep(1);
		} while ((MPI_Wtime() - time) < WAIT_SECONDS);

		MPI_Recv(&junk, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, &stat);
		MPI_Recv(&junk, 1, MPI_INT, 0, 2, MPI_COMM_WORLD, &stat);

		if (flag) {
			printf("ERROR: MPI_Ssend did not synchronize: "
				"tag 2 received before tag 1\n");
		}
	}

	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Finalize();
	return(0);
}
