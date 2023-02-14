/*
 *
 * Copyright 1998-1999, University of Notre Dame.
 * Authors: Jeffrey M. Squyres, Kinis L. Meyer with M. D. McNally 
 *          and Andrew Lumsdaine
 *
 * This file is part of the Notre Dame LAM implementation of MPI.
 *
 * You should have received a copy of the License Agreement for the
 * Notre Dame LAM implementation of MPI along with the software; see
 * the file LICENSE.  If not, contact Office of Research, University
 * of Notre Dame, Notre Dame, IN 46556.
 *
 * Permission to modify the code and to distribute modified code is
 * granted, provided the text of this NOTICE is retained, a notice that
 * the code was modified is included with the above COPYRIGHT NOTICE and
 * with the COPYRIGHT NOTICE in the LICENSE file, and that the LICENSE
 * file is distributed with the modified code.
 *
 * LICENSOR MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR IMPLIED.
 * By way of example, but not limitation, Licensor MAKES NO
 * REPRESENTATIONS OR WARRANTIES OF MERCHANTABILITY OR FITNESS FOR ANY
 * PARTICULAR PURPOSE OR THAT THE USE OF THE LICENSED SOFTWARE COMPONENTS
 * OR DOCUMENTATION WILL NOT INFRINGE ANY PATENTS, COPYRIGHTS, TRADEMARKS
 * OR OTHER RIGHTS.  
 *
 * Additional copyrights may follow.
 *
 *
 *	University of Notre Dame LAM
 *	$Id: trivial.c,v 6.4 1999/07/09 04:38:32 jsquyres Exp $
 *
 *	Transmit a message in a two process system.
 */

#include <stdio.h>
#include <mpi.h>

#define BUFSIZE		64

int			buf[BUFSIZE];

int
main(argc, argv)

int			argc;
char			*argv[];

{
	int		size, rank;
	MPI_Status	status;
/*
 * Initialize MPI.
 */
	MPI_Init(&argc, &argv);
/*
 * Error check the number of processes.
 * Determine my rank in the world group.
 * The sender will be rank 0 and the receiver, rank 1.
 */
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	if (size < 2) {
		printf("Need at least 2 processes.\n");
		MPI_Finalize();
		return(1);
	}

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
/*
 * As rank 0, send a message to rank 1.
 */
	if (0 == rank) {
		MPI_Send(buf, BUFSIZE, MPI_INT, 1, 11, MPI_COMM_WORLD);
		printf("rank %d sent message\n", rank);
	}
/*
 * As rank 1, receive a message from rank 0.
 */
	else if (1 == rank) {
		MPI_Recv(buf, BUFSIZE, MPI_INT, 0, 11, MPI_COMM_WORLD,
				&status);
		printf("rank %d received message\n", rank);
	}

	MPI_Finalize();
	return(0);
}
