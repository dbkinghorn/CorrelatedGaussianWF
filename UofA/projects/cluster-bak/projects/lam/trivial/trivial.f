c
c Copyright 1998-1999, University of Notre Dame.
c Authors: Jeffrey M. Squyres, Kinis L. Meyer with M. D. McNally 
c          and Andrew Lumsdaine
c
c This file is part of the Notre Dame LAM implementation of MPI.
c
c You should have received a copy of the License Agreement for the
c Notre Dame LAM implementation of MPI along with the software; see
c the file LICENSE.  If not, contact Office of Research, University
c of Notre Dame, Notre Dame, IN 46556.
c
c Permission to modify the code and to distribute modified code is
c granted, provided the text of this NOTICE is retained, a notice that
c the code was modified is included with the above COPYRIGHT NOTICE and
c with the COPYRIGHT NOTICE in the LICENSE file, and that the LICENSE
c file is distributed with the modified code.
c
c LICENSOR MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR IMPLIED.
c By way of example, but not limitation, Licensor MAKES NO
c REPRESENTATIONS OR WARRANTIES OF MERCHANTABILITY OR FITNESS FOR ANY
c PARTICULAR PURPOSE OR THAT THE USE OF THE LICENSED SOFTWARE COMPONENTS
c OR DOCUMENTATION WILL NOT INFRINGE ANY PATENTS, COPYRIGHTS, TRADEMARKS
c OR OTHER RIGHTS.  
c
c Additional copyrights may follow.
c
c
c	University of Notre Dame LAM
c	$Id: trivial.f,v 6.4 1999/07/09 04:38:32 jsquyres Exp $
c
c	Transmit a message in a two process system.
c
	program trivial

        include 'mpif.h'

	integer*4 BUFSIZE

	parameter (BUFSIZE = 64)

	integer*4	buffer(BUFSIZE)
	integer		rank
	integer		size
	integer 	status(MPI_STATUS_SIZE)

c
c Initialize MPI.
c
	call MPI_INIT(ierror)
c
c Error check the number of processes.
c Determine my rank in the world group.
c The sender will be rank 0 and the receiver, rank 1.
c
	call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

	if (size .lt. 2) then
		print *,'Need at least 2 processes.'
		call MPI_FINALIZE(ierror)
		stop
	endif

	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
c
c As rank 0, send a message to rank 1.
c
	if (rank .eq. 0) then
		call MPI_SEND(buffer(1), BUFSIZE, MPI_INTEGER,
     +				1, 11, MPI_COMM_WORLD, ierror)
		print *,'rank ',rank,' sent message'
c
c As rank 1, receive a message from rank 0.
c
	else if (rank .eq. 1) then
		call MPI_RECV(buffer(1), BUFSIZE, MPI_INTEGER,
     +				0, 11, MPI_COMM_WORLD, status, ierror)      
		print *,'rank ',rank,' received message'
	endif

	call MPI_FINALIZE(ierror)
	stop
	end
