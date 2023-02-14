# Generated automatically from Makefile.in by configure.
##
# Copyright 1998-1999, University of Notre Dame.
# Authors: Jeffrey M. Squyres, Kinis L. Meyer, with M. D. McNally 
#          and Andrew Lumsdaine
#
# This file is part of the Notre Dame LAM implementation of MPI.
#
# You should have received a copy of the License Agreement for the
# Notre Dame LAM implementation of MPI along with the software; see
# the file LICENSE.  If not, contact Office of Research, University of
# Notre Dame, Notre Dame, IN 46556.
#
# Permission to modify the code and to distribute modified code is
# granted, provided the text of this NOTICE is retained, a notice that
# the code was modified is included with the above COPYRIGHT NOTICE and
# with the COPYRIGHT NOTICE in the LICENSE file, and that the LICENSE
# file is distributed with the modified code.
#
# LICENSOR MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR IMPLIED.
# By way of example, but not limitation, Licensor MAKES NO
# REPRESENTATIONS OR WARRANTIES OF MERCHANTABILITY OR FITNESS FOR ANY
# PARTICULAR PURPOSE OR THAT THE USE OF THE LICENSED SOFTWARE COMPONENTS
# OR DOCUMENTATION WILL NOT INFRINGE ANY PATENTS, COPYRIGHTS, TRADEMARKS
# OR OTHER RIGHTS.
#
# Additional copyrights may follow.
#
##	Ohio LAM
##
##	Function:	- Mandelbozo demo
##			- uses MPI
##

include ../config-defs

all: trivialf trivialc ezstart

trivialf: trivial.f
	$(F77) $(FFLAGS) -o trivialf trivial.f $(LDFLAGS)

trivialc: trivial.c
	$(CC) $(CFLAGS) -o trivialc trivial.c $(LDFLAGS)

ezstart: ezstart.o
	$(CC) -o ezstart ezstart.o $(LDFLAGS)

clean:
	$(RM) trivialf trivialc ezstart.o ezstart
