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
##
##	$Id: Makefile,v 1.1 1999/06/14 22:06:44 jsquyres Exp $
##
##	Function:	- Makefile for LAM's MPI test suite
##			- based in large part on the IBM test suite
##			- collective routines
##

BIN	= allgather allreduce alltoall barrier bcast bcast_struct \
	  gather op reduce reduce_scatter scan scatter struct_gatherv

include ../make-defs

build: $(BIN)

allgather: allgather.o; $(LINK)
allreduce: allreduce.o; $(LINK)
alltoall: alltoall.o; $(LINK)
barrier: barrier.o; $(LINK)
bcast: bcast.o; $(LINK)
bcast_struct: bcast_struct.o; $(LINK)
gather: gather.o; $(LINK)
op: op.o; $(LINK)
reduce: reduce.o; $(LINK)
reduce_scatter: reduce_scatter.o; $(LINK)
scan: scan.o; $(LINK)
scatter: scatter.o; $(LINK)
struct_gatherv: struct_gatherv.o; $(LINK)
