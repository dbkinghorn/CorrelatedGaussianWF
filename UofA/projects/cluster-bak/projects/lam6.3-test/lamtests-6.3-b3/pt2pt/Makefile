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
##	$Id: Makefile,v 1.1 1999/06/14 22:07:02 jsquyres Exp $
##
##	Function:	- Makefile for LAM's MPI test suite
##			- based in large part on the IBM test suite
##			- point-to-point routines
##

BIN	= badbuf bsend bsend_free buffer free getcount interf iprobe \
	  isend probe rsend rsend2 sendrecv sendrecv_rep seq ssend \
	  start startall test1 test2 test3 testall testany testsome \
	  waitall waitany waitnull waitsome wildcard

include ../make-defs

build:	$(BIN)

badbuf: badbuf.o; $(LINK)
bsend: bsend.o; $(LINK)
bsend_free: bsend_free.o; $(LINK)
buffer: buffer.o; $(LINK)
cancel1: cancel1.o; $(LINK)
cancel2: cancel2.o; $(LINK)
cancel3: cancel3.o; $(LINK)
free: free.o; $(LINK)
getcount: getcount.o; $(LINK)
interf: interf.o; $(LINK)
iprobe: iprobe.o; $(LINK)
isend: isend.o; $(LINK)
probe: probe.o; $(LINK)
rsend: rsend.o; $(LINK)
rsend2: rsend2.o; $(LINK)
sendrecv: sendrecv.o; $(LINK)
sendrecv_rep: sendrecv_rep.o; $(LINK)
seq: seq.o; $(LINK)
ssend: ssend.o; $(LINK)
start: start.o; $(LINK)
startall: startall.o; $(LINK)
test1: test1.o; $(LINK)
test2: test2.o; $(LINK)
test3: test3.o; $(LINK)
testall: testall.o; $(LINK)
testany: testany.o; $(LINK)
testsome: testsome.o; $(LINK)
waitall: waitall.o; $(LINK)
waitany: waitany.o; $(LINK)
waitnull: waitnull.o; $(LINK)
waitsome: waitsome.o; $(LINK)
wildcard: wildcard.o; $(LINK)
