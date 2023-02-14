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
##	$Id: Makefile,v 1.1 1999/06/14 22:06:58 jsquyres Exp $
##
##	Function:	- Makefile for LAM's MPI test suite
##			- based in large part on the IBM test suite
##			- environment routines
##

BIN	= abort err final initialized pcontrol procname wtime

include ../make-defs

build:	$(BIN)

abort: abort.o; $(LINK)
err: err.o; $(LINK)
errstr: errstr.o; $(LINK)
final: final.o; $(LINK)
mpiinit: mpiinit.o; $(LINK)
initialized: initialized.o; $(LINK)
pcontrol: pcontrol.o; $(LINK)
procname: procname.o; $(LINK)
wtime: wtime.o; $(LINK)
