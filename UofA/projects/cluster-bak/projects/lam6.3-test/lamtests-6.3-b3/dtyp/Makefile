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
##	$Id: Makefile,v 1.1 1999/06/14 22:06:52 jsquyres Exp $
##
##	Function:	- Makefile for LAM's MPI test suite
##			- based in large part on the IBM test suite
##			- datatype handling
##

BIN	= bakstr bottom getel lbub lbub2 loop paktest pptransp \
	  strangest1 structsr structsr2 transp transp2 transp3 \
	  transpa zero1 zero2 zero3 zero5 zero6

include ../make-defs

build:	$(BIN)

bakstr: bakstr.o; $(LINK)
bottom: bottom.o; $(LINK)
getel: getel.o; $(LINK)
lbub: lbub.o; $(LINK)
lbub2: lbub2.o; $(LINK)
loop: loop.o; $(LINK)
paktest: paktest.o; $(LINK)
pptransp: pptransp.o; $(LINK)
strangest1: strangest1.o; $(LINK)
structsr: structsr.o; $(LINK)
structsr2: structsr2.o; $(LINK)
transp: transp.o; $(LINK)
transp2: transp2.o; $(LINK)
transp3: transp3.o; $(LINK)
transpa: transpa.o; $(LINK)
zero1: zero1.o; $(LINK)
zero2: zero2.o; $(LINK)
zero3: zero3.o; $(LINK)
zero5: zero5.o; $(LINK)
zero6: zero6.o; $(LINK)
