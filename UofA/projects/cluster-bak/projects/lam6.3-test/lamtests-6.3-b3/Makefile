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
##	LAM Mini Test Suite
##
##	$Id: Makefile,v 1.1 1999/06/14 22:06:37 jsquyres Exp $
##
##	Function:	- Makefile for LAM's MPI test suite
##			- based in large part on the IBM test suite
##			- grand-daddy Makefile
##

SHELL	= /bin/sh
DIRS	= ccl comm dtyp env group pt2pt topo

DO	= @for dir in $(DIRS) ; do \
		(cd $$dir ; echo "$$dir:" ; make $@) ; \
	  done

default: testing

# Build, run both types of tests

testing:
	$(DO)

# Build only

build:
	$(DO)

# Run lamd tests only

testing-lamd:
	$(DO)

# Run c2c tests only

testing-c2c:
	$(DO)

clean:
	$(DO)

#
# Helper targets to restart the tests
#

ccl-lamd:
	@(cd ccl; echo "ccl:"; $(MAKE) build testing-lamd)

ccl-c2c:
	@(cd ccl; echo "ccl:"; $(MAKE) build testing-c2c)

comm-lamd:
	@(cd comm; echo "comm:"; $(MAKE) build testing-lamd)

comm-c2c:
	@(cd comm; echo "comm:"; $(MAKE) build testing-c2c)

dtyp-lamd:
	@(cd dtyp; echo "dtyp:"; $(MAKE) build testing-lamd)

dtyp-c2c:
	@(cd dtyp; echo "dtyp:"; $(MAKE) build testing-c2c)

env-lamd:
	@(cd env; echo "env:"; $(MAKE) build testing-lamd)

env-c2c:
	@(cd env; echo "env:"; $(MAKE) build testing-c2c)

group-lamd:
	@(cd group; echo "group:"; $(MAKE) build testing-lamd)

group-c2c:
	@(cd group; echo "group:"; $(MAKE) build testing-c2c)

pt2pt-lamd:
	@(cd pt2pt; echo "pt2pt:"; $(MAKE) build testing-lamd)

pt2pt-c2c:
	@(cd pt2pt; echo "pt2pt:"; $(MAKE) build testing-c2c)

topo-lamd:
	@(cd topo; echo "topo:"; $(MAKE) build testing-lamd)

topo-c2c:
	@(cd topo; echo "topo:"; $(MAKE) build testing-c2c)


