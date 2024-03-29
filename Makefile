# Copyright (C) Simon Wright <simon@pushface.org>

# This package is free software; you can redistribute it and/or
# modify it under terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version. This package is distributed in
# the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU General Public License for more
# details. You should have received a copy of the GNU General Public
# License distributed with this package; see file COPYING.  If not,
# write to the Free Software Foundation, 59 Temple Place - Suite
# 330, Boston, MA 02111-1307, USA.

GPRBUILD ?= gprbuild

all::
	@echo "Say 'make setup' if you wish to set ColdFrame up."

# See Makefile.inc for overridable variable definitions.
include Makefile.inc

# Set up the library
setup:: force
	$(GPRBUILD) -p -P ColdFrame

# Set up subdirectories
SETUP_SUBDIRS = src lib tools examples
setup::
	for s in $(SETUP_SUBDIRS); do		\
	  make -C $$s -w setup;			\
	done

.PHONY: all force setup
