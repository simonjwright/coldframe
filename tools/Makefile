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

# This Makefile is used to set up the tools and for tool development.

include ../Makefile.inc

GPRBUILD ?= gprbuild

ifeq (, $(XIA))
  xia := $(HOME)/xia
else
  xia := $(XIA)
endif

ADA_PROJECT_PATH := $(xia)

ifneq (, $(CYGWIN))
  ADA_PROJECT_PATH := $(shell cygpath -pm "$(ADA_PROJECT_PATH)")
endif

all::

setup:
	$(GPRBUILD) -p -P tools.gpr

.PHONY: all setup
