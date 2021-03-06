#  Copyright (C) Simon Wright <simon@pushface.org>

#  This package is free software; you can redistribute it and/or
#  modify it under terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 2, or
#  (at your option) any later version. This package is distributed in
#  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
#  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE. See the GNU General Public License for more
#  details. You should have received a copy of the GNU General Public
#  License distributed with this package; see file COPYING.  If not,
#  write to the Free Software Foundation, 59 Temple Place - Suite
#  330, Boston, MA 02111-1307, USA.

# Makefile for examples, use with Cygwin under Windows or Unix
# (assumes a Unix system if Cygwin not detected).

# It's assumed that no project-special settings are in place and that
# the command 'gprbuild' will compile and generate an executable.

# It's assumed that this is still a subdirectory of the ColdFrame
# install directory, and that we have write access (OK on Windows,
# may require admin cooperation on Unix).

GPRBUILD ?= gprbuild
GPRCLEAN ?= gprclean

all::
setup::
clean::

include ../Makefile.inc

# Setup
# Nothing in this directory

# Set up subdirectories
SETUP_SUBDIRS = arduino microbit stm32f4
setup::
	for s in $(SETUP_SUBDIRS); do		\
	  make -C $$s setup;			\
	done

ifeq (, $(TASH))
  tash := $(HOME)/tcladashell
else
  tash := $(TASH)
endif

ifeq (, $(SCRIPTED_TESTING))
  scripted_testing := $(HOME)/scripted_testing
else
  scripted_testing := $(SCRIPTED_TESTING)
endif

ADA_PROJECT_PATH := $(scripted_testing):$(tash):$(ADA_PROJECT_PATH)

ifneq ($(filter CYGWIN%, $(shell uname -s)), )
  ADA_PROJECT_PATH := $(shell cygpath -pm "$(ADA_PROJECT_PATH)")
endif

export ADA_PROJECT_PATH

#EXECUTABLES += house_management-harness$(EXE)
house_management-harness$(EXE): \
  House_Management.gen Digital_IO_Interface.gen force
	$(GPRBUILD) -p -PHouse_Management_Test

EXECUTABLES += house_management-scripting$(EXE)
house_management-scripting$(EXE): \
  House_Management.gen Digital_IO_Interface.gen force
	$(GPRBUILD) -p -PHouse_Management_Scripting

EXECUTABLES += interrupt_handling-harness$(EXE)
interrupt_handling-harness$(EXE): Interrupt_Handling.gen force
	$(GPRBUILD) -p -PInterrupt_Handling

EXECUTABLES += library_test_harness$(EXE)
library_test_harness$(EXE): Library.gen force
	$(GPRBUILD) -p -PLibrary

EXECUTABLES += performance-harness$(EXE)
performance-harness$(EXE): Performance.gen force
	$(GPRBUILD) -p -PPerformance

EXECUTABLES += problem_reporting_harness$(EXE)
problem_reporting_harness$(EXE): Problem_Reporting.gen force
	$(GPRBUILD) -p -PProblem_Reporting

EXECUTABLES += simple_buttons-scripting$(EXE)
simple_buttons-scripting$(EXE): \
  Simple_Buttons.gen Digital_IO_Interface.gen force
	$(GPRBUILD) -p -PSimple_Buttons_Scripting

EXECUTABLES += states-t$(EXE)
states-t$(EXE): States.gen force
	$(GPRBUILD) -p -PStates

EXECUTABLES += stairwell_demo$(EXE)
stairwell_demo$(EXE):				\
  House_Management.gen				\
  Digital_IO.gen				\
  Digital_IO.Tcl.gen				\
  force
	$(GPRBUILD) -p -Pstairwell_demo

EXECUTABLES += van_fleet-demo$(EXE)
van_fleet-demo$(EXE): Van_Fleet.gen
	$(GPRBUILD) -p -PVan_Fleet

all:: $(EXECUTABLES)

Digital_IO_Interface.gen: Digital_IO_Interface.norm
	$(MAKE) -f $(COLDFRAME)/Makefile.inc $@ \
	  GENERATE_STUBS=yes

# ArgoUML support

NORM_FILES =					\
 Digital_IO.Tcl.norm				\
 Digital_IO.norm				\
 Digital_IO_Interface.norm			\
 House_Management.norm				\
 Interrupt_Handling.norm			\
 Library.norm					\
 Performance.norm				\
 Problem_Reporting.norm				\
 States.norm					\
 Van_Fleet.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(NORM_FILES): ColdFrame_Examples.norm-stamp ;

TUML_NORM_FILES =				\
Simple_Buttons.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(TUML_NORM_FILES): Simple_Buttons.tuml-norm-stamp ;

clean::
	-for g in *.gpr; do			\
	  $(GPRCLEAN) -P $$g;			\
	done
	for s in $(SETUP_SUBDIRS); do		\
	  make -C $$s clean;			\
	done
	-rm -rf *.{norm,ada,gen,html,images} *-stamp

.PHONY: force all setup clean
