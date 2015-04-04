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

all::
setup::
clean::

include ../Makefile.inc

# Setup
# Nothing in this directory

# Set up subdirectories
SETUP_SUBDIRS = stm32f4
setup::
	for s in $(SETUP_SUBDIRS); do		\
	  make -C $$s setup;			\
	done

# Assume that tash.gpr is on the existing ADA_PROJECT_PATH (or
# installed).
ifeq (, $(ADA_PROJECT_PATH))
  export ADA_PROJECT_PATH := ..
else
  ADA_PROJECT_PATH := $(ADA_PROJECT_PATH):..
  ifneq (, $(CYGWIN))
    ADA_PROJECT_PATH := $(shell cygpath -pm "$(ADA_PROJECT_PATH)")
  endif
endif

EXECUTABLES += house_management-harness$(exe) house_management-scripting$(exe)
house_management-harness$(exe): \
  House_Management.gen Digital_IO_Interface.gen force
	gprbuild -p -PHouse_Management_Test
house_management-scripting$(exe): \
  House_Management.gen Digital_IO_Interface.gen force
	gprbuild -p -PHouse_Management_Scripting

EXECUTABLES += interrupt_handling-harness$(exe)
interrupt_handling-harness$(exe): Interrupt_Handling.gen force
	gprbuild -p -PInterrupt_Handling

EXECUTABLES += library_test_harness$(exe)
library_test_harness$(exe): Library.gen force
	gprbuild -p -PLibrary

EXECUTABLES += performance-harness$(exe)
performance-harness$(exe): Performance.gen force
	gprbuild -p -PPerformance

EXECUTABLES += problem_reporting_harness$(exe)
problem_reporting_harness$(exe): Problem_Reporting.gen force
	gprbuild -p -PProblem_Reporting

#EXECUTABLES += serialization_client$(exe)
serialization_client$(exe): serialization_server$(exe)

#EXECUTABLES += serialization_server$(exe)
serialization_server: Serialization.gen \
 Serialization_Demo.gen \
 Serialization_Demo_Other.gen \
 force
	gprbuild -p -PSerialization

EXECUTABLES += states-t$(exe)
states-t$(exe): States.gen force
	gprbuild -p -PStates

EXECUTABLES += stairwell_demo$(exe)
stairwell_demo$(exe):				\
  House_Management.gen				\
  Digital_IO.gen				\
  Digital_IO.Tcl.gen				\
  force
	gprbuild -p -Pstairwell_demo

EXECUTABLES += van_fleet-demo$(exe)
van_fleet-demo$(exe): Van_Fleet.gen
	gprbuild -p -PVan_Fleet

all:: $(EXECUTABLES)

Digital_IO_Interface.gen: Digital_IO_Interface.norm
	$(MAKE) -f $(COLDFRAME)/Makefile.inc $@ \
	  GENERATE_STUBS=yes

# ArgoUML support

NORM_FILES =					\
 Digital_IO.norm				\
 Digital_IO.Tcl.norm				\
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

clean::
	for g in *.gpr; do			\
	  gnatclean -P $$g;			\
	done
	rm -rf *.{norm-stamp,stamp,norm,ada,gen,html,images}

.PHONY: force all setup clean
