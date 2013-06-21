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

# $Id$

# Makefile for examples, use with Cygwin under Windows or Unix
# (assumes a Unix system if Cygwin not detected).

# It's assumed that no project-special settings are in place and that
# the command 'gnatmake' will compile and generate an executable.

# It's assumed that this is still a subdirectory of the ColdFrame
# install directory, and that we have write access (OK on Windows,
# may require admin cooperation on Unix).

all::
clean::

include ../Makefile.inc

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

EXECUTABLES += house_management-harness$(exe)
house_management-harness$(exe): \
  House_Management.gen Digital_IO_Interface.gen force
	$(GNATMAKE) -p -PHouse_Management_Test

EXECUTABLES += interrupt_handling-harness$(exe)
interrupt_handling-harness$(exe): Interrupt_Handling.gen force
	$(GNATMAKE) -p -PInterrupt_Handling

EXECUTABLES += library_test_harness$(exe)
library_test_harness$(exe): Library.gen force
	$(GNATMAKE) -p -PLibrary

EXECUTABLES += performance-harness$(exe)
performance-harness$(exe): Performance.gen force
	$(GNATMAKE) -p -PPerformance

#EXECUTABLES += problem_reporting_harness$(exe)
problem_reporting_harness$(exe): Problem_Reporting.gen force
	$(GNATMAKE) -p -PProblem_Reporting

#EXECUTABLES += serialization_client$(exe)
serialization_client$(exe): serialization_server$(exe)

#EXECUTABLES += serialization_server$(exe)
serialization_server: Serialization.gen \
 Serialization_Demo.gen \
 Serialization_Demo_Other.gen \
 force
	$(GNATMAKE) -p -PSerialization

EXECUTABLES += states-t$(exe)
states-t$(exe): States.gen force
	$(GNATMAKE) -p -PStates

EXECUTABLES += stairwell_demo$(exe)
stairwell_demo$(exe): House_Management.gen Digital_IO.gen force
	$(GNATMAKE) -p -Pstairwell_demo

EXECUTABLES += van_fleet-demo$(exe)
van_fleet-demo$(exe): Van_Fleet.gen
	$(GNATMAKE) -p -PVan_Fleet

all:: $(EXECUTABLES)

Digital_IO_Interface.gen: Digital_IO_Interface.norm
	$(MAKE) -f $(COLDFRAME)/Makefile.inc $@ \
	  GENERATE_STUBS=yes

# ArgoUML support

NORM_FILES =					\
 Digital_IO.norm				\
 Digital_IO_Interface.norm			\
 House_Management.norm				\
 Interrupt_Handling.norm			\
 Library.norm					\
 Performance.norm				\
 States.norm					\
 Van_Fleet.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(NORM_FILES): ColdFrame_Examples.norm-stamp ;

clean::
	find $(BUILD_BASE) -type f -name \*.o -o -name \*.ali | xargs rm -f
	rm -f $(EXECUTABLES)
	rm -rf *.{raw,norm,ada,gen}
	rm -f parser.out parsetab.py

.PHONY: force all clean
