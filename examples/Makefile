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

# Makefile for tests, use with Cygwin under Windows or Unix (assumes a
# Unix system if Cygwin not detected).

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
app = ADA_PROJECT_PATH=$$ADA_PROJECT_PATH:$(COLDFRAME)

EXECUTABLES += house_management-harness$(exe)
house_management-harness$(exe): \
  House_Management.gen Digital_IO_Interface.gen force
	$(app) gnatmake -p -PHouse_Management_Test

EXECUTABLES += interrupt_handling-harness$(exe)
interrupt_handling-harness$(exe): Interrupt_Handling.gen force
	$(app) gnatmake -p -PInterrupt_Handling

EXECUTABLES += library_test_harness$(exe)
library_test_harness$(exe): Library.gen force
	$(app) gnatmake -p -PLibrary

EXECUTABLES += performance_harness$(exe)
performance_harness$(exe): Performance.gen force
	$(app) gnatmake -p -PPerformance

EXECUTABLES += problem_reporting_harness$(exe)
problem_reporting_harness$(exe): Problem_Reporting.gen force
	$(app) gnatmake -p -PProblem_Reporting

EXECUTABLES += serialization_client$(exe)
serialization_client$(exe): serialization_server$(exe)

EXECUTABLES += serialization_server$(exe)
serialization_server: Serialization.gen \
 Serialization_Demo.gen \
 Serialization_Demo_Other.gen \
 force
	$(app) gnatmake -p -PSerialization

EXECUTABLES += states-t$(exe)
states-t$(exe): States.gen force
	$(app) gnatmake -p -PStates

EXECUTABLES += stairwell_demo$(exe)
stairwell_demo$(exe): House_Management.gen Digital_IO.gen force
	$(app) gnatmake -p -Pstairwell_demo

EXECUTABLES += van_fleet-demo$(exe)
van_fleet-demo$(exe): Van_Fleet.gen
	$(app) gnatmake -p -PVan_Fleet

all:: $(EXECUTABLES)

Digital_IO_Interface.gen: $(COLDFRAMEOUT)/Digital_IO_Interface.raw
	$(MAKE) -f $(COLDFRAME)/Makefile.inc $@ \
	  DOMAIN_NAME=Digital_IO \
	  GENERATE_STUBS=yes

# ArgoUML support

NORM_FILES = Library.norm Van_Fleet.norm

$(NORM_FILES): ColdFrame_Examples.norm-stamp

clean::
	find $(BUILD_BASE) -type f -name \*.o -o -name \*.ali | xargs rm -f
	rm -f $(EXECUTABLES)
	rm -rf *.{raw,norm,ada,gen}
	rm -f parser.out parsetab.py

.PHONY: force all clean
