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

# Makefile for tests, use with Cygwin under Windows or Unix (assumes a
# Unix system if Cygwin not detected).

# It's assumed that no project-special settings are in place and that
# the command 'gnatmake' will compile and generate an executable.

# It's assumed that this is still a subdirectory of the ColdFrame
# install directory, and that we have write access (OK on Windows,
# may require admin cooperation on Unix).

all::

include ../Makefile.inc

app = ADA_PROJECT_PATH=$$ADA_PROJECT_PATH:$(COLDFRAME)

all:: build-dirs runs fails

runs:: event_test-harness$(exe)
	./$< -v 2>&1 | tee -a event_test.log

runs:: hierarchies-harness$(exe)
	./$< -v 2>&1 | tee -a hierarchies.log

runs:: initialization-harness$(exe)
	./$< -v 2>&1 | tee -a initialization.log

runs:: regression_tests$(exe)
	./$< -v 2>&1 | tee -a regressions.log

runs:: stub_test_harness$(exe)
	./$< -v 2>&1 | tee -a stub_test.log

runs:: unit_testing-harness$(exe)
	./$< -v 2>&1 | tee -a unit_testing.log

fails:: Bad_Model_Regressions.stamp-regardless

fails:: Bad_Code_Regressions.stamp-regardless

event_test-harness$(exe): Event_Test.stamp force
	$(app) gnatmake -PEvent_Test 2>&1 | tee -a Event_Test.log
exes:: event_test-harness$(exe)
clean::
	-$(app) gnatclean -PEvent_Test

hierarchies-harness$(exe): Hierarchies.stamp force
	$(app) gnatmake -PHierarchies 2>&1 | tee -a Hierarchies.log
exes:: hierarchies-harness$(exe)
clean::
	-$(app) gnatclean -PHierarchies

initialization-harness$(exe): Initialization.stamp force
	$(app) gnatmake -PInitialization 2>&1 | tee -a Initialization.log
exes:: initialization-harness$(exe)
clean::
	-$(app) gnatclean -PInitialization

regression_tests$(exe): Regressions.stamp Compilation_Regressions.stamp force
	$(app) gnatmake -PRegressions 2>&1 | tee -a Regressions.log
exes:: regression_tests$(exe)
clean::
	-$(app) gnatclean -PRegressions

stub_test_harness$(exe): Stub_Test_Interface.stamp force
	$(app) gnatmake -PStub_Test 2>&1 | tee -a Stub_Test.log
Stub_Test_Interface.stamp: Stub_Test_Interface.norm
	$(MAKE) $(basename $@).gen GENERATE_STUBS=yes 2>&1 \
	  | tee $(basename $@).log
	touch $@
exes:: stub_test_harness$(exe)
clean::
	-$(app) gnatclean -PStub_Test

unit_testing-harness$(exe): Unit_Testing.stamp force
	$(app) gnatmake -PUnit_Testing 2>&1 | tee -a Unit_Testing.log
Unit_Testing.stamp: Unit_Testing.norm
	$(MAKE) $(basename $@).gen UNIT_TEST_SUPPORT=yes 2>&1 \
	  | tee $(basename $@).log
	touch $@
exes:: unit_testing-harness$(exe)
clean::
	-$(app) gnatclean -PUnit_Testing

%.stamp: %.norm
	$(MAKE) $(basename $@).gen 2>&1 | tee $(basename $@).log
	touch $@

%.stamp-regardless: $(COLDFRAMEOUT)/%.raw
	-$(MAKE) $(basename $@).gen 2>&1 | tee $(basename $@).log
	touch $@

# ArgoUML support

NORM_FILES =					\
  Stub_Test_Interface.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(NORM_FILES): ColdFrame_Tests.norm-stamp ;

clean::
	-rm -rf *.{ada,gen,log,norm,stamp*}

.PHONY: all force clean
