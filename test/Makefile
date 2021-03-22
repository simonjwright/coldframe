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
# the command 'gprbuild' will compile and generate an executable.

# It's assumed that this is still a subdirectory of the ColdFrame
# install directory, and that we have write access (OK on Windows,
# may require admin cooperation on Unix).

GPRBUILD ?= gprbuild

all::

include ../Makefile.inc

ifeq (, $(ADA_PROJECT_PATH))
  export ADA_PROJECT_PATH := ..
else
  ADA_PROJECT_PATH := $(ADA_PROJECT_PATH):..
  ifneq (, $(CYGWIN))
    ADA_PROJECT_PATH := $(shell cygpath -pm "$(ADA_PROJECT_PATH)")
  endif
endif

all:: runs fails

# In case no actual targets.
runs::
fails::

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

event_test-harness$(exe): Event_Test.gen force
	$(GPRBUILD) -p -PEvent_Test 2>&1 | tee -a Event_Test.log
exes:: event_test-harness$(exe)
clean::
	-gnatclean -PEvent_Test

hierarchies-harness$(exe): Hierarchies.gen force
	$(GPRBUILD) -p -PHierarchies 2>&1 | tee -a Hierarchies.log
exes:: hierarchies-harness$(exe)
clean::
	-gnatclean -PHierarchies

initialization-harness$(exe): Initialization.gen force
	$(GPRBUILD) -p -PInitialization 2>&1 | tee -a Initialization.log
exes:: initialization-harness$(exe)
clean::
	-gnatclean -PInitialization

stub_test_harness$(exe): Stub_Test_Interface.stub force
	$(GPRBUILD) -p -PStub_Test 2>&1 | tee -a Stub_Test.log
exes:: stub_test_harness$(exe)
clean::
	-gnatclean -PStub_Test

unit_testing-harness$(exe): Unit_Testing.unit force
	$(GPRBUILD) -p -PUnit_Testing 2>&1 | tee -a Unit_Testing.log
exes:: unit_testing-harness$(exe)
clean::
	-gnatclean -PUnit_Testing

%.unit: %.norm
	$(MAKE) $(basename $@).gen UNIT_TEST_SUPPORT=yes 2>&1 \
	  | tee $(basename $@).log
	touch $@

%.stub: %.norm
	$(MAKE) $(basename $@).gen GENERATE_STUBS=yes 2>&1 \
	  | tee $(basename $@).log
	touch $@

# ArgoUML support

NORM_FILES =					\
  Event_Test.norm				\
  Hierarchies.norm				\
  Initialization.norm				\
  Stub_Test_Interface.norm			\
  Unit_Testing.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(NORM_FILES): ColdFrame_Tests.norm-stamp ;

# Regression tests (by which I mean, tests related to bug reports on
# Sourceforge/Github)

# Some of the domains are in the ColdFrame_Development model.
DEVELOPMENT_DOMAINS =				\
  Associations					\
  Association_Classes				\
  Ignored_Child_Packages			\
  Reflexive_Associations			\
  Types

# Others (more regression-specific) in the Regressions model.
REGRESSION_DOMAINS =				\
  SF_2991721					\
  SF_Bug_79					\
  Strings

DEVELOPMENT_NORMS = $(addsuffix .norm,$(DEVELOPMENT_DOMAINS))
$(DEVELOPMENT_NORMS): ColdFrame_Development.norm-stamp ;
DEVELOPMENT_GENS = $(addsuffix .gen,$(DEVELOPMENT_DOMAINS))

REGRESSION_NORMS = $(addsuffix .norm,$(REGRESSION_DOMAINS))
$(REGRESSION_NORMS): Regressions.norm-stamp ;
REGRESSION_GENS = $(addsuffix .gen,$(REGRESSION_DOMAINS))

# Has to come after definition of DEVELOPMENT_GENS, REGRESSION_GENS
regression_tests$(exe): $(DEVELOPMENT_GENS) $(REGRESSION_GENS) force
	$(GPRBUILD) -p -PRegressions 2>&1 | tee -a Regressions.log
exes:: regression_tests$(exe)
clean::
	-gnatclean -PRegressions

# Bad models
#BAD_MODEL_REGRESSION_DOMAINS =			\
#  Identifiers					\
#  Multiplicities

#BAD_MODEL_REGRESSION_NORMS = $(addsuffix .norm,$(BAD_MODEL_REGRESSION_DOMAINS))
#$(BAD_MODEL_REGRESSION_NORMS): Bad_Model_Regressions.norm-stamp ;
#BAD_MODEL_REGRESSION_GENS = $(addsuffix .gen,$(BAD_MODEL_REGRESSION_DOMAINS))

fails::
	-$(MAKE) Bad_Model_Regressions.norm-stamp
	-$(MAKE) Naming.norm-stamp

# Bad code
BAD_CODE_REGRESSION_DOMAINS =			\
  Abbreviations					\
  State_Machines

BAD_CODE_REGRESSION_NORMS = $(addsuffix .norm,$(BAD_CODE_REGRESSION_DOMAINS))
$(BAD_CODE_REGRESSION_NORMS): Bad_Code_Regressions.norm-stamp ;
BAD_CODE_REGRESSION_GENS = $(addsuffix .gen,$(BAD_CODE_REGRESSION_DOMAINS))

fails::
	-for g in $(BAD_CODE_REGRESSION_GENS); do	\
	  $(MAKE) $$g 2>&1 | tee $(basename $@).log;	\
	done

clean::
	rm -rf *.{norm-stamp,norm,stamp,stub,ada,gen,html,images}

.PHONY: all exes force clean runs fails
