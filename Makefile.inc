# -*- Makefile -*-

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

# Contains common variable definitions for user Makefiles.

# use with Unix or with Cygwin under Windows (assumes a Unix system if
# Cygwin not detected).
sys = $(shell uname)
ifneq (, $(findstring CYGWIN, $(sys)))
  SYSTEM = winnt
  EXE = .exe
else
  SYSTEM = unix
  EXE =
endif

BLANK_LINES ?= yes
CHECKING ?= strict
GENERATE_ACCESSORS ?= defined
GENERATE_DIAGRAMS ?= yes
GENERATE_EVENT_LOGGING ?= no
GENERATE_STUBS ?= no
STACK_DUMP ?= yes
UNIT_TEST_SUPPORT ?= no
VERBOSE ?= no

# Work out flags for code generation
ifeq ($(CHECKING), strict)
  NORM_CHECKING = --checking strict
else
  NORM_CHECKING = --checking relaxed
endif

ifeq ($(DOMAIN_NAME), )
  NORM_DOMAIN_NAME =
else
  NORM_DOMAIN_NAME = --domain-name "$(DOMAIN_NAME)"
endif

ifeq ($(STACK_DUMP), yes)
  NORM_STACK_DUMP = --stack-dump
else
  NORM_STACK_DUMP =
endif

ifeq ($(VERBOSE), yes)
  NORM_VERBOSE = --verbose
  CHOP_VERBOSE =
else
  NORM_VERBOSE =
  CHOP_VERBOSE = -q
endif

# Work out the location of this file (the top level of ColdFrame).
COLDFRAME := $(shell echo $(dir $(abspath $(lastword $(MAKEFILE_LIST)))) | sed -e "s;/$$;;")

# Where's the base for the build directories?
export BUILD_BASE ?= $(COLDFRAME)/.build

# Where are the scripts?
SCRIPTS := $(COLDFRAME)/scripts

# Where are the models?
MODELS ?= $(COLDFRAME)/models

CASE_EXCEPTIONS ?= $(SCRIPTS)/emacs_case_exceptions
COLDFRAMEOUT ?= $(COLDFRAME)/coldframeout

# Environmental overrides for 'standard' commands

CHMOD ?= chmod
CD ?= cd
CP ?= cp
DOT ?= dot
ECHO ?= echo
EXIT ?= exit
FIND ?= find
GNATCHOP ?= gnatchop -gnat95
GREP ?= grep
JAVA ?= java
MKDIR ?= mkdir
MV ?= mv
RM ?= rm
RSYNC ?= rsync
SED ?= sed
SH ?= sh
TAR ?= tar
TEXI2HTML ?= texi2html
TRUE ?= true
ZIP ?= zip

# Non-standard commands
ITCLSH ?= tclsh
SAXON_CLASSPATH ?= /usr/local/lib/saxon/saxon.jar
SAXON ?= $(JAVA) -cp $(SAXON_CLASSPATH) com.icl.saxon.StyleSheet

# Control depth of .impl directory search
MAXIMPLDEPTH ?= 1

# Scripts
CODEGEN_SCRIPT ?= $(SCRIPTS)/generate-ada.xsl
DIAGRAM_SCRIPT = $(SCRIPTS)/generate-diagrams.xsl
HTMLGEN_SCRIPT = $(SCRIPTS)/generate-html.xsl
NORMALIZE_RAW_SCRIPT = $(SCRIPTS)/normalize-raw.xsl
NORMALIZE_ROSE_SCRIPT = $(SCRIPTS)/normalize-rose.tcl

# Construct the build directories if necessary.
build-dirs::
	@[ -n "$(BUILD_BASE)" ] \
	  || ($(ECHO) "BUILD_BASE must be set" && $(EXIT) 1)
	@for d in coldframe fixes main; do \
	  [ -d $(BUILD_BASE)/$$d ] || \
	    ($(ECHO) $(MKDIR) -p $(BUILD_BASE)/$$d; \
	     $(MKDIR) -p $(BUILD_BASE)/$$d);\
	done

# Pattern rules

# Generate .raw files.
# Must have ply installed in python.
# Will need extra dependencies if there are controlled child packages.
$(COLDFRAMEOUT)/%.raw: $(MODELS)/%.cat
	python $(SCRIPTS)/cat2raw.py -o $@ $<

%.raw-norm: $(COLDFRAMEOUT)/%.raw
	  $(SAXON) $< $(NORMALIZE_RAW_SCRIPT)  >$@

# Generate .norm files.
%.norm: $(COLDFRAMEOUT)/%.raw $(NORMALIZE_ROSE_SCRIPT)
	@$(ECHO) generating $@ ...
	$(ITCLSH) $(NORMALIZE_ROSE_SCRIPT) \
	    --casing '$(CASE_EXCEPTIONS)' \
	    $(NORM_CHECKING) \
	    $(NORM_DOMAIN_NAME) \
	    --output $@ \
	    $(NORM_STACK_DUMP) \
	    $(NORM_VERBOSE) \
	    <$< || ($(RM) -f $@; $(EXIT) 1)

# Generate .html (and image) files.
# Do the diagrams first, because the HTML requires the cmapx output.
# The cwd parameter to the HTML script is because XSLT has no access
# otherwise.
%.html: %.norm $(HTMLGEN_SCRIPT) $(DIAGRAM_SCRIPT)
	@if [ "$(GENERATE_DIAGRAMS)" = "yes" ]; then \
	  $(RM) -rf `basename $@ .html`.images; \
	  $(MKDIR) `basename $@ .html`.images; \
	  $(ECHO) generating diagrams for $@ ...; \
	  $(SAXON) $< $(DIAGRAM_SCRIPT) \
	    >`basename $@ .html`.images/$@.sh; \
	  $(SH) -v `basename $@ .html`.images/$@.sh; \
	fi
	@$(ECHO) generating $@ ...
	@$(SAXON) $< $(HTMLGEN_SCRIPT) \
	  cwd=$(shell pwd) diagrams=$(GENERATE_DIAGRAMS) >$@ || \
	  ($(RM) -f $@; $(EXIT) 1)

# Generate .ada files.
%.ada: %.norm $(CODEGEN_SCRIPTS)
	@$(ECHO) generating $@ ...
	@$(SAXON) $< $(CODEGEN_SCRIPT) \
	  add-blank-lines=$(BLANK_LINES) \
	  checking-policy=$(CHECKING) \
	  generate-accessors=$(GENERATE_ACCESSORS) \
	  generate-event-logging=$(GENERATE_EVENT_LOGGING) \
	  generate-stubs=$(GENERATE_STUBS) \
	  unit-test-support=$(UNIT_TEST_SUPPORT) \
	  verbose=$(VERBOSE) \
	  >$@-t \
	  || ($(ECHO) "Generation problem."; $(RM) -f $@ $@-t; $(EXIT) 1)
	@$(SED) -e "s/LINES-OF-CODE/`tr -cd ';' <$@-t | wc -c | tr -d ' '`/" \
	  <$@-t >$@
	@$(RM) -f $@-t

# Generate .h files.
%.h: %.norm $(C_CODEGEN_SCRIPTS)
	@$(ECHO) generating $@ ...
	@$(SAXON) $< $(C_CODEGEN_SCRIPT) \
	  add-blank-lines=$(BLANK_LINES) \
	  generate-accessors=$(GENERATE_ACCESSORS) \
	  verbose=$(VERBOSE) \
	  >$@ \
	  || ($(ECHO) "Generation problem."; $(RM) -f $@; $(EXIT) 1)

# Generate the .gen directory.
# Delete the target directory & all contents.
# Create the target directory.
# Gnatchop the .ada file; allow overwrites (in case user-suplied
# additional processing steps generate actual implementations). Allow
# "failure" because GNAT 3.16a1 reports an error here.
# Remove any generated files which are also present in the implementation
# directory (.impl).
# Write-protect the generated files (careful, in case there are a lot of
# them!).
# Make the target directory itself writable (so users can delete files in it).
# Report unimplemented bodies.
%.gen: %.ada
	@$(ECHO) generating $@ ...
	@-$(RM) -rf $@
	@$(MKDIR) $@
	@-$(GNATCHOP) -w $(CHOP_VERBOSE) $< $@
	@if [ -d $*.impl ]; then \
	  TODELETE=""; \
	  IMPLFILES=`$(FIND) $*.impl \
	    -maxdepth $(MAXIMPLDEPTH) \
	    -name \*.ad[bs]`; \
	  for f in $$IMPLFILES; do \
	    GENFILE=$@/`basename $$f`; \
	    if [ -f $$GENFILE ]; then \
	      TODELETE="$$TODELETE $$GENFILE"; \
	    else \
	      $(ECHO) "  extra source file $$f"; \
	    fi; \
	  done; \
	  for f in $$TODELETE; do \
	    if [ -f $$f ]; then \
	      $(ECHO) "    removing $$f"; $(RM) $$f; \
	    fi; \
	  done; \
	fi
	@$(CHMOD) -R a-w $@
	@$(CHMOD) u+w $@
	@$(ECHO) "checking for unimplemented bodies ..."
	@$(GREP) -rl 'edit this' $@ || $(ECHO) "... none."

