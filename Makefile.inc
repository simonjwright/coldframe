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
  # Java and GNAT code need to see /cygwin in front of paths (not sure
  # what happens if Cygwin is installed elsewhere ...)
  CYGWIN = /cygwin
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
PROFILE ?= standard
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
  NORM_VERBOSE = -v
  CHOP_VERBOSE =
else
  NORM_VERBOSE =
  CHOP_VERBOSE = -q
endif

# Work out the location of this file (the top level of ColdFrame),
# without the trailing /.
COLDFRAME := $(shell echo $(dir $(abspath $(lastword $(MAKEFILE_LIST)))) \
	| sed -e "s;/$$;;")

# Where are the scripts?
SCRIPTS := $(COLDFRAME)/scripts

# Where are the models?
MODELS ?= $(COLDFRAME)/models

# Where are non-standard Ada project files?
export ADA_PROJECT_PATH := $(COLDFRAME):$(ADA_PROJECT_PATH)

CASE_EXCEPTIONS ?= $(SCRIPTS)/emacs_case_exceptions
COLDFRAMEOUT ?= $(COLDFRAME)/coldframeout

# Environmental overrides for 'standard' commands

CD ?= cd
CHMOD ?= chmod
CP ?= cp
DOT ?= dot
ECHO ?= echo
EXIT ?= exit
FIND ?= find
GNATCHOP ?= gnatchop -gnat12
GNATMAKE ?= gprbuild
GPRBUILD ?= gprbuild
GREP ?= grep
JAVA ?= java
MAKEINFO ?= makeinfo
MKDIR ?= mkdir
MV ?= mv
RM ?= rm
RSYNC ?= rsync
SED ?= sed
SH ?= sh
TAR ?= tar
TRUE ?= true
UNZIP ?= unzip
ZIP ?= zip

# Non-standard commands
SAXON_CLASSPATH ?= $(CYGWIN)$(HOME)/saxon/saxon.jar
SAXON ?= $(JAVA) -cp $(SAXON_CLASSPATH) com.icl.saxon.StyleSheet

# Control depth of .impl directory search
MAXIMPLDEPTH ?= 1

# Scripts
CODEGEN_SCRIPT   ?= $(SCRIPTS)/generate-ada.xsl
C_CODEGEN_SCRIPT ?= $(SCRIPTS)/generate-c.xsl
DIAGRAM_SCRIPT    = $(SCRIPTS)/generate-diagrams.xsl
HTMLGEN_SCRIPT    = $(SCRIPTS)/generate-html.xsl

# All code generation scripts, for development use
CODEGEN_SCRIPTS = $(SCRIPTS)/generate-ada.xsl		\
  $(SCRIPTS)/ada-association.xsl			\
  $(SCRIPTS)/ada-association-class.xsl			\
  $(SCRIPTS)/ada-association-collection.xsl		\
  $(SCRIPTS)/ada-association-class-collection.xsl	\
  $(SCRIPTS)/ada-attribute.xsl				\
  $(SCRIPTS)/ada-callback.xsl				\
  $(SCRIPTS)/ada-class.xsl				\
  $(SCRIPTS)/ada-collection.xsl				\
  $(SCRIPTS)/ada-inheritance.xsl			\
  $(SCRIPTS)/ada-operation.xsl				\
  $(SCRIPTS)/ada-state.xsl				\
  $(SCRIPTS)/ada-type.xsl				\
  $(SCRIPTS)/ada-teardown.xsl				\
  $(SCRIPTS)/ada-unittest.xsl				\
  $(SCRIPTS)/ada-utilities.xsl

# Pattern rules

# Generate .rr files (resolved references in ArgoUML files) from .uml.
# XXX This is a workround for a Mac OS X issue.
$(COLDFRAMEOUT)/%.rr: LANG=C
$(COLDFRAMEOUT)/%.rr: $(MODELS)/%.uml
	# XXX This is a workround for ArgoUML Issue 6385.
	($(GREP) -q "xmlns:argouml" $< && $(CP) $< $@~1) || 		\
	$(SED)								\
	  -e "s;<pgml ;<pgml xmlns:argouml='http://argouml.org' ;"	\
	  <$<								\
	  >$@~1
	$(SAXON)							  \
	  $(CYGWIN)$@~1							  \
	  $(CYGWIN)$(COLDFRAME)/scripts/resolve-references.xsl		  \
	  library_path=$(CYGWIN)$(COLDFRAME)/lib/			  \
	  >$@~2								  \
	  || ($(ECHO) "Resolution problem."; $(RM) -f $@ $@~?; $(EXIT) 1)
	# XXX This is a workround for an XML/Ada issue.
	$(SED)								\
	  -e "s;org.omg.xmi.namespace.UML;http://www.omg.org/spec/XMI;"	\
	  <$@~2								\
	  >$@

# Generate .rr files (resolved references in ArgoUML files) from .zargo.
# XXX This is a workround for a Mac OS X issue.
$(COLDFRAMEOUT)/%.rr: LANG=C
$(COLDFRAMEOUT)/%.rr: $(MODELS)/%.zargo
	$(UNZIP) -p $< $(*F).xmi >$@~1
	$(SAXON)							  \
	  $(CYGWIN)$@~1							  \
	  $(CYGWIN)$(COLDFRAME)/scripts/resolve-references.xsl		  \
	  library_path=$(CYGWIN)$(COLDFRAME)/lib/			  \
	  >$@~2								  \
	  || ($(ECHO) "Resolution problem."; $(RM) -f $@ $@~?; $(EXIT) 1)
	# XXX This is a workround for an XML/Ada issue.
	$(SED)								\
	  -e "s;org.omg.xmi.namespace.UML;http://www.omg.org/spec/XMI;"	\
	  <$@~2								\
	  >$@

# Generate .norm files from .rr.
# This is actually cast as a rule to generate .norm-stamp files from .rr,
# because a .rr file can contain any number of domains.
# Depends on normalize_xmi while under development.
%.norm-stamp: $(COLDFRAMEOUT)/%.rr $(COLDFRAME)/tools/normalize_xmi$(EXE)
	$(COLDFRAME)/tools/normalize_xmi	\
	  -c $(CYGWIN)$(CASE_EXCEPTIONS)	\
	  $(NORM_VERBOSE)			\
	  $(CYGWIN)$<
	touch $@

# Generate .xmi files from .tuml (TextUML) in $(MODELS)
# Depends on tuml2xml.py while under development.
$(COLDFRAMEOUT)/%.xmi: $(MODELS)/%.tuml $(COLDFRAME)/scripts/tuml2xmi.py
	$(COLDFRAME)/scripts/tuml2xmi.py --output=$@ $<

# Generate .xmi files from .tuml (TextUML) in $PWD
#
$(COLDFRAMEOUT)/%.xmi: %.tuml $(COLDFRAME)/scripts/tuml2xmi.py
	$(COLDFRAME)/scripts/tuml2xmi.py --output=$@ $<

# Generate .norm files from .xmi (as produced by tuml2xmi.py).
# This is actually cast as a rule to generate .tuml-norm-stamp files
# from .xmi, because a .xmi file can contain any number of domains.
# Depends on normalize_xmi while under development.
%.tuml-norm-stamp: $(COLDFRAMEOUT)/%.xmi $(COLDFRAME)/tools/normalize_xmi$(EXE)
	$(COLDFRAME)/tools/normalize_xmi	\
	  -c $(CYGWIN)$(CASE_EXCEPTIONS)	\
	  $(NORM_VERBOSE)			\
	  $(CYGWIN)$<
	touch $@

# Generate .html (and image) files.
# Do the diagrams first, because the HTML requires the cmapx output.
# The cwd parameter to the HTML script is because XSLT has no access
# otherwise.
%.html: %.norm $(HTMLGEN_SCRIPT) $(DIAGRAM_SCRIPT)
	@if [ "$(GENERATE_DIAGRAMS)" = "yes" ]; then	\
	  $(RM) -rf `basename $@ .html`.images;		\
	  $(MKDIR) `basename $@ .html`.images;		\
	  $(ECHO) generating diagrams for $@ ...;	\
	  $(SAXON) $< $(CYGWIN)$(DIAGRAM_SCRIPT)	\
	    >`basename $@ .html`.images/$@.sh;		\
	  $(SH) -v `basename $@ .html`.images/$@.sh;	\
	fi
	@$(ECHO) generating $@ ...
	@$(SAXON) $< $(CYGWIN)$(HTMLGEN_SCRIPT)				\
	  cwd="$(shell pwd)" diagrams=$(GENERATE_DIAGRAMS) >$@ ||	\
	  ($(RM) -f $@; $(EXIT) 1)

# Generate .ada files.
%.ada: %.norm $(CODEGEN_SCRIPTS)
	@$(ECHO) generating $@ ...
	@$(SAXON) $< $(CYGWIN)/$(CODEGEN_SCRIPT)			  \
	  add-blank-lines=$(BLANK_LINES)				  \
	  checking-policy=$(CHECKING)					  \
	  generate-accessors=$(GENERATE_ACCESSORS)			  \
	  generate-event-logging=$(GENERATE_EVENT_LOGGING)		  \
	  generate-stubs=$(GENERATE_STUBS)				  \
	  profile=$(PROFILE)						  \
	  unit-test-support=$(UNIT_TEST_SUPPORT)			  \
	  verbose=$(VERBOSE)						  \
	  >$@-t								  \
	  || ($(ECHO) "Generation problem."; $(RM) -f $@ $@-t; $(EXIT) 1)
	@$(SED) -e "s/LINES-OF-CODE/`tr -cd ';' <$@-t | wc -c | tr -d ' '`/" \
	  <$@-t >$@
	@$(RM) -f $@-t

# Generate .h files.
%.h: %.norm $(C_CODEGEN_SCRIPTS)
	@$(ECHO) generating $@ ...
	@$(SAXON) $< $(CYGWIN)$(C_CODEGEN_SCRIPT)			\
	  add-blank-lines=$(BLANK_LINES)				\
	  generate-accessors=$(GENERATE_ACCESSORS)			\
	  verbose=$(VERBOSE)						\
	  >$@								\
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
	@if [ -d $*.impl ]; then			\
	  TODELETE="";					\
	  IMPLFILES=`$(FIND) $*.impl/			\
	    -maxdepth $(MAXIMPLDEPTH)			\
	    -name \*.ad[bs]`;				\
	  for f in $$IMPLFILES; do			\
	    GENFILE=$@/`basename $$f`;			\
	    if [ -f $$GENFILE ]; then			\
	      TODELETE="$$TODELETE $$GENFILE";		\
	    else					\
	      $(ECHO) "  extra source file $$f";	\
	    fi;						\
	  done;						\
	  for f in $$TODELETE; do			\
	    if [ -f $$f ]; then				\
	      $(ECHO) "    removing $$f"; $(RM) $$f;	\
	    fi;						\
	  done;						\
	fi
	@$(CHMOD) -R a-w $@
	@$(CHMOD) u+w $@
	@$(ECHO) "checking for unimplemented bodies ..."
	@$(GREP) -rl 'edit this' $@ || $(ECHO) "... none."

# Generate single-page HTML from texi.
%.html: %.texi
	$(MAKEINFO) --html --no-split $<
