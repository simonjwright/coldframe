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

CHECKING ?= relaxed
GENERATE_DIAGRAMS ?= yes
STACK_DUMP ?= yes
VERBOSE ?= no

# Define these variables in an including Makefile (before the actual
# include) or in the environment

CASE_EXCEPTIONS ?= ~/.emacs_case_exceptions
COLDFRAMEOUT ?= .

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
else
  NORM_VERBOSE =
endif

# Environmental overrides for 'standard' commands

CHMOD ?= chmod
CD ?= cd
CP ?= cp
DOT ?= dot
ECHO ?= echo
EXIT ?= exit
FIND ?= find
GNATCHOP ?= gnatchop
GREP ?= grep
JAVA ?= java
MKDIR ?= mkdir
MV ?= mv
PS2PDF ?= ps2pdf
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
SAXON ?= $(JAVA) -cp /usr/local/lib/saxon/saxon.jar com.icl.saxon.StyleSheet

# Control depth of .impl directory search
MAXIMPLDEPTH ?= 1

DIAGRAM_SCRIPT = ../scripts/generate-diagrams.xsl
HTMLGEN_SCRIPT = ../scripts/generate-html.xsl
NORMALIZE_ROSE_SCRIPT = ../scripts/normalize-rose.tcl

DOCGEN_SCRIPTS = $(HTMLGEN_SCRIPT) $(DIAGRAM_SCRIPT)

# Pattern rules

# Must have ply installed in python.
# Will need extra dependencies if there are controlled child packages.
%.raw: ../models/%.cat
	../scripts/cat2raw.py -o $@ $<

%.norm: %.raw $(NORMALIZE_ROSE_SCRIPT)
	@$(ECHO) generating $@ ...
	@TCLLIBPATH=$(TCLXML) $(ITCLSH) $(NORMALIZE_ROSE_SCRIPT) \
	    --casing '$(CASE_EXCEPTIONS)' \
	    $(NORM_CHECKING) \
	    $(NORM_DOMAIN_NAME) \
	    --output $@ \
	    $(NORM_STACK_DUMP) \
	    $(NORM_VERBOSE) \
	    <$< || ($(RM) -f $@; $(EXIT) 1)

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
	  cwd=$(PWD) diagrams=$(GENERATE_DIAGRAMS) >$@ || \
	  ($(RM) -f $@; $(EXIT) 1)

use-cases.html: use-cases.texi
	$(TEXI2HTML) -monolithic $<

%.pdf: %.ps
	$(PS2PDF) $<

all:: html

html:: use-cases.html coldframe-architecture.html

# Specific rule for coldframe-architecture.cat: (a) different
# generator, (b) avoid clash with handwritten architecture.html.
coldframe-architecture.html: Architecture.raw generate-architecture-html.xsl
	$(SAXON) \
	    Architecture.raw generate-architecture-html.xsl >$@

GIFS = States.gif States-Monitor.gif
JPEGS = navigation.jpg window-screen.jpg
PNGS = \
better-association.png \
browse-state-model.png \
class-mappings.png \
collections-mapping.png \
digital-io.png \
digital-io-interface.png \
discriminated-record.png \
event-mapping.png \
hierarchies-full.png \
hierarchies.png \
house-digital-io.png \
house-management.png \
house-management-operation.png \
inheritance.png \
lamp.png \
lamp-state.png\
lamp-state-resetting.png\
metamodel.png \
operations-mapping.png \
real_time.png \
recordable_real_time.png \
reflexive.png \
relationships-mapping.png \
sample_a.png \
self-events.png \
serialization-class-model-t.png \
serialization-class-model.png \
serialization-sequence-t.png \
serialization-sequence.png \
serialization-state-t.png \
serialization-state.png \
serialization.png \
simple-association.png \
type-mapping.png \
vague-association.png

# graphviz:
%.png: %.dot
	$(DOT) -o $@ -Tpng $<

############################
# Distribution construction

# The subrelease is 'hg' by default. Change on the command line for
# anything else.
SUBRELEASE ?= hg

# Create the current date, in the form yyyymmdd. This certainly works in
# Linux.
#
# Used to construct release IDs (eg, cf-20050423cvs). You can set the
# whole thing from the command line -- for example, if creating a patch
# release.
DATE ?= $(shell date +%Y%m%d)$(SUBRELEASE)

DOCS = \
cf.css \
active-classes.html \
analysis.html \
annotations.html \
architecture.html \
associations.html \
attributes.html \
bugs.html \
callbacks.html \
classes.html \
compilation.html \
conversion.html \
copying.html \
directions.html \
documentation.html \
domains.html \
error-messages.html \
events.html \
event-modelling.html \
event-motivation.html \
event-translation.html \
event-use.html \
extending.html \
extraction.html \
faq.html \
generalizations.html generalizations-extended.html \
generation.html \
index.html \
initialization.html \
installation.html \
operation-parameters.html \
operations.html \
preparation.html \
principles.html \
releases.html \
reserved-names.html \
resources.html \
serialization-page.html \
strategy.html \
support.html \
target.html \
testing.html \
translation-rules.html \
types.html \
use-of-bcs.html \
use-cases.html use-cases.texi \
xml-schemas.html States-check.xsv States-check.html \
coldframe-architecture.html \
$(GIFS) $(JPEGS) $(PNGS) $(PDFS) \
coldframe-architecture.cat \
ColdFrame-raw.xsd ColdFrame-norm.xsd

# These are the parts of doc generated from models. Generation also
# generates {Domain}.images/, not mentioned as part of the target.
GEN_DOCS = \
Serialization.html \
Digital_IO.html \
House_Management.html

GEN_DOC_IMAGES = $(subst .html,.images,$(GEN_DOCS))

docs: $(DOCS) $(GEN_DOCS)


# Documentation upload to SF

SFUSER ?= simonjwright

upload-docs: top-index.html docs force
	$(RSYNC) \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --delete \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  top-index.html \
	  $(SFUSER),coldframe@web.sourceforge.net:htdocs/index.html
	$(RSYNC) \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --delete \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  cf.css \
	  $(SFUSER),coldframe@web.sourceforge.net:htdocs/
	$(RSYNC) \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --delete \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  $(DOCS)  $(GEN_DOCS) $(GEN_DOC_IMAGES) \
	  $(SFUSER),coldframe@web.sourceforge.net:htdocs/coldframe

# The complete distribution

dist: cf-$(DATE) $(DISTRIBUTION_FILES) $(DOCS) $(GEN_DOCS)
	-@$(RM) -rf dist
	$(MKDIR) -p dist/download
	$(CP) -p $(DOCS) $(GEN_DOCS) dist/
	$(CP) -pR $(GEN_DOC_IMAGES) dist/
	$(CD) dist && $(ZIP) -9r download/cf-html-$(DATE).zip .
	$(CP) $(DISTRIBUTION_FILES) dist/download/

# Files that need DATE substituted
DATED_FILES = \
 normalize-rose.tcl \
 ada-utilities.xsl \
 generate-c.xsl

cf-$(DATE): $(MAKEFILES) $(GPRS) $(PROGS) $(SUPPORT) $(PROJECT) $(EXTRAS) \
  $(EXAMPLES) $(TEST) $(TOOL_SRC) Makefile-test force
	-$(RM) -rf $@
	$(MKDIR) $@
	$(CP) -p $(MAKEFILES) $(GPRS) $(PROGS) $@
	for f in $(DATED_FILES); do \
	    ($(SED) -e "s;DATE;$(DATE);g" <$$f >$@/$$f) \
	done
	$(MKDIR) $@/lib
	$(CP) -p $(SUPPORT) $@/lib
	$(MKDIR) $@/project
	$(CP) -p $(PROJECT) $@/project
	$(MKDIR) $@/extras
	$(CP) -p $(EXTRAS) $@/extras
	$(MKDIR) $@/examples
	$(TAR) cf - $(EXAMPLES) | $(TAR) xf - -C $@/examples
	$(MKDIR) $@/test
	$(TAR) cf - $(TEST) | $(TAR) xf - -C $@/test
	$(MKDIR) $@/tools
	$(TAR) cf - $(TOOL_SRC) | $(TAR) xf - -C $@/tools
	$(CP) -p Makefile-test $@/test/Makefile
	$(CP) -p Makefile-examples $@/examples/Makefile

cf-$(DATE).tgz: cf-$(DATE)
	-$(RM) $@
	$(TAR) zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	-$(RM) $@
	$(ZIP) -r -9 $@ $</*

.PHONY: force
