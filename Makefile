# Copyright (c) 2001-2002 Simon Wright <simon@pushface.org>
# $Id$

BLANK_LINES = yes
CASE_EXCEPTIONS = ~/.emacs_case_exceptions
GENERATE_ACCESSORS = defined
STACK_DUMP = yes
VERBOSE = no

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

AWK = awk
ITCLSH = /usr/bin/itclsh3.1
TCLXML = $(HOME)/tclxml-2.1theta

SAXON = java com.icl.saxon.StyleSheet

ESCAPE_MARKUP_SCRIPT = escape-markup.awk
NORMALIZE_ROSE_SCRIPT = normalize-rose.tcl
HTMLGEN_SCRIPT = generate-html.xsl
CODEGEN_SCRIPT = generate-ada.xsl
CODEGEN_SCRIPTS = $(CODEGEN_SCRIPT) \
  ada-association.xsl \
  ada-attribute.xsl \
  ada-callback.xsl \
  ada-class.xsl \
  ada-collection.xsl \
  ada-operation.xsl \
  ada-state.xsl \
  ada-type.xsl \
  ada-teardown.xsl \
  ada-utilities.xsl

%.norm: %.raw $(NORMALIZE_ROSE_SCRIPT) $(ESCAPE_MARKUP_SCRIPT)
	$(AWK) -f $(ESCAPE_MARKUP_SCRIPT) <$< | \
	TCLLIBPATH=$(TCLXML) $(ITCLSH) $(NORMALIZE_ROSE_SCRIPT) \
	  --casing $(CASE_EXCEPTIONS) \
	  $(NORM_STACK_DUMP) \
	  $(NORM_VERBOSE) \
	  --version cf-DATE \
	  >$@ || (rm -f $@; exit 1)

%.html: %.norm $(HTMLGEN_SCRIPT)
	$(SAXON) $< $(HTMLGEN_SCRIPT) >$@ || (rm -f $@; exit 1)

%.ada: %.norm $(CODEGEN_SCRIPTS)
	$(SAXON) $< $(CODEGEN_SCRIPT) \
	  add-blank-lines=$(BLANK_LINES) \
	  coldframe-version=cf-DATE \
	  generate-accessors=$(GENERATE_ACCESSORS) \
	  verbose=$(VERBOSE) \
	  >$@-t \
	  || (echo "Generation problem."; rm -f $@ $@-t; exit 1)
	sed -e "s/LINES-OF-CODE/`tr -cd ';' <$@-t | wc -c | tr -d ' '`/" \
	  <$@-t >$@
	rm -f $@-t

# Create the target directory
# get rid of any files in it already
# gnatchop the .ada file
# remove any generated files which are also present in the implementation
# directory (.impl)
# write-protect the generated files
%.gen: %.ada
	-mkdir $@
	rm -f $@/*.ad[bs]
	gnatchop $(CHOP_VERBOSE) $< $@
	[ ! -d $*.impl ] || for f in `(cd $*.impl; ls *.ad?)`; do \
	   [ ! -f $@/$$f ] || ( echo rm $@/$$f; rm $@/$$f); \
	done
	chmod a=r $@/*.ad[bs]

TG = $(HOME)/bin/tg
%.adb: %.ts
	$(TG) $<

TEXI2HTML = texi2html
%.html: %.texi
	$(TEXI2HTML) -monolithic $<

PS2PDF = ps2pdf
%.pdf: %.ps
	$(PS2PDF) $<

all:: html pdf

html:: use-cases.html coldframe-architecture.html

# Architecture.raw is extracted from the Rose Architecture package
# (coldframe-architecture.cat) using ddf.ebs
coldframe-architecture.html: Architecture.raw generate-architecture-html.xsl
	java com.icl.saxon.StyleSheet \
	    Architecture.raw generate-architecture-html.xsl >$@

# coldframe-architecture.ps is made by printing the Rose Architecture package
# diagram (from coldframe-architecture.cat) to PostScript, from within Rose.

PDFS = coldframe-architecture.pdf \
  coldframe-callback.pdf \
  coldframe-relationships.pdf
pdf:: $(PDFS)

GIFS = States.gif States-Monitor.gif inheritance.gif
JPEGS = navigation.jpg window-screen.jpg

############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works in Linux.
DATE = $(shell date +%Y%m%d)

DOCS = architecture.html \
bugs.html \
conversion.html \
directions.html \
extraction.html \
faq.html \
generation.html \
index.html \
preparation.html \
principles.html \
releases.html \
resources.html \
strategy.html \
support.html \
target.html \
translation-rules.html \
use-of-bcs.html \
use-cases.html use-cases.texi \
coldframe-architecture.html \
$(GIFS) $(JPEGS) $(PDFS) \
coldframe-architecture.cat \
ddf.dtd coldframe.dtd \
xslide-diff

# Makefile-cf is the published makefile for development of ColdFrame itself.
# Other makefiles are
# * Makefile-winnt for inclusion in a user makefile under Windows
# * Makefile-unix for inclusion in a user makefile under Unix.

MAKEFILES = Makefile-cf Makefile-unix Makefile-winnt

Makefile-cf: Makefile force
	cp -p $< $@
Makefile-unix: Makefile-unix-proto force
	sed -e "s;DATE;$(DATE);g" <$< >$@
Makefile-winnt: Makefile-winnt-proto force
	sed -e "s;DATE;$(DATE);g" <$< >$@

extractor.ebs: ddf.ebs force
	sed -e "s;DATE;$(DATE);g" <$< >$@

PROGS = COPYING \
  extractor.ebs \
  escape-markup.awk \
  normalize-rose.tcl \
  $(HTMLGEN_SCRIPT) \
  $(CODEGEN_SCRIPTS)

SUPPORT = coldframe*.ad[bs]

DEMO = Problem_Reporting.cat Problem_Reporting.raw \
Problem_Reporting.impl/demo.adb \
Problem_Reporting.impl/problem_reporting-component-clean.adb \
Problem_Reporting.impl/problem_reporting-component-report.adb \
Problem_Reporting.impl/problem_reporting-component-report_all.adb \
Problem_Reporting.impl/problem_reporting-defect-report.adb \
Problem_Reporting.impl/problem_reporting-interface-add_component.adb \
Problem_Reporting.impl/problem_reporting-interface-add_problem.adb \
Problem_Reporting.impl/problem_reporting-interface-delete_component.adb \
Problem_Reporting.impl/problem_reporting-interface-note_defect.adb \
Problem_Reporting.impl/problem_reporting-interface-reject_problem.adb \
Problem_Reporting.impl/problem_reporting-interface-report_problems.adb \
States.cat States.raw \
States.impl/states-monitor-clear_heartbeat_timeout.adb \
States.impl/states-monitor-clear_warmup_timeout.adb \
States.impl/states-monitor-set_2_second_warmup_timeout.adb \
States.impl/states-monitor-set_6_second_warmup_timeout.adb \
States.impl/states-monitor-set_heartbeat_timeout.adb \
States.impl/states-t.adb \
States.impl/states-t.ads

DISTRIBUTION_FILES = \
cf-$(DATE).tgz \
cf-$(DATE).zip

dist: $(DISTRIBUTION_FILES) \
$(DOCS) \
$(PROGS) \
$(SUPPORT) \
$(DEMO) \
cf-$(DATE)
	-@rm -rf dist
	mkdir -p dist/download
	cp -p $(DOCS) dist/
	cp $(DISTRIBUTION_FILES) dist/download/

cf-$(DATE): $(MAKEFILES) $(PROGS) force
	-rm -rf $@
	mkdir $@
	cp -p $(PROGS) $(MAKEFILES) $@
	mkdir $@/lib
	cp -p $(SUPPORT) $@/lib
	mkdir $@/example
	tar cf - $(DEMO) | tar xf - -C $@/example

cf-$(DATE).tgz: cf-$(DATE)
	-rm $@
	tar zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	-rm $@
	zip -lr $@ $</*

.PHONY: force
