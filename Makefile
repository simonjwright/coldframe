# Copyright (c) 2001 Simon Wright <simon@pushface.org>
# $Id$

AWK = awk
ITCLSH = /usr/bin/itclsh3.1
TCLXML = $(HOME)/TclXML-1.2

SAXON = java com.icl.saxon.StyleSheet

ESCAPE_MARKUP_SCRIPT = escape-markup.awk
NORMALIZE_ROSE_SCRIPT = normalize-rose.tcl
HTMLGEN_SCRIPT = generate-html.xsl
CODEGEN_SCRIPT = generate-ada.xsl
CODEGEN_SCRIPTS = $(CODEGEN_SCRIPT) \
  ada-association.xsl \
  ada-attribute.xsl \
  ada-class.xsl \
  ada-collection.xsl \
  ada-operation.xsl \
  ada-utilities.xsl

%.norm: %.raw $(NORMALIZE_ROSE_SCRIPT) $(ESCAPE_MARKUP_SCRIPT)
	$(AWK) -f $(ESCAPE_MARKUP_SCRIPT) <$< | \
	TCLLIBPATH=$(TCLXML) $(ITCLSH) $(NORMALIZE_ROSE_SCRIPT) >$@ || rm -f $@

%.html: %.norm $(HTMLGEN_SCRIPT)
	$(SAXON) $< $(HTMLGEN_SCRIPT) >$@ || rm -f $@

%.ada: %.norm $(CODEGEN_SCRIPTS)
	$(SAXON) $< $(CODEGEN_SCRIPT) >$@ || rm -f $@

%.gen: %.ada
	-mkdir $@
	rm -f $@/*.ad[bs]
	gnatchop $< $@

%.doc: %.norm $(HTMLGEN_SCRIPT)
	-mkdir $@
	rm -f $@/*
	$(MAKE) `basename $@ .doc`.html
	mv `basename $@ .doc`.html $@

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

pdf:: coldframe-architecture.pdf

# preserve intermediate files
.PRECIOUS:: Problem_Reporting.norm Problem_Reporting.ada
.PRECIOUS:: Tewa.norm  Tewa.ada
.PRECIOUS:: Weapon_Assignment.norm  Weapon_Assignment.ada

############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works in Linux.
DATE = $(shell date +%Y%m%d)

DOCS = architecture.html \
directions.html \
index.html \
principles.html \
releases.html \
resources.html \
use-cases.html use-cases.texi \
coldframe-architecture.html coldframe-architecture.pdf \
coldframe-architecture.cat \
ddf.dtd coldframe.dtd \
xslide-diff

# Makefile-cf is the published makefile for development of ColdFrame itself.
# Other makefiles are
# * Makefile-winnt for inclusion in a user makefile under Windows
# * Makefile-unix for inclusion in a user makefile under Unix.

MAKEFILES = Makefile-cf Makefile-unix Makefile-winnt

Makefile-cf: Makefile
	cp -p $< $@
Makefile-unix: Makefile-unix-proto
	sed -e "s;DATE;$(DATE);g" <$< >$@
Makefile-winnt: Makefile-winnt-proto
	sed -e "s;DATE;$(DATE);g" <$< >$@

PROGS = COPYING \
  ddf.ebs \
  escape-markup.awk \
  normalize-rose.tcl \
  $(HTMLGEN_SCRIPT) \
  $(CODEGEN_SCRIPTS)

SUPPORT = coldframe*.ad[bs]

DEMO = Problem_Reporting.cat Problem_Reporting.raw \
Problem_Reporting.impl/demo.adb \
Problem_Reporting.impl/problem_reporting-component-hash.adb \
Problem_Reporting.impl/problem_reporting-interface-add_component.adb \
Problem_Reporting.impl/problem_reporting-interface-add_problem.adb \
Problem_Reporting.impl/problem_reporting-interface-delete_component.adb \
Problem_Reporting.impl/problem_reporting-interface-note_defect.adb \
Problem_Reporting.impl/problem_reporting-interface-reject_problem.adb \
Problem_Reporting.impl/problem_reporting-interface-report_problems.adb

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

cf-$(DATE): force
	-rm -rf $@
	-rm -f $(MAKEFILES)
	$(MAKE) $(MAKEFILES)
	mkdir $@
	cp -p $(PROGS) $(MAKEFILES) $@
	mkdir $@/lib
	cp -p $(SUPPORT) $@/lib
	mkdir $@/example
	cp -p $(DEMO) $@/example

cf-$(DATE).tgz: cf-$(DATE)
	-rm $@
	tar zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	-rm $@
	zip -lr $@ $</*

.PHONY: force
