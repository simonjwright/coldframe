# Copyright (c) 2001 Simon Wright <simon@pushface.org>
# $Id$

NORMALIZE_ROSE_SCRIPT = normalize-rose.tcl
HTMLGEN_SCRIPT = generate-html.xsl
CODEGEN_SCRIPT = generate-ada.xsl

ITCLSH = itclsh3.1
SAXON = java com.icl.saxon.StyleSheet

%.norm: %.raw $(NORMALIZE_ROSE_SCRIPT)
	$(ITCLSH) $(NORMALIZE_ROSE_SCRIPT) <$< >$@

%.html: %.norm $(HTMLGEN_SCRIPT)
	$(SAXON) $< $(HTMLGEN_SCRIPT) >$@

%.ada: %.norm $(CODEGEN_SCRIPT)
	$(SAXON) $< $(CODEGEN_SCRIPT) >$@

%: %.ada
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

PROGS = COPYING Makefile ddf.ebs normalize-rose.tcl generate-ada.xsl
SUPPORT = architecture*.ad[bs]

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

dist: $(DISTRIBUTION_FILES) $(DOCS) $(PROGS) $(SUPPORT) $(DEMO) cf-$(DATE)
	-@rm -rf dist
	mkdir -p dist/download
	cp -p $(DOCS) dist/
	cp $(DISTRIBUTION_FILES) dist/download/

cf-$(DATE): force
	-rm -rf $@
	mkdir $@
	cp -p $(PROGS) $@
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
