# Copyright (c) 2001 Simon Wright <simon@pushface.org>
# $Id$

NORMALIZE_ROSE_SCRIPT = normalize-rose.tcl
CODEGEN_SCRIPT = generate-ada.xsl

%.norm: %.raw $(NORMALIZE_ROSE_SCRIPT)
	$(NORMALIZE_ROSE_SCRIPT) <$< >$@

%.ada: %.norm $(CODEGEN_SCRIPT)
	java com.icl.saxon.StyleSheet $< $(CODEGEN_SCRIPT) >$@

%: %.ada
	-mkdir $@
	rm -f $@/*.ad[bs]
	gnatchop $< $@

TEXI2HTML = texi2html
%.html: %.texi
	$(TEXI2HTML) -monolithic $<

all:: html

html:: use-cases.html

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
use-cases.html \
ddf.dtd coldframe.dtd \
xslide-diff

PROGS = Makefile ddf.ebs normalize-rose.tcl generate-ada.xsl

DISTRIBUTION_FILES = \
cf-$(DATE).tgz \
cf-$(DATE).zip

dist: $(DISTRIBUTION_FILES) $(DOCS) $(PROGS)
	-@rm -rf dist
	mkdir -p dist/download
	cp -p $(DOCS) $(PROGS) dist/
	cp $(DISTRIBUTION_FILES) dist/download/

cf-$(DATE): force
	-rm -rf $@
	mkdir $@
	cp -p $(DOCS) $@

cf-$(DATE).tgz: cf-$(DATE)
	tar zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	zip -lr $@ $</*

.PHONY: force
