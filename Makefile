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

.PRECIOUS:: Problem_Reporting.norm Problem_Reporting.ada
.PRECIOUS:: Weapon_Assignment.norm  Weapon_Assignment.ada
