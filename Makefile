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

# $Id$

BLANK_LINES = yes
GENERATE_ACCESSORS = defined
STACK_DUMP = yes
VERBOSE = no

# Define these variables in an including Makefile (before the actual
# include) or in the environment

ifeq ($(CASE_EXCEPTIONS), )
  CASE_EXCEPTIONS = ~/.emacs_case_exceptions
endif

ifeq ($(COLDFRAMEOUT), )
  COLDFRAMEOUT = .
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

AWK = awk
ITCLSH = tclsh
TCLXML = /usr/local/lib/tclxml-2.1theta

SAXON = java -cp /usr/local/lib/saxon/saxon.jar com.icl.saxon.StyleSheet

ESCAPE_MARKUP_SCRIPT = escape-markup.awk
NORMALIZE_ROSE_SCRIPT = normalize-rose.tcl
HTMLGEN_SCRIPT = generate-html.xsl
CODEGEN_SCRIPT = generate-ada.xsl
CODEGEN_SCRIPTS = $(CODEGEN_SCRIPT) \
  ada-association.xsl \
  ada-association-collection.xsl \
  ada-attribute.xsl \
  ada-callback.xsl \
  ada-class.xsl \
  ada-collection.xsl \
  ada-inheritance.xsl \
  ada-operation.xsl \
  ada-serialization.xsl \
  ada-state.xsl \
  ada-type.xsl \
  ada-teardown.xsl \
  ada-utilities.xsl
OTHER_SCRIPTS = serialized-to-csv.xsl

%.norm: $(COLDFRAMEOUT)/%.raw $(NORMALIZE_ROSE_SCRIPT) $(ESCAPE_MARKUP_SCRIPT)
	echo $(COLDFRAMEOUT) $< $*
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

# Delete the target directory & all contents
# create the target directory
# gnatchop the .ada file
# remove any generated files which are also present in the implementation
# directory (.impl)
# write-protect the generated files (careful, in case there are a lot of them!)
# report unimplemented bodies
%.gen: %.ada
	-rm -rf $@
	-mkdir $@
	gnatchop $(CHOP_VERBOSE) $< $@
	[ ! -d $*.impl ] || for f in `(cd $*.impl; ls *.ad?)`; do \
	   [ ! -f $@/$$f ] || ( echo rm $@/$$f; rm $@/$$f); \
	done
	chmod -R a-w $@
	chmod u+w $@
	@echo "checking for unimplemented bodies .."
	@grep -rl 'edit this' $@ || echo ".. none."

# Split serialized XML files into CSV format, one per serialized type found.
%.csv: %.xml
	echo "<recording>" >$<-t
	cat $< >>$<-t
	echo "</recording>" >>$<-t
	$(SAXON) $<-t $(HOME)/cf/serialized-to-csv.xsl
	rm -f $<-t

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
	$(SAXON) \
	    Architecture.raw generate-architecture-html.xsl >$@

# coldframe-architecture.ps is made by printing the Rose Architecture package
# diagram (from coldframe-architecture.cat) to PostScript, from within Rose.

PDFS = coldframe-architecture.pdf \
  coldframe-callback.pdf \
  coldframe-relationships.pdf
pdf:: $(PDFS)

GIFS = States.gif States-Monitor.gif inheritance.gif lamp.gif lamp-state.gif
JPEGS = navigation.jpg window-screen.jpg
PNGS = hierarchies.png hierarchies-full.png discriminated-record.png \
 serialization.png serialization-class-model.png serialization-sequence-t.png \
 serialization-class-model-t.png serialization-state.png \
 serialization-state-t.png serialization-sequence.png \
 real_time.png recordable_real_time.png sample_a.png

############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works in Linux.
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DOCS = architecture.html \
associations.html \
attributes.html \
bugs.html \
classes.html \
compilation.html \
conversion.html \
directions.html \
domains.html \
error-messages.html \
events.html \
event-modelling.html \
event-motivation.html \
event-translation.html \
event-use.html \
extraction.html \
faq.html \
generalizations.html \
generation.html \
index.html \
installation.html \
operation-parameters.html \
operation-results.html \
operations.html \
preparation.html \
principles.html \
releases.html \
reserved-names.html \
resources.html \
serialization.html \
serialization-model.html \
strategy.html \
support.html \
target.html \
translation-rules.html \
types.html \
use-of-bcs.html \
use-cases.html use-cases.texi \
coldframe-architecture.html \
$(GIFS) $(JPEGS) $(PNGS) $(PDFS) \
coldframe-architecture.cat \
ddf.dtd coldframe.dtd \
xslide-diff

serialization-model.html: Serialization.html
	cp -p $< $@

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

TOOLS = generated_lines

TOOL_SRC = generated_lines.adb \
  generated_lines_support.ad[bs]

PROGS = COPYING \
  extractor-trampoline.ebs extractor.ebs rose-addin.mnu \
  escape-markup.awk \
  normalize-rose.tcl \
  $(HTMLGEN_SCRIPT) \
  $(CODEGEN_SCRIPTS) \
  $(TOOL_SRC)

GPRS = BC.gpr \
ColdFrame_Defaults.gpr \
Options.gpr

SUPPORT = \
coldframe-callbacks.adb \
coldframe-callbacks.ads \
coldframe-events_g-creation_g.adb \
coldframe-events_g-creation_g.ads \
coldframe-events_g-debug_g.adb \
coldframe-events_g-debug_g.ads \
coldframe-events_g-standard_g.adb \
coldframe-events_g-standard_g.ads \
coldframe-events_g-test_g.adb \
coldframe-events_g-test_g.ads \
coldframe-events_g.adb \
coldframe-events_g.ads \
coldframe-exceptions-message.adb \
coldframe-exceptions-message.ads \
coldframe-exceptions-symbolic_traceback.adb \
coldframe-exceptions-symbolic_traceback.ads \
coldframe-exceptions-traceback.adb \
coldframe-exceptions-traceback.ads \
coldframe-exceptions.ads \
coldframe-hash-access_hash.adb \
coldframe-hash-access_hash.ads \
coldframe-hash-combine_hash-test.adb \
coldframe-hash-combine_hash.adb \
coldframe-hash-combine_hash.ads \
coldframe-hash-instance_access_hash.ads \
coldframe-hash-strings-bounded.adb \
coldframe-hash-strings-bounded.ads \
coldframe-hash-strings-standard.adb \
coldframe-hash-strings-standard.ads \
coldframe-hash-strings-test.adb \
coldframe-hash-strings-unbounded.adb \
coldframe-hash-strings-unbounded.ads \
coldframe-hash-strings.adb \
coldframe-hash-strings.ads \
coldframe-hash.ads \
coldframe-events_g-held_event_queue_signature.ads \
coldframe-instances.ads \
coldframe-interrupts.adb \
coldframe-interrupts.ads \
coldframe-logging_signature.ads \
coldframe-project.ads \
coldframe-serialization.adb \
coldframe-serialization.ads \
coldframe-time_signature.ads \
coldframe.ads

PROJECT = \
coldframe-project-calendar.adb \
coldframe-project-calendar.ads \
coldframe-project-event_support.ads \
coldframe-project-events-creation.ads \
coldframe-project-events-standard.ads \
coldframe-project-events-standard-debug.ads \
coldframe-project-events-standard-test.ads \
coldframe-project-events.ads \
coldframe-project-held_events.adb \
coldframe-project-held_events.ads \
coldframe-project-held_event_support.ads \
coldframe-project-global_storage_pool.ads \
coldframe-project-global_storage_pool.ads-debug \
coldframe-project-global_storage_pool.ads-standard \
coldframe-project-logging_support.adb \
coldframe-project-logging_support.ads \
coldframe-project-serialization.ads \
coldframe-project-times.adb \
coldframe-project-times.ads

coldframe-project-global_storage_pool.ads-standard: \
  coldframe-project-global_storage_pool.ads
	cp -p $< $@

DEMO = \
Makefile-demo-unix Makefile-demo-winnt \
Stairwell-Demo.mdl \
Digital_IO.cat Digital_IO.raw \
Digital_IO.impl/digital_io-application-set_output.adb \
Digital_IO.impl/digital_io-hci-get_state.adb \
Digital_IO.impl/digital_io-hci-set_input.adb \
Digital_IO.impl/digital_io-input-changed.adb \
Digital_IO.impl/digital_io-output-changed.adb \
Digital_IO.impl/digital_io-signal-initialize.adb \
Digital_IO.impl/digital_io-signal-set_state.adb \
stairwell_demo.adp stairwell_demo.gpr tash.gpr \
stairwelllights.tcl house-2.gif \
House_Management.cat House_Management.raw \
House_Management.impl/house_management-button-changed.adb \
House_Management.impl/house_management-button-pushed.adb \
House_Management.impl/house_management-events-initialize.adb \
House_Management.impl/house_management-lamp-button_pushed.adb \
House_Management.impl/house_management-lamp-clear_timeout.adb \
House_Management.impl/house_management-lamp-initialize.adb \
House_Management.impl/house_management-lamp-set_timeout.adb \
House_Management.impl/house_management-lamp-turn_off.adb \
House_Management.impl/house_management-lamp-turn_on.adb \
House_Management.impl/stairwell_demo.adb

DEMO += \
Serialization.cat Serialization.raw Serialization_Demo.raw \
Serialization.html \
Serialization.impl/Serialization.gpr \
Serialization.impl/client.adb \
Serialization.impl/serialization-events-initialize.adb \
Serialization.impl/serialization-interface-open.adb \
Serialization.impl/serialization-interface-output.adb \
Serialization.impl/serialization-server-create.adb \
Serialization.impl/serialization-server-post.adb \
Serialization.impl/serialization_support.ads \
Serialization.impl/server.adb \
Serialization_Demo.impl/serialization_demo-real_time_image.adb

DEMO += \
Interrupt_Handling.cat Interrupt_Handling.raw \
Interrupt_Handling.impl/interrupt_handling-device-clear_timeout.adb \
Interrupt_Handling.impl/interrupt_handling-device-initialize.adb \
Interrupt_Handling.impl/interrupt_handling-device-report_entry.adb \
Interrupt_Handling.impl/interrupt_handling-device-set_timeout.adb \
Interrupt_Handling.impl/interrupt_handling-device-t.adb \
Interrupt_Handling.impl/interrupt_handling-events-initialize.adb \
Interrupt_Handling.impl/interrupt_handling-harness.adb \
Interrupt_Handling.impl/interrupt_handling-harness.ads

DEMO += \
Library.cat Library.raw \
Library.impl/library-test.adb \
Library.impl/library-test.ads \
Library.impl/library_test_harness.adb \
Library.impl/library-tests.adb \
Library.impl/library-tests.ads

DEMO += \
Problem_Reporting.cat Problem_Reporting.raw \
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
Problem_Reporting.impl/problem_reporting-interface-report_problems.adb

DEMO += \
States.cat States.raw \
States.impl/states-events-initialize.adb \
States.impl/states-monitor-clear_heartbeat_timeout.adb \
States.impl/states-monitor-clear_warmup_timeout.adb \
States.impl/states-monitor-receive.adb \
States.impl/states-monitor-set_2_second_warmup_timeout.adb \
States.impl/states-monitor-set_6_second_warmup_timeout.adb \
States.impl/states-monitor-set_heartbeat_timeout.adb \
States.impl/states-monitor-setup.adb \
States.impl/states-t.adb \
States.impl/states-t.ads

TEST = \
coldframe-events_test.adb \
coldframe-events_test_support.adb \
coldframe-events_test_support.ads \
Event_Test.cat Event_Test.raw \
Event_Test.gpr Event_Test_Test.gpr \
Event_Test.test/debugging.ads \
Event_Test.impl/event_test-events-initialize.adb \
Event_Test.test/event_test-harness.adb \
Event_Test.test/event_test-harness.ads \
Event_Test.impl/event_test-machine-handle_mark.adb \
Event_Test.impl/event_test-machine-handle_self.adb \
Event_Test.impl/event_test-machine-send_done.adb \
Event_Test.impl/event_test-recipient-handle_mark.adb \
Event_Test.impl/event_test-recipient-handle_self.adb \
Event_Test.impl/event_test-recipient-information_handler.adb \
Event_Test.impl/event_test-recipient-init.adb \
Event_Test.impl/event_test-recipient-send_done.adb \
Event_Test.impl/event_test-recipient-wait_handler.adb \
Event_Test.test/event_test-suite.adb \
Event_Test.test/event_test-suite.ads \
Event_Test.test/event_test-test_class.adb \
Event_Test.test/event_test-test_class.ads \
Event_Test.test/event_test-test_engine.adb \
Event_Test.test/event_test-test_engine.ads \
Event_Test.test/event_test-test_instance.adb \
Event_Test.test/event_test-test_instance.ads \
Event_Test.test/event_test-test_singleton_instance.adb \
Event_Test.test/event_test-test_singleton_instance.ads

TEST += \
Hierarchies.cat Hierarchies.raw \
Hierarchies.gpr Hierarchies_Test.gpr \
Hierarchies.impl/hierarchies-f_2-create_new.adb \
Hierarchies.test/hierarchies-harness.adb \
Hierarchies.test/hierarchies-harness.ads \
Hierarchies.test/hierarchies-suite.adb \
Hierarchies.test/hierarchies-suite.ads \
Hierarchies.test/hierarchies-test_creations.adb \
Hierarchies.test/hierarchies-test_creations.ads \
Hierarchies.test/hierarchies-test_deletions.adb \
Hierarchies.test/hierarchies-test_deletions.ads \
Hierarchies.test/hierarchies-test_finds.adb \
Hierarchies.test/hierarchies-test_finds.ads

DISTRIBUTION_FILES = \
cf-$(DATE).tgz \
cf-$(DATE).zip

dist: cf-$(DATE) $(DISTRIBUTION_FILES) $(DOCS)
	-@rm -rf dist
	mkdir -p dist/download
	cp -p $(DOCS) dist/
	cd dist && zip download/cf-html-$(DATE).zip *
	cp $(DISTRIBUTION_FILES) dist/download/

cf-$(DATE): $(MAKEFILES) $(GPRS) $(PROGS) $(SUPPORT) $(PROJECT) $(DEMO) \
$(TEST) force
	-rm -rf $@
	mkdir $@
	cp -p $(MAKEFILES) $(GPRS) $(PROGS) $@
	mkdir $@/lib
	cp -p $(SUPPORT) $@/lib
	mkdir $@/project
	cp -p $(PROJECT) $@/project
	mkdir $@/example
	tar cf - $(DEMO) | tar xf - -C $@/example
	mkdir $@/test
	tar cf - $(TEST) | tar xf - -C $@/test

cf-$(DATE).tgz: cf-$(DATE)
	-rm $@
	tar zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	-rm $@
	zip -r $@ $</*

.PHONY: force
