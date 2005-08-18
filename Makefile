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
GENERATE_STUBS = no
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

AWK = awk
ITCLSH = tclsh
TCLXML = /usr/local/lib/tclxml-2.1theta

SAXON = java -cp /usr/local/lib/saxon/saxon.jar com.icl.saxon.StyleSheet

NORMALIZE_ROSE_SCRIPT = normalize-rose.tcl
HTMLGEN_SCRIPT = generate-html.xsl
Codegen_Script = generate-ada.xsl
ifeq ($(CODEGEN_SCRIPT), )
  CODEGEN_SCRIPT = $(Codegen_Script)
endif
CODEGEN_SCRIPTS = $(Codegen_Script) \
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
C_Codegen_Script = generate-c.xsl
ifeq ($(C_CODEGEN_SCRIPT), )
  C_CODEGEN_SCRIPT = $(C_Codegen_Script)
endif
C_CODEGEN_SCRIPTS = $(C_Codegen_Script) \
  c-utilities.xsl
OTHER_SCRIPTS = create-build-directories \
  serialized-to-csv.tcl \
  split-csv.tcl \
  make-build.tcl \
  case_exceptions.py

%.norm: $(COLDFRAMEOUT)/%.raw $(NORMALIZE_ROSE_SCRIPT)
	@echo generating $@ ...
	TCLLIBPATH=$(TCLXML) $(ITCLSH) $(NORMALIZE_ROSE_SCRIPT) \
	    --casing '$(CASE_EXCEPTIONS)' \
	    $(NORM_DOMAIN_NAME) \
	    $(NORM_STACK_DUMP) \
	    $(NORM_VERBOSE) \
	    <$< >$@ || (rm -f $@; exit 1)

%.html: %.norm $(HTMLGEN_SCRIPT)
	@echo generating $@ ...
	@$(SAXON) $< $(HTMLGEN_SCRIPT) >$@ || (rm -f $@; exit 1)

%.ada: %.norm $(CODEGEN_SCRIPTS)
	@echo generating $@ ...
	@$(SAXON) $< $(CODEGEN_SCRIPT) \
	  add-blank-lines=$(BLANK_LINES) \
	  generate-accessors=$(GENERATE_ACCESSORS) \
	  generate-stubs=$(GENERATE_STUBS) \
	  verbose=$(VERBOSE) \
	  >$@-t \
	  || (echo "Generation problem."; rm -f $@ $@-t; exit 1)
	@sed -e "s/LINES-OF-CODE/`tr -cd ';' <$@-t | wc -c | tr -d ' '`/" \
	  <$@-t >$@
	@rm -f $@-t

%.h: %.norm $(C_CODEGEN_SCRIPTS)
	@echo generating $@ ...
	@$(SAXON) $< $(C_CODEGEN_SCRIPT) \
	  add-blank-lines=$(BLANK_LINES) \
	  generate-accessors=$(GENERATE_ACCESSORS) \
	  verbose=$(VERBOSE) \
	  >$@ \
	  || (echo "Generation problem."; rm -f $@; exit 1)

# Delete the target directory & all contents.
# Create the target directory.
# Gnatchop the .ada file; allow overwrites (in case user-suplied
# additional processing steps generate actual implementations). Allow
# "failure" because GNAT 3.16a1 reports an error here.
# Remove any generated files which are also present in the implementation
# directory (.impl).
# Write-protect the generated files (careful, in case there are a lot of them!).
# Make the target directory itself writable (so users can delete files in it).
# Report unimplemented bodies.
%.gen: %.ada
	@echo generating $@ ...
	@-rm -rf $@
	@mkdir $@
	@-gnatchop -w $(CHOP_VERBOSE) $< $@
	@([ -d $*.impl ] && \
	for f in `(cd $*.impl; find . -maxdepth 1 -name \*.ad[bs])`; do \
	  if [ -f $@/$$f ]; then \
	    echo "    removing $@/$$f"; rm $@/$$f; \
	  else \
	    echo "  extra source file $$f in .impl"; \
	  fi \
	done) || true
	@chmod -R a-w $@
	@chmod u+w $@
	@echo "checking for unimplemented bodies ..."
	@grep -rl 'edit this' $@ || echo "... none."

# Split serialized XML files into CSV format, one per serialized type found.
%.csv: %.xml
	TCLLIBPATH=$(TCLXML) $(ITCLSH) $(CF)/serialized-to-csv.tcl <$<

# Creates the build directory (Ada Library) tree, under $BUILD_BASE
build-dirs::
	@[ -n "$(BUILD_BASE)" ] || (echo "BUILD_BASE must be set" && exit 1)
	@for d in aunit bc coldframe fixes main; do \
	  [ -d $(BUILD_BASE)/$$d ] || \
	    (echo mkdir -p $(BUILD_BASE)/$$d; mkdir -p $(BUILD_BASE)/$$d);\
	done

TEXI2HTML = texi2html
%.html: %.texi
	$(TEXI2HTML) -monolithic $<

PS2PDF = ps2pdf
%.pdf: %.ps
	$(PS2PDF) $<

all:: html

html:: use-cases.html coldframe-architecture.html

# Architecture.raw is extracted from the Rose Architecture package
# (coldframe-architecture.cat) using ddf.ebs
coldframe-architecture.html: Architecture.raw generate-architecture-html.xsl
	$(SAXON) \
	    Architecture.raw generate-architecture-html.xsl >$@

GIFS = States.gif States-Monitor.gif inheritance.gif
JPEGS = navigation.jpg window-screen.jpg
PNGS = \
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
serialization-class-model-t.png \
serialization-class-model.png \
serialization-sequence-t.png \
serialization-sequence.png \
serialization-state-t.png \
serialization-state.png \
serialization.png \
simple-association.png \
type-mapping.png


############################
# Distribution construction

# The subrelease is 'cvs' by default. Change on the command line for
# anything else.
SUBRELEASE = cvs

# Create the current date, in the form yyyymmdd. This certainly works in
# Linux.
#
# Used to construct release IDs (eg, cf-20050423cvs). You can set the
# whole thing from the command line -- for example, if creating a patch
# release.
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DOCS = \
active-classes.html \
annotations.html \
architecture.html \
associations.html \
attributes.html \
bugs.html \
callbacks.html \
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
extending.html \
extraction.html \
faq.html \
generalizations.html generalizations-extended.html \
generation.html \
index.html \
installation.html \
operation-parameters.html \
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
testing.html \
translation-rules.html \
types.html \
use.html \
use-of-bcs.html \
use-cases.html use-cases.texi \
xml-schemas.html States-check.xsv States-check.html \
coldframe-architecture.html \
$(GIFS) $(JPEGS) $(PNGS) $(PDFS) \
coldframe-architecture.cat \
ColdFrame-raw.xsd ColdFrame-norm.xsd \
xslide-diff \
House_Management.html Digital_IO.html

# We used to generate Serialization.html from Serialization.raw and
# copy it to serialization-model.html, but unfortunately Darwin's
# default file system is case-insensitive; which makes for confusion
# with the hand-written serialization.html.
serialization-model.raw: Serialization.raw
	cp $< $@

# The published makefiles are
# * Makefile-winnt for inclusion in a user makefile under Windows
# * Makefile-unix for inclusion in a user makefile under Unix.
# * test/Makefile for running the test pack.

MAKEFILES = Makefile-unix Makefile-winnt

# Files that need editing for release
Makefile-unix: Makefile-unix-proto force
	sed -e "s;DATE;$(DATE);g" <$< >$@
Makefile-winnt: Makefile-winnt-proto force
	sed -e "s;DATE;$(DATE);g" <$< >$@
extractor.ebs: ddf.ebs force
	sed -e "s;DATE;$(DATE);g" <$< >$@
releases.html: releases.html-proto force
	sed -e "s;DATE;$(DATE);g" <$< >$@

TOOLS = generated_lines

TOOL_SRC = generated_lines.adb \
  generated_lines_support.ad[bs]

PROGS = COPYING \
  extractor-trampoline.ebs extractor.ebs rose-addin.mnu \
  $(NORMALIZE_ROSE_SCRIPT) \
  cf-banner.el \
  $(HTMLGEN_SCRIPT) \
  $(CODEGEN_SCRIPTS) \
  $(OTHER_SCRIPTS) \
  $(TOOL_SRC)

GPRS = AUnit.gpr \
BC.gpr \
ColdFrame.gpr \
Options.gpr

SUPPORT = \
coldframe-bounded_storage_pools.adb \
coldframe-bounded_storage_pools.ads \
coldframe-callbacks.adb \
coldframe-callbacks.ads \
coldframe-event_basis.adb \
coldframe-event_basis.ads \
coldframe-events_g-creation_g.adb \
coldframe-events_g-creation_g.ads \
coldframe-events_g-debug_g.adb \
coldframe-events_g-debug_g.ads \
coldframe-events_g-standard_g.adb \
coldframe-events_g-standard_g.ads \
coldframe-events_g-test_g.adb \
coldframe-events_g-test_g.ads \
coldframe-events_g-trace_g.adb \
coldframe-events_g-trace_g.ads \
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
coldframe-hash-strings.ads \
coldframe-hash.ads \
coldframe-events_g-held_event_queue_signature.ads \
coldframe-instances.adb \
coldframe-instances.ads \
coldframe-interrupts.adb \
coldframe-interrupts.ads \
coldframe-logging_event_basis.adb \
coldframe-logging_event_basis.ads \
coldframe-logging_signature.ads \
coldframe-project.ads \
coldframe-serialization.adb \
coldframe-serialization.ads \
coldframe-serialization_signature.ads \
coldframe-stubs.adb \
coldframe-stubs.ads \
coldframe-time_signature.ads \
coldframe-unbounded_storage_pools.adb \
coldframe-unbounded_storage_pools.ads \
coldframe.ads

PROJECT = \
coldframe-project-calendar.adb \
coldframe-project-calendar.ads \
coldframe-project-event_support.ads \
coldframe-project-events-creation.ads \
coldframe-project-events-standard.ads \
coldframe-project-events-standard-debug.ads \
coldframe-project-events-standard-test.ads \
coldframe-project-events-standard-trace.ads \
coldframe-project-events-standard-test_debug.ads \
coldframe-project-events-standard-test_trace.ads \
coldframe-project-events.ads \
coldframe-project-events.ads-standard \
coldframe-project-events.ads-logging \
coldframe-project-global_storage_pool.ads \
coldframe-project-held_events.adb \
coldframe-project-held_events.ads \
coldframe-project-held_events-signature.ads \
coldframe-project-high_resolution_time.ads \
coldframe-project-logging_support.adb \
coldframe-project-logging_support.ads \
coldframe-project-log_error.ads \
coldframe-project-log_error.adb \
coldframe-project-serialization.ads \
coldframe-project-storage_pools.ads \
coldframe-project-times.adb \
coldframe-project-times.ads

coldframe-project-events.ads-standard: coldframe-project-events.ads
	cp -p $< $@

EXTRAS = \
coldframe-logging_event_basis-ews_support.adb \
coldframe-logging_event_basis-ews_support.ads

DEMO = \
Makefile-demo-unix Makefile-demo-winnt \
Examples.mdl

DEMO += \
stairwell_demo.gpr tash.gpr \
stairwelllights.tcl house-2.gif \
Stairwell-Demo.mdl \
Digital_IO.cat Digital_IO_Interface.cat \
Digital_IO.raw Digital_IO_Interface.raw \
Digital_IO.impl/digital_io-application-set_output.adb \
Digital_IO.impl/digital_io-hci-get_state.adb \
Digital_IO.impl/digital_io-hci-set_input.adb \
Digital_IO.impl/digital_io-input-changed.adb \
Digital_IO.impl/digital_io-output-changed.adb \
Digital_IO.impl/digital_io-signal-initialize.adb \
Digital_IO.impl/digital_io-signal-set_state.adb \
House_Management.cat House_Management.raw \
House_Management.impl/house_management-button-changed.adb \
House_Management.impl/house_management-button-pushed.adb \
House_Management.impl/house_management-events-initialize.adb \
House_Management.impl/house_management-lamp-clear_timeout.adb \
House_Management.impl/house_management-lamp-initialize.adb \
House_Management.impl/house_management-lamp-set_timeout.adb \
House_Management.impl/house_management-lamp-turn_off.adb \
House_Management.impl/house_management-lamp-turn_on.adb \
House_Management.impl/stairwell_demo.adb

DEMO += \
Serialization.cat \
Serialization.raw Serialization_Demo.raw Serialization_Demo_Other.raw \
serialization-model.html Serialization.gpr \
Serialization.impl/serialization_client.adb \
Serialization.impl/serialization-events-initialize.adb \
Serialization.impl/serialization-interface-open.adb \
Serialization.impl/serialization-interface-output.adb \
Serialization.impl/serialization-server-create.adb \
Serialization.impl/serialization-server-post.adb \
Serialization.impl/serialization_support.ads \
Serialization.impl/serialization_server.adb \
Serialization_Demo.impl/serialization_demo-real_time_image.adb

DEMO += \
Interrupt_Handling.cat Interrupt_Handling.raw Interrupt_Handling.gpr \
Interrupt_Handling.impl/interrupt_handling-device-clear_timeout.adb \
Interrupt_Handling.impl/interrupt_handling-device-initialize.adb \
Interrupt_Handling.impl/interrupt_handling-device-report_entry.adb \
Interrupt_Handling.impl/interrupt_handling-device-set_timeout.adb \
Interrupt_Handling.impl/interrupt_handling-device-t.adb \
Interrupt_Handling.impl/interrupt_handling-events-initialize.adb \
Interrupt_Handling.impl/interrupt_handling-harness.adb \
Interrupt_Handling.impl/interrupt_handling-harness.ads

DEMO += \
Library.cat Library.raw Library.gpr \
Library.impl/library-test.adb \
Library.impl/library-test.ads \
Library.impl/library_test_harness.adb \
Library.impl/library-tests.adb \
Library.impl/library-tests.ads

DEMO += \
Performance.cat Performance.raw Performance.gpr \
Performance.impl/performance-cat-speak.adb \
Performance.impl/performance-event_timing.adb \
Performance.impl/performance-event_timing.ads \
Performance.impl/performance-harness.adb \
Performance.impl/performance-harness.ads \
Performance.impl/performance-pet-eat.adb

DEMO += \
Problem_Reporting.cat Problem_Reporting.raw Problem_Reporting.gpr \
Problem_Reporting.impl/problem_reporting_harness.adb \
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
States.cat States.raw States.gpr \
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

TEST = Test.mdl

TEST += \
coldframe-events_test.adb \
coldframe-events_test_support.adb \
coldframe-events_test_support.ads \
Event_Test.cat Event_Test.raw Event_Test.gpr \
Event_Test.impl/event_test-completion_transitions-a1.adb \
Event_Test.impl/event_test-completion_transitions-a2.adb \
Event_Test.impl/event_test-completion_transitions-a3.adb \
Event_Test.impl/event_test-events-initialize.adb \
Event_Test.impl/event_test-machine-handle_mark.adb \
Event_Test.impl/event_test-machine-handle_self.adb \
Event_Test.impl/event_test-machine-send_done.adb \
Event_Test.impl/event_test-machine-set_timer.adb \
Event_Test.impl/event_test-recipient-handle_mark.adb \
Event_Test.impl/event_test-recipient-handle_self.adb \
Event_Test.impl/event_test-recipient-information_handler.adb \
Event_Test.impl/event_test-recipient-init.adb \
Event_Test.impl/event_test-recipient-send_done.adb \
Event_Test.impl/event_test-recipient-wait_handler.adb \
Event_Test.impl/event_test-timer_task_teardown-e1_handler.adb \
Event_Test.impl/event_test-timer_task_teardown-e2_handler.adb \
Event_Test.impl/event_test-timer_task_teardown-start.adb \
Event_Test.impl/event_test-timer_task_teardown-t.adb \
Event_Test.test/debugging.ads \
Event_Test.test/event_test-harness.adb \
Event_Test.test/event_test-harness.ads \
Event_Test.test/event_test-suite.adb \
Event_Test.test/event_test-suite.ads \
Event_Test.test/event_test-test_class.adb \
Event_Test.test/event_test-test_class.ads \
Event_Test.test/event_test-test_completion_transitions.adb \
Event_Test.test/event_test-test_completion_transitions.ads \
Event_Test.test/event_test-test_engine.adb \
Event_Test.test/event_test-test_engine.ads \
Event_Test.test/event_test-test_instance.adb \
Event_Test.test/event_test-test_instance.ads \
Event_Test.test/event_test-test_queue.adb \
Event_Test.test/event_test-test_queue.ads \
Event_Test.test/event_test-test_singleton_instance.adb \
Event_Test.test/event_test-test_singleton_instance.ads \
Event_Test.test/event_test-test_timer_task_teardown.adb \
Event_Test.test/event_test-test_timer_task_teardown.ads

TEST += \
Hierarchies.cat Hierarchies.raw Hierarchies.gpr \
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

TEST += \
Regressions.cat Regressions.raw Regressions.gpr \
Regressions.impl/regressions-class_with_private_init_operation-private_init_operation.adb\
Regressions.impl/regressions-event_holder-set_timer.adb \
Regressions.impl/regressions-event_holder-t.adb \
Regressions.impl/regressions-find_active_singleton-t.adb \
Regressions.impl/regressions-find_active-t.adb \
Regressions.impl/regressions-max_more-f.adb \
Regressions.impl/regressions-max_more-t.adb \
Regressions.impl/regressions-max_one-f.adb \
Regressions.impl/regressions-max_one-t.adb \
Regressions.impl/regressions-pt_holder.adb \
Regressions.impl/regressions-pt_owner-get_h_access.adb \
Regressions.impl/regressions-pt_user-get_state.adb \
Regressions.impl/regressions-pt_user-set_state.adb \
Regressions.impl/regressions-rule-create.adb \
Regressions.impl/regressions-self_immolator-t.adb \
Regressions.impl/regressions-self_immolator-terminate_self.adb \
Regressions.impl/regressions-self_immolator-terminate_yourself.adb \
Regressions.impl/regressions-suite.adb \
Regressions.impl/regressions-suite.ads \
Regressions.impl/regression_tests.adb

TEST += \
Stub_Test_Interface.cat Stub_Test_Interface.raw Stub_Test.gpr \
Stub_Test.test/stub_test_harness.adb \
Stub_Test.test/stub_test_suite.adb \
Stub_Test.test/stub_test_suite.ads

TEST += \
Bad_Code_Regressions.cat Bad_Code_Regressions.raw \
Bad_Model_Regressions.cat Bad_Model_Regressions.raw

DISTRIBUTION_FILES = \
cf-$(DATE).tgz \
cf-$(DATE).zip

# Documentation upload to SF

SFUSER ?= simonjwright

upload-docs: $(DOCS) force
	rsync \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  $(DOCS) \
	  $(SFUSER)@shell.sourceforge.net:/home/groups/c/co/coldframe/htdocs/

# The complete distribution

dist: cf-$(DATE) $(DISTRIBUTION_FILES) $(DOCS)
	-@rm -rf dist
	mkdir -p dist/download
	cp -p $(DOCS) dist/
	cd dist && zip -9 download/cf-html-$(DATE).zip *
	cp $(DISTRIBUTION_FILES) dist/download/

# Files that need DATE substituted
DATED_FILES = \
 normalize-rose.tcl \
 ada-utilities.xsl \
 generate-c.xsl

cf-$(DATE): $(MAKEFILES) $(GPRS) $(PROGS) $(SUPPORT) $(PROJECT) $(EXTRAS) \
$(DEMO) $(TEST) Makefile-test force
	-rm -rf $@
	mkdir $@
	cp -p $(MAKEFILES) $(GPRS) $(PROGS) $@
	for f in $(DATED_FILES); do \
	    (sed -e "s;DATE;$(DATE);g" <$$f >$@/$$f) \
	done
	mkdir $@/lib
	cp -p $(SUPPORT) $@/lib
	mkdir $@/project
	cp -p $(PROJECT) $@/project
	mkdir $@/extras
	cp -p $(EXTRAS) $@/extras
	mkdir $@/example
	tar cf - $(DEMO) | tar xf - -C $@/example
	mkdir $@/test
	tar cf - $(TEST) | tar xf - -C $@/test
	cp -p Makefile-test $@/test/Makefile

cf-$(DATE).tgz: cf-$(DATE)
	-rm $@
	tar zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	-rm $@
	zip -r -9 $@ $</*

.PHONY: force
