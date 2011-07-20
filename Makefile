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

# See Makefile.inc for overridable variable definitions.
include Makefile.inc

CODEGEN_SCRIPTS = generate-ada.xsl \
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
  ada-unittest.xsl \
  ada-utilities.xsl
DOCGEN_SCRIPTS = $(HTMLGEN_SCRIPT) $(DIAGRAM_SCRIPT)

C_CODEGEN_SCRIPT ?= generate-c.xsl
C_CODEGEN_SCRIPTS = generate-c.xsl c-utilities.xsl

OTHER_SCRIPTS = \
  case_exceptions.py \
  cat2raw.py \
  create-build-directories \
  make-build.tcl \
  serialized-to-csv.tcl \
  split-csv.tcl

# Split serialized XML files into CSV format, one per serialized type found.
%.csv: %.xml
	TCLLIBPATH=$(TCLXML) $(ITCLSH) $(CF)/serialized-to-csv.tcl <$<

############################
# Distribution construction

# The subrelease is 'hg' by default. Change on the command line for
# anything else.
SUBRELEASE = hg

# Create the current date, in the form yyyymmdd. This certainly works in
# Linux.
#
# Used to construct release IDs (eg, cf-20050423hg). You can set the
# whole thing from the command line -- for example, if creating a patch
# release.
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

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
ColdFrame-raw.xsd ColdFrame-norm.xsd \
xslide-diff

# These are the parts of doc generated from models. Generation also
# generates {Domain}.images/, not mentioned as part of the target.
GEN_DOCS = \
Serialization.html \
Digital_IO.html \
House_Management.html

GEN_DOC_IMAGES = $(subst .html,.images,$(GEN_DOCS))

docs: $(DOCS) $(GEN_DOCS)

# The published makefiles are
# * Makefile-winnt for inclusion in a user makefile under Windows.
# * Makefile-unix for inclusion in a user makefile under Unix.
# * test/Makefile for running the test pack, copied from Makefile-test.

MAKEFILES = Makefile-unix Makefile-winnt

# Files that need editing for release
Makefile-unix: Makefile-unix-proto force
	$(SED) -e "s;DATE;$(DATE);g" <$< >$@
Makefile-winnt: Makefile-winnt-proto force
	$(SED) -e "s;DATE;$(DATE);g" <$< >$@
extractor.ebs: ddf.ebs force
	$(SED) -e "s;DATE;$(DATE);g" <$< >$@
releases.html: releases.html-proto force
	$(SED) -e "s;DATE;$(DATE);g" <$< >$@

TOOLS = generated_lines serialized_to_csv

TOOL_SRC = \
  generated_lines.adb generated_lines_support.ad[bs] \
  serialized_to_csv.adb serialized_to_csv_support.ads

PROGS = COPYING \
  extractor-trampoline.ebs extractor.ebs rose-addin.mnu \
  $(NORMALIZE_RAW_SCRIPT) \
  $(NORMALIZE_ROSE_SCRIPT) \
  cf-banner.el \
  $(DOCGEN_SCRIPTS) \
  $(CODEGEN_SCRIPTS) \
  $(OTHER_SCRIPTS)

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
coldframe-events_g-held_event_queue_signature-inspection_signature.ads \
coldframe-events_g-monitoring_g.adb \
coldframe-events_g-monitoring_g.ads \
coldframe-events_g-standard_g-inspection_g.adb \
coldframe-events_g-standard_g-inspection_g.ads \
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
coldframe-project.ads \
coldframe-serialization.adb \
coldframe-serialization.ads \
coldframe-serialization_signature.ads \
coldframe-stubs.adb \
coldframe-stubs.ads \
coldframe-task_deletion_g.adb \
coldframe-task_deletion_g.ads \
coldframe-time_signature.ads \
coldframe-unbounded_debug_storage_pools.adb \
coldframe-unbounded_debug_storage_pools.ads \
coldframe-unbounded_storage_pools.adb \
coldframe-unbounded_storage_pools.ads \
coldframe-unbounded_strings.adb \
coldframe-unbounded_strings.ads \
coldframe.ads

PROJECT = \
coldframe-project-calendar.adb \
coldframe-project-calendar.ads \
coldframe-project-event_support.ads \
coldframe-project-events-creation.ads \
coldframe-project-events-monitoring.ads \
coldframe-project-events-monitoring-test.ads \
coldframe-project-events-standard-debug.ads \
coldframe-project-events-standard-inspection.ads \
coldframe-project-events-standard-test.ads \
coldframe-project-events-standard-test_debug.ads \
coldframe-project-events-standard-test_trace.ads \
coldframe-project-events-standard-trace.ads \
coldframe-project-events-standard.ads \
coldframe-project-events.ads \
coldframe-project-events.ads-logging \
coldframe-project-events.ads-standard \
coldframe-project-global_storage_pool.ads \
coldframe-project-held_events-inspection.adb \
coldframe-project-held_events-inspection.ads \
coldframe-project-held_events-inspection_signature.ads \
coldframe-project-held_events-signature.ads \
coldframe-project-held_events.adb \
coldframe-project-held_events.ads \
coldframe-project-high_resolution_time.ads \
coldframe-project-limits.adb \
coldframe-project-limits.ads \
coldframe-project-log_error.adb \
coldframe-project-log_error.ads \
coldframe-project-log_info.adb \
coldframe-project-log_info.ads \
coldframe-project-logging_support.adb \
coldframe-project-logging_support.ads \
coldframe-project-serialization.ads \
coldframe-project-storage_pools.ads \
coldframe-project-task_deletion.adb \
coldframe-project-task_deletion.ads \
coldframe-project-times.adb \
coldframe-project-times.ads

coldframe-project-events.ads-standard: coldframe-project-events.ads
	$(CP) -p $< $@

EXTRAS = \
coldframe-logging_event_basis-ews_support.adb \
coldframe-logging_event_basis-ews_support.ads

EXAMPLES = Examples.mdl

EXAMPLES += \
stairwell_demo.gpr \
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

EXAMPLES += \
House_Management.test/House_Management_Test.gpr \
House_Management.test/house_management-harness.adb \
House_Management.test/house_management-test_suite.adb \
House_Management.test/house_management-test_suite.ads

EXAMPLES += \
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

EXAMPLES += \
Interrupt_Handling.cat Interrupt_Handling.raw Interrupt_Handling.gpr \
Interrupt_Handling.impl/interrupt_handling-device-clear_timeout.adb \
Interrupt_Handling.impl/interrupt_handling-device-initialize.adb \
Interrupt_Handling.impl/interrupt_handling-device-report_entry.adb \
Interrupt_Handling.impl/interrupt_handling-device-set_timeout.adb \
Interrupt_Handling.impl/interrupt_handling-device-t.adb \
Interrupt_Handling.impl/interrupt_handling-events-initialize.adb \
Interrupt_Handling.impl/interrupt_handling-harness.adb \
Interrupt_Handling.impl/interrupt_handling-harness.ads

EXAMPLES += \
Library.cat Library.raw Library.gpr \
Library.impl/library-test.adb \
Library.impl/library-test.ads \
Library.impl/library_test_harness.adb \
Library.impl/library-tests.adb \
Library.impl/library-tests.ads

EXAMPLES += \
Performance.cat Performance.raw Performance.gpr \
Performance.impl/performance-cat-speak.adb \
Performance.impl/performance-event_timing.adb \
Performance.impl/performance-event_timing.ads \
Performance.impl/performance-harness.adb \
Performance.impl/performance-harness.ads \
Performance.impl/performance-pet-eat.adb

EXAMPLES += \
Problem_Reporting.cat Problem_Reporting.raw Problem_Reporting.gpr \
Problem_Reporting.impl/problem_reporting_harness.adb \
Problem_Reporting.impl/problem_reporting-component-clean.adb \
Problem_Reporting.impl/problem_reporting-component-report.adb \
Problem_Reporting.impl/problem_reporting-component-report_all.adb \
Problem_Reporting.impl/problem_reporting-defect-report.adb \
Problem_Reporting.impl/problem_reporting-public-add_component.adb \
Problem_Reporting.impl/problem_reporting-public-add_problem.adb \
Problem_Reporting.impl/problem_reporting-public-delete_component.adb \
Problem_Reporting.impl/problem_reporting-public-note_defect.adb \
Problem_Reporting.impl/problem_reporting-public-reject_problem.adb \
Problem_Reporting.impl/problem_reporting-public-report_problems.adb

EXAMPLES += \
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
Compilation_Regressions.cat Compilation_Regressions.raw \
Compilation_Regressions.impl/compilation_regressions-aliased_components-check_access.adb \
Compilation_Regressions.impl/compilation_regressions-subunits.adb \
Compilation_Regressions.impl/compilation_regressions-subunits.ads

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
Event_Test.test/event_test-test_inspection.adb \
Event_Test.test/event_test-test_inspection.ads \
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
Initialization.cat Initialization.raw Initialization.gpr \
Initialization.impl/initialization-checking_domain_initialization-initialize.adb \
Initialization.impl/initialization-checking_domain_initialization-t.adb \
Initialization.impl/initialization-harness.adb \
Initialization.impl/initialization-harness.ads \
Initialization.impl/initialization-suite.adb \
Initialization.impl/initialization-suite.ads \
Initialization.impl/initialization-suite-check_domain_initialization.adb

TEST += \
Regressions.cat Regressions.raw Regressions.gpr \
Regressions.impl/regressions-event_holder-set_timer.adb \
Regressions.impl/regressions-event_holder-t.adb \
Regressions.impl/regressions-find_active_singleton-t.adb \
Regressions.impl/regressions-find_active-t.adb \
Regressions.impl/regressions-max_more-f.adb \
Regressions.impl/regressions-max_more-t.adb \
Regressions.impl/regressions-max_one-f.adb \
Regressions.impl/regressions-max_one-t.adb \
Regressions.impl/regressions-phoenix-reincarnate.adb \
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
Unit_Testing.cat Unit_Testing.raw Unit_Testing.gpr \
Unit_Testing.impl/unit_testing-arr-post_c.adb \
Unit_Testing.impl/unit_testing-normal-post_c.adb \
Unit_Testing.impl/unit_testing-singleton-post_c.adb \
Unit_Testing.test/unit_testing-harness.adb \
Unit_Testing.test/unit_testing-suite.adb \
Unit_Testing.test/unit_testing-suite.ads

TEST += \
Bad_Code_Regressions.cat Bad_Code_Regressions.raw \
Bad_Model_Regressions.cat Bad_Model_Regressions.raw

DISTRIBUTION_FILES = \
cf-$(DATE).tgz \
cf-$(DATE).zip

# Documentation upload to SF

SFUSER ?= simonjwright

upload-docs: top-index.html doc force
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

old-dist: cf-$(DATE) $(DISTRIBUTION_FILES) $(DOCS) $(GEN_DOCS)
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

old-cf-$(DATE): $(MAKEFILES) $(GPRS) $(PROGS) $(SUPPORT) $(PROJECT) $(EXTRAS) \
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

dist: cf-$(DATE)

TOP_LEVEL_ITEMS =				\
  Makefile.inc					\
  ColdFrame.gpr					\
  Options.gpr

SUBDIRS = doc examples extras lib models project scripts test

cf-$(DATE): force
	-$(RM) -rf $@
	$(MKDIR) -p $@/coldframeout
	$(CP) -p $(TOP_LEVEL_ITEMS) $@/
	for s in $(SUBDIRS); do						\
	  make -C $$s -f Makefile.dist DIST=$(COLDFRAME)/$@ dist;	\
	done

cf-$(DATE).tgz: cf-$(DATE)
	-$(RM) $@
	$(TAR) zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	-$(RM) $@
	$(ZIP) -r -9 $@ $</*

.PHONY: force
