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

# This Makefile is used for documentation generation.

include ../Makefile.inc

# This is to be a directory containing the gh-pages branch of the
# Github repo.
DEST ?= $(HOME)/coldframe-pages

all::

DATE = $(subst cf-,,$(notdir $(DIST)))

DOCS =						\
  cf.css					\
  active-classes.html				\
  analysis.html					\
  annotations.html				\
  architecture.html				\
  associations.html				\
  attributes.html				\
  bugs.html					\
  building-for-scripting.html			\
  callbacks.html				\
  classes.html					\
  coldframe-architecture.html			\
  compilation.html				\
  conversion.html				\
  copying.html					\
  documentation.html				\
  domains.html					\
  error-messages.html				\
  events.html					\
  event-modelling.html				\
  event-motivation.html				\
  event-translation.html			\
  event-use.html				\
  extending.html				\
  faq.html					\
  generalizations.html				\
  generalizations-extended.html			\
  generation.html				\
  index.html					\
  initialization.html				\
  installation.html				\
  operation-parameters.html			\
  operations.html				\
  preparation.html				\
  principles.html				\
  releases.html					\
  reserved-names.html				\
  resources.html				\
  scripting-callbacks.html			\
  scripting-event-queue.html			\
  scripting-stubs.html				\
  serialization-page.html			\
  strategy.html					\
  stubs.html					\
  support.html					\
  support-ravenscar.html			\
  target.html					\
  testing.html					\
  testing-class.html				\
  testing-domain.html				\
  translation-rules.html			\
  types.html					\
  use-of-containers.html			\
  ColdFrame-norm.xsd

GIFS = States.gif States-Monitor.gif house-2.gif

JPEGS = navigation.jpg window-screen.jpg

PNGS =						\
Screenshot-Project_Properties-Profiles.png	\
Screenshot-Settings-Diagram_Appearance.png	\
Screenshot-Settings-Notations.png		\
Screenshot-Settings-Profiles.png		\
Screenshot-Settings-User.png			\
argouml-installation.html			\
better-association.png				\
browse-state-model.png				\
button-class-state-machine.png			\
class-mappings.png				\
collections-mapping.png				\
digital-io-interface.png			\
digital-io.png					\
discriminated-record.png			\
event-mapping.png				\
event-payload.png				\
hierarchies-full.png				\
hierarchies.png					\
house-digital-io.png				\
house-management-operation.png			\
house-management.png				\
inheritance.png					\
lamp-button-event-browsing.png			\
lamp-state-resetting.png			\
lamp-state.png					\
lamp.png					\
metamodel.png					\
operations-mapping.png				\
real_time.png					\
recordable_real_time.png			\
reflexive.png					\
relationships-mapping.png			\
sample_a.png					\
save-project-as-uml.png				\
serialization-class-model-t.png			\
serialization-class-model.png			\
serialization-sequence-t.png			\
serialization-sequence.png			\
serialization-state-t.png			\
serialization-state.png				\
serialization.png				\
simple-association.png				\
type-mapping.png				\
vague-association.png

ALL = $(DOCS) $(GIFS) $(JPEGS) $(PNGS)

# Files that need DATE substituted
DATED_FILES =					\
 releases.html

# Files that need to be generated
GENERATED_FILES =				\
  ColdFrameProfile.html				\
  self-events.png				\
  use-cases.html

GENERATED_DOCUMENTS =				\
  Digital_IO.html				\
  House_Management.html

GENERATED_IMAGES = $(subst .html,.images,$(GENERATED_DOCUMENTS))

NORM_FILES = $(subst .html,.norm,$(GENERATED_DOCUMENTS))

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(NORM_FILES): Housing.tuml-norm-stamp ;

# graphviz:
%.png: %.dot
	$(DOT) -o $@ -Tpng $<

ColdFrameProfile.html: $(COLDFRAMEOUT)/ColdFrameProfile.rr
	$(SAXON) $< $(COLDFRAME)/scripts/generate-profile-html.xsl >$@

all::
	$(CP) -p $(ALL) $(DEST)/

all::
	for f in $(DATED_FILES); do \
	    ($(SED) -e "s;DATE;$(DATE);g" <$$f >$(DEST)/$$f) \
	done

all:: $(GENERATED_FILES) $(GENERATED_DOCUMENTS) $(GENERATED_IMAGES)
	$(TAR) cf -							  \
	  $(GENERATED_FILES) $(GENERATED_DOCUMENTS) $(GENERATED_IMAGES) | \
	  tar xf - -C $(DEST)/

.PHONY: all force
