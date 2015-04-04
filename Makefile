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

all::
	@echo "Say 'make setup' if you wish to set ColdFrame up."

# See Makefile.inc for overridable variable definitions.
include Makefile.inc

# Set up the library
setup:: force
	gprbuild -p -P ColdFrame

# Set up subdirectories
SETUP_SUBDIRS = lib tools examples
setup::
	for s in $(SETUP_SUBDIRS); do		\
	  make -C $$s setup;			\
	done

# Used to construct release IDs (eg, cf-20050423hg). You can set the
# whole thing from the command line -- for example, if creating a patch
# release.
SUBRELEASE = hg
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DISTRIBUTION_FILES = cf-$(DATE).tgz cf-$(DATE).zip

# Documentation upload to SF

SFUSER ?= simonjwright

upload-docs:: doc/top-index.html
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
	  doc/top-index.html \
	  $(SFUSER),coldframe@web.sourceforge.net:htdocs/index.html

upload-docs:: doc/cf.css
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
	  doc/cf.css \
	  $(SFUSER),coldframe@web.sourceforge.net:htdocs/

upload-docs:: cf-$(DATE)
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
	  cf-$(DATE)/doc/* \
	  $(SFUSER),coldframe@web.sourceforge.net:htdocs/coldframe

# The complete distribution

dist: $(DISTRIBUTION_FILES)

dist-files: coldframeout cf-$(DATE)

coldframeout:
	$(MKDIR) $@

TOP_LEVEL_ITEMS =				\
  COPYING					\
  Makefile.inc					\
  ColdFrame.gpr					\
  Options.gpr					\
  ColdFrame_Ravenscar.gpr			\
  Options_Ravenscar.gpr				\
  debian-6.diff

SUBDIRS = doc examples extras lib models project scripts test tools

cf-$(DATE): force
	-$(RM) -rf $@
	$(MKDIR) -p $@/coldframeout
	$(CP) -p $(TOP_LEVEL_ITEMS) $@/
	$(CP) -p Makefile-for-distribution $@/Makefile
	for s in $(SUBDIRS); do						\
	  make -C $$s -f Makefile.dist DIST=$(COLDFRAME)/$@ dist;	\
	done

cf-$(DATE).tgz: cf-$(DATE)
	-$(RM) $@
	$(TAR) zcvf $@ $</

cf-$(DATE).zip: cf-$(DATE)
	-$(RM) $@
	$(ZIP) -r -9 $@ $</*

.PHONY: all dist force setup upload-docs
