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

# Makefile for examples, use with Cygwin under Windows or Unix
# (assumes a Unix system if Cygwin not detected).

# It's assumed that no project-special settings are in place and that
# the command 'gprbuild' will compile and generate an executable.

# It's assumed that this is still a subsubdirectory of the ColdFrame
# install directory, and that we have write access (OK on Windows, may
# require admin cooperation on Unix).

all::
setup::
clean::

PROFILE=ravenscar
ADA_PROJECT_PATH=../..:$(HOME)/Developer/cortex-gnat-rts/local/microbit
RTS=$(HOME)/Developer/cortex-gnat-rts/local/microbit

include ../../Makefile.inc

EXECUTABLES = simple_buttons.hex

simple_buttons: Simple_Buttons.gen Digital_IO.gen force
	gprbuild -P simple_buttons.gpr		\
	  --target=arm-eabi			\
	  -XRTS=$(RTS)				\
	  -XBUILD=Production			\
	  -XPLATFORM_BUILD=Production		\
	  -XADL_BUILD=Production		\
	  -XADL_BUILD_CHECKS=Disabled

%.hex: %
	arm-eabi-objcopy -O ihex $^ $@

all:: $(EXECUTABLES)

# ArgoUML support

NORM_FILES =					\
 Digital_IO.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(NORM_FILES): ColdFrame_Examples.norm-stamp ;

# TextUML support

TUML_NORM_FILES =				\
Simple_Buttons.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(TUML_NORM_FILES): Simple_Buttons.tuml-norm-stamp ;

setup::
	-ln -s ../Digital_IO.impl .
	-ln -s ../Simple_Buttons.impl .

clean::
	-for g in *.gpr; do			\
	  gprclean -r -P $$g -XRTS=microbit;	\
	done
	rm -f Digital_IO.impl Simple_Buttons.impl
	rm -rf *.{hex,map,norm-stamp,stamp,norm,ada,gen,html,images}

.PHONY: force all setup clean
