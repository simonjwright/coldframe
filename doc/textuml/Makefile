all::
setup::
clean::

ADA_PROJECT_PATH=../..

include ../../Makefile.inc

# override where model files are looked for
MODELS = .

# nuweb
NUWEB ?= ~/nuweb.py/nuweb.py

all:: Simple_Buttons.gen
all:: Test_It.gen

# twice round the block isn't quite enough!
%.pdf: %.w
	$(NUWEB) -r $(basename $<)
	pdflatex -r $(basename $<)
	$(NUWEB) -r $(basename $<)
	pdflatex -r $(basename $<)
	pdflatex -r $(basename $<)

%.tuml: %.w
	$(NUWEB) -t $(basename $<)

# TextUML support

NORM_FILES =					\
Simple_Buttons.norm				\
Test_It.norm

# NB! the ';' at the end is important - without it, Make may not know
# to rebuild the .gen directories.
$(NORM_FILES): test.tuml-norm-stamp textuml.tuml-norm-stamp ;

clean::
	-for g in *.gpr; do			\
	  gprclean -P $$g;			\
	done
	rm -rf *.{bin,map,norm-stamp,tuml-norm-stamp,stamp,norm,ada,gen,html,images}

.PHONY: force all setup clean
