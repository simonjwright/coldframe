# Populate containers/ with files from minimal_containers.

# If minimal_containers is pinned locally, there'll be no environment
# variable MINIMAL_CONTAINERS_ALIRE_PREFIX, so assume they're in
# ../../minimal_containers.

MINIMAL_CONTAINERS_ALIRE_PREFIX ?= ../../minimal_containers

sources = $(wildcard $(MINIMAL_CONTAINERS_ALIRE_PREFIX)/src/*.ad[bs])

all:

setup:
	cat $(MINIMAL_CONTAINERS_ALIRE_PREFIX)/src/*.ad[bs] |		 \
	  sed -e "s/Minimal_Containers/ColdFrame.Containers/g" >all.ada; \
	mkdir -p containers;						 \
	gnatchop -w all.ada containers;					 \
	rm all.ada
