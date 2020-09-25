# The config options are in $SRCDIR ! That's ./src relative to here.  But we
# build in # $BLDDIR. You might as well go to $BLDDIR and run
#
#   $SRCDIR/configure --srcdir=$SRCDIR; make ; make install
#
# yourself.  This makefile will just do that for you. It'll choose as
# $BLDDIR an arch-specific subdir of the ./build directory. You can
# override that target by defining BLDDIR=foo on the call to this make,
# or in this file. It just needs to be writable.

# The configure stage makes a Makefile and Make.conf in the target
# (build) area. You should read the Make.conf file after running
# configure just to make sure (tm) that it has what you want.

# Version is defined for real in src/VERSION
#P_VERSION=260

# fixme if you can do better! This is not dumb enough for really dumb makes.
#ARCH=$(shell ( arch || uname -m ) 2>/dev/null || src/config.guess | sed -e 's/-.*//')
ARCH=`src/config.guess`
BLDDIR=build/$(ARCH)

###########################################################################
#  No user serviceable parts below. Look at Make.conf                     #
# 
#  We can be called as
#                   make 
#                   make all
#                   make test
#                   make clean

# this is only used for cleaning up. $(BLDDIR) is used o/w.
SUBDIRS=build

# in hope that make knows about .PHONY
.PHONY: dummy

# the "cd foo ; make" business is because we may not have "make -C foo"
all: config
	cd $(BLDDIR) ; $(MAKE) $@

config: $(BLDDIR)/configure

$(BLDDIR)/include: $(BLDDIR)
	mkdir -p $(BLDDIR)/include
$(BLDDIR):
	mkdir -p $(BLDDIR)

$(BLDDIR)/configure: $(BLDDIR)/include
	sh -c 'cwd=`pwd`; cd $(BLDDIR) ; \
        $$cwd/src/configure --srcdir=$$cwd/src'

install: $(BLDDIR)
	cd $(BLDDIR); export INSTALL_ROOT=$(INSTALL_ROOT); \
	  $(MAKE)  $@
	sh -c 'cwd=`pwd`; cd $(BLDDIR); blddir=`pwd`; cd $$cwd; \
	cd doc; export INSTALL_ROOT=$(INSTALL_ROOT); \
	  $(MAKE) $@ BLDDIR=$$blddir TOPDIR=$$cwd'

check: test

test: $(BLDDIR)
	cd $(BLDDIR); $(MAKE) $@

examples: dummy
	cd $(BLDDIR); $(MAKE) $@

clean: dummy
	rm -f *~ *.bak *.sw? core
	- sh -c 'cwd=`pwd`; \
        cd $(BLDDIR) 2>/dev/null && \
        ( TOPDIR=$$cwd; export TOPDIR; $(MAKE) $@ ) '

realclean: clean
	- sh -c 'cwd=`pwd`; \
        cd $(BLDDIR) 2>/dev/null && \
        ( TOPDIR=$$cwd; export TOPDIR; $(MAKE) realclean ) '
	rm -f src/config.cache src/Makefile
	rm -f $(BLDDIR)/config.cache $(BLDDIR)/Makefile
distclean: realclean
	- rm -fr $(BLDDIR)

#####

# make may not recognize .PHONY

dummy:

