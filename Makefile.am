#
# This file is processed by automake (produces file Makefile.in)
#
ACLOCAL_AMFLAGS = -I autoconf

CLEANFILES = *.o *.mod *~ bin/*exe

TESTSUITE = $(srcdir)/Sources/testsuite

SUBDIRS = Sources  Sources/testsuite


# Additional files or directories to be distributed

EXTRA_DIST  = LICENSE emis_2016.sh util namelist_emis.nml
EXTRA_DIST += $(TESTSUITE)

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):
