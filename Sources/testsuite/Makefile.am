#
# This file is processed by automake (produces file Makefile.in)
#
# Compiler flags -I$(top_srcdir)
FC =$(NC_FC)
AM_FCFLAGS  = $(NC_INC) -I$(top_srcdir)
AM_FCFLAGS += $(NC_LIB)

FCFLAGS = $(AM_FCFLAGS)

TESTS = test1 
check_PROGRAMS= test1
test1_SOURCES = ../check.F90 t_check.F90

#  Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod 

# The `:;' works around a Bash 3.2 bug when the output is not writeable.
$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	:;{ \
	echo '# Signature of the current package.' && \
	echo 'm4_define([AT_PACKAGE_NAME],' && \
	echo '  [@PACKAGE_NAME@])' && \
	echo 'm4_define([AT_PACKAGE_TARNAME],' && \
	echo '  [@PACKAGE_TARNAME@])' && \
	echo 'm4_define([AT_PACKAGE_VERSION],' && \
	echo '  [@PACKAGE_VERSION@])' && \
	echo 'm4_define([AT_PACKAGE_STRING],' && \
	echo '  [@PACKAGE_STRING@])' && \
	echo 'm4_define([AT_PACKAGE_BUGREPORT],' && \
	echo '  [@PACKAGE_BUGREPORT@])'; \
	echo 'm4_define([AT_PACKAGE_URL],' && \
	echo '  [@PACKAGE_URL@])'; \
	}>'$(srcdir)/package.m4'

TESTSUITE = $(srcdir)/testsuite $(srcdir)/package.m4 

check-local:	atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)
     
installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	$(TESTSUITEFLAGS)

maintainer-clean-local:
	rm -rf aclocal.m4 configure Makefile.in autotools m4

clean-local:
	test ! -f '$(TESTSUITE)' || \
	$(SHELL) '$(TESTSUITE)' --clean
     
AUTOM4TE = $(SHELL) $(top_srcdir)/autotools/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

EXTRA_DIST = testsuite.at package.m4 $(TESTSUITE) atlocal.in

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):

