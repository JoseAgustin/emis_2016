#
# This file is processed by automake (produces file Makefile.in)
#
# Compiler flags
FC =$(NC_FC)
AM_FCFLAGS  = $(NC_INC) -I$(top_srcdir)/Sources
AM_FCFLAGS += $(NC_LIB)
if WITH_GFOR
  AM_FCFLAGS +=-DPGI
endif
  
# Parallel version
if WITH_OMPI
  AM_FCFLAGS += $(OPENMP_FCFLAGS)
endif

FCFLAGS = $(AM_FCFLAGS)

TESTS = test1 test2 test3 test4 test5
check_PROGRAMS= test1 test2 test3 test4 test5
test1_SOURCES = $(top_srcdir)/Sources/check.F90 t_check.F90
test2_SOURCES = $(top_srcdir)/Sources/master_mod.F90 t_leenm.F90
test3_SOURCES = $(top_srcdir)/Sources/master_mod.F90 t_kverano.F90
test4_SOURCES = $(top_srcdir)/Sources/master_mod.F90 t_get_position.F90 
test5_SOURCES = $(top_srcdir)/Sources/sort_uniq.F90 t_unique.F90

#  Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod package.m4 testsuite

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

TESTSUITE = ./testsuite

check-local:  atconfig  $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)
     
installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	$(TESTSUITEFLAGS)

maintainer-clean-local:
	rm -rf aclocal.m4 configure Makefile.in autoconf m4

clean-local:
	test ! -f '$(TESTSUITE)' || \
	$(SHELL) '$(TESTSUITE)' --clean
     
AUTOM4TE = $(SHELL) $(top_srcdir)/autoconf/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

EXTRA_DIST = testsuite.at package.m4 $(TESTSUITE) atlocal.in

.PHONY: $(EXTRA_DIST)
$(EXTRA_DIST):

