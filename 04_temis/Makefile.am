#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = Atemporal.exe

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~

# Parallel version
if WITH_OMPI
  AM_FCFLAGS = $(OPENMP_FCFLAGS)
endif

Atemporal_exe_SOURCES = atemporal.F90
