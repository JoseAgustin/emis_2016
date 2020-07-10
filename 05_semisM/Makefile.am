#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = MSpatial.exe

# Parallel version
if WITH_OMPI
  AM_FCFLAGS = $(OPENMP_FCFLAGS)
endif

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~

MSpatial_exe_SOURCES = movil_spatial.F90
