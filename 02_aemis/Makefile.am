#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS=ASpatial.exe
# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~

# Parallel version
if WITH_OMPI
  AM_FCFLAGS = $(OPENMP_FCFLAGS)
endif

ASpatial_exe_SOURCES = area_espacial.F90
