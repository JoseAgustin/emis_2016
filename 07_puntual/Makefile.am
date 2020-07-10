#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = Puntual.exe
#
# Compiler flags
# Parallel version
if WITH_OMPI
  AM_FCFLAGS = $(OPENMP_FCFLAGS)
endif

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~

Puntual_exe_SOURCES = t_puntal.F90
