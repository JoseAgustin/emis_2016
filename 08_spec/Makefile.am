#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = spa.exe spm.exe spp.exe
#
# Compiler flags
# Parallel version
if WITH_OMPI
  AM_FCFLAGS = $(OPENMP_FCFLAGS)
endif

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~

spa_exe_SOURCES = agg_a.F90
spm_exe_SOURCES = agg_m.F90
spp_exe_SOURCES = agg_p.F90

