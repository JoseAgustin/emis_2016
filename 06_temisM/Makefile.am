#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = Mtemporal.exe
#
# Compiler flags
  FC =$(NC_FC)
  AM_FCFLAGS  = $(NC_INC)
  AM_FCFLAGS += $(NC_LIB)
  
# Parallel version
if WITH_OMPI
  AM_FCFLAGS += $(OPENMP_FCFLAGS)
endif

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~
Mtemporal_exe_SOURCES = movil_temp.F90
