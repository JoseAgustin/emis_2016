#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = emiss.exe

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~
# Compiler flags
  AM_FCFLAGS  = $(NC_INC)
  AM_FCFLAGS += $(NC_LIB)
# Parallel version
if WITH_OMPI
  AM_FCFLAGS += $(OPENMP_FCFLAGS)
endif

emiss_exe_SOURCES = g_emis2.F90
