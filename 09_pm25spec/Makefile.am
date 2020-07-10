#
# This file is processed by automake (produces file Makefile.in)
#
bin_PROGRAMS = spm25a.exe spm25m.exe spm25p.exe
#
# Compiler flags
# Parallel version
if WITH_OMPI
  AM_FCFLAGS = $(OPENMP_FCFLAGS)
endif

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~

spm25a_exe_SOURCES = pm25_speci_a.F90
spm25m_exe_SOURCES = pm25_speci_m.F90
spm25p_exe_SOURCES = pm25_speci_p.F90

