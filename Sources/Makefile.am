#
# This file is processed by automake (produces file Makefile.in)
# Cofiguration file for emis_2016
#
bin_PROGRAMS = ASpatial.exe
bin_PROGRAMS += Aanual.exe
bin_PROGRAMS += carr.exe vial.exe agrega.exe
bin_PROGRAMS += Atemporal.exe
bin_PROGRAMS += MSpatial.exe
bin_PROGRAMS += Mtemporal.exe
bin_PROGRAMS += Puntual.exe
bin_PROGRAMS += spa.exe spm.exe spp.exe
bin_PROGRAMS += spm25a.exe spm25m.exe spm25p.exe
bin_PROGRAMS += emiss.exe

# Compiler flags
  FC =$(NC_FC)
  AM_FCFLAGS = $(NC_INC)
  AM_FCFLAGS += $(NC_LIB)

if WITH_GFOR
  AM_FCFLAGS +=-DPGI
endif
  
# Parallel version
if WITH_OMPI
  AM_FCFLAGS += $(OPENMP_FCFLAGS)
endif

# Additional files to be cleaned with 'make clean'
CLEANFILES = *.o *.mod *~

# Area emissions spatial distribution and temporal
ASpatial_exe_SOURCES = master_mod.F90 area_espacial.F90
Atemporal_exe_SOURCES = master_mod.F90 atemporal.F90
Aanual_exe_SOURCES = check.F90 master_mod.F90 areaanual.F90
# Mobile  spatial allocation
carr_exe_SOURCES = master_mod.F90 mobile_mod.F90 suma_carretera.F90
vial_exe_SOURCES = master_mod.F90 mobile_mod.F90 suma_vialidades.F90
agrega_exe_SOURCES    = master_mod.F90 mobile_mod.F90 agrega.F90
#
MSpatial_exe_SOURCES  = sort_uniq.F90 movil_spatial.F90
Mtemporal_exe_SOURCES = check.F90 master_mod.F90 movil_temp.F90
# Point emissions temporal
Puntual_exe_SOURCES   = master_mod.F90 t_puntal.F90
# Gas Chemical speciation
spa_exe_SOURCES = sort_uniq.F90 agg_mod.F90 master_mod.F90 agg_a.F90
spm_exe_SOURCES = sort_uniq.F90 agg_mod.F90 master_mod.F90 agg_m.F90
spp_exe_SOURCES = sort_uniq.F90 agg_mod.F90 master_mod.F90 agg_p.F90
# Particle speciation
spm25a_exe_SOURCES = sort_uniq.F90 e_pm25_mod.F90 master_mod.F90 pm25_speci_a.F90
spm25m_exe_SOURCES = sort_uniq.F90 e_pm25_mod.F90 master_mod.F90 pm25_speci_m.F90
spm25p_exe_SOURCES = sort_uniq.F90 e_pm25_mod.F90 master_mod.F90 pm25_speci_p.F90
# Storage
emiss_exe_SOURCES = check.F90 master_mod.F90 g_emis2.F90
