!
!	area_espacial.F90
!
!> @brief For area_espacial.F90 program. Area emissions spatial distribution
!>
!> Currently uses 59 categories and considers 2,458 municipalities in Mexico
!>
!> GRIDCODE is based on a grid covering the hole country in 1x1 km or 3x3 km or 9x9 km grid cels
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version  2.7
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!	Created by Agustin on 14/08/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!
!  Reads land use fraction per cell and land use tyepe and converts
!  to a one line.
!  ifort -o ASpatial.exe -O3 area_espacial.F90
module area_spatial_mod
!> Number of lines in surrogate file
    integer nl     ; !>  State ID
    integer edo    ; !>  Municpaity ID in the state
    integer mun;     !>  Number of municipalities
    integer, parameter  :: nm=2458;   !> Number of pollutant files
    integer, parameter  :: nf=10  ;   !> Number of emissions categories for SCC
    integer, parameter  :: nnscc=59;  !> GRIDCODE ID for agriculture land use data
    integer,allocatable :: gria(:) ;  !> Agriculture municipality ID
    integer,allocatable :: ida(:)  ;  !> GRIDCODE ID for vegetation land use data
    integer,allocatable :: grib(:) ;  !> Vegetation municipality ID
    integer,allocatable :: idb(:)  ;  !> GRIDCODE ID for airport land use data
    integer,allocatable :: grie(:) ;  !> Airport municipality ID
    integer,allocatable :: ide(:)  ;  !> GRIDCODE ID for seaports land use data
    integer,allocatable :: grim(:) ;  !> Seaports municipality ID
    integer,allocatable :: idm(:)  ;  !> GRIDCODE ID for population  data
    integer,allocatable :: grip(:) ;  !> Population municipality ID
    integer,allocatable :: idp(:)  ;  !> GRIDCODE ID for unpaved streets land use data
    integer,allocatable :: grir(:) ;  !> Unpaved streets municipality ID
    integer,allocatable :: idr(:)  ;  !> GRIDCODE ID for train stations land use data
    integer,allocatable :: grit(:) ;  !> Train stations municipality ID
    integer,allocatable :: idt(:)  ;  !> GRIDCODE ID for bus terminals land use data
    integer,allocatable :: griu(:) ;  !> Bus terminals municipality ID
    integer,allocatable :: idu(:)  ;  !> GRIDCODE ID for streets land cover data
    integer,allocatable :: griv(:) ;  !> Streets municipality ID
    integer,allocatable :: idv(:)  ;  !> Number of emissions categories per pollutant file
    integer,dimension(nf) :: nscc  ;  !> State ID + Municpaity ID
    integer,dimension (nf,nm):: iem;  !> Agriculture area fraction in grid
    real,allocatable :: fa(:) ;  !> Vegetation area fraction in grid
    real,allocatable :: fb(:) ;  !> Airport area fraction in grid
    real,allocatable :: fe(:) ;  !> Bus terminals fraction in grid
    real,allocatable :: fu(:) ;  !> Seaports area fraction in grid
    real,allocatable :: fm(:) ;  !> Train stations area fraction in grid
    real,allocatable :: ft(:) ;  !> Unpaved streets area fraction in grid
    real,allocatable :: fr(:) ;  !> Streets area fraction in grid
    real,allocatable :: fv(:) ;  !> Urbana1 population number fraction in grid
    real,allocatable :: fp1(:);  !> Rural2 population number fraction in grid
    real,allocatable :: fp2(:);  !> Total population number fraction in grid
    real,allocatable :: fp3(:)
!   Emisiones fuentes agricolas, bosques y poblacion grid, n, nnscc
!>  Emissions from agricultural sources array
    real,allocatable :: eagr(:,:,:);    !> Emissions from vegetation sources array
    real,allocatable :: ebos(:,:,:);    !> Emissions related to population  array per GRIDCODE
    real,allocatable :: epob(:,:,:)
!   Emisiones fuentes aeropuertos, central, puertos grid, n, nnscc
!>  Emissions from airports sources array
    real,allocatable :: eaer(:,:,:);    !> Emissions from bus terminals sources array
    real,allocatable :: ecen(:,:,:);    !> Emissions from seaports sources array
    real,allocatable :: epue(:,:,:);    !> Emissions from train stations sources array
    real,allocatable :: etre(:,:,:);    !> Emissions from unpaved streets sources array
    real,allocatable :: eter(:,:,:);    !> Emissions from streets sources array
    real,allocatable :: evia(:,:,:)
!> Emissions array per municipality, SCC and pollutant
    real,dimension(nm,nnscc,nf):: emiss
!> SCC codes array per polluntan and source
    character(len=10),dimension(nf,nnscc) ::scc; !>  National emission file name, array per pollutant
    character(len=14),dimension(nf) :: efile;    !>   Output emission file name
    character(len=14),dimension(nf) :: ofile

!   Emissions Inventory files
    data efile /'INH3_2016.csv','INOx_2016.csv','ISO2_2016.csv',&
&           'IVOC_2016.csv','ICO__2016.csv','IPM10_2016.csv',&
&           'IPM25_2016.csv','ICO2_2016.csv','IBC__2016.csv',&
&           'imet__2016.csv'/
!            NH3          NO2         SO2    VOC    CO PM10 PM25
    data ofile /'ANH3_2016.csv','ANOx_2016.csv','ASO2_2016.csv',&
&           'AVOC_2016.csv','ACO__2016.csv','APM10_2016.csv',&
&          'APM25_2016.csv','ACO2_2016.csv','ACN__2016.csv', &
&          'ACH4_2016.csv'/
end module area_spatial_mod

!>	@brief Allocates municipality area emissions in an emissions grid
!> by using gridded distributed population information,
!> land cover, roads, trains, airports, seaports and
!> buses terminals
!>
!> the grid depends on the selected zona in namelist_emis.nml
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version  2.7
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program area_espacial
    use master
    use area_spatial_mod

       call lee_namelist

       call area_emiss_reading

       call area_spatial_locating

       call area_spatial_storing

contains
!>	@brief Reads distributed population information,
!> land cover, roads, trains, airports, seaports and
!> buses terminals to spatially allocate area emissions.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version  2.7
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|                           _ _
!   ___ _ __ ___ (_)___ ___     _ __ ___  __ _  __| (_)_ __   __ _
!  / _ \ '_ ` _ \| / __/ __|   | '__/ _ \/ _` |/ _` | | '_ \ / _` |
! |  __/ | | | | | \__ \__ \   | | |  __/ (_| | (_| | | | | | (_| |
!  \___|_| |_| |_|_|___/___/___|_|  \___|\__,_|\__,_|_|_| |_|\__, |
!                         |_____|                            |___/
subroutine area_emiss_reading
implicit none
    integer i,j,k,iun
    character(len=38):: cdum,fname
    cdum="../01_datos/"//trim(zona)//"/"
!$omp parallel sections num_threads (4) private(fname,nl)
!$omp section
    fname=trim(cdum)//'gri_pob.csv'
    nl=cuenta_linea(fname)-1
    allocate(grip(nl),idp(nl),fp1(nl),fp2(nl),fp3(nl))
    call lee_file(fname,grip,idp,fp1,fp2,fp3)
!$omp section
    fname=trim(cdum)//"bosque.csv"
      nl= cuenta_linea(fname)
      allocate (grib(nl),idb(nl),fb(nl))
      call lee_file (fname, grib,idb,fb)
!$omp section
    fname=trim(cdum)//"agricola.csv"
      nl=cuenta_linea(fname)
      allocate (gria(nl),ida(nl),fa(nl))
      call lee_file(fname, gria,ida,fa)
!$omp section
    fname=trim(cdum)//'aeropuerto.csv'
      nl=cuenta_linea(fname)
      allocate (grie(nl),ide(nl),fe(nl))
      call lee_file(fname, grie,ide,fe)
    fname=trim(cdum)//'centrales.csv'
      nl=cuenta_linea(fname)
      allocate (griu(nl),idu(nl),fu(nl))
      call lee_file(fname, griu,idu,fu)
    fname=trim(cdum)//'puertos.csv'
      nl=cuenta_linea(fname)
      allocate (grim(nl),idm(nl),fm(nl))
      call lee_file(fname, grim,idm,fm)
    fname=trim(cdum)//'ffcc.csv'
      nl=cuenta_linea(fname)
      allocate(grit(nl),idt(nl),ft(nl))
      call lee_file(fname, grit,idt,ft)
    fname=trim(cdum)//'gri_ter.csv'
      nl=cuenta_linea(fname)
      allocate(grir(nl),idr(nl),fr(nl))
      call lee_file(fname, grir,idr,fr)
    fname=trim(cdum)//'gri_pav.csv'
      nl=cuenta_linea(fname)
      allocate(griv(nl),idv(nl),fv(nl))
      call lee_file(fname, griv,idv,fv)
!$omp end parallel sections
    do k=1,nf
        open (newunit=iun,file=efile(k),status='OLD',action='read')
        read (iun,'(A)') cdum
        read (iun,'(A)') cdum
        print *,zona,"  ",efile(k)
        read (iun,*) nscc(k),cdum,(scc(k,i),i=1,nscc(k))
        print '(5(A10,x))',(scc(k,i),i=1,nscc(k))
        print *,k,nscc(k)
        do i=1,nm
          read(iun,*) edo,mun,iem(k,i),(emiss(i,j,k),j=1,nscc(k))
        end do ! i
        close(iun)
    end do! k
end subroutine area_emiss_reading
!>	@brief Allocates emissions using distributed population information,
!> land cover, roads, trains, airports, seaports and
!> buses terminals to distribute area emissions.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version  2.7
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|       _     _                 _   _
!  ___ _ __   __ _| |_(_) __ _| |   | | ___   ___ __ _| |_(_)_ __   __ _
! / __| '_ \ / _` | __| |/ _` | |   | |/ _ \ / __/ _` | __| | '_ \ / _` |
! \__ \ |_) | (_| | |_| | (_| | |   | | (_) | (_| (_| | |_| | | | | (_| |
! |___/ .__/ \__,_|\__|_|\__,_|_|___|_|\___/ \___\__,_|\__|_|_| |_|\__, |
!     |_|                      |_____|                             |___/
subroutine area_spatial_locating
    implicit none
    integer i,j,k,l,m
	allocate(eagr(size(gria),nf,nnscc))
	allocate(ebos(size(grib),nf,nnscc))
    allocate(epob(size(grip),nf,nnscc))
    allocate(eaer(size(grie),nf,nnscc))
    allocate(ecen(size(griu),nf,nnscc))
    allocate(epue(size(grim),nf,nnscc))
    allocate(etre(size(grit),nf,nnscc))
    allocate(eter(size(grir),nf,nnscc))
    allocate(evia(size(griv),nf,nnscc))
    eagr=0.0
    ebos=0.0
    epob=0.0
    eaer=0.0
    ecen=0.0
    epue=0.0
    etre=0.0
    eter=0.0
    evia=0.0
    print *," Inicia Calculos de Distribucion Espacial"
    Clase: do k=1,nf
!$omp parallel sections num_threads (4) private(i,j,l)
!$omp section
    print *,"     Agricola  ", efile(k)
    agricola: do j=1,size(fa) ! grid
    inven: do i=1,nm          ! municipality
        if(ida(j).eq.iem(k,i)) then
           do l=1,nscc(k)     ! SCC
    if(scc(k,l).eq.'2801500100') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Qmas_agricolas
    if(scc(k,l).eq.'2801000002') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Labranza Siemb
    if(scc(k,l).eq.'2267005000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Comb_agricola_LPG
    if(scc(k,l).eq.'2801700000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Fertilizantes
!    if(scc(k,l).eq.'2270005000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Comb_agricola_Diesel
    if(scc(k,l).eq.'2805020000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Ganaderas
    if(scc(k,l).eq.'2461850000') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Pesticidas
    if(scc(k,l).eq.'2801000005') eagr(j,k,l)=emiss(i,l,k)*fa(j)*1e6  ! Labranza_Cose
           end do
           exit inven
        end if
    end do inven
    end do agricola
!$omp section
    print *,"     Bosque"
    Bosque: do j=1,size(fb) ! grid
    invenb: do i=1,nm       ! municipality
        if(idb(j).eq.iem(k,i)) then
            do l=1,nscc(k)      ! SCC
    if(scc(k,l).eq.'2810001000') ebos(j,k,l)=emiss(i,l,k)*fb(j)*1e6 ! conversion de Mg to g.
            end do
            exit invenb
         end if
        end do invenb
    end do Bosque
!$omp section
!   Aeropuertos
    print *,"     Aeropuertos"
   aerop: do j=1,size(fe) !grid
    invee: do i=1,nm          ! municipality
       if(ide(j).eq.iem(k,i)) then
           do l=1,nscc(k)
            if(scc(k,l).eq.'2275000000') eaer(j,k,l)=emiss(i,l,k)*fe(j)*1e6 !Aviacion
            if(scc(k,l).eq.'2275050000') eaer(j,k,l)=emiss(i,l,k)*fe(j)*1e6 !Equipo basico aeropouertos
           end do
          exit invee
       end if
    end do invee
   end do aerop
!   C Caminoneras
    print *,"     Central Caminoneras"
    ccam: do j=1,size(fu) !grid
        invcc: do i=1,nm          ! municipality
        if(idu(j).eq.iem(k,i)) then
            do l=1,nscc(k)
                if(scc(k,l).eq.'2230070310') ecen(j,k,l)=emiss(i,l,k)*fu(j)*1e6 !Terminal Buses
                if(scc(k,l).eq.'2270008010') ecen(j,k,l)=emiss(i,l,k)*fu(j)*1e6 !Terminales de autobuses
            end do
            exit invcc
            end if
        end do invcc
    end do ccam
!   Puertos Maritimos
    print *,"     Puertos Maritimos"
    pmar: do j=1,size(fm) !grid
        invpm: do i=1,nm          ! municipality
        if(idm(j).eq.iem(k,i)) then
            do l=1,nscc(k)
                if(scc(k,l).eq.'2280000000') epue(j,k,l)=emiss(i,l,k)*fm(j)*1e6 !Embarcaciones marinas
            end do
            exit invpm
        end if
        end do invpm
    end do pmar
!   Puertos Maritimos
    print *,"     Ferrocarriles"
    pffc: do j=1,size(ft) !grid
        invfcc: do i=1,nm          ! municipality
        if(idt(j).eq.iem(k,i)) then
        do l=1,nscc(k)
            if(scc(k,l).eq.'2285000000') etre(j,k,l)=emiss(i,l,k)*ft(j)*1e6 !Locomotoras de arrastre
            if(scc(k,l).eq.'2285002010') etre(j,k,l)=emiss(i,l,k)*ft(j)*1e6 !Locomotoras de patio
        end do
        exit invfcc
        end if
        end do invfcc
    end do pffc
    print *,"     Terraceria"
    Terrac: do j=1,size(fr) ! grid
    invenr: do i=1,nm       ! municipality
        if(idr(j).eq.iem(k,i)) then
        do l=1,nscc(k)      ! SCC
   !         if(scc(k,l).eq.'2296000000') eter(j,k,l)=emiss(i,l,k)*fr(j)*1e6  ! Caminos Terra
        end do
        exit invenr
        end if
    end do invenr
    end do Terrac
    print *,"     Vialidades"
    Vialid: do j=1,size(fv) ! grid
    invenv: do i=1,nm       ! municipality
        if(idv(j).eq.iem(k,i)) then
        do l=1,nscc(k)      ! SCC
            if(scc(k,l).eq.'2461020000') evia(j,k,l)=emiss(i,l,k)*fv(j)*1e6  ! Asfaltado
            if(scc(k,l).eq.'2401008000') evia(j,k,l)=emiss(i,l,k)*fv(j)*1e6  ! Senializacion
        end do
        exit invenv
        end if
    end do invenv
    end do Vialid
!$omp section
        print *,"     Poblacion"
    poblacion: do j=1,size(grip)! grid
    invenp: do i=1,nm       ! municipality
        if(idp(j).eq.iem(k,i)) then
!dir$ loop count min(16)
            do l=1,nscc(k)
    if(scc(k,l).eq.'2104007000') epob(j,k,l)=emiss(i,l,k)*(fp2(j)*0.2+fp1(j)*0.8)*1e6!Comb_res_LPG
    if(scc(k,l).eq.  '30500304') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6  ! Ladrillera
    if(scc(k,l).eq.'2102004000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_ind_Diesel
    if(scc(k,l).eq.'2102006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_ind_NG
    if(scc(k,l).eq.'2102007000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_ind_LPG
    if(scc(k,l).eq.'2103006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_comer_NG
    if(scc(k,l).eq.'2103007000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_comer_LPG
    if(scc(k,l).eq.'2104006000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Comb_res_GN
    if(scc(k,l).eq.'2104008000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6  ! Comb_res_lena
    if(scc(k,l).eq.'2104011000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6  ! Comb_res_keroseno
    if(scc(k,l).eq.'2222222222') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Cruces_front
    if(scc(k,l).eq.'2302002000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Asados al C
    if(scc(k,l).eq.'2302050000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Panificacion
    if(scc(k,l).eq.'2311010000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Construccion
    if(scc(k,l).eq.'2401001000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Recub Arq
    if(scc(k,l).eq.'2401005000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Pintado automotriz
    if(scc(k,l).eq.'2401065000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Fab Acceso Elec
    if(scc(k,l).eq.'2401080000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Fab eq transport
    if(scc(k,l).eq.'2401020000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Muebles Colc
    if(scc(k,l).eq.'2401990000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Otr Ind Manuf
    if(scc(k,l).eq.'2415000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! limpieza_Sup_ind
    if(scc(k,l).eq.'2415010000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Ind met basc
    if(scc(k,l).eq.'2420000055') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Lavado en seco
    if(scc(k,l).eq.'2425000000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Serigrafia
    if(scc(k,l).eq.'2425010000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Offset
    if(scc(k,l).eq.'2425030000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Rotograbado
    if(scc(k,l).eq.'2425040000') epob(j,k,l)=emiss(i,l,k)*fp1(j)*1e6  ! Flexografia
    if(scc(k,l).eq.'2465000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos en aerosol
    if(scc(k,l).eq.'2465100000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos_personal
    if(scc(k,l).eq.'2465200000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos dom_sticos
    if(scc(k,l).eq.'2465400000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos de cuidado automotriz
    if(scc(k,l).eq.'2465600000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Adhesivos y selladores
    if(scc(k,l).eq.'2465800000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Pesticidas comerciales y domesticos
    if(scc(k,l).eq.'2465900000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Productos miscelaneos
    if(scc(k,l).eq.'2501060000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Dist Comb
    if(scc(k,l).eq.'2610000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Qmas Cielo Ab
    if(scc(k,l).eq.'2630030000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Aguas Resd
    if(scc(k,l).eq.'2810030000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Incend_cosntruc
    if(scc(k,l).eq.'2850000010') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Esteri Hosp
    if(scc(k,l).eq.'3333333333') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Dist_LPG
    if(scc(k,l).eq.'5555555555') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Uso_domestico
    if(scc(k,l).eq.'2296000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Caminos terraceria
    if(scc(k,l).eq.'2294000000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Caminos pavim
    if(scc(k,l).eq.'2265005000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Maquinaria Agricola
    if(scc(k,l).eq.'2620030000') epob(j,k,l)=emiss(i,l,k)*fp2(j)*1e6  ! Relleno sanitario
    if(scc(k,l).eq.'2270005000') epob(j,k,l)=emiss(i,l,k)*fp3(j)*1e6  ! Comb_agricola_Diesel
     end do
            exit invenp
        end if
    end do invenp
    end do poblacion
!$omp end parallel sections
    end do Clase
end subroutine area_spatial_locating
!>	@brief Stores pollutan emissions, columns contains SCC and rows GRIDID.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version  2.7
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|       _         _             _
!  ___ _ __   __ _| |_(_) __ _| |    ___| |_ ___  _ __(_)_ __   __ _
! / __| '_ \ / _` | __| |/ _` | |   / __| __/ _ \| '__| | '_ \ / _` |
! \__ \ |_) | (_| | |_| | (_| | |   \__ \ || (_) | |  | | | | | (_| |
! |___/ .__/ \__,_|\__|_|\__,_|_|___|___/\__\___/|_|  |_|_| |_|\__, |
!     |_|                      |_____|                         |___/
subroutine area_spatial_storing
    implicit none
    character(len=32):: FMT
    character(len=65):: FTT
    integer i,k,l,iun
    real suma
!310 format(I9,",",I6,",",F7.4,",",F7.4,60(",",ES12.5))

    Print *,"   ***   Guardando Emisiones de Area   ***"
    write(FMT,"('(I3,'',g_per_year'',',I0,'('','',A10))')")nnscc
    write(FTT,"('(I9,'','',I6,2('',''F7.4),',I0,'('','',ES12.5))')")nnscc
!$omp parallel do private(iun,i,l,suma)
    do k=1,nf
        open(newunit=iun,file=ofile(k),ACTION='write')
        write(iun,*)'grid,CID,Furb,Frural,SCCs'
        write(iun,FMT)nscc(k),(scc(k,i),i=1,nscc(k))
        print *,"   Agricola ",ofile(k)
        do i=1,size(fa)
            suma=0
            do l=1,nscc(k)
               suma=suma+eagr(i,k,l)
            end do
            if (suma.gt.0) &
&           write(iun,FTT) gria(i),ida(i),0.,fa(i),(eagr(i,k,l),l=1,nscc(k))
        end do
        print *,"   Bosque",k
        do i=1,size(fb)
            suma=0
            do l=1,nscc(k)
            suma=suma+ebos(i,k,l)
            end do
            if (suma.gt.0) &
&            write(iun,FTT) grib(i),idb(i),0.,fb(i),(ebos(i,k,l),l=1,nscc(k))
        end do
        print *,"   Poblacion",k
        do i=1,size(fp1)
           suma=0
            do l=1,nscc(k)
            suma=suma+epob(i,k,l)
            end do
            if (suma.gt.0) &
&            write(iun,FTT) grip(i),idp(i),fp1(i),fp2(i),(epob(i,k,l),l=1,nscc(k))
        end do
        print *,"   Aeropuerto"
        do i=1,size(fe)
            suma=0
            do l=1,nscc(k)
            suma=suma+eaer(i,k,l)
            end do
            if (suma.gt.0) &
&        write(iun,FTT) grie(i),ide(i),fe(i),0.,(eaer(i,k,l),l=1,nscc(k))
        end do
        print *,"   Centrales Autobuses"
        do i=1,size(fu)
            suma=0
            do l=1,nscc(k)
            suma=suma+ecen(i,k,l)
            end do
            if (suma.gt.0) &
&        write(iun,FTT) griu(i),idu(i),fu(i),0.,(ecen(i,k,l),l=1,nscc(k))
        end do
        print *,"   Puertos Maritimos"
        do i=1,size(fm)
            suma=0
            do l=1,nscc(k)
            suma=suma+epue(i,k,l)
            end do
            if (suma.gt.0) &
&        write(iun,FTT) grim(i),idm(i),fm(i),0.,(epue(i,k,l),l=1,nscc(k))
        end do
        print *,"   Ferrocarriles"
        do i=1,size(ft)
            suma=0
            do l=1,nscc(k)
            suma=suma+etre(i,k,l)
            end do
            if (suma.gt.0) &
&          write(iun,FTT) grit(i),idt(i),ft(i),0.,(etre(i,k,l),l=1,nscc(k))
        end do
    print *,"   Terraceria"
    do i=1,size(fr)
        suma=0
        do l=1,nscc(k)
        suma=suma+eter(i,k,l)
        end do
        if (suma.gt.0) &
&        write(iun,FTT) grir(i),idr(i),fr(i),0.,(eter(i,k,l),l=1,nscc(k))
        end do
    print *,"   Vialidades"
    do i=1,size(fv)
        suma=0
        do l=1,nscc(k)
        suma=suma+evia(i,k,l)
        end do
        if (suma.gt.0) &
&        write(iun,FTT) griv(i),idv(i),fv(i),0.,(evia(i,k,l),l=1,nscc(k))
        end do
    close(10)
    end do
!$omp end parallel do
    deallocate (gria,ida,fa,eagr)
    deallocate (grib,idb,fb,ebos)
    deallocate (grip,idp,fp1,fp2,epob)
    deallocate (grie,ide,fe,eaer)
    deallocate (griu,idu,fu,ecen)
    deallocate (grim,idm,fm,epue)
    deallocate (grit,idt,ft,etre)
    deallocate (grir,idr,fr,eter)
    deallocate (griv,idv,fv,evia)
end subroutine area_spatial_storing
!>	@brief Counts the number of lines per surrogate file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version  2.7
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!> @param archivo input file for line counting
!                        _            _ _
!   ___ _   _  ___ _ __ | |_ __ _    | (_)_ __   ___  __ _
!  / __| | | |/ _ \ '_ \| __/ _` |   | | | '_ \ / _ \/ _` |
! | (__| |_| |  __/ | | | || (_| |   | | | | | |  __/ (_| |
!  \___|\__,_|\___|_| |_|\__\__,_|___|_|_|_| |_|\___|\__,_|
!                               |_____|
integer function cuenta_linea(archivo)
  implicit none
  integer iun
  character (len=*), intent(in)::archivo
  character (len=24) ::cdum
  print *,'Lee ',archivo
  open(newunit=iun,file=archivo,status='OLD',action='read')
    read (iun,*) cdum
    cuenta_linea=0
    do
      read(iun,*,end=100) cdum
      cuenta_linea=cuenta_linea+1
    end do
    100 print *,' Numero de lineas',cuenta_linea
  close(iun)
end function
!>	@brief Reads area fraction data from the surrogate file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version  2.7
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param archivo file to be read
!>   @param grid GRIDCODE ID for the surrogate fraction
!>   @param id municpality ID
!>   @param frac  surrogate fraction
!>   @param frac2 surrogate fraction 2
!>   @param frac3 surrogate fraction 3
!  _                __ _ _
! | | ___  ___     / _(_) | ___
! | |/ _ \/ _ \   | |_| | |/ _ \
! | |  __/  __/   |  _| | |  __/
! |_|\___|\___|___|_| |_|_|\___|
!            |_____|
subroutine lee_file(archivo,grid,id,frac,frac2,frac3)
    implicit none
    integer i,nl,iun
    integer, dimension(:), intent(inout)::grid,id
    real,dimension(:), intent(inout)::frac
    real,dimension(:), optional,intent(inout)::frac2,frac3
    character (len=*), intent(in)::archivo
    character (len=18):: cdum
    open(newunit=iun,file=archivo,status='OLD',action='read')
    read (iun,*) cdum
    if (present(frac2).and.present(frac3))then
        read (iun,*) cdum
        do i=1,size(id)
            read(iun,*)grid(i),id(i),frac(i),frac2(i),frac3(i)
        end do
    else
        do i=1,size(id)
            read(iun,*)grid(i),id(i),frac(i)
        end do
    end if
    close(iun)
end subroutine
end program area_espacial
