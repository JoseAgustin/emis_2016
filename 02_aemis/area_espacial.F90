!
!	area_espacial.f90
!	
!
!	Created by Agustin on 14/08/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!
!  Reads lan use fracction per cell and land use tyepe and converts
!  to a one line.
!  ifort -o ASpatial.exe -O3 area_espacial.F90
!
!  4/03/2015  Correction in Terminasl 2801500002 and agricultural fires 2801500250
!  8/07/2017  For 2014 from 57 to 58 categories (ladrilleras), 2457 Municipalidades
!  4/06/2019  For 2016 59 categories, 2458 municipalities  
!
module land
    integer nl,nf,nm,nnscc,edo, mun
    parameter (nm=2458,nf=10,nnscc=59)
    integer,allocatable :: grib(:),idb(:)  ! Bosque
    integer,allocatable :: gria(:),ida(:)  ! Agricola
    integer,allocatable :: grip(:),idp(:)  ! Poblacion
    integer,allocatable :: grie(:),ide(:)  ! Aeropuertos
    integer,allocatable :: griu(:),idu(:)  ! Centrales Autobuses
    integer,allocatable :: grim(:),idm(:)  ! Puertos Maritimos
    integer,allocatable :: grit(:),idt(:)  ! Ferrocarriles
    integer,allocatable :: grir(:),idr(:)  ! Terraceria
    integer,allocatable :: griv(:),idv(:)  ! Vialidades

    integer,dimension(nf) :: nscc
    integer,dimension (nf,nm):: iem
    real,allocatable ::fb(:),fa(:)! Fracciones Bosque Agricola
    real,allocatable ::fe(:),fu(:),fm(:),ft(:)! Fracciones Aerop, C.A., P.M FFCC
    real,allocatable ::fr(:),fv(:)    !  Fracciones de terraceria y vialidades
    real,allocatable ::fp1(:),fp2(:),fp3(:)!Fracc Urbana1, Rural2 y total3
!   Emisiones fuentes agricolas, bosques y poblacion grid, n, nnscc
    real,allocatable :: eagr(:,:,:), ebos(:,:,:), epob(:,:,:)
!   Emisiones fuentes aeropuertos, central, puertos grid, n, nnscc
    real,allocatable :: eaer(:,:,:),ecen(:,:,:), epue(:,:,:),etre(:,:,:)
    real,allocatable :: eter(:,:,:),evia(:,:,:)
    real,dimension(nm,nnscc,nf):: emiss
    character(len=10),dimension(nf,nnscc) ::scc
    character(len=14),dimension(nf) ::efile,ofile
    character(len=12):: zona
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
end module land

program area_espacial
       use land

       call lee_namelist

       call lee

       call calculos

       call guarda

contains
!  _     _____ _____
! | |   | ____| ____|
! | |   |  _| |  _|
! | |___| |___| |___
! |_____|_____|_____|
!
subroutine lee
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
end subroutine lee
!  CCC  AA  L     CCC U   U L     OOO   SSS
! C    A  A L    C    U   U L    O   O S
! C    AAAA L    C    U   U L    O   O  SSS
! C    A  A L    C    U   U L    O   O     S
!  CCC A  A LLLL  CCC  UUU  LLLL  OOO  SSSS
subroutine calculos
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
end subroutine calculos
!  GGG  U   U  AA  RRRR  DDD   AA
! G     U   U A  A R   R D  D A  A
! G  GG U   U AAAA RRRR  D  D AAAA
! G   G U   U A  A R R   D  D A  A
!  GGG   UUU  A  A R  RR DDD  A  A
subroutine guarda
    implicit none
    integer i,k,l,iun
    real suma
    Print *,"   ***   Guarda   ***"
!$omp parallel do private(iun,i,l,suma)
    do k=1,nf
        open(newunit=iun,file=ofile(k),ACTION='write')
        write(iun,*)'grid,CID,Furb,Frural,SCCs'
        write(iun,300)nscc(k),(scc(k,i),i=1,nscc(k))
        print *,"   Agricola ",ofile(k)
        do i=1,size(fa)
            suma=0
            do l=1,nscc(k)
               suma=suma+eagr(i,k,l)
            end do
            if (suma.gt.0) &
&           write(iun,310) gria(i),ida(i),0.,fa(i),(eagr(i,k,l),l=1,nscc(k))
        end do
        print *,"   Bosque",k
        do i=1,size(fb)
            suma=0
            do l=1,nscc(k)
            suma=suma+ebos(i,k,l)
            end do
            if (suma.gt.0) &
&            write(iun,310) grib(i),idb(i),0.,fb(i),(ebos(i,k,l),l=1,nscc(k))
        end do
        print *,"   Poblacion",k
        do i=1,size(fp1)
           suma=0
            do l=1,nscc(k)
            suma=suma+epob(i,k,l)
            end do
            if (suma.gt.0) &
&            write(iun,310) grip(i),idp(i),fp1(i),fp2(i),(epob(i,k,l),l=1,nscc(k))
        end do
        print *,"   Aeropuerto"
        do i=1,size(fe)
            suma=0
            do l=1,nscc(k)
            suma=suma+eaer(i,k,l)
            end do
            if (suma.gt.0) &
&        write(iun,310) grie(i),ide(i),fe(i),0.,(eaer(i,k,l),l=1,nscc(k))
        end do
        print *,"   Centrales Autobuses"
        do i=1,size(fu)
            suma=0
            do l=1,nscc(k)
            suma=suma+ecen(i,k,l)
            end do
            if (suma.gt.0) &
&        write(iun,310) griu(i),idu(i),fu(i),0.,(ecen(i,k,l),l=1,nscc(k))
        end do
        print *,"   Puertos Maritimos"
        do i=1,size(fm)
            suma=0
            do l=1,nscc(k)
            suma=suma+epue(i,k,l)
            end do
            if (suma.gt.0) &
&        write(iun,310) grim(i),idm(i),fm(i),0.,(epue(i,k,l),l=1,nscc(k))
        end do
        print *,"   Ferrocarriles"
        do i=1,size(ft)
            suma=0
            do l=1,nscc(k)
            suma=suma+etre(i,k,l)
            end do
            if (suma.gt.0) &
&          write(iun,310) grit(i),idt(i),ft(i),0.,(etre(i,k,l),l=1,nscc(k))
        end do
    print *,"   Terraceria"
    do i=1,size(fr)
        suma=0
        do l=1,nscc(k)
        suma=suma+eter(i,k,l)
        end do
        if (suma.gt.0) &
&        write(iun,310) grir(i),idr(i),fr(i),0.,(eter(i,k,l),l=1,nscc(k))
        end do
    print *,"   Vialidades"
    do i=1,size(fv)
        suma=0
        do l=1,nscc(k)
        suma=suma+evia(i,k,l)
        end do
        if (suma.gt.0) &
&        write(iun,310) griv(i),idv(i),fv(i),0.,(evia(i,k,l),l=1,nscc(k))
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
#ifndef PGI
300 format(I3,", g_per_year",<nnscc>(",",A10))
310 format(I9,",",I6,",",F,",",F,<nnscc>(",",ES12.5))
#else
300 format(I3,", g_per_year",60(",",A10))
310 format(I9,",",I6,",",F7.4,",",F7.4,60(",",ES12.5))
#endif
end subroutine guarda
!   CCC U   U EEEE N   N TTTTTT  AA      L    III N   N EEEE  AA
!  C    U   U E    NN  N   TT   A  A     L     I  NN  N E    A  A
!  C    U   U EEE  N N N   TT   AAAA     L     I  N N N EEE  AAAA
!  C    U   U E    N  NN   TT   A  A     L     I  N  NN E    A  A
!   CCC  UUU  EEEE N   N   TT   A  A ____LLLL III N   N EEEE A  A
!                                    ____
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
! L    EEEE EEEE     FFFF III L    EEEE
! L    E    E        F     I  L    E
! L    EEE  EEE      FFF   I  L    EEE
! L    E    E        F     I  L    E
! LLLL EEEE EEEE ____F    III LLLL EEEE
!                ____
subroutine lee_file(archivo,grid,id,frac,frac2,frac3)
    implicit none
    integer, dimension(:), intent(inout)::grid,id
    integer i,nl,iun
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
subroutine lee_namelist
    NAMELIST /region_nml/ zona
    integer unit_nml
    logical existe
    unit_nml = 9
    existe = .FALSE.
    write(6,*)' >>>> Reading file - namelist_emis.nml'
    inquire ( FILE = '../namelist_emis.nml' , EXIST = existe )

    if ( existe ) then
    !  Opening the file.
    open ( FILE   = '../namelist_emis.nml' ,      &
    UNIT   =  unit_nml        ,      &
    STATUS = 'OLD'            ,      &
    FORM   = 'FORMATTED'      ,      &
    ACTION = 'READ'           ,      &
    ACCESS = 'SEQUENTIAL'     )
    !  Reading the file
    READ (unit_nml , NML = region_nml )
    !WRITE (6    , NML = region_nml )
    close(unit_nml)
    else
    stop '***** No namelist_emis.nml in .. directory'
    end if

end subroutine lee_namelist
end program area_espacial

