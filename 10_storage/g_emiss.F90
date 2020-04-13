!
!	g_emiss.F90
!	
!
!  Creado por Jose Agustin Garcia Reynoso el 19/11/2017.
!
!
!  Proposito:
!            Guarda los datos del inventario para el
!            mecanismo los diferentes mecanismos en formato netcdf y con NAMELIST
!
! ifort g_emiss.F90 -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include  -o emiss.exe
!
!
!   09/04/2020   Version 1.1
!
module vars
    integer,parameter :: nh=24    !number hours day
    integer :: nf!=36    ! number of files antropogenic
    integer :: ns!=34    ! number of compounds
    integer ::radm!=ns+5 ! number of Mechanism classes
    integer :: ipm!=29  ! Posicion del archivo PM2.5
    integer :: icn!=36    ! Posicion archivo CN del INEM
    integer :: jcn!=34    ! Posicion archivo CN de especiacion
    integer :: imt!=35    ! Posicion archivo CH4 del INEM
    integer :: jmt!=7     ! Posicion archivo CH4 de especiacion
    integer :: idia     ! dia para el calculo de emisiones
    integer :: anio     ! anio de las emisiones 2016
    integer :: month    ! mes de la modelacio
    integer ::ncel   ! number of cell in the grid
    integer ::nl     ! number of lines in files
    integer :: nx,ny ! grid dimensions
    integer :: ncty  ! number of point stations
    integer*8 :: idcf  ! ID cell in file
    integer :: zlev       ! Layer of emission (1 to 8) 8 lower 1 upper
    integer,allocatable :: idcg(:) ! ID cell in grid
    integer,allocatable:: utmz(:),utmzd(:,:)  !utmz
    integer,allocatable :: isp(:) ! storage
    real,allocatable::wtm(:)  !storage
    real,allocatable:: eft(:,:,:,:,:)  ! emissions by nx,ny,file,nh,level
    real,allocatable :: utmx(:),utmy(:)
    real,allocatable :: lon(:),lat(:),pop(:)
    real,allocatable ::xlon(:,:),xlat(:,:),pob(:,:)
    real,allocatable :: utmxd(:,:),utmyd(:,:)
    real,dimension(:), allocatable:: scala,scalm,scalp !Scaling factors
    real :: CDIM      ! cell dimension in km
    character(len=3) :: cday
    character(len=11),dimension(:),allocatable:: ename
    character(len=16),dimension(:),allocatable:: cname
    character(len=18),dimension(:),allocatable:: fnameA,fnameM,fnameP
    character (len=19) :: current_date,mecha
    character (len=40) :: titulo
    character(len=12):: zona
    common /quimicos/ nf,ns,radm,ipm,icn,jcn,imt,jmt
    common /domain/ ncel,nl,nx,ny,zlev,CDIM,zona
    common /date/ current_date,cday,mecha,titulo
    common /nlm_vars/month,idia,anio
end module vars
!   __ _ _   _  __ _ _ __ __| | __ _     _ __   ___
!  / _` | | | |/ _` | '__/ _` |/ _` |   | '_ \ / __|
! | (_| | |_| | (_| | | | (_| | (_| |   | | | | (__
!  \__, |\__,_|\__,_|_|  \__,_|\__,_|___|_| |_|\___|
!  |___/                           |_____|          
program guarda_nc
use vars
use netcdf

    call lee_namelist

    call setup_mecha

	call lee

	call store

contains
!  _
! | | ___  ___
! | |/ _ \/ _ \
! | |  __/  __/
! |_|\___|\___|
!
subroutine lee
    IMPLICIT NONE
	integer :: ii,i,j,k,levl,levld,ih
    integer :: is
	integer :: iunit=14
	real ::rdum
	real,dimension(nh)::edum
	character(len=39) :: flocaliza
	character(len=13) cdum,crdum
    character (len=15)::ruta
! Mole weight
  flocaliza='../01_datos/'//trim(zona)//'/'//'localiza.csv'
  write(6,*)' >>>> Reading file -',flocaliza,' ---------'
  open (unit=10,file=flocaliza,status='old',action='read')
  read (10,*) cdum  !Header
  read (10,*) nx,ny,titulo  ! Dimensions and Title
  ncel=nx*ny
  allocate(idcg(ncel),lon(ncel),lat(ncel),pop(ncel))
  allocate(utmx(ncel),utmy(ncel),utmz(ncel))
  allocate(xlon(nx,ny),xlat(nx,ny),pob(nx,ny))
  allocate(utmxd(nx,ny),utmyd(nx,ny),utmzd(nx,ny))
  allocate(eft(nx,ny,nf,nh,8))
  zlev=0
  eft=0
  do k=1,ncel
	read(10,*) idcg(k),lon(k),lat(k),i,pop(k),utmx(k),utmy(k),utmz(k)
  end do
!
  do i=1,nx
    do j=1,ny
      k=i+(j-1)*nx
        xlon(i,j)=lon(k)
        xlat(i,j)=lat(k)
        pob(i,j)=pop(k)
        utmxd(i,j)=utmx(k)
        utmyd(i,j)=utmy(k)
        utmzd(i,j)=utmz(k)
    end do
  end do
  CDIM=(utmx(2)-utmx(1))/1000.  ! from meters to km
  print *,CDIM,trim(titulo)
  close(10)

  do ii=1,nf
  !write(6,*)' >>>> Reading emissions file -',fnameA(i),fnameM(i)
    if (ii.le.5 .or. (ii.ge.ipm-2 .and. ii.le.ipm).or.ii.eq.icn .or. ii .eq.imt)then
        ruta="../04_temis/"
    else if( ii.gt. ipm) then; ruta="../09_pm25spec/"
    else;     ruta="../08_spec/"; end if
    open(11,file=trim(ruta)//fnameA(ii),status='OLD',action='READ')
    read(11,*)cdum
    if (ii.eq.1)then
      read(11,*)j,current_date,cday  !j number of lines in file
      print *,current_date,' ',cday
    else
      read(11,*)j,current_date
    end if
    is= ii
    if(ii.eq.icn) is=jcn  ! suma todo el Carbono Negro
    if(ii.eq.imt) is=jmt   ! suma todo el Metano
    write(6,'(i4,x,A,A,I3,I3)') ii,ruta//fnameA(ii),current_date,is
    do
      if(ii.eq.ipm) then
        read(11,*,END=100) idcf,rdum,(edum(ih),ih=1,nh)
      else
        read(11,*,END=100) idcf,(edum(ih),ih=1,nh)
      end if
      k=0
    busca: do j=1,ny
      do i=1,nx
        k=k+1
        if(idcg(k).eq.idcf) then
          do ih=1,nh
            eft(i,j,is,ih,1)=eft(i,j,is,ih,1)+edum(ih)/WTM(is)*scala(ii) ! Emission from kg to gmol
          end do
          exit busca
        end if
      end do
      end do busca
    end do
 100 close(11)
        if (ii.le.5 .or. (ii.ge.ipm-2 .and. ii.le.ipm).or.ii.eq.icn .or. ii .eq.imt)then
        ruta="../06_temisM/"
        else if( ii.gt. ipm) then; ruta="../09_pm25spec/"
        else;     ruta="../08_spec/"; end if
 		open(11,file=trim(ruta)//fnameM(ii),status='OLD',action='READ')
		read(11,*)cdum
		if (ii.eq.1)then
            read(11,*)j,current_date,cday  !j number of lines in file
			!print *,current_date,cday
        else    
            read(11,*)j,current_date
        end if
        write(6,'(i4,x,A,A,I3,I3)') ii,ruta//fnameM(ii),current_date
		do 
		 if(ii.eq.ipm) then !for PM2.5
		 read(11,*,END=200) idcf,crdum,(edum(ih),ih=1,nh)
		 else
		 read(11,*,END=200) idcf,(edum(ih),ih=1,nh)
		 end if
		 k=0
		 busca2: do j=1,ny
		  do i=1,nx
			k=k+1
			if(idcg(k).eq.idcf) then
			  do ih=1,nh
                ! Emission from g to gmol by 1/WTM
                eft(i,j,is,ih,1)=eft(i,j,is,ih,1)+edum(ih)/WTM(is)*scalm(ii)
			  end do
			  exit busca2
			end if
		 end do
		end do busca2
		end do	
 200 close(11)
!  For point sources
!	if (ii.ne.2) then
        if (ii.le.5 .or. (ii.ge.ipm-2 .and. ii.le.ipm).or.ii.eq.icn .or. ii .eq.imt)then
        ruta="../07_puntual/"
        else if( ii.gt. ipm) then; ruta="../09_pm25spec/"
        else;     ruta="../08_spec/"; end if
		open(11,file=trim(ruta)//fnameP(ii),status='OLD',action='READ')
		read(11,*)cdum
		if (ii.eq.1)then
            read(11,*)j,current_date,cday  !j number of lines in file
			!print *,current_date,cday
        else    
            read(11,*)j,current_date
        end if
        write(6,'(i4,x,A,A,I3,I3)') ii,ruta//fnameP(ii),current_date
		do 
		 if(ii.eq.ipm) then   !for PM2.5
		 read(11,*,END=300) idcf,rdum,levl,(edum(ih),ih=1,nh),levld
         !print *,idcf,rdum,levl,(edum(ih),ih=1,nh)
		 else
		 read(11,*,END=300) idcf,levl,(edum(ih),ih=1,nh),levld
		 end if
		 k=0
		 busca3: do j=1,ny
		  do i=1,nx
			k=k+1
			if(idcg(k).eq.idcf) then
		  do ih=1,nh
                ! Emission from g to gmol by 1/WTM
                if(ih.gt.9 .and. ih.lt.19) then
                  if(levl.lt.2) then
                    eft(i,j,is,ih,levl)=eft(i,j,is,ih,levl)+edum(ih)/WTM(is)*scalp(ii)
                   else
                    eft(i,j,is,ih,levl)=eft(i,j,is,ih,levl)+edum(ih)/WTM(is)
                  end if
                 else
                  if(levld.lt.2) then
                    eft(i,j,is,ih,levld)=eft(i,j,is,ih,levld)+edum(ih)/WTM(is)*scalp(ii)
                  else
                    eft(i,j,is,ih,levld)=eft(i,j,is,ih,levld)+edum(ih)/WTM(is)
                  end if
                 end if
	        end do
          zlev =max(zlev,levl,levld)
          if(zlev.gt.8) Stop "*** Change dimension line  allocate(eft.."
			  exit busca3
			end if
		 end do
		end do busca3
		end do
300 continue;    close(11)
!	end if
	end do! ii nf
	
	return

end subroutine lee
!           _                                      _
!  ___  ___| |_ _   _ _ __     _ __ ___   ___  ___| |__   __ _
! / __|/ _ \ __| | | | '_ \   | '_ ` _ \ / _ \/ __| '_ \ / _` |
! \__ \  __/ |_| |_| | |_) |  | | | | | |  __/ (__| | | | (_| |
! |___/\___|\__|\__,_| .__/___|_| |_| |_|\___|\___|_| |_|\__,_|
!                    |_| |_____|
subroutine setup_mecha
	IMPLICIT NONE
    select case (mecha)
    case("cbm04")
        print *,"   **************************************"
        print *,"    CBM04 MECHANISM  NOT  YET IMPLEMENTED"
        print *,"   **************************************"
        stop "END program..."
    case("cbm05")
        print *,"Setup variables for ",mecha
        nf=30    ! number of files antropogenic
        ns=28    ! number of compounds
        radm=ns+5 ! number of Mechanism classes
        ipm=23  ! Posicion del archivo PM2.5
        icn=30    ! Posicion archivo CN del INEM
        jcn=28    ! Posicion archivo CN de especiacion
        imt=29    ! Posicion archivo CH4 del INEM
        jmt=7     ! Posicion archivo CH4 de especiacion
        allocate(ename(radm),cname(radm))
        allocate(isp(radm))
        allocate(wtm(ns))
        allocate(fnameA(nf),fnameM(nf),fnameP(nf))
        allocate(scala(nf),scalm(nf),scalp(nf))
ename=(/'E_CO       ','E_NH3      ','E_NO       ','E_NO2      ',&
'E_SO2      ','E_ALD2     ','E_CH4      ','E_ALDX     ','E_ETH      ',&
'E_ETHA     ','E_ETOH     ','E_IOLE     ','E_MEOH     ','E_HCHO     ',&
'E_ISOP     ','E_OLE      ','E_PAR      ','E_TERP     ','E_TOL      ',&
'E_XYL      ','E_CO2      ','E_PM_10    ','E_PM25     ','E_SO4I     ',&
'E_NO3I     ','E_PM25I    ','E_ORGI     ','E_ECI      ','E_SO4J     ',&
'E_NO3J     ','E_PM25J    ','E_ORGJ     ','E_ECJ      '/)
cname=(/'Carbon Monoxide ','Ammonia NH3     ','NO              ', &
'NO2             ','SO2             ','Acetaldehyde    ','METHANE         ',&
'C3+Aldehydes    ','Ethene          ','Ethane          ','Ethanol         ',&
'Internal OLE    ','Methanol        ','Formaldehyde    ','Isoprene        ',&
'TOB (R-C=C)     ','Paraffin (C-C)  ','Terpenes        ','Toluene+others  ',&
'Xylene+others   ','Carbon Dioxide  ','PM_10           ','PM_25           ',&
'Sulfates        ','Nitrates        ','PM25I           ','Organic         ',&
'Elemental Carbon','SulfatesJ       ','NitratesJ       ','PM25J           ',&
'Organic Carbon  ','Elemental Carbon'/)
fnameA=(/'TACO__2016.csv   '  ,&
 'TANH3_2016.csv   ','TANOx_2016.csv   ','TANOx_2016.csv   ','TASO2_2016.csv   ',&
 'CBM05_ALD2_A.txt ','CBM05_CH4_A.txt  ','CBM05_ALDX_A.txt ','CBM05_ETH_A.txt  ',&
 'CBM05_ETHA_A.txt ','CBM05_ETOH_A.txt ','CBM05_IOLE_A.txt ','CBM05_MEOH_A.txt ',&
 'CBM05_FORM_A.txt ','CBM05_ISOP_A.txt ','CBM05_OLE_A.txt  ','CBM05_PAR_A.txt  ',&
 'CBM05_TERP_A.txt ','CBM05_TOL_A.txt  ','CBM05_XYL_A.txt  ',&
 'TACO2_2016.csv   ','TAPM102016.csv   ','TAPM2_2016.csv   ',&
 'GSO4_A.txt       ','PNO3_A.txt       ','OTHE_A.txt       ','POA_A.txt        ',&
 'PEC_A.txt        ','TACH4_2016.csv   ','TACN__2016.csv   '/)
 fnameM=(/'TMCO__2016.csv   ',&
'TMNH3_2016.csv   ','TMNO__2016.csv   ','TMNO2_2016.csv   ','TMSO2_2016.csv   ',&
'CBM05_ALD2_M.txt ','CBM05_CH4_M.txt  ','CBM05_ALDX_M.txt ','CBM05_ETH_M.txt  ',&
'CBM05_ETHA_M.txt ','CBM05_ETOH_M.txt ','CBM05_IOLE_M.txt ','CBM05_MEOH_M.txt ',&
'CBM05_FORM_M.txt ','CBM05_ISOP_M.txt ','CBM05_OLE_M.txt  ','CBM05_PAR_M.txt  ',&
'CBM05_TERP_M.txt ','CBM05_TOL_M.txt  ','CBM05_XYL_M.txt  ',&
'TMCO2_2016.csv   ','TMPM102016.csv   ','TMPM2_2016.csv   ',&
'GSO4_M.txt       ','PNO3_M.txt       ','OTHE_M.txt       ','POA_M.txt        ',&
'PEC_M.txt        ','TMCH4_2016.csv   ','TMCN__2016.csv   '/)
 fnameP=(/'T_ANNCO.csv      ',&
'T_ANNNH3.csv     ','T_ANNNOX.csv     ','T_ANNNOX.csv     ','T_ANNSO2.csv     ',&
'CBM05_ALD2_P.txt ','CBM05_CH4_P.txt  ','CBM05_ALDX_P.txt ','CBM05_ETH_P.txt  ',&
'CBM05_ETHA_P.txt ','CBM05_ETOH_P.txt ','CBM05_IOLE_P.txt ','CBM05_MEOH_P.txt ',&
'CBM05_FORM_P.txt ','CBM05_ISOP_P.txt ','CBM05_OLE_P.txt  ','CBM05_PAR_P.txt  ',&
'CBM05_TERP_P.txt ','CBM05_TOL_P.txt  ','CBM05_XYL_P.txt  ',&
'T_ANNCO2.csv     ','T_ANNPM10.csv    ','T_ANNPM25.csv    ',&
'GSO4_P.txt       ','PNO3_P.txt       ','OTHE_P.txt       ','POA_P.txt        ',&
'PEC_P.txt        ','T_ANNCH4.csv     ','T_ANNCN.csv      '/)
        isp=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10, &
             11,12,13,14,15, 16,17,18,19,20, &
             21,22,23,24,25, 26,27,28,29,30, &
             31,32,33]
        WTM=(/28., 17., 30., 46., 64.,32.,  16., 32., 32.,32.,32.,&   !
             64., 16., 16., 80., 32.,16., 160.,112.,128., 44.,&
             3600.,3600.,3600.,3600.,3600.,3600.,3600./)
        call lee_namelist_mecha('cbm05  ')
    case("mozart")
        print *,"Setup variables for ",mecha
        nf=39    ! number of files antropogenic
        ns=37    ! number of compounds
        radm=ns+5 ! number of Mechanism classes
        ipm=32  ! Posicion del archivo PM2.5
        icn=39    ! Posicion archivo CN del INEM
        jcn=37    ! Posicion archivo CN de especiacion
        imt=38    ! Posicion archivo CH4 del INEM
        jmt=6     ! Posicion archivo CH4 de especiacion
        allocate(ename(radm),cname(radm))
        allocate(isp(radm))
        allocate(wtm(ns))
        allocate(fnameA(nf),fnameM(nf),fnameP(nf))
        allocate(scala(nf),scalm(nf),scalp(nf))
ename=(/'E_CO       ','E_NO       ','E_NO2      ','E_NH3      ','E_SO2      ','E_CH4      ',&
'E_BENZENE  ','E_BIGALK   ','E_BIGENE   ','E_C10H16   ','E_C2H2     ','E_C2H4     ',&
'E_C2H5OH   ','E_C2H6     ','E_C3H6     ','E_C3H8     ','E_CH2O     ','E_CH3CHO   ',&
'E_CH3COCH3 ','E_GLY      ','E_HCOOH    ','E_ISOP     ','E_MACR     ','E_MEK      ',&
'E_CH3OH    ','E_MGLY     ','E_MVK      ','E_TOLUENE  ','E_XYLENE   ',&
'E_CO2      ','E_PM_10    ','E_PM25     ',&
'E_SO4I     ','E_NO3I     ','E_PM25I    ','E_ORGI     ','E_ECI      ',&
'E_SO4J     ','E_NO3J     ','E_PM25J    ','E_ORGJ     ','E_ECJ      '/)
cname=(/'Carbon Monoxide ','Nitrogen Oxide  ','Nitrogen Dioxide',&
'Ammonia         ','Sulfur Dioxide  ','Methane         ','Benzene         ',&
'Lumped Alkan C>3','Lumped Alkanes  ','A Pinene        ','Ethyne          ',&
'Ethene          ','Ethanol         ','Ethane          ','Propene         ',&
'Propane         ','Formaldehyde    ','Acetaldehyde    ','Acetone         ',&
'Glyoxal         ','Formic Acid     ','Isoprene        ','Methacrolein    ',&
'Methyl Ethyl Ket','Methanol        ','Methyl Glyoxal  ','Methyl Vinyl Ket',&
'Toluene         ','Xylenes         ','Carbon Dioxide  ','PM_10           ',&
'PM_25           ','Sulfates        ','Nitrates        ','OTHER           ',&
'Organic Carbon  ','Elemental Carbon','SulfatesJ       ','NitratesJ       ',&
'OTHER           ','Organic Carbon  ','Elemental Carbon'/)
fnameA=(/'TACO__2016.csv   ','TANOx_2016.csv   ','TANOx_2016.csv   ',&
'TANH3_2016.csv   ','TASO2_2016.csv   ',&
'MOZART_CH4_A.txt ','MOZART_BENZ_A.txt','MOZART_BIGA_A.txt','MOZART_BIGE_A.txt',&
'MOZART_C10H_A.txt','MOZART_C2H2_A.txt','MOZART_C2H4_A.txt','MOZART_C2H5_A.txt',&
'MOZART_C2H6_A.txt','MOZART_C3H6_A.txt','MOZART_C3H8_A.txt','MOZART_CH2O_A.txt',&
'MOZART_CH3C_A.txt','MOZART_CH3O_A.txt','MOZART_GLYO_A.txt','MOZART_HCOO_A.txt',&
'MOZART_ISOP_A.txt','MOZART_MACR_A.txt','MOZART_MEK_A.txt ','MOZART_METO_A.txt',&
'MOZART_MGLY_A.txt','MOZART_MVK_A.txt ','MOZART_TOLU_A.txt','MOZART_XYLE_A.txt',&
'TACO2_2016.csv   ','TAPM102016.csv   ','TAPM2_2016.csv   ', &
'GSO4_A.txt       ','PNO3_A.txt       ','OTHE_M.txt       ','POA_A.txt        ',&
'PEC_A.txt        ','TACH4_2016.csv   ','TACN__2016.csv   '/)
 fnameM=(/'TMCO__2016.csv   ',&
'TMNO__2016.csv   ','TMNO2_2016.csv   ','TMNH3_2016.csv   ','TMSO2_2016.csv   ',&
'MOZART_CH4_M.txt ','MOZART_BENZ_M.txt','MOZART_BIGA_M.txt','MOZART_BIGE_M.txt',&
'MOZART_C10H_M.txt','MOZART_C2H2_M.txt','MOZART_C2H4_M.txt','MOZART_C2H5_M.txt',&
'MOZART_C2H6_M.txt','MOZART_C3H6_M.txt','MOZART_C3H8_M.txt','MOZART_CH2O_M.txt',&
'MOZART_CH3C_M.txt','MOZART_CH3O_M.txt','MOZART_GLYO_M.txt','MOZART_HCOO_M.txt',&
'MOZART_ISOP_M.txt','MOZART_MACR_M.txt','MOZART_MEK_M.txt ','MOZART_METO_M.txt',&
'MOZART_MGLY_M.txt','MOZART_MVK_M.txt ','MOZART_TOLU_M.txt','MOZART_XYLE_M.txt',&
'TMCO2_2016.csv   ','TMPM102016.csv   ','TMPM2_2016.csv   ', &
'GSO4_M.txt       ','PNO3_M.txt       ','OTHE_M.txt       ','POA_M.txt        ',&
'PEC_M.txt        ','TMCH4_2016.csv   ','TMCN__2016.csv   '/)
 fnameP=(/'T_ANNCO.csv      ',&
'T_ANNNOX.csv     ','T_ANNNOX.csv     ','T_ANNNH3.csv     ','T_ANNSO2.csv     ',&
'MOZART_CH4_P.txt ','MOZART_BENZ_P.txt','MOZART_BIGA_P.txt','MOZART_BIGE_P.txt',&
'MOZART_C10H_P.txt','MOZART_C2H2_P.txt','MOZART_C2H4_P.txt','MOZART_C2H5_P.txt',&
'MOZART_C2H6_P.txt','MOZART_C3H6_P.txt','MOZART_C3H8_P.txt','MOZART_CH2O_P.txt',&
'MOZART_CH3C_P.txt','MOZART_CH3O_P.txt','MOZART_GLYO_P.txt','MOZART_HCOO_P.txt',&
'MOZART_ISOP_P.txt','MOZART_MACR_P.txt','MOZART_MEK_P.txt ','MOZART_METO_P.txt',&
'MOZART_MGLY_P.txt','MOZART_MVK_P.txt ','MOZART_TOLU_P.txt','MOZART_XYLE_P.txt',&
'T_ANNCO2.csv     ','T_ANNPM10.csv    ','T_ANNPM25.csv    ', &
'GSO4_P.txt       ','PNO3_P.txt       ','OTHE_P.txt       ','POA_P.txt        ',&
'PEC_P.txt        ','T_ANNCH4.csv     ','T_ANNCN.csv      '/)
        isp=(/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10, &
        11,12,13,14,15, 16,17,18,19,20, &
        21,22,23,24,25, 26,27,28,29,30, &
        31,32,33,34,35, 36,37,38,39,40, &
        41,42/)
        WTM=(/ 28.0, 30.00, 46.00, 17.00, 64.0,  16.043,&
        78.00, 72.00, 56.00,136.00,26.00, 28.00,&
        46.00,30.00, 42.00, 44.00, 30.00, 44.00,&
        56.00, 58.00, 46.00, 68.00, 70.00, 72.00,& !
        32.00, 72.00, 70.00, 92.00, 106.00,&
        44., 7*3600./)
        call lee_namelist_mecha('mozart ')

    case("racm2")
        print *,"Setup variables for ",mecha
        nf=55    ! number of files antropogenic
        ns=53    ! number of compounds
        radm=ns+5 ! number of Mechanism classes
        ipm=48  ! Posicion del archivo PM2.5
        icn=55    ! Posicion archivo CN del INEM
        jcn=52    ! Posicion archivo CN de especiacion
        imt=54    ! Posicion archivo CH4 del INEM
        jmt=6     ! Posicion archivo CH4 de especiacion
        allocate(ename(radm),cname(radm))
        allocate(isp(radm))
        allocate(wtm(ns))
        allocate(fnameA(nf),fnameM(nf),fnameP(nf))
        allocate(scala(nf),scalm(nf),scalp(nf))
ename=(/'E_CO   ','E_NH3  ','E_NO   ', &
'E_NO2  ','E_SO2  ','E_CH4  ','E_ETH  ','E_HC3  ','E_HC5  ','E_HC8  ',&
'E_ETE  ','E_OLI  ','E_OLT  ','E_DIEN ','E_BEN  ','E_TOL  ','E_XYL  ',&
'E_XYP  ','E_XYO  ','E_HCHO ','E_ALD  ','E_ACT  ','E_MEK  ','E_KET  ',&
'E_ROH  ','E_HKET ','E_CSL  ','E_PHEN ','E_API  ','E_ISO  ','E_LIM  ',&
'E_MVK  ','E_MACR ','E_ONIT ','E_GLY  ','E_MGLY ','E_UALD ','E_ACD  ',&
'E_ORA2 ','E_ACE  ','E_BALD ','E_EOH  ','E_ETEG ','E_ORA1 ','E_MOH  ',&
'E_CO2  ','E_PM_10','E_PM_25', &
'E_SO4I ','E_NO3I ','E_PM25I','E_ORGI ','E_ECI  ',&
'E_SO4J ','E_NO3J ','E_PM25J','E_ORGJ ','E_ECJ  '/)
 cname=(/'Carbon Monoxide ','Ammonia NH3     ','Nitrogen Oxide  ', &
'Nitrogen Dioxide','Sulfur Dioxide  ','Methane CH4     ','Ethane          ',&
'Alkanes, alcohol','Alkanes, alcohol','Alkanes, alcohol','Ethene          ',&
'Internal alkenes','Terminal alkenes','Butadiene and ot','Benzene         ',&
'Toluene and less','m-Xylene        ','p-Xylene        ','o-xylene        ',&
'Formaldehyde    ','C3 and higher al','Acetone         ','Methyl ethyl ket',&
'Ketones         ','C3 and higher al','Hydroxy ketone  ','Cresol and other',&
'Phenol          ','Alpha-pinenes an','Isoprene        ','d-limonene      ',&
'Methyl vinyl ket','Methacrolein    ','Organic nitrate ','Glyoxal CHOCHO  ',&
'Methylglyoxal an','Unsaturated alde','Acetaldehyde    ','Acetic acid and ',&
'Acetylene       ','Benzaldehyde and','Ethanol         ','Ethylene glycol ',&
'Formic acid     ','Methanol        ','Carbon Dioxide  ','PM_10           ',&
'PM_25           ','Sulfates Particl','Nitrates Particl','OTHER PM25I     ',&
'Organic Carbon  ','Elemental Carbon','SulfatesJ Partic','NitratesJ Partic',&
'OTHER PM25J     ','OrganicJ Carbon ','Elemental CarboJ'/)
fnameA=(/'TACO__2016.csv   ','TANH3_2016.csv   ','TANOx_2016.csv   ','TANOx_2016.csv   ',&
'TASO2_2016.csv   ','RACM2_CH4_A.txt  ','RACM2_ETH_A.txt  ','RACM2_HC3_A.txt  ',&
'RACM2_HC5_A.txt  ','RACM2_HC8_A.txt  ','RACM2_ETE_A.txt  ','RACM2_OLI_A.txt  ',&
'RACM2_OLT_A.txt  ','RACM2_DIEN_A.txt ','RACM2_BEN_A.txt  ','RACM2_TOL_A.txt  ',&
'RACM2_XYM_A.txt  ','RACM2_XYP_A.txt  ','RACM2_XYO_A.txt  ','RACM2_HCHO_A.txt ',&
'RACM2_ALD_A.txt  ','RACM2_ACT_A.txt  ','RACM2_MEK_A.txt  ','RACM2_KET_A.txt  ',&
'RACM2_ROH_A.txt  ','RACM2_HKET_A.txt ','RACM2_CSL_A.txt  ','RACM2_PHEN_A.txt ',&
'RACM2_API_A.txt  ','RACM2_ISO_A.txt  ','RACM2_LIM_A.txt  ','RACM2_MVK_A.txt  ',&
'RACM2_MACR_A.txt ','RACM2_ONIT_A.txt ','RACM2_GLY_A.txt  ','RACM2_MGLY_A.txt ',&
'RACM2_UALD_A.txt ','RACM2_ACD_A.txt  ','RACM2_ORA2_A.txt ','RACM2_ACE_A.txt  ',&
'RACM2_BALD_A.txt ','RACM2_EOH_A.txt  ','RACM2_ETEG_A.txt ','RACM2_ORA1_A.txt ',&
'RACM2_MOH_A.txt  ','TACO2_2016.csv   ','TAPM102016.csv   ','TAPM2_2016.csv   ',&
'GSO4_A.txt       ','PNO3_A.txt       ','OTHE_A.txt       ','POA_A.txt        ',&
'PEC_A.txt        ','TACH4_2016.csv   ','TACN__2016.csv   '/)
 fnameM=(/'TMCO__2016.csv   ','TMNH3_2016.csv   ','TMNO__2016.csv   ','TMNO2_2016.csv   ',&
'TMSO2_2016.csv   ','RACM2_CH4_M.txt  ','RACM2_ETH_M.txt  ','RACM2_HC3_M.txt  ',&
'RACM2_HC5_M.txt  ','RACM2_HC8_M.txt  ','RACM2_ETE_M.txt  ','RACM2_OLI_M.txt  ',&
'RACM2_OLT_M.txt  ','RACM2_DIEN_M.txt ','RACM2_BEN_M.txt  ','RACM2_TOL_M.txt  ',&
'RACM2_XYM_M.txt  ','RACM2_XYP_M.txt  ','RACM2_XYO_M.txt  ','RACM2_HCHO_M.txt ',&
'RACM2_ALD_M.txt  ','RACM2_ACT_M.txt  ','RACM2_MEK_M.txt  ','RACM2_KET_M.txt  ',&
'RACM2_ROH_M.txt  ','RACM2_HKET_M.txt ','RACM2_CSL_M.txt  ','RACM2_PHEN_M.txt ',&
'RACM2_API_M.txt  ','RACM2_ISO_M.txt  ','RACM2_LIM_M.txt  ','RACM2_MVK_M.txt  ',&
'RACM2_MACR_M.txt ','RACM2_ONIT_M.txt ','RACM2_GLY_M.txt  ','RACM2_MGLY_M.txt ',&
'RACM2_UALD_M.txt ','RACM2_ACD_M.txt  ','RACM2_ORA2_M.txt ','RACM2_ACE_M.txt  ',&
'RACM2_BALD_M.txt ','RACM2_EOH_M.txt  ','RACM2_ETEG_M.txt ','RACM2_ORA1_M.txt ',&
'RACM2_MOH_M.txt  ','TMCO2_2016.csv   ','TMPM102016.csv   ','TMPM2_2016.csv   ', &
'GSO4_M.txt       ','PNO3_M.txt       ','OTHE_M.txt       ','POA_M.txt        ',&
'PEC_M.txt        ','TMCH4_2016.csv   ','TMCN__2016.csv   '/)
 fnameP=(/'T_ANNCO.csv      ','T_ANNNH3.csv     ','T_ANNNOX.csv     ','T_ANNNOX.csv     ',&
'T_ANNSO2.csv     ','RACM2_CH4_P.txt  ','RACM2_ETH_P.txt  ','RACM2_HC3_P.txt  ',&
'RACM2_HC5_P.txt  ','RACM2_HC8_P.txt  ','RACM2_ETE_P.txt  ','RACM2_OLI_P.txt  ',&
'RACM2_OLT_P.txt  ','RACM2_DIEN_P.txt ','RACM2_BEN_P.txt  ','RACM2_TOL_P.txt  ',&
'RACM2_XYM_P.txt  ','RACM2_XYP_P.txt  ','RACM2_XYO_P.txt  ','RACM2_HCHO_P.txt ',&
'RACM2_ALD_P.txt  ','RACM2_ACT_P.txt  ','RACM2_MEK_P.txt  ','RACM2_KET_P.txt  ',&
'RACM2_ROH_P.txt  ','RACM2_HKET_P.txt ','RACM2_CSL_P.txt  ','RACM2_PHEN_P.txt ',&
'RACM2_API_P.txt  ','RACM2_ISO_P.txt  ','RACM2_LIM_P.txt  ','RACM2_MVK_P.txt  ',&
'RACM2_MACR_P.txt ','RACM2_ONIT_P.txt ','RACM2_GLY_P.txt  ','RACM2_MGLY_P.txt ',&
'RACM2_UALD_P.txt ','RACM2_ACD_P.txt  ','RACM2_ORA2_P.txt ','RACM2_ACE_P.txt  ',&
'RACM2_BALD_P.txt ','RACM2_EOH_P.txt  ','RACM2_ETEG_P.txt ','RACM2_ORA1_P.txt ',&
'RACM2_MOH_P.txt  ','T_ANNCO2.csv     ','T_ANNPM10.csv    ','T_ANNPM25.csv    ',&
'GSO4_P.txt       ','PNO3_P.txt       ','OTHE_P.txt       ','POA_P.txt        ',&
'PEC_P.txt        ','T_ANNCH4.csv     ','T_ANNCN.csv      '/)
        isp=(/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10, &
        11,12,13,14,15, 16,17,18,19,20, &
        21,22,23,24,25, 26,27,28,29,30,&
        31,32,33,34,35, 36,37,38,39,40,&
        41,42,43,44,45, 46,47,48,49,50,&
        51,52,53,54,55, 56,57,58/)
        WTM=(/28., 17., 30., 46., 64.,   16., 30., 44., 72., 114.,&
        28., 68., 42., 54., 78.,   92.,106.,106.,106., 30., &
        58., 58., 72., 86., 60.,   74.,108., 94.,136., 68., &
        136., 70., 70.,119., 58.,   72., 84., 44., 60., 26., &
        106., 46., 62., 46., 32.,    44.,&
        3600.,3600.,3600.,3600.,3600.,3600.,3600./)
        call lee_namelist_mecha('racm   ')
    case("radm2")
        print *,"Setup variables for ",mecha
        nf=36    ! number of files antropogenic
        ns=34    ! number of compounds
        radm=ns+5 ! number of Mechanism classes
        ipm=29  ! Posicion del archivo PM2.5
        icn=36    ! Posicion archivo CN del INEM
        jcn=34    ! Posicion archivo CN de especiacion
        imt=35    ! Posicion archivo CH4 del INEM
        jmt=7     ! Posicion archivo CH4 de especiacion
        allocate(ename(radm),cname(radm))
        allocate(isp(radm))
        allocate(wtm(ns))
        allocate(fnameA(nf),fnameM(nf),fnameP(nf))
        allocate(scala(nf),scalm(nf),scalp(nf))
ename=(/'E_CO   ','E_NH3  ','E_NO   ', &
'E_NO2  ','E_SO2  ','E_ALD  ','E_CH4  ','E_CSL  ','E_ETH  ','E_GLY  ', &
'E_HC3  ','E_HC5  ','E_HC8  ','E_HCHO ','E_ISO  ','E_KET  ','E_MACR ', &
'E_MGLY ','E_MVK  ','E_OL2  ','E_OLI  ','E_OLT  ','E_ORA1 ','E_ORA2 ', &
'E_TOL  ','E_XYL  ','E_CO2  ','E_PM_10','E_PM25 ','E_SO4I ','E_NO3I ','E_PM25I',&
'E_ORGI ','E_ECI  ','E_SO4J ','E_NO3J ','E_PM25J','E_ORGJ ','E_ECJ  '/)
cname=(/'Carbon Monoxide ','NH3             ','NO              ', &
'NO2             ','SO2             ','ALDEHYDES       ','METHANE         ','CRESOL          ',&
'Ethane          ','Glyoxal         ','HC3             ','HC5             ','HC8             ',&
'HCHO            ','ISOPRENE        ','Acetone         ','Acrolein        ','MGLY            ',&
'Methyl Vinil Ket','Alkenes         ','alkenes         ','Terminal Alkynes','Formic Acid     ',&
'Acetic Acid     ','TOLUENE         ','XYLENE          ','Carbon Dioxide  ','PM_10           ',&
'PM_25           ','Sulfates        ','Nitrates        ','PM25I           ','Organic         ',&
'Elemental Carbon','SulfatesJ       ','NitratesJ       ','PM25J           ','Organic         ',&
'Elemental Carbon'/)
 fnameA=['TACO__2016.csv   ',&
& 'TANH3_2016.csv   ','TANOx_2016.csv   ','TANOx_2016.csv   ','TASO2_2016.csv   ',&
& 'RADM-2_ALD_A.txt ','RADM-2_CH4_A.txt ','RADM-2_CSL_A.txt ','RADM-2_ETH_A.txt ',&
& 'RADM-2_GLY_A.txt ','RADM-2_HC3_A.txt ','RADM-2_HC5_A.txt ','RADM-2_HC8_A.txt ',&
& 'RADM-2_HCHO_A.txt','RADM-2_ISO_A.txt ','RADM-2_KET_A.txt ','RADM-2_MACR_A.txt',&
& 'RADM-2_MGLY_A.txt','RADM-2_MVK_A.txt ','RADM-2_OL2_A.txt ','RADM-2_OLI_A.txt ',&
& 'RADM-2_OLT_A.txt ','RADM-2_ORA1_A.txt','RADM-2_ORA2_A.txt','RADM-2_TOL_A.txt ',&
& 'RADM-2_XYL_A.txt ','TACO2_2016.csv   ','TAPM102016.csv   ','TAPM2_2016.csv   ', &
& 'GSO4_A.txt       ','PNO3_A.txt       ','OTHE_M.txt       ','POA_A.txt        ',&
& 'PEC_A.txt        ','TACH4_2016.csv   ','TACN__2016.csv   ']
 fnameM=['TMCO__2016.csv   ',&
& 'TMNH3_2016.csv   ','TMNO__2016.csv   ','TMNO2_2016.csv   ','TMSO2_2016.csv   ',&
& 'RADM-2_ALD_M.txt ','RADM-2_CH4_M.txt ','RADM-2_CSL_M.txt ','RADM-2_ETH_M.txt ',&
& 'RADM-2_GLY_M.txt ','RADM-2_HC3_M.txt ','RADM-2_HC5_M.txt ','RADM-2_HC8_M.txt ',&
& 'RADM-2_HCHO_M.txt','RADM-2_ISO_M.txt ','RADM-2_KET_M.txt ','RADM-2_MACR_M.txt',&
& 'RADM-2_MGLY_M.txt','RADM-2_MVK_M.txt ','RADM-2_OL2_M.txt ','RADM-2_OLI_M.txt ',&
& 'RADM-2_OLT_M.txt ','RADM-2_ORA1_M.txt','RADM-2_ORA2_M.txt','RADM-2_TOL_M.txt ',&
& 'RADM-2_XYL_M.txt ','TMCO2_2016.csv   ','TMPM102016.csv   ','TMPM2_2016.csv   ', &
& 'GSO4_M.txt       ','PNO3_M.txt       ','OTHE_M.txt       ','POA_M.txt        ',&
& 'PEC_M.txt        ','TMCH4_2016.csv   ','TMCN__2016.csv   ']
 fnameP=['T_ANNCO.csv      ',&
& 'T_ANNNH3.csv     ','T_ANNNOX.csv     ','T_ANNNOX.csv     ','T_ANNSO2.csv     ',&
& 'RADM-2_ALD_P.txt ','RADM-2_CH4_P.txt ','RADM-2_CSL_P.txt ','RADM-2_ETH_P.txt ',&
& 'RADM-2_GLY_P.txt ','RADM-2_HC3_P.txt ','RADM-2_HC5_P.txt ','RADM-2_HC8_P.txt ',&
& 'RADM-2_HCHO_P.txt','RADM-2_ISO_P.txt ','RADM-2_KET_P.txt ','RADM-2_MACR_P.txt',&
& 'RADM-2_MGLY_P.txt','RADM-2_MVK_P.txt ','RADM-2_OL2_P.txt ','RADM-2_OLI_P.txt ',&
& 'RADM-2_OLT_P.txt ','RADM-2_ORA1_P.txt','RADM-2_ORA2_P.txt','RADM-2_TOL_P.txt ',&
& 'RADM-2_XYL_P.txt ','T_ANNCO2.csv     ','T_ANNPM10.csv    ','T_ANNPM25.csv    ', &
& 'GSO4_P.txt       ','PNO3_P.txt       ','OTHE_P.txt       ','POA_P.txt        ',&
& 'PEC_P.txt        ','T_ANNCH4.csv     ','T_ANNCN.csv      ']
        isp=[ 1, 2, 3, 4, 5,  6, 7, 8, 9,10, &
             11,12,13,14,15, 16,17,18,19,20, &
             21,22,23,24,25, 26,27,28,29,30, &
             31,32,33,34,35, 36,37,38,39]
        WTM=[28., 17., 30., 46., 64.,   44.,16.,108.,30.,58.,&   !
        &    44., 72.,114., 30.,68., 72.,   70.,72., 70.,28.,56.,&
        &    42., 46., 60., 92.,106.,44.,&
        &  7*3600.]! MW 3600 for unit conversion to ug/s
        call lee_namelist_mecha('radm   ')
    case("saprc99")
        print *,"Setup variables for ",mecha
        nf=47    ! number of files antropogenic
        ns=45    ! number of compounds
        radm=ns+5 ! number of Mechanism classes
        ipm=40  ! Posicion del archivo PM2.5
        icn=47    ! Posicion archivo CN del INEM
        jcn=45    ! Posicion archivo CN de especiacion
        imt=46    ! Posicion archivo CH4 del INEM
        jmt=6     ! Posicion archivo CH4 de especiacion
        allocate(ename(radm),cname(radm))
        allocate(isp(radm))
        allocate(wtm(ns))
        allocate(fnameA(nf),fnameM(nf),fnameP(nf))
        allocate(scala(nf),scalm(nf),scalp(nf))
ename=['E_CO       ','E_NO       ','E_NO2      ','E_NH3      ','E_SO2      ',&
'E_CH4      ','E_ACET     ','E_ALK3     ','E_ALK4     ','E_ALK5     ','E_ARO1     ',&
'E_ARO2     ','E_BACL     ','E_BALD     ','E_C2H6     ','E_C3H8     ','E_CCHO     ',&
'E_CCO_OH   ','E_CRES     ','E_ETHENE   ','E_GLY      ','E_HCHO     ','E_HCOOH    ',&
'E_ISOPRENE ','E_ISOPROD  ','E_MEK      ','E_MEOH     ','E_METHACRO ','E_MGLY     ',&
'E_MVK      ','E_OLE1     ','E_OLE2     ','E_PHEN     ','E_PROD2    ','E_RCHO     ',&
'E_RCO_OH   ','E_TERP     ','E_CO2      ','E_PM_10    ','E_PM25     ','E_SO4I     ',&
'E_NO3I     ','E_PM25I    ','E_ORGI     ','E_ECI      ','E_SO4J     ','E_NO3J     ',&
'E_PM25J    ','E_ORGJ     ','E_ECJ      ']
cname=['Carbon Monoxide ','Nitrogen Oxide  ','Nitrogen Dioxide','Ammonia         ',&
'Sulfur Dioxide  ','Methane         ','Acetone         ','Alkanes 3       ',&
'Alkanes 4       ','Alkanes 5       ','Aromatics 1     ','Aromatics 2     ',&
'Biacetyl        ','Aromatic aldehyd','Alkanes 1       ','Alkanes 2       ',&
'Acetaldehyde    ','Acetic Acid     ','Cresol          ','Ethene          ',&
'Glyoxal         ','Formaldehyde    ','Formic Acid     ','Isoprene        ',&
'Lumped isoprene ','Ketones and othe','Methanol        ','Methacrolein    ',&
'Methyl Glyoxal  ','Methyl Vinyl Ket','Alkenes 1       ','Alkenes 2       ',&
'Phenol          ','Ketones + other ','Lumped C3+ Aldeh','Higher Carboxyl ',&
'Terpenes        ','Carbon Dioxide  ','PM_10           ','PM_25           ',&
'Sulfates Particl','Nitrates Particl','OTHER Particles ','Organic C partic',&
'Elemental Carbon','SulfatesJ       ','NitratesJ       ','OTHER           ',&
'Organic Carbon  ','Elemental Carbon']
fnameA=(/'TACO__2016.csv    ',&
'TANOx_2016.csv    ','TANOx_2016.csv    ','TANH3_2016.csv    ','TASO2_2016.csv    ',&
'SAPRC99_CH4_A.txt ','SAPRC99_ACET_A.txt','SAPRC99_ALK3_A.txt','SAPRC99_ALK4_A.txt',&
'SAPRC99_ALK5_A.txt','SAPRC99_ARO1_A.txt','SAPRC99_ARO2_A.txt','SAPRC99_BACL_A.txt',&
'SAPRC99_BALD_A.txt','SAPRC99_ALK1_A.txt','SAPRC99_ALK2_A.txt','SAPRC99_CCHO_A.txt',&
'SAPRC99_AACD_M.txt','SAPRC99_CRES_A.txt','SAPRC99_ETHE_A.txt','SAPRC99_GLY_A.txt ',&
'SAPRC99_HCHO_A.txt','SAPRC99_FACD_A.txt','SAPRC99_ISOP_A.txt','SAPRC99_IPRD_A.txt',&
'SAPRC99_MEK_A.txt ','SAPRC99_MEOH_A.txt','SAPRC99_MACR_A.txt','SAPRC99_MGLY_A.txt',&
'SAPRC99_MVK_A.txt ','SAPRC99_OLE1_A.txt','SAPRC99_OLE2_A.txt','SAPRC99_PHEN_A.txt',&
'SAPRC99_PRD2_A.txt','SAPRC99_RCHO_A.txt','SAPRC99_PACD_A.txt','SAPRC99_TERP_A.txt',&
'TACO2_2016.csv    ','TAPM102016.csv    ','TAPM2_2016.csv    ','GSO4_A.txt        ',&
'PNO3_A.txt        ','OTHE_M.txt        ','POA_A.txt         ','PEC_A.txt         ',&
'TACH4_2016.csv    ','TACN__2016.csv    '/)
 fnameM =(/'TMCO__2016.csv    ',&
'TMNO__2016.csv    ','TMNO2_2016.csv    ','TMNH3_2016.csv    ','TMSO2_2016.csv    ',&
'SAPRC99_CH4_M.txt ','SAPRC99_ACET_M.txt','SAPRC99_ALK3_M.txt','SAPRC99_ALK4_M.txt',&
'SAPRC99_ALK5_M.txt','SAPRC99_ARO1_M.txt','SAPRC99_ARO2_M.txt','SAPRC99_BACL_M.txt',&
'SAPRC99_BALD_M.txt','SAPRC99_ALK1_M.txt','SAPRC99_ALK2_M.txt','SAPRC99_CCHO_M.txt',&
'SAPRC99_AACD_M.txt','SAPRC99_CRES_M.txt','SAPRC99_ETHE_M.txt','SAPRC99_GLY_M.txt ',&
'SAPRC99_HCHO_M.txt','SAPRC99_FACD_M.txt','SAPRC99_ISOP_M.txt','SAPRC99_IPRD_M.txt',&
'SAPRC99_MEK_M.txt ','SAPRC99_MEOH_M.txt','SAPRC99_MACR_M.txt','SAPRC99_MGLY_M.txt',&
'SAPRC99_MVK_M.txt ','SAPRC99_OLE1_M.txt','SAPRC99_OLE2_M.txt','SAPRC99_PHEN_M.txt',&
'SAPRC99_PRD2_M.txt','SAPRC99_RCHO_M.txt','SAPRC99_PACD_M.txt','SAPRC99_TERP_M.txt',&
'TMCO2_2016.csv    ','TMPM102016.csv    ','TMPM2_2016.csv    ','GSO4_M.txt        ',&
'PNO3_M.txt        ','OTHE_M.txt        ','POA_M.txt         ','PEC_M.txt         ',&
'TMCH4_2016.csv    ','TMCN__2016.csv    '/)
fnameP=(/'T_ANNCO.csv       ',&
'T_ANNNOX.csv      ','T_ANNNOX.csv      ','T_ANNNH3.csv      ','T_ANNSO2.csv      ',&
'SAPRC99_CH4_P.txt ','SAPRC99_ACET_P.txt','SAPRC99_ALK3_P.txt','SAPRC99_ALK4_P.txt',&
'SAPRC99_ALK5_P.txt','SAPRC99_ARO1_P.txt','SAPRC99_ARO2_P.txt','SAPRC99_BACL_P.txt',&
'SAPRC99_BALD_P.txt','SAPRC99_ALK1_P.txt','SAPRC99_ALK2_P.txt','SAPRC99_CCHO_P.txt',&
'SAPRC99_AACD_P.txt','SAPRC99_CRES_P.txt','SAPRC99_ETHE_P.txt','SAPRC99_GLY_P.txt ',&
'SAPRC99_HCHO_P.txt','SAPRC99_FACD_P.txt','SAPRC99_ISOP_P.txt','SAPRC99_IPRD_P.txt',&
'SAPRC99_MEK_P.txt ','SAPRC99_MEOH_P.txt','SAPRC99_MACR_P.txt','SAPRC99_MGLY_P.txt',&
'SAPRC99_MVK_P.txt ','SAPRC99_OLE1_P.txt','SAPRC99_OLE2_P.txt','SAPRC99_PHEN_P.txt',&
'SAPRC99_PRD2_P.txt','SAPRC99_RCHO_P.txt','SAPRC99_PACD_P.txt','SAPRC99_TERP_P.txt',&
'T_ANNCO2.csv      ','T_ANNPM10.csv     ','T_ANNPM25.csv     ','GSO4_P.txt        ',&
'PNO3_P.txt        ','OTHE_P.txt        ','POA_P.txt         ','PEC_P.txt         ',&
'T_ANNCH4.csv      ','T_ANNCN.csv       '/)
        isp=(/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10, &
              11,12,13,14,15, 16,17,18,19,20, &
              21,22,23,24,25, 26,27,28,29,30, &
              31,32,33,34,35, 36,37,38,39,40, &
              41,42,43,44,45, 46,47,48,49,50/)
        WTM=(/ 28.0, 30.00, 46.00, 17.00, 64.0,  16.043,&
              58.08, 58.61, 77.60,118.89, 95.16,118.72,&
              86.09,106.13, 30.07, 36.73, 44.05, 60.05,&
             108.14, 28.05, 58.04, 30.03, 46.03, 68.12,& !
             100.12, 72.11, 32.04, 70.09, 72.07, 70.09,&
              72.34, 75.78, 94.11,116.16, 58.08, 74.08,&
             136.238,44.,&
        7*3600./)
        call lee_namelist_mecha('saprc  ')
    case default
    print *,"   **************************"
    print *,"Mechanism :",mecha," does not exists!!"
    print *,"   **************************"
    stop  "End program, review namelist_emiss.nml"
    end select
end subroutine setup_mecha
!      _
!  ___| |_ ___  _ __ ___
! / __| __/ _ \| '__/ _ \
! \__ \ || (_) | | |  __/
! |___/\__\___/|_|  \___|
!
subroutine store
	IMPLICIT NONE
    integer :: NDIMS
    parameter (NDIMS=6)
    integer :: i,j,k,l
    integer :: ncid
    integer :: periodo,iit,eit,it
    integer :: ikk
    integer :: dimids2(2),dimids3(3),dimids4(4)
    integer,dimension(radm):: id_var
    integer :: id_varlong,id_varlat,id_varpop
    integer :: id_unlimit,id_utmx,id_utmy,id_utmz
    integer :: id,iu,JULDAY
    integer,dimension(NDIMS):: dim,id_dim
    real,ALLOCATABLE :: ea(:,:,:,:)
    character (len=19),dimension(NDIMS) ::sdim
    character(len=44):: FILE_NAME
    character(len=19),dimension(1,1)::Times
    character(len=19):: iTime
    character(8)  :: date
    character(10) :: time
    character(24) :: hoy


    data sdim /"Time               ","DateStrLen         ","west_east          ",&
	&          "south_north        ","bottom_top         ","emissions_zdim_stag"/	

	 print *,"Guarda Archivo"	 
! ******************************************************************
    call date_and_time(date,time)
     hoy=date(7:8)//'-'//mes(date(5:6))//'-'//date(1:4)//' '//time(1:2)//':'//time(3:4)//':'//time(5:10)
    print *,hoy
    write(current_date(1:4),'(I4)') anio
    write(current_date(6:7),'(I2.2)') month
    write(current_date(9:10),'(I2.2)') idia
    JULDAY=juliano(current_date(1:4),current_date(6:7),current_date(9:10))
     do periodo=1,1! 2
	  if(periodo.eq.1) then
        FILE_NAME='wrfchemi.d01.'//trim(mecha)//'.'//trim(zona)//'.'//current_date(1:19)         !******
	   iit= 0
	   eit= 23 !11
	   iTime=current_date
	  else if(periodo.eq.2) then
	   iit=12
	   eit=23
       write(iTime(12:13),'(I2)') iit
        FILE_NAME='wrfchemi.d01.'//trim(mecha)//'.'//trim(zona)//'.'//iTime
	 end if
	  ! Open NETCDF emissions file	
       call check( nf90_create(path =FILE_NAME,cmode = NF90_CLOBBER, ncid = ncid) )
!        call check( nf90_create(path =FILE_NAME,cmode = NF90_NETCDF4,ncid = ncid) )
!     Define dimensiones
		  dim(1)=1
		  dim(2)=19
		  dim(3)=nx
		  dim(4)=ny
		  dim(5)=1!mkx
		  dim(6)=zlev! !8
         if(.not.ALLOCATED(ea)) allocate (ea(dim(3),dim(4),dim(6),dim(1)))
       print *, "dimensions definition ****"
		 call check( nf90_def_dim(ncid,sdim(1), NF90_UNLIMITED, id_dim(1)) )

       do i=2,NDIMS
         call check( nf90_def_dim(ncid, sdim(i), dim(i), id_dim(i)) )
       end do

      dimids2 = (/id_dim(2),id_dim(1)/)
      dimids3 = (/id_dim(3),id_dim(2),id_dim(1) /)
      dimids4 = (/id_dim(3),id_dim(4),id_dim(6),id_dim(1)/)
      print *,"Attributos Globales NF90_GLOBAL"
      !Attributos Globales NF90_GLOBAL
      call check( nf90_put_att(ncid, NF90_GLOBAL, "TITLE",titulo))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "START_DATE",iTime))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "DAY ",cday))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "SIMULATION_START_DATE",iTime))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "WEST-EAST_GRID_DIMENSION",nx))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "SOUTH-NORTH_GRID_DIMENSION",ny))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "BOTTOM-TOP_GRID_DIMENSION",1))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "DX",CDIM*1000))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "DY",CDIM*1000))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LAT",xlat(nx/2,ny/2)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LON",xlon(nx/2,ny/2)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT1",17.5))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT2",29.5))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "MOAD_CEN_LAT",xlat(nx/2,ny/2)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "STAND_LON",xlon(nx/2,ny/2)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LAT",90.))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LON",0.))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "GRIDTYPE","C"))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "GMT",12.))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "JULYR",int2char(current_date(1:4))))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "JULDAY",JULDAY))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ",1))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "MMINLU","USGS"))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "MECHANISM",mecha))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "CREATION_DATE",hoy))

	print *,"Define las variables"
!  Define las variables
	call check( nf90_def_var(ncid, "Times", NF90_CHAR, dimids2,id_unlimit ) )
!  Attributos para cada variable 
      call check( nf90_def_var(ncid, "XLONG", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlong ) )
	        ! Assign  attributes
        call check( nf90_put_att(ncid, id_varlong, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_varlong, "MemoryOrder", "XYZ") )
        call check( nf90_put_att(ncid, id_varlong, "description", "LONGITUDE, WEST IS NEGATIVE") )
        call check( nf90_put_att(ncid, id_varlong, "units", "degree_east"))
        call check( nf90_put_att(ncid, id_varlong, "axis", "X") )
      call check( nf90_def_var(ncid, "XLAT", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlat ) )
	        ! Assign  attributes
        call check( nf90_put_att(ncid, id_varlat, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_varlat, "MemoryOrder", "XYZ") )
        call check( nf90_put_att(ncid, id_varlat, "description", "LATITUDE, SOUTH IS NEGATIVE") )
        call check( nf90_put_att(ncid, id_varlat, "units", "degree_north"))
        call check( nf90_put_att(ncid, id_varlat, "axis", "Y") )
         print *," Pob"
        call check( nf90_def_var(ncid,"POB",NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/) ,id_varpop ) )
            ! Assign  attributes
        call check( nf90_put_att(ncid, id_varpop, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_varpop, "MemoryOrder", "XYZ") )
        call check(nf90_put_att(ncid,id_varpop,"description","Population in each grid"))
        call check( nf90_put_att(ncid, id_varpop, "units", "number"))
! Para Mercator
      call check( nf90_def_var(ncid, "UTMx", NF90_REAL,(/id_dim(3),id_dim(4)/),id_utmx ) )
      ! Assign  attributes
        call check( nf90_put_att(ncid, id_utmx, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_utmx, "MemoryOrder", "XYZ") )
        call check( nf90_put_att(ncid, id_utmx, "description", "UTM coordinate west-east") )
        call check( nf90_put_att(ncid, id_utmx, "units", "km"))
        call check( nf90_put_att(ncid, id_utmx, "axis", "X") )
      call check( nf90_def_var(ncid, "UTMy", NF90_REAL,(/id_dim(3),id_dim(4)/),id_utmy ) )
      ! Assign  attributes
        call check( nf90_put_att(ncid, id_utmy, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_utmy, "MemoryOrder", "XYZ") )
        call check( nf90_put_att(ncid, id_utmy, "description", "UTM coordinate sotuth-north") )
        call check( nf90_put_att(ncid, id_utmy, "units", "km"))
        call check( nf90_put_att(ncid, id_utmy, "axis", "Y") )
      call check( nf90_def_var(ncid, "UTMz", NF90_INT,(/id_dim(3),id_dim(4)/),id_utmz ) )
      ! Assign  attributes
        call check( nf90_put_att(ncid, id_utmz, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_utmz, "MemoryOrder", "XYZ") )
        call check( nf90_put_att(ncid, id_utmz, "description", "UTM Zone") )
        call check( nf90_put_att(ncid, id_utmz, "units", "None"))
	do i=1,radm
		if(i.lt.ipm-1 ) then
			call crea_attr(ncid,4,dimids4,ename(i),cname(i),id_var(i))
		else
			call crea_attr2(ncid,4,dimids4,ename(i),cname(i),id_var(i))
		end if
	end do
!
!   Terminan definiciones
		call check( nf90_enddef(ncid) )
!  Coordenadas Mercator UTM
      call check( nf90_put_var(ncid, id_utmx,utmxd,start=(/1,1/)) )
      call check( nf90_put_var(ncid, id_utmy,utmyd,start=(/1,1/)) )
      call check( nf90_put_var(ncid, id_utmz,utmzd,start=(/1,1/)) )
!
!    Inicia loop de tiempo
tiempo: do it=iit,eit
		write(6,'(A,x,I3)')'TIEMPO: ', it
        gases: do ikk=1,ipm-2!for gases
			ea=0.0
		if(ikk.eq.1) then
          write(current_date(12:13),'(I2.2)') it
  	      Times(1,1)=current_date(1:19)
			  if (periodo.eq. 1) then
              call check( nf90_put_var(ncid, id_unlimit,Times,start=(/1,it+1/)) )
              call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1,it+1/)) )
              call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1,it+1/)) )
              call check( nf90_put_var(ncid, id_varpop,pob,  start=(/1,1,it+1/)) )
			  else
              call check( nf90_put_var(ncid, id_unlimit,Times,start=(/1,it-11/)) )
              call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1,it-11/)) )
              call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1,it-11/)) )
              call check( nf90_put_var(ncid, id_varpop,pob,start=(/1,1,it-11/)) )
			  endif
            end if   ! for kk == 1
          do i=1, nx
            do j=1, ny
              do l=1,zlev
                 ea(i,j,l,1)=eft(i,j,ikk,it+1,l) /(CDIM*CDIM)
              end do
            end do
          end do
            if(periodo.eq.1) then
                call check( nf90_put_var(ncid, id_var(isp(ikk)),ea,start=(/1,1,1,it+1/)) )
            else
                call check( nf90_put_var(ncid, id_var(isp(ikk)),ea,start=(/1,1,1,it-11/)) )        !******
            endif
		 end do gases
        aerosol: do ikk=ipm-1,ns ! from PM10
			ea=0.0
        do i=1, nx
          do j=1, ny
            do l=1,zlev
              ea(i,j,l,1)=eft(i,j,ikk,it+1,l) /(CDIM*CDIM) !entre 9x9 km
            end do
          end do
        end do
!
        if(periodo.eq.1) then
          call check( nf90_put_var(ncid, id_var(isp(ikk)),ea*0.8,start=(/1,1,1,it+1/)) )
          call check( nf90_put_var(ncid, id_var(isp(ikk+5)),ea*0.2,start=(/1,1,1,it+1/)) )
        else
          call check( nf90_put_var(ncid, id_var(isp(ikk)),ea*0.8,start=(/1,1,1,it-11/)) )        !******
          call check( nf90_put_var(ncid, id_var(isp(ikk+5)),ea*0.2,start=(/1,1,1,it-11/)) )        !******
        endif
		 end do aerosol
		end do tiempo
        call check( nf90_close(ncid) )
	 end do !periodo
    deallocate(ea)
    deallocate(idcg,lon,lat,pop,pob)
    deallocate(utmx,utmy,utmz)
    deallocate(xlon,xlat)
    deallocate(utmxd,utmyd,utmzd)
    deallocate(eft)
    deallocate(ename,cname)
    deallocate(isp)
    deallocate(wtm)
    deallocate(fnameA,fnameM,fnameP)
    deallocate(scala,scalm,scalp)

end subroutine store

subroutine check(status)
	integer, intent ( in) :: status
	if(status /= nf90_noerr) then 
		print *, trim(nf90_strerror(status))
		stop 2
	end if
end subroutine check  
!                                _   _
!    ___ _ __ ___  __ _     __ _| |_| |_ _ __
!  / __| '__/ _ \/ _` |   / _` | __| __| '__|
! | (__| | |  __/ (_| |  | (_| | |_| |_| |
! \___|_|  \___|\__,_|___\__,_|\__|\__|_|
!                    |_____|
	  subroutine crea_attr(ncid,idm,dimids,svar,cname,id_var)
	  use netcdf
      implicit none
	  integer , INTENT(IN) ::ncid,idm
	  integer, INTENT(out) :: id_var
	  integer, INTENT(IN),dimension(idm):: dimids
	  character(len=*), INTENT(IN)::svar,cname
	  character(len=50) :: cvar
		cvar="Emissions rate of "//trim(cname)
	  
	   call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
 ! Assign  attributes
        call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
        call check( nf90_put_att(ncid, id_var, "description", Cvar) )
        call check( nf90_put_att(ncid, id_var, "units", "mol km^-2 hr^-1"))
        call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
		call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
		! print *,"Entro a Attributos de variable",dimids,id,jd
	  return
	  end subroutine crea_attr
!                               _   _       ____
!   ___ _ __ ___  __ _     __ _| |_| |_ _ _|___ \
!  / __| '__/ _ \/ _` |   / _` | __| __| '__|__) |
! | (__| | |  __/ (_| |  | (_| | |_| |_| |  / __/
!  \___|_|  \___|\__,_|___\__,_|\__|\__|_| |_____|
!                    |_____|
	  subroutine crea_attr2(ncid,idm,dimids,svar,cname,id_var)
	  use netcdf
      implicit none
	  integer, INTENT(IN) ::ncid,idm
	  integer, INTENT(out) :: id_var
	  integer,INTENT(IN) ,dimension(idm):: dimids
	  character(len=*),INTENT(IN) ::svar,cname
	  character(len=50) :: cvar
		cvar="Emissions rate of "//trim(cname)
	   call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
 ! Assign  attributes
        call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
        call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
        call check( nf90_put_att(ncid, id_var, "description",cvar) )
        call check( nf90_put_att(ncid, id_var, "units", "ug m-2 s-1"))
        call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
		call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
		! print *,"Entro a Attributos de variable",dimids,id,jd
	  return
	  end subroutine crea_attr2
!
!  _ __ ___   ___  ___
! | '_ ` _ \ / _ \/ __|
! | | | | | |  __/\__ \
! |_| |_| |_|\___||___/
!
character(len=3)function mes(num)
          character*2 num
          select case (num)
            case('01')
              mes='Jan'
             case('02')
             mes='Feb'
             case('03')
             mes='Mar'
             case('04')
             mes='Apr'
             case('05')
             mes='May'
             case('06')
             mes='Jun'
             case('07')
             mes='Jul'
             case('08')
             mes='Aug'
             case('09')
             mes='Sep'
             case('10')
             mes='Oct'
             case('11')
             mes='Nov'
             case('12')
             mes='Dec'
             end select
          return
          end function
!
!    _       _ _
!   (_)_   _| (_) __ _ _ __   ___
!   | | | | | | |/ _` | '_ \ / _ \
!   | | |_| | | | (_| | | | | (_) |
!  _/ |\__,_|_|_|\__,_|_| |_|\___/
! |__/
!
integer function juliano(year,mes,day)
  implicit none
  character*4,intent(in) :: year
  character*2,intent(in) :: mes
  character*2,intent(in) :: day
  integer,dimension(12)::month=[31,28,31,30,31,30,31,31,30,31,30,31]
  integer i,iyear,imes,iday
  iyear=int2char(year)
  imes=int2char(mes)
  iday=int2char(day)
  if (mod(iyear,4)==0.and.mod(iyear,100)/=0) month(2)=29
  if (imes==1) then
    juliano=iday
    else
    juliano=0
    do i=1,imes-1
      juliano=juliano+month(i)
    end do
    juliano=juliano+iday
  end if
  return
end function

!  _       _   ____      _
! (_)_ __ | |_|___ \ ___| |__   __ _ _ __
! | | '_ \| __| __) / __| '_ \ / _` | '__|
! | | | | | |_ / __/ (__| | | | (_| | |
! |_|_| |_|\__|_____\___|_| |_|\__,_|_|
!
integer function int2char(char)
  implicit none
  character(len=*),intent(in):: char
  integer :: i,l
  l=len(char)
  int2char=0
  do i=1,l
    if(ichar(char(i:i)).lt.48 .or. ichar(char(i:i)).gt.57) then
      print *,"Character not a number function INT2CHAR() ",char
      stop
    end if
    int2char=(ichar(char(i:i))-48)*10**(l-i)+int2char
  end do
  return
end function
!  _                                          _ _     _
! | | ___  ___     _ __   __ _ _ __ ___   ___| (_)___| |_
! | |/ _ \/ _ \   | '_ \ / _` | '_ ` _ \ / _ \ | / __| __|
! | |  __/  __/   | | | | (_| | | | | | |  __/ | \__ \ |_
! |_|\___|\___|___|_| |_|\__,_|_| |_| |_|\___|_|_|___/\__|
!            |_____|
subroutine lee_namelist
    implicit none
    NAMELIST /region_nml/ zona
    NAMELIST /fecha_nml/ idia,month,anio
    NAMELIST /chem_nml/ mecha
    integer unit_nml
    logical existe
    unit_nml = 9
    existe = .FALSE.
    write(6,*)' >>>> Reading file - ../namelist_emis.nml'
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
        READ (unit_nml , NML = fecha_nml)
        READ (unit_nml , NML = chem_nml )
        !WRITE (6    , NML = chem_nml )
        close(unit_nml)
    else
        stop '***** No namelist_emis.nml in .. directory'
    end if
end subroutine lee_namelist
!   _                               _ _    _                  _
! | |___ ___   _ _  __ _ _ __  ___| (_)__| |_   _ __  ___ __| |_  __ _
! | / -_) -_) | ' \/ _` | '  \/ -_) | (_-<  _| | '  \/ -_) _| ' \/ _` |
! |_\___\___|_|_||_\__,_|_|_|_\___|_|_/__/\__|_|_|_|_\___\__|_||_\__,_|
!          |___|                            |___|
subroutine lee_namelist_mecha(mecanismo)
implicit none
    character(len=7),intent(in) :: mecanismo
    character(len=15) ::cfile
    NAMELIST /SCALE/ scala,scalm,scalp
    integer unit_nml
    logical existe

    unit_nml = 9
    existe = .FALSE.
    cfile='namelist.'//trim(mecanismo)
    write(6,*)' >>>> Reading file - ',cfile
    inquire ( FILE = cfile , EXIST = existe )

    if ( existe ) then
    !  Opening the file.
        open ( FILE   = cfile ,      &
        UNIT   =  unit_nml        ,      &
        STATUS = 'OLD'            ,      &
        FORM   = 'FORMATTED'      ,      &
        ACTION = 'READ'           ,      &
        ACCESS = 'SEQUENTIAL'     )
        !  Reading the file
        READ (unit_nml , NML = SCALE )
        !WRITE (6    , NML = SCALE )
        close(unit_nml)
    else
        print *, cfile
        stop '***** No namelist File exists   ******'
    end if
end subroutine lee_namelist_mecha

end program guarda_nc
