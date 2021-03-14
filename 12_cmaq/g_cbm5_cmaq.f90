!
!	g_cbm5_cmaq.f90
!	
!
!  Creado por Jose Agustin Garcia Reynoso el 12/07/17.
!
!
!  Proposito:
!            Guarda los datos del inventario para el
!            mecanismo CBM05 en formato netcdf y CMAQ
!
! ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include  g_cbm5_cmaq.f90 -o cbm5_cmaq.exe
!
!   Si son mas de 5 capas arreglar linea 219
!      eeft(i,j,ii,ih,9-levl)=eft(i,j,ii,ih,9-levl)+edum(ih)/WTM(ii)
!
!   Actualizacion de xlat, xlon             26/08/2012
!   Conversion de unidades en aerosoles     04/10/2012
!   Inclusion de NO2 en las emisiones       19/02/2014
!   Inclusion de poblacion en la salida     10/06/2014
!   Para año 2014, ns, ipm, icn             12/07/2017
!   Dos capas en puntuales                  18707/2017
!   Se incluyen NO y NO2 de moviles         01/11/2017
!   Se lee CDIM y titulo de localiza.csv    19/11/2017
!   Salidas preparadas para CMAQ            04/08/2018
!   Se emplea namelist.cbm05                04/08/2018
!   Se actualiza las fechas y datos         21/06/2019
!   Se incluye descripción de archivo        8/10/2019
!
module emissions_vars
    integer ::ncel   ! number of cell in the grid
    integer ::nl     ! number of lines in files
    integer :: nx,ny ! grid dimensions
    integer :: ncty  ! number of point stations
    integer*8 :: idcf  ! ID cell in file
    integer :: zlev       ! Layer of emission (1 to 8) 8 lower 1 upper
    integer,parameter :: nf=30    ! number of files antropogenic
    integer,parameter :: ns=28    ! number of compounds
    integer,parameter ::radm=ns   ! number of Mechanism classes
    integer,parameter :: nh=24    !  hours in a day
    integer,parameter :: ipm=23   ! Posicion del archivo PM2.5
    integer,parameter :: icn=30    ! Posicion archivo CN del INEM
    integer,parameter :: jcn=28    ! Posicion archivo CN de especiacion
    integer,parameter :: imt=29    ! Posicion archivo CH4 del INEM
    integer,parameter :: jmt=7     ! Posicion archivo CH4 de especiacion
    integer,allocatable :: idcg(:) ! ID cell in grid
    integer,allocatable:: utmz(:),utmzd(:,:)  ;!> day in emissions output file
    integer :: idia  ;!> month in emissions output file
    integer :: month ;!> year in emissions output file
    integer :: anio  ;!> model ID for output 0=WRF 1=CHIMERE 2=CMAQ
    integer :: model ;!> =1 uno 24 hr, =2 dos de 12hrs c/u
    integer ::periodo;
    real,allocatable:: eft(:,:,:,:,:)  ! emissions by nx,ny,file,nh,level
    real,allocatable :: lon(:),lat(:),pop(:)
    real,allocatable :: utmx(:),utmy(:)
    real,allocatable :: xlon(:,:),xlat(:,:),pob(:,:)
    real,allocatable :: utmxd(:,:),utmyd(:,:)
    real :: CDIM      ! celdimension in km
!> geogrphical area selected in namelist_emis.nml
    character(len=363) :: var_list
    character(len=12):: zona
    character(len=3) :: cday
    character(len=11),dimension(radm):: ename=(/&
        'CO         ','NH3        ','NO         ', 'NO2        ',&
        'SO2        ','ALD2       ','CH4        ','ALDX       ','ETH        ',&
        'ETHA       ','ETOH       ','IOLE       ','MEOH       ','FORM       ',&
        'ISOP       ','OLE        ','PAR        ','TERP       ','TOL        ',&
        'XYL        ','CO2        ','PM_10      ','PMFP       ','PSO4I      ',&
        'PNO3I      ','PM25I      ','APOCI      ','PECI       '/)
    character(len=16),dimension(radm):: &
     cname=(/'Carbon Monoxide ','Ammonia NH3     ','NO              ', &
        'NO2             ','SO2             ','Acetaldehyde    ','METHANE         ',&
        'C3+Aldehydes    ','Ethene          ','Ethane          ','Ethanol         ',&
        'Internal OLE    ','Methanol        ','Formaldehyde    ','Isoprene        ',&
        'TOB (R-C=C)     ','Paraffin (C-C)  ','Terpenes        ','Toluene+others  ',&
        'Xylene+others   ','Carbon Dioxide  ','PM 10um mode    ','PM 2.5um mode   ',&
        'Sulfates        ','Nitrates        ','PM2.5I          ','Organic Carbon  ',&
        'Elemental Carbon'/)
    character (len=19) :: current_date,current_datem,mecha
    character (len=40)  ::titulo
    common /domain/ ncel,nl,nx,ny,zlev,CDIM,zona
    common /date/ current_date,cday,mecha,cname,titulo
    common /nlm_vars/model,month,idia,anio,periodo
end module emissions_vars

program guarda_nc
use emissions_vars
use netcdf
    call lee_namelist

	call lee
		
	call store
contains
!  _                                          _ _     _
! | | ___  ___     _ __   __ _ _ __ ___   ___| (_)___| |_
! | |/ _ \/ _ \   | '_ \ / _` | '_ ` _ \ / _ \ | / __| __|
! | |  __/  __/   | | | | (_| | | | | | |  __/ | \__ \ |_
! |_|\___|\___|___|_| |_|\__,_|_| |_| |_|\___|_|_|___/\__|
!            |_____|
!>  @brief Reads global namelist input file
!>
!>   for setting up geographical, temporal and chemical mechamism settings
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_namelist
implicit none
  NAMELIST /region_nml/ zona
  NAMELIST /fecha_nml/ idia,month,anio,periodo
  NAMELIST /chem_nml/ mecha,model
  integer:: unit_nml=9
  logical existe
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
    close(unit_nml)
   ! WRITE (6    , NML = chem_nml )
    if (trim(mecha).ne."saprc07") model=0
  else
    stop '***** No namelist_emis.nml in .. directory'
  end if
end subroutine lee_namelist
subroutine lee
    IMPLICIT NONE
    integer :: ii,i,j,k,levl,levld,ih
    integer :: is     ! indice de compuesto
    integer :: iunit=14
    real ::rdum, constant
    real,dimension(nh)::edum
    real,dimension(ns)::wtm
    real,dimension(nf)::scala,scalm,scalp
    character(len=46) :: description
    character(len=39) flocaliza,cdum
    character (len=15)::ruta
    character(len=17),dimension(nf):: fnameA,fnameM,fnameP
    data fnameA /&
    & 'TACO__2016.csv'  ,'TANH3_2016.csv' ,'TANOx_2016.csv'  ,'TANOx_2016.csv','TASO2_2016.csv',&
	& 'CBM05_ALD2_A.txt','CBM05_CH4_A.txt','CBM05_ALDX_A.txt','CBM05_ETH_A.txt',&
	& 'CBM05_ETHA_A.txt','CBM05_ETOH_A.txt','CBM05_IOLE_A.txt','CBM05_MEOH_A.txt',&
	& 'CBM05_FORM_A.txt','CBM05_ISOP_A.txt','CBM05_OLE_A.txt','CBM05_PAR_A.txt',&
	& 'CBM05_TERP_A.txt','CBM05_TOL_A.txt', 'CBM05_XYL_A.txt',&
	& 'TACO2_2016.csv' ,'TAPM102016.csv','TAPM2_2016.csv',&
	& 'GSO4_A.txt','PNO3_A.txt','OTHE_A.txt','POA_A.txt','PEC_A.txt',&
    & 'TACH4_2016.csv','TACN__2016.csv'/
	data fnameM /'TMCO__2016.csv','TMNH3_2016.csv','TMNO__2016.csv','TMNO2_2016.csv','TMSO2_2016.csv',&
    & 'CBM05_ALD2_M.txt','CBM05_CH4_M.txt','CBM05_ALDX_M.txt' ,'CBM05_ETH_M.txt',&
    & 'CBM05_ETHA_M.txt','CBM05_ETOH_M.txt','CBM05_IOLE_M.txt','CBM05_MEOH_M.txt',&
    & 'CBM05_FORM_M.txt','CBM05_ISOP_M.txt','CBM05_OLE_M.txt','CBM05_PAR_M.txt',&
    & 'CBM05_TERP_M.txt','CBM05_TOL_M.txt','CBM05_XYL_M.txt',&
    & 'TMCO2_2016.csv','TMPM102016.csv','TMPM2_2016.csv',&
	& 'GSO4_M.txt','PNO3_M.txt','OTHE_M.txt','POA_M.txt','PEC_M.txt',&
    & 'TMCH4_2016.csv','TMCN__2016.csv'/
    data fnameP /'T_ANNCO.csv','T_ANNNH3.csv','T_ANNNOX.csv','T_ANNNOX.csv','T_ANNSO2.csv',&
    & 'CBM05_ALD2_P.txt','CBM05_CH4_P.txt','CBM05_ALDX_P.txt' ,'CBM05_ETH_P.txt',&
    & 'CBM05_ETHA_P.txt','CBM05_ETOH_P.txt','CBM05_IOLE_P.txt','CBM05_MEOH_P.txt',&
    & 'CBM05_FORM_P.txt','CBM05_ISOP_P.txt','CBM05_OLE_P.txt','CBM05_PAR_P.txt',&
    & 'CBM05_TERP_P.txt','CBM05_TOL_P.txt','CBM05_XYL_P.txt',&
    & 'T_ANNCO2.csv','T_ANNPM10.csv','T_ANNPM25.csv', &
	& 'GSO4_P.txt','PNO3_P.txt','OTHE_P.txt','POA_P.txt','PEC_P.txt',&
    & 'T_ANNCH4.csv','T_ANNCN.csv'/
  NAMELIST /SCALE/ scala,scalm,scalp
  integer unit_nml
  logical existe
! Mole weight
       DATA WTM /28., 17., 30., 46., 64.,32.,  16., 32., 32.,32.,32.,&   !
	   &         64., 16., 16., 80., 32.,16., 160.,112.,128., 44.,&
	   &      1.,1.,1.,1.,1.,1.,1./ ! MW 1 for unit conversion to g/h

!    SCALA      CO   NH3  NO   NO2  SO2 ALD2   CH4  ALDx  ETH ETHA ETOH
!             IOLE  MEOH HCHO ISOP  OLE  PAR   TERP  TOL  XYL  CO2
!             PM10 PM2.5 PSO4 PNO3 OTHER POA   PEC  CH4   CN

    unit_nml = 9
    existe = .FALSE.
    write(6,*)' >>>> Reading file - namelist.cbm05'
    inquire ( FILE = 'namelist.cbm05' , EXIST = existe )

    if ( existe ) then
      !  Opening the file.
      open ( FILE   = 'namelist.cbm05' ,      &
      UNIT   =  unit_nml        ,      &
      STATUS = 'OLD'            ,      &
      FORM   = 'FORMATTED'      ,      &
      ACTION = 'READ'           ,      &
      ACCESS = 'SEQUENTIAL'     )
      !  Reading the file
      READ (unit_nml , NML = SCALE )
      !WRITE (6    , NML = SCALE )
    else
      stop '***** No namelist.cbm05'
    ENDIF
       mecha="CMB05"
    flocaliza='../01_datos/'//trim(zona)//'/'//'localiza.csv'
    write(6,*)' >>>> Reading file -',flocaliza,' ---------'
    write(var_list,'(33A11)') ename
	open (unit=10,file=flocaliza,status='old',action='read')
	read (10,*) cdum  !Header
	read (10,*) nx,ny,titulo  !Header
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
!
        is= ii
        if(ii.eq.icn) is=jcn  ! suma todo el Carbono Negro
        if(ii.eq.imt) is=jmt   ! suma todo el Metano
    constant=scala(ii)/WTM(is)
    write(6,'(i4,x,A,A,2ES11.1)') ii,ruta//fnameA(ii),current_date(1:13),constant
		do
		 if(ii.eq.ipm) then !for PM2.5
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
    if(ii.ge.ipm-1) then; is=ipm ;else ;is=ii;end if
    if(ii.eq.imt) is=jmt
    write(6,'(i4,x,A,A,f7.1)') ii,ruta//fnameM(ii),current_date(1:13)
    constant=scalm(ii)/WTM(is)
       do
		 if(ii.eq.ipm) then !for PM2.5
		 read(11,*,END=200) idcf,rdum,(edum(ih),ih=1,nh)
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
if(ii.ge.ipm-1) then; is=ipm ;else ;is=ii;end if
if(ii.eq.imt) is=jmt
write(6,'(i4,x,A,A,f7.1)') ii,ruta//fnameP(ii),current_date(1:13)
constant=scalp(ii)/WTM(is)
rdum=1./WTM(is)
       do
		 if(ii.eq.ipm) then   !for PM2.5
		   read(11,*,END=300) idcf,rdum,levl,(edum(ih),ih=1,nh),levld
           !print *,idcf,rdum,levl,(edum(ih),ih=1,nh)
		 else
		   read(11,*,END=300) idcf,levl,(edum(ih),ih=1,nh),levld
		 end if
    zlev =max(zlev,levl,levld)
    if(levl.gt.zlev .or.levld.gt.zlev) Stop "*** Change dimension line  allocate(eft.."
		 k=0
		 busca3: do j=1,ny
		  do i=1,nx
			k=k+1
			if(idcg(k).eq.idcf) then
			  do ih=1,nh
                ! Emission from g to gmol by 1/WTM
                if(ih.gt.9 .and. ih.lt.19) then
                  if(levl.lt.2) then
                    eft(i,j,is,ih,levl)=eft(i,j,is,ih,levl)+edum(ih)*constant
                  else
                    eft(i,j,is,ih,levl)=eft(i,j,is,ih,levl)+edum(ih)*rdum
                  end if
                else
                  if(levld.lt.2) then
                    eft(i,j,is,ih,levld)=eft(i,j,is,ih,levld)+edum(ih)*constant
                  else
                    eft(i,j,is,ih,levld)=eft(i,j,is,ih,levld)+edum(ih)*rdum
                  end if
                end if
			  end do
			  exit busca3
			end if
		 end do
		end do busca3
		end do
 300 close(11)
!	end if
	end do! ii nf
	
	return

end subroutine lee

subroutine calculos
	IMPLICIT NONE

end subroutine calculos

subroutine store
    IMPLICIT NONE
    integer :: NDIMS
    parameter (NDIMS=6)
    integer :: i,j,k,l
    integer :: ncid
    integer :: periodo,iit,eit,it
    integer :: ikk
    integer :: dimids2(2),dimids3(3),dimids4(4)
    integer,dimension(radm+1):: id_var
    integer :: id_varlong,id_varlat,id_varpop
    integer :: id_utmx,id_utmy,id_utmz
    integer :: id,iu
    integer :: julday
    integer :: isp(radm)
    integer :: icdate,ictime,jcdate,jctime
    integer,dimension(NDIMS):: dim,id_dim
    integer,dimension(2,1,1)::TFLAG
    real,ALLOCATABLE :: ea(:,:,:,:)
    character (len=19),dimension(NDIMS) ::sdim
    character(len=39):: FILE_NAME
    character(len=19),dimension(1,1)::Times
    character(len=19):: iTime
    character(8)  :: date
    character(10) :: time
    character(24) :: hoy
  	   DATA isp / 1, 2, 3, 4, 5, 6, 7, 8, 9,10, &
                 11,12,13,14,15, 16,17,18,19,20, &
                 21,22,23,24,25, 26,27,28/

    data sdim /"TSTEP              ","DATE-TIME          ","COL                ",&
    &          "ROW                ","VAR                ","LAY                "/

	 print *,"Guarda Archivo"	 
! ******************************************************************
    call date_and_time(date,time)
     hoy=date(7:8)//'-'//mes(date(5:6))//'-'//date(1:4)//' '//time(1:2)//':'//time(3:4)//':'//time(5:10)
    icdate=intc(date(1:4))*1000 + juliano(date(1:4),date(5:6),date(7:8))
    ictime=intc(time(1:2))*10000+intc(time(3:4))*100+intc(time(5:6))
    print *,hoy,icdate,ictime
    !write(current_date(4:4),'(A1)')char(8+48)
    julday=juliano(current_date(1:4),current_date(6:7),current_date(9:10))
     do periodo=1,1! 1 o 2
	  if(periodo.eq.1) then
        FILE_NAME='cmaq.d01.'//trim(mecha)//'.'//current_date(1:19)         !******
	   iit= 0
	   eit= 23 !11
	   iTime=current_date
	  else if(periodo.eq.2) then
	   iit=12
	   eit=23
       write(iTime(12:13),'(I2)') iit
        FILE_NAME='cmaq.d01.'//trim(mecha)//'.'//iTime
	 end if
	  ! Open NETCDF emissions file	
       call check( nf90_create(path=FILE_NAME,cmode = or(nf90_clobber,nf90_64bit_offset),ncid=ncid) )
!     Define dimensiones
		  dim(1)=1
		  dim(2)=2
		  dim(3)=nx
		  dim(4)=ny
		  dim(5)=radm!mkx
		  dim(6)=zlev! !8
         if(.not.ALLOCATED(ea)) allocate (ea(dim(3),dim(4),dim(6),dim(1)))
		 call check( nf90_def_dim(ncid,sdim(1), NF90_UNLIMITED, id_dim(1)) )
       do i=2,NDIMS
         call check( nf90_def_dim(ncid, sdim(i), dim(i), id_dim(i)) )
       end do

      dimids2 = (/id_dim(2),id_dim(1)/)
      dimids3 = (/id_dim(2),id_dim(5),id_dim(1) /)
      dimids4 = (/id_dim(3),id_dim(4),id_dim(6),id_dim(1)/)
      print *,"Attributos Globales NF90_GLOBAL"
      !Attributos Globales NF90_GLOBAL
      call check( nf90_put_att(ncid, NF90_GLOBAL, "TITLE",titulo))
call check( nf90_put_att(ncid, NF90_GLOBAL, "IOAPI_VERSION","$Id: @(#) ioapi library version 3.0 OpenMP enabled $"))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "EXEC_ID","????????????????"))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "FTYPE ",1))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "CDATE ",intc(current_date(1:4))*1000+julday))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "CTIME ",0))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "WDATE ",intc(current_date(1:4))*1000+julday))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "WTIME ",0))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "SDATE ",intc(current_date(1:4))*1000+julday))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "STIME ",0))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "TSTEP ",10000))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "NTHIK ",1))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "NCOLS ",nx))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "NROWS ",ny))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "NLAYS ",zlev))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "NVARS",radm))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "GDTYP ",2))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "P_ALP ",17.5))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "P_BET ",29.5))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "P_GAM ",xlon(nx/2,ny/2)))! -100.5
      call check( nf90_put_att(ncid, NF90_GLOBAL, "XCENT ",xlon(nx/2,ny/2)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "YCENT ",xlat(nx/2,ny/2)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "XORIG ",(-nx/2*CDIM*1000)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "YORIG ",(-ny/2*CDIM*1000)))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "XCELL ",(CDIM*1000)))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "YCELL ",(CDIM*1000)))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "VGTYP ",-1))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "VGTOP ",0))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "VGLVLS ","0,0"))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "GDNAM ","MEXICO_9"))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "UPNAM ","CREATESET"))!
      call check( nf90_put_att(ncid, NF90_GLOBAL, "VAR-LIST ",var_list))
call check( nf90_put_att(ncid, NF90_GLOBAL, "FILEDESC","Area source emissions data /FROM/ CCAUNAM /VERSION/ "&
//"CIEM1.0_ /BASE YEAR/ 2016 /NUMBER OF FILES/   1/FILE POSITION/   1/NUMBER OF VARIABLES/  28"))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "HISTORY",""))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "MECHANISM",mecha))
      call check( nf90_put_att(ncid, NF90_GLOBAL, "CREATION_DATE",hoy))

	print *,"Define las variables"
!  Define las variables
	  call check( nf90_def_var(ncid, "TFLAG", NF90_INT, dimids3,id_var(radm+1)))
!      Assign  attributes
      call check( nf90_put_att(ncid, id_var(radm+1), "units", "<YYYYDDD,HHMMSS>"  ) )
      call check( nf90_put_att(ncid, id_var(radm+1), "long_name", "FLAG           "  ) )
      call check( nf90_put_att(ncid, id_var(radm+1), "var_desc", "Timestep-valid flags: "&
//"(1) YYYYDDD or (2) HHMMSS                                "  ) )
!  Attributos para cada variable
print *," Pob"
    call check( nf90_def_var(ncid,"POB",NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/) ,id_varpop ) )
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

!    Inicia loop de tiempo

tiempo: do it=iit,eit
		write(6,'(A,x,I3)')'TIEMPO: ', it
        gases: do ikk=1,ipm-2!for gases
			ea=0.0
		if(ikk.eq.1) then
		      if (it.lt.10) then
			  write(current_date(13:13),'(A1)')char(it+48)
			    else
		        id = int((it)/10)+48 !  Decenas
                iu = it-10*int((it)/10)+48 ! unidades
			  write(current_date(12:13),'(A1,A1)')char(id),char(iu)
			  end if 
      end if   ! for kk == 1
        TFLAG(1,1,1)=intc(current_date(1:4))*1000+julday
        TFLAG(2,1,1)=it*10000 !HHMMSS
			  if (periodo.eq. 1) then
            call check( nf90_put_var(ncid,id_var(radm+1),TFLAG,start=(/1,ikk,it+1/)) )
!            call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1,it+1/)) )
!            call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1,it+1/)) )
            call check( nf90_put_var(ncid, id_varpop,pob,  start=(/1,1,it+1/)) )
			  else
            call check( nf90_put_var(ncid,id_var(radm+1),TFLAG,start=(/1,ikk,it-11/)) )
 !           call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1,it-11/)) )
 !           call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1,it-11/)) )
            call check( nf90_put_var(ncid, id_varpop,pob,start=(/1,1,it-11/)) )
			  endif
            do i=1, nx
              do j=1, ny
                do l=1,zlev
                   ea(i,j,l,1)=eft(i,j,ikk,it+1,l)! /(CDIM*CDIM)
                end do
              end do
            end do
            if(periodo.eq.1) then
                call check( nf90_put_var(ncid, id_var(isp(ikk)),ea,start=(/1,1,1,it+1/)) )
            else
                call check( nf90_put_var(ncid, id_var(isp(ikk)),ea,start=(/1,1,1,it-11/)) )        !******
            endif
		 end do gases
        aerosol: do ikk=ipm-1,ns
			ea=0.0
            do i=1, nx
              do j=1, ny
               do l=1,zlev
                  ea(i,j,l,1)=eft(i,j,ikk,it+1,l)! /(CDIM*CDIM) !entre 3x3 km
                end do
              end do
            end do
!
            if(periodo.eq.1) then
                call check( nf90_put_var(ncid,id_var(radm+1),TFLAG,start=(/1,ikk,it+1/)) )
                call check( nf90_put_var(ncid, id_var(isp(ikk)),ea,start=(/1,1,1,it+1/)) )
            else
                call check( nf90_put_var(ncid,id_var(radm+1),TFLAG,start=(/1,ikk,it-11/)) )
                call check( nf90_put_var(ncid, id_var(isp(ikk)),ea,start=(/1,1,1,it-11/)) )        !******
            endif
		 end do aerosol
		end do tiempo
        call check( nf90_close(ncid) )
	 end do !periodo
    deallocate(ea)

end subroutine store

subroutine check(status)
	integer, intent ( in) :: status
	if(status /= nf90_noerr) then 
		print *, trim(nf90_strerror(status))
		stop 2
	end if
end subroutine check  
!   CCC RRRR  EEEEE   A        A   TTTTT TTTTT RRRR
!  C    R  RR E      A A      A A    T     T   R  RR
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR
!  C    R  R  E     A   A    A   A   T     T   R  R
!   CCC R   R EEEEE A   A____A   A   T     T   R   R
	  subroutine crea_attr(ncid,idm,dimids,svar,cname,id_var)
	  use netcdf
      implicit none
	  integer , INTENT(IN) ::ncid,idm
	  integer, INTENT(out) :: id_var
	  integer, INTENT(IN),dimension(idm):: dimids
	  character(len=*), INTENT(IN)::svar,cname
	  character(len=50) :: cvar
		cvar="Model species "//trim(cname)
	  
	   call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
! Assign  attributes
      call check( nf90_put_att(ncid, id_var, "long_name", cname) )
      call check( nf90_put_att(ncid, id_var, "units", "mol h^-1"))
      call check( nf90_put_att(ncid, id_var, "var_desc", cvar) )
      ! print *,"Entro a Attributos de variable",dimids,id,jd
	  return
	  end subroutine crea_attr
!   CCC RRRR  EEEEE   A        A   TTTTT TTTTT RRRR   222
!  C    R  RR E      A A      A A    T     T   R  RR 2   2
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR     2
!  C    R  R  E     A   A    A   A   T     T   R  R   2
!   CCC R   R EEEEE A   A____A   A   T     T   R   R 22222
	  subroutine crea_attr2(ncid,idm,dimids,svar,cname,id_var)
	  use netcdf
      implicit none
	  integer, INTENT(IN) ::ncid,idm
	  integer, INTENT(out) :: id_var
	  integer,INTENT(IN) ,dimension(idm):: dimids
	  character(len=*),INTENT(IN) ::svar,cname
	  character(len=50) :: cvar
		cvar="Model species "//trim(cname)
	   call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
 ! Assign  attributes
      call check( nf90_put_att(ncid, id_var, "long_name", cname) )
      call check( nf90_put_att(ncid, id_var, "units", "g h^-1"))
      call check( nf90_put_att(ncid, id_var, "var_desc", cvar) )
		! print *,"Entro a Attributos de variable",dimids,id,jd
	  return
	  end subroutine crea_attr2
!
! M      M EEEEE  SSSS
! M M  M M E     S
! M  M   M EEE    SSS
! M      M E         S
! M      M EEEEE SSSS

character(len=3) function mes(num)
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
  integer function juliano(year,mes,day)
    character*4,intent(in) :: year
    character*2,intent(in) :: mes
    character*2,intent(in) :: day
    integer,dimension(12)::month=[31,28,31,30,31,30,31,31,30,31,30,31]
    integer i
    iyear=intc(year)
    imes=intc(mes)
    iday=intc(day)
    print *,iyear,imes,iday
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

! i  n         t     ccccc
!    nnnnn   ttttt  c
! i  n    n    t    c
! i  n    n    t    c
! i  n    n    t     ccccc
integer function intc(char)
  character(len=*),intent(in):: char
  integer :: i,l
  l=len(char)
  intc=0
  do i=1,l
    if(ichar(char(i:i)).lt.48 .or. ichar(char(i:i)).gt.57) then
      print *,"Character not a number function INTC() ",char
      stop
    end if
    intc=(ichar(char(i:i))-48)*10**(l-i)+intc
  end do
  return
end function
end program guarda_nc
