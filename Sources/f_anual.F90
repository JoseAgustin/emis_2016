!  Creado por Jose Agustin Garcia Reynoso el 26/06/23
! Proposito
!          Almacenamiento emisioens anuales de fuentes puntuales
!
! ifort -O2 -axAVX2 f_anual.F90 -o Panual.exe
!
!>  @brief for f_anual.F90 program. Emissions storage in grid annually.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module point_vars_mod
!>   number of pollutants
integer, parameter::nsp=10 !number of pollutants
!>   CAMS categories
integer, parameter:: nh=9 ;!> CAMS categories number
integer,parameter :: ncams=9  ; !> layers where emissions are reach day and night
integer,allocatable :: capa(:,:); !> **i** index in grid to allocate a point emission
integer,allocatable :: ict(:) ; !> **j** index in grid to allocate a point emission
integer,allocatable :: jct(:) ;!> _GRIDCODE_ in grid domain
integer*8,allocatable :: idcg(:,:);!> longitudes in output file from localiza
real,allocatable :: xlon(:,:)  ;!> latitudes in output file from localiza
real,allocatable :: xlat(:,:) ; !> population in output file from localiza
real,allocatable :: pob(:,:) ;!> UTMx coordinates in output file from localiza
real,allocatable :: utmxd(:,:) ;!> UTMy coordinates in output file from localiza
real,allocatable :: utmyd(:,:) ;!> temporal array for storage
real,allocatable:: aguardar(:,:) ;!> UTM Z zone
integer, allocatable:: utmzd(:,:) ;!> cell dimension CDIM km
real :: CDIM ;!> grid 1/area  (km^-2)
real :: SUPF1 ;!> Number of lines in input file
integer :: nl ;!> Number of longitudes (columns) in localiza file
integer :: nx ;!> Number of latitudes (rows( in localiza file
integer :: ny ;;!> Time zone integer
integer,allocatable :: mcst(:,:)
!> Latitude point source
real,allocatable :: lat(:) ;!> Longitude point source
real,allocatable :: lon(:)
real,allocatable :: pf(:,:) ;!> point source emission input
real,allocatable :: e_mis(:,:) ;!> point source emission for each pollutant
real,allocatable :: emis(:,:,:);!> output file name
character(len=27),dimension(nsp)  :: casn  ;!> Categories in CAMS
character(len=3),dimension(ncams) :: idCAMS;!> IPCC classification
character(len=4),dimension(ncams) :: idIPCC;!> Pollutant abreviation
character(len=4),dimension(nsp)   :: ename ;!> Long name description
character(len=76),dimension(ncams):: cname ;!> Variable description
character(len=66),dimension(nsp)  :: long_nm

!> Source clasification code array
character(len=10),allocatable::iscc(:)
!> Pollutant name
character (len=7) :: cvar(nsp)
!> Initial date of the emissions period
character (len=19) :: current_date
data casn /'MEX_POINT_9km_2016_PM10.nc','MEX_POINT_9km_2016_PM25.nc',&
'MEX_POINT_9km_2016_SO2.nc ','MEX_POINT_9km_2016_CO.nc  ',&
'MEX_POINT_9km_2016_NOx.nc ','MEX_POINT_9km_2016_VOC.nc ',&
'MEX_POINT_9km_2016_CO2.nc ','MEX_POINT_9km_2016_CH4.nc ',&
'MEX_POINT_9km_2016_NH3.nc ','MEX_POINT_9km_2016_CN.nc  '/
data idCAMS/'AGL','AGS','ENE','IND','RES','SHP','SWD','TNR','TRO'/
data idIPCC/'  3A','  3C',' 1A2',' 1A2',' 1A4','1A3d','   4',' 1A3','1A3b'/
data ename/ 'PM10','PM25','SO2 ','CO  ','NOx ',&
            'VOC ','CO2 ','CH4 ','NH3 ','BC  '/
data cname / &
'AGL: Agriculture_livestock                                                  ',&
'AGS: Agricultural_soils (without fires)                                     ',&
'ENE: Power_generation                                                       ',&
'IND: Industrial_process (Energy consumption of manufacture industry+process)',&
'RES: Residential_commercial_and_other_combustion                            ',&
'SHP: Navigation                                                             ',&
'SWD: Solid_waste_and_waste_water                                            ',&
'TNR: Non-road_transportation                                                ',&
'TRO: Road_transportation                                                    '/
data long_nm/&
'surface_upward_mass_flux_of_pm10_ambient_aerosol_particles_in_air ',&
'surface_upward_mass_flux_of_pm2p5_ambient_aerosol_particles_in_air',&
'surface_upward_mass_flux_of_sulfur_dioxide                        ',&
'surface_upward_mass_flux_of_carbon_monoxide                       ',&
'surface_upward_mass_flux_of_nitrogen_oxides                       ',&
'surface_upward_mass_flux_of_nmvoc                                 ',&
'surface_upward_mass_flux_of_carbon_dioxide                        ',&
'surface_upward_mass_flux_of_methane                               ',&
'surface_upward_mass_flux_of_ammonia                               ',&
'surface_upward_mass_flux_of_black_carbon                          '/
common /dat/ nl,nx,ny,daytype,fweek,cvar,current_date

end module point_vars_mod
!              _       _      _                                       _
!  _ __   ___ (_)_ __ | |_   | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |
! | '_ \ / _ \| | '_ \| __|  | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |
! | |_) | (_) | | | | | |_   | ||  __/ | | | | | |_) | (_) | | | (_| | |
! | .__/ \___/|_|_| |_|\__|___\__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|
! |_|                    |_____|               |_|
!
!>  @brief Temporal distribution of point sources emissions.
!>
!>  using EPA temporal profiles based on SCC
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program point_temporal
#ifdef _OPENMP
    use omp_lib
#endif
use point_vars_mod
use master

    call lee_namelist

    call point_emis_reading

    call point_cams_distribution

    call point_emissions_storage

contains
!              _       _
!  _ __   ___ (_)_ __ | |_
! | '_ \ / _ \| | '_ \| __|
! | |_) | (_) | | | | | |_
! | .__/ \___/|_|_| |_|\__|
! |_|             _                            _ _
!   ___ _ __ ___ (_)___     _ __ ___  __ _  __| (_)_ __   __ _
!  / _ \ '_ ` _ \| / __|   | '__/ _ \/ _` |/ _` | | '_ \ / _` |
! |  __/ | | | | | \__ \   | | |  __/ (_| | (_| | | | | | (_| |
!  \___|_| |_| |_|_|___/___|_|  \___|\__,_|\__,_|_|_| |_|\__, |
!                    |_____|                            |___/
!>  @brief Reads emissions file and temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine point_emis_reading
implicit none
	integer :: i,j,k,l,m,iun
	integer :: idum, imon,iwk,ipdy
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	real,allocatable ::xlat(:,:),xlon(:,:)
	real rdum
	logical fil1,fil2
  character(len=10)::jscc
	character(len=40)::cdum,canio
	character(len=18):: nfile,nfilep

   write(current_date,'(I4,"-",I2.2,"-",I2.2,A9)')anio,month,idia,'_00:00:00'
  fweek= 7./daym(month)  !semanas en el mes

!$omp section
	open(newunit=iun,file="../emis/punt/"//'puntual.csv',status='old',action='read')
	i=0
	read (iun,*) cdum
	do
		read(iun,*,end=100) cdum
		i=i+1
	end do
100 continue
    nl=i
!$omp end parallel sections
    print *,'**  Numero de lineas ',nl
    allocate(iscc(nl),capa(nl,2),lat(nl),lon(nl),e_mis(nl,nsp))
    allocate(emis(i,nsp,nh))
    allocate(ict(nl),jct(nl))
    e_mis=0

    rewind(iun)
    read (iun,*) cdum,cdum,cdum,(cvar(i),i=1,nsp)
    do i=1,nl
      read(iun,*,err=110)lat(i),lon(i),iscc(i),(e_mis(i,j),j=1,nsp),capa(i,1),capa(i,2)
    end do
    close(iun)
    e_mis=e_mis*1000000 !para g desde (Mg) TON
    print *,'Done puntual.csv '!,cvar,maxval(e_mis)
!
!	localiza.csv
!
   cdum="../"//trim(zona)//"/"//"localiza.csv"
	write(6,*)' >>>> Reading file - ',cdum,' ----'
	open (newunit=iun,file=cdum,status='old',action='read')
	read (iun,*) cdum  !Header
	read (iun,*) nx,ny  !Header
	allocate(idcg(nx,ny),xlon(nx,ny),xlat(nx,ny),mcst(nx,ny))
	do j=1,ny
		do i=1,nx
			read(iun,*) idcg(i,j),xlon(i,j),xlat(i,j),mcst(i,j)
		end do
	end do
	!print *,ncel
    close(iun)
#ifdef _OPENMP
!$omp parallel sections num_threads (3)
!$omp section
  print *,'   >>>>>  Finding emissions in grid'
    call localiza(xlat,xlon,nx,ny,lat,lon,ict,jct,1,nl/3)   ! Point Sources
!$omp section
    print *,'   >>>>>  Finding emissions in grid 2'
    call localiza(xlat,xlon,nx,ny,lat,lon,ict,jct,nl/3+1,2*nl/3)   ! Point Sources
!$omp section
  print *,'   >>>>>  Finding emissions in grid 3'
  call localiza(xlat,xlon,nx,ny,lat,lon,ict,jct,2*nl/3+1,nl)   ! Point Sources
!$omp end parallel sections
#else
    print *,'   >>>>>  Finding emissions in grid'
    call localiza(xlat,xlon,nx,ny,lat,lon,ict,jct,1,nl)   ! Point Sources
#endif
    print *,'   Done '
	return
110	print *,'Error en ',i
    STOP
end subroutine point_emis_reading
!              _       _
!  _ __   ___ (_)_ __ | |_     ___ __ _ _ __ ___  ___
! | '_ \ / _ \| | '_ \| __|   / __/ _` | '_ ` _ \/ __|
! | |_) | (_) | | | | | |_   | (_| (_| | | | | | \__ \
! | .__/ \___/|_|_| |_|\__|___\___\__,_|_| |_| |_|___/
! |_|      _ _     _     |_____|          _   _
!       __| (_)___| |_ _ __(_) |__  _   _| |_(_) ___  _ __
!      / _` | / __| __| '__| | '_ \| | | | __| |/ _ \| '_ \
!     | (_| | \__ \ |_| |  | | |_) | |_| | |_| | (_) | | | |
! ____\__,_|_|___/\__|_|  |_|_.__/ \__,_|\__|_|\___/|_| |_|
!|_____|
!>  @brief Distributes point annual emissions from SCC to CAMS categories
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine point_cams_distribution
	implicit none
	integer i,j,kk,l,ival,ii
!
	print *,'CAMS distribution'
!$omp parallel do
  do i=1,nl
    if(ict(i).ne.0 .or.jct(i).ne.0) then
      do kk=1,nsp
      !print *,'k=',kk
        do l=1,nh

        end do
      end do
    end if
  end do
!$omp end parallel do
end subroutine point_cams_distribution
!               _       _                     _         _
!  _ __   ___ (_)_ __ | |_     ___ _ __ ___ (_)___ ___(_) ___  _ __  ___
! | '_ \ / _ \| | '_ \| __|   / _ \ '_ ` _ \| / __/ __| |/ _ \| '_ \/ __|
! | |_) | (_) | | | | | |_   |  __/ | | | | | \__ \__ \ | (_) | | | \__ \
! | .__/ \___/|_|_| |_|\__|___\___|_| |_| |_|_|___/___/_|\___/|_| |_|___/
! | |     ___| |_ ___  _ __|_____|__ _  ___
!       / __| __/ _ \| '__/ _` |/ _` |/ _ \
!       \__ \ || (_) | | | (_| | (_| |  __/
!   ____|___/\__\___/|_|  \__,_|\__, |\___|
!  |_____|                      |___/
!>  @brief Stores houry emission by pollutant
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine point_emissions_storage
	implicit none
	integer:: i,k,l,iun
	character(len=14) ::fname
	character(len=3):: cdia(7)
	data cdia/'MON','TUE','WND','THR','FRD','SAT','SUN'/
   Write(6,*)"Guarda"
!$omp parallel do private(iun,i,k,l,fname)
	do k=1,nsp
		fname='T_'//trim(cvar(k))//'.csv'
		open(newunit=iun,file=fname,action='write')
		if(k.ne.nsp .and. k.ne.4) then
		write(iun,*)cvar(k),'Lat,Lon,Capa 1, H1,H2,H3,H4,H5,H6,H7,H8,H9,to Hr24, Capa2'
		else
		write(iun,*)cvar(k),'SCC,Lat,Lon,Capa 1, H1,H2,H3,H4,H5,H6,H7,H8,H9,to Hr24,Capa 2'
		end if
		write(iun,'(I6,4A)') nl,', ',current_date,', ',cdia(daytype)
			do i=1,nl
			if(ict(i).ne.0 .or.jct(i).ne.0)then
             !write(iun,300)iscc(i),lat(i),lon(i),capa(i),(emis(i,k,l),l=1,nh)
            write(iun,310)iscc(i),idcg(ict(i),jct(i)),capa(i,1),(emis(i,k,l),l=1,nh),capa(i,2)
			end if
			end do
		close(unit=iun)
	end do
!$omp end parallel do
     print *,"****** DONE PUNTUAL *****"
    deallocate(iscc,capa,lat,lon,e_mis)
    deallocate(emis)
    deallocate(ict,jct)
    deallocate(idcg,mcst)

#ifndef PGI
210 format(I8,',',I3,',',23(ES,","),ES,",",I3)
220 format(f10.6,',',f10.4,',',I3,',',23(ES,","),ES)
300 format(A10,',',f10.6,',',f10.4,',',I3,',',23(ES,","),ES)
310 format(A10,',',I8,',',I3,',',23(ES,","),ES,",",I3)
#else
210 format(I8,',',I3,',',23(E12.4,","),E12.4,",",I3)
220 format(f10.6,',',f10.4,',',I3,',',23(E12.4,","),E12.4)
300 format(A10,',',f10.6,',',f10.4,',',I3,',',23(E12.4,","),E12.4)
310 format(A10,',',I8,',',I3,',',23(E12.4,","),E12.4,",",I3)
#endif
end subroutine point_emissions_storage
!  _                 _ _
! | | ___   ___ __ _| (_)______ _
! | |/ _ \ / __/ _` | | |_  / _` |
! | | (_) | (_| (_| | | |/ / (_| |
! |_|\___/ \___\__,_|_|_/___\__,_|
!
!>  @brief Identifies the i,j index in the grid for the spatial
!>   allocation of point source emissions
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>  @param xlat two dimensional array with latitudes
!>  @param xlon two dimensional array with longitudes
!>  @param mi x dimension in arrays xlat and xlon
!>  @param mj y dimension in arrays xlat and xlon
!>  @param clat array of nst-inst+1 elemets of latitudes
!>  @param clon array of nst-inst+1 elemets of longitudes
!>  @param inst start value of clat and clon arrays
!>  @param nst end  value of clat and clon arrays
!>  @param ist column index value
!>  @param jst row index value
  Subroutine localiza(xlat,xlon,mi,mj,clat,clon,ist,jst,inst,nst)
  implicit none
  integer,intent(IN) :: mi,mj,nst,inst
  integer ::i,j,l
  integer,dimension(:),intent(out):: ist,jst
  real,dimension(:,:),intent(IN):: xlat,xlon
  real,dimension(:),intent(IN):: clat,clon
   do l=inst,nst
		! Out of the region
   ist(l)=0
   jst(l)=0
	  do i = 1,mi-1
	    do j= 1,mj-1
        if(clon(l) .ge. xlon(i,j)  .and. clon(l) .le. xlon(i+1,j).and.&
		  &clat(l) .ge. xlat(i,j)  .and. clat(l) .le. xlat(i,j+1))then
		   ist(l)= i
		   jst(l)= j
          goto 987
		end if
		end do
	  end do
987 continue
   end do
   RETURN
   end subroutine localiza
end program point_temporal
