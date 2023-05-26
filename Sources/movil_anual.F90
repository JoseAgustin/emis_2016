!  Creado por Jose Agustin Garcia Reynoso el 25/05/12.
!
! Proposito
!          Lectura de emisiones de fuentes moviles y creacion de archivo netcdf
!   ifort -O3 movil_anual.F90 -o Mtemporal.exe
!
!> @brief For movil_anual.F90 program. Annual mobile emissions storage in CAMS
!> categories
!> Currently uses EPA temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
module movil_annual_mod
!> type day (1=Mon, 2= Tue, ... 7=Sun)
integer :: daytype ! tipo de dia 1 lun a 7 dom
!> Hourly temporal profile
integer :: perfil  ;!> number of pollutant emission files
integer,parameter :: nf=11 ;!> hours per day
integer,parameter :: hday=24 ;!> max number of scc descriptors in input files
integer,parameter :: nnscc=36; !> x dim for grid temporal prifiles gridded
integer,parameter :: nxt=28  ; !> y dim for grid temporal prifiles gridded
integer,parameter :: nyt=34  ; !> Day type 1 weekday, 2 saturday, 3 sunday
integer,parameter :: iday=3  ; !> Pollutant ID in temporl profile  CO,NO,VOC, VOC Diesel, SO2
integer,parameter :: ispc=5  ; !> Type number of vehicle  1 all, 2 automobile gas
integer,parameter :: v_type=9 ;!> stores gricode values for grids with temporal profiles
integer,allocatable :: idcg(:) ;!> stores gricode value from localiza.csv
integer,allocatable :: idcg2(:);!> i index in temporal domain for each gricode
integer,allocatable :: idx(:) ;!> j index in temporal domain for each gricode
integer,allocatable :: idy(:) ;!> array with map scc domain and TP domain SCC
integer,allocatable :: mscc(:);!> longitudes in output file from localiza
real,allocatable :: xlon(:)  ;!> latitudes in output file from localiza
real,allocatable :: xlat(:)  ;!> longitud coordinate for temporal profile mesh
real,dimension(nxt,nyt) :: tlon;!> latitude coordinate for temporal profile mesh
real,dimension(nxt,nyt) :: tlat ;!>t_prof_m gridded temporal profiles
real,dimension(nxt,nyt,iday,ispc,v_type,hday)::t_prof_m
!> number of emissions file
integer :: nm ! grid number in emissions file
!> if is dayligth time saving period
integer :: iverano  ! si es en periodo de verano
!> number of scc codes per file
integer,dimension(nf) :: nscc    ;!> GRIDID in emissions
integer, allocatable :: idcel(:) ;!> Difference in number of hours (CST, PST, MST)
integer, allocatable :: mst(:)   ;!> fraction = weeks/month days
real :: fweek                    ;!> Mobile emisions from files cel,ssc,file
real,allocatable :: emiM(:,:,:)   ;!> Emission by cel,file and hour (inorganic)
real,allocatable :: emis(:,:,:)  ;!> VOC emissions cel,scc and hour
real,allocatable :: evoc(:,:,:)  ;!> PM2.5 emissions cel,scc and hour
real,allocatable :: epm2(:,:,:)  ;!> Time zone  CST for gridded temporal profiles
real,allocatable :: tCST(:,:,:,:) ! idcel,pollutant,veh_type,hrs
!> month interger
real,dimension(nnscc,nf) :: mes ;!> current day
real,dimension(nnscc,nf) :: dia ;!> previus day
real,dimension(nnscc,nf) ::diap ! dia currentday, diap previous day
!> Time zone  CST
real,dimension(nnscc,nf,hday):: hCST ;!> Time zone MST
real,dimension(nnscc,nf,hday):: hMST ;!> Time zone PST
real,dimension(nnscc,nf,hday):: hPST ;!> Time zone EST
real,dimension(nnscc,nf,hday):: hEST
!> profile ID 1=mon 2=weekday 3=hourly per SCC and pollutant.
integer,dimension(3,nnscc,nf):: profile  ! 1=mon 2=weekday 3=hourly
!> SCC codes per file
character (len=10),dimension(nnscc) ::iscc ;!> SCC codes from Temporal Profiles files
character(len=10),dimension(v_type,1)::cscc
!> Initial date of the emissions period
character (len=19) :: current_date
character(len=14),dimension(nf) :: efile ; !> output file name
character(len=14),dimension(nf) :: casn


 data efile / 'M_CO.csv' ,'M_NH3.csv','M_NO2.csv','M_NO.csv',&
              'M_SO2.csv','M_CN.csv' ,'M_CO2.csv','M_CH4.csv',&
              'M_PM10.csv',&
              'M_PM25.csv','M_VOC.csv'/

 data casn /'TMCO__2016.csv','TMNH3_2016.csv','TMNO2_2016.csv', &
            'TMNO__2016.csv','TMSO2_2016.csv','TMCN__2016.csv', &
            'TMCO2_2016.csv','TMCH4_2016.csv','TMPM102016.csv', &
            'TMPM2_2016.csv','TMCOV_2016.csv'/

common /vars/ fweek,nscc,nm,daytype,perfil,mes,hora,current_date
common /emi_vars/dia,diap,hCST,hMST,hPST,hEST,efile,casn,iscc
common /domain/ iverano
common /leenetcdf/profile,tlon,tlat,t_prof_m,cscc
end module movil_annual_mod
!                        _ _    _                                       _
!  _ __ ___   _____   _(_) |  | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |
! | '_ ` _ \ / _ \ \ / / | |  | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |
! | | | | | | (_) \ V /| | |  | ||  __/ | | | | | |_) | (_) | | | (_| | |
! |_| |_| |_|\___/ \_/ |_|_|___\__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|
!                        |_____|               |_|
!  Progran  movil_anual.F90
!
!  Make the temporal distribution of mobile emissions
!
!>  @brief Make temporal distribution of mobile emissions using profiles based on SCC.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program movil_annual
   use master
   use movil_annual_mod

    call lee_namelist

    call lee_localiza

    call movil_annual_storing

    call libera_memoria

contains
!>  @brief Deallocates allocated arrays.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine libera_memoria

  if(allocated(idcel))  deallocate(idcel)
  if(allocated(mst))    deallocate(mst)
  if(allocated(emiM))   deallocate(emiM)
  if(allocated(emis))   deallocate(emis)
  if(allocated(evoc))   deallocate(evoc)
  if(allocated(epm2))   deallocate(epm2)
  if(allocated(idcg))   deallocate(idcg)
  if(allocated(tCST))   deallocate(tCST)

  write(6,170)
  write(6,180)
170 format(7x,"XXXXXX  Released memory    XXXXXX")
180 format(7x,"*****  DONE MOBILE TEMPORAL *****")
end subroutine libera_memoria

!  _               _                 _ _
! | | ___  ___    | | ___   ___ __ _| (_)______ _
! | |/ _ \/ _ \   | |/ _ \ / __/ _` | | |_  / _` |
! | |  __/  __/   | | (_) | (_| (_| | | |/ / (_| |
! |_|\___|\___|___|_|\___/ \___\__,_|_|_/___\__,_|
!            |_____|
!>  @brief Reada the lon,lat and utm coordinates for the output grid
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_localiza
    implicit NONE
    character(len=39) :: flocaliza,cdum,titulo
    integer i,j,k,idum,ncel
    integer :: nx,ny
    flocaliza='../'//trim(zona)//'/'//'localiza.csv'
    write(6,*)' >>>> Reading file -',flocaliza,' ---------'
    open (unit=10,file=flocaliza,status='old',action='read')
    read (10,*) cdum  !Header
    read (10,*) nx,ny,titulo  ! Dimensions and Title
    ncel=nx*ny
    allocate(idcg2(ncel))
    allocate(xlon(ncel),xlat(ncel))
    do k=1,ncel
      read(10,*) idcg2(k),xlon(k),xlat(k)
    end do
    close(10)
end subroutine lee_localiza
!                       _ _                                  _
!  _ __ ___   _____   _(_) |    __ _ _ __  _ __  _   _  __ _| |
! | '_ ` _ \ / _ \ \ / / | |   / _` | '_ \| '_ \| | | |/ _` | |
! | | | | | | (_) \ V /| | |  | (_| | | | | | | | |_| | (_| | |
! |_| |_| |_|\___/ \_/ |_|_|___\__,_|_| |_|_| |_|\__,_|\__,_|_|
!            _             |_____|
!       ___| |_ ___  _ __(_)_ __   __ _
!      / __| __/ _ \| '__| | '_ \ / _` |
!      \__ \ || (_) | |  | | | | | (_| |
!  ____|___/\__\___/|_|  |_|_| |_|\__, |
! |_____|                         |___/
!>  @brief Saves mobile emission with the temporal profile in hourly basis.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/26/2023
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine movil_annual_storing
#ifdef _OPENMP
    use omp_lib
#endif
  implicit none
  integer i,j,k,l,iun
  real suma
  character(len=3):: cdia(7)
  data cdia/'MON','TUE','WND','THR','FRD','SAT','SUN'/
!$omp parallel sections num_threads (3) private(iun,i,l)
!$omp section
  do k=1,nf-2
    write(6,170) casn(k),efile(k)
    open(newunit=iun,file=casn(k),action='write')
    write(iun,*)casn(k),',ID, Hr to Hr24,g/h'
    write(iun,'(I8,4A)')size(emis,dim=1),",",current_date,', ',cdia(daytype)
    do i=1,size(emis,dim=1)
      suma=0
      do l=1,hday
        suma=suma+emis(i,k,l)
      end do
      if(suma.gt.0) write(iun,100)idcel(i),(emis(i,k,l),l=1,hday)
    end do
    close(unit=iun)
  end do
100 format(I7,",",23(ES12.4,","),ES12.4)
!$omp section
   k=nf-1
! WARNING iscc voc must be the last one to be read.
  write(6,170) casn(k),efile(k)
  open(newunit=iun,file=casn(k),action='write')
  write(iun,*)casn(k),'ID, SCC,  Hr to Hr24'
  write(iun,'(I8,4A)')size(epm2,dim=1)*nscc(k),', ',current_date,', ',cdia(daytype)
  do i=1,size(epm2,dim=1)
    do j=1,nscc(k)
      suma=0
      do l=1,hday
        suma=suma+epm2(i,j,l)
      end do
      if(suma.gt.0) write(iun,110)idcel(i),iscc(j),(epm2(i,j,l),l=1,hday)
    end do
  end do
  close(iun)
!$omp section
! WARNING iscc voc must be the last one to be read.
  k=nf
  write(6,170) casn(k),efile(k)
  open(newunit=iun,file=casn(k),action='write')
  write(iun,*)casn(k),'ID, SCC,  Hr to Hr24'
  write(iun,'(I8,4A)')size(evoc,dim=1)*nscc(k),', ',current_date,', ',cdia(daytype)
  do i=1,size(evoc,dim=1)
    do j=1,nscc(k)
      suma=0
      do l=1,hday
        suma=suma+evoc(i,j,l)
      end do
      if(suma.gt.0)write(iun,110)idcel(i),iscc(j),(evoc(i,j,l),l=1,hday)
    end do
  end do
  close(iun)
!$omp end parallel sections

    write(6,180)
110 format(I7,",",A10,",",23(ES12.4,","),ES12.4)
170 format(5x,"Storing: ",A15,1x,A15)
180 format(7x,"*****  DONE SAVING DATAFILES *****")
end subroutine movil_annual_storing


end program movil_annual
