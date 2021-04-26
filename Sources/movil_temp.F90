!  Creado por Jose Agustin Garcia Reynoso el 25/05/12.
!
! Proposito
!          Distribución temporal de las emisiones de fuentes moviles
!   ifort -O3 movil_temp.f90 -o Mtemporal.exe
!
!> @brief For movil_temp.f90 program. Mobile emissions temporal distribution
!>
!> Currently uses EPA temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module movil_temporal_mod
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
end module movil_temporal_mod
!                       _ _    _                                       _
!  _ __ ___   _____   _(_) |  | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |
! | '_ ` _ \ / _ \ \ / / | |  | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |
! | | | | | | (_) \ V /| | |  | ||  __/ | | | | | |_) | (_) | | | (_| | |
! |_| |_| |_|\___/ \_/ |_|_|___\__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|
!                        |_____|               |_|
!  Progran  movil_temp.f90
!
!  Make the temporal distribution of mobile emissions
!
!>  @brief Make temporal distribution of mobile emissions using profiles based on SCC.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program movil_temporal
   use master
   use movil_temporal_mod

    call lee_namelist

    call mobile_spatial_reading

    call lee_movil_temp

    call lee_localiza

    call ubicar

    if(size(idcg).gt.1) call crea_perfil

    call mobile_temporal_distribution

    call mobile_temporal_storing

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
!            _    _ _
!  _ __  ___| |__(_) |___
! | '  \/ _ \ '_ \ | / -_)
! |_|_|_\___/_.__/_|_\___|  _                   _ _
!  ____ __  __ _| |_(_)__ _| |  _ _ ___ __ _ __| (_)_ _  __ _
! (_-< '_ \/ _` |  _| / _` | | | '_/ -_) _` / _` | | ' \/ _` |
! /__/ .__/\__,_|\__|_\__,_|_|_|_| \___\__,_\__,_|_|_||_\__, |
!    |_|                    |___|                       |___/
!>  @brief Reads emissions and temporal profiles based on SCC.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine mobile_spatial_reading
  implicit none
  integer i,j,k,l,m,iprof
  integer idum,imon,iwk,ipdy
  integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
  real rdum
  logical fil1,fil2
  character (len=10):: jscc ! scc code from temporal file
  character(len=4):: cdum
  character(len=18):: nfile,nfilep
  character(len=35):: canio

    write(current_date,'(I4,"-",I2.2,"-",I2.2,A9)')anio,month,idia,'_00:00:00'
    fweek=7./daym(month) !weeks per month
!Horario de verano Abril 6 a octubre 26
    iverano=0
    if(lsummer) iverano=kverano(idia,month)
    print *,'Current Date: ',current_date,month,idia,fweek
!
!   Days in 2016 year
!
    write(canio,'("../time/anio",I4,".csv")')anio
    print *," READING FILE: ",canio
    open (unit=10,file=canio,status='OLD',action='read')
    daytype=0
    read(10,*)cdum
        do
        read(10,*,end=95)imon,ipdy,idum,cdum
		if(imon.eq.month.and. ipdy.eq.idia) then
		 daytype=idum
		 print *,'Day type :',daytype,cdum
		 exit
		 end if
        end do
95  continue
    close(10)
	if(daytype.eq.0) STOP 'Error in daytype = 0'
!
	do k=1,nf  ! per pollutant
	open (unit=10,file="../"//efile(k),status='OLD',action='read')
	read (10,'(A)') cdum
	read (10,*) nscc(k),(iscc(i),i=1,nscc(k))
	!print '(5(I10,x))',(iscc(i),i=1,nscc(k))
	!print *,cdum,nscc(k)
	nm=0
	do
	   read(10,*,end=100) cdum
	   nm=nm+1
	end do
100	 continue
     !print *,nm
	 rewind(10)
	 if(k.eq.1) then
	allocate(idcel(nm),mst(nm))
	allocate(emiM(nm,nnscc,nf))
	end if
	read (10,'(A)') cdum
	read (10,'(A)') cdum
	do i=1,nm
		read(10,*) idcel(i),(emiM(i,j,k),j=1,nscc(k)),mst(i)
		!print *,i,idcel(i),(emiM(i,j,k),j=1,nscc(k)),mst(i)
	end do
	close(10)
	print *,"Done reading: ",efile(k)
!  Reading and findig monthly, week and houry code profiles
    inquire(15,opened=fil1)
    if(.not.fil1) then
      canio="../time/temporal_01.txt"
	  open(unit=15,file=canio,status='OLD',action='read')
	else
	  rewind(15)
	end if
	read (15,'(A)') cdum
      do
	  read(15,*,END=200)jscc,imon,iwk,ipdy
	    do i=1,nscc(k)
		  if(iscc(i).eq.jscc) then
		    profile(1,i,k)=imon
		    profile(2,i,k)=iwk
		    profile(3,i,k)=ipdy
		   end if
		end do
	  end do
 200 continue
    !print '(A3,<nscc(k)>(I5))','mon',(profile(1,i,k),i=1,nscc(k))
    !print '(A3,<nscc(k)>(I4,x))','day',(profile(2,i,k),i=1,nscc(k))
    !print '(A3,<nscc(k)>(I4,x))','hr ',(profile(3,i,k),i=1,nscc(k))
	 print *,'   Done Temporal_01'
!  Reading and findig monthly  profile
    inquire(16,opened=fil1)
    if(.not.fil1) then
        canio="../time/"//"temporal_mon.txt"
        open(unit=16,file=canio,status='OLD',action='read')
    else
        rewind(16)
	end if
	read (16,'(A)') cdum
     do
	    read(16,*,END=210)iprof,(itfrc(l),l=1,13)
	    do i=1,nscc(k)
	      if(iprof.eq.profile(1,i,k)) then
	        mes(i,k)=real(itfrc(month))/real(itfrc(13))
	      end if
		end do !i
	 end do
 210 continue
    ! print '(A3,<nscc(k)>(f6.3))','mon',(mes(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_mon'
!  REading and findig weekely  profile
    inquire(17,opened=fil1)
    if(.not.fil1) then
        canio="../time/"//"temporal_week.txt"
        open(unit=17,file=canio,status='OLD',action='read')
	else
        rewind(17)
	end if
	read (17,'(A)') cdum
     do
	    read(17,*,END=220)iprof,(itfrc(l),l=1,8)
    week: do i=1,nscc(k)
	      if(iprof.eq.profile(2,i,k)) then
	        dia(i,k)=real(itfrc(daytype))/real(itfrc(8))
            if(daytype.eq.1) then
              diap(i,k)=real(itfrc(daytype+6))/real(itfrc(8))
            else
              diap(i,k)=real(itfrc(daytype-1))/real(itfrc(8))
            end if
	      end if
		end do week!i
	 end do
 220 continue
    ! print '(A5,<nscc(k)>(f6.3))','day',(dia(i,k),i=1,nscc(k))
    ! print '(A5,<nscc(k)>(f6.3))','day p',(diap(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_week'
	 nfilep='temporal_wkend.txt'
	 nfile='temporal_wkday.txt'
!  REading and findig houlry  profile
    inquire(18,opened=fil1)
    if(.not.fil1) then
        canio="../time/"//nfile !"temporal_wkday.txt"
        open(unit=18,file=canio,status='OLD',action='read')
	else
	  rewind(18)
	end if
	read (18,'(A)') cdum
     do
	    read(18,*,END=230)iprof,(itfrc(l),l=1,25)
	   dias: do i=1,nscc(k)
           call adecua(profile(3,i,k),daytype,perfil)
	      if(iprof.eq.perfil) then
            m=4-iverano
            do l=1,hday
            if(m+l.gt.hday) then
              hEST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
              hEST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
		    m=5-iverano
		    do l=1,hday
            if(m+l.gt.hday) then
                hCST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
                hCST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
			end do
		    m=6-iverano
            do l=1,hday
            if(m+l.gt.hday) then
                hMST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
                hMST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
			end do
		    m=7-iverano
            do l=1,hday
            if(m+l.gt.hday) then
                hPST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
                hPST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
	      end if
		end do dias!i
	 end do  !  Files 18
 230 continue
!   do l=1,hday
!    print '(A3,x,I3,x,<nscc(k)>(f7.4))','hr',l,(hCST(i,k,l),i=1,nscc(k))
!   end do

     if(daytype.eq.1 .or. daytype.ge.6) then !lunes, Sabado y Domingo
        inquire(19,opened=fil2)
        if(.not.fil2) then
            canio="../time/"//nfilep
            open(unit=19,file=canio,status='OLD',action='read')
        else
            rewind(19)
        end if
       read (19,'(A)') cdum
      do
          read(19,*,END=240)iprof,(itfrc(l),l=1,25)
        fds:  do i=1,nscc(k)
            call adecua(profile(3,i,k),daytype,perfil)
            if(iprof.eq.perfil) then
            m=5-iverano
            do l=1,hday
            if(daytype.eq.1 )then
                if(m+l.gt.hday) then
                 hEST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.hday) then
                 hEST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hEST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if  ! daytype
            end do
            m=6-iverano
            do l=1,hday
            if(daytype.eq.1) then
                if(m+l.gt.hday) then
                 hCST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.hday) then
                 hCST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hCST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if !daytype
            end do
            m=7-iverano
            do l=1,hday
            if(daytype.eq.1) then
                if(m+l.gt.hday) then
                 hMST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.hday) then
                 hMST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hMST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if !daytype
            end do
            m=8-iverano
            do l=1,hday
            if(daytype.eq.1 )then
                if(m+l.gt.hday) then
                 hPST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                end if
            else
                if(m+l.gt.hday) then
                 hPST(i,k,m+l-hday)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                 hPST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
            end if ! daytype
            end do
            !exit fds
            end if
          end do fds!i
        end do ! File 19
    240 continue
!
!      do l=1,hday
!        print '("Hr",x,I3,x,<nscc(k)>(f7.4))', l,(hCST(i,k,l),i=1,nscc(k))
!      end do
!
    end if
	 print *,'   Done ',nfile,daytype
	end do ! K
  close(15)
  close(16)
  close(17)
  close(18)
  close(19)
end subroutine mobile_spatial_reading
!                  _     _ _         _                                       _
!  _ __ ___   ___ | |__ (_) | ___   | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |
! | '_ ` _ \ / _ \| '_ \| | |/ _ \  | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |
! | | | | | | (_) | |_) | | |  __/  | ||  __/ | | | | | |_) | (_) | | | (_| | |
! |_| |_| |_|\___/|_.__/|_|_|\___|___\__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|
!                              |_____|               |_|
!      _ _     _        _ _           _   _
!   __| (_)___| |_ _ __(_) |__  _   _| |_(_) ___  _ __
!  / _` | / __| __| '__| | '_ \| | | | __| |/ _ \| '_ \
! | (_| | \__ \ |_| |  | | |_) | |_| | |_| | (_) | | | |
!  \__,_|_|___/\__|_|  |_|_.__/ \__,_|\__|_|\___/|_| |_|
!
!>  @brief Computes the hourly emissions based on SCC temporal profiles from annual to hourly.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine mobile_temporal_distribution
	implicit none
	integer i,j,k,l,ival,ii
logical,allocatable:: lsi(:)
!
    allocate(emis(nm,nf-2,hday))
    allocate(epm2(nm,nscc(nf-1),hday))
    allocate(evoc(nm,nscc(nf),hday))
    allocate(lsi(nm))
    emis=0
    epm2=0
    evoc=0
    write(6,180)
! For inorganics
!
     mes=mes*fweek! computes nuber of weeks per month
  do k=1,nf-2
    lsi=.true.
    do i=1,nm  ! idcel(i)
      do l=1,hday
        do j=1,nscc(k)
        if(mst(i).eq.5) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hEST(j,k,l)
          if(mst(i).eq.6) then
            do ii=1,size(idcg)
              if(idcg(ii).eq. idcel(i)) then
                emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*tCST(ii,k,j,l)
                lsi(i)=.false.
                exit
              end if
            end do
          if(lsi(i) ) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hCST(j,k,l)
        end if
        if(mst(i).eq.7) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hMST(j,k,l)
        if(mst(i).eq.8) emis(i,k,l)=emis(i,k,l)+emiM(i,j,k)*mes(j,k)*hPST(j,k,l)
        end do
      end do
    end do
  end do
!  For PM2.5
!
  lsi=.true.
  k=nf-1
  do i=1,nm
    do l=1,hday
      do j=1,nscc(k)
      if(mst(i).eq.5) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hEST(j,k,l)
        if(mst(i).eq.6)then
          do ii=1,size(idcg)
            if(idcg(ii).eq. idcel(i)) then
              epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*tCST(ii,k,j,l)
              lsi(i)=.false.
              exit
            end if
          end do
          if(lsi(i)) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hCST(j,k,l)
        end if
      if(mst(i).eq.7) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hMST(j,k,l)
      if(mst(i).eq.8) epm2(i,j,l)=epm2(i,j,l)+emiM(i,j,k)*mes(j,k)*hPST(j,k,l)
      end do
    end do
  end do
!  For VOCs
!
  lsi=.true.
  k=nf
  do i=1,nm
    do l=1,hday
      do j=1,nscc(k)
      if(mst(i).eq.5) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hEST(j,nf,l)
        if(mst(i).eq.6)then
          do ii=1,size(idcg)
            if(idcg(ii).eq. idcel(i)) then
              evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*tCST(ii,nf,j,l)
              lsi(i)=.false.
              exit
            end if
          end do
          if(lsi(i)) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hCST(j,nf,l)
      end if
      if(mst(i).eq.7) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hMST(j,nf,l)
      if(mst(i).eq.8) evoc(i,j,l)=evoc(i,j,l)+emiM(i,j,nf)*mes(j,nf)*hPST(j,nf,l)
      end do
    end do
  end do
180 format(7x,"++++++    Starting Computations")
    deallocate(lsi)
	end subroutine mobile_temporal_distribution
!  _                                    _ _
! | | ___  ___     _ __ ___   _____   _(_) |
! | |/ _ \/ _ \   | '_ ` _ \ / _ \ \ / / | |
! | |  __/  __/   | | | | | | (_) \ V /| | |
! |_|\___|\___|___|_| |_| |_|\___/ \_/ |_|_|
!  _          |_____|
! | |_ ___ _ __ ___  _ __
! | __/ _ \ '_ ` _ \| '_ \
! | ||  __/ | | | | | |_) |
!  \__\___|_| |_| |_| .__/
!                   |_|
!> @brief reads netcdf files with gridded temporal profiles for CDMX
!>
!>  input files are ´temporal_week.nc´ ´temporal_saba.nc´ and ´temporal_domi.nc´
!>  these contain temporal profiles TP for different pollutants (5) and vehicle type (8)
!>  these profiles are for week days, saturday (saba) and sunday (domi)
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_movil_temp
use netcdf
integer :: ncid
integer :: id,j,k,l
integer :: varid  ;!> id_vardescr Vehicular types description
integer :: id_varscc
real,dimension(nxt,nyt,v_type,hday)::data_in
real,dimension(nxt,nyt,hday):: xxlon,xxlat
real,dimension(nxt,nyt,v_type):: e_co
character(len=11),dimension(ispc):: ename
character(len=26) :: FILE_NAME(iday)

ename=(/'TP_CO        ','TP_NO        ','TP_VOC       ',&
        'TP_VOC_diesel','TP_SO2       '/)
FILE_NAME=(/'temporal_week.nc','temporal_saba.nc','temporal_domi.nc'/)
do id=1,iday
  write(6,180) FILE_NAME(id)
  call check( nf90_open('../time/'//FILE_NAME(id), NF90_NOWRITE, ncid) )
  call check( nf90_inq_varid(ncid, "SCC", varid) )
  call check( nf90_get_var(ncid, varid, cscc) )
  do is=1,ispc
    call check( nf90_inq_varid(ncid, ename(is), varid) )
    ! Read the data.
    call check( nf90_get_var(ncid, varid, data_in) )
      do i=1,nxt
        do j=1,nyt
          do l=1,v_type
          do ih=1,hday
            t_prof_m(i,j,id,is,l,ih)=data_in(i,j,l,ih)
          end do
          end do
        end do
      end do
  end do
  if(id.eq.1)then
    call check( nf90_inq_varid(ncid, "E_CO", varid) )
    call check( nf90_get_var(ncid, varid, e_co) )
    call check( nf90_inq_varid(ncid, "XLONG", varid) )
    call check( nf90_get_var(ncid, varid, xxlon) )
    call check( nf90_inq_varid(ncid, "XLAT", varid) )
    call check( nf90_get_var(ncid, varid, xxlat) )
    do i=1,nxt
      do j=1,nyt
        if(e_co(i,j,1).gt.0) then !only with temporal profiles
          tlon(i,j)=xxlon(i,j,1)
          tlat(i,j)=xxlat(i,j,1)
        else
          tlon(i,j)=0.
          tlat(i,j)=0.
        end if
      end do
    end do
  end if
  call check( nf90_close(ncid) )
end do
!print *,tlon(nxt/2-1,nyt/2-1),tlon(nxt/2,nyt/2),tlat(nxt/2-1,nyt/2-1),tlat(nxt/2,nyt/2)
!print *,(t_prof_m(5,10,3,3,i,6),i=1,v_type)
180 format(7x,"******  Reading file: ",A20)
return
end subroutine lee_movil_temp
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
!        _     _
!  _   _| |__ (_) ___ __ _ _ __
! | | | | '_ \| |/ __/ _` | '__|
! | |_| | |_) | | (_| (_| | |
!  \__,_|_.__/|_|\___\__,_|_|
!> @brief obtains the _i_,_j_ index for localization in emissions mesh the temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine ubicar 
implicit none
integer:: id,k ! domain index
integer:: it,jt   ! temporal profile domain index
integer :: ndx ! domain dimensions
integer :: ntx,nty ! temporal profile domain dimensions
integer,allocatable :: ixv(:),jyv(:)
real :: lomin,lomax,lamin,lamax,diag,radio,menor
logical,allocatable:: valor(:)

ndx=size(xlon)
ntx=size(tlon,1)
nty=size(tlon,2)
allocate(ixv(ndx),jyv(ndx),valor(ndx))
valor=.false.
!domain Temporal profiles
lomin=minval(tlon)
lomax=maxval(tlon)
lamin=minval(tlat)
lamax=maxval(tlat)
radio=abs(tlon(ntx/2,nty/2)-tlon(ntx/2+1,nty/2+1)) &
     +abs(tlat(ntx/2,nty/2)-tlat(ntx/2+1,nty/2+1))
do id=1,ndx
  if(xlon(id).ge.lomin.and.xlon(id).le.lomax &
  .and. xlat(id).ge.lamin.and.xlat(id).le.lamax) then
      menor=radio
      do it=1,ntx
        do jt=1,nty
          diag=abs(xlon(id)-tlon(it,jt))+abs(xlat(id)-tlat(it,jt))
          if(diag.lt.radio.and. diag .lt. menor) then
            ixv(id)=it
            jyv(id)=jt
            if(diag.lt.menor) valor(id)=.true.
            menor=diag
          end if
        end do
      end do
  end if
end do
idx=pack(ixv,valor)
idy=pack(jyv,valor)
idcg=pack(idcg2,valor)
deallocate(ixv,jyv,valor)
deallocate(idcg2,xlon,xlat)
write(6,160) size(idcg)
return
160 format("Array with:",I5," elements")
end subroutine ubicar
!                                           __ _ _
!   ___ _ __ ___  __ _      _ __   ___ _ __ / _(_) |
!  / __| '__/ _ \/ _` |    | '_ \ / _ \ '__| |_| | |
! | (__| | |  __/ (_| |    | |_) |  __/ |  |  _| | |
!  \___|_|  \___|\__,_|____| .__/ \___|_|  |_| |_|_|
!                    |_____|_|
!>  @brief Develops a 24 hour profile for CST with or witput daylight savingd day
!>   @param status NetCDF functions return a non-zero status codes on error.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine crea_perfil
implicit none
integer :: i,l,m
integer :: is,it
integer :: id(nf)
allocate(tCST(size(idcg),nf, nscc(1) ,hday))
!
id =(/1,2,2,2,5,1,1,1,2,2,3/)
! 1  2   3   4   5  6   7   8   9     10  11
! CO,NH3,NO,NO2,SO2, CN CO2 CH4, PM10, PM25 VOC
! 1  2   2  2   5    1  1   1    2     2    3,4
  call map_scc ! for obtaining mscc array
m=7-iverano
do i=1,size(idcg)
  do is=1,nf
   do it=1,nscc(1)
    do l=1,hday
      if(m+l.gt.hday) then
        if(daytype.eq.1) tCST(i,is,it,m+l-hday)=t_prof_m(idx(i),idy(i),3,id(is),mscc(it),l)*diap(it,is)
        if(daytype.ge.2 .or. daytype.le.6) & !martes a sabado
        tCST(i,is,it,m+l-hday)=t_prof_m(idx(i),idy(i),1,id(is),mscc(it),l)*diap(it,is)
        if(daytype.eq.7) tCST(i,is,it,m+l-hday)=t_prof_m(idx(i),idy(i),2,id(is),mscc(1),l)*diap(it,is)
        else
        if(daytype.eq.1) tCST(i,is,it,m+l)=t_prof_m(idx(i),idy(i),3,id(is),mscc(it),l)*dia(it,is)
        if(daytype.ge.2 .or. daytype.le.6) &!martes a sabado
        tCST(i,is,it,m+l)=t_prof_m(idx(i),idy(i),1,id(is),mscc(it),l)*dia(it,is)
        if(daytype.eq.7) tCST(i,is,it,m+l)=t_prof_m(idx(i),idy(i),2,id(is),mscc(1),l)*dia(it,is)
      end if
    end do
   end do
  end do
end do
return
180 format(I6,21(I3,x))
end subroutine crea_perfil
!>  @brief Maps corresponding SCC from TP file to movil emissions SCC
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine map_scc
  implicit none
  integer i,j,k
  character(len=10):: comp
  allocate(mscc(nscc(1)))
  mscc=1
  do i=1,nscc(1)
    comp=iscc(i)
    do j=2,v_type-1
      if(comp.eq.cscc(j,1)) mscc(i)=j
    end do
    if(mscc(i).eq.1 .and. comp(1:4).eq."2230") mscc(i)=7
  end do
! 1 gasoline vehicles is allready assigned to all
! v_type= 9 municipal truck is not considered.
! 7 is for Diesel Trucks HDD
end subroutine map_scc
!            _    _ _
!  _ __  ___| |__(_) |___
! | '  \/ _ \ '_ \ | / -_)
! |_|_|_\___/_.__/_|_\___|          _      _           _
! | |_ ___ _ __  _ __  ___ _ _ __ _| |  __| |_ ___ _ _(_)_ _  __ _
! |  _/ -_) '  \| '_ \/ _ \ '_/ _` | | (_-<  _/ _ \ '_| | ' \/ _` |
!  \__\___|_|_|_| .__/\___/_| \__,_|_|_/__/\__\___/_| |_|_||_\__, |
!               |_|                 |___|                    |___/
!>  @brief Saves mobile emission with the temporal profile in hourly basis.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine mobile_temporal_storing
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
end subroutine mobile_temporal_storing
!
!> @brief Update temporal profile per day
!>
!> Currently uses EPA temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!> @param perfili mobile temporal profile to be update
!> @param idia day of the week 1 to 7 to be used to update the profile
!> @param perfilo update mobile temporal profile
subroutine adecua(perfili,idia,perfilo)
implicit none
integer, INTENT(IN)  :: perfili,idia
integer, INTENT(OUT) :: perfilo

if (perfili.eq.2013) then; perfilo=perfili+(idia-1)*100
else;perfilo=perfili;end if

end subroutine adecua

end program movil_temporal
