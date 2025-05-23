!  Creado por Jose Agustin Garcia Reynoso el 06/06/12.
! Proposito
!          Distribución temporal de las emisiones de fuentes puntuales
!
! ifort -O2 -axAVX2 t_puntal.F90 -o Puntual.exe
!
!>  @brief for t_puntal.F90 program. Emissions allocation in grid and hourly.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module point_vars_mod
!>   number of pollutants
integer, parameter::nsp=10 !number of pollutants
!>   hours per day
integer, parameter:: nh=24 !number of hours
!>  column for PM2.5 in emissions input file puntual.csv
integer,parameter:: ipm=2  ! PM2.5
!>  column for VOC in emissions input file puntual.csv
integer,parameter:: ivoc=6  ! VOC position in puntual.csv
!> type day (1=Mon, 2= Tue, ... 7=Sun)
integer :: daytype ! tipo de dia 1 lun a 7 dom
!> layers where emissions are reach day and night
integer,allocatable :: capa(:,:); !> **i** index in grid to allocate a point emission
integer,allocatable :: ict(:) ; !> **j** index in grid to allocate a point emission
integer,allocatable :: jct(:) ;!> _GRIDCODE_ in grid domain
integer,allocatable :: idcg(:,:)
!> profile ID 1=mon 2=weekday 3=hourly per SCC and pollutant.
integer,allocatable :: profile(:,:);!> Time zone integer
integer,allocatable :: mcst(:,:) ;!> Temporal profile (temp_var)
integer :: iprof ;!> Number of lines in input file
integer :: nl ;!> Number of longitudes (columns) in localiza file
integer :: nx ;!> Number of latitudes (rows( in localiza file
integer :: ny ;!> if is dayligth time saving period
integer :: iverano
!> Fraction of weeks per days in the month
real :: fweek
!> Latitude point source
real,allocatable :: lat(:) ;!> Longitude point source
real,allocatable :: lon(:)
real,allocatable :: pf(:,:) ;!> point source emission input
real,allocatable :: e_mis(:,:) ;!> point source emission w/temporal profile
real,allocatable :: emis(:,:,:)
!> month integer
real,allocatable :: mes(:) ;!> current day
real,allocatable :: dia(:) ;!> previus day
real,allocatable :: diap(:)
!> Time zone  CST
real,allocatable :: hCST(:,:) ;!> Time zone MST
real,allocatable :: hMST(:,:) ;!> Time zone PST
real,allocatable :: hPST(:,:) ;!> Time zone EST
real,allocatable :: hEST(:,:)
!> Source clasification code array
character(len=10),allocatable::iscc(:)
!> Pollutant name
character (len=7) :: cvar(nsp)
!> Initial date of the emissions period
character (len=19) :: current_date
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

    call point_temp_distribution

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
!   Horario de verano Abril 3 a octubre 30 en 2016
    iverano=0
    if(lsummer) iverano=kverano(idia,month)
    print *,'Current Date: ',current_date,month,idia,fweek
!
!   Days in year
!
    write(canio,'("../time/anio",I4,".csv")')anio
    print *," READING FILE: ",canio

    open (newunit=m,file=canio,status='OLD',action='read')
    daytype=0
    read(m,*)cdum
!$omp parallel sections num_threads (2)
!$omp section
    do
      read(m,*,end=95)imon,ipdy,idum,cdum
      if(imon.eq.month.and. ipdy.eq.idia) then
        daytype=idum
        print *,'Day type :',daytype,cdum
        exit
      end if
    end do
95  continue
    close(m)
	if(daytype.eq.0) STOP 'Error in daytype=0'
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
    allocate(mes(nl),dia(nl),diap(nl))
    allocate(hEST(nl,nh),hCST(nl,nh),hMST(nl,nh),hPST(nl,nh))
    allocate(emis(i,nsp,nh))
    allocate(profile(3,nl))
    allocate(ict(nl),jct(nl))
    e_mis=0
!$omp parallel do
    do i=1,nl  ! Defaul values for temporal profile
      profile(1,i)=262
      profile(2,i)=7
      profile(3,i)=24
    end do
!$omp end parallel do
    rewind(iun)
    read (iun,*) cdum,cdum,cdum,(cvar(i),i=1,nsp)
    do i=1,nl
      read(iun,*,err=110)lat(i),lon(i),iscc(i),(e_mis(i,j),j=1,nsp),capa(i,1),capa(i,2)
    end do
    close(iun)
    e_mis=e_mis*1000000 !para g desde (Mg) TON
    print *,'Done puntual.csv '!,cvar,maxval(e_mis)
!
!	temporal_01.txt
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
!  Reading and findig monthly, week and houry code profiles
    inquire(15,opened=fil1)
    if(.not.fil1) then
        canio="../time/"//"temporal_01.txt"
        open(unit=15,file=canio,status='OLD',action='read')
	else
        rewind(15)
	end if
	read (15,'(A)') cdum
    do
      read(15,*,END=200)jscc,imon,iwk,ipdy
      if(trim(jscc).ne."0") then
!$omp parallel do
        do i=1,nl
          if(iscc(i).eq.jscc) then
            profile(1,i)=imon
            profile(2,i)=iwk
            profile(3,i)=ipdy
          end if
        end do
!$omp end parallel do
      end if
    end do
 200 continue
     !print '(A3,<nl>(I5))','mon',(profile(1,i),i=1,6)
     !print '(A3,<nl>(I4,x))','day',(profile(2,i),i=1,6)
	  !print '(A3,<nl>(I3,x))','hr ',(profile(3,i),i=1,6)
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
	    read(16,*,END=215)iprof,(itfrc(l),l=1,13)
!$omp parallel do
	    do i=1,nl
	      if(iprof.eq.profile(1,i)) then
	        mes(i)=real(itfrc(month))/real(itfrc(13))
	      end if
		end do !i
!$omp end parallel do
	 end do
 215 continue
    ! print '(A3,<nl>(f6.3))','mon',(mes(i),i=1,nl)
	 print *,'   Done Temporal_mon'
!  Reading and findig weekely  profile
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
!$omp parallel do
	    do i=1,nl
	      if(iprof.eq.profile(2,i)) then
	        dia(i)=real(itfrc(daytype))/real(itfrc(8))
            if(daytype.eq.1) then
                diap(i)=real(itfrc(daytype+6))/real(itfrc(8))
            else
                diap(i)=real(itfrc(daytype-1))/real(itfrc(8))
            end if
	      end if
		end do !i
!$omp end parallel do
	 end do
 220 continue
     !print '(A3,<nl>(f6.3))','day',(dia(i),i=1,nl)
	 print *,'   Done Temporal_week'

	 nfilep='temporal_wkend.txt'
	 nfile='temporal_wkday.txt'
!  Reading and findig houlry  profile
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
	    do i=1,nl
	      if(iprof.eq.profile(3,i)) then
            m=4-iverano
            do l=1,nh
            if(m+l.gt.nh) then
              hEST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
            else
              hEST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
            end if
            end do
            m=5-iverano
		    do l=1,nh
              if(m+l.gt.nh) then
                hCST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
			  else
                hCST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
			  end if
			end do
		    m=6-iverano
		    do l=1,nh
              if(m+l.gt.nh) then
                hMST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
              else
                hMST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
              end if
			end do
		    m=7-iverano
		    do l=1,nh
              if(m+l.gt.nh) then
                hPST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
			  else
                hPST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
			  end if
			end do
	      end if
		end do !i
	 end do
 230 continue
!   do l=1,nh
!   print '(A3,x,I3,x,<nscc(k)>(f7.4))','hr',l,(hCST(i,k,l),i=1,nscc(k))
!   end do
    if(daytype.eq.1) then
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
          do i=1,nl
            if(iprof.eq.profile(3,i)) then
              m=4-iverano
                do l=1,nh
                if(m+l.gt.nh) then
                  hEST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
                else
                  hEST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
                end if
              end do
              m=5-iverano
              do l=1,nh
              if(m+l.gt.nh) then
                  hCST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
              end if
              end do
              m=6-iverano
              do l=1,nh
                if(m+l.gt.nh) then
                    hMST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
                end if
              end do
              m=7-iverano
              do l=1,nh
                if(m+l.gt.nh) then
                    hPST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
                end if
              end do
            end if
          end do !i
        end do ! File 19
240 continue
!
!    do l=1,nh
!    print '(A3,x,I3,x,<nscc(k)>(f7.4))','hr',l,(hCST(i,k,l),i=1,nscc(k))
!    end do
!
    end if !daytype
    print *,'   Done ',nfile
    close(15)
    close(16)
    close(17)
    close(18)
    close(19)

	return
110	print *,'Error en ',i
    STOP
end subroutine point_emis_reading
!              _       _
!  _ __   ___ (_)_ __ | |_
! | '_ \ / _ \| | '_ \| __|
! | |_) | (_) | | | | | |_
! | .__/ \___/|_|_| |_|\__|
! |_|
!  _                             _ _     _        _ _           _   _
! | |_ ___ _ __ ___  _ __     __| (_)___| |_ _ __(_) |__  _   _| |_(_) ___  _ __
! | __/ _ \ '_ ` _ \| '_ \   / _` | / __| __| '__| | '_ \| | | | __| |/ _ \| '_ \
! | ||  __/ | | | | | |_) | | (_| | \__ \ |_| |  | | |_) | |_| | |_| | (_) | | | |
!  \__\___|_| |_| |_| .__/___\__,_|_|___/\__|_|  |_|_.__/ \__,_|\__|_|\___/|_| |_|
!                   |_| |_____|
!>  @brief Distributes point emissions from anual to hourly fluxes
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine point_temp_distribution
	implicit none
	integer i,j,kk,l,ival,ii
!
	print *,'Temporal distribution'
    mes=mes*fweek
!$omp parallel do
  do i=1,nl
    if(ict(i).ne.0 .or.jct(i).ne.0) then
      do kk=1,nsp
      !print *,'k=',kk
        do l=1,nh
    if(mcst(ict(i),jct(i)).eq.6 )emis(i,kk,l)=e_mis(i,kk)*mes(i)*hCST(i,l) ! Mg to kg
    if(mcst(ict(i),jct(i)).eq.7 )emis(i,kk,l)=e_mis(i,kk)*mes(i)*hMST(i,l)
    if(mcst(ict(i),jct(i)).eq.8 )emis(i,kk,l)=e_mis(i,kk)*mes(i)*hPST(i,l)
        end do
      end do
    end if
  end do
!$omp end parallel do
end subroutine point_temp_distribution
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
				if(k.ne.ivoc.and.k.ne.ipm) then
					!write(iun,220)lat(i),lon(i),capa(i),(emis(i,k,l),l=1,nh)
					write(iun,210) idcg(ict(i),jct(i)),capa(i,1),(emis(i,k,l),l=1,nh),capa(i,2)
				else ! For VOC and PM2.5
					!write(iun,300)iscc(i),lat(i),lon(i),capa(i),(emis(i,k,l),l=1,nh)
					write(iun,310)iscc(i),idcg(ict(i),jct(i)),capa(i,1),(emis(i,k,l),l=1,nh),capa(i,2)
				end if
			end if
			end do
		close(unit=iun)
	end do
!$omp end parallel do
     print *,"****** DONE PUNTUAL *****"
    deallocate(iscc,capa,lat,lon,e_mis)
    deallocate(mes,dia,diap)
    deallocate(hEST,hCST,hMST,hPST)
    deallocate(emis)
    deallocate(profile)
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
