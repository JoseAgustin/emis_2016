!
!	t_puntal.F90
!
!
!  Creado por Jose Agustin Garcia Reynoso el 06/06/12.
! Proposito
!          Distribución temporal de las emisiones de fuentes puntuales
!
! ifort -O2 -axAVX2 t_puntal.F90 -o Puntual.exe
!
!   modificado
!   14/08/2012  nombre archivos de PM
!   02/10/2012  Ajuste en horas dia previo subroutina lee
!   12/07/2017  para 2014 y hEST
!   18/07/2017  Incluye CO2, CN y CH4, dos alturas.
!   06/04/2020  Incluye Horario de verano
!   29/04/2020  Para openmp
!>  @brief for t_puntal.F90 program. Emissions allocation in grid and hourly.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module vars_point
!> number of pollutants
integer, parameter::nsp=10 !number of pollutants
integer, parameter:: nh=24 !number of hours
integer,parameter:: ipm=2  ! PM2.5
integer,parameter:: ivoc=6  ! VOC position in puntual.csv
integer :: month
integer :: daytype ! tipo de dia 1 lun a 7 dom
integer,allocatable :: capa(:,:),ict(:),jct(:),idcg(:,:)
integer,allocatable :: profile(:,:),mcst(:,:)
integer :: iprof
integer :: nl,nx,ny
integer :: iverano  ! si es en periodo de verano
integer :: idia     ! dia para el calculo de emisiones
integer :: anio     ! anio de las emisiones 2016
integer ::periodo! =1 uno 24 hr, =2 dos de 12hrs c/u
integer,dimension(2014:2020) :: inicia   ! dia inicio horario verano
integer,dimension(2014:2020) :: termina  ! dia fin del horario de verano
integer,dimension(12) :: daym ! days in a month
real :: fweek
real,allocatable :: lat(:),lon(:),pf(:,:)
real,allocatable :: e_mis(:,:),emis(:,:,:)! line compounds nsp
real,allocatable :: mes(:),dia(:),diap(:)
real,allocatable :: hEST(:,:),hCST(:,:),hMST(:,:),hPST(:,:)
logical :: lsummer
character(len=10),allocatable::iscc(:)
character(len=12):: zona
character (len=7) :: cvar(nsp)
character (len=19) :: current_date
    ! number of day in a month
    !          jan feb mar apr may jun jul aug sep oct nov dec
    data daym /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!              2014 2015 2016 2017 2018 2019 2020
    data inicia  /6,  5,  3,   2,   1,   7,   5/
    data termina /26,25, 30,  29,  28,  27,  25/
common /dat/ nl,nx,ny,daytype,fweek,cvar,current_date
common /nlm_vars/lsummer,zona,month,idia,anio,periodo,inicia,termina

end module vars_point
!>  @brief Temporal distribution of point sources emissions.
!>
!>  using EPA temporal profiles based on SCC
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program t_puntual
use vars_point

    call lee_namelist

    call lee

    call calculos

    call guarda

contains
!  _
! | | ___  ___
! | |/ _ \/ _ \
! | |  __/  __/
! |_|\___|\___|
!
!>  @brief Reads emissions file and temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee
implicit none
	integer :: i,j,k,l,m,iun
	integer :: idum, imon,iwk,ipdy
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	real,allocatable ::xlat(:,:),xlon(:,:)
	real rdum
	logical fil1,fil2
  character(len=10)::jscc
	character(len=35)::cdum,canio
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
    write(canio,'("../01_datos/time/anio",I4,".csv")')anio
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
	open(newunit=iun,file='puntual.csv',status='old',action='read')
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
   cdum="../01_datos/"//trim(zona)//"/"//"localiza.csv"
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
        canio="../01_datos/time/"//"temporal_01.txt"
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
        canio="../01_datos/time/"//"temporal_mon.txt"
        open(unit=16,file=canio,status='OLD',action='read')
    else
	  rewind(16)
	end if
	read (16,'(A)') cdum
     do
	    read(16,*,END=210)iprof,(itfrc(l),l=1,13)
!$omp parallel do
	    do i=1,nl
	      if(iprof.eq.profile(1,i)) then
	        mes(i)=real(itfrc(month))/real(itfrc(13))
	      end if
		end do !i
!$omp end parallel do
	 end do
 210 continue
    ! print '(A3,<nl>(f6.3))','mon',(mes(i),i=1,nl)
	 print *,'   Done Temporal_mon'
!  Reading and findig weekely  profile
    inquire(17,opened=fil1)
    if(.not.fil1) then
      canio="../01_datos/time/"//"temporal_week.txt"
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
      canio="../01_datos/time/"//nfile !"temporal_wkday.txt"
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
            canio="../01_datos/time/"//nfilep
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
end subroutine lee
!            _            _
!   ___ __ _| | ___ _   _| | ___  ___
!  / __/ _` | |/ __| | | | |/ _ \/ __|
! | (_| (_| | | (__| |_| | | (_) \__ \
!  \___\__,_|_|\___|\__,_|_|\___/|___/
!>  @brief Distributes emissions from anual to hourly fluxes
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine calculos
	implicit none
	integer i,j,kk,l,ival,ii
!
	print *,'Computations'
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
end subroutine calculos
!                            _
!   __ _ _   _  __ _ _ __ __| | __ _
!  / _` | | | |/ _` | '__/ _` |/ _` |
! | (_| | |_| | (_| | | | (_| | (_| |
!  \__, |\__,_|\__,_|_|  \__,_|\__,_|
!  |___/
!>  @brief Stores houry emission by pollutant
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine guarda
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
end subroutine guarda
!
!  _                 _ _
! | | ___   ___ __ _| (_)______ _
! | |/ _ \ / __/ _` | | |_  / _` |
! | | (_) | (_| (_| | | |/ / (_| |
! |_|\___/ \___\__,_|_|_/___\__,_|
!
!>  @brief Identifies the grid code for the spatial
!>   allocation of point source emissions
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>  @param xlat two dimensional array with latitudes
!>  @param xlon two dimensional array with longitudes
!>  @param mi x dimension in arrays xlat and xlon
!>  @param mj y dimension in arrays xlat and xlon
!>  @param clat array of nst-inst+1 elemets of latitudes
!>  @param clon array of nst-inst+1 elemets of longitudes
!>  @param inst start value of clat and clon arrays
!>  @param nst end  value of clat and clon arrays
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
!  _
! | | ____   _____ _ __ __ _ _ __   ___
! | |/ /\ \ / / _ \ '__/ _` | '_ \ / _ \
! |   <  \ V /  __/ | | (_| | | | | (_) |
! |_|\_\  \_/ \___|_|  \__,_|_| |_|\___/
!
!>  @brief Identifies if it is summert time period and if it is considered or not
!>
!> based on the day and month of the selected period
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param ida day of the month
!>   @param mes month
integer function kverano(ida,mes)
    implicit none
    integer, intent(in):: ida,mes
    if (mes.lt.4  .or. mes .gt.10)then
      kverano = 0
      return
    end if
    if (mes.gt.4 .and. mes .lt.10) then
      kverano = 1
      write(6, 233) inicia(anio),termina(anio)
      return
    end if
    if (mes.eq.4 .and. ida .ge. inicia(anio)) then
      kverano = 1
      write(6, 233) inicia(anio),termina(anio)
      return
      elseif (mes.eq.10 .and. ida .le.termina(anio)) then
        kverano = 1
        write(6, 233) inicia(anio),termina(anio)
        return
      else
        kverano =0
        return
    end if
233 format("******  HORARIO de VERANO *******",/,3x,"Abril ",I2,x,"a Octubre ",I2)
end function
!  _                                          _ _     _
! | | ___  ___     _ __   __ _ _ __ ___   ___| (_)___| |_
! | |/ _ \/ _ \   | '_ \ / _` | '_ ` _ \ / _ \ | / __| __|
! | |  __/  __/   | | | | (_| | | | | | |  __/ | \__ \ |_
! |_|\___|\___|___|_| |_|\__,_|_| |_| |_|\___|_|_|___/\__|
!            |_____|
!>  @brief Reads global namelist input file for the temporal settings.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_namelist
    implicit none
    NAMELIST /region_nml/ zona
    NAMELIST /fecha_nml/ idia,month,anio,periodo
    NAMELIST /verano_nml/ lsummer,inicia,termina
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
    READ (unit_nml , NML = fecha_nml )
    READ (unit_nml , NML = verano_nml )
    !WRITE (6    , NML = verano_nml )
    close(unit_nml)
    else
    stop '***** No namelist_emis.nml in .. directory'
    end if
    if (month.lt.1 .or. month.gt.12) then
    print '(A,I3)','Error in month (from 1 to 12) month= ',month
    stop
    end if
    if (idia.gt.daym(month))then
    print '(A,I2,A,I2)','Error in day value: ',idia,' larger than days in month ',daym(month)
    stop
    end if
    close(10)
    end subroutine lee_namelist
end program t_puntual
