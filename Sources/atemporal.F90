!   Creado por Jose Agustin Garcia Reynoso 12/07/2017.
!
! Proposito:
!          Realiza la distribucion temporal de las emisiones de area
!   ifort -O3 -axAVX atemporal.F90 -o Atemporal.exe
!   -c -parallel -guide
!  -parallel -Dtest_gap -opt-report=1 -opt-report-phase=par -opt-report-file=stdout atemporal.F90
! gfortran -DPGI  -fopenmp -O2 atemporal.F90 -o Atemporal.exe
!
!> @brief For atemporal.F90 program. Area emissions temporal distribution variables
!>
!> Currently uses EPA temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module area_temporal_mod
!> type day (1=Mon, 2= Tue, ... 7=Sun)
integer :: daytype ;!> number of emission files
integer,parameter :: nf=10    ;!> max number of scc descriptors in input files
integer,parameter :: nnscc=59 ;!> number of day in year
integer,parameter ::juliano=366 ;!> number of hour per day
integer,parameter :: nh=24 ;!> number of max lines in emiA
integer :: nmax
!> Number of lines in emissions file
integer :: nm
!> Number of lines in Time zone file
integer :: lh ; !> if is dayligth time saving period
integer :: iverano  ! si es en periodo de verano
!> number of scc codes per file
integer,dimension(nf) :: nscc ; !> GRIDID in emissions
integer, allocatable :: idcel(:) ;!> GRIDID not duplictes
integer, allocatable ::idcel2(:) ;!> GRIDID fir identification of not duplicate
integer, allocatable ::idcel3(:) ;!> state municipality IDs emiss and time zone
integer, allocatable :: idsm(:,:)
!> Fraction of weeks per days in the month
real ::fweek
!>Area emisions from files cel,ssc,file
real,allocatable ::emiA(:,:,:)
!> Emission by cel,file and hour (inorganic)
real,allocatable :: emis(:,:,:)
!> PM25 emissions cel,scc and hour
real,allocatable :: epm2(:,:,:)
!> VOC emissions cel,scc and hour
real,allocatable :: evoc(:,:,:)
!> month integer
real,dimension(nnscc,nf) :: mes ;!> current day
real,dimension(nnscc,nf) :: dia ;!> previus day
real,dimension(nnscc,nf) ::diap ! dia currentday, diap previous day
!> Time zone  CST
real,dimension(nnscc,nf,nh):: hCST ;!> Time zone MST
real,dimension(nnscc,nf,nh):: hMST ;!> Time zone PST
real,dimension(nnscc,nf,nh):: hPST ;!> Time zone EST
real,dimension(nnscc,nf,nh):: hEST
!> profile ID 1=mon 2=weekday 3=hourly per SCC and pollutant.
integer,dimension(3,nnscc,nf):: profile
!> index per file
integer,allocatable :: id5(:,:)
!> SCC codes per file
character(len=10),dimension(nnscc) ::iscc
!> Number of days in year
character(len=3),dimension(juliano):: cdia
!> Initial date of the emissions period
character (len=19) :: current_date
 !> Input file name
character(len=14),dimension(nf) ::efile ; !> output file name
character(len=14),dimension(nf) :: casn

 data efile/'ASO2_2016.csv','ANOx_2016.csv','ANH3_2016.csv',&
&           'ACO__2016.csv','APM10_2016.csv','ACO2_2016.csv',&
&           'ACN__2016.csv','ACH4_2016.csv','APM25_2016.csv',&
&           'AVOC_2016.csv'/
 data casn /'TASO2_2016.csv','TANOx_2016.csv','TANH3_2016.csv',&
&           'TACO__2016.csv','TAPM102016.csv','TACO2_2016.csv',&
&           'TACN__2016.csv','TACH4_2016.csv','TAPM2_2016.csv',&
&           'TAVOC_2016.csv'/
common /vars/ fweek,nscc,nm,lh,daytype,mes,dia,current_date
end module area_temporal_mod
!
!  Progran  atemporal.F90
!
!>  @brief Make the area emissions temporal distribution using profiles based on SCC.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program area_temporal
   use master
   use area_temporal_mod

   call lee_namelist

   call area_spatial_reading

   call area_temp_distribution

   call area_temporal_storing

contains
!
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|       _                        _ _
!  ___ _ __   __ _| |_(_) __ _| |    _ __ ___  __ _  __| (_)_ __   __ _
! / __| '_ \ / _` | __| |/ _` | |   | '__/ _ \/ _` |/ _` | | '_ \ / _` |
! \__ \ |_) | (_| | |_| | (_| | |   | | |  __/ (_| | (_| | | | | | (_| |
! |___/ .__/ \__,_|\__|_|\__,_|_|___|_|  \___|\__,_|\__,_|_|_| |_|\__, |
!     |_|                      |_____|                            |___/
!>  @brief Reads spatial area emissions and temporal profiles based on SCC.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine area_spatial_reading
	implicit none
	integer i,j,k,l,m
	integer idum,imon,iwk,ipdy,iun
	integer iprof ! scc code from temporal file
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	real rdum
	logical fil1,fil2
  character(len=10):: jscc
	character(len=4):: cdum
	character(len=18):: nfile,nfilep
  character(len=35):: canio

    write(current_date,'(I4,"-",I2.2,"-",I2.2,A9)')anio,month,idia,'_00:00:00'
    fweek= 7./daym(month)   !semanas en el mes
!   Horario de verano Abril 3 a octubre 30 en 2016
     iverano=0
    if(lsummer) iverano=kverano(idia,month)
    print *,'Current Date: ',current_date,month,idia!,fweek
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
	if(daytype.eq.0) STOP 'Error in daytype=0'

!
   call maxline(nmax)

	do k=1,nf
	open (newunit=iun,file="../"//efile(k),status='OLD',action='read')
	read (iun,'(A)') cdum
	read (iun,*) nscc(k),cdum,(iscc(i),i=1,nscc(k))
	!print '(5(I10,x))',(iscc(i),i=1,nscc(k))
	!print *,cdum,nscc(k)
	nm=0
	do
	   read(iun,*,end=100) cdum
	   nm=nm+1
	end do
100	 continue
     write(6,134)"  mn=",nm,"nmax=",nmax
    if (nm.gt.nmax) STOP "*** ERROR: nm larger than nmax edit code line 140"
	 rewind(iun)
	 if(k.eq.1) then
        allocate(idcel(nm),idcel2(nm),idcel3(nm))
        allocate(emiA(nf,nmax,nnscc),id5(nf,nmax),idsm(nf,nmax))
        emiA=0
        idsm=0
        id5=0
    else
        deallocate(idcel,idcel2,idcel3)
        allocate(idcel(nm),idcel2(nm),idcel3(nm))
	end if
	read (iun,'(A)') cdum
	read (iun,'(A)') cdum
	do i=1,nm
		read(iun,*) idcel(i),idsm(k,i),rdum,rdum,(emiA(k,i,j),j=1,nscc(k))
      id5(k,i)=idcel(i)
	        !print *,idcel(i),idsm(i),(emiA(k,i,j),j=1,16),nscc(k),k
	end do
    idcel3=idcel
	close(iun)
  print *,"Done reading: ",efile(k),size(idcel3)
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
            exit
		   end if
		end do
	  end do
 200 continue
      !print '(A3,<nscc(k)>(I5))','mon',(profile(1,i,k),i=1,nscc(k))
      !print '(A3,<nscc(k)>(I3,x))','day',(profile(2,i,k),i=1,nscc(k))
	  !print '(A3,<nscc(k)>(I3,x))','hr ',(profile(3,i,k),i=1,nscc(k))
	 print *,'   Done Temporal_01'

!  Reading and findig monthly profile
    inquire(16,opened=fil1)
    if(.not.fil1) then
       canio="../time/temporal_mon.txt"
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
            exit
	      end if
		end do !i
	 end do
 210 continue
    !print *,'   Mes antes ',maxval(mes)
     ! weeks per month
	! print '(A3,<nscc(k)>(f6.3))','mon',(mes(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_mon'!,maxval(mes)
!  Reading and findig weekely  profile
    inquire(17,opened=fil1)
    if(.not.fil1) then
      canio="../time/temporal_week.txt"
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
            !exit week
	      end if
		end do week!i
	 end do
 220 continue
     !print '(A5,<nscc(k)>(f6.3))','day  ',(dia(i,k),i=1,nscc(k))
     !print '(A5,<nscc(k)>(f6.3))','day p',(diap(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_week',maxval(diap)
     nfile='temporal_wkday.txt'
	 nfilep='temporal_wkend.txt'
!  Reading and findig houlry  profile
    inquire(18,opened=fil1)

    if(.not.fil1) then
        canio="../time/"//nfile !"temporal_wkday.txt"
        open(unit=18,file=canio,status='OLD',action='read')
	else
	  rewind(18)
	end if
!
	read (18,'(A)') cdum

     do
	    read(18,*,END=230)iprof,(itfrc(l),l=1,25)
    dias: do i=1,nscc(k)
	      if(iprof.eq.profile(3,i,k)) then
            m=4-iverano
            do l=1,nh
            if(m+l.gt.nh) then
              hEST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
              hEST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
		    m=5-iverano
		    do l=1,nh
			if(m+l.gt.nh) then
              hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
             else
              hCST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
          m=6-iverano
            do l=1,nh
            if(m+l.gt.nh) then
              hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
              hMST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
            m=7-iverano
            do l=1,nh
            if(m+l.gt.nh) then
              hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
              hPST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
			end do
!            exit dias
	      end if
		end do dias!i
     end do    ! File 18
230 continue
print *,'   Done ',nfile,daytype,maxval(hCST)!,maxval(hPST),maxval(hMST)

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
    fds: do i=1,nscc(k)
         if(iprof.eq.profile(3,i,k)) then
            m=4-iverano
            do l=1,nh
         if(daytype.eq.1 )then
            if(m+l.gt.nh) then
            hEST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            end if
          else
            if(m+l.gt.nh) then
            hEST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
            hEST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
          end if  ! daytype
            end do
           m=5-iverano
           do l=1,nh
           if(daytype.eq.1) then
             if(m+l.gt.nh) then
                hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
             end if
           else
            if(m+l.gt.nh) then
               hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
               hCST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
           end if !daytype
           end do
           m=6-iverano
           do l=1,nh
            if(daytype.eq.1) then
             if(m+l.gt.nh) then
                hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
             end if
            else
             if(m+l.gt.nh) then
                hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
              else
                hMST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
              end if
            end if !daytype
           end do
           m=7-iverano
           do l=1,nh
             if(daytype.eq.1 )then
                 if(m+l.gt.nh) then
                    hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                 end if
             else
                if(m+l.gt.nh) then
                    hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
                else
                    hPST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
                end if
             end if ! daytype
           end do
          !exit fds
         end if
        end do fds !i
        end do ! File 19
  end if
 240 continue
     !do l=1,nh
     ! print '("Hr",x,I2,x,<nscc(k)>(f6.3))',l,(hCST(i,k,l),i=1,nscc(k))
	 !end do
	 print *,'   Done ',nfilep,daytype,maxval(hEST)!,maxval(hPST),maxval(hMST)
	end do ! K
    close(15)
    close(16)
    close(17)
    close(18)
    close(19)
134 FORMAT(4x,A5,x,I6,x,A5,I6)
end subroutine area_spatial_reading
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|         _ _     _        _ _           _   _
! | |_ ___ _ __ ___  _ __     __| (_)___| |_ _ __(_) |__  _   _| |_(_) ___  _ __
! | __/ _ \ '_ ` _ \| '_ \   / _` | / __| __| '__| | '_ \| | | | __| |/ _ \| '_ \
! | ||  __/ | | | | | |_) | | (_| | \__ \ |_| |  | | |_) | |_| | |_| | (_) | | | |
!  \__\___|_| |_| |_| .__/___\__,_|_|___/\__|_|  |_|_.__/ \__,_|\__|_|\___/|_| |_|
!                   |_| |_____|
!>  @brief Computes the hourly emissions based on SCC temporal profiles from annual to hourly.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine area_temp_distribution
	implicit none
	integer i,j,k,l,ival,ii
!
    call area_grids_count ! computes the number of different cells
    call huso_horario ! identifies the time lag in each cell
!
! For inorganics
!
!$omp parallel sections num_threads (3) private(k,ii,i,l,j)
!$omp section
    print *,"   Temporal distribution for Inorganics"
    emis=0
    mes=mes*fweek! weeks per month
    do k=1,nf-2
      print *,efile(k)
	  do ii=1,size(idcel2)
	  do i=1,size(id5,2)
		 if(idcel2(ii).eq.id5(k,i))then
		  do l=1,nh
	        do j=1,nscc(k)
    if(idsm(k,i).eq.5) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hEST(j,k,l)
    if(idsm(k,i).eq.6) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hCST(j,k,l)
    if(idsm(k,i).eq.7) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hMST(j,k,l)
    if(idsm(k,i).eq.8) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hPST(j,k,l)
		    end do
		  end do
		  end if
        end do
	  end do
	end do
!
!  For PM2.5
!
!$omp section
    print *,"   Temporal distribution for PM2.5"
    epm2=0
	k=nf-1
   do ii=1,size(idcel2)
    do i=1,size(id5,2)
    if(idcel2(ii).eq.id5(k,i))then
		  do l=1,nh
	        do j=1,nscc(k)
    if(idsm(k,i).eq.5) epm2(ii,j,l)=epm2(ii,j,l)+emiA(k,i,j)*mes(j,k)*hEST(j,k,l)
    if(idsm(k,i).eq.6) epm2(ii,j,l)=epm2(ii,j,l)+emiA(k,i,j)*mes(j,k)*hCST(j,k,l)
    if(idsm(k,i).eq.7) epm2(ii,j,l)=epm2(ii,j,l)+emiA(k,i,j)*mes(j,k)*hMST(j,k,l)
    if(idsm(k,i).eq.8) epm2(ii,j,l)=epm2(ii,j,l)+emiA(k,i,j)*mes(j,k)*hPST(j,k,l)
		    end do
		  end do
      end if
     end do
    end do
!
!  For VOCs
!
!$omp section
    evoc=0
	k=nf

print *,"   Temporal distribution for VOCs"!,size(idcel2)
   do ii=1,size(idcel2)
    do i=1,size(id5,2)
     if(idcel2(ii).eq.id5(k,i))then
		  do l=1,nh
	        do j=1,nscc(k)
    if(idsm(k,i).eq.5) evoc(ii,j,l)=evoc(ii,j,l)+emiA(nf,i,j)*mes(j,nf)*hEST(j,nf,l)
    if(idsm(k,i).eq.6) evoc(ii,j,l)=evoc(ii,j,l)+emiA(nf,i,j)*mes(j,nf)*hCST(j,nf,l)
    if(idsm(k,i).eq.7) evoc(ii,j,l)=evoc(ii,j,l)+emiA(nf,i,j)*mes(j,nf)*hMST(j,nf,l)
    if(idsm(k,i).eq.8) evoc(ii,j,l)=evoc(ii,j,l)+emiA(nf,i,j)*mes(j,nf)*hPST(j,nf,l)
		    end do
		  end do
      end if
     end do
    end do
!$omp end parallel sections
	end subroutine area_temp_distribution
!
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|                   _         _             _
! | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |    ___| |_ ___  _ __(_)_ __   __ _
! | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |   / __| __/ _ \| '__| | '_ \ / _` |
! | ||  __/ | | | | | |_) | (_) | | | (_| | |   \__ \ || (_) | |  | | | | | (_| |
!  \__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|___|___/\__\___/|_|  |_|_| |_|\__, |
!                   |_|                    |_____|                         |___/
!>  @brief Saves area emission with the temporal profile in hourly basis.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine area_temporal_storing
  implicit none
  integer i,j,k,l,iun
  real suma
  character(len=3):: cdia(7)
  data cdia/'MON','TUE','WND','THR','FRD','SAT','SUN'/
  print *,"Area Emissions Temporal distribution saving"
!$omp parallel sections num_threads (3) private(k,i,l,j,iun)
!$omp section
  do k=1,nf-2
   open(newunit=iun,file=casn(k),action='write')
   write(iun,*)casn(k),'ID, Hr to Hr24'
   write(iun,'(I8,4A)')size(emis,dim=1),",",current_date,", ",cdia(daytype)
   do i=1,size(emis,dim=1)
    suma=0
    do l=1,nh
        suma=suma+emis(i,k,l)
    end do
        if(suma.gt.0) write(iun,100)idcel2(i),(emis(i,k,l),l=1,nh)
    end do
   close(unit=iun)
  end do
100 format(I7,",",23(ES12.3,","),ES12.3)
!$omp section
   k=nf-1
! WARNING iscc and pm25 must be the before last one to be read.
   print *," PM2.5"
   open(newunit=iun,file=casn(k),action='write')
   write(iun,*)casn(k),'ID, SCC,  Hr to Hr24'
   write(iun,'(I8,4A)')size(epm2,dim=1)*nscc(k),",",current_date,', ',cdia(daytype)
   do i=1,size(epm2,dim=1)
     do j=1,nscc(k)
     suma=0
     do l=1,nh
       suma=suma+epm2(i,j,l)
     end do
     if(suma.gt.0)  write(iun,110)idcel2(i),iscc(j),(epm2(i,j,l),l=1,nh)
     end do
   end do
	close(iun)
!$omp section
	k=nf
! WARNING iscc and voc must be the last one to be read.
   print *," VOC"
   open(newunit=iun,file=casn(k),action='write')
   write(iun,*)casn(k),'ID, SCC,  Hr to Hr24'
   write(iun,'(I8,4A)')size(evoc,dim=1)*nscc(k),",",current_date,', ',cdia(daytype)
   do i=1,size(evoc,dim=1)
     do j=1,nscc(k)
     suma=0
     do l=1,nh
       suma=suma+evoc(i,j,l)
     end do
     if(suma.gt.0) write(iun,110)idcel2(i),iscc(j),(evoc(i,j,l),l=1,nh)
     end do
   end do
!$omp end parallel sections
	close(iun)
    print *,"*****  DONE Temporal Area *****"
110 format(I7,",",A10,",",23(ES12.4,","),ES12.4)
    deallocate(idcel,id5,idcel2,idsm,emiA,emis,epm2,evoc)
end subroutine area_temporal_storing
!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|                           _
!   __ _ _ __(_) __| |___     ___ ___  _   _ _ __ | |_
!  / _` | '__| |/ _` / __|   / __/ _ \| | | | '_ \| __|
! | (_| | |  | | (_| \__ \  | (_| (_) | |_| | | | | |_
!  \__, |_|  |_|\__,_|___/___\___\___/ \__,_|_| |_|\__|
!  |___/                |_____|
!>  @brief Counts the number of different cells in file and stores in index.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine area_grids_count
  integer i,j
  integer idum
!  Se ordenan los indices
  call hpsort(size(idcel3))

  idcel2(1)=idcel3(1)
  j=1
  do i=2,nm
   if(idcel2(j).ne.idcel3(i)) then
    j=j+1
	idcel2(j)=idcel3(i)
	end if
  end do
  deallocate(idcel3)
  allocate(idcel3(j))
  print *,'Number of different cells',j

  open(unit=123,file="index.csv")
  write(123,*)j,"Index"
  do i=1,j
   write(123,'(I8)')idcel2(i)
   idcel3(i)=idcel2(i)
  end do
  deallocate(idcel2)
  allocate(idcel2(j))
  idcel2=idcel3
  close(123)
  allocate(emis(j,nf-2,nh))
  allocate(epm2(j,nscc(nf-1),nh),evoc(j,nscc(nf),nh))
   emis=0
   evoc=0
  deallocate(idcel3)
end subroutine area_grids_count
!  _                        _                          _
! | |__  _   _ ___  ___    | |__   ___  _ __ __ _ _ __(_) ___
! | '_ \| | | / __|/ _ \   | '_ \ / _ \| '__/ _` | '__| |/ _ \
! | | | | |_| \__ \ (_) |  | | | | (_) | | | (_| | |  | | (_) |
! |_| |_|\__,_|___/\___/___|_| |_|\___/|_|  \__,_|_|  |_|\___/
!                     |_____|
!>  @brief Identifies the time zone by state ID.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine huso_horario
    integer ::i,k,iedo
    print *,'Start uso horario'
    do k=1,nf
       do i=1,nm
        iedo=int(idsm(k,i)/1000)
        idsm(k,i)=6
        if(iedo.eq.2 .or.iedo.eq.3) idsm(k,i)=8
        if(iedo.eq.8 .or.iedo.eq.18 .and.iedo.eq.25 .and.iedo.eq.26)idsm(k,i)=7
        if(iedo.eq.23) idsm(k,i)=5
       end do
    end do
    if(maxval(idsm).ge.9 ) then
        print *,'Error item:', MAXLOC(idsm),'value:',maxval(idsm)
        STOP 'Value must be less or equal to 8'
    end if
    if(minval(idsm).le. 4 ) then
        print *,'Error item:', MINLOC(idsm),'value:',minval(idsm)
        STOP 'Value must be larger or equal to 5'
    end if
print *,'** END uso horario'
end subroutine huso_horario
!  _                          _
! | |__  _ __  ___  ___  _ __| |_
! | '_ \| '_ \/ __|/ _ \| '__| __|
! | | | | |_) \__ \ (_) | |  | |_
! |_| |_| .__/|___/\___/|_|   \__|
!       |_|
!>  @brief Sorts an array from minumun to max value.
subroutine hpsort(n)
    implicit none
    integer n
    integer i,ir,j,l
    real rra
    if (n.lt.2) return
    l=n/2+1
    ir=n
10 continue
    if(l.gt.1)then
        l=l-1
        rra=idcel3(l)
    else
        rra=idcel3(ir)
        idcel3(ir)=idcel3(1)
        ir=ir-1
        if(ir.eq.1)then
          idcel3(1)=rra
          return
        endif
    endif
    i=l
    j=l+l
20 if(j.le.ir)then
    if(j.lt.ir)then
      if(idcel3(j).lt.idcel3(j+1))j=j+1
    end if
    if(rra.lt.idcel3(j))then
        idcel3(i)=idcel3(j)
        i=j
        j=j+j
    else
        j=ir+1
    endif
    goto 20
    endif
      idcel3(i)=rra
    goto 10
end subroutine hpsort
!                       _ _
!  _ __ ___   __ ___  _| (_)_ __   ___
! | '_ ` _ \ / _` \ \/ / | | '_ \ / _ \
! | | | | | | (_| |>  <| | | | | |  __/
! |_| |_| |_|\__,_/_/\_\_|_|_| |_|\___|
!
!>  @brief Obtains the number of lines in *efile* file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine maxline(entero)
    implicit none
    integer,intent(out):: entero
    integer:: k,inum
    character(len=14):: cdum
    entero=-1
    do k=1,nf
      open(unit=14,file="../"//efile(k),status='OLD')
       inum=0
      do
        read(14,*,end=100) cdum
        inum=inum+1
      end do
100   close(14)
       inum=inum-2
      entero=max(inum,entero)
      !print '(I6)',entero
    end do
end subroutine maxline

end program area_temporal
