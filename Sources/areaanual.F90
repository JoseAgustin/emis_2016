!
! Proposito:
!      Realiza la lectura de las emisiones anuales de area y guarda en netcdf
!   ifort -O3 -axAVX areaanual.F90 -o Aanual.exe
!   -c -parallel -guide
!  -parallel -Dtest_gap -opt-report=1 -opt-report-phase=par -opt-report-file=stdout atemporal.F90
! gfortran -DPGI  -fopenmp -O2 atemporal.F90 -o Atemporal.exe
!
!> @brief For areaanual.F90 program. Read annual area emissions and save in netcdf file per pollutant.
!>
!>   Currently uses EPA temporal profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
module area_anual_mod
!> type day (1=Mon, 2= Tue, ... 7=Sun)
integer :: daytype ;!> number of emission files
integer,parameter :: nf=10    ;!> max number of scc descriptors in input files
integer,parameter :: nnscc=59 ;!> CAMS categories number
integer,parameter :: ncams=9 ;!> number of day in year
integer,parameter ::yjuliano=366 ;!> number of hour per day
integer,parameter :: nh=ncams ;!> number of max lines in emiA
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
!> Emission by cel,file (pollutant) and idCAMS
real,allocatable :: emis(:,:,:)
!> index per file
integer,allocatable :: id5(:,:)
!> SCC codes per file
character(len=10),dimension(nnscc) ::iscc
!> Initial date of the emissions period
character (len=19) :: current_date='2016-01-01_00:00:00'
 !> Input file name
character(len=14),dimension(nf)   :: efile ; !> output file name
character(len=14),dimension(nf)   :: casn;!> Categories in CAMS
character(len=3),dimension(ncams) :: idCAMS!> IPCC classification
character(len=4),dimension(ncams) :: idIPCC !> Pollutant abreviation
character(len=4),dimension(nf)    :: comp  !> Long name description
character(len=76),dimension(ncams):: long_name

 data efile/'ASO2_2016.csv','ANOx_2016.csv','ANH3_2016.csv',&
&           'ACO__2016.csv','APM10_2016.csv','ACO2_2016.csv',&
&           'ACN__2016.csv','ACH4_2016.csv','APM25_2016.csv',&
&           'AVOC_2016.csv'/
 data casn /'TASO2_2016.csv','TANOx_2016.csv','TANH3_2016.csv',&
&           'TACO__2016.csv','TAPM102016.csv','TACO2_2016.csv',&
&           'TACN__2016.csv','TACH4_2016.csv','TAPM2_2016.csv',&
&           'TAVOC_2016.csv'/
 data idCAMS/'AGL','AGS','ENE','IND','RES','SHP','SWD','TNR','TRO'/
 data idIPCC/'  3A','  3C',' 1A2',' 1A2',' 1A4',' 1A3d','   4',' 1A3','1A3b'/
 data long_name / &
'AGL: Agriculture_livestock                                                  ',&
'AGS: Agricultural_soils (without fires)                                     ',&
'ENE: Power_generation                                                       ',&
'IND: Industrial_process (Energy consumption of manufacture industry+process)',&
'RES: Residential_commercial_and_other_combustion                            ',&
'SHP: Navigation                                                             ',&
'SWD: Solid_waste_and_waste_water                                            ',&
'TNR: Non-road_transportation                                                ',&
'TRO: Road_transportation                                                    '/
data comp/'SO2 ','NOx ','NH3 ','CO  ','PM10','CO2 ','BC  ','CH4 ','PM25','VOC '/
common /vars/ fweek,nscc,nm,lh,daytype,mes,dia,current_date
common /texto/ idCAMS,idIPCC,long_name,comp
end module area_anual_mod
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
   use area_anual_mod

   call lee_namelist

   call area_spatial_reading

   call area_sorting

   call area_anual_storing

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
!>  @brief Reads spatial area emissions and stores in netcdf for CAMS.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine area_spatial_reading
	implicit none
	integer i,j,k,l,m
	integer idum,iun
	real rdum
	character(len=4):: cdum
	character(len=18):: nfile,nfilep
!
   call maxline(nmax)
!
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

      !print '(A3,<nscc(k)>(I5))','mon',(profile(1,i,k),i=1,nscc(k))
      !print '(A3,<nscc(k)>(I3,x))','day',(profile(2,i,k),i=1,nscc(k))
	  !print '(A3,<nscc(k)>(I3,x))','hr ',(profile(3,i,k),i=1,nscc(k))
	 print *,'   Done Reading files'

     !do l=1,nh
     ! print '("Hr",x,I2,x,<nscc(k)>(f6.3))',l,(hCST(i,k,l),i=1,nscc(k))
	 !end do
	 print *,'   Done ',nfilep
	end do ! K
    close(15)
    close(16)
    close(17)
    close(18)
    close(19)
134 FORMAT(4x,A5,x,I6,x,A5,I6)
end subroutine area_spatial_reading
subroutine area_sorting
  implicit none
  integer i,j,k,l,ival,ii

  call area_grids_count ! computes the number of different cells
  print *,"   Annual emissions"
  emis=0
  do k=1,nf
    print *,efile(k)
    do ii=1,size(idcel2)
        do i=1,size(id5,2)
        if(idcel2(ii).eq.id5(k,i))then
            do j=1,nscc(k)
            if(iscc(j).eq.'2102007000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2102004000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2103006000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2103007000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2104011000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2104006000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2104008000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2104007000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2267005000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2270005000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2302002000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2610000000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2801500100') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2810030000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2810001000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(trim(iscc(j)).eq.'30500304') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2280000000') emis(ii,k,6)=emis(ii,k,6)+emiA(k,i,j)
            if(iscc(j).eq.'2285000000') emis(ii,k,8)=emis(ii,k,8)+emiA(k,i,j)
            if(iscc(j).eq.'2275000000') emis(ii,k,8)=emis(ii,k,8)+emiA(k,i,j)
            if(iscc(j).eq.'2275050000') emis(ii,k,8)=emis(ii,k,8)+emiA(k,i,j)
            if(iscc(j).eq.'2222222222') emis(ii,k,9)=emis(ii,k,9)+emiA(k,i,j)
            if(iscc(j).eq.'2620030000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j).eq.'2230070310') emis(ii,k,9)=emis(ii,k,9)+emiA(k,i,j)
            if(iscc(j).eq.'2285002010') emis(ii,k,8)=emis(ii,k,8)+emiA(k,i,j)
            if(iscc(j).eq.'2265005000') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j).eq.'2801700000') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j).eq.'2805020000') emis(ii,k,1)=emis(ii,k,1)+emiA(k,i,j)
            if(iscc(j).eq.'5555555555') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2311010000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2801000002') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j).eq.'2801000005') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j).eq.'2294000000') emis(ii,k,9)=emis(ii,k,9)+emiA(k,i,j)
            if(iscc(j).eq.'2296000000') emis(ii,k,9)=emis(ii,k,9)+emiA(k,i,j)
            if(iscc(j).eq.'2425010000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2425030000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2425040000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2425000000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2461020000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2420000055') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2415000000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2401005000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2401008000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2415010000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2401990000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2401080000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2401065000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2401020000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2401001000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2465900000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2465200000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2465100000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2465400000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2465600000') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            if(iscc(j).eq.'2465800000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2465000000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'3333333333') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2501060000') emis(ii,k,5)=emis(ii,k,5)+emiA(k,i,j)
            if(iscc(j).eq.'2302050000') emis(ii,k,1)=emis(ii,k,1)+emiA(k,i,j)
            if(iscc(j).eq.'2461850000') emis(ii,k,2)=emis(ii,k,2)+emiA(k,i,j)
            if(iscc(j).eq.'2630030000') emis(ii,k,7)=emis(ii,k,7)+emiA(k,i,j)
            if(iscc(j).eq.'2850000010') emis(ii,k,4)=emis(ii,k,4)+emiA(k,i,j)
            end do
        end if
        end do
    end do
end do
!  end do
end subroutine area_sorting


!   __ _ _ __ ___  __ _
!  / _` | '__/ _ \/ _` |
! | (_| | | |  __/ (_| |
!  \__,_|_|  \___|\__,_|                   _         _             _
! | |_ ___ _ __ ___  _ __   ___  _ __ __ _| |    ___| |_ ___  _ __(_)_ __   __ _
! | __/ _ \ '_ ` _ \| '_ \ / _ \| '__/ _` | |   / __| __/ _ \| '__| | '_ \ / _` |
! | ||  __/ | | | | | |_) | (_) | | | (_| | |   \__ \ || (_) | |  | | | | | (_| |
!  \__\___|_| |_| |_| .__/ \___/|_|  \__,_|_|___|___/\__\___/|_|  |_|_| |_|\__, |
!                   |_|                    |_____|                         |___/
!>  @brief Saves area emission for CAMS.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
subroutine area_anual_storing
  implicit none
  integer i,j,k,l,iun
  real suma
  print *,"Area Emissions Temporal distribution saving"
!$omp parallel sections num_threads (3) private(k,i,l,j,iun)
!$omp section
  do k=1,nf
   open(newunit=iun,file=casn(k),action='write')
   write(iun,*)casn(k),'ID, Hr to Hr24'
   write(iun,'(I8,4A)')size(emis,dim=1),",",current_date
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
    print *,"*****  DONE Temporal Area *****"
    deallocate(idcel,id5,idcel2,idsm,emiA,emis)
end subroutine area_anual_storing
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
!>   @date  05/24/2023
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2023
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
  allocate(emis(j,nf,nh))
   emis=0
   evoc=0
  deallocate(idcel3)
end subroutine area_grids_count

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
