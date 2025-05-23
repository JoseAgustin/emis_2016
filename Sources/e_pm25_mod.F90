!>  @brief For programs pm25_speci_a.F90, pm25_speci_m.F90 and pm25_speci_p.F90. PM2.5  emisions speciation
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module PM25_speciation_mod ;!>number of hours in a day
integer :: nh     ;!> number the clasess in profiles_spc.txt
integer :: nclass ;!> line number in area file TPM252016.txt
integer lfa  ;!>   grid id from emissions file
integer,allocatable ::grid(:)   ;!> different grid id from emissions file
integer,allocatable ::grid2(:)  ;!> number of chemical species in profile j
integer,allocatable :: isp(:)   ;!> profile IDs from file scc-profiles
integer,allocatable :: profile(:) ;!> profile IDs wtih no duplicates
integer,allocatable :: prof2(:);!> level of the emission
integer,allocatable :: capa(:,:) ;!> SCC from emissions file
!integer*8, allocatable:: iscc(:) ;!> SCC from emissions file character
character(len=10),allocatable:: iscc(:);!>emissions en TPM25 file grid , nh
real,allocatable :: ea(:,:)      ;!> emissions id cel, category, hours
real,allocatable :: emis(:,:,:)  ;!>aggregation factor by size(prof2), nclass
real,allocatable :: fclass(:,:)  ;!> Category name used for file name
character(len=4),allocatable::cname(:) ;!> Character day type
character(len=3) ::cdia ;!> Current date
character (len=19) :: current_date


parameter (nspecies=5,nh=24)

common /date/ current_date,cdia
contains
!            _            _
!   ___ __ _| | ___ _   _| | ___  ___
!  / __/ _` | |/ __| | | | |/ _ \/ __|
! | (_| (_| | | (__| |_| | | (_) \__ \
!  \___\__,_|_|\___|\__,_|_|\___/|___/
!>  @brief Split PM2.5 emissions in categories clasess
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine calculos
implicit none
  integer i,j,k,l,ih
  integer ns,ng,ii
  print *,'Starting computations'
  allocate (emis(size(grid2),nclass,nh))
  emis=0
  ng =size(grid2)
  ns =size(prof2)
!$omp parallel do private(i,j,k,l,ih)
  do ii=1,lfa
    do k=1,ng    ! grid
    if(grid(ii).eq.grid2(k)) then
      do i=1,ns  !profiles
      if(prof2(i).eq.profile(ii)) then
          do l=1,nclass ! PM2.5 classes
           do ih=1,nh   ! hours
  if(fclass(i,l).ne.0) emis(k,l,ih)=emis(k,l,ih)+fclass(i,l)*ea(ii,ih)
           end do
          end do
      end if
      end do
    end if
    end do
  end do
!$omp end parallel do
end subroutine calculos
!                            _
!   __ _ _   _  __ _ _ __ __| | __ _
!  / _` | | | |/ _` | '__/ _` |/ _` |
! | (_| | |_| | (_| | | | (_| | (_| |
!  \__, |\__,_|\__,_|_|  \__,_|\__,_|
!  |___/
!>  @brief Stores PM2.5 emissions in files by categories clasess
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param isource type of emissions source 1=area 2=mobile 3=point
subroutine guarda(isource)
implicit none
  integer, INTENT(IN)  ::isource
  integer i,j,k,iun
  real suma
  character(len=20)::fname
  character(len=6 )::cfuente,csource
  SELECT CASE (isource)
    CASE(1)
    cfuente='_A.txt'
    csource=" AREA "
    CASE(2)
    cfuente='_M.txt'
    csource="MOBILE"
    CASE(3)
    cfuente='_P.txt'
    csource="POINT"
    CASE DEFAULT
          STOP "Error not identifcable type source "
    END SELECT
  print *,maxval(emis),'Valor maximo'
!$omp parallel do private(iun,k,j,i,suma,fname)
  do j=1,size(emis,dim=2)
    suma=0.
    fname=trim(cname(j))//cfuente
    open(newunit=iun,file=fname,action='write')
    write(iun,'(A,A)')cname(j), 'Emissions'
    write(iun,*) size(grid2),current_date,', ',cdia
    if(isource.eq.3) then
      do k=1,size(emis,dim=1)
      if(emis(k,j,1).ne.0 .and. emis(k,j,12).ne.0 .and. emis(k,j,23).ne.0 )then
      write(iun,200)grid2(k),capa(k,1),(emis(k,j,i),i=1,size(emis,dim=3)),capa(k,2)
        do i=1,size(emis,dim=3)
          suma=suma+emis(k,j,i)
        end do
      end if
      end do
    else
      do k=1,size(grid2)
        write(iun,701)grid2(k),(emis(k,j,i),i=1,size(emis,dim=3))
        do i=1,size(emis,dim=3)
          suma=suma+emis(k,j,i)
        end do
      end do
    end if
    close(iun)
    write(6,*)cname(j),",",suma
  end do
!$omp end parallel do
  print *,"***** DONE PM2.5",csource," SPECIATION *****"
200 format(I7,x,I3,x,24(ES11.4,x),I3)
701 format(I7,24(",",ES11.4))
end subroutine guarda
!                        _   ____  __  __ ____  ____
!   ___ ___  _   _ _ __ | |_|  _ \|  \/  |___ \| ___|
!  / __/ _ \| | | | '_ \| __| |_) | |\/| | __) |___ \
! | (_| (_) | |_| | | | | |_|  __/| |  | |/ __/ ___) |
!  \___\___/ \__,_|_| |_|\__|_|   |_|  |_|_____|____/
!
!>  @brief Identifies the different PM2.5 profiles and grid cells
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  20/12/2022
!>   @version  3.5
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
subroutine countPM25
  use SortUnique
  integer(kind=4), allocatable  :: lista(:)
  integer i,j,nn
!
    lista=profile
    prof2 =Unique(lista)
    deallocate(lista)
    j=size(prof2)
    allocate(isp(j))
!
 print *,'    Number different profiles',j !,prof2
!
    lista=grid
    grid2=Unique(lista)
  deallocate(lista)
  print *,'    Number of different cells',size(grid2)
end subroutine countPM25
!  _
! | | ___  ___
! | |/ _ \/ _ \
! | |  __/  __/
! |_|\___|\___|
!>  @brief Reads emissions PM2.5 and speciation profiles based on SCC
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param isource type of emissions source 1=area 2=mobile 3=point
subroutine lee(isource)
  implicit none
  integer, INTENT(IN)  ::isource
  integer :: i,j,id,idun,l
  real,dimension(5) :: fagg ! aggregation factor for 5 pm2.5 species
  character(len=10) :: cdum
  character(len=10) :: isccf
  character(len=27) :: fname
  character(len=20) :: FMT
  logical ::lfil
  SELECT CASE (isource)
  CASE(1)
  fname='TAPM2_2016.csv'
  CASE(2)
  fname='TMPM2_2016.csv'
  CASE(3)
  fname='T_ANNPM25.csv'
  CASE DEFAULT
        STOP "Error not identificable type source "
  END SELECT
  print *, 'Reading : ',trim(fname)
  open (unit=10,file=fname,status='old',action='read')
  read(10,*) cdum  ! header
  read(10,*) lfa,current_date,cdia  ! header
  i=0
  do
  read(10,*,end=100) cdum
  i=i+1
  end do
100 continue
  print *,'Number of lines=',i,lfa
  lfa=i
  allocate(grid(lfa),iscc(lfa),ea(lfa,nh),profile(lfa))
  if(isource.eq.3) allocate(capa(lfa,2))
  rewind(10)
  read (10,*) cdum  ! header 1
  read (10,*) cdum  ! header 2
  if(isource.eq.3)then
    do i=1,lfa
       read (10,*)iscc(i),grid(i),capa(i,1),(ea(i,j),j=1,nh),capa(i,2)
    end do
  else
    do i=1,lfa
      read (10,*)grid(i),iscc(i),(ea(i,j),j=1,nh)
    end do
  end if
  close(10)
! READING  and findign profiles
  fname='scc-profile_pm25.csv'
  print *, 'Reading : ',trim(fname)
  open(unit=15,file="../chem/"//fname,status='old',action='read')
  read (15,*) cdum !header
  do
    read(15,*,END=200) isccf,cdum,j
    do i=1,lfa
     if (isccf.eq.iscc(i)) profile(i)=j
    end do
  end do
200 continue
  do i=1,60
   if(profile(i).eq.0) print *,iscc(i),' ',i
  end do
  close(15)
  !print '(15I5)',(profile(i),i=1,lfa)
 !323 format(2i10,60F10.4)
  print *,'Start countPM2.5'
  call countPM25  ! counts the number of different profiles
  print *,'Finishing count'
! READING  and finding speciation for profiles
  fname='pm25_profiles.csv'
  print *, 'Reading : ',trim(fname)
  open(unit=16,file="../chem/"//fname,status='old',action='read')
  read(16,*)cdum
  read(16,*) nclass
  if(nclass.gt.30) stop "Change size in fagg dimension"
  rewind(16)
  allocate(cname(nclass))
  read(16,*)cdum
  read(16,*) nclass,(cname(i),i=1,nclass)
  !print *,nclass,FMT
  !print '(<nclass>(A,x))',cname
  allocate(fclass(size(prof2),nclass))
  do
    read(16,*,end=300,ERR=300)id,(fagg(j),j=1,nclass)
    do i=1,size(prof2)
    if(id.eq.prof2(i)) then
      do l=1,nclass
        fclass(i,l)=fagg(l)
      end do
    end if
    end do
  end do
300 continue
  write(FMT,"('(2I10,',I0,'F10.4)')")nclass
  do i=1,size(prof2)
    write(6,FMT) prof2(i),i,(fclass(i,l),l=1,nclass)
  end do
  close(16)
  return
end subroutine lee
end module PM25_speciation_mod
