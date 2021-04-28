!>  @brief For programs agg_a.F90 agg_m.F90 and agg_p.F90. VOC emisions speciation
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module voc_split
  !>number of hours in a day
  integer,parameter :: nh=24        ;!> max number species in profile 0 (292)
  integer,parameter :: nspecies=292 ;!> max number chemical categories in mechanis
  integer,parameter :: ncat=40      ;!>number the clasess in profiles_spc.txt
  integer :: nclass    ;!> line number in area file TCOV_2016.txt
  integer :: lines_in_file       ;!> model type 0 WRF 1 CHIMERE
  integer :: model     ;!> Emission layer from point source
  integer,allocatable :: capa(:,:) ;!> mission layer from point source to be written
  integer,allocatable :: layer(:,:) ;!> grid id from emissions file
  integer,allocatable ::grid(:) ;!> different grid id from emissions file
  integer,allocatable ::grid2(:) ;!> number of chemical species in profile j
  integer,allocatable :: isp(:)   ;!> profile ID from file scc-profiles
  integer,allocatable ::profile(:);!> profile ID from file scc-profiles
  integer,allocatable ::prof2(:) ;!>  emissions en TCOV file grid , nh
  real,allocatable :: ea(:,:)    ;!> emissions id cel, category, hours
  real,allocatable :: emis(:,:,:)  ;!> aggregation factor by size(prof2), species, nclass
  real,allocatable :: fclass(:,:,:);!> SCC from emissions file
  character (len=10), allocatable:: iscc(:) ;!> !> Mechanism class name used for a file name
  character(len=4),allocatable::cname(:) ;!> Type of day (lun, mar, mie, ..., dom)
  character(len=3) ::cdia        ;!> Current date
  character (len=19) :: current_date
  !> Photochemical mecanism in profile_MECHA.csv file
  character (len=19) ::cprof
  common /date/ model,lines_in_file,current_date,cdia,cprof
contains
!  _
! | | ___  ___
! | |/ _ \/ _ \
! | |  __/  __/
! |_|\___|\___|
!>  @brief Reads area emissions and SCC profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param isource type of emissions source 1=area 2=mobile 3=point
subroutine lee_voc(isource)
  use master
  implicit none
  integer,intent(IN) :: isource
  integer :: i,j,id,idum,l
  real,dimension(ncat)::fagg ! aggregation factor for 40 species
  character(len=10)::isccf ! SCC prom profile file
  character(len=10)::cdum
  character(len=27):: fname
  print *,"Inicia lectura"
  SELECT CASE (isource)
  CASE(1)
    fname='TAVOC_2016.csv'
  CASE(2)
    fname='TMCOV_2016.csv'
  CASE(3)
    fname='T_ANNVOC.csv'
  CASE DEFAULT
    STOP "Error not identifcable type source "
  END SELECT
    print *,fname
  open (unit=10,file=fname,status='old',action='read')
  read(10,*) cdum                   ! header 1
  read(10,*) lines_in_file,current_date,cdia  ! header 2
  i=0
  do
    read(10,*,end=100) cdum
    i=i+1
  end do
100 continue
  write(6,*)'  Number of rows in ',fname,i
  lines_in_file=i
  allocate(grid(lines_in_file),iscc(lines_in_file),ea(lines_in_file,nh),profile(lines_in_file))
  if(isource.eq.3) allocate(capa(lines_in_file,2))
  rewind(10)
  read (10,*) cdum  ! header 1
  read (10,*) cdum  ! header 2
  if(isource.eq.3)then
  do i=1,lines_in_file
       read (10,*)iscc(i),grid(i),capa(i,1),(ea(i,j),j=1,nh),capa(i,2)
  end do
  else
    do i=1,lines_in_file
      read (10,*)grid(i),iscc(i),(ea(i,j),j=1,nh)
    end do
  end if
  close(10)
! READING  and finding profiles
  open(unit=15,file='../chem/scc-profiles.txt',status='old',action='read')
  do
    read(15,*,END=200) isccf,cdum,j
!dir$ loop count min(512)
    do i=1,lines_in_file
     if (isccf.eq.iscc(i)) profile(i)=j
    end do
  end do
200 continue
  close(15)
  !print '(15I5)',(profile(i),i=1,lines_in_file)
  print *,'  Start count'
  call count(isource)  ! counts the number of different profiles
  print *,'  Finish count'
! READING  and findign speciation for profiles
  print *,'  Speciation for Mechanism: ',trim(cprof),"->",mecha
  open(unit=16,file='../chem/profile_'//trim(mecha)//'.csv',status='old',action='read')
  read(16,*)cdum,cprof
  read(16,*) nclass
  print *,'  Speciation for Mechanism: ',trim(cprof),"->",mecha
  if(nclass.gt.ncat) stop "Change size dimension ncat >40"
  rewind(16)
  allocate(cname(nclass))
  read(16,*)cdum
  read(16,*) nclass,cdum,(cname(i),i=1,nclass)
  !print *,nclass
  !print '(<nclass>(A,x))',cname
  j=0
  isp=0
  do
    read(16,*,end=300,ERR=300)id
    do i=1,size(prof2)
            if(id.eq.prof2(i)) isp(i)=isp(i)+1
    end do
    j=j+1
  end do
300 continue
  !print *,"isp,maxval",isp,maxval(isp)
  allocate(fclass(size(prof2),maxval(isp),nclass))
  rewind(16)
  read(16,*)cdum  ! Header 1
  read(16,*)cdum  ! Header 2
  isp=0
  do
    read(16,*,end=400,ERR=400)id,idum,(fagg(i),i=1,nclass)
    do i=1,size(prof2)
    if(id.eq.prof2(i)) then
      isp(i)=isp(i)+1
      do l=1,nclass
        fclass(i,isp(i),l)=fagg(l)
      end do
    end if
    end do
  end do
400 continue
!  i=1
!  do j=1,isp(i)
!    print '(2i,<nclass>F)',prof2(i),j,(fclass(i,j,l),l=1,nclass)
!  end do

  close(16)
print *,'Fin lectura'
end subroutine lee_voc

!>  @brief agreggates VOC species in photechemical mechanism classes
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param isource type of emissions source 1=area 2=mobile 3=point
subroutine voc_agregation(isource)
  implicit none
  integer, INTENT(IN)  ::isource
  integer i,j,k,l,ih,m
  integer ns,ng,ii
  print *,'Starting computations'
  allocate (emis(size(grid2),nclass,nh))
  emis=0
  ng =size(grid2)
  ns =size(prof2) ! Only for Point sources
  if(isource.eq.3) then
!$omp parallel do private(k,i,j,l,ih)
    do ii=1,lines_in_file
      do k=1,ng    ! grid
      if(grid(ii).eq.grid2(k).and. capa(ii,1).eq.layer(k,1)) then
        do i=1,ns  !profiles
          if(prof2(i).eq.profile(ii)) then
            do j=1,isp(i)  ! species in profile isp(i)
              do l=1,nclass ! mechanism classes
                do ih=1,nh   ! hours
                if(fclass(i,j,l).ne.0) emis(k,l,ih)=emis(k,l,ih)+fclass(i,j,l)*ea(ii,ih)
                end do
              end do
            end do
          end if
        end do
      end if
      end do
    end do
!$omp end parallel do
  else
!$omp parallel do private(k,i,j,l,ih)
    do ii=1,lines_in_file
      do k=1,ng    ! grid
        if(grid(ii).eq.grid2(k)) then
          do i=1,ns  !profiles
            if(prof2(i).eq.profile(ii)) then
              do j=1,isp(i)  ! species in profile isp(i)
                do l=1,nclass ! mechanism classes
                  do ih=1,nh   ! hours
                  if(fclass(i,j,l).ne.0) emis(k,l,ih)=emis(k,l,ih)+fclass(i,j,l)*ea(ii,ih)
                  end do
                end do
              end do
            end if
          end do
        end if
      end do
    end do
!$omp end parallel do
  end if
end subroutine voc_agregation
!                            _
!   __ _ _   _  __ _ _ __ __| | __ _
!  / _` | | | |/ _` | '__/ _` |/ _` |
! | (_| | |_| | (_| | | | (_| | (_| |
!  \__, |\__,_|\__,_|_|  \__,_|\__,_|
!  |___/
!>  @brief Stores mechanism classes
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param isource type of emissions source 1=area 2=mobile 3=point
subroutine guarda_voc(isource)
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
        STOP "Error not identificable type source "
  END SELECT
  print *,maxval(emis),'Valor maximo'
!$omp parallel do private(iun,k,j,i,suma,fname)
  do j=1,size(emis,dim=2)
    suma=0
    fname=trim(cprof)//'_'//trim(cname(j))//cfuente
    open(newunit=iun,file=fname,action='write')
    write(iun,'(4A)')cname(j),',',trim(cprof),', Emissions'
    write(iun,*) size(emis,dim=1),',',current_date,',',cdia
    do k=1,size(emis,dim=1)
      if(isource.eq.3) then
        write(iun,200)grid2(k),layer(k,1),(emis(k,j,i),i=1,size(emis,dim=3)),layer(k,2)
      else
        write(iun,701)grid2(k),(emis(k,j,i),i=1,size(emis,dim=3))
      end if
      do i=1,size(emis,dim=3)
          suma=suma+emis(k,j,i)
      end do
    end do
    close(iun)
    write (6,*)cname(j),',',suma
  end do
!$omp end parallel do
    print *,"*****  DONE ",csource," SPECIATION *****"
200 format (I8,",",I3,",",24(ES12.5,","),I3)
701 format(I7,",",24(ES12.5,","))
end subroutine guarda_voc
!                        _
!   ___ ___  _   _ _ __ | |_
!  / __/ _ \| | | | '_ \| __|
! | (_| (_) | |_| | | | | |_
!  \___\___/ \__,_|_| |_|\__|
!
!>  @brief Counts the number of differnt profiles
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param isource type of emissions source 1=area 2=mobile 3=point
subroutine count(isource)
  implicit none
  integer, INTENT(IN)  ::isource
  integer i,j,nn,iun
  logical,allocatable::xl(:)
  nn=size(profile)
  allocate(xl(nn))
  xl=.true.
!$omp parallel do private(j)
  do i=1,nn-1
    do j=i+1,nn
      if(profile(j).eq.profile(i).and.xl(j))then
        xl(j)=.false.
        exit
      end if
    end do
  end do
!$omp end parallel do
  j=0
  do i=1,nn
    if(xl(i)) j=j+1
  end do
  allocate(prof2(j),isp(j))
  j=0
  do i=1,nn
   if(xl(i)) then
     j=j+1
     prof2(j)=profile(i)
   end if
  end do
!
  print *,'   Number different profiles',j !,prof2
!
  if(isource.eq.1) then
    open(newunit=iun,file='index.csv',status='old')
    read(iun,*)j
    allocate(grid2(j))
    do i=1,j
      read(iun,*)grid2(i)
    end do
    close(iun)
  end if
  if(isource.eq.2) then
    deallocate(xl)
    allocate(xl(size(iscc)))
    xl=.true.
!$omp parallel do private(j)
    do i=1,lines_in_file-1
      do j=i+1,lines_in_file
        if(grid(j).eq.grid(i).and.xl(j)) xl(j)=.false.
      end do
    end do
!$omp end parallel do
    j=0
    do i=1,lines_in_file
      if(xl(i)) j=j+1
    end do
    allocate(grid2(j))
    j=0
    do i=1,lines_in_file
      if(xl(i)) then
        j=j+1
        grid2(j)=grid(i)
      end if
    end do
  end if
  if(isource.eq.3) then
    deallocate(xl)
    allocate(xl(size(iscc)))
    xl=.true.
!$omp parallel do private(j)
    do i=1,lines_in_file-1
      do j=i+1,lines_in_file
        if(grid(j).eq.grid(i).and.xl(j).and. capa(i,1).eq.capa(j,1)) xl(j)=.false.
      end do
    end do
!$omp end parallel do
    j=0
    do i=1,lines_in_file
      if(xl(i)) j=j+1
    end do
    allocate(grid2(j),layer(j,2))
    j=0
    do i=1,lines_in_file
      if(xl(i)) then
        j=j+1
        grid2(j)=grid(i)
        layer(j,1)=capa(i,1)
        layer(j,2)=capa(i,2)
      end if
    end do
  end if
print *,'   Number of different cells',j,size(grid2)
deallocate(xl)
end subroutine count
end module voc_split
