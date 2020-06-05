!
!	agg_m.f90
!	
!
!  Creado por Jose Agustin Garcia Reynoso el 31/05/12.
!
! Proposito:
!               Especiacion y agreacion en diferenes especies y
!               clases de un mecanismo especifico
!
!				to do speciation anad aggregation to the different
!               species and Classes for an specific mechanism
!
!  compile: ifort -O2 -axAVX2 agg_m.f90 -o spm.exe
!
!   9/04/2020      namelist general
!
module var_agg
integer,parameter :: nh=24     !number of hours in a day
integer,parameter :: nspecies=292 ! max number species in profile 0 (292)
integer,parameter :: ncat=40 ! max number chemical species
integer :: nclass !number of clasess in profiles_spc.txt
integer lfa  ! line number in area file TCOV_2016.txt
integer,allocatable ::grid(:)   ! grid id from emissions file
integer,allocatable ::grid2(:)   ! different grid id from emissions file
integer,allocatable :: isp(:)   ! number of chemical species in profile j
integer,allocatable ::profile(:),prof2(:) ! profile ID from file scc-profiles
real,allocatable :: ea(:,:)      ! emissions in TCOV file grid , nh
real,allocatable :: emis(:,:,:)  ! emissions id cel, category, hours
real,allocatable :: fclass(:,:,:)! aggregation factor by size(prof2), species, nclass
character (len=10), allocatable:: iscc(:) !SCC from emissions file
character(len=4),allocatable::cname(:)
character(len=3) :: cday
character(len=7) ::mecha
character (len=19) :: current_date,cprof

common /date/ current_date,cday,cprof,mecha
end module var_agg

program agg_m
use var_agg

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
subroutine lee
    implicit none
    integer :: i,j,id,idum,l
    integer:: iunit
    real,dimension(ncat)::fagg ! aggregation factor for 34 species
    character(len=10)::isccf ! SCC prom profile file
    character(len=10)::cdum
    logical :: lfil
    print *,"Inicia lectura"
    print *,"../06_temisM/TMCOV_2016.csv"
    open (newunit=iunit,file='../06_temisM/TMCOV_2016.csv',status='old',action='read')
    read(iunit,*) cdum  ! header
    print *, cdum
    read(iunit,*) lfa,current_date,cday  ! header
    print *,lfa,current_date,cday
    i=0
    do
        read(iunit,*,end=100) cdum
        i=i+1
    end do
100 continue
	print *,'Number in TMCOV_2016',i
	lfa=i
	allocate(grid(lfa),iscc(lfa),ea(lfa,nh),profile(lfa))
	rewind(iunit)
	read (iunit,*) cdum  ! header 1
	read (iunit,*) cdum  ! header 2
	do i=1,lfa
        read (iunit,*)grid(i),iscc(i),(ea(i,j),j=1,nh)
	end do
	close(iunit)
! READING  and findign profiles
	open(newunit=iunit,file='scc-profiles.txt',status='old',action='read')
	do
		read(iunit,*,END=200) isccf,cdum,j
		do i=1,lfa
		 if (isccf.eq.iscc(i)) profile(i)=j
		end do
	end do
200 continue
    close(iunit)
    !print '(15I5)',(profile(i),i=1,lfa)
    print *,'Start count'
    call count  ! counts the number of different profiles
    print *,'Finishing count'
    ! READING  and findign speciation for profiles
    open(unit=16,file='profile_'//trim(mecha)//'.csv',status='old',action='read')
    read(16,*)cdum,cprof
    read(16,*) nclass
    print *,'Speciation for Mechanism: ',trim(cprof),"->",mecha
    if(nclass.gt.ncat) stop "Change size in fagg dimension ncat"
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
    !print *,isp,maxval(isp)
    allocate(fclass(size(prof2),maxval(isp),nclass))
    rewind(16)
    read(16,*)cdum  ! Header 1
    read(16,*)cdum	! Header 2
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
    print *,'done reading profiles'
!	i=1
!	do j=1,isp(i)			
!		print '(2i,<nclass>F)',prof2(i),j,(fclass(i,j,l),l=1,nclass)
!	end do

	close(16)
      print *,'done lee'
end subroutine lee
!            _            _
!   ___ __ _| | ___ _   _| | ___  ___
!  / __/ _` | |/ __| | | | |/ _ \/ __|
! | (_| (_| | | (__| |_| | | (_) \__ \
!  \___\__,_|_|\___|\__,_|_|\___/|___/
!
subroutine calculos
	implicit none
	integer i,j,k,l,ih
	integer ns,ng,ii
	print *,'Starting computations'
	allocate (emis(size(grid2),nclass,nh))
	emis=0
	ng =size(grid2)
	ns =size(prof2)
!$omp parallel do private(k,i,j,l,ih)
	do ii=1,lfa
	  do k=1,ng		! grid
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
end subroutine calculos
!                            _
!   __ _ _   _  __ _ _ __ __| | __ _
!  / _` | | | |/ _` | '__/ _` |/ _` |
! | (_| | |_| | (_| | | | (_| | (_| |
!  \__, |\__,_|\__,_|_|  \__,_|\__,_|
!  |___/
!
subroutine guarda
	implicit none
	integer i,j,k,iun
   real suma
	character(len=20)::fname
	print *,maxval(emis),'Valor maximo'
!$omp parallel do private(iun,k,j,i,suma,fname)
	do j=1,size(emis,dim=2)
    suma=0
	fname=trim(cprof)//'_'//trim(cname(j))//'_M.txt'
	open(newunit=iun,file=fname,action='write')
	write(iun,'(4A)')cname(j),',',trim(cprof),', Emissions'
	write(iun,*) size(emis,dim=1),',',current_date,',',cday
		do k=1,size(emis,dim=1)
			write(iun,701)grid2(k),(emis(k,j,i),i=1,size(emis,dim=3))
        do i=1,size(emis,dim=3)
            suma=suma+emis(k,j,i)
        end do
		end do
	close(iun)
    write (6,*)cname(j),',',suma
	end do
!$omp end parallel do
    print *,"*****  DONE MOVIL SPECIATION *****"
701 format(I7,",",24(ES12.5,","))
end subroutine guarda
!                        _
!   ___ ___  _   _ _ __ | |_
!  / __/ _ \| | | | '_ \| __|
! | (_| (_) | |_| | | | | |_
!  \___\___/ \__,_|_| |_|\__|
!
subroutine count
  integer i,j,nn
  logical,allocatable::xl(:)
  nn=size(profile)
  allocate(xl(nn))
  xl=.true.
!$omp parallel do private(j)
  do i=1,nn-1
    do j=i+1,nn
      if(profile(j).eq.profile(i).and.xl(j)) then
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
  print *,'Number different profiles',j !,prof2
!
  deallocate(xl)
  allocate(xl(size(iscc)))

  xl=.true.
!$omp parallel do private(j)
  do i=1,lfa-1
    do j=i+1,lfa
      if(grid(j).eq.grid(i).and.xl(j)) xl(j)=.false.
    end do
  end do
!$omp end parallel do
  j=0
  do i=1,lfa
    if(xl(i)) j=j+1
  end do
  allocate(grid2(j))
  j=0
  do i=1,lfa
    if(xl(i)) then
      j=j+1
      grid2(j)=grid(i)
    end if
  end do

  print *,'Number of different cells',j
  deallocate(xl)
end subroutine count
!  _                                          _ _     _
! | | ___  ___     _ __   __ _ _ __ ___   ___| (_)___| |_
! | |/ _ \/ _ \   | '_ \ / _` | '_ ` _ \ / _ \ | / __| __|
! | |  __/  __/   | | | | (_| | | | | | |  __/ | \__ \ |_
! |_|\___|\___|___|_| |_|\__,_|_| |_| |_|\___|_|_|___/\__|
!            |_____|
subroutine lee_namelist
    NAMELIST /chem_nml/ mecha
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
        READ (unit_nml , NML = chem_nml )
        !WRITE (6    , NML = chem_nml )
        close(unit_nml)
    else
        stop '***** No namelist_emis.nml in .. directory'
    end if
end subroutine lee_namelist
end program agg_m
