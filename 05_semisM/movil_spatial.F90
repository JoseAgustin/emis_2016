!  ifort -O3 -axAVX -o MSpatial.exe movil_spatial.F90
!
!  Creado por Jose Agustin Garcia Reynoso el 1/11/2017
!
!
! Proposito
!          Distribución espacial de las emisiones de fuentes moviles
!          Program that reads MOVES2015 and spatial allocation
!
!   Usa emiss_2015,csv, fracc_2008.csv, salida.csv, gri_movil.csv
!
!  Cambios
!       Se incluye Carbono Negro
!       18/11/2017  Se incluyen GSO4, OTHER, POA
!> @brief For movil_spatial.F90 program. Mobile emissions spatial distribution
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module mobile_spatial_mod ;!> Number of lines in salida.csv
integer nl   ;!> Number of lines in gri_movil.csv
integer nl2  ;!> Number of pollutants
integer,parameter :: npol=11 ;!> Number of States
integer,parameter :: nstates=32 ;!>Number of pollutant name fraction
integer,parameter :: fracp=10 ;!>State ID
integer,allocatable :: iest(:)  ;!>ID State_municipality
integer,allocatable :: cventmun(:)
!>State Mun code in emis and grid files
integer,allocatable :: id(:) ;!>State Mun code in emis and grid files
integer,allocatable :: id2(:) ;!>gridcode in gri_pob
integer,allocatable ::grid(:) ;!>gridcode no duplicates
integer,allocatable ::grid2(:) ;!>time lag emis and grid files
integer,allocatable :: im(:)    ;!>time lag grid files
integer,allocatable ::  im2(:)
!>emid edo mun id
integer*8,allocatable :: emid(:) ;!>SCC from emissions
character (len=10),allocatable::iscc(:) ;!>scc code in emis and subset of different scc codes.
character (len=10),allocatable::jscc(:) ;!>pollutant name
character (len= 5),dimension(npol) :: pol ; !>pollutant name fraction file
character (len= 5),dimension(fracp) :: polf
!>ei emission in emissfile (nl dimension)
real,allocatable:: ei(:,:) ;!>uf, rf urban population fraction
real,allocatable:: uf(:)   ;!>rf rural population fraction
real,allocatable:: rf(:)   ;!>pemi emission in grid cell,pollutan,scc category
real,allocatable:: pemi(:,:,:) ;!>
real,allocatable:: frac(:,:)
common /vari/ nl,nl2,pol
end module mobile_spatial_mod
!> @brief Spatial distribution of emissions from mobile sources
!> @par
!> Allocates the municipatily emission in to a grid
!> the grid is obtained from specific geographical area seting by zona variable in namelist_emis.nml
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
program movil_spatial
use mobile_spatial_mod

    call mobile_emiss_reading

    call mobile_spatial_locating

    call mobile_spatial_storing

contains
!> @brief Stores mobile sources emissions in a grid per pollutant file
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
subroutine mobile_spatial_storing
    integer i,j,k,iun
	character(len=15) ::name
	do i=1,npol
	name='M_'//trim(pol(i))//'.csv'
	open(newunit=iun,file=name)
	write(iun,*)'GRIDCODE emissions in g per year'
	write(iun,210)size(jscc),(jscc(j),j=1,size(jscc))
	do k=1,size(grid2)
	  write(iun,220) grid2(k),(1000000*pemi(k,i,j),j=1,size(jscc)),im2(k)
	end do
	close(iun)
	end do
    print *,"+++++   DONE SPATIAL MOVIL +++++"
#ifndef PGI
210 format(i8,",",<size(jscc)>(A11,","))
220 format(i8,",",<size(jscc)>(ES12.4,","),I2)
#else
210 format(i8,",",30(A11,","))
220 format(i8,",",30(ES12.4,","),I2)
#endif
end subroutine mobile_spatial_storing
!> @brief Allocates municipality sources emissions in a grid
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
subroutine mobile_spatial_locating
implicit none
  integer i,j,ii,l,k
	print *,' Start doing spatial allocation'
	print *,(pol(i),i=1,npol)
	call count  ! counts grids and scc different values
	print *,'end count'
	ii=1
!$omp parallel do private(k,i,j,l,ii)
   do k=1, size(grid2)
	do i=1,nl2 ! gri_movil
       if(grid2(k).eq.grid(i))then
		do j=1,nl ! F_movil
		  if (id2(i).eq.emid(j)) then
			  do l=1,size(jscc)
			   if(iscc(j).eq.jscc(l))then
                 do ii=1,npol
                pemi(k,ii,l)=pemi(k,ii,l) &
				+(uf(i)+rf(i))*ei(j,ii)
                 end do !ii
			   end if!scc
			  end do! l
           end if!  id2
		end do!j
      end if! grid
	end do !i
end do! k
!$omp end parallel do
end subroutine mobile_spatial_locating
!
!> @brief Reads mobile emission file from MOVES emiss_2016.csv
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
subroutine mobile_emiss_reading
	implicit none
	integer:: i,j,iedo
    integer:: anio,cint
    character(len=1):: st
	character(len=10):: cdum
	print *,'Starts reading emissions file in Mg/yr'
	open(10,file='emiss_2016.csv',status='old',action='read')
	read(10,*) cdum,cdum,(pol(i),i=1,npol) !read header
	i=0
	do
	 read(10,*,END=100)cdum
	 i=1+i
	end do
100 continue
    print *,'number of lines',i
	rewind(10)
	read(10,'(A)') cdum ! read header
	allocate(emid(i),iscc(i),ei(i,npol))
	nl=i
	do i=1,nl
      read(10,*,ERR=140)emid(i),iscc(i),(ei(i,j),j=1,npol)
!    print *,emid(i),iscc(i),ei(i,7)
	end do
	print *,'End reading file emiss_2016.csv '
	close(10)
!
	open(10,file='../03_movilspatial/gri_movil.csv',status='old',action='read')
	read(10,'(A)') cdum !read header line 1
	read(10,'(A)') cdum !read header line 2
	i=0
	do
	 read(10,*,END=110)cdum
	 i=1+i
	end do
110 continue
    !print *,'number of lines',i
	rewind(10)
	read(10,'(A)') cdum !read header line 1
	read(10,'(A)') cdum !read header line 2
	allocate(grid(i),id2(i),uf(i),rf(i),im(i))
	nl2=i
  im=6
  do i=1,nl2
	read(10,*,ERR=160) grid(i),id2(i),uf(i),rf(i)
    iedo=int(id2(i)/1000)
    if(iedo.eq.2.or.iedo.eq.3) im(i)=8
    if(iedo.eq.8.or.iedo.eq.18.and.iedo.eq.25.and.iedo.eq.26)im(i)=7
	!print *,i,grid(i),id2(i),uf(i),rf(i)
	end do
	print *,'End reading file gri_movil.csv',nl2
	close(10)
!
!	Se considera que el 10 % va en carretera
!   y el 90% en ciudad
!
	uf=uf*0.90
	rf=rf*0.10
	return
140 print *,"Error in reading file emiss_2016.csv",i
    stop
160 print *,"Error in reading file gri_movil.csv",i
end subroutine mobile_emiss_reading
!
!> @brief Counting the number of different GRDICODE cells
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
subroutine count
  integer i,j
  logical,allocatable::xl(:)
  allocate(xl(size(grid)))
  xl=.true.
!$omp parallel do private(j)
  do i=1,nl2-1
   do j=i+1,nl2
   if(grid(j).eq.grid(i).and.xl(j)) then
    xl(j)=.false.
    exit
    end if
   end do
  end do
!$omp end parallel do
  j=0
  do i=1,nl2
    if(xl(i)) j=j+1
  end do
  allocate(grid2(j),im2(j))
  j=0
  do i=1,nl2
    if(xl(i)) then
	j=j+1
	grid2(j)=grid(i)
    im2(j)=im(i)
	end if
  end do

  print *,'Number of different cells',j
  deallocate(xl)
!
! From emissions file F_moviles.csv
  allocate(xl(size(iscc)))

  xl=.true.
!$omp parallel do private(i)
  do ii=1,nl-1
    do i=ii+1,nl
    if(iscc(ii).eq.iscc(i).and.xl(i)) xl(i)=.false.
	end do
  end do
!$omp end parallel do
  ii=0
  do i=1,nl
   if(xl(i)) then
   ii=ii+1
   end if
  end do
  !print *,'different SCC ',ii
  allocate(jscc(ii))
  allocate(pemi(j,npol,ii))
  pemi=0
   ii=0
    do i=1,nl
     if(xl(i)) then
	 ii=ii+1
	 jscc(ii)=iscc(i)
	 end if
  end do
!  print *,(jscc(i),i=1,ii)
  deallocate(xl)
end subroutine count

end program
