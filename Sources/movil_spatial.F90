!
!  Creado por Jose Agustin Garcia Reynoso el 1/11/2017
!
!
! Proposito
!          Distribución espacial de las emisiones de fuentes moviles
!          Program that reads MOVES2015 and spatial allocation
!
!   Usa emiss_2015,csv, fracc_2008.csv, salida.csv, gri_movil.csv
!  ifort -O3 -axAVX -o MSpatial.exe movil_spatial.F90
!
!  Cambios
!       Se incluye Carbono Negro
!       18/11/2017  Se incluyen GSO4, OTHER, POA
!> @brief For movil_spatial.F90 program. Mobile emissions spatial distribution
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
module mobile_spatial_mod
!> Number of lines in emissions file
integer nl   ;!> Number of lines in gri_movil.csv
integer nl2  ;!> Number of pollutants
integer,parameter :: npol=11 ;!> Number of States
integer,parameter :: nstates=32 ;!>State ID
integer,allocatable :: iest(:)
!>State Mun code in emis and grid files
integer,allocatable :: id(:) ;!>State Mun code in emis and grid files
integer,allocatable :: id2(:) ;!>gridcode in gri_pob
integer,allocatable ::grid(:) ;!>gridcode unique values array
integer,allocatable ::grid2(:) ;!>time zone emis and grid files
integer,allocatable :: im(:)   ;!>time zone grid file unique values array
integer,allocatable :: im2(:)
!>Emissions CVENMUN ID (ID_state*1000 + ID_municipality)
integer*8,allocatable :: emid(:) ;!>SCC from emission file
character (len=10),allocatable::iscc(:) ;!>SCC code in emis and subset of unique SCC codes.
character (len=10),allocatable::jscc(:) ;!>pollutant name from emissions file header
character (len= 5),dimension(npol) :: pol
!>ei emission in emissions file (nl dimension)
real,allocatable:: ei(:,:) ;!>streeF  streets area fraction
real,allocatable:: streeF(:)   ;!>highwF rural highways area fraction
real,allocatable:: highwF(:)   ;!>annual emission by grid cell, pollutant,scc category
real,allocatable:: pemi(:,:,:)

common /vari/ nl,nl2,pol
end module mobile_spatial_mod
!> @brief Spatial distribution of emissions from mobile sources
!> @par
!> Allocates the municipatily emission in to a grid
!> the grid is obtained from specific geographical area selected by _zona_ variable in namelist_emis.nml
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
program movil_spatial
use mobile_spatial_mod

    call mobile_emiss_reading

    call mobile_spatial_locating

    call mobile_spatial_storing

contains
!                  _     _ _
!  _ __ ___   ___ | |__ (_) | ___
! | '_ ` _ \ / _ \| '_ \| | |/ _ \
! | | | | | | (_) | |_) | | |  __/
! |_| |_| |_|\___/|_.__/|_|_|\___|____   _             _
!  ___ _ __   __ _| |_(_) __ _| |_____|_| |_ ___  _ __(_)_ __   __ _
! / __| '_ \ / _` | __| |/ _` | |   / __| __/ _ \| '__| | '_ \ / _` |
! \__ \ |_) | (_| | |_| | (_| | |   \__ \ || (_) | |  | | | | | (_| |
! |___/ .__/ \__,_|\__|_|\__,_|_|___|___/\__\___/|_|  |_|_| |_|\__, |
!     |_|                      |_____|                         |___/
!> @brief Stores mobile sources emissions in a grid per pollutant file
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
subroutine mobile_spatial_storing
    integer i,j,k,iun
	character(len=15) ::name
    character(len=24):: FMT
    character(len=34):: FFT
    write(FMT,"('(I8,',I0,'('','',A11))')")size(jscc)
    write(FFT,"('(I8,',I0,'('','',ES12.4),'','',I2)')")size(jscc)
	do i=1,npol
	name='M_'//trim(pol(i))//'.csv'
	open(newunit=iun,file=name)
	write(iun,*)'GRIDCODE, emissions in gram per year'
	write(iun,FMT)size(jscc),(jscc(j),j=1,size(jscc))
	do k=1,size(grid2)
	  write(iun,FFT) grid2(k),(1000000*pemi(k,i,j),j=1,size(jscc)),im2(k)
	end do
	close(iun)
	end do
    print *,"+++++   DONE SPATIAL MOVIL +++++"
    deallocate(grid2,jscc,im2,emid,iscc,ei)
    deallocate(grid,id2,streeF,highwF,im,pemi)
end subroutine mobile_spatial_storing
!                  _     _ _
!  _ __ ___   ___ | |__ (_) | ___
! | '_ ` _ \ / _ \| '_ \| | |/ _ \
! | | | | | | (_) | |_) | | |  __/
! |_| |_| |_|\___/|_.__/|_|_|\___|   _                 _   _
!  ___ _ __   __ _| |_(_) __ _| |   | | ___   ___ __ _| |_(_)_ __   __ _
! / __| '_ \ / _` | __| |/ _` | |   | |/ _ \ / __/ _` | __| | '_ \ / _` |
! \__ \ |_) | (_| | |_| | (_| | |   | | (_) | (_| (_| | |_| | | | | (_| |
! |___/ .__/ \__,_|\__|_|\__,_|_|___|_|\___/ \___\__,_|\__|_|_| |_|\__, |
!     |_|                      |_____|                             |___/
!> @brief Allocates municipality sources emissions in a grid
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
subroutine mobile_spatial_locating
implicit none
  integer i,j,ii,l,k
	print *,' Start doing spatial allocation'
	print *,(pol(i),i=1,npol)
	call count  ! counts grids and scc different values
	print *,'end count'
	ii=1
!!!!$omp parallel do private(i,j,l,ii)
   do k=1, size(grid2)
	do i=1,nl2 ! gri_movil
       if(grid2(k).eq.grid(i))then
		do j=1,nl ! emiss_movil
		  if (id2(i).eq.emid(j)) then
			  do l=1,size(jscc)
			   if(iscc(j).eq.jscc(l))then
                 do ii=1,npol
                pemi(k,ii,l)=pemi(k,ii,l) &
				+(streeF(i)+highwF(i))*ei(j,ii)
                 end do !ii
			   end if!scc
			  end do! l
           end if!  id2
		end do!j
      end if! grid
	end do !i
end do! k
!!!!$omp end parallel do
end subroutine mobile_spatial_locating
!                  _     _ _
!  _ __ ___   ___ | |__ (_) | ___
! | '_ ` _ \ / _ \| '_ \| | |/ _ \
! | | | | | | (_) | |_) | | |  __/
! |_| |_| |_|\___/|_.__/|_|_|\___|
!                 _                                _ _
!   ___ _ __ ___ (_)___ ___     _ __ ___  __ _  __| (_)_ __   __ _
!  / _ \ '_ ` _ \| / __/ __|   | '__/ _ \/ _` |/ _` | | '_ \ / _` |
! |  __/ | | | | | \__ \__ \   | | |  __/ (_| | (_| | | | | | (_| |
!  \___|_| |_| |_|_|___/___/___|_|  \___|\__,_|\__,_|_|_| |_|\__, |
!                         |_____|                            |___/
!> @brief Reads mobile emission file from MOVES emiss_2016.csv
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/23/2021
!>   @version  3.0
subroutine mobile_emiss_reading
	implicit none
	integer:: i,j,iedo
    integer:: anio,cint
    character(len=1):: st
	character(len=10):: cdum
    character(len=33):: fname='emiss_2016.csv'
	print *,'Starts reading emissions file in Mg/yr ',fname
	open(10,file="emis/movil/"//fname,status='old',action='read')
	read(10,*) cdum,cdum,(pol(i),i=1,npol) !read header
	i=0
	do
        read(10,*,END=100)cdum
        i=1+i
	end do
100 continue
    print *,' Rows number:',i
	rewind(10)
	read(10,'(A)') cdum ! read header
	allocate(emid(i),iscc(i),ei(i,npol))
	nl=i
	do i=1,nl
        read(10,*,ERR=140)emid(i),iscc(i),(ei(i,j),j=1,npol)
!      print *,emid(i),iscc(i),ei(i,7)
	end do
	print *,'End reading file ',fname
	close(10)
!
    fname='./gri_movil.csv'
    print *,'Starts reading spatial file Streets and Higways fraction: ',fname(20:33)

	open(10,file=fname,status='old',action='read')
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
	allocate(grid(i),id2(i),streeF(i),highwF(i),im(i))
    nl2=i
    im=6
    do i=1,nl2
        read(10,*,ERR=160) grid(i),id2(i),streeF(i),highwF(i)
        iedo=int(id2(i)/1000)
        if(iedo.eq.2 .or.iedo.eq.3) im(i)=8
        if(iedo.eq.8 .or.iedo.eq.18.and.iedo.eq.25.and.iedo.eq.26)im(i)=7
        !print *,i,grid(i),id2(i),streeF(i),highwF(i)
    end do
    print *,'End reading file gri_movil.csv',nl2
    close(10)
!
!	Se considera que el 10 % va en carretera
!   y el 90% en ciudad
!
	streeF=streeF*0.90
	highwF=highwF*0.10
	return
140 print *,"Error in reading file emiss_2016.csv",i
    stop
160 print *,"Error in reading file gri_movil.csv",i
    stop
end subroutine mobile_emiss_reading
!                        _
!   ___ ___  _   _ _ __ | |_
!  / __/ _ \| | | | '_ \| __|
! | (_| (_) | |_| | | | | |_
!  \___\___/ \__,_|_| |_|\__|
!> @brief Counting the number of different GRDICODE cells,
!>  obtains unique SCC and timezones (im)
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  20/12/2022
!>   @version  3.5
subroutine count
    use SortUnique
    integer :: ii,i,j
    integer(kind=4), allocatable  :: lista(:)
    character(len=10), allocatable :: listc(:)
    lista=grid
    grid2=Unique(lista)
    deallocate(lista)
    allocate(im2(size(grid2)))
    ! Busca la timezone a partir del grid
    do j=1,size(grid2)
        do i=1,size(grid)
            if(grid(i).eq.grid2(j)) then
                im2(j)=im(i)
                exit
            end if
        end do
    end do
    print *,'    Number of different cells',size(grid2)
!
! From emissions file F_moviles.csv
    listc=iscc
    jscc=CUnique(listc)
    deallocate(listc)
    ii=size(jscc)
    print *,'    Different SCC ',ii
    allocate(pemi(size(grid2),npol,ii))
    pemi=0
end subroutine count

end program
