!
!  Programa suma Carreteras
!
!  Creado por Jose Agustin Garcia Reynoso el 12/05/2012
!
! ifort -O2 -axAVX suma_carretera.F90 -o carr.exe
!
! Proposito:
! Este programa identifica las diferentes carreteras en la celda y las suma.
! y obtiene la fraccion de carretera en la celda con respecto al municipio
!
!  Modificaciones
!
!   2/Ago/2012  se considera la vialidad en todo el municipio rlm
!   9/Abr/2020  usa namelist global
!> @brief for suma_carretera.F90 program. For aggregation Highway fractions in cells.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!> @param grid
!> @param grid2  GRIDCODEs  in higway output file (salida2.csv)
!> @param icve  Municipality ID in higway input file
!> @param icve3  Municipality ID in higway output file
module highway_vars_mod
!> Number of lines in input file
integer :: nm         ;!>  GRIDCODEs  in higway input file (VIALIDADES.csv)
integer,allocatable :: grid(:) ; !> GRIDCODEs  in higway output file (salida2.csv)
integer,allocatable :: grid2(:)
!> Municipality ID in higway input file
integer,allocatable :: icve(:)
!>  Array with unique Municipality ID
integer,allocatable :: icve2(:)
!> Municipality ID in higway output file
integer,allocatable :: icve3(:)
!> Fraction higway area for each GIRDCODE and municipality
real,allocatable :: rc(:)
!> Municipality total higway area from inputfile
real,allocatable :: rlm(:)
!> Higway area for each GIRDCODE
real,allocatable :: rlc(:)
!> Municipality total higway area array in output file
real,allocatable :: sum(:)
!> Geographical area selected in namelist_emis.csv
character(len=12):: zona

common /vars1/ nm,zona

end module highway_vars_mod
!> @brief This program identifies the different Highway in the cell and adds them together.
!>
!> Obtains the Highway area fraction in the cell with respect to the total municipality Highway area
!> Highways can cover many cels, thus cels can be repeated by different higways this code reduce
!> the number of repeated cels by addig the fraction of each highway that includes each cel.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program suma_carreteras
use highway_vars_mod

    call lee_namelist_zona

    call highway_area_read

    call highway_fraction_calculation

    call highway_fraction_saving

contains
!> @brief Reads higway surface area file (CARRETERAS.csv)
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine highway_area_read
implicit none
    integer i
    character(len=50) ::fname
    character (len=10) ::cdum

    fname= "../01_datos/"//trim(zona)//"/"//"CARRETERAS.csv"
    open (unit=10,file=fname,status='OLD')
    read(10,*) cdum
    i=0
    do
        read(10,*,END=100) cdum
        i=i+1
    end do
100 continue
    nm=i
    print *,'Number of lines in file',nm
    allocate(grid(nm),icve(nm),rlm(nm),rlc(nm))
    rewind(10)
    read(10,*) cdum
    do i=1,nm
        read (10,*) grid(i),icve(i),rlm(i),rlc(i)
    end do
    print *,'Done reading file ',fname
    close(10)
end subroutine highway_area_read
!> @brief Identifyes the duplicate GRIDCODES, add higway areas and obtains
!> the fractional area from the total area in the municipality.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine highway_fraction_calculation
implicit none
    integer i,j,l
    call count
	rc=0
    do i=1,nm
        do j=1,size(grid2)
        if(grid(i).eq.grid2(j).and.icve(i).eq.icve3(j)) then
          do l=1,size(icve2)
            if(icve(i).eq.icve2(l)) then
                rc(j)=rlc(i)/rlm(i)+rc(j)
                sum(j)=rlm(i)
            end if
          end do
        end if
        end do
    end do
end subroutine highway_fraction_calculation
!> @brief Stores the Highway fractional area and total area for each grid code
!>  and municipality.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine highway_fraction_saving
implicit none
integer i,j
open(unit=11,file='salida.csv',action='write')
    write(11, *)"GRID, CVE_ENT_MUN, frac, suma"
    do i=1,size(grid2)
#ifndef PGI
     write(11, '(I8,",",I6,2(",",ES14.7))') grid2(i),icve3(i),rc(i),sum(i)
#else
     write(11, '(I8,",",I6,2(",",E))') grid2(i),icve3(i),rc(i),sum(i)
#endif
    end do
close (11)
end subroutine highway_fraction_saving
!> @brief Identifies the different municipalities in the CARRETERAS.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine count
    use highway_vars_mod
    logical,allocatable::xl(:)
    allocate(xl(size(icve)))
    xl=.true.
    do i=1,nm-1
        do j=i+1,nm
        if(icve(j).eq.icve(i).and.xl(j)) xl(j)=.false.
        end do
    end do
    j=0
    do i=1,nm
    if (xl(i)) j=j+1
    end do
    allocate(icve2(j))
    j=0
    do i=1,nm
        if(xl(i)) then
            j=j+1
            icve2(j) = icve(i)
        end if
    end do

  print *,'Number of different municipalities',j
    deallocate(xl)
    allocate(xl(size(grid)))
    xl=.true.
    do i=1,nm-1
        do j=i+1,nm
        if(grid(j).eq.grid(i).and.xl(j).and.icve(j).eq.icve(i)) xl(j)=.false.
        end do
    end do
    j=0
    do i=1,nm
        if (xl(i)) j=j+1
    end do
    allocate(grid2(j),rc(j),icve3(j),sum(j))
    j=0
    do i=1,nm
        if(xl(i)) then
            j=j+1
            icve3(j) = icve(i)
            grid2(j) = grid(i)
        end if
    end do
    print *,'Number of different grids',j
    deallocate(xl)
end subroutine count
!> @brief Reads zona variable from global namelist input file.
!>
!> for selecting the dommain used in the spatial allocation
!> and for gathering grid values from CARRETERAS.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_namelist_zona
    NAMELIST /region_nml/ zona
    integer unit_nml
    logical existe
    unit_nml = 9
    existe = .FALSE.
    write(6,*)' >>>> Reading file - namelist_emis.nml'
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
        !WRITE (6    , NML = region_nml )
        close(unit_nml)
    else
        stop '***** No namelist_emis.nml in .. directory'
    end if
end subroutine lee_namelist_zona
end program suma_carreteras
