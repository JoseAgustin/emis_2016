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
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!> @param grid  GRIDCODEs  in higway input file (VIALIDADES.csv)
!> @param grid2  GRIDCODEs  in higway output file (salida2.csv)
!> @param icve  Municipality ID in higway input file
!> @param icve2  Array with unique Municipality ID
!> @param icve3  Municipality ID in higway output file
!> @param rc     Fractional higway area for each GIRDCODE and municipality
!> @param rlc    Higway area for each GIRDCODE
!> @param rlm    Municipality total higway area from inputfile
!> @param sum    Municipality total higway area array in output file
module vars
integer ::nm
integer,allocatable :: grid(:),icve(:),grid2(:),icve2(:),icve3(:)
real,allocatable ::rc(:),rlm(:),rlc(:)
real,allocatable :: sm(:),sc(:),sum(:)
!> Geographical area selected in namelist_emis.csv
character(len=12):: zona

common /vars1/ nm,zona

end module vars
!> @brief This program identifies the different Highway in the cell and adds them together.
!>
!> Obtains the Highway area fraction in the cell with respect to the total municipality Highway area
!> Highways can cover many cels, thus cels can be repeated by different higways this code reduce
!> the number of repeated cels by addig the fraction of each highway that includes each cel.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
program suma
use vars

    call lee_namelist

    call lee

    call calculos

    call guarda

contains
!> @brief Reads higway area file (CARRETERAS.csv)
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee
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
end subroutine
!> @brief Identifyes the duplicate GRIDCODES, add higway areas and obtains
!> the fractional area from the total area in the municipality.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine calculos
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
end subroutine calculos
!> @brief Stores the Highway fractional area and total area for each grid code
!>  and municipality.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine guarda
implicit none
integer i,j
open(unit=11,file='salida.csv',action='write')
    write(11, *)"GRID, CVE_ENT_MUN, frac, suma"
    do i=1,size(grid2)
#ifndef PGI
     write(11, '(I8,",",I6,2(",",ES))') grid2(i),icve3(i),rc(i),sum(i)
#else
     write(11, '(I8,",",I6,2(",",E))') grid2(i),icve3(i),rc(i),sum(i)
#endif
    end do
close (11)
end subroutine guarda
!> @brief Identifies the different municipalities in the CARRETERAS.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine count
    use vars
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
    allocate(icve2(j),sm(j),sc(j))
    sm=0
    sc=0
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
!> @brief Reads global namelist_emis for area identification in order to obtain specific
!> grid values from CARRETERAS.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine lee_namelist
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
end subroutine lee_namelist
end program suma
