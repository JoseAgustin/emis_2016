!> @brief for mobile_mod.F990 module contains variables and subroutines
!> for managing streets, highways and combine those in a single CSV file to
!> allocate the mobile emissions in the grid
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  12/18/2022
!>   @version 1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
!
!
!  ifort -O2 -axAVX -c mobile_mod.F90
! Proposito:
!  Da las variables y modulos para leer las fracciones de vialidad (salida2)
!  y carretera (salida.csv), y genera un archivo combinado de ambas
!
!                  _     _ _                              _
!  _ __ ___   ___ | |__ (_) | ___     _ __ ___   ___   __| |
! | '_ ` _ \ / _ \| '_ \| | |/ _ \   | '_ ` _ \ / _ \ / _` |
! | | | | | | (_) | |_) | | |  __/   | | | | | | (_) | (_| |
! |_| |_| |_|\___/|_.__/|_|_|\___|___|_| |_| |_|\___/ \__,_|
!                              |_____|
module mobile_mod
; !> GRIDCODE number in Highway file
integer :: nm      ; !> GRIDCODE number in Streets file
integer :: nm2     ; !> GRIDCODE number in combined Highway and street file
integer :: nm3
!> Array of fractional surface Highway area in Highway file
real,allocatable :: fcc(:)
!> Array of fractional surface Street area in Street file
real,allocatable :: fcv(:)
!> Fractional surface Highway area in combined Highway and street array
real,allocatable :: fc3(:)
!> Fractional surface Street area in combined Highway and street array
real,allocatable :: fv3(:)
!> Array of Municipality ID in Highway file
integer,allocatable :: grid(:)
!>  Array of GRIDCODEs  in Street file
integer,allocatable :: grid2(:)
!> Array of GRIDCODEs  in combined Highway and street file
integer,allocatable :: grid3(:)
!>  Municipality ID in Street input file
integer,allocatable :: icve(:)
!>  Array of Municipality ID in Street file
integer,allocatable :: icve2(:)
!> Array of Municipality ID in Highway and street file
integer,allocatable ::icve3(:)
!>  Fractional street area for each GIRDCODE and municipality
real,allocatable :: rc(:)
!>   Street area for each GIRDCODE
real,allocatable :: rlm(:)
!>  Municipality total street area array in output file
real,allocatable ::rlc(:)
!> Total Highway surface area in Highway file
real,allocatable :: smc(:)
!> Total Street surface area in Street file
real,allocatable :: smv(:)
!>   Municipality total Street area from inputfile
real,allocatable ::sum(:)

common /dims/ nm,nm2,nm3
contains
!  ___ ___  _   _ _  _ _____
! / __/ _ \| | | | \| |_   _|
!| (_| (_) | |_| | .` | | |
! \___\___/ \___/|_|\_| |_|
!
!> @brief Identifies the different municipalities in the CARRETERAS.csv file.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/12/2020
!>   @version 2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
subroutine count
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
!
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
!> @brief Reads street area file (VIALIDADES.csv)
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2022
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!                                                  _
!   __ _ _ __ ___  __ _ ___     _ __ ___  __ _  __| |
!  / _` | '__/ _ \/ _` / __|   | '__/ _ \/ _` |/ _` |
! | (_| | | |  __/ (_| \__ \   | | |  __/ (_| | (_| |
!  \__,_|_|  \___|\__,_|___/___|_|  \___|\__,_|\__,_|
!                       |_____|
subroutine areas_read(fname_in)
use master
implicit none

    integer :: i
   character(len=15) ::fname_in
    character(len=50) ::fname
    character (len=10) ::cdum

    fname= "../01_datos/"//trim(zona)//"/"//trim(fname_in)
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
end subroutine areas_read
!> @brief Identifyes the duplicate GRIDCODES, add street areas and obtains
!> the fractional area from the total area in the municipality.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!        _       _      __                _   _
! __   _(_) __ _| |    / _|_ __ __ _  ___| |_(_) ___  _ __
! \ \ / / |/ _` | |   | |_| '__/ _` |/ __| __| |/ _ \| '_ \
!  \ V /| | (_| | |   |  _| | | (_| | (__| |_| | (_) | | | |
!   \_/ |_|\__,_|_|___|_| |_|  \__,_|\___|\__|_|\___/|_| |_|____
!   ___ __ _| | |_____| _| | __ _| |_(_) ___  _ __       |_____|
!  / __/ _` | |/ __| | | | |/ _` | __| |/ _ \| '_ \
! | (_| (_| | | (__| |_| | | (_| | |_| | (_) | | | |
!  \___\__,_|_|\___|\__,_|_|\__,_|\__|_|\___/|_| |_|
                                                              
subroutine vial_fraction_calculation
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
end subroutine vial_fraction_calculation
!> @brief Stores the Street fractional area and total area for each grid code
!>  and municipality.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version  3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!        _       _      __                _   _
! __   _(_) __ _| |    / _|_ __ __ _  ___| |_(_) ___  _ __
! \ \ / / |/ _` | |   | |_| '__/ _` |/ __| __| |/ _ \| '_ \
!  \ V /| | (_| | |   |  _| | | (_| | (__| |_| | (_) | | | |
!   \_/ |_|\__,_|_|___|_| |_|  \__,_|\___|\__|_|\___/|_| |_|____
!                |_____|                                  |_____|
!  ___  __ ___   _(_)_ __   __ _
! / __|/ _` \ \ / / | '_ \ / _` |
! \__ \ (_| |\ V /| | | | | (_| |
! |___/\__,_| \_/ |_|_| |_|\__, |
!                         |___/
!
subroutine vial_fraction_saving(ofile)
implicit none
character (len=12):: ofile
integer i,j
open(unit=11,file=ofile,action='write')
    write(11, *)"GRID, CVE_ENT_MUN, frac, suma"
    do i=1,size(grid2)
#ifdef PGI
     write(11, '(I8,",",I6,2(",",ES14.7))') grid2(i),icve3(i),rc(i),sum(i)
#else
     write(11, '(I8,",",I6,2(",",E))') grid2(i),icve3(i),rc(i),sum(i)
#endif
    end do
close (11)
end subroutine vial_fraction_saving
!> @brief Reads street and Highway fractional area files.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
! _     _       _                                _                 _
!| |__ (_) __ _| |____      ____ _ _   _     ___| |_ _ __ ___  ___| |_
!| '_ \| |/ _` | '_ \ \ /\ / / _` | | | |   / __| __| '__/ _ \/ _ \ __|
!| | | | | (_| | | | \ V  V / (_| | |_| |   \__ \ |_| | |  __/  __/ |_
!|_|_|_|_|\__, |_| |_|\_/\_/ \__,_|\__, |___|___/\__|_|  \___|\___|\__|
! / _|_ __|___/  ___| |_(_) ___  _ |___/_____| _ __ ___  __ _  __| |
!| |_| '__/ _` |/ __| __| |/ _ \| '_ \/ __|   | '__/ _ \/ _` |/ _` |
!|  _| | | (_| | (__| |_| | (_) | | | \__ \   | | |  __/ (_| | (_| |
!|_| |_|  \__,_|\___|\__|_|\___/|_| |_|___/___|_|  \___|\__,_|\__,_|
!                                        |_____|
    subroutine highway_street_fractions_read
    implicit none
    integer ::i
    character (len=12):: fname,cdum
    fname='salida.csv'
    open (unit=10,file=fname,status='old',action='READ')
    read(10,*) cdum
    i=0
    do
        read(10,*,END=100) cdum
        i=i+1
    end do
100 continue
    nm=i
    print *,'Number of lines in file',nm
    allocate(grid(nm),icve(nm),fcc(nm),smc(nm))
    rewind(10)
    read(10,*) cdum
    do i=1,nm
    read (10,*) grid(i),icve(i),fcc(i),smc(i)
    end do
    print *,'Done reading file ',fname
    close(10)
!
    fname='salida2.csv'
    open (unit=10,file=fname,status='old',action='READ')
    read(10,*) cdum
    i=0
    do
    read(10,*,END=110) cdum
    i=i+1
    end do
110 continue
    nm2=i
    print *,'Number of lines in file',nm2
    allocate(grid2(nm2),icve2(nm2),fcv(nm2),smv(nm2))
    rewind(10)
    read(10,*) cdum
    do i=1,nm2
    read (10,*) grid2(i),icve2(i),fcv(i),smv(i)
    end do
    print *,'Done reading file ',fname
    close(10)
    nm3=nm+nm2
     print *,'Max number of lines ',nm3
    end subroutine highway_street_fractions_read
!
!> @brief Allocates the total fractional area in each GRIDCODE.
!>
!> Identifies the all the GRICODEs for Highways and Streets
!> adds the fractianal area for each  Highways and Streets
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
! _     _       _                                _                 _
!| |__ (_) __ _| |____      ____ _ _   _     ___| |_ _ __ ___  ___| |_
!| '_ \| |/ _` | '_ \ \ /\ / / _` | | | |   / __| __| '__/ _ \/ _ \ __|
!| | | | | (_| | | | \ V  V / (_| | |_| |   \__ \ |_| | |  __/  __/ |_
!|_|_|_|_|\__, |_| |_|\_/\_/ \__,_|\__, |___|___/\__|_|  \___|\___|\__|____
! / _|_ __|___/  ___| |_(_) ___  _ |___/_____| ___ _   _ _ __ ___    |_____|
!| |_| '__/ _` |/ __| __| |/ _ \| '_ \/ __|   / __| | | | '_ ` _ \
!|  _| | | (_| | (__| |_| | (_) | | | \__ \   \__ \ |_| | | | | | |
!|_| |_|  \__,_|\___|\__|_|\___/|_| |_|___/___|___/\__,_|_| |_| |_|
!    | | | | '_ \                        |_____|
!    | |_| | |_) |
! ____\__,_| .__/
!|_____|   |_|
    subroutine highway_street_fractions_sum_up
    implicit none
    integer imax,imin
    integer i,j,k,l,m
    real, allocatable::  suma(:)
    logical, allocatable:: xl(:)
     allocate(xl(nm2))
     allocate(grid3(nm3))
     allocate(suma(nm3),fc3(nm3),fv3(nm3),icve3(nm3))
    print *,">>>>>>>>>   Computations  <<<<<<<<<<<"
    xl=.false.
    l=1
    m=nm+1
    do i  =1,nm
    icve3(i)=icve(i )
    grid3(i)=grid(i)
    fc3(i)=fcc(i )
    fv3(i)=0
    do j =1,nm2
     if(grid(i).eq.grid2(j)) then
        do k=j,nm2
    if(grid(i).eq.grid2(k).and.icve(i).eq.icve2(k).and. .not.(xl(k))) then
          fv3(i)=fcv(k)
          xl(k)=.true.
         !print *,grid3(i),icve3(i),fc3(i),fv3(i),i
         end if
        end do! k
       end if
      end do ! j
    end do !i
    !
    do i  =1,nm
    do j =1,nm2
    if(grid(i).eq.grid2(j)) then
     do k=j,nm2
    if(grid(i).eq.grid2(k).and.icve(i).ne.icve2(k).and. .not.(xl(k))) then
          icve3(m )=icve2(k)
          grid3(m)=grid2(k)
          fc3(m)= 0
          fv3(m )=fcv(k)
          xl(k)=.true.
         ! print *,grid3(m),icve3(m),fc3(m),fv3(m),m
           m=m+1
         end if
         end do! k
      end if
    end do !j
    end do !i
    do i=1,nm2
    if(.not.(xl(i))) then
        icve3(m)=icve2(i)
        grid3(m)=grid2(i)
        fc3(m)=0
        fv3(m)=fcv(i)
        m=m+1
      end if
    end do
    nm3=m-1
    end subroutine highway_street_fractions_sum_up
!
!> @brief Stores GRIDCODE total fractional areas in gri_movil.csv
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  04/26/2021
!>   @version 3.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!      _             _
!  ___| |_ ___  _ __(_)_ __   __ _
! / __| __/ _ \| '__| | '_ \ / _` |
! \__ \ || (_) | |  | | | | | (_| |
! |___/\__\___/|_|  |_|_| |_|\__, |____           _                 _
! | |__ (_) __ _| |____      |___/_____|_     ___| |_ _ __ ___  ___| |_
! | '_ \| |/ _` | '_ \ \ /\ / / _` | | | |   / __| __| '__/ _ \/ _ \ __|
! | | | | | (_| | | | \ V  V / (_| | |_| |   \__ \ |_| | |  __/  __/ |_
! |_| |_|_|\__, |_| |_|\_/\_/ \__,_|\__, |___|___/\__|_|  \___|\___|\__|____
!  _       |___/     _              |___/_____|                       |_____|
! | |_ ___ | |_ __ _| |
! | __/ _ \| __/ _` | |
! | || (_) | || (_| | |
!  \__\___/ \__\__,_|_|
!
subroutine storing_highway_street_total
    implicit none
    integer i,var
    print *," Max number of lines after combining",nm3
    open (unit=10,file='gri_movil.csv',action='write')
    write(10,*)'GRID, CVE_ENT_MUN, Fv, Fc'
    write(10,*)nm3
    do i=1,nm3
      write(10,*)grid3(i),",",icve3(i),",",fv3(i),",",fc3(i)
    end do
    end subroutine storing_highway_street_total

end module mobile_mod
