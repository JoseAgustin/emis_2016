!> @brief Combines street and Higway fractional areas in one.
!>  salida.csv contains  Higway fractional areas
!>  salida2.csv contains  Street fractional areas
!> gri_movil.csv  combined Highway and street fractional areas
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!   Programa agrega.f90
!
!  Creado por Jose Agustin Garcia Reynoso el 12/05/2012
!
!  ifort -O2 -axAVX agrega.f90 -o agrega.exe
! Proposito:
!  Lee las fracciones de vialidad y carretera (salida.csv)y genera un archivo
!  combinado de ambas
!
!  Modificaciones
!
!   9/Sep/2014  se actualiza salida para varios municipios
!   2/Ago/2012  se incluye en la salida la clave del municipio
!
!> @param nm    GRIDCODE number in Highway file
!> @param nm2   GRIDCODE number in Streets file
!> @param nm3   GRIDCODE number in combined Highway and street file
!> @param grid  Array of GRIDCODEs  in Highway file
!> @param grid2 Array of GRIDCODEs  in Street file
!> @param grid3 Array of GRIDCODEs  in combined Highway and street file
!> @param icve  Array of Municipality ID in Highway file
!> @param icve2 Array of Municipality ID in Street file
!> @param icve3 Array of Municipality ID in Highway and street file
!> @param fcc  Array of fractional surface Highway area in Highway file
!> @param fcv Array of fractional surface Street area in Street file
!> @param fc3 Fractional surface Highway area in combined Highway and street array
!> @param fv3 Fractional surface Street area in combined Highway and street array
!> @param smc Total Highway surface area in Highway file
!> @param smv Total Street surface area in Street file
module vars2
integer nm,nm2,nm3
integer,allocatable :: grid(:),icve(:)
integer,allocatable :: grid2(:),icve2(:)
integer,allocatable :: grid3(:),icve3(:)
real,allocatable ::fcc(:),smc(:)
real,allocatable ::fcv(:),smv(:)
real,allocatable ::fc3(:),fv3(:)

common /dims/ nm,nm2,nm3

end module vars2

program agrega
use vars2
    call lee

    call calcula

    call guarda

contains
!> @brief Reads street and Highway fractional area files.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!
	subroutine lee
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
	end subroutine lee
!
!> @brief Allocates fractional areas in each GRIDCODE.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
	subroutine calcula
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
           if(grid(i).eq.grid2(k).and.icve(i).eq.icve2(k).and.not(xl(k))) then
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
          if(grid(i).eq.grid2(k).and.icve(i).ne.icve2(k).and.not(xl(k))) then
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
      if(not(xl(i))) then
        icve3(m)=icve2(i)
        grid3(m)=grid2(i)
        fc3(m)=0
        fv3(m)=fcv(i)
        m=m+1
       end if
     end do
       nm3=m-1
	end subroutine calcula
!
!> @brief Stores GRIDCODE fractional areas in gri_movil.csv.
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  2020/06/20
!>   @version  2.1
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
	subroutine guarda
	implicit none
    integer i,var
    print *," Max number of lines after combining",nm3
    open (unit=10,file='gri_movil.csv',action='write')
    write(10,*)'GRID, CVE_ENT_MUN, Fv, Fc'
    write(10,*)nm3
    do i=1,nm3
    write(10,*)grid3(i),",",icve3(i),",",fv3(i),",",fc3(i)
    end do
    end subroutine guarda
    end program agrega
