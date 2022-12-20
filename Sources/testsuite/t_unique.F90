! Testing UNIQUE function
!>  @brief Testing unique function
!>   Returns an array without duplicates
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  19/12/2022
!>   @version  1.0
!>   @copyright Universidad Nacional Autonoma de Mexico 2022
!
Program TestUnique
  use SortUnique
  implicit none
  !   find "indices", the list of unique numbers in "list"
  integer (kind=4),allocatable :: list(:),newlist(:)
  integer (kind=4)  :: kx,N=100000 !N  even
  real (kind=4) :: start,finish,myrandom

  allocate(list(N))
  do kx=1,N
     call random_number(myrandom)
     list(kx)=ifix(float(N)/2.*myrandom)
  end do

  call cpu_time(start)

  newlist=unique(list)

  call cpu_time(finish)
  print *,"Whithout duplicates: ",finish-start
  print *,"The size(newlist) ",size(newlist)

End Program TestUnique
