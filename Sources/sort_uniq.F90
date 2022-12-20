Module SortUnique
!  ____             _   _   _       _
! / ___|  ___  _ __| |_| | | |_ __ (_) __ _ _   _  ___
! \___ \ / _ \| '__| __| | | | '_ \| |/ _` | | | |/ _ \
!  ___) | (_) | |  | |_| |_| | | | | | (_| | |_| |  __/
! |____/ \___/|_|   \__|\___/|_| |_|_|\__, |\__,_|\___|
!                                        |_|
contains
!>   @brief Extracts unique elements from an array
!>   @date  05/26/2017
!>   @author Clinton Winant
!>   @version  2.0
!>   @param temp  initial array with duplicates
!>   @param Begin index of the first element
!>   @param Finish index of the last element
!>   @param list  integer array without dupilcates
!  __  __                     ____             _
! |  \/  | ___ _ __ __ _  ___/ ___|  ___  _ __| |_
! | |\/| |/ _ \ '__/ _` |/ _ \___ \ / _ \| '__| __|
! | |  | |  __/ | | (_| |  __/___) | (_) | |  | |_
! |_|  |_|\___|_|  \__, |\___|____/ \___/|_|   \__|
!                  |___/
  Recursive Subroutine MergeSort(temp, Begin, Finish, list)
    ! 1st 3 arguments are input, 4th is output sorted list
    implicit none
    integer(kind=4),intent(inout) :: Begin,list(:),temp(:)
    integer(kind=4),intent(in) :: Finish
    integer(kind=4) :: Middle
    if (Finish-Begin<2) then    !if run size =1
       return                   !it is sorted
    else
       ! split longer runs into halves
       Middle = (Finish+Begin)/2
       ! recursively sort both halves from list into temp
       call MergeSort(list, Begin, Middle, temp)
       call MergeSort(list, Middle, Finish, temp)
       ! merge sorted runs from temp into list
       call Merge(temp, Begin, Middle, Finish, list)
     endif
  End Subroutine MergeSort
!>   @brief Merge two arrays
!>   @date  05/26/2017
!>   @author Clinton Winant
!>   @version  2.0
!>   @param Middle (Finish+Begin)/2
!>   @param temp  initial array with duplicates
!>   @param Begin index of the first element
!>   @param Finish index of the last element
!>   @param list  integer array without dupilcates
!  __  __
! |  \/  | ___ _ __ __ _  ___
! | |\/| |/ _ \ '__/ _` |/ _ \
! | |  | |  __/ | | (_| |  __/
! |_|  |_|\___|_|  \__, |\___|
!                   |___/
  Subroutine Merge(list, Begin, Middle, Finish, temp)
    implicit none
    integer(kind=4),intent(inout) :: list(:),temp(:)
    integer(kind=4),intent(in) ::Begin,Middle,Finish
    integer(kind=4)            :: kx,ky,kz
    ky=Begin
    kz=Middle
    !! While there are elements in the left or right runs...
    do kx=Begin,Finish-1
       !! If left run head exists and is <= existing right run head.
       if (ky.lt.Middle.and.(kz.ge.Finish.or.list(ky).le.list(kz))) then
          temp(kx)=list(ky)
          ky=ky+1
       else
          temp(kx)=list(kz)
          kz = kz + 1
       end if
    end do

  End Subroutine Merge
!>   @brief Extract the unique elements from the integer array
!>   @date  05/26/2017
!>   @author  Clinton Winant
!>   @version  2.0
!  _   _       _
! | | | |_ __ (_) __ _ _   _  ___
! | | | | '_ \| |/ _` | | | |/ _ \
! | |_| | | | | | (_| | |_| |  __/
!  \___/|_| |_|_|\__, |\__,_|\___|
!                  |_|
  Function Unique(list)
    !! usage sortedlist=Unique(list)
    implicit none
    integer(kind=4) :: strt,fin,N
    integer(kind=4), intent(inout) :: list(:)
    integer(kind=4), allocatable   :: unique(:),work(:)
    logical,allocatable :: mask(:)
    ! sort
    work=list;strt=1;N=size(list);fin=N+1
    call MergeSort(work,strt,fin,list) 
    ! cull duplicate indices
    allocate(mask(N));
    mask=.false.
    mask(1:N-1)=list(1:N-1)==list(2:N)
    unique=pack(list,.not.mask)
    deallocate(mask)
  End Function Unique
!>   @brief Extracts unique elements from a character array
!>   @date  20/12/2022
!>   @author  Jose Agustin Garcia Reynoso
!>   @version  1.0
!>   @param temp  initial character array with duplicates
!>   @param Begin index of the first element
!>   @param Finish index of the last element
!>   @param list  character array without dupilcates
!   ____ __  __                     ____             _
!  / ___|  \/  | ___ _ __ __ _  ___/ ___|  ___  _ __| |_
! | |   | |\/| |/ _ \ '__/ _` |/ _ \___ \ / _ \| '__| __|
! | |___| |  | |  __/ | | (_| |  __/___) | (_) | |  | |_
!  \____|_|  |_|\___|_|  \__, |\___|____/ \___/|_|   \__|
!                       |___/
Recursive Subroutine CMergeSort(temp, Begin, Finish, list)
  ! 1st 3 arguments are input, 4th is output sorted list
  implicit none
  integer(kind=4),intent(in)      :: Begin
  character(len=10),intent(inout) :: temp(:)
  integer(kind=4),intent(in)      :: Finish
  character(len=10),intent(inout) :: list(:)
  integer(kind=4) :: Middle
  if (Finish-Begin<2) then    !if run size =1
     return                   !it is sorted
  else
     ! split longer runs into halves
     Middle = (Finish+Begin)/2
     ! recursively sort both halves from list into temp
     call CMergeSort(list, Begin, Middle, temp)
     call CMergeSort(list, Middle, Finish, temp)
     ! merge sorted runs from temp into list
     call CMerge(temp, Begin, Middle, Finish, list)
   endif
End Subroutine CMergeSort
!>   @brief Merge two character arrays
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  20/12/2022
!>   @version  1.0
!>   @param Middle (Finish+Begin)/2
!>   @param temp  initial character array with duplicates
!>   @param Begin index of the first element
!>   @param Finish index of the last element
!>   @param list  character array without dupilcates
!   ____ __  __
!  / ___|  \/  | ___ _ __ __ _  ___
! | |   | |\/| |/ _ \ '__/ _` |/ _ \
! | |___| |  | |  __/ | | (_| |  __/
!  \____|_|  |_|\___|_|  \__, |\___|
!                        |___/
!
Subroutine CMerge(list, Begin, Middle, Finish, temp)
  implicit none
  character(len=10),intent(inout) :: list(:),temp(:)
  integer(kind=4),intent(in) ::Begin,Middle,Finish
  integer(kind=4)    :: kx,ky,kz
  ky=Begin
  kz=Middle
  !! While there are elements in the left or right runs...
  do kx=Begin,Finish-1
     !! If left run head exists and is <= existing right run head.
     if (ky.lt.Middle.and.(kz.ge.Finish.or.list(ky).le.list(kz))) then
        temp(kx)=list(ky)
        ky=ky+1
     else
        temp(kx)=list(kz)
        kz = kz + 1
     end if
  end do
End Subroutine CMerge
!
!>   @brief Extract the unique elements from the character array
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  20/12/2022
!>   @version  1.0
!   ____ _   _       _
!  / ___| | | |_ __ (_) __ _ _   _  ___
! | |   | | | | '_ \| |/ _` | | | |/ _ \
! | |___| |_| | | | | | (_| | |_| |  __/
!  \____|\___/|_| |_|_|\__, |\__,_|\___|
!                         |_|
Function CUnique(list)
  !! usage sortedlist=Unique(list)
  implicit none
  integer(kind=4) :: strt,fin,N
  character(len=10), intent(inout) :: list(:)
  character(len=10), allocatable  :: cunique(:),work(:)
  logical,allocatable :: mask(:)
  ! sort
  work=list;strt=1;N=size(list);fin=N+1
  call CMergeSort(work,strt,fin,list)
  ! cull duplicate indices
  allocate(mask(N));
  mask=.false.
  mask(1:N-1)=list(1:N-1)==list(2:N)
  cunique=pack(list,.not.mask)
  deallocate(mask)
End Function CUnique
End Module SortUnique
