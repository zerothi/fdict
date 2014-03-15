program test_dict

  use variable
  use dictionary

  implicit none

  integer, parameter :: N = 26
  ! create a really long dict (N**3)
  type(dict) :: dic
  character(len=10) :: key
  integer :: i,j,k

  do k = 1 , N
     do j = 1 , N
        do i = 1 , N
           key = achar(64+i)//achar(64+j)//achar(64+k)
           call add(dic,(key.kv.1))
        end do
     end do
  end do

  print '(2(a,i0))','Added ',N**3,' keys and looped through them...',len(dic)

  call delete(dic)

  print '(2(a,i0))','Deleted ',N**3,' keys...',len(dic)

end program test_dict
