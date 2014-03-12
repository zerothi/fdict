program test_dict

  use variable
  use dictionary

  implicit none

  integer, parameter :: N = 26
  ! create a really long dict (N**3)
  type(dict) :: dic
  character(len=10) :: key
  integer :: i,j,k


  dic = ('first'.kv.1)
  do k = 1 , N
     do j = 1 , N
        do i = 1 , N
           key = achar(64+i)//achar(64+j)//achar(64+k)
           dic = dic//(key.kv.1)
        end do
     end do
  end do

  ! print all the values
  call dict_print(dic)

  print '(a,i0,a)','Added ',N**3,' keys and looped through them...'

end program test_dict
