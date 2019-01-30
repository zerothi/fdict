program tests

  use tst_utils

  implicit none

  real(dp) :: va(400,400) ! roughly 1.22 MB
  
  integer :: i, N, step

  N = 500
  step = 25

  write(*,*)'Dictionary passing to routine (in sub-call)'
  do i = 1 , N
     call after_mem
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do
  
  write(*,*)'Dictionary passing to routine'
  do i = 1 , N
     call mem('hello'.kv.va)
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do

  print *,'SUCCESS'

contains

  subroutine after_mem()
    type(dictionary_t) :: dic
    dic = 'hello'.kv.va
    call mem(dic)
  end subroutine after_mem
    
  subroutine mem(dic)
    type(dictionary_t), intent(in) :: dic

    integer :: i, N
    ! do nothing with the dictionary, check that it gets deleted
    N = 1
    do i = 1 , 10
       N = N + i
    end do
  end subroutine mem

end program tests
