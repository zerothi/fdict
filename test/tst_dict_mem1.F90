program tests

  use tst_utils

  implicit none
  
  integer :: i, N, step

  N = 500
  step = 25

  write(*,*)'Delete and deallocation'
  ! we should here allocate around 1Gb
  do i = 1 , N
     call mem(.true.)
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do

  write(*,*)'Delete and NO deallocation'
  ! we should here allocate around 1Gb
  do i = 1 , N
     call mem(.false.)
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do

  print *, 'SUCCESS'

contains

  subroutine mem(dealloc)
    logical, intent(in) :: dealloc
    real(dp) :: va(400,400) ! roughly 1.22 MB
    type(dictionary_t) :: d
    va = 0.
    d = 'hello'.kv.va
    if ( dealloc ) call delete(d,'hello')
    d = 'hello'.kv.va
    if ( dealloc ) call delete(d)
  end subroutine mem

end program tests
