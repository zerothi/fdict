program tests

  use tst_utils
  use variable

  implicit none
  
  integer :: i, N, step

  N = 500
  step = 25

  write(*,*)'Running with deallocation'
  ! 
  ! we should here allocate around 1Gb
  do i = 1 , N
     call mem(.true.)
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do

  write(*,*)'Running without deallocation'
  ! we should here allocate around 1Gb
  do i = 1 , N
     call mem(.false.)
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do

contains

  subroutine mem(dealloc)
    logical, intent(in) :: dealloc
    real(dp) :: va(400,400) ! roughly 1.22 MB
    type(variable_t) :: v
    call assign(v,va)
    if ( dealloc ) call delete(v)
  end subroutine mem

end program tests
