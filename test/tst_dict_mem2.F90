program tests

  use tst_utils

  implicit none
  
  integer :: i, N, step

  N = 500
  step = 25

  write(*,*)'Remove and delete var'
  ! we should here allocate around 1Gb
  do i = 1 , N
     call mem_rem(.true.)
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do

  write(*,*)'Remove and NO deallocation'
  ! we should here allocate around 1Gb
  do i = 1 , N
     call mem_rem(.false.)
     if ( mod(i,step) == 0 ) then
        call show_mem
     end if
  end do

contains

  subroutine mem_rem(dealloc)
    logical, intent(in) :: dealloc
    type(dictionary_t) :: d
    real(dp) :: va(400,400) ! roughly 1.22 MB
    type(variable_t) :: v
    va = 0.
    call extend(d,'hello'.kv.va)
    call associate(v,d,'hello')
    call nullify(d,'hello')
    if ( dealloc ) call delete(v)
  end subroutine mem_rem

end program tests
