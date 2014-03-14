program tests

  use iso_var_str
  use variable

  implicit none
  
  integer, parameter :: dp = selected_real_kind(p=15)

  integer :: i, N, step

  N = 1000
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
    type(var) :: v
    call assign(v,va)
    if ( dealloc ) call delete(v)
  end subroutine mem

  subroutine show_mem()
    call system("free | grep Mem | awk '{print $1,$2/1024,$3/1024,$4/1024}'")
  end subroutine show_mem

end program tests
