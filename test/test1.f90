program test1
  
  use variable

  implicit none
  
  integer, parameter :: dp = selected_real_kind(p=10)

  type(var) :: va , vb
  real(dp) :: a, b(2), c(2,2)
  real(dp), pointer :: pa =>null(), pb(:)=>null(), pc(:,:)=>null()
  character(len=20) :: ca, cb
  logical :: success
  
  a = 1.0_dp
  b = 2._dp
  c = 3._dp
  
  call assign(va,a)
  print *,va%t,va%d0
  call associate(pa,va,success=success)
  if ( success ) then
     print *,'Success: ',pa
  end if

  call assign(va,b)
  call assign(b,va)
  print *,va%t,va%d1

  call assign(va,c)
  print *,va%t,va%d2

  call assign(va,1)
  print *,va%t,va%i0


  ca = 'hello world'
!  call assign(va,ca)

!  call assign(cb,va)
  print *,cb

end program test1
