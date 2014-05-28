program test1

  use tst_utils
  use iso_var_str
  use variable

  implicit none
  
  type(var) :: va
  real(dp) :: a, b(2), c(2,2)
  real(dp), pointer :: pa =>null()
  type(var_str) :: sa, sb
  character(len=20) :: ca, cb
  logical :: success
  
  a = 1.0_dp
  b = 2._dp
  c = 3._dp
  
  call assign(va,a)
  print '(a2,tr1,f4.2)',va%t,va%d0
  call associate(pa,va,success=success)
  if ( success ) then
     print *,'Success: ',pa
  end if

  call assign(va,b)
  call assign(b,va)
  print '(a2,2(tr1,f4.2))',va%t,va%d1

  call assign(va,c)
  print '(a2,4(tr1,f4.2))',va%t,va%d2

  call assign(va,1)
  print '(a2,tr1,i0)',va%t,va%i0

  ca = 'hello world'
  sa = ca
  call assign(va,sa)

  call assign(sb,va)
  cb = sb
  print *,cb

end program test1
