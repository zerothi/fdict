program tests

  use tst_utils

  implicit none
  
  type(var) :: va , vb
  real(dp) :: a, b(2), c(2,2)
  real(dp), pointer :: pa =>null(), pb(:)=>null(), pc(:,:)=>null()
  logical :: success
  
  a      = 1.0_dp
  b(:)   = 2._dp
  c(:,:) = 3._dp

  ! do 'va = a'
  call assign(va,a)
  print '(a2,tr1,f4.2)',va%t,va%d0
  ! associate pa with va 'pa => va'
  call associate(pa,va,success=success)
  if ( success ) then
     print *,'Success: ',pa
  end if
  if ( associatd(pa,va) ) then
     print *,'Associated: ',pa
  end if
  call associate(pb,va,success=success)
  if ( .not. success ) then
     print *,'Success, not associated'
  end if

  call assign(va,b)
  call assign(b,va)
  print '(a2,2(tr1,f4.2))',va%t,va%d1

  call assign(va,c)
  print '(a2,4(tr1,f4.2))',va%t,va%d2

  call assign(va,1)
  print '(a2,tr1,i0)',va%t,va%i0

end program tests
