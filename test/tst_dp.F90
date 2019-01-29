program tests

  use tst_utils

  implicit none
  
  type(variable_t) :: va
  real(dp) :: a, b(2), c(2,2)
  real(dp), pointer :: pa =>null(), pb(:)=>null(), pc(:,:)=>null()
  logical :: success

#include "variable_declarations_.inc"
  
  a      = 1.0_dp
  b(:)   = 2._dp
  c(:,:) = 3._dp

  ! do 'va = a'
  call assign(va,a)
  pd0 = transfer(va%enc,pd0)
  print '(a2,tr1,f4.2)',va%t,pd0%p
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
  pd1 = transfer(va%enc,pd1)
  print '(a2,2(tr1,f4.2))',va%t,pd1%p

  call assign(va,c)
  pd2 = transfer(va%enc,pd2)
  print '(a2,4(tr1,f4.2))',va%t,pd2%p

  call assign(va,1)
  pi0 = transfer(va%enc,pi0)
  print '(a2,tr1,i0)',va%t,pi0%p

  call delete(va)

end program tests
