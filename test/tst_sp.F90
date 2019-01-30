program tests

  use tst_utils

  implicit none
  
  type(variable_t) :: va , vb
  real(sp) :: a, b(2), c(2,2)
  real(sp), pointer :: pa =>null(), pb(:)=>null(), pc(:,:)=>null()
  logical :: success

#include "variable_declarations_.inc"
  
  a      = 1.0_sp
  b(:)   = 2._sp
  c(:,:) = 3._sp

  ! do 'va = a'
  call assign(va,a)
  ps0 = transfer(va%enc,ps0)
  print '(a2,tr1,f4.2)',va%t,ps0%p
  ! associate pa with va 'pa => va'
  call associate(pa,va,success=success)
  if ( success ) then
     print *,'Success: ',pa
  else
     stop 9
  end if
  if ( associatd(pa,va) ) then
     print *,'Associated: ',pa
  else
     stop 9
  end if
  if ( .not. associatd(pb,va) ) then
     print *,'Correctly non-associated'
  else
     stop 9
  end if

  call assign(va,b)
  call assign(b,va)
  ps1 = transfer(va%enc,ps1)
  print '(a2,2(tr1,f4.2))',va%t,ps1%p
  if ( any(b /= ps1%p) ) stop 9

  call assign(va,c)
  ps2 = transfer(va%enc,ps2)
  print '(a2,4(tr1,f4.2))',va%t,ps2%p
  if ( any(c /= ps2%p) ) stop 9

  call assign(va,1)
  pi0 = transfer(va%enc,pi0)
  print '(a2,tr1,i0)',va%t,pi0%p
  if ( 1 /= pi0%p ) stop 9

  call delete(va)

end program tests
