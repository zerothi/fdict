program tests

  use tst_utils

  implicit none
  
  type(var) :: va , vb
  real(sp) :: a, b(2), c(2,2)
  real(sp), pointer :: pa =>null(), pb(:)=>null(), pc(:,:)=>null()
  logical :: success

#include "../var_declarations.inc"
  
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
  end if
  if ( associatd(pa,va) ) then
     print *,'Associated: ',pa
  end if
  if ( .not. associatd(pb,va) ) then
     print *,'Correctly non-associated'
  end if

  call assign(va,b)
  call assign(b,va)
  ps1 = transfer(va%enc,ps1)
  print '(a2,2(tr1,f4.2))',va%t,ps1%p

  call assign(va,c)
  ps2 = transfer(va%enc,ps2)
  print '(a2,4(tr1,f4.2))',va%t,ps2%p

  call assign(va,1)
  pi0 = transfer(va%enc,pi0)
  print '(a2,tr1,i0)',va%t,pi0%p

end program tests
