program tests

  use iso_var_str
  use variable

  implicit none
  
  integer, parameter :: sp = selected_real_kind(p=6)

  type(var) :: va , vb
  real(sp) :: a, b(2), c(2,2)
  real(sp), pointer :: pa =>null(), pb(:)=>null(), pc(:,:)=>null()
  logical :: success
  
  a      = 1.0_sp
  b(:)   = 2._sp
  c(:,:) = 3._sp

  ! do 'va = a'
  call assign(va,a)
  print '(a2,tr1,f4.2)',va%t,va%s0
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
  print '(a2,2(tr1,f4.2))',va%t,va%s1

  call assign(va,c)
  print '(a2,4(tr1,f4.2))',va%t,va%s2

  call assign(va,1)
  print '(a2,tr1,i0)',va%t,va%i0

end program tests
