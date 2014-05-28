program test

  use tst_utils
  use iso_var_str
  use variable

  implicit none
  
  type(var) :: va
  type(var_str) :: sa
  character(len=20) :: ca
  
  call assign(va,'hello')
  call assign(sa,va)
  ca = sa
  print '(a2,tr1,a)',va%t,ca

  call assign(ca,va)
  print '(a2,tr1,a)',va%t,ca

end program test
