program test

  use tst_utils
  use variable

  implicit none
  
  type(variable_t) :: va
  character(len=20) :: ca, sa
  character, pointer :: pa(:) => null()
  character :: aa(20)

  
  call assign(va, 'hello')
  call assign(sa, va)
  ca = sa
  print '(a2,tr1,a)',va%t,ca
  if ( 'hello' /= ca ) stop 9

  ca = ' '
  call assign(ca, va)
  print '(a2,tr1,a)',va%t,ca
  if ( 'hello' /= ca ) stop 9

  ca = 'none'
  aa = ' '
  aa(1:5) = cpack('hello')
  call assign(va, aa)
  print '(a2,tr1,20a)',va%t,aa
  call assign(ca, va)
  if ( 'hello' /= ca ) stop 9

  ca = 'none'
  aa = ' '
  aa(1:5) = cpack('hello')
  call associate(va, aa, dealloc=.true.)
  print '(a2,tr1,20a)',va%t,aa
  call associate(pa, va)
  ca = cunpack(pa)
  if ( 'hello' /= ca ) stop 9
  aa = ''
  aa(1) = 'H'
  ca = cunpack(pa)
  if ( 'H' /= ca ) stop 9

  call nullify(va)
  print *,'SUCCESS'

end program test
