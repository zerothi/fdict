program tests

  use tst_utils

  implicit none

  character(len=20) :: a, b
  type(variable_t) :: v
  type(dictionary_t) :: da

  a = 'Hello world1'
  b = 'Hello world2'
  da = ('Hello1'.kv.a) // ('Hello2'.kv.b)
  call print(da)

  a = ' '
  call assign(v, da, 'Hello1')
  call assign(a, v)
  if ( a /= 'Hello world1' ) stop 9

  call assign(v, da, 'Hello2')
  call assign(a, v)
  if ( a /= 'Hello world2' ) stop 9

  call delete(v)
  call delete(da)

  print *,'SUCCESS'
  
end program tests
