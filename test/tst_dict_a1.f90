program tests

  use tst_utils

  implicit none

  character(len=1), target:: a(20), b(20)
  character(len=1), pointer :: d(:) => null()
  character(len=20) :: c
  type(dictionary_t) :: da

  a = ' '
  b = ' '
  c = 'Hello world3'
  a(1:12) = cpack('Hello world1')
  b(1:12) = cpack('Hello world2')
  da = ('Hello1'.kv.a) // ('Hello2'.kvp.b) // ('Hello3'.kv.c)
  call print(da)

  a = ' '
  call assign(a, da, 'Hello1')
  c = cunpack(a)
  if ( c /= 'Hello world1' ) stop 9

  b(1:12) = cpack('World hello2')
  call assign(a, da, 'Hello2')
  c = cunpack(a)
  if ( c /= 'World hello2' ) stop 10

  call associate(d, da, 'Hello3')
  d(1) = 'U'
  nullify(d)
  call associate(d, da, 'Hello3')
  c = cunpack(d)
  if ( c /= 'Uello world3' ) stop 11

  call nullify(da, 'Hello2')
  call delete(da)

  print *,'SUCCESS'

end program tests
