program tst_dict_valgrind

  use variable
  use dictionary
  use tst_utils

  implicit none

  real :: a, b(2),c(2,2)
  real, pointer :: d1 => null()
  real, pointer :: d2 => null()

  integer :: i
  type(variable_t) :: v
  type(dictionary_t) :: dic, tmp

  a = 1.
  b = 2.
  c = 3.

  allocate(d1)
  d1 = 4.
  allocate(d2)
  d2 = 5.

  ! NO LEAK
  call assign(v, 'hello')
  ! NO LEAK
  call assign(v, 'hello2')

  ! fill dictionary
  dic = ('a'.kv.a)
  ! NO LEAK
  dic = dic // ('b'.kv.b)
  ! LEAK <
  dic = dic // ('b'.kv.b)
  ! NO LEAK
  dic = dic // ('c'.kv.'hello')
  ! LEAK <
  dic = dic // ('c'.kv.'hello2')
  ! NO LEAK
  dic = dic // ('d'.kvp.d1)
  ! LEAK <
  dic = dic // ('d'.kvp.d2)
  ! NO LEAK
  dic = dic // ('string'.kv.'Hello world')

  ! print all the values
  call print(dic)

  ! Assign first value
  call assign(v, dic, 'string')
  call delete(v)

  v = .val. dic
  call delete(v)

  call delete(dic)
  if ( associated(d1) ) print *, 'd1 associated'
  if ( associated(d2) ) print *, 'd2 associated'
  print *,'SUCCESS'

end program tst_dict_valgrind
