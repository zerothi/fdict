program tst_dict_valgrind

  use variable
  use dictionary
  use tst_utils

  implicit none

  real :: a, b(2),c(2,2)
  real, pointer :: d => null()
  integer :: i
  type(variable_t) :: v
  type(dictionary_t) :: dic, tmp

  a = 1.
  b = 2.
  c = 3.

  allocate(d)
  d = 4.

  ! fill dictionary
  dic = ('a'.kv.a)
  dic = dic // ('b'.kv.b)
  dic = dic // ('c'.kv.c)
  dic = dic // ('d'.kvp.d)
  dic = dic // ('string'.kv.'Hello world')

  ! print all the values
  call print(dic)

  ! Assign first value
  call assign(v, dic, 'string')
  call delete(v)

  v = .val. dic
  call delete(v)

  call delete(dic)
  if ( associated(d) ) print *, 'd associated'
  print *,'SUCCESS'

end program tst_dict_valgrind
