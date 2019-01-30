program test_dict

  use tst_utils

  implicit none

  real :: a, b(2),c(2,2)
  real, pointer :: d => null()
  type(variable_t) :: v
  type(dictionary_t) :: dic

#include "variable_declarations_.inc"

  a = 1.
  b = 2.
  c = 3.

  allocate(d)
  d = 4.

  ! fill dictionary
  dic = ('a'.kv.a)//('b'.kv.b)//('c'.kv.c)//('d'.kvp.d)// &
       ('string'.kv."Hello world")

  ! print all the values
  call print(dic)

  ! assign to v the value in dic[a]
  call assign(v,dic,'a')

  ! delete a
  call delete(dic,'a')
  ps0 = transfer(v%enc,ps0)
  print *,'After delete, ensure assign:',ps0%p,a

  ! associate to v the value in dic[a]
  call delete(v)
  call associate(v,dic,'d')

  call nullify(dic,'d')
  ! As the memory reference is the same,
  ! we will see the same result here. 
  ! However, the value in the key HAS been
  ! deleted.
  ps0 = transfer(v%enc,ps0)
  print *,ps0%p,d
  ! In certain cases, deallocation and immediate
  ! allocation allows to test whether the same 
  ! memory element is refenced in v
  deallocate(d) ; nullify(d)
  allocate(d)
  d = 2
  ps0 = transfer(v%enc,ps0)
  print *,ps0%p,d

  print *,'Length:',len(dic),3
  
  ! print all the values
  call print(dic)

  deallocate(d)
  call delete(dic)

  print *,'SUCCESS'

end program test_dict
