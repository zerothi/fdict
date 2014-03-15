program test_dict

  use variable
  use dictionary

  implicit none

  real :: a, b(2),c(2,2)
  real, pointer :: d => null(),e=>null()
  type(var) :: v
  type(dict) :: dic

  a = 1.
  b = 2.
  c = 3.

  allocate(d)
  d = 4.

  ! fill dictionary
  dic = ('a'.kv.a)//('b'.kv.b)//('c'.kv.c)//('d'.kvp.d)// &
       ('string'.kv."Hello world")

  ! print all the values
  call dict_print(dic)

  ! assign to v the value in dic[a]
  call dict_assign(v,dic,'a')

  ! delete a
  call delete(dic,'a')
  print *,v%s0

  ! assign to v the value in dic[a]
  call delete(v)
  call dict_associate(v,dic,'d')

  call remove(dic,'d')
  print *,v%s0,d
  print *,len(dic)
  
  ! print all the values
  call dict_print(dic)

end program test_dict
