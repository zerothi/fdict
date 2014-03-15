program test_dict

  use variable
  use dictionary

  implicit none

  real :: a, b(2),c(2,2)
  real, pointer :: d => null(),e=>null()
  type(var) :: v
  type(dict) :: dic, tmp

  a = 1.
  b = 2.
  c = 3.

  allocate(d)
  d = 4.

  ! fill dictionary
  dic = &
       ('a'.kv.a)//('b'.kv.b)//('c'.kv.c)//('d'.kvp.d)// &
       ('string'.kv."Hello world")

  call add(dic, &
       ('aa'.kv.a)//('bb'.kv.b)//('cc'.kv.c)//('dd'.kvp.d)// &
       ('stringa'.kv."Hello world"))

  ! print all the values
  call dict_print(dic)

  tmp = .first. dic
  do while ( .not. .empty. tmp )
     call assign(v,.val.tmp)
     tmp = .next. tmp
  end do

end program test_dict
