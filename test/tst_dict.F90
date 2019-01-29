program tst_dict

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
  dic = &
       ('a'.kv.a)//('b'.kv.b)//('c'.kv.c)//('d'.kvp.d)// &
       ('string'.kv."Hello world")
  if ( len(dic) /= 5 ) stop 9

  call extend(dic, &
       ('aa'.kv.a)//('bb'.kv.b)//('cc'.kv.c)//('dd'.kvp.d)// &
       ('stringa'.kv."Hello world"))
  if ( len(dic) /= 10 ) stop 9

  ! print all the values
  call print(dic)

  tmp = .first. dic
  i = 0
  do while ( .not. .empty. tmp )
     call assign(v,.val.tmp)
     tmp = .next. tmp
     i = i + 1
  end do
  if ( i /= 10 ) stop 9

  call nullify(dic, 'dd')
  call delete(v)
  call delete(dic)
  nullify(d) ! deleted through v

  print *,'SUCCESS'

end program tst_dict
