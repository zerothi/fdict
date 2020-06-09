program test_dict

  use tst_utils

  implicit none

  type(dictionary_t) :: dic
  integer :: i, N, step

  dic = dic // ('v1'.kv.1) // ('v1'.kv.2)
  dic = dic // ('v2'.kv.1) // ('v3'.kv.2)
  dic = dic // ('v2'.kv.1)

  call extend(dic, ('v2'.kv.1))

  ! print all the values
  call print(dic)

  call delete(dic)

end program test_dict
