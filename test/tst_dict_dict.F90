program tests

  use tst_utils

  implicit none

  real(dp) :: va(400,400) ! roughly 1.22 MB
  real(dp) :: vb(400,400) ! roughly 1.22 MB
  type(dictionary_t) :: dic1, dic2, dic_va
  type(variable_t) :: av

  ! Create dictionary
  va = 1._dp

  ! Copying values
  dic_va = ('va'.kv.va)
  
  ! Create dict of dict
  ! Insert dictionary reference into dict
  dic1 = ('dict'.kvp.dic_va)
  ! retrieve dictionary
  call associate(dic2,dic1,'dict')
  ! retrieve value
  call associate(av,dic2,'va')
  ! retrive values
  call assign(vb,av)
  call assert(vb == va,'Retrieval of values not correct')
  vb = 0._dp
  call assign(vb,dic2,'va')
  call assert(vb == va,'Retrieval of values not correct')

  call nullify(dic1,'dict')
  print *,'dic1',len(dic1)
  ! If we delete dic2, we delete dic_va, so we should not delete it
  ! We do not even need to nullify it (the dicitonary
  !call delete(dic2,dealloc=.false.)
  print *,'dic_va'
  call delete(dic_va)
  call nullify(av)

end program tests
