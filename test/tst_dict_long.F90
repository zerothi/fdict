program tst_dict

  use tst_utils

  implicit none

  integer, parameter :: N = 26
  ! create a really long dict (N**3)
  type(dictionary_t) :: d1,d2
  character(len=10) :: key
  integer :: i,j,k,l,tot

  tot = 0
  do l = 1 , 1
  do k = 1 , N
     do j = 1 , N
        do i = 1 , N
           key = achar(64+i)//achar(64+j)//achar(64+k)//achar(64+l)
           d1 = d1 // ('z'//key.kv.1)
           call extend(d2,(key.kv.1))
           tot = tot + 1
        end do
     end do
  end do
  end do

  print '(3(a,i0))','Added ',2*tot,' keys and looped through them...',len(d1)+len(d2),' vs. real LL ',llen(d1)+llen(d2)

  ! Concatenating two long dicts
  d1 = d1 // d2
  print '(3(a,i0))','Concatenated two ',tot,' keys... ',len(d1),' vs. real LL ',llen(d1)
  print '(2(a,i0),a)','Of ',len(d1),' keys ',hash_coll(d1),' collide hash values'

  print '(2(a,i0),a)','Of ',len(d1),' keys ',hash_coll(d1,max=.true.),' are the maximum same hash values'

  call delete(d1)
  ! Now we cannot delete d2
  call nullify(d2)

  print '(2(a,i0))','Deleted ',2*tot,' keys...',len(d1)+len(d2)+llen(d1)+llen(d2)

end program tst_dict
