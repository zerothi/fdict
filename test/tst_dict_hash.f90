program tests

  use tst_utils

  implicit none

  integer :: iu, iostat
  character(len=200) :: line
  integer, target :: i0
  type(dictionary_t) :: d

  ! initialize
  line = ' '
  i0 = 1
  iu = 102
  open(iu, file='list_words_en.dat',action='read')
  do
     read(iu,*,iostat=iostat) line
     ! For errors, exit the loop
     if ( iostat /= 0 ) exit

     ! add to dictionary
     d = d // (trim(line).kvp.i0)
     
  end do
  
  ! close the file handle
  close(iu)

  ! print found values
  print '(2(a,i0),a)','Of ',len(d),' keys ', &
       hash_coll(d),' collide hash values'
  print '(2(a,i0),a)','Of ',len(d),' keys ', &
       hash_coll(d,max=.true.),' maximum at one hash value'

  call delete(d,dealloc=.false.)

  print *,'SUCCESS'
  
end program tests
