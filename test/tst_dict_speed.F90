program tst_dict

  use tst_utils

  implicit none

  integer, parameter :: N = 100, ITT = 300000
  ! create a really long dict (N**3)
  type(dictionary_t) :: d
  character(len=10) :: key
  integer :: i,j, itot
  real :: tic, toc, tot(N), mean, std

  do i = 1 , N
     key = achar(64+i)
     d = d // ('z'//key.kv.1)
  end do

  ! Loop through them 10,000,000 times
  ! for item in the dict
  tot = 0.
  do j = 1 , N
     key = 'z'//achar(64+j)
     call cpu_time(tic)
     itot = 0
     do i = 1 , ITT
        if ( key .in. d ) then
           itot = itot + 1
           ! do nothing
        end if
     end do
     call cpu_time(toc)
     tot(j) = toc - tic
  end do
  mean = sum(tot) / N
  print '(2(a,i0),a,f15.10,a)','Searched ',N,' items ',ITT,' times (mean) ',&
       mean,' seconds'
  ! Calc std dev.
  std = sqrt(sum((tot - mean)**2)/N)
  print '(2(a,i0),a,f15.10,a)','Searched ',N,' items ',ITT,' times (std) ',&
       std,' seconds'
  
  key = 'ybeuh'
  call cpu_time(tic)
  itot = 0
  do j = 1 , N
     do i = 1 , ITT
        if ( key .nin. d ) then
           itot = itot + 1
        end if
     end do
  end do
  call cpu_time(toc)

  toc = (toc-tic)/N
  print '(a,i0,a,f15.10,a)','Searched for missing item ',ITT,' times ',&
       toc,' seconds'
  print '(a,f15.10,a)','Difference from std+mean ',mean+std-toc,' seconds'

  call delete(d)
  
end program tst_dict
