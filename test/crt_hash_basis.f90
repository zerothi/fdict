program fnv_hash

  integer :: hash, hash_mod
  ! this _is_ 32 bit, but in a
  integer :: FNV_prime = 16777619
  integer :: offset
  character(len=36) :: b = 'fdict \\./o\\/ <Nick Rubner Papior>'
  integer :: i
  
  print '(2a)','Basis offset: ',trim(b)
  offset = 0
  hash_mod = 2**16
  do i = 1 , len_trim(b)
     offset = ieor(offset,iachar(b(i:i)))
     offset = mod(offset * FNV_prime,hash_mod)
  end do

  print *,offset
  
end program fnv_hash
