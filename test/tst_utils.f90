module tst_utils

  use iso_var_str
  use variable
  use dictionary

  implicit none
  
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)

contains

  subroutine show_mem()
    call system("free | grep Mem | awk '{print $1,$2/1024,$3/1024,$4/1024}'")
  end subroutine show_mem

end module tst_utils
