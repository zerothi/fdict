module tst_utils

  ! Since we load iso_c_binding here we expose to all tests
  use, intrinsic :: iso_c_binding
  use variable
  use dictionary

  implicit none

  integer, parameter :: ih = selected_int_kind(4)
  integer, parameter :: is = selected_int_kind(9)
  integer, parameter :: il = selected_int_kind(18)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)

  interface assert
     module procedure assert_0d
     module procedure assert_1d
     module procedure assert_2d
  end interface assert

  private :: assert_0d, assert_1d, assert_2d
  
contains

  subroutine show_mem()
    call system("free | grep Mem | awk '{printf ""%s  %8.2f  %8.2f  %8.2f\n"", $1,$2/1024,$3/1024,$4/1024}'")
  end subroutine show_mem

  subroutine assert_0d(log,msg,good)
    logical, intent(in) :: log
    character(len=*) :: msg
    character(len=*), optional :: good
    if ( .not. log ) then
       write(*,*) msg
       stop
    else
       if ( present(good) ) then
          write(*,*)'SUCCESS '//trim(good)
       else
          write(*,*)'SUCCESS'
       end if
    end if
  end subroutine assert_0d
  subroutine assert_1d(log,msg,good)
    logical, intent(in) :: log(:)
    character(len=*) :: msg
    character(len=*), optional :: good
    if ( .not. all(log) ) then
       write(*,*) msg
       stop 9
    else
       if ( present(good) ) then
          write(*,*)'SUCCESS '//trim(good)
       else
          write(*,*)'SUCCESS'
       end if
    end if
  end subroutine assert_1d
  subroutine assert_2d(log,msg,good)
    logical, intent(in) :: log(:,:)
    character(len=*) :: msg
    character(len=*), optional :: good
    if ( .not. all(log) ) then
       write(*,*) msg
       stop 9
    else
       if ( present(good) ) then
          write(*,*)'SUCCESS '//trim(good)
       else
          write(*,*)'SUCCESS'
       end if
    end if
  end subroutine assert_2d

end module tst_utils
