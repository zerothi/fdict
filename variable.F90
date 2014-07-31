! Generic purpose variable as in any scripting language
! It has the power to transform into any variable at any time
! This means
module variable

  use iso_var_str

  implicit none

  private 

  integer, parameter :: ih = selected_int_kind(4)
  integer, parameter :: is = selected_int_kind(9)
  integer, parameter :: il = selected_int_kind(18)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)
  
  type :: var
     character(len=2) :: t = '  '
     ! The encoding placement of all data
     character(len=1), dimension(:), allocatable :: enc
  end type var
  public :: var

!  public :: size
  interface which
     module procedure which_
  end interface which
  public :: which
  interface delete
     module procedure delete_
  end interface delete
  public :: delete
  interface nullify
     module procedure nullify_
  end interface nullify
  public :: nullify

  interface print
     module procedure print_
  end interface print
  public :: print

#include "var_interface.inc"

contains

  subroutine print_(this)
    type(var), intent(in) :: this
    write(*,'(t2,a)') this%t
  end subroutine print_

  function which_(this) result(t)
    type(var), intent(in) :: this
    character(len=2) :: t
    t = this%t
  end function which_
    
  subroutine delete_(this)
    type(var), intent(inout) :: this
#include "var_declarations.inc"
#include "var_delete.inc"
    call nullify(this)
  end subroutine delete_

  subroutine nullify_(this)
    type(var), intent(inout) :: this
    this%t = '  '
    if ( allocated(this%enc) ) deallocate(this%enc)
  end subroutine nullify_

  subroutine assign_var(this,rhs,dealloc)
    type(var), intent(inout) :: this
    type(var), intent(in) :: rhs
    logical, intent(in), optional :: dealloc
    logical :: ldealloc
#include "var_declarations2.inc"
    ! collect deallocation option (default as =)
    ! ASSIGNMENT in fortran is per default destructive
    ldealloc = .true.
    if(present(dealloc))ldealloc = dealloc
    if (.not. ldealloc) then
       ! if we do not deallocate, nullify
       call nullify(this)
    else
       call delete(this)
    end if
    this%t = rhs%t
    ! First allocate the LHS
#include "var_var_alloc.inc"

    ! copy over RHS and Save encoding
#define ASS_ACC =
#include "var_var_set.inc"
#undef ASS_ACC

  end subroutine assign_var

  subroutine associate_var(this,rhs,dealloc,success)
    type(var), intent(inout) :: this
    type(var), intent(in) :: rhs
    logical, intent(in), optional :: dealloc
    logical, intent(out), optional :: success
    logical :: ldealloc
    ! collect deallocation option (default as =)
    ! ASSOCIATION in fortran is per default non-destructive
    ldealloc = .false.
    if ( present(success) ) success  = .true.
    if ( present(dealloc) ) ldealloc = dealloc
    if (.not. ldealloc) then
       ! if we do not deallocate, nullify
       call nullify(this)
    else
       call delete(this)
    end if
    
    ! Association is done by copying the encoding
    this%t = rhs%t
    allocate(this%enc(size(rhs%enc)))
    this%enc = rhs%enc

  end subroutine associate_var

  pure function associatd_var(this,rhs) result(ret)
    type(var), intent(in) :: this
    type(var), intent(in) :: rhs
    logical :: ret
#include "var_declarations2.inc"
    ret = this%t==rhs%t
    if ( .not. ret ) return
    
#include "var_var_assoc.inc"
    
  end function associatd_var

  subroutine assign_set_char0(this,rhs,dealloc)
    type(var), intent(inout) :: this
    character(len=*), intent(in) :: rhs
    logical, intent(in), optional :: dealloc
    type(var_str) :: str
    str = rhs
    call assign(this,str,dealloc=dealloc)
  end subroutine assign_set_char0

  subroutine assign_get_char0(lhs,this,success)
    character(len=*), intent(out) :: lhs
    type(var), intent(inout) :: this
    logical, intent(out), optional :: success
    type(var_str) :: str
    logical :: lsuccess
    call assign(str,this,success=lsuccess)
    if ( present(success) ) success = lsuccess
    if ( lsuccess ) lhs = str
  end subroutine assign_get_char0
  
#include "var_funcs.inc"
  
end module variable

