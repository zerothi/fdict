! Generic purpose variable as in any scripting language
! It has the power to transform into any variable at any time
! This means
module variable

  use iso_var_str

  implicit none

  private 

  integer, parameter :: is = selected_int_kind(5)
  integer, parameter :: il = selected_int_kind(16)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)
  
  type :: var
     character(len=2) :: t = '  '
#include 'var_content.inc'
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

#include 'var_interface.inc'

contains

  function which_(this) result(t)
    type(var), intent(in) :: this
    character(len=2) :: t
    t = this%t
  end function which_
    
  subroutine delete_(this)
    type(var), intent(inout) :: this
#include 'var_delete.inc'
    call nullify(this)
  end subroutine delete_

  subroutine nullify_(this)
    type(var), intent(inout) :: this
#include 'var_nullify.inc'
    this%t = '  '
  end subroutine nullify_

subroutine assign_v0(this,rhs,dealloc)
  type(var), intent(inout) :: this
  type(var), intent(in) :: rhs
  logical, intent(in), optional :: dealloc
  logical :: ldealloc
  ! collect deallocation option (default as =)
  ldealloc = .true.
  if(present(dealloc))ldealloc = dealloc
  if (.not. ldealloc) then
     ! if we don't deallocate, nullify
     call nullify(this)
     this%t = rhs%t

#include 'var_var_alloc.inc'

  else
     ldealloc = this%t /= rhs%t
     if (ldealloc) then
        call delete(this)
        this%t = rhs%t

#include 'var_var_alloc.inc'

     end if
  end if

#define ASS_ACC =
#include 'var_var_set.inc'

end subroutine assign_v0

subroutine associate_v0(this,rhs,dealloc)
  type(var), intent(inout) :: this
  type(var), intent(in) :: rhs
  logical, intent(in), optional :: dealloc
  logical :: ldealloc
  ! collect deallocation option (default as =)
  ldealloc = .true.
  if(present(dealloc))ldealloc = dealloc
  if (.not. ldealloc) then
     ! if we don't deallocate, nullify
     call nullify(this)
     this%t = rhs%t
  else
     ldealloc = this%t /= rhs%t
     if (ldealloc) then
        call delete(this)
        this%t = rhs%t
     end if
  end if

#define ASS_ACC =>
#include 'var_var_set.inc'

end subroutine associate_v0

pure function associatd_v0(this,rhs) result(ret)
  type(var), intent(in) :: this
  type(var), intent(in) :: rhs
  logical :: ret
  ret = this%t==rhs%t
  if ( .not. ret ) return
  
#include 'var_var_assoc.inc'

end function associatd_v0

#include 'var_funcs.inc'

end module variable

