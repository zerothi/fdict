! Generic purpose variable as in any scripting language
! It has the power to transform into any variable at any time
! This means
module variable

  implicit none

  private 

  integer, parameter :: is = selected_int_kind(5)
  integer, parameter :: il = selected_int_kind(16)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)
  
  type :: var
     character(len=2) :: t = '  '
#include 'types.inc'
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

#include 'mods.inc'
  public :: assign, associate
  public :: operator(.eq.), operator(.ne.)
  public :: operator(.lt.), operator(.le.)
  public :: operator(.gt.), operator(.ge.)

contains

  function which_(this) result(t)
    type(var), intent(in) :: this
    character(len=2) :: t
    t = this%t
  end function which_
    
  subroutine delete_(this)
    type(var), intent(inout) :: this
#include 'delete.inc'
  end subroutine delete_

  subroutine nullify_(this)
    type(var), intent(inout) :: this
#include 'nullify.inc'
    this%t = '  '
  end subroutine nullify_

#include 'funcs.inc'

end module variable

