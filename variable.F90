! Generic purpose variable as in any scripting language
! It has the power to transform into any variable at any time
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

  ! Specific routines for passing types to variables
  interface associate_type
     module procedure associate_type_
  end interface associate_type
  public :: associate_type
  interface enc
     module procedure enc_
  end interface enc
  public :: enc
  interface size_enc
     module procedure size_enc_
  end interface size_enc
  public :: size_enc


#include "var_interface.inc"

contains

  subroutine print_(this)
    type(var), intent(in) :: this
    write(*,'(t2,a)') this%t
  end subroutine print_

  elemental function which_(this) result(t)
    type(var), intent(in) :: this
    character(len=2) :: t
    t = this%t
  end function which_
    
  subroutine delete_(this,dealloc)
    type(var), intent(inout) :: this
    logical, intent(in), optional :: dealloc
    logical :: ldealloc
#include "var_declarations.inc"
    ldealloc = .true.
    if ( present(dealloc) ) ldealloc = dealloc
    if ( ldealloc ) then
#include "var_delete.inc"
    end if
    call nullify(this)
  end subroutine delete_

  elemental subroutine nullify_(this)
    type(var), intent(inout) :: this
    this%t = '  '
    if ( allocated(this%enc) ) deallocate(this%enc)
  end subroutine nullify_


  ! Returns the bare encoding of this variable
  ! This can ease the process of assigning
  ! user-types to a variable.
  ! An encoding might be 2, or 10000000 bytes big.
  ! Therefore we use a subroutine to determine
  ! the size of the returning encoding characters.
  ! If the size of the returning enc is not 
  ! big enough it will be reset to ' '
  subroutine enc_(this,enc)
    type(var), intent(in) :: this
    character(len=1), intent(out) :: enc(:)
    integer :: i
    if ( this%t == '  ' ) then
       enc = ' '
    else
       ! We do have an encoding
       i = size(this%enc)
       if ( i > size(enc) ) then
          enc = ' '
       else
          enc(1:i) = this%enc
       end if
    end if
  end subroutine enc_

  function size_enc_(this) result(len)
    type(var), intent(in) :: this
    integer :: len
    if ( this%t == '  ' ) then
       len = 0
    else
       len = size(this%enc)
    end if
    
  end function size_enc_

  ! We allow the user to pass an encoding field.
  ! As this is the same as passing a char
  ! we MUST use a specific routine for this.
  ! One _could_, in principle, add an optional
  ! logical flag for the assign_var_char0, however
  ! one cannot assign a type by passing a reference
  ! and hence we ONLY allow associate_type
  ! This also means that any de-allocation of variables
  ! containing an external type will only de-reference it.
  ! A bit counter-intuitive, yet the variable type needs
  ! all information about the type to successfully de-allocate it.
  ! It is ALSO very important that the user
  ! passed the full-encoding WITHOUT padding of ' '.
  ! We cannot know for sure whether the encoding actually terminates
  ! in a bit corresponding to char(' ')!
  subroutine associate_type_(this,enc,dealloc)
    type(var), intent(inout) :: this
    character(len=1), intent(in) :: enc(:)
    logical, intent(in), optional :: dealloc
    logical :: ldealloc
    ldealloc = .false.
    if(present(dealloc))ldealloc = dealloc
    if (.not. ldealloc) then
       ! if we do not deallocate, nullify
       call nullify(this)
    else
       call delete(this)
    end if
    this%t = 'ut'
    allocate(this%enc(size(enc)))
    this%enc = enc

  end subroutine associate_type_

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
#include "var_var_set.inc"

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

