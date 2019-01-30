! @LICENSE@, see README.md

! Generic purpose variable as in any scripting language
! It has the power to transform into any variable at any time
module variable
  !! A type-free variable module to contain _any_ data in fortran.
  !!
  !! This module implements a generic variable-type (`type(variable_t)`)
  !! which may contain _any_ data-type (even user-derived type constructs).
  !!
  !! Its basic usage is somewhat different than the regular assignment
  !! in fortran.
  !!
  !! Example:
  !!
  !!```fortran
  !! real :: r
  !! real :: ra(10)
  !! real, target :: rb(10)
  !! type(variable_t) :: v
  !! call assign(v, r) ! v now contains value of r
  !! call assign(v, ra) ! v now contains array with values of ra
  !! call delete(v) ! delete content
  !! call associate(v, ra) ! v now contains a pointer to rb
  !! call assign(ra, v) ! copies data from rb to ra
  !!```
  !!
  !! The assignment routine behaves like `=` (delete old value)
  !! whereas the associate routine behaves like `=>` (nullify old value).
  !!
  !! The data-types allowed in this type is *not* limited by this
  !! module, but we currently allow integers, reals, complex and C-pointers.

  ! Load the iso_c_binding for containing a C-pointer
  use, intrinsic :: iso_c_binding
  
  implicit none

  private 

  integer, parameter :: ih = selected_int_kind(4)
  integer, parameter :: is = selected_int_kind(9)
  integer, parameter :: il = selected_int_kind(18)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)

  ! To create a constant transfer data-type of the 
  ! pointer methods
  character(len=1) :: local_enc_type(1)

  ! Internal variable to hold the size of the "type" switch
  !> Maximum character length of the type specifier in the variable, no
  !! unique identifier may be longer than this.
  integer, parameter, public :: VARIABLE_TYPE_LENGTH = 4

  type :: variable_t
     !! Container for _any_ fortran data-type, intrinsically handles all
     !! from fortran and any external type may be added via external routines.
     !!
     !! The container is based on a type-transfer method by storing a pointer
     !! to the data and transfer the type to a character array via encoding.
     !! This enables one to retrieve the pointer position later and thus enables
     !! pointer assignments and easy copying of data.
     
     character(len=VARIABLE_TYPE_LENGTH) :: t = '    '
     ! The encoding placement of all data
     character(len=1), dimension(:), allocatable :: enc
  end type variable_t
  public :: variable_t

  interface which
     !! Type of content stored in the variable (`character(len=VARIABLE_TYPE_LENGTH)`)
     module procedure which_
  end interface
  public :: which
  interface delete
     !! Delete the variable (equivalent to `deallocate(<>)`).
     module procedure delete_
  end interface
  public :: delete
  interface nullify
     !! Nullify the variable (equivalent to `nullify(<>)`).
     module procedure nullify_
  end interface
  public :: nullify

  interface print
     !! Print (to std-out) information regarding the variable, i.e. the type.
     module procedure print_
  end interface
  public :: print

  ! Specific routines for passing types to variables
  interface associate_type
     module procedure associate_type_
  end interface
  public :: associate_type
  interface enc
     !! The encoding of the stored pointer (`character, dimension(:)`)
     !!
     !! This is mainly intenteded for internal use to transfer between real
     !! data and the data containers.
     !!
     !! It is however required to enable external type storage routines.
     module procedure enc_
  end interface
  public :: enc
  interface size_enc
     !! The size of the encoding character array (`size(enc(<>))`)
     !!
     !! This is mainly intenteded for internal use to transfer between real
     !! data and the data containers.
     module procedure size_enc_
  end interface
  public :: size_enc


  ! Specific routine for packing a character(len=*) to
  ! character(len=1) (:)
  interface cpack
     !! Convert a `character(len=*)` to `character, dimension(:)`
     !!
     !! A routine requirement for creating pointers to character storages.
     !! One can convert from `len=*` to an array of `len=1` and back using [[cunpack]].
     !!
     !! Because fortran requires dimensions of arrays assignments to be same size it
     !! one has to specify ranges if the length of the character is not equivalent
     !! to the size of the array.
     !!
     !! Example:
     !!
     !!```fortran
     !! character(len=20) :: a
     !! character :: b(10)
     !! a = 'Hello'
     !! b(1:5) = cpack('Hello')
     !!```
     !!
     !! @note
     !! This is a requirement because it is not possible to create a unified pointer
     !! to arbitrary length characters. Hence we store all `len=*` variables as `len=1` character arrays.
     module procedure cpack_
  end interface cpack
  public :: cpack

  ! Specific routine for packing a character(len=*) to
  ! character(len=1) (:)
  interface cunpack
     !! Convert a `character(len=1), dimensions(:)` to `character(len=*)`
     !!
     !! Pack an array into a character of arbitrary length.
     !! This convenience function helps converting between arrays of characters
     !! and fixed length characters.
     !!
     !! As character assignment is not restricted similarly as array assignments
     !! it is not a requirement to specify ranges when using this function.
     module procedure cunpack_
  end interface cunpack
  public :: cunpack

#include "variable_interface_.inc"

contains

  subroutine print_(this)
    type(variable_t), intent(in) :: this
    write(*,'(t2,a)') this%t
  end subroutine print_

  elemental function which_(this) result(t)
    type(variable_t), intent(in) :: this
    character(len=VARIABLE_TYPE_LENGTH) :: t
    t = this%t
  end function which_
    
  subroutine delete_(this,dealloc)
    type(variable_t), intent(inout) :: this
    logical, intent(in), optional :: dealloc
    logical :: ldealloc
#include "variable_declarations_.inc"
    integer :: i

    ldealloc = .true.
    if ( present(dealloc) ) ldealloc = dealloc
    if ( ldealloc ) then
#include "variable_delete_.inc"
       
       if ( this%t == 'a-' ) then
          pa_ = transfer(this%enc,pa_)
          do i = 1 , size(pa_%p)
             deallocate(pa_%p(i)%p)
          end do
          deallocate(pa_%p)
       end if
    end if
    call nullify(this)
  end subroutine delete_

  elemental subroutine nullify_(this)
    type(variable_t), intent(inout) :: this
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
    type(variable_t), intent(in) :: this
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
    type(variable_t), intent(in) :: this
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
  ! logical flag for the assign_set_a_, however
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
    type(variable_t), intent(inout) :: this
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
    this%t = 'USER'
    allocate(this%enc(size(enc)))
    this%enc(:) = enc

  end subroutine associate_type_

  function cpack_(c) result(car)
    character(len=*), intent(in) :: c
    character(len=1) :: car(len(c))
    integer :: i
    
    do i = 1 , len(c)
       car(i) = c(i:i)
    end do
    
  end function cpack_
  function cunpack_(car) result(c)
    character(len=1), intent(in) :: car(:)
    character(len=size(car)) :: c
    integer :: i

    do i = 1 , size(car)
       c(i:i) = car(i)
    end do
    
  end function cunpack_
  
  subroutine assign_var(this,rhs,dealloc)
    type(variable_t), intent(inout) :: this
    type(variable_t), intent(in) :: rhs
    logical, intent(in), optional :: dealloc
    logical :: ldealloc
    integer :: i
#include "variable_declarations2_.inc"
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
#include "variable_variable_alloc_.inc"

    if ( this%t == 'a-' ) then ! character(len=*)
       pa__2 = transfer(rhs%enc, pa__2)
       allocate(pa__1%p(size(pa__2%p)))
       do i = 1 , size(pa__2%p)
          allocate(pa__1%p(i)%p)
          pa__1%p(i)%p = pa__2%p(i)%p
       end do
       allocate(this%enc(size(transfer(pa__1, local_enc_type))))
       this%enc(:) = transfer(pa__1, local_enc_type)
    end if

    ! copy over RHS and Save encoding
#include "variable_variable_set_.inc"

  end subroutine assign_var

  subroutine associate_var(this,rhs,dealloc,success)
    type(variable_t), intent(inout) :: this
    type(variable_t), intent(in) :: rhs
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
    this%enc(:) = rhs%enc

  end subroutine associate_var

  pure function associatd_var(this,rhs) result(ret)
    type(variable_t), intent(in) :: this
    type(variable_t), intent(in) :: rhs
    logical :: ret
#include "variable_declarations2_.inc"
    ret = this%t==rhs%t
    if ( .not. ret ) return
    
#include "variable_variable_assoc_.inc"
    
  end function associatd_var

  ! The character(len=*) is a bit difficult because
  ! there is no way to generate a specific type for _all_
  !   len=1,2,3,...
  ! variables.
  ! Instead we convert the character to char(len=1)
  ! and store a pointer to this.
  ! This ensures that it can be retrieved (via associate)
  ! and mangled through another variable type
  subroutine assign_set_a0_0(this,rhs,dealloc)
    type(variable_t), intent(inout) :: this
    character(len=*), intent(in) :: rhs
    logical, intent(in), optional :: dealloc
    character(len=1), pointer :: c(:) => null()
    integer :: i
    allocate(c(len(rhs)))
    do i = 1 , size(c)
       c(i) = rhs(i:i)
    end do
    ! This is still a "copy"
    call associate(this, c, dealloc)
    nullify(c)
  end subroutine assign_set_a0_0
  subroutine assign_get_a0_0(lhs,this,success)
    character(len=*), intent(out) :: lhs
    type(variable_t), intent(inout) :: this
    logical, intent(out), optional :: success
    character(len=1), pointer :: c(:) => null()
    logical :: lsuccess
    integer :: i
    call associate(c, this, success=lsuccess)
    if ( lsuccess ) lsuccess = len(lhs) >= size(c)
    if ( present(success) ) success = lsuccess
    lhs = ' '
    if ( .not. lsuccess ) return
    do i = 1 , size(c)
       lhs(i:i) = c(i)
    end do
  end subroutine assign_get_a0_0


#ifdef NOT_WORKING
  ! This routine is actually working, but
  ! we do not allow it.
  ! One should use the len=1 version of the characters.
  subroutine associate_set_a0_0(this,rhs,dealloc)
    type(variable_t), intent(inout) :: this
    character(len=*), intent(in), target :: rhs
    logical, intent(in), optional :: dealloc
    logical :: ldealloc
    type :: pta_
       type(pta__), pointer :: p(:) => null()
    end type pta_
    type :: pta__
       character(len=1), pointer :: p => null()
    end type pta__
    type(pta_) :: p
    integer :: i
    ! ASSOCIATION in fortran is per default non-destructive
    ldealloc = .false.
    if(present(dealloc))ldealloc = dealloc
    if (ldealloc) then
       call delete(this)
    else
       call nullify(this)
    end if
    ! With pointer transfer we need to deallocate
    ! else bounds might change...
    this%t = 'a-'
    allocate(p%p(len(rhs)))
    do i = 1 , len(rhs)
       p%p(i)%p => rhs(i:i)
    end do
    allocate(this%enc(size(transfer(p, local_enc_type)))) ! allocate encoding
    this%enc(:) = transfer(p, local_enc_type) ! transfer pointer type to the encoding
    nullify(p%p)
  end subroutine associate_set_a0_0

  !! THIS IS NOT WORKING
  subroutine associate_get_a0_0(lhs,this,dealloc,success)
    character(len=*), pointer :: lhs
    type(variable_t), intent(in) :: this
    logical, intent(in), optional :: dealloc
    logical, intent(out), optional :: success
    logical :: ldealloc, lsuccess
    type :: pt
       type(ptc), pointer :: p(:) => null()
    end type pt
    type :: ptc
       character(len=1), pointer :: p => null()
    end type ptc
    type(pt) :: p
    integer :: i, ns
    lsuccess = this%t == 'a-'
    if ( lsuccess ) then
       p = transfer(this%enc, p)
       ! Figure out the trimmed length of the string
       ! If it is smaller or equal to the output string
       ! then all is good, else we consider it a failure
       ns = size(p%p)
       do i = size(p%p), 1, -1
          if ( p%p(i)%p == ' ' ) then
             ns = i-1
          else
             exit
          end if
       end do
       lsuccess = len(lhs) >= ns
    end if
    ldealloc = .false.
    if( present(dealloc) ) ldealloc = dealloc
    if ( ldealloc .and. associated(lhs) ) deallocate(lhs)
    nullify(lhs)
    if ( present(success) ) success = lsuccess
    if ( .not. lsuccess ) return
    ns = min(ns, size(p%p))
    do i = 1, ns
       lhs(i:i) => p%p(i)%p(1:1)
    end do
  end subroutine associate_get_a0_0
#endif

#include "variable_funcs_.inc"
  
end module variable

