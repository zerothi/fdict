! A simple dictionary module for a key-value based system...
! 
! This module has entirely been created by:
! Nick Papior Andersen, copyright 2012.
! nickpapior@gmail.com
!
! Only to be used for not-for-profit development/applications.
module dictionary

  use variable

  implicit none

  private

  integer, parameter :: is = selected_int_kind(5)
  integer, parameter :: il = selected_int_kind(16)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)

  public :: dict
  ! Create a dict type: 'key' .KV. 'val', 'key' .KVP. 'pointer'
  public :: operator(.KV.), operator(.KVP.)
  ! Retrieve the value of a list of dicts by searching for a key: type(dict)(:) .LU. 'a' (returns .VAL. from 'a'.KV.'val')
  ! 'Look-Up'
  public :: operator( .LU. ), operator( .LOOKUP. )
  ! Concatenates lists of dicts or just two dicts: type(dict)(:) //('a'.KV.'val')
  public :: operator( // )
  ! Retrieve the key of a dict: .KEY. type(dict)
  public :: operator(.KEY.)
  ! Retrieve the value of a dict: .VAL. type(dict)
  public :: operator(.VAL.)
  public :: operator(.EQ.) ! Overloaded
  public :: operator(.NE.) ! Overloaded
  ! The unary operator of retrieving the next element
  public :: operator(.NEXT.)
  ! The unary operator of retrieving the next element
  public :: operator(.FIRST.)
  ! The unary operator of checking for emptiness
  public :: operator(.EMPTY.)
  ! The unary operator of returning the hash value
  public :: operator(.HASH.), hash
  ! Return the length of the dictionary...
  public :: LEN
  public :: dict_copy, dict_print
  

  ! Internal variables for determining the maximum size of the dictionaries.
  ! We could consider changing this to a variable size string
  ! However, that will increase the dependencies and will most likely not yield
  ! a better interface.
  integer, parameter :: DICT_KEY_LENGTH   = 50
  
  ! A parameter returned if not found.
  character(len=DICT_KEY_LENGTH), parameter :: DICT_NOT_FOUND = 'ERROR: key not found'
  public :: DICT_NOT_FOUND

  ! We need to create a linked list to create arbitrarily long dictionaries...
  ! The dictionary entry is not visible outside.
  type :: d_entry
     private
     character(len=DICT_KEY_LENGTH) :: key = ' '
     ! in order to extend the dictionary to contain a dictionary
     ! we simply need to add the dictionary type to the variable
     ! library.
     type(var) :: value
     integer :: hash = 0
     type(d_entry), pointer :: next => null()
  end type d_entry
  
  type :: dict
     ! We will keep the dictionary private so that any coding
     ! has to use .KEY. and .VAL. etc.
     private
     type(d_entry), pointer :: first => null()
     integer :: len = 0
  end type dict
  
  ! HASH-comparisons are MUCH faster...
  ! hence we store all values in an incremental fashion in terms
  ! of the HASH-value
  integer, parameter :: HASH_SIZE = 9857 ! a prime !
  integer, parameter :: HASH_MULT = 31
  
  ! Retrieve the value from a dictionary list by specifying the key...
  interface operator( .LU. )
     module procedure d_value_from_key
  end interface operator( .LU. )
  interface operator( .LOOKUP. )
     module procedure d_value_from_key
  end interface operator( .LOOKUP. )


  ! Concatenate dicts or list of dicts to list of dicts
  interface operator( // )
     module procedure d_cat_d
     module procedure ds_cat_d
     module procedure d_cat_ds
  end interface operator( // )

  
  ! Retrieve the key from a dictionary (unary)
  interface operator( .KEY. )
     module procedure key
  end interface operator( .KEY. )
  
  ! Retrieve the value from a dictionary (unary)
  interface operator( .VAL. )
     module procedure value
  end interface operator( .VAL. )

  ! Retrieve the hash value from a dictionary (unary)
  interface operator( .HASH. )
     module procedure hash
  end interface operator( .HASH. )

  ! Checks for two dicts have all the same keys
  interface operator( .EQ. )
     module procedure d_eq_d
  end interface operator( .EQ. )

  ! Checks for two dicts don't share any common keys
  interface operator( .NE. )
     module procedure d_ne_d
  end interface operator( .NE. )

  ! Steps one time in the dictionary (unary)
  interface operator( .NEXT. )
     module procedure d_next
  end interface operator( .NEXT. )

  ! Retrieve the first of a dictionary (unary)
  interface operator( .FIRST. )
     module procedure d_first
  end interface operator( .FIRST. )

  ! Check whether the dictionary is empty (unary)
  interface operator( .EMPTY. )
     module procedure d_empty
  end interface operator( .EMPTY. )

  ! Create a dictionary type from 
#include 'dict_interface.inc'

contains

  pure function hash_val(key) result(val)
    character(len=*), intent(in) :: key
    integer :: val
    integer :: i
    val = 0
    do i = 1 , min(DICT_KEY_LENGTH,len_trim(key))
       val = val + iachar(key(i:i))
    end do
    ! A hash has to be distinguished from the "empty"
    val = 1 + mod(val*HASH_MULT,HASH_SIZE)
  end function hash_val

  pure function new_d_key(key) result(d)
    character(len=*), intent(in) :: key
    type(dict) :: d
    allocate(d%first)
    if ( len_trim(key) > DICT_KEY_LENGTH ) then
       d%first%key = key(1:DICT_KEY_LENGTH)
    else
       d%first%key = trim(key)
    end if
    d%first%hash = hash_val(key)
    d%len = 1
    nullify(d%first%next)
  end function new_d_key

  ! Retrieves the key value in a dictionary type (or a list)
  ! We expect that the key will only be called on single element dictionaries...
  function key(d)
    type(dict), intent(in) :: d
    character(len=DICT_KEY_LENGTH) :: key
    key = d%first%key
  end function key

  ! Retrieves the value value in a dictionary type (or a list)
  function value(d) 
    type(dict), intent(in) :: d
    type(var) :: value
    call assign(value,d%first%value)
  end function value

  ! Returns the hash value of the dictionary first item...
  function hash(d)
    type(dict), intent(in) :: d
    integer :: hash
    hash = d%first%hash
  end function hash

  ! Retrieves the value for a key in a list of dictionaries
  ! Will use .EQ. to tesh for their equivalence.
  function d_value_from_key(d,key) result(val)
    type(dict), intent(in) :: d
    character(len=*), intent(in) :: key
    type(var) :: val
    type(dict) :: ld
    integer :: hash
    ! empty the value
    call delete(val)
    hash = hash_val(key)
    ld = .first. d
    search: do while ( .not. (.empty. ld) )
       if (      hash  < .hash. ld ) then
          exit search
       else if ( hash  > .hash. ld ) then
          ! Do nothing... step
       else if ( hash == .hash. ld ) then
          if ( key .eq. .KEY. ld ) then
             val = .VAL. ld
             return
          end if
       end if
       ld = .next. ld
    end do search
  end function d_value_from_key

  ! Compares two dict types against each other
  ! Will do comparison, first by hash, and if that matches then
  ! for the key and value of the dict's
  function d_eq_d(d1,d2) result(bool)
    type(dict), intent(in) :: d1,d2
    logical :: bool
    type(dict) :: tmp1, tmp2
    bool = len(d1) == len(d2)
    if ( .not. bool ) return
    bool = .hash. d1 == .hash. d2
    if ( .not. bool ) return
    ! if all the keys are going to be the same
    ! the we know that the hash-tags are going to
    ! be the same... :)
    tmp1 = .first. d1
    tmp2 = .first. d2
    do while ( .not. (.empty. tmp1) )
       bool = .hash. tmp1 == .hash. tmp2
       if ( .not. bool ) return
       tmp1 = .next. tmp1
       tmp2 = .next. tmp2
    end do
  end function d_eq_d

  ! Compares two dict types against each other
  ! The negative of .EQ.
  function d_ne_d(d1,d2) result(bool)
    type(dict), intent(in) :: d1,d2
    logical :: bool
    type(dict) :: tmp1, tmp2
    tmp1 = .first. d1
    do while ( .not. (.empty. tmp1) )
       tmp2 = .first. d2
       do while ( .not. (.empty. tmp2) )
          bool = .hash. tmp1 == .hash. tmp2
          if ( bool ) then
             bool = .false.
             return
          end if
          tmp2 = .next. tmp2
       end do
       tmp1 = .next. tmp1
    end do
  end function d_ne_d

  ! Concatenate two dictionaries to one dictionary...
  ! it does not work with elemental as the 
  function d_cat_d(d1,d2) result(d)
    type(dict), intent(in) :: d1,d2
    type(dict) :: d
    type(d_entry), pointer :: ladd,lnext
    call dict_copy(d1,d)
    ladd => d2%first
    do 
       ! step ...
       lnext => ladd%next
       call d_insert(d,ladd)
       if ( .not. associated(lnext) ) return
       ladd => lnext
    end do
  end function d_cat_d

  function d_cat_ds(d,ds) result(this)
    type(dict), intent(in) :: d,ds(:)
    type(dict) :: this
    integer :: i
    call dict_copy(d,this)
    do i = 1 , size(ds)
       this = this//ds(i)
    end do
  end function d_cat_ds
  function ds_cat_d(ds,d) result(this)
    type(dict), intent(in) :: ds(:),d
    type(dict) :: this
    integer :: i
    call dict_copy(d,this)
    do i = 1 , size(ds)
       this = ds(i)//this
    end do
  end function ds_cat_d

  subroutine d_insert(d,entry)
    type(dict),    intent(inout) :: d
    type(d_entry), intent(inout), pointer :: entry
    type(d_entry), pointer :: search, prev
    nullify(prev)
    ! Initialize search...
    search => d%first
    ! The easy case...
    if ( search%hash > entry%hash ) then
       entry%next => d%first
       d%first => entry
       d%len = d%len + 1
       return
    else if ( search%hash == entry%hash ) then
       ! If the key already exists we will simply overwrite
       if ( search%key == entry%key ) then
          search%value = entry%value
          return
       end if
    end if
    search_loop: do 
       ! step...
       prev   => search
       ! step...
       search => prev%next
       if ( .not. associated(search) ) exit search_loop
       if ( search%hash > entry%hash ) then
          prev%next  => entry
          entry%next => search
          d%len = d%len + 1
          return
       else if ( search%hash == entry%hash ) then
          ! If the key already exists we will simply overwrite
          if ( search%key == entry%key ) then
             search%value = entry%value
             return
          end if
       end if
    end do search_loop
    prev%next => entry
    ! Increment length of the dictionary...
    d%len = d%len+1
    ! As we could insert from a dictionary we have to reset, to not do endless loops...
    nullify(entry%next)
  end subroutine d_insert

  ! Retrieve the length of the dictionary...
  pure function len(d)
    type(dict), intent(in) :: d
    integer :: len
    len = d%len
  end function len

  function d_next(d)
    type(dict), intent(in) :: d
    type(dict) :: d_next
    d_next%first => d%first%next
    d_next%len = len(d)-1
  end function d_next

  function d_empty(d)
    type(dict), intent(in) :: d
    logical :: d_empty
    d_empty = .not. associated(d%first)
  end function d_empty

  function d_first(d)
    type(dict), intent(in) :: d
    type(dict) :: d_first
    call dict_copy(d,d_first)
  end function d_first
  
  subroutine dict_copy(din,dcopy)
    type(dict), intent(in)  :: din
    type(dict), intent(out) :: dcopy
    dcopy%first => din%first
    dcopy%len = din%len
  end subroutine dict_copy

  subroutine dict_print(d)
    type(dict), intent(in)  :: d
    type(dict)  :: ld
    ld = .first. d
    d_loop: do 
       if ( .empty. ld ) exit d_loop
       write(*,*) trim(.key. ld) !,' : ',trim(.val. ld)
       ld = .next. ld
    end do d_loop
  end subroutine dict_print

#include 'dict_funcs.inc'

end module dictionary
