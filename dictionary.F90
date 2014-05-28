! A simple dictionary module for a key-value based system...
! 
! This module has entirely been created by:
! Nick Papior Andersen, copyright 2012.
! nickpapior@gmail.com
!
! Only to be used for not-for-profit development/applications.
module dictionary

  use iso_var_str
  use variable

  implicit none

  private

  integer, parameter :: ih = selected_int_kind(4)
  integer, parameter :: is = selected_int_kind(9)
  integer, parameter :: il = selected_int_kind(18)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)

  ! the type
  public :: dict
  public :: dict_print

  ! Internal variables for determining the maximum size of the dictionaries.
  ! We could consider changing this to a variable size string
  ! However, that will increase the dependencies and will most likely not yield
  ! a better interface.
  integer, parameter :: DICT_KEY_LENGTH = 50
  
  ! A parameter returned if not found.
  character(len=DICT_KEY_LENGTH), parameter :: DICT_NOT_FOUND = 'ERROR: key not found'
  public :: DICT_NOT_FOUND
  
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
  integer, parameter :: HASH_SIZE = 62171 ! a prime !
  integer, parameter :: HASH_MULT = 31
  
  interface len
     module procedure len_
  end interface len
  public :: LEN

  ! Concatenate dicts or list of dicts to list of dicts
  interface operator( // )
     module procedure d_cat_d
  end interface operator( // )
  public :: operator( // )

  ! Retrieve the key from a dictionary (unary)
  interface operator( .KEY. )
     module procedure key
  end interface operator( .KEY. )
  public :: operator(.KEY.)

  ! check whether key exists in dictionary
  interface operator( .IN. )
     module procedure in
  end interface operator( .IN. )
  public :: operator(.IN.)
  
  ! Retrieve the value from a dictionary (unary)
  interface operator( .VAL. )
     module procedure value
  end interface operator( .VAL. )
  public :: operator(.VAL.)

  ! Retrieve the hash value from a dictionary entry (unary)
  interface operator( .HASH. )
     module procedure hash
  end interface operator( .HASH. )
  public :: operator(.HASH.)

  ! Checks for two dicts have all the same keys
  interface operator( .EQ. )
     module procedure d_eq_d
  end interface operator( .EQ. )
  public :: operator(.EQ.) ! Overloaded

  ! Checks for two dicts do not share any common keys
  interface operator( .NE. )
     module procedure d_ne_d
  end interface operator( .NE. )
  public :: operator(.NE.) ! Overloaded

  ! Steps one time in the dictionary (unary)
  interface operator( .NEXT. )
     module procedure d_next
  end interface operator( .NEXT. )
  public :: operator(.NEXT.)

  ! Retrieve the first of a dictionary (unary)
  interface operator( .FIRST. )
     module procedure d_first
  end interface operator( .FIRST. )
  public :: operator(.FIRST.)

  ! Check whether the dictionary is empty (unary)
  interface operator( .EMPTY. )
     module procedure d_empty
  end interface operator( .EMPTY. )
  public :: operator(.EMPTY.)

  interface delete
     module procedure delete_
  end interface delete
  public :: delete

  interface remove
     module procedure remove_
  end interface remove
  public :: remove

  interface add
     module procedure sub_d_cat_d
  end interface add
  public :: add

  interface assign
     module procedure dict_key2val
  end interface assign
  public :: assign

  interface associate
     module procedure dict_key_p_val
  end interface associate
  public :: associate


  ! Create a dictionary type from 
#include "dict_interface.inc"

  ! Create a dict type: 'key' .KV. 'val'
  public :: operator(.KV.)
  ! Create a dict type: 'key' .KVP. 'pointer'
  public :: operator(.KVP.)

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

contains

  pure function hash_val(key) result(val)
    character(len=*), intent(in) :: key
    integer :: val
    integer :: i, fac
    val = 0
    fac = mod(iachar(key(1:1)),4)
    do i = 1 , min(DICT_KEY_LENGTH,len_trim(key))
       val = val + iachar(key(i:i)) + fac * iachar(key(i:i))
       fac = fac + 1
       if ( fac > 3 ) then
          fac = -2
       end if
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
  pure function key(d)
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
  pure function hash(d)
    type(dict), intent(in) :: d
    integer :: hash
    hash = d%first%hash
  end function hash

  subroutine dict_key2val(val,d,key,dealloc)
    type(var), intent(inout) :: val
    type(dict), intent(inout) :: d
    character(len=*), intent(in) :: key
    logical, intent(in), optional :: dealloc
    type(dict) :: ld
    integer :: hash
    
    hash = hash_val(key)
    ld = .first. d
    search: do while ( .not. (.empty. ld) )
       if (      hash  < .hash. ld ) then
          exit search
       else if ( hash  > .hash. ld ) then
          ! the key does not exist, so return immediately
          call delete(val)
          return
       else if ( hash == .hash. ld ) then
          if ( key .eq. .KEY. ld ) then
             call assign(val,ld%first%value,dealloc=dealloc)
             return
          end if
       end if
       ld = .next. ld
    end do search

  end subroutine dict_key2val

  function in(key,d)
    character(len=*), intent(in) :: key
    type(dict), intent(in) :: d
    type(dict) :: ld
    integer :: hash
    logical :: in
    
    hash = hash_val(key)
    ld = .first. d
    search: do while ( .not. (.empty. ld) )
       if (      hash  < .hash. ld ) then
          exit search
       else if ( hash  > .hash. ld ) then
          ! The key does not exist... so quit fast
          in = .false.
          return
       else if ( hash == .hash. ld ) then
          if ( key .eq. .KEY. ld ) then
             in = .true.
             return
          end if
       end if
       ld = .next. ld
    end do search
    in = .false.

  end function in

  subroutine dict_key_p_val(val,d,key,dealloc)
    type(var), intent(inout) :: val
    type(dict), intent(inout) :: d
    character(len=*), intent(in), optional :: key
    logical, intent(in), optional :: dealloc
    type(dict) :: ld
    integer :: hash

    if ( .not. present(key) ) then
       if ( .not. (.empty. d) ) then
          call associate(val,d%first%value,dealloc=dealloc)
       end if
       return
    end if

    hash = hash_val(key)
    ld = .first. d
    search: do while ( .not. (.empty. ld) )
       if (      hash  < .hash. ld ) then
          exit search
       else if ( hash  > .hash. ld ) then
          ! the value does not exist, return quickly
          return
       else if ( hash == .hash. ld ) then
          if ( key .eq. .KEY. ld ) then
             call associate(val,ld%first%value,dealloc=dealloc)
             return
          end if
       end if
       ld = .next. ld
    end do search

  end subroutine dict_key_p_val

  ! Compares two dict types against each other
  ! Will do comparison, first by hash, and if that matches then
  ! for the key and value of the dictionaries
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
  ! not necessarily the negative of .eq.
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
    if ( .empty. d1 ) then
       if ( .empty. d2 ) return
       call copy_assign(d2,d)
       return
    end if
    call copy_assign(d1,d)
    if ( .empty. d2 ) return
    ladd => d2%first
    do 
       ! step ...
       lnext => ladd%next
       call d_insert(d,ladd)
       if ( .not. associated(lnext) ) return
       ladd => lnext
    end do
  end function d_cat_d

  ! Concatenate two dictionaries to one dictionary...
  ! it does not work with elemental as the 
  subroutine sub_d_cat_d(d,d2)
    type(dict), intent(inout) :: d
    type(dict), intent(in) :: d2
    type(d_entry), pointer :: ladd,lnext
    if ( .empty. d2 ) return
    ladd => d2%first
    do 
       ! step ...
       lnext => ladd%next
       call d_insert(d,ladd)
       if ( .not. associated(lnext) ) return
       ladd => lnext
    end do
  end subroutine sub_d_cat_d

  subroutine d_insert(d,entry)
    type(dict),    intent(inout) :: d
    type(d_entry), intent(inout), pointer :: entry
    type(d_entry), pointer :: search, prev

    ! if the dictionary is empty
    ! simply put it first
    if ( .not. associated(d%first) ) then
       d%first => entry
       d%len = 1
       return
    end if

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
          call assign(search%value,entry%value)
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
             call assign(search%value,entry%value)
             return
          end if
       end if
    end do search_loop
    prev%next => entry
    ! Increment length of the dictionary...
    d%len = d%len + 1

    ! As we could insert from a dictionary we have to reset, to not do endless loops...
    nullify(entry%next)

  end subroutine d_insert

  ! Retrieve the length of the dictionary...
  pure function len_(d)
    type(dict), intent(in) :: d
    integer :: len_
    len_ = d%len
  end function len_

  function d_next(d)
    type(dict), intent(in) :: d
    type(dict) :: d_next
    d_next%first => d%first%next
    d_next%len = d%len - 1
  end function d_next

  pure function d_empty(d)
    type(dict), intent(in) :: d
    logical :: d_empty
    d_empty = .not. associated(d%first)
  end function d_empty

  function d_first(d)
    type(dict), intent(in) :: d
    type(dict) :: d_first
    call copy_assign(d,d_first)
  end function d_first
  
  subroutine copy_assign(din,dcopy)
    type(dict), intent(in)  :: din
    type(dict), intent(out) :: dcopy
    dcopy%first => din%first
    dcopy%len = din%len
  end subroutine copy_assign

  subroutine dict_print(d)
    type(dict), intent(in)  :: d
    type(dict)  :: ld
    ld = .first. d
    do while ( .not. .empty. ld ) 
       write(*,'(t2,a,tr1,a,i0,a)') trim(.key. ld),' (',.hash. ld,')'
       ld = .next. ld
    end do
  end subroutine dict_print


  subroutine delete_(this,key)
    type(dict), intent(inout) :: this
    character(len=*), intent(in), optional :: key
    type(d_entry), pointer :: de, pr

    ! if no keys are present, simply return
    if ( .not. associated(this%first) ) then
       this%len = 0
       return
    end if

#ifdef DICT_DEBUG
    if ( len(this) == 0 ) then
       stop 'Something went wrong'
    end if
#endif

    if ( present(key) ) then
       
       ! we only need to delete the one key

       pr => this%first
       if ( pr%key == key ) then
          this%first => pr%next
          this%len = this%len - 1 
          call delete(pr%value)
          nullify(pr%next)
          deallocate(pr)
          nullify(pr)

          return

       end if

       ! more complicated case
       de => pr%next
       do while ( associated(de) )
          if ( de%key == key ) then
             pr%next => de%next
             call delete(de%value)
             nullify(de%next)
             deallocate(de)
             this%len = this%len - 1 
             exit
          end if
          pr => de
          de => de%next
       end do

       return

    end if
       

    ! delete the entire entry-tree
    call del_d_entry_tree(this%first)
    call delete(this%first%value)
    deallocate(this%first)
    nullify(this%first)    
    this%len = 0

  contains

    recursive subroutine del_d_entry_tree(d)
      type(d_entry), pointer :: d
      if ( associated(d) ) then
         if ( associated(d%next) ) then
            call del_d_entry_tree(d%next)
            call delete(d%next%value)
            deallocate(d%next)
            nullify(d%next)
         end if
      end if
    end subroutine del_d_entry_tree

  end subroutine delete_

  elemental subroutine remove_(this,key)
    type(dict), intent(inout) :: this
    character(len=*), intent(in) :: key
    type(d_entry), pointer :: de, pr

    ! if no keys are present, simply return
    if ( .not. associated(this%first) ) then
       this%len = 0
       return
    end if

    pr => this%first
    if ( pr%key == key ) then
       this%first => pr%next
       deallocate(pr)
       this%len = this%len - 1
       return
    end if

    de => pr%next
    do while ( associated(de) )
       if ( de%key == key ) then
          pr%next => de%next
          deallocate(de)
          this%len = this%len - 1
          exit
       end if
       pr => de
       de => de%next
    end do
    
  end subroutine remove_

  function dict_kv_char0(key,val) result(this)
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: val
    type(dict) :: this
    type(var_str) :: str
    this = new_d_key(key)
    str = val
    call assign(this%first%value,str)
  end function dict_kv_char0

  function dict_kv_var(key,val) result(this)
    character(len=*), intent(in) :: key
    type(var), intent(in) :: val
    type(dict) :: this
    this = new_d_key(key)
    call assign(this%first%value,val)
  end function dict_kv_var
  function dict_kvp_var(key,val) result(this)
    character(len=*), intent(in) :: key
    type(var), intent(in) :: val
    type(dict) :: this
    this = new_d_key(key)
    call associate(this%first%value,val)
  end function dict_kvp_var

#include "dict_funcs.inc"

end module dictionary
