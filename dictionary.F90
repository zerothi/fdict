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

  ! Internal variables for determining the maximum size of the dictionaries.
  ! We could consider changing this to a variable size string
  ! However, that will increase the dependencies and will most likely not yield
  ! a better interface.
  integer, parameter, public :: DICT_KEY_LENGTH = 50
  
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

  interface print
     module procedure print_
  end interface print
  public :: print

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

  ! check whether key not exists in dictionary
  interface operator( .NIN. )
     module procedure nin
  end interface operator( .NIN. )
  public :: operator(.NIN.)
  
  ! Retrieve the value from a dictionary (unary)
  interface operator( .VAL. )
     module procedure value
  end interface operator( .VAL. )
  public :: operator(.VAL.)
  interface operator( .VALP. )
     module procedure value_p
  end interface operator( .VALP. )
  public :: operator(.VALP.)

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

  interface nullify
     module procedure nullify_
  end interface nullify
  public :: nullify

  interface extend
     module procedure sub_d_cat_d
  end interface extend
  public :: extend

  interface assign
     module procedure dict_key2val
     !module procedure dict_key2dict
  end interface assign
  public :: assign

  interface associate
     module procedure dict_key_p_val
     module procedure dict_key_p_dict
  end interface associate
  public :: associate

  interface which
     module procedure dict_key_which
  end interface which
  public :: which


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
  function value_p(d) 
    type(dict), intent(in) :: d
    type(var) :: value_p
    call associate(value_p,d%first%value)
  end function value_p

  ! Returns the hash value of the dictionary first item...
  pure function hash(d)
    type(dict), intent(in) :: d
    integer :: hash
    hash = d%first%hash
  end function hash

  subroutine dict_key2val(val,d,key,dealloc)
    type(var), intent(inout) :: val
    type(dict), intent(inout) :: d
    character(len=*), intent(in), optional :: key
    logical, intent(in), optional :: dealloc
    type(dict) :: ld
    integer :: hash, lhash

    if ( .not. present(key) ) then
       if ( .not. (.empty. d) ) then
          call assign(val,d%first%value,dealloc=dealloc)
       else
          call val_delete_request(val,dealloc=dealloc)
       end if
       return
    end if

    hash = hash_val(key)
    ld = .first. d
    search: do while ( .not. (.empty. ld) )
       lhash = .hash. ld
       if (      hash > lhash ) then
          ! skip to next search
       else if ( hash < lhash ) then
          ! the key does not exist, delete if requested, else clean it
          call val_delete_request(val,dealloc=dealloc)
          exit search
       else if ( hash == lhash ) then
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
    integer :: hash, lhash
    logical :: in
    
    hash = hash_val(key)
    ld = .first. d
    search: do while ( .not. (.empty. ld) )
       lhash = .hash. ld
       if (      hash > lhash ) then
          ! skip to next search
       else if ( hash < lhash ) then
          exit search
       else if ( hash == lhash ) then
          if ( key .eq. .KEY. ld ) then
             in = .true.
             return
          end if
       end if
       ld = .next. ld
    end do search
    in = .false.

  end function in

  function nin(key,d)
    character(len=*), intent(in) :: key
    type(dict), intent(in) :: d
    logical :: nin
    nin = .not. in(key,d)
  end function nin

  subroutine dict_key_p_val(val,d,key,dealloc)
    type(var), intent(inout) :: val
    type(dict), intent(inout) :: d
    character(len=*), intent(in), optional :: key
    logical, intent(in), optional :: dealloc
    type(dict) :: ld
    integer :: hash, lhash

    if ( .not. present(key) ) then
       if ( .not. (.empty. d) ) then
          call associate(val,d%first%value,dealloc=dealloc)
       else
          call val_delete_request(val,dealloc=dealloc)
       end if
       return
    end if

    hash = hash_val(key)
    ld = .first. d
    search: do while ( .not. (.empty. ld) )
       lhash = .hash. ld
       if (      hash > lhash ) then
          ! skip to next search
       else if ( hash < lhash ) then
          call val_delete_request(val,dealloc=dealloc)
          exit search
       else if ( hash == lhash ) then
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

  subroutine print_(d)
    type(dict), intent(in)  :: d
    type(dict)  :: ld
    ld = .first. d
    do while ( .not. .empty. ld ) 
       write(*,'(t2,a,tr1,a,i0,a)') trim(.key. ld), &
            '['//ld%first%value%t//'] (',.hash. ld,')'
       ld = .next. ld
    end do
  end subroutine print_


  subroutine delete_(this,key,dealloc)
    type(dict), intent(inout) :: this
    character(len=*), intent(in), optional :: key
    logical, intent(in), optional :: dealloc
    type(d_entry), pointer :: de, pr
    logical :: ldealloc
    integer :: kh, lhash

    ! We default to de-allocation of everything
    ldealloc = .true.
    if ( present(dealloc) ) ldealloc = dealloc

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

       kh = hash_val(key)

       pr => this%first
       if ( kh == hash_val(pr%key) ) then
          if ( key == pr%key ) then
             this%first => pr%next
             this%len = this%len - 1 
             call delete(pr%value,dealloc=ldealloc)
             nullify(pr%next)
             deallocate(pr)
             nullify(pr)
             
             return
          end if

       end if

       ! more complicated case
       de => pr%next
       do while ( associated(de) )
          ! We know it is sorted with hash-tags.
          ! So if we are beyond the hash, we just quit.
          lhash = hash_val(de%key)
          if ( kh < lhash ) exit ! it does not exist
          if ( lhash == kh ) then
             if ( de%key == key ) then
                pr%next => de%next
                call delete(de%value,dealloc=ldealloc)
                nullify(de%next)
                deallocate(de)
                this%len = this%len - 1 
                exit
             end if
          end if
          pr => de
          de => de%next
       end do

       return

    end if
       
    ! delete the entire entry-tree
    call del_d_entry_tree(this%first,dealloc=ldealloc)
    call delete(this%first%value,dealloc=ldealloc)
    deallocate(this%first)
    nullify(this%first)    
    this%len = 0

  contains

    recursive subroutine del_d_entry_tree(d,dealloc)
      type(d_entry), pointer :: d
      logical, intent(in) :: dealloc
      if ( associated(d) ) then
         if ( associated(d%next) ) then
            call del_d_entry_tree(d%next,dealloc)
            call delete(d%next%value,dealloc=dealloc)
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
       ! Ensures that the encoding gets removed
       call nullify(pr%value)
       deallocate(pr)
       this%len = this%len - 1
       return
    end if

    de => pr%next
    do while ( associated(de) )
       if ( de%key == key ) then
          pr%next => de%next
          ! Ensures that the encoding gets removed
          call nullify(de%value)
          deallocate(de)
          this%len = this%len - 1
          exit
       end if
       pr => de
       de => de%next
    end do
    
  end subroutine remove_

  elemental subroutine nullify_(this)
    type(dict), intent(inout) :: this

    ! This will simply nullify the dictionary, thereby
    ! remove all references to all objects.
    nullify(this%first)
    this%len = 0

  end subroutine nullify_

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

  function dict_key_which(this,key) result(t)
    type(dict), intent(in) :: this
    character(len=*), optional, intent(in) :: key
    character(len=2) :: t
    type(dict) :: ld
    integer :: hash, lhash
    
    if ( present(key) ) then
       hash = hash_val(key)
       ld = .first. this
       search: do while ( .not. (.empty. ld) )
          lhash = .hash. ld
          if (      hash > lhash ) then
            ! skip to next search
          else if ( hash < lhash ) then
             t = '  '
             exit search
          else if ( hash == lhash ) then
             if ( key .eq. .KEY. ld ) then
                t = which(ld%first%value)
                return
             end if
          end if
          ld = .next. ld
       end do search
    else
       t = which(this%first%value)
    end if
  end function dict_key_which

#include "dict_funcs.inc"

  ! helper routines for often used stuff
  subroutine val_delete_request(val,dealloc)
    type(var), intent(inout) :: val
    logical, intent(in), optional :: dealloc
    if ( present(dealloc) ) then
       if ( dealloc ) call delete(val)
    end if
    call nullify(val)
  end subroutine val_delete_request

  ! Create a routine for making the dictionary point to the data
  ! key.
  function dict_kvp_dict(key,dic) result(this)
    character(len=*), intent(in) :: key
    type(dict), intent(in), target :: dic
    type(dict) :: this

    type :: pd_entry
       type(d_entry), pointer :: d
    end type pd_entry
    type(pd_entry) :: pd
    type(var) :: v
    character(len=1) :: c(1)
    
    pd%d => dic%first
    call associate_type(v,transfer(pd,c))
    this = (key.kvp.v)

  end function dict_kvp_dict

  ! In case the value of the dictionary is a dictionary we can request that 
  ! dictionary directly
  subroutine dict_key2dict(dic,d,key,dealloc)
    type(dict), intent(inout) :: dic
    type(dict), intent(inout) :: d
    character(len=*), intent(in), optional :: key
    logical, intent(in), optional :: dealloc

    ! Retrieving a dictionary will NEVER
    ! be copying the entire dictionary.
    call dict_key_p_dict(dic,d,key=key,dealloc=dealloc)

  end subroutine dict_key2dict

  subroutine dict_key_p_dict(dic,d,key,dealloc)
    type(dict), intent(inout) :: dic
    type(dict), intent(inout) :: d
    character(len=*), intent(in), optional :: key
    logical, intent(in), optional :: dealloc

    ! Instead of saving the data-type dict
    ! we save the first pointer.
    ! This will allow greater flexibility as the
    ! parent container can then be re-used with out
    ! worries. 
    ! I.e.
    !  if one uses :
    !    type :: pdict
    !      type(dict), pointer :: d
    !    end type
    !  then the address of the "parenting" dictionary is saved,
    !  And hence, doing:
    !  dic1 = ('a'.kv.1)
    !  dic2 = ('dic1'.kvp.dic1)
    !  call nullify(dic1)
    !  dic1 = ('b'.kv.1)
    !  will make dic1 in dic2 contain ('b'.kv.1)
    ! Specifically because the address of the dic1 does not change.
    ! However, the d_entry pointer is irrespective of parent locality.
    type :: pd_entry
       type(d_entry), pointer :: d
    end type pd_entry
    type(pd_entry) :: pd
    type(dict) :: ld
    type(var) :: v
    character(len=1), allocatable :: c(:)
    integer :: i
    logical :: ldealloc

    ldealloc = .false.
    if ( present(dealloc) ) ldealloc = dealloc
    if ( ldealloc ) then
       call delete(dic)
    else
       call nullify(dic)
    end if

    ! Retrieve the dictionary key
    call associate(v,d,key=key)
    
    i = size_enc(v)
    allocate(c(i))
    call enc(v,c)
    pd = transfer(c,pd)
    deallocate(c)
    dic%first => pd%d
    call nullify(v)

    ! we need to re-count the number of entries in
    ! the d_entry tree.
    ! Sadly, this is because we contain the d_entry
    ! type, and NOT the dict type :(
    ! However, it makes the programming style more
    ! intuitive (dependent on how you look at it)
    ld = .first. dic
    dic%len = 0
    do while ( .not. (.empty. ld) )
       dic%len = dic%len + 1
       ld = .next. ld
    end do

  end subroutine dict_key_p_dict

end module dictionary
