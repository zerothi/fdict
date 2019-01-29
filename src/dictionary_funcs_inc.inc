#ifdef COMMENTS
! For LICENSE, see README.md
#endif
function ROUTINE(d_kv,VAR)(key,val) result(this)
character(len=*), intent(in) :: key
VAR_TYPE, intent(in)DIMS :: val
type(dictionary_t) :: this
this = new_d_key(key)
call assign(this%first%value,val)
end function ROUTINE(d_kv,VAR)

function ROUTINE(d_kvp,VAR)(key, val) result(this)
character(len=*), intent(in) :: key
#ifdef COMMENTS
  ! Setting the intent(inout) ensures that no constants
  ! will be able to be passed.
  ! However, the dictionary type does not allow
  ! this due to OPERATORS, hence we keep it as this
  ! and proclaim that any user creating a pointer
  ! to a constant is insane...
#endif
VAR_TYPE, intent(in)DIMS, target :: val
type(dictionary_t) :: this
this = new_d_key(key)
call associate(this%first%value,val)
end function ROUTINE(d_kvp,VAR)

subroutine ROUTINE(d_get_val,VAR)(val,this,key,success)
VAR_TYPE, intent(out)DIMS :: val
type(dictionary_t), intent(inout) :: this
character(len=*), intent(in) :: key
logical, intent(out), optional :: success
type(variable_t) :: v
call associate(v,this,key=key)
call assign(val,v,success=success)
call nullify(v)
end subroutine ROUTINE(d_get_val,VAR)

subroutine ROUTINE(d_get_val_first,VAR)(val,this,success)
VAR_TYPE, intent(out)DIMS :: val
type(dictionary_t), intent(inout) :: this
logical, intent(out), optional :: success
call assign(val,this%first%value,success=success)
end subroutine ROUTINE(d_get_val_first,VAR)

subroutine ROUTINE(d_get_p,VAR)(val,this,key,success)
VAR_TYPE, pointer DIMS :: val
type(dictionary_t), intent(inout) :: this
character(len=*), intent(in) :: key
logical, intent(out), optional :: success
type(variable_t) :: v
call associate(v,this,key=key)
call associate(val,v,success=success)
call nullify(v)
end subroutine ROUTINE(d_get_p,VAR)

subroutine ROUTINE(d_get_p_first,VAR)(val,this,success)
VAR_TYPE, pointer DIMS :: val
type(dictionary_t), intent(inout) :: this
logical, intent(out), optional :: success
call associate(val,this%first%value,success=success)
end subroutine ROUTINE(d_get_p_first,VAR)
