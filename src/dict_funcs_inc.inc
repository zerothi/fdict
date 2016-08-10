#ifdef COMMENTS
! For LICENSE, see README.md
#endif
function ROUTINE(dict_kv,VAR)(key,val) result(this)
character(len=*), intent(in) :: key
VAR_TYPE, intent(in)DIMS :: val
type(dict) :: this
this = new_d_key(key)
call assign(this%first%value,val)
end function ROUTINE(dict_kv,VAR)

function ROUTINE(dict_kvp,VAR)(key, val) result(this)
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
type(dict) :: this
this = new_d_key(key)
call associate(this%first%value,val)
end function ROUTINE(dict_kvp,VAR)

subroutine ROUTINE(dict_get_val,VAR)(val,this,key)
VAR_TYPE, intent(out)DIMS :: val
type(dict), intent(inout) :: this
character(len=*), intent(in) :: key
type(var) :: v
call associate(v,this,key=key)
call assign(val,v)
call nullify(v)
end subroutine ROUTINE(dict_get_val,VAR)

subroutine ROUTINE(dict_get_val_first,VAR)(val,this)
VAR_TYPE, intent(out)DIMS :: val
type(dict), intent(inout) :: this
call assign(val,this%first%value)
end subroutine ROUTINE(dict_get_val_first,VAR)

subroutine ROUTINE(dict_get_p,VAR)(val,this,key)
VAR_TYPE, pointer DIMS :: val
type(dict), intent(inout) :: this
character(len=*), intent(in) :: key
type(var) :: v
call associate(v,this,key=key)
call associate(val,v)
call nullify(v)
end subroutine ROUTINE(dict_get_p,VAR)

subroutine ROUTINE(dict_get_p_first,VAR)(val,this)
VAR_TYPE, pointer DIMS :: val
type(dict), intent(inout) :: this
call associate(val,this%first%value)
end subroutine ROUTINE(dict_get_p_first,VAR)
