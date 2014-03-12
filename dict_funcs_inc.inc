function ROUTINE(dict_kv,VAR)(key,val) result(this)
character(len=*), intent(in) :: key
VAR_TYPE, intent(in)DIMS :: val
type(dict) :: this
this = new_d_key(key)
call assign(this%first%value,val)
end function ROUTINE(dict_kv,VAR)

function ROUTINE(dict_kvp,VAR)(key, val) result(this)
character(len=*), intent(in) :: key
VAR_TYPE, intent(in)DIMS, target :: val
type(dict) :: this
this = new_d_key(key)
call associate(this%first%value,val)
end function ROUTINE(dict_kvp,VAR)
