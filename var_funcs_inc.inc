subroutine ROUTINE(assign_set,VAR)(this,rhs,dealloc)
  type(var), intent(inout) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical, intent(in), optional :: dealloc
  logical :: ldealloc
  ! collect deallocation option (default as =)
  ldealloc = .true.
  if(present(dealloc))ldealloc = dealloc
  if (.not. ldealloc) then
     ! if we don't deallocate, nullify
     call nullify(this)
     this%t = STR(VAR)
ALLOC(VAR,rhs)
     THIS(VAR) = rhs
     return
  end if
  ldealloc = this%t /= STR(VAR)
#if DIM > 0 
  if (.not.ldealloc) then
CHECK_SIZE(ldealloc,VAR,rhs,DIM)
  end if
#endif
  if (ldealloc) then
     call delete(this)
     this%t = STR(VAR)
ALLOC(VAR,rhs)
  end if
  THIS(VAR) = rhs
end subroutine ROUTINE(assign_set,VAR)

subroutine ROUTINE(assign_get,VAR)(lhs,this,success)
  VAR_TYPE, intent(out)DIMS :: lhs
  type(var), intent(in) :: this
  logical, intent(out), optional :: success
  logical :: lsuccess
  lsuccess = this%t==STR(VAR)
#if DIM > 0
  if (lsuccess) then
CHECK_SIZE(lsuccess,VAR,lhs,DIM)
  end if
#endif
  if (present(success)) success = lsuccess
  if (.not. lsuccess) return
  lhs = THIS(VAR)
end subroutine ROUTINE(assign_get,VAR)

subroutine ROUTINE(associate_get,VAR)(lhs,this,dealloc,success)
  VAR_TYPE, pointer DIMS :: lhs
  type(var), intent(in) :: this
  logical, intent(in), optional :: dealloc
  logical, intent(out), optional :: success
  logical :: ldealloc, lsuccess
  lsuccess = this%t==STR(VAR)
  if (present(success)) success = lsuccess
  ldealloc = .false.
  if(present(dealloc))ldealloc = dealloc
  ! there is one problem, say if lhs is not nullified...
  if (ldealloc.and.associated(lhs)) then
     deallocate(lhs)
     nullify(lhs)
  end if
  if (.not. lsuccess ) return
  lhs => THIS(VAR)
end subroutine ROUTINE(associate_get,VAR)
subroutine ROUTINE(associate_set,VAR)(this,rhs,dealloc)
  type(var), intent(inout) :: this
  VAR_TYPE, intent(in)DIMS, target :: rhs
  logical, intent(in), optional :: dealloc
  logical :: ldealloc
  ldealloc = .false.
  if(present(dealloc))ldealloc = dealloc
  if (ldealloc) then
     call delete(this)
  else
     call nullify(this)
  end if
  this%t = STR(VAR)
  THIS(VAR) => rhs
end subroutine ROUTINE(associate_set,VAR)

pure function ROUTINE(associatd_l,VAR)(lhs,this) result(ret)
  VAR_TYPE, pointer DIMS :: lhs
  type(var), intent(in) :: this
  logical :: ret
  ret = this%t==STR(VAR)
  if (ret) then	
     ret = associated(lhs,THIS(VAR))
  endif
end function ROUTINE(associatd_l,VAR)
pure function ROUTINE(associatd_r,VAR)(this,rhs) result(ret)
  type(var), intent(in) :: this
  VAR_TYPE, pointer DIMS :: rhs
  logical :: ret
  ret = this%t==STR(VAR)
  if (ret) then	
     ret = associated(THIS(VAR),rhs)
  endif
end function ROUTINE(associatd_r,VAR)

! All boolean functions
#ifdef BOOLEANS
function ROUTINE(eq_l,VAR)(this,rhs) result(ret)
  type(var), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = this%t==STR(VAR)
  if (.not. ret) return
  ret = all(THIS(VAR) == rhs)
end function ROUTINE(eq_l,VAR)

function ROUTINE(eq_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(var), intent(in) :: this
  logical :: ret
  ret = this == lhs
end function ROUTINE(eq_r,VAR)

function ROUTINE(ne_l,VAR)(this,rhs) result(ret)
  type(var), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = .not. this == rhs
end function ROUTINE(ne_l,VAR)

function ROUTINE(ne_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(var), intent(in) :: this
  logical :: ret
  ret = .not. this == lhs
end function ROUTINE(ne_r,VAR)

function ROUTINE(gt_l,VAR)(this,rhs) result(ret)
  type(var), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = this%t==STR(VAR)
  if (.not. ret) return
  ret = all(THIS(VAR) > rhs)
end function ROUTINE(gt_l,VAR)
function ROUTINE(gt_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(var), intent(in) :: this
  logical :: ret
  ret = this%t==STR(VAR)
  if (.not. ret) return
  ret = all(lhs > THIS(VAR))
end function ROUTINE(gt_r,VAR)

function ROUTINE(lt_l,VAR)(this,rhs) result(ret)
  type(var), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = rhs > this
end function ROUTINE(lt_l,VAR)
function ROUTINE(lt_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(var), intent(in) :: this
  logical :: ret
  ret = this > lhs
end function ROUTINE(lt_r,VAR)

function ROUTINE(ge_l,VAR)(this,rhs) result(ret)
  type(var), intent(in) :: this 
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = .not. this < rhs
end function ROUTINE(ge_l,VAR)
function ROUTINE(ge_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(var), intent(in) :: this
  logical :: ret
  ret = .not. lhs < this
end function ROUTINE(ge_r,VAR)

function ROUTINE(le_l,VAR)(this,rhs) result(ret)
  type(var), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = .not. this > rhs
end function ROUTINE(le_l,VAR)
function ROUTINE(le_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(var), intent(in) :: this
  logical :: ret
  ret = .not. lhs > this
end function ROUTINE(le_r,VAR)
#endif
