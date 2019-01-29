#ifdef COMMENTS
! For LICENSE, see README.md
#endif
#include "settings.inc"
subroutine ROUTINE(assign_set,VAR)(this,rhs,dealloc)
  type(variable_t), intent(inout) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical, intent(in), optional :: dealloc
  logical :: ldealloc
  type :: pt
    VAR_TYPE, pointer DIMS :: p => null()
  end type
  type(pt) :: p
  ! ASSIGNMENT in fortran is per default destructive
  ldealloc = .true.
  if(present(dealloc))ldealloc = dealloc
  if (ldealloc) then
     call delete(this)
  else
     call nullify(this)
  end if
  ! With pointer transfer we need to deallocate
  ! else bounds might change...
  this%t = STR(VAR)
  ALLOC(p%p,rhs) ! allocate space
  p%p = rhs ! copy data over
  allocate(this%enc(size(transfer(p, local_enc_type)))) ! allocate encoding
  this%enc(:) = transfer(p, local_enc_type) ! transfer pointer type to the encoding
  ! We already have shipped it
  nullify(p%p)
end subroutine ROUTINE(assign_set,VAR)

subroutine ROUTINE(assign_get,VAR)(lhs,this,success)
  VAR_TYPE, intent(out)DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical, intent(out), optional :: success
  logical :: lsuccess
  type :: pt
    VAR_TYPE, pointer DIMS :: p => null()
  end type
  type(pt) :: p
  lsuccess = this%t == STR(VAR)
#if DIM > 0
  if (lsuccess) then
    p = transfer(this%enc,p) ! retrieve pointer encoding
    lsuccess = all(shape(p%p)==shape(lhs))   !&
     !     .and. all((lbound(p%p) == lbound(lhs))) &
     !     .and. all((ubound(p%p) == ubound(lhs))) 

  end if
#endif
  if (present(success)) success = lsuccess
  if (.not. lsuccess) return
#if DIM == 0
  p = transfer(this%enc,p) ! retrieve pointer encoding
#endif
  lhs = p%p
end subroutine ROUTINE(assign_get,VAR)

subroutine ROUTINE(associate_get,VAR)(lhs,this,dealloc,success)
  VAR_TYPE, pointer DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical, intent(in), optional :: dealloc
  logical, intent(out), optional :: success
  logical :: ldealloc, lsuccess
  type :: pt
    VAR_TYPE, pointer DIMS :: p => null()
  end type
  type(pt) :: p
  lsuccess = this%t == STR(VAR)
  if (present(success)) success = lsuccess
  ! ASSOCIATION in fortran is per default non-destructive
  ldealloc = .false.
  if(present(dealloc))ldealloc = dealloc
  ! there is one problem, say if lhs is not nullified...
  if (ldealloc.and.associated(lhs)) then
     deallocate(lhs)
     nullify(lhs)
  end if
  if (.not. lsuccess ) return
  p = transfer(this%enc,p) ! retrieve pointer encoding
  lhs => p%p
end subroutine ROUTINE(associate_get,VAR)
subroutine ROUTINE(associate_set,VAR)(this,rhs,dealloc)
  type(variable_t), intent(inout) :: this
#ifdef COMMENTS
  ! Setting the intent(inout) ensures that no constants
  ! will be able to be passed.
  ! However, the dictionary type does not allow
  ! this due to OPERATORS, hence we keep it as this
  ! and proclaim that any user creating a pointer
  ! to a constant is insane...
#endif
  VAR_TYPE, intent(in)DIMS, target :: rhs
  logical, intent(in), optional :: dealloc
  logical :: ldealloc
  type :: pt
    VAR_TYPE, pointer DIMS :: p => null()
  end type
  type(pt) :: p
  ! ASSOCIATION in fortran is per default non-destructive
  ldealloc = .false.
  if(present(dealloc))ldealloc = dealloc
  if (ldealloc) then
     call delete(this)
  else
     call nullify(this)
  end if
  this%t = STR(VAR)
  p%p => rhs
  allocate(this%enc(size(transfer(p, local_enc_type)))) ! allocate encoding
  this%enc(:) = transfer(p, local_enc_type) ! transfer pointer type to the encoding
end subroutine ROUTINE(associate_set,VAR)

pure function ROUTINE(associatd_l,VAR)(lhs,this) result(ret)
  VAR_TYPE, pointer DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical :: ret
  type :: pt
    VAR_TYPE, pointer DIMS :: p
  end type
  type(pt) :: p
  ret = this%t == STR(VAR)
  if (ret) then
     nullify(p%p)
     p = transfer(this%enc,p)
     ret = associated(lhs,p%p)
  endif
end function ROUTINE(associatd_l,VAR)
pure function ROUTINE(associatd_r,VAR)(this,rhs) result(ret)
  type(variable_t), intent(in) :: this
  VAR_TYPE, pointer DIMS :: rhs
  logical :: ret
  type :: pt
    VAR_TYPE, pointer DIMS :: p
  end type
  type(pt) :: p
  ret = this%t == STR(VAR)
  if (ret) then	
     nullify(p%p)
     p = transfer(this%enc,p)
     ret = associated(p%p,rhs)
  endif
end function ROUTINE(associatd_r,VAR)

! All boolean functions
#ifdef BOOLEANS
function ROUTINE(eq_l,VAR)(this,rhs) result(ret)
  type(variable_t), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = this%t == STR(VAR)
  if (.not. ret) return
  ret = all(THIS(VAR) == rhs)
end function ROUTINE(eq_l,VAR)

function ROUTINE(eq_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical :: ret
  ret = this == lhs
end function ROUTINE(eq_r,VAR)

function ROUTINE(ne_l,VAR)(this,rhs) result(ret)
  type(variable_t), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = .not. this == rhs
end function ROUTINE(ne_l,VAR)

function ROUTINE(ne_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical :: ret
  ret = .not. this == lhs
end function ROUTINE(ne_r,VAR)

function ROUTINE(gt_l,VAR)(this,rhs) result(ret)
  type(variable_t), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = this%t == STR(VAR)
  if (.not. ret) return
  ret = all(THIS(VAR) > rhs)
end function ROUTINE(gt_l,VAR)
function ROUTINE(gt_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical :: ret
  ret = this%t == STR(VAR)
  if (.not. ret) return
  ret = all(lhs > THIS(VAR))
end function ROUTINE(gt_r,VAR)

function ROUTINE(lt_l,VAR)(this,rhs) result(ret)
  type(variable_t), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = rhs > this
end function ROUTINE(lt_l,VAR)
function ROUTINE(lt_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical :: ret
  ret = this > lhs
end function ROUTINE(lt_r,VAR)

function ROUTINE(ge_l,VAR)(this,rhs) result(ret)
  type(variable_t), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = .not. this < rhs
end function ROUTINE(ge_l,VAR)
function ROUTINE(ge_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical :: ret
  ret = .not. lhs < this
end function ROUTINE(ge_r,VAR)

function ROUTINE(le_l,VAR)(this,rhs) result(ret)
  type(variable_t), intent(in) :: this
  VAR_TYPE, intent(in)DIMS :: rhs
  logical :: ret
  ret = .not. this > rhs
end function ROUTINE(le_l,VAR)
function ROUTINE(le_r,VAR)(lhs,this) result(ret)
  VAR_TYPE, intent(in)DIMS :: lhs
  type(variable_t), intent(in) :: this
  logical :: ret
  ret = .not. lhs > this
end function ROUTINE(le_r,VAR)
#endif
