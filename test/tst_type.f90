program tst_type

  use tst_utils

  implicit none

  type(variable_t) :: va
  type :: tType
    real(real64) :: b(2), c(2,2)
    integer :: i
    real(real64), pointer :: pa(:) =>null()
  end type tType

  ! To not copy all data, we need to retain the
  ! type in a pointing type
  type :: tpType
    type(tType), pointer :: t => null()
  end type tpType
  type(tType), target :: a
  type(tType), pointer :: b
  type(tpType) :: container
  character(len=1) :: local_enc_type(1)
  character(len=1), allocatable :: tmp_enc(:)
  integer :: lenc

  ! Create a
  a%b = 1._real64
  a%b(2) = 0.2_real64
  a%c = 0._real64
  a%i = -1
  allocate(a%pa(3))
  a%pa = 0.2_real64

  ! Try and save the type in the variable
  container%t => a
  call associate_type(va,transfer(container,local_enc_type))

  ! We should now have variable a contained in the variable
  call print(va)
  call assert(va%t == 'ut','User-type not asserted.')

  ! Try and retrieve data to b
  container%t => b
  lenc = size_enc(va)
  allocate(tmp_enc(lenc))
  call enc(va,tmp_enc)
  container = transfer(tmp_enc(1:lenc),container)
  deallocate(tmp_enc)
  b => container%t

  call assert(abs(b%b - a%b) < 0.0001_real64, &
      'Copying type did not work, b')
  call assert(abs(b%c - a%c) < 0.0001_real64, &
      'Copying type did not work, c')
  call assert(b%i==a%i, &
      'Copying type did not work, i')
  call assert(associated(b%pa,a%pa), &
      'Copying type did not work, target')

  call delete(va)
  deallocate(a%pa)

end program tst_type
