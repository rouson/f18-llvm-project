! RUN: %S/test_errors.sh %s %t %flang_fc1
! REQUIRES: shell
! Check for semantic errors in co_sum() function calls

subroutine test

  integer i, status
  character(len=*) message

  ! correct calls, should produce no errors
  call co_sum(i)
  call co_sum(i,   1)
  call co_sum(i,   1,              status)
  call co_sum(i,   1,              message)
  call co_sum(i,   1,              stat=status)
  call co_sum(i,   1,              errmsg=message)
  call co_sum(i,   1,              stat=status, errmsg=message)
  call co_sum(i,   result_image=1, stat=status, errmsg=message)
  call co_sum(a=i, result_image=1, stat=status, errmsg=message)
  call co_sum(i,   result_image=1, stat=status, errmsg=message)

  ! the error is seen as too many arguments to the co_sum() call
  !ERROR: too many actual arguments for collective subroutines 'co_sum'
  call co_sum(i,   result_image=1, stat=status, errmsg=message, 3.4)

  ! keyword argument with incorrect type
  !ERROR: unknown keyword argument to intrinsic 'co_sum'
  call co_sum(fake=3.4)

end subroutine
