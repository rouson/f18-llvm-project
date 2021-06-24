! RUN: bbc %s -o - | FileCheck %s

!!! Test scalar (with implicit none)

! CHECK: func @_QPtest1(
subroutine test1
  implicit none
  integer i
  ! CHECK-DAG: %[[i:.*]] = fir.alloca i32 {{.*}}uniq_name = "_QFtest1Ei"
  ! CHECK-DAG: %[[tup:.*]] = fir.alloca tuple<!fir.ptr<i32>>
  ! CHECK: %[[addr:.*]] = fir.coordinate_of %[[tup]], %c0
  ! CHECK: %[[ii:.*]] = fir.convert %[[i]]
  ! CHECK: fir.store %[[ii]] to %[[addr]] : !fir.ref<!fir.ptr<i32>>
  ! CHECK: fir.call @_QFtest1Ptest1_internal(%[[tup]]) : (!fir.ref<tuple<!fir.ptr<i32>>>) -> ()
  call test1_internal
  print *, i
contains
  ! CHECK: func @_QFtest1Ptest1_internal(%[[arg:[^:]*]]: !fir.ref<tuple<!fir.ptr<i32>>> {fir.host_assoc}) {
  ! CHECK: %[[iaddr:.*]] = fir.coordinate_of %[[arg]], %c0
  ! CHECK: %[[i:.*]] = fir.load %[[iaddr]] : !fir.ref<!fir.ptr<i32>>
  ! CHECK: %[[val:.*]] = fir.call @_QPifoo() : () -> i32
  ! CHECK: fir.store %[[val]] to %[[i]] : !fir.ptr<i32>
  subroutine test1_internal
    integer, external :: ifoo
    i = ifoo()
  end subroutine test1_internal
end subroutine test1

!!! Test scalar

! CHECK: func @_QPtest2() {
subroutine test2
  a = 1.0
  b = 2.0
  ! CHECK: %[[tup:.*]] = fir.alloca tuple<!fir.ptr<f32>, !fir.ptr<f32>>
  ! CHECK-DAG: %[[a0:.*]] = fir.coordinate_of %[[tup]], %c0
  ! CHECK-DAG: %[[p0:.*]] = fir.convert %{{.*}} : (!fir.ref<f32>) -> !fir.ptr<f32>
  ! CHECK: fir.store %[[p0]] to %[[a0]] : !fir.ref<!fir.ptr<f32>>
  ! CHECK-DAG: %[[b0:.*]] = fir.coordinate_of %[[tup]], %c1
  ! CHECK-DAG: %[[p1:.*]] = fir.convert %{{.*}} : (!fir.ref<f32>) -> !fir.ptr<f32>
  ! CHECK: fir.store %[[p1]] to %[[b0]] : !fir.ref<!fir.ptr<f32>>
  ! CHECK: fir.call @_QFtest2Ptest2_internal(%[[tup]]) : (!fir.ref<tuple<!fir.ptr<f32>, !fir.ptr<f32>>>) -> ()
  call test2_internal
  print *, a, b
contains
  ! CHECK: func @_QFtest2Ptest2_internal(%[[arg:[^:]*]]: !fir.ref<tuple<!fir.ptr<f32>, !fir.ptr<f32>>> {fir.host_assoc}) {
  subroutine test2_internal
    ! CHECK: %[[a:.*]] = fir.coordinate_of %[[arg]], %c0
    ! CHECK: %[[aa:.*]] = fir.load %[[a]] : !fir.ref<!fir.ptr<f32>>
    ! CHECK: %[[b:.*]] = fir.coordinate_of %[[arg]], %c1
    ! CHECK: %{{.*}} = fir.load %[[b]] : !fir.ref<!fir.ptr<f32>>
    ! CHECK: fir.alloca
    ! CHECK: fir.load %[[aa]] : !fir.ptr<f32>
    c = a
    a = b
    b = c
    call test2_inner
  end subroutine test2_internal

  ! CHECK: func @_QFtest2Ptest2_inner(%[[arg:[^:]*]]: !fir.ref<tuple<!fir.ptr<f32>, !fir.ptr<f32>>> {fir.host_assoc}) {
  subroutine test2_inner
    ! CHECK: %[[a:.*]] = fir.coordinate_of %[[arg]], %c0
    ! CHECK: %[[aa:.*]] = fir.load %[[a]] : !fir.ref<!fir.ptr<f32>>
    ! CHECK: %[[b:.*]] = fir.coordinate_of %[[arg]], %c1
    ! CHECK: %[[bb:.*]] = fir.load %[[b]] : !fir.ref<!fir.ptr<f32>>
    ! CHECK-DAG: %[[bd:.*]] = fir.load %[[bb]] : !fir.ptr<f32>
    ! CHECK-DAG: %[[ad:.*]] = fir.load %[[aa]] : !fir.ptr<f32>
    ! CHECK: %{{.*}} = cmpf ogt, %[[ad]], %[[bd]] : f32
    if (a > b) then
       b = b + 2.0
    end if
  end subroutine test2_inner
end subroutine test2

!!! Test array

! CHECK: func @_QPtest3(
subroutine test3(p,q)
  real :: p(:)
  real :: q(:)

  q = -42.0
  ! CHECK: fir.call @_QFtest3Ptest3_inner() : () -> ()
  call test3_inner

  if (p(1) .ne. -42.0) then
     print *, "failed"
  end if
  
contains
  ! CHECK: func @_QFtest3Ptest3_inner(
  subroutine test3_inner
    ! FIXME: fails at a TODO
    !p = q
  end subroutine test3_inner
end subroutine test3

!!! Test scalar allocatable

! CHECK: func @_QPtest4() {
subroutine test4
  real, pointer :: p
  real, allocatable, target :: ally

  allocate(ally)
  ally = -42.0
  ! CHECK: fir.call @_QFtest4Ptest4_inner() : () -> ()
  call test4_inner

  if (p .ne. -42.0) then
     print *, "failed"
  end if
  
contains
  ! CHECK: func @_QFtest4Ptest4_inner(
  subroutine test4_inner
    ! FIXME: fails at a TODO
    !p => ally
  end subroutine test4_inner
end subroutine test4

!!! Test allocatable array

! CHECK: func @_QPtest5() {
subroutine test5
  real, pointer :: p(:)
  real, allocatable, target :: ally(:)

  allocate(ally(10))
  ally = -42.0
  ! CHECK: fir.call @_QFtest5Ptest5_inner() : () -> ()
  call test5_inner

  if (p(1) .ne. -42.0) then
     print *, "failed"
  end if
  
contains
  ! CHECK: func @_QFtest5Ptest5_inner(
  subroutine test5_inner
    ! FIXME: fails at a TODO
    !p => ally
  end subroutine test5_inner
end subroutine test5

!!! Test CHARACTER type

! CHECK: func @_QPtest6(
subroutine test6(p)
  character(*) :: p
  character(40) :: ch

  ch = "Hi there"
  ! CHECK: fir.call @_QFtest6Ptest6_inner() : () -> ()
  call test6_inner
  print *, p
  
contains
  ! CHECK: func @_QFtest6Ptest6_inner(
  subroutine test6_inner
    ! FIXME: fails at a TODO
    !p = ch
  end subroutine test6_inner
end subroutine test6
