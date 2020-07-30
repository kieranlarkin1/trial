MODULE mod_space

USE KindSet

IMPLICIT NONE

CONTAINS

SUBROUTINE linspace(lb, ub, gridnum, X)

IMPLICIT NONE

INTEGER(ik), INTENT(IN):: gridnum
REAL(rk), INTENT(IN) :: lb, ub
REAL(rk), INTENT(OUT) ::  X(gridnum)

INTEGER(ik) :: j1
REAL(rk)::  Y(gridnum-2_ik)

DO j1 = 1_ik, gridnum - 2_ik
	Y(j1) = dble(j1)
END DO

Y = ((ub - lb)/dble(gridnum-1_ik))*Y + lb

X(1_ik) = lb
X(2_ik:gridnum-1_ik) = Y
X(gridnum) = ub

END SUBROUTINE linspace
!-------------------------------------------------------
SUBROUTINE logspace(lowerbound, upperbound, n, X)

IMPLICIT NONE

INTEGER, INTENT(IN) :: n
REAL(rk), INTENT(IN) :: lowerbound, upperbound
REAL(rk), INTENT(OUT) :: X(n)

INTEGER :: j1
REAL(rk):: term0, lb, ub, Y(n-1)

term0 = DLOG(10.0_rk)
lb = DLOG(lowerbound)/term0
ub = DLOG(upperbound)/term0

! This is lifted directly from the linspace subroutine.
DO j1 = 1, n - 2
	Y(j1) = DBLE(j1)
END DO

Y = ((ub - lb)/dble(n-1))*Y + lb

X(1) = lb
X(2:n-1) = Y
X(n) = ub

X = 10.0_rk**X

END SUBROUTINE logspace

END MODULE mod_space
