MODULE param

IMPLICIT NONE

    REAL(8), PARAMETER :: alpha=2.d0       !elasticity of substitution
    REAL(8), PARAMETER :: beta=0.989d0
    REAL(8), PARAMETER :: r=0.01d0
    REAL(8), PARAMETER :: omega=1.d0

    REAL(8), PARAMETER :: vmin = -1.d10

    INTEGER, PARAMETER :: na = 100
    REAL(8), PARAMETER :: mn_a = 0.d0
    REAL(8), PARAMETER :: mx_a = 10.d0

    INTEGER, PARAMETER :: mx_it = 10000
    REAL(8), PARAMETER :: tol = 1.d-6

!    REAL(8), PARAMETER :: phi = 1.d0

! Spline parameters
  INTEGER, PARAMETER :: ns=25                    ! total number of knots, including endpoints, on S
  CHARACTER(30), PARAMETER :: indicator = "not-a-knot"  ! complete or natural spline endpoint condition
  INTEGER, PARAMETER :: m=1                ! Number of functions to evaluate.

  INTEGER, PARAMETER :: nf=na                    ! total number of function evaluations
  INTEGER, PARAMETER :: order=0                 ! 0: value, 1: first derivative, 2: 2nd derivative
END MODULE param
