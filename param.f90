MODULE param

USE kindset

IMPLICIT NONE

    REAL(rk), PARAMETER :: alpha=2.d0       !elasticity of substitution
    REAL(rk), PARAMETER :: beta=0.989d0
    REAL(rk), PARAMETER :: r=0.01d0

    REAL(rk), PARAMETER :: omega=2.5d-3    ! transfer
    REAL(rk), PARAMETER :: pi = 0.85d0      ! Persistence

    REAL(rk), PARAMETER :: vmin = -1.d10

    INTEGER(ik), PARAMETER :: na = 100
    REAL(rk), PARAMETER :: mn_a = 1.d-5
    REAL(rk), PARAMETER :: mx_a = 10.d0

    INTEGER(ik), PARAMETER :: ne = 15
    REAL(rk), PARAMETER :: mn_e = 1.d-5
    REAL(rk), PARAMETER :: mx_e = 0.25d0
    INTEGER(ik), PARAMETER :: ne0 = ne/2 + 1              ! mean income level
    REAL(rk), PARAMETER :: sigma_r = 0.d0 !2.d-2


    INTEGER(ik), PARAMETER :: mx_it = 10000
    REAL(rk), PARAMETER :: tol = 1.d-6

!    REAL(rk), PARAMETER :: phi = 1.d0

! Spline parametersá
  INTEGER(ik), PARAMETER :: ns=15                   ! total number of knots, including endpoints, on S
  CHARACTER(30), PARAMETER :: indicator = "not-a-knot"  ! complete or natural spline endpoint condition
  INTEGER(ik), PARAMETER :: m=5                    ! Number of functions to evaluate.

  INTEGER(ik), PARAMETER :: nf=na                    ! total number of function evaluations
  INTEGER(ik), PARAMETER :: order=0                 ! 0: value, 1: first derivative, 2: 2nd derivative

! Multidimensional spline
  INTEGER(ik), PARAMETER :: md = 2                 ! Number of dimesions
!  INTEGER(ik), PARAMETER :: nw = 3                 ! Number of knots in 2nd dimension to evaluate
  INTEGER(ik), DIMENSION(md), PARAMETER :: ns_m = (/ ns, m /)
  INTEGER(ik), DIMENSION(md), PARAMETER :: order_m= (/ 0, 0 /)              ! 0: value, 1: first derivative, 2: 2nd derivative


END MODULE param
