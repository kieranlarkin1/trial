MODULE mod_subrou

USE kindset
!use param

IMPLICIT NONE

REAL(rk), PARAMETER :: phi = 4.d0

CONTAINS

SUBROUTINE trial_sub(v,n,out)

  USE kindset
  USE param

IMPLICIT NONE

INTEGER(ik), INTENT(IN) :: n
REAL(rk), DIMENSION(n), INTENT(IN) :: v
REAL(rk), DIMENSION(n), INTENT(OUT) :: out
REAL(rk), PARAMETER :: theta = 2.d0
!REAL(rk) :: beta
!beta = 0.9

WRITE(*,*) ' SR: beta, phi, theta= ', beta, phi, theta
out = (phi/theta) * V

END SUBROUTINE trial_sub

END MODULE mod_subrou
