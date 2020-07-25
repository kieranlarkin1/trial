MODULE mod_subrou

use param

IMPLICIT NONE

REAL(8), PARAMETER :: phi = 4.d0

CONTAINS

SUBROUTINE trial_sub(v,n,out)

!USE param

IMPLICIT NONE

INTEGER, INTENT(IN) :: n
REAL(8), DIMENSION(n), INTENT(IN) :: v
REAL(8), DIMENSION(n), INTENT(OUT) :: out
REAL(8), PARAMETER :: theta = 2.d0
!REAL(8) :: beta
!beta = 0.9

WRITE(*,*) ' SR: beta, phi, theta= ', beta, phi, theta
out = (phi/theta) * V

END SUBROUTINE trial_sub

END MODULE mod_subrou
