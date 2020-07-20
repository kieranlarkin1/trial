MODULE mod_subrou

IMPLICIT NONE

REAL(8), PARAMETER :: phi = 4.d0

CONTAINS

SUBROUTINE trial_sub(v,n,out)
IMPLICIT NONE

INTEGER, INTENT(IN) :: n
REAL(8), DIMENSION(n), INTENT(IN) :: v
REAL(8), DIMENSION(n), INTENT(OUT) :: out
REAL(8), PARAMETER :: theta = 2.d0

out = (phi/theta) * V

END SUBROUTINE trial_sub

END MODULE mod_subrou
