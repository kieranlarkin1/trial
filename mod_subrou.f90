MODULE mod_subrou

REAL(8), PARAMETER :: aa = 2.d0

CONTAINS

SUBROUTINE trial_sub(v,n,out)

INTEGER, INTENT(IN) :: n
REAL(8), DIMENSION(n), INTENT(IN) :: v

out = aa * V

END SUBROUTINE trial_sub

END MODULE mod_subrou
