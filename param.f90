MODULE param

    REAL(8), PARAMETER :: alpha=1.5d0       !elasticity of substitution
    REAL(8), PARAMETER :: beta=0.99d0
    REAL(8), PARAMETER :: r=0.01d0

    REAL(8), PARAMETER :: gamma=0.5d0
    REAL(8), PARAMETER :: omega=1.d0

    INTEGER, PARAMETER :: na = 1000
    INTEGER, PARAMETER :: mn_a = 0.d0
    INTEGER, PARAMETER :: mx_a = 10.d0
   
    INTEGER, PARAMETER :: mx_it = 10000
    REAL(8), PARAMETER :: tol = 1.d-6

END MODULE param
