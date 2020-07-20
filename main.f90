PROGRAM main  !Program shell

 USE param
 USE mod_subrou

IMPLICIT NONE

  INTEGER :: t,i,j
  REAL(8) :: step, v0
  REAL(8), DIMENSION(na) :: v, tv, a, ap, w
  INTEGER, DIMENSION(na) :: pol
  REAL(8), DIMENSION(na,na) :: c
  ! REAL(8) :: beta
  REAL(8) :: phi

  !beta = 0.9
  phi = 2.d0

  WRITE(*,*) ' HELLO WORLD '
  step = (mx_a-mn_a)/(na-1)
  a = (/ (mn_a+i*step, i = 0, na-1) /)

  v0 = log(omega)/(1-beta)
  v = 0.d0
  tv = 0.d0
  c = 0.d0

  ! Choose to consume all or gamma share
  DO i=1,na
    DO j=1,na
      c(i,j) = a(i)*(1.d0+r)+omega-a(j)
    END DO
  END DO

  DO t=1,mx_it
    DO i=1,na
      DO j=1,na
        IF (c(i,j) .GT. 0.d0) THEN
          w(j) = LOG( c(i,j) ) + beta*v(j)
        ELSE
          w(j) = vmin
        END IF
      END DO
      pol(i) = MAXLOC(w,1)
      tv(i) = w(pol(i))
    END DO
  step = MAXVAL(ABS(tv-v))
  IF (step .LT. tol) EXIT
  v = tv
  END DO

  CALL trial_sub(v,na,w)

  WRITE(*,*) ' FINISHED! '
  WRITE(*,*) ' iteration: ',i, ' tolerance: ', step
  WRITE(*,*) ' beta= ', beta, phi
  WRITE(*,*) ' V(0)= ', v0, v(1)

  OPEN (UNIT=25, FILE="Output.txt", ACTION="WRITE", POSITION="REWIND")
  WRITE(25,*) ' V ',' POL ',' W '
    DO i=1,na
    WRITE(25,*) v(i),pol(i),w(i)
    END DO
  CLOSE(25)

END PROGRAM main
