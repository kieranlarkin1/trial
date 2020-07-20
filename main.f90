PROGRAM main  !Program shell

 USE param
 USE mod_subrou

IMPLICIT NONE

  INTEGER :: t,i,j
  REAL(8) :: step
  REAL(8), DIMENSION(na) :: v, tv, a, ap, w
  INTEGER, DIMENSION(na) :: ap_i, pol
  REAL(8), DIMENSION(na,2) :: c
  ! REAL(8) :: beta
  !
  ! beta = 0.9

  WRITE(*,*) ' HELLO WORLD '
  step = (mx_a-mn_a)/(na-1)
  a = (/ (mn_a+i*step, i = 0, na-1) /)

  WRITE(*,*) ' step: ', step
  WRITE(*,*) ' c, a ',c,a

  v = 0.d0
  tv = 0.d0

  ! Choose to consume all or gamma share
  DO i=1,na
  c(i,1) = LOG(a(i)+omega)
  c(i,2) = LOG(gamma*a(i)+omega)
  ap(i) = (1.d0-gamma)*a(i)*(1.d0+r)
  ap_i(i) = MINLOC( ABS(ap(i)-a), 1 )
  END DO

  DO t=1,mx_it
    DO i=1,na
    tv(i) = MAX(c(i,1), c(i,2)+beta*v(ap_i(i)) )
      IF ( tv(i) .GT. c(i,1) ) THEN
      pol(i) = 1
      ELSE
      pol(i) = 0
      END IF
    END DO
  step = MAXVAL(ABS(tv-v))
  IF (step .LT. tol) EXIT
  v = tv
  END DO

  CALL trial_sub(v,na,w)

  WRITE(*,*) ' FINISHED! '
  WRITE(*,*) ' iteration: ',i, ' tolerance: ', step
  WRITE(*,*) ' beta= ', beta, aa

  OPEN (UNIT=25, FILE="Output.txt", ACTION="WRITE", POSITION="REWIND")
  WRITE(25,*) ' V ',' POL ',' W '
    DO i=1,na
    WRITE(25,*) v(i),pol(i),w(i)
    END DO
  CLOSE(25)

END PROGRAM main
