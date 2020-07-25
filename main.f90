PROGRAM main  !Program shell

 USE param
 USE mod_subrou
 USE ppsplinefit3edit

IMPLICIT NONE

  INTEGER :: t,i,j,k,b
  REAL(8) :: step, v0
  REAL(8), DIMENSION(na) :: v, tv, a, ap, w
  INTEGER, DIMENSION(na) :: pol
  REAL(8), DIMENSION(na,na) :: c
  ! REAL(8) :: beta
!  REAL(8) :: phi
  REAL(8) :: theta
  INTEGER:: ns2
  REAL(8), ALLOCATABLE:: s(:), csV(:,:), L(:,:), U(:,:), dtau(:), &
    v2(:,:), Sf(:), Vf(:,:), SVEC(:,:)
  !beta = 0.9
!  phi = 2.d0
  theta = 4.d0

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


  ! Fit spline
  ! ----------

  ns2 = ns - 2
  ALLOCATE(s(ns), L(1, ns2-1), U(2, ns2), dtau(ns2+1))  ! Triangulation of knot points
  ALLOCATE(v2(ns,m),SVEC(2,m),csV(4,(ns2+1)*m))         ! V at knot points. Coefficients of spline
  ALLOCATE(Sf(nf),Vf(order+1,nf*m))                     ! Evaluate spline function

  ! Select subset of nodes

  b = (na-1)/(ns-1) ! Integer jump
  DO i=0,ns-1
  j = 1 + b*i
  s(i+1) = a(j)
    DO k=1,m
    v2(i+1,k) = v(j)
    END DO
  END DO

  ! Set end point condition values (Only relevant if indicator = natural or complete)
  SVEC(1,:)=-100000.d0
  SVEC(2,:)=0.d0

  ! Evaluate all nodes
  Sf = a

  ! Find LU decompostion of knots
  CALL SPLHS(s, ns2, indicator, L, U, dtau)
  ! Fit a spline to update polynomial approximations V - Find coefficient csV
  CALL SPpp(v2, ns2, m, L, U, dtau, indicator, SVEC, csV)
  ! Evaluate function value on nodes Sf: m functions
  CALL SPeval(csV, s, ns2, m, Sf, nf, order, Vf)

  WRITE(*,*) ' FINISHED! '
!  WRITE(*,*) ' s, v2', s,v2
!  WRITE(*,*) ' csv: ', csV
  WRITE(*,*) ' iteration: ',t, ' tolerance: ', step
  WRITE(*,*) ' MA: beta, phi, theta= ', beta, phi, theta
  WRITE(*,*) ' V(0)= ', v0, v(1)
  WRITE(*,*) ' Vf= ', v(na/2), Vf(:,nf/2)

  OPEN (UNIT=25, FILE="Output.txt", ACTION="WRITE", POSITION="REWIND", RECL=1)
!  WRITE(25,*) ' A ', ' V ',' POL ',' W ',' Vf '
!    DO i=1,na
!    WRITE(25,*) a(i),v(i),pol(i),w(i),Vf(1,i),';'
!    END DO
!  -----------
   WRITE(25,*) a,';'
   WRITE(25,*) v,';'
   WRITE(25,*) pol,';'
   WRITE(25,*) w,';'
   WRITE(25,*) Vf,';'
  CLOSE(25)


END PROGRAM main
