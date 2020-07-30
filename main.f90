PROGRAM main  !Program shell

 USE kindset
 USE param
 USE mod_subrou
 USE ppsplinefit3edit

IMPLICIT NONE

  INTEGER :: t,i,j,k,o
  REAL(rk) :: step, v0, b,bb
  REAL(rk), DIMENSION(na) :: a, w
  REAL(rk), DIMENSION(ne) :: e, risk
  REAL(rk), DIMENSION(na,ne) :: v, tv
  INTEGER(ik), DIMENSION(na,ne) :: pol
  REAL(rk), DIMENSION(na,ne,na) :: c
  ! REAL(rk) :: beta
!  REAL(rk) :: phi
  REAL(rk) :: theta
  INTEGER:: ns2
  REAL(rk), ALLOCATABLE:: s(:), csV(:,:), L(:,:), U(:,:), dtau(:), &
    v2(:,:), Sf(:), Vf(:,:), SVEC(:,:), Vf_out(:,:)

  INTEGER(ik) :: clmat
  REAL(rk), ALLOCATABLE :: s_e(:), Vfm(:,:), knots(:,:) ! knots is as many dimesions and dimensions of function. Vfm stays (:,:)
  INTEGER(ik), ALLOCATABLE :: numelem(:,:)
  REAL(rk), ALLOCATABLE :: LM(:,:), UM(:,:,:), dtaum(:,:,:), cV(:,:) ! These stay same regardless of dimension

  !beta = 0.9
!  phi = 2.d0
  theta = 4.d0

  WRITE(*,*) ' HELLO WORLD '
  step = (mx_a-mn_a)/(na-1)
  a = (/ (mn_a+i*step, i = 0, na-1) /)
  step = (mx_e-mn_e)/(ne-1)
  e = (/ (mn_e+i*step, i = 0, ne-1) /)
  step = 2.d0*sigma_r/(ne-1)
  risk = (/ (-sigma_r+i*step, i = 0, ne-1) /)

  v0 = log(omega+e(ne0))/(1-beta)
  v = 0.d0
  tv = 0.d0
  c = 0.d0

  ! Choose to consume all or gamma share
  DO i=1,na
    DO j=1,ne
      DO k=1,na
      c(i,j,k) = a(i)*(1.d0+r+risk(j))+omega+e(j)-a(k)
      END DO
    END DO
  END DO

  DO t=1,mx_it
    DO i=1,na
      DO j=1,ne
        DO k=1,na
          IF (c(i,j,k) .GT. 0.d0) THEN
            IF (alpha==1.d0) THEN
              w(k) = LOG( c(i,j,k) )
            ELSE
              w(k) = ( c(i,j,k)**(1.d0-alpha) - 1.d0 )/(1.d0-alpha)
            END IF
            w(k) = w(k) + beta*( pi*v(k,j) + (1.d0-pi)*v(k,ne0) )
          ELSE
            w(k) = vmin
          END IF
        END DO

        pol(i,j) = MAXLOC(w,1)
        tv(i,j) = w(pol(i,j))
      END DO
    END DO
  step = MAXVAL(ABS(tv-v))
  IF (step .LT. tol) EXIT
  v = tv
  END DO

  CALL trial_sub(v(:,ne0),na,w)


  ! Fit spline
  ! ----------

  ns2 = ns - 2
  ALLOCATE(s(ns), L(1, ns2-1), U(2, ns2), dtau(ns2+1))  ! Triangulation of knot points
  ALLOCATE(v2(ns,m),SVEC(2,m),csV(4,(ns2+1)*m))         ! V at knot points. Coefficients of spline
  ALLOCATE(Sf(nf),Vf(order+1,nf*m),Vf_out(nf,m))        ! Evaluate spline function
  ALLOCATE(s_e(m))                                      ! Multidimensional spline

  ! Select subset of nodes
  b = (DBLE(na-1))/(DBLE(ns-1)) ! Integer jump
  bb = (DBLE(ne-1))/(DBLE(m-1)) ! Integer jump
  DO i=0,ns-1
  j = 1 + NINT(b*i)
  s(i+1) = a(j)
    DO k=0,m-1
      IF (i==0) THEN ! Store nodes
      s_e(k+1)=e(o)   ! Note: Could be either e or risk
!      s_e(k+1)=e(risk)
      END IF
    o = 1 + NINT(bb*k)
    v2(i+1,k+1) = v(j,o)
    END DO
  END DO

  ! Set end point condition values (Only relevant if indicator = natural or complete)
  SVEC(1,:)=-1.d1
  SVEC(2,:)=0.d0

  ! Evaluate all nodes
  Sf = a

  ! Find LU decompostion of knots
  CALL SPLHS(s, ns2, indicator, L, U, dtau)
  ! Fit a spline to update polynomial approximations V - Find coefficient csV
  CALL SPpp(v2, ns2, m, L, U, dtau, indicator, SVEC, csV)
  ! Evaluate function value on nodes Sf: m functions
  CALL SPeval(csV, s, ns2, m, Sf, nf, order, Vf)

  ! Reallocate Vf to similar form as value function
  DO j=1,m
  Vf_out(:,j) = Vf(1, (j-1)*nf+1:j*nf )
  END DO
    WRITE(*,*) ' 1: '
  ! Fit multi-dimensional spline
  ! ----------
  ! MULTIVARIATE KNOT SETUP
  ALLOCATE(numelem(4,md))
  ! preliminary spline fit step, SPFitA computes knot dependent terms, numelem is always (4 x m).
  CALL SPFitA0(md, ns_m, clmat, numelem)
      WRITE(*,*) ' 2: '  ! clmat: largest dimension. numelem: spline coefficents dimensions
  ALLOCATE(knots(clmat+2,md),LM(md, clmat), UM(2, clmat+1, md), dtaum(clmat+1, numelem(1,md), md), &
  		 cV(4, numelem(3,md)) )

  ALLOCATE( Vfm(na,ne) )

  ! Knot points for multidimensional spline (just collect grids in each direction)
  knots(1:ns_m(1),1)=s
  knots(1:ns_m(2),2)=s_e

  cV = 0.d0
  ! first spline fit step: determine function independent LHS terms for slope solution
  CALL SPFitA(md, ns_m, clmat, knots, numelem, LM, UM, dtaum)
  ! Generate LU and dtau matricies for multidimensional spline.
      WRITE(*,*) ' 3: '
  CALL SPFitB(md, clmat, ns_m, numelem, LM, UM, dtaum, v2, cV)
      WRITE(*,*) ' 4: '
  ! Fit polynomial to splines. Generates Coefficients cV

  ! Only evaluates one value at a time
  DO i=1,na
    DO j=1,ne
    Vfm(i,j) = FastSplinEval(md, numelem(3,md), cV, ns_m-1, clmat+2, knots, &
              (/a(i), e(j)/), order_m)
   END DO
 END DO
     WRITE(*,*) ' 5: '

  WRITE(*,*) ' FINISHED! '
!  WRITE(*,*) ' s: ', s
!  WRITE(*,*) ' v2: ', v2
!  WRITE(*,*) ' csv: ', csV
!  WRITE(*,*) ' risk: ', risk
  WRITE(*,*) ' iteration: ',t, ' tolerance: ', step
  WRITE(*,*) ' MA: beta, phi, theta= ', beta, phi, theta
  WRITE(*,*) ' V(0)= ', v0, v(1,ne0)
  WRITE(*,*) ' Vf= ', v(na/2,1), Vf_out(na/2,1),Vfm(na/2,1)
!  WRITE(*,*) ' Vf= ', Vf

  OPEN (UNIT=25, FILE="Output.txt", ACTION="WRITE")
!  WRITE(25,*) ' A ', ' V ',' POL ',' W ',' Vf '
!    DO i=1,na
!    WRITE(25,*) a(i),v(i),pol(i),w(i),Vf(1,i),';'
!    END DO
!  -----------
   WRITE(25,*) a,';'
   WRITE(25,*) pol(:,ne0),';'
   WRITE(25,*) w,';'
   DO i=1,ne
   WRITE(25,*) v(:,i),';'
   END DO
   DO i=1,m
   WRITE(25,*) Vf_out(:,i),';'
   END DO
   DO i=1,ne
   WRITE(25,*) Vfm(:,i),';'
   END DO
  CLOSE(25)


END PROGRAM main
