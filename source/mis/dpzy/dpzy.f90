!*==dpzy.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpzy(Kb,Iz,I,J1,J2,Ifirst,Ilast,Yb,Zb,Avr,Arb,Th1a,Th2a,Nt121,Nt122,Nbaray,Ncaray,Nzykb,Dpz,Dpy)
   USE c_dlbdy
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kb
   INTEGER :: Iz
   INTEGER :: I
   INTEGER :: J1
   INTEGER :: J2
   INTEGER :: Ifirst
   INTEGER :: Ilast
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Zb
   REAL , DIMENSION(1) :: Avr
   REAL , DIMENSION(1) :: Arb
   REAL , DIMENSION(1) :: Th1a
   REAL , DIMENSION(1) :: Th2a
   INTEGER , DIMENSION(1) :: Nt121
   INTEGER , DIMENSION(1) :: Nt122
   INTEGER , DIMENSION(1) :: Nbaray
   INTEGER , DIMENSION(1) :: Ncaray
   INTEGER :: Nzykb
   COMPLEX , DIMENSION(1) :: Dpz
   COMPLEX , DIMENSION(1) :: Dpy
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cgr , cmult , delth , pi , rho , sgr , smult , theta , thm1 , thp1 , yrec , zrec
   INTEGER :: ix , ix1 , ix2 , ixm1 , ixp1 , j , ksp , l , ls , nbcum , nbxs , nc1
   COMPLEX :: sum
   EXTERNAL subpb
!
! End of declarations rewritten by SPAG
!
!   ***   GENERATES ROWS OF THE SUBMATRICES  DPZ  AND DPY  USING
!         SUBROUTINE  SUBP
   pi = 3.1415926
   ix1 = 1
   Iz = Iz + 1
!  IZ  IS THE BODY-ELEMENT NUMBER FOR BODY  KB  --  IZ RUNS FROM  1
!  THROUGH  NBE-SUB-KB
   ix2 = Nt122(Kb)
   IF ( Iz>=Ifirst .AND. Iz<=Ilast ) ix2 = Nt121(Kb)
   DO ix = ix1 , ix2
      l = 1
      ksp = 0
!  L IS THE PANEL NUMBER ASSOCIATED WITH SENDING   POINT  J
      ls = 1
!  LS IS THE STRIP NUMBER ASSOCIATED WITH SENDING   POINT  J
      nbxs = Nbaray(l)
      nc1 = Ncaray(l)
      nbcum = nc1
      ixp1 = ix + 1
      IF ( ixp1>ix2 ) ixp1 = ix1
      ixm1 = ix - 1
      IF ( ixm1==0 ) ixm1 = ix2
      IF ( Iz>=Ifirst .AND. Iz<=Ilast ) THEN
         theta = Th1a(ix)
         thp1 = Th1a(ixp1)
         thm1 = Th1a(ixm1)
      ELSE
         theta = Th2a(ix)
         thp1 = Th2a(ixp1)
         thm1 = Th2a(ixm1)
      ENDIF
      IF ( ix==ix1 ) thm1 = thm1 - 2.0*pi
      IF ( ix==ix2 ) thp1 = thp1 + 2.0*pi
      delth = 0.5*(thp1-thm1)
      yrec = Yb(Kb) + Avr(Kb)*cos(theta)
      zrec = Zb(Kb) + Avr(Kb)*Arb(Kb)*sin(theta)
      rho = sqrt(1.0+(Arb(Kb)**2-1.0)*(cos(theta))**2)
      sgr = -Arb(Kb)*cos(theta)/rho
      cgr = sin(theta)/rho
      smult = sin(theta)*rho/pi
      cmult = cos(theta)*rho/pi
      DO j = J1 , J2
         CALL subpb(I,l,ls,j,sgr,cgr,yrec,zrec,sum,z(ixic),z(idelx),z(iee),z(ixlam),z(isg),z(icg),z(iys),z(izs),z(inas),z(inasb+ksp)&
                  & ,z(iavr),z(izb),z(iyb),z(iarb),z(ixle),z(ixte),z(ia),nb)
         IF ( Nzykb==3 ) THEN
            Dpy(j) = Dpy(j) + sum*cmult*delth
         ELSE
            Dpz(j) = Dpz(j) + sum*smult*delth
            IF ( Nzykb/=1 ) Dpy(j) = Dpy(j) + sum*cmult*delth
         ENDIF
         IF ( j/=J2 ) THEN
            IF ( j>=nbxs ) THEN
               ksp = ksp + z(inas+l-1)
               l = l + 1
               nc1 = Ncaray(l)
               nbxs = Nbaray(l)
            ENDIF
            IF ( j>=nbcum ) THEN
               ls = ls + 1
               nbcum = nbcum + nc1
            ENDIF
         ENDIF
      ENDDO
   ENDDO
END SUBROUTINE dpzy
