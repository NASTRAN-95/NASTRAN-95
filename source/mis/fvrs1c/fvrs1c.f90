!*==fvrs1c.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fvrs1c(Z,W1,Omega,Nf)
   USE c_blank
   USE c_condas
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nf
   COMPLEX , DIMENSION(3,Nf) :: Z
   REAL , DIMENSION(Nf) :: W1
   REAL :: Omega
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , b , cy , cz , f , phi , rad , xo , yo , yy , zo , zz
   INTEGER :: i , j , kk , kkk , ll
   COMPLEX :: p , py1a , py1b , py2a , py2b , pz1a , pz1b , pz2a , pz2b , z1
   EXTERNAL tab
!
! End of declarations rewritten by SPAG
!
!----------------------------------------------------------------------
!
!
!----------------------------------------------------------------------
   ll = 1
   DO kkk = 1 , Nf
      IF ( W1(kkk)==0.0 ) THEN
!
!     COMPUTE BASE(FI)(3X2) IF W1=0.0, FOR MODFRL=TRUE
!
         a = 1.0
         IF ( Omega<0.0 ) a = -1.0
!------ZERO OUT MATRIX(3X2)
         kk = ll + 1
         DO i = 1 , 3
            DO j = ll , kk
               Z(i,j) = (0.0,0.0)
            ENDDO
         ENDDO
         f = W1(kkk)/twopi
         IF ( ixt==-1 ) THEN
            Z(1,ll) = (0.0,0.0)
         ELSE
            CALL tab(ixt,f,xo)
            IF ( ixp==-1 ) THEN
               p = (1.0,0.0)
            ELSE
               CALL tab(ixp,f,phi)
               rad = phi*degra
               z1 = cmplx(0.0,rad)
               p = cexp(z1)
            ENDIF
            Z(1,ll) = xo*p
         ENDIF
         IF ( iyt==-1 ) THEN
            yy = 0.0
         ELSE
            CALL tab(iyt,f,yo)
            IF ( iyp==-1 ) THEN
               cy = 1.0
            ELSE
               CALL tab(iyp,f,phi)
               rad = phi*degra
               cy = cos(rad)
            ENDIF
            yy = yo*cy
         ENDIF
         IF ( izt==-1 ) THEN
            zz = 0.0
         ELSE
            CALL tab(izt,f,zo)
            IF ( izp==-1 ) THEN
               cz = 1.0
            ELSE
               CALL tab(izp,f,phi)
               rad = phi*degra
               cz = cos(rad)
            ENDIF
            zz = zo*cz
         ENDIF
         Z(2,kk) = yy - a*cmplx(0.0,zz)
         Z(3,kk) = zz + a*cmplx(0.0,yy)
         ll = ll + 2
      ELSE
         a = 1.0
         IF ( W1(kkk)-Omega<0.0 ) a = -1.0
         b = 1.0
         IF ( W1(kkk)+Omega<0.0 ) b = -1.0
!
!     COMPUTE BASE(FI)(3X3)  IF W.NE.0--MODFRL=TRUE
!
!     ZERO OUT MATRIX
!
         kk = ll + 2
         DO i = 1 , 3
            DO j = ll , kk
               Z(i,j) = (0.0,0.0)
            ENDDO
         ENDDO
         f = W1(kkk)/twopi
         IF ( ixt/=-1 ) THEN
            CALL tab(ixt,f,xo)
            IF ( ixp==-1 ) THEN
               p = (1.0,0.0)
            ELSE
               CALL tab(ixp,f,phi)
               rad = phi*degra
               z1 = cmplx(0.0,rad)
               p = cexp(z1)
            ENDIF
            Z(1,ll+1) = xo*p
         ENDIF
         IF ( iyt==-1 ) THEN
            yo = 0.0
         ELSE
            CALL tab(iyt,f,yo)
         ENDIF
         IF ( izt==-1 ) THEN
            zo = 0.0
         ELSE
            CALL tab(izt,f,zo)
         ENDIF
         IF ( iyp==-1 ) THEN
            phi = 0.0
         ELSE
            CALL tab(iyp,f,phi)
         ENDIF
         rad = phi*degra
         z1 = cmplx(0.0,rad)
         py1a = cexp(a*z1)
         py1b = cexp(b*z1)
         z1 = cmplx(0.0,rad-0.5*pi*a)
         py2a = cexp(a*z1)
         z1 = cmplx(0.0,rad-0.5*pi*b)
         py2b = cexp(b*z1)
         IF ( izp==-1 ) THEN
            phi = 0.0
         ELSE
            CALL tab(izp,f,phi)
         ENDIF
         rad = phi*degra
         z1 = cmplx(0.0,rad)
         pz1a = cexp(a*z1)
         pz1b = cexp(b*z1)
         z1 = cmplx(0.0,rad-0.5*pi*a)
         pz2a = cexp(a*z1)
         z1 = cmplx(0.0,rad-0.5*pi*b)
         pz2b = cexp(b*z1)
         Z(2,ll) = (yo*py1a-a*zo*pz2a)*0.5
         Z(3,ll) = (a*yo*py2a+zo*pz1a)*0.5
         Z(2,ll+2) = (yo*py1b+b*zo*pz2b)*0.5
         Z(3,ll+2) = (-b*yo*py2b+zo*pz1b)*0.5
         ll = ll + 3
      ENDIF
   ENDDO
END SUBROUTINE fvrs1c
