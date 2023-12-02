!*==subb.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subb(Kb,Ks,I,J,Jb,Lb,Ls,Ndy,Nyfl,Pi,Eps,Sgr,Cgr,Ar,Beta,Sum,Ria,Delx,Yb,Zb,Ys,Zs,X)
   USE c_amgmn
   USE c_kds
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kb
   INTEGER :: Ks
   INTEGER :: I
   INTEGER :: J
   INTEGER :: Jb
   INTEGER :: Lb
   INTEGER :: Ls
   INTEGER :: Ndy
   INTEGER :: Nyfl
   REAL :: Pi
   REAL :: Eps
   REAL :: Sgr
   REAL :: Cgr
   REAL :: Ar
   REAL :: Beta
   COMPLEX :: Sum
   REAL , DIMENSION(1) :: Ria
   REAL , DIMENSION(1) :: Delx
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Zb
   REAL , DIMENSION(1) :: Ys
   REAL , DIMENSION(1) :: Zs
   REAL , DIMENSION(1) :: X
!
! Local variable declarations rewritten by SPAG
!
   REAL :: absyb , abszb , anot , ao , d2d , dxs , dzdyi , dzdyr , eta , flnd , flne , sign1 , test1 , test2 , xi1 , xi2 , xx , y , &
         & z , zeta
   COMPLEX :: dp , dpll , dplr , dpul , dpur
   INTEGER :: idflag , idzdy , iflag , igo , lhs
   EXTERNAL dzy
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!   ***   COMPUTES ELEMENTS OF THE SUBMATRICES  DZP, DZZ, DZY, DYP,
!         DYZ  AND  DYY  USING  SUBROUTINE  DZY
         flnd = float(nd)
         flne = float(ne)
         ind = 0
         dpur = (0.0,0.0)
         dpul = (0.0,0.0)
         dplr = (0.0,0.0)
         dpll = (0.0,0.0)
         anot = Ria(Jb)
         dxs = Delx(J)
         absyb = abs(Yb(Lb))
         abszb = abs(Zb(Lb))
         iflag = 0
         idflag = 0
         IF ( Kb/=0 ) THEN
            test1 = abs(Yb(Lb)-Yb(Kb))
            test2 = abs(Zb(Lb)-Zb(Kb))
            IF ( test1<=Eps .AND. test2<=Eps ) THEN
               iflag = 1
               IF ( Ndy==Nyfl ) THEN
                  IF ( I==J ) THEN
                     idflag = 1
                     d2d = 1.0/(2.0*Pi*anot*anot*(1.0+Ar))
                     IF ( Ndy/=0 ) d2d = d2d/Ar
                     Sum = cmplx(d2d,0.0)
                     sign1 = 1.0
                     IF ( Ndy/=0 ) sign1 = -1.0
                     IF ( absyb<Eps ) Sum = (1.0+sign1*flnd)*Sum
                     IF ( abszb<Eps ) Sum = (1.0+sign1*flne)*Sum
                     dpur = Sum
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         xx = X(I)
         y = Ys(Ks)
         z = Zs(Ks)
         xi1 = X(J) - 0.5*dxs
         xi2 = X(J) + 0.5*dxs
         eta = Ys(Ls)
         zeta = Zs(Ls)
         ao = anot
         idzdy = Ndy
         igo = 1
         lhs = 0
         IF ( iflag==1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL dzy(xx,y,z,Sgr,Cgr,xi1,xi2,eta,zeta,Ar,ao,kr,refc,Beta,fmach,lhs,idzdy,dzdyr,dzdyi)
            dp = cmplx(dzdyr,dzdyi)
            IF ( igo==2 ) THEN
!  UPPER LEFT-HAND SIDE CONTRIBUTION
               dpul = dp
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( igo==3 ) THEN
!  LOWER RIGHT-HAND SIDE CONTRIBUTION
               dplr = dp
               IF ( nd==0 ) THEN
                  Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
                  RETURN
               ELSEIF ( idflag==1 .AND. absyb<Eps ) THEN
                  Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
                  RETURN
               ELSE
                  igo = 4
                  eta = -Ys(Ls)
                  zeta = -Zs(Ls)
                  lhs = 0
               ENDIF
            ELSEIF ( igo==4 ) THEN
!  LOWER  LEFT-HAND SIDE CONTRIBUTION
               dpll = dp
               Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
               RETURN
            ELSE
!  UPPER RIGHT-HAND SIDE CONTRIBUTION
               dpur = dp
               IF ( Kb/=Lb ) EXIT SPAG_Loop_1_1
               RETURN
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         IF ( nd/=0 ) THEN
            IF ( idflag/=1 .OR. absyb>=Eps ) THEN
               igo = 2
               eta = -Ys(Ls)
               lhs = 1
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( ne==0 ) THEN
            Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
         ELSEIF ( idflag==1 .AND. abszb<Eps ) THEN
            Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
         ELSE
            igo = 3
            eta = Ys(Ls)
            zeta = -Zs(Ls)
            lhs = 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE subb
