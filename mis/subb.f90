
SUBROUTINE subb(Kb,Ks,I,J,Jb,Lb,Ls,Ndy,Nyfl,Pi,Eps,Sgr,Cgr,Ar,Beta,Sum,Ria,Delx,Yb,Zb,Ys,Zs,X)
   IMPLICIT NONE
   REAL Fmach , Kd1i , Kd1r , Kd2i , Kd2r , Refc
   INTEGER Ind , Kr , Mcb(7) , Nd , Ne , Nrow
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Kr
   COMMON /kds   / Ind , Kd1r , Kd1i , Kd2r , Kd2i
   REAL Ar , Beta , Cgr , Eps , Pi , Sgr
   INTEGER I , J , Jb , Kb , Ks , Lb , Ls , Ndy , Nyfl
   COMPLEX Sum
   REAL Delx(1) , Ria(1) , X(1) , Yb(1) , Ys(1) , Zb(1) , Zs(1)
   REAL absyb , abszb , anot , ao , d2d , dxs , dzdyi , dzdyr , eta , flnd , flne , sign1 , test1 , test2 , xi1 , xi2 , xx , y , z ,&
      & zeta
   COMPLEX dp , dpll , dplr , dpul , dpur
   INTEGER idflag , idzdy , iflag , igo , lhs
!   ***   COMPUTES ELEMENTS OF THE SUBMATRICES  DZP, DZZ, DZY, DYP,
!         DYZ  AND  DYY  USING  SUBROUTINE  DZY
   flnd = float(Nd)
   flne = float(Ne)
   Ind = 0
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
   IF ( iflag==1 ) GOTO 200
 100  DO
      CALL dzy(xx,y,z,Sgr,Cgr,xi1,xi2,eta,zeta,Ar,ao,Kr,Refc,Beta,Fmach,lhs,idzdy,dzdyr,dzdyi)
      dp = cmplx(dzdyr,dzdyi)
      IF ( igo==2 ) THEN
!  UPPER LEFT-HAND SIDE CONTRIBUTION
         dpul = dp
         GOTO 300
      ELSEIF ( igo==3 ) THEN
!  LOWER RIGHT-HAND SIDE CONTRIBUTION
         dplr = dp
         IF ( Nd==0 ) THEN
            Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
            GOTO 99999
         ELSEIF ( idflag==1 .AND. absyb<Eps ) THEN
            Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
            GOTO 99999
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
         GOTO 99999
      ELSE
!  UPPER RIGHT-HAND SIDE CONTRIBUTION
         dpur = dp
         IF ( Kb/=Lb ) EXIT
         GOTO 99999
      ENDIF
   ENDDO
 200  IF ( Nd/=0 ) THEN
      IF ( idflag/=1 .OR. absyb>=Eps ) THEN
         igo = 2
         eta = -Ys(Ls)
         lhs = 1
         GOTO 100
      ENDIF
   ENDIF
 300  IF ( Ne==0 ) THEN
      Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
   ELSEIF ( idflag==1 .AND. abszb<Eps ) THEN
      Sum = dpur + flnd*dpul + flne*dplr + flnd*flne*dpll
   ELSE
      igo = 3
      eta = Ys(Ls)
      zeta = -Zs(Ls)
      lhs = 1
      GOTO 100
   ENDIF
99999 RETURN
END SUBROUTINE subb