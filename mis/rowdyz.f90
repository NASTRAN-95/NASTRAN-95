
SUBROUTINE rowdyz(Nfb,Nlb,Row,Ntzys,D,Dx,Dy,Dz,Beta,Idzdy,Ntape,Sgr,Cgr,Iprnt,Yb,Zb,Ar,Nsbe,Xis1,Xis2,A0)
   IMPLICIT NONE
   REAL Fmach , Kr , Refc
   INTEGER Mcb(7) , N , Nd , Ne , Npot , Nrow
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Kr
   COMMON /system/ N , Npot
   REAL Beta , Cgr , Dx , Dy , Dz , Sgr
   INTEGER Idzdy , Iprnt , Nfb , Nlb , Ntape , Ntzys , Row
   REAL A0(1) , Ar(1) , D(2,Ntzys) , Xis1(1) , Xis2(1) , Yb(1) , Zb(1)
   INTEGER Nsbe(1)
   REAL azro , dar , delta , dzyi , dzyr , epslon , eta , xi1 , xi2 , zeta
   INTEGER b , b1 , it1 , jdzdy , lhs , nsbeb , t , t1
!
!     CALCULATE A ROW OF DZ OR DY
!
!     SLENDER BODY
!
!     NFB       FIRST BODY OF THE DESIRED ORIENTATION - Z OR Y -
!     NLB       LAST  BODY OF THE DESIRED ORIENTATION
!     ROW       ROW OF DZ OR DY BEING CALCULATED
!     NTZYS     NO. COLUMNS TO BE CALCULATED
!     D         CALCULATED ROW
!     DX        X - COORD. OF RECEIVING POINT
!     DY        Y - COORD. OF RECEIVING POINT
!     DZ        Z - COORD. OF RECEIVING POINT
!     BETA      EQUALS SQRT(1-M**2)
!     MACH      MACH NO., M
!     IDZDY     FLAG REQUIRED FOR FLLD
!
!
   delta = float(Nd)
   epslon = float(Ne)
   b1 = 0
   t1 = 0
   it1 = 0
   IF ( Nfb/=1 .AND. Idzdy/=0 ) THEN
      b = Nfb - 1
      DO t = 1 , b
         it1 = it1 + Nsbe(t)
      ENDDO
   ENDIF
   DO b = Nfb , Nlb
      b1 = b1 + 1
      dar = Ar(b)
      nsbeb = Nsbe(b)
      IF ( Iprnt/=0 ) WRITE (Npot,99001) b , Dy , Yb(b) , Dz , Zb(b)
99001 FORMAT (12H ROWDYZ  B =,I10,4E20.8)
!
!     LOOP FOR EACH ELEMENT IN BODY -B-
!
      DO t = 1 , nsbeb
         t1 = t1 + 1
         it1 = it1 + 1
         D(1,t1) = 0.0
         D(2,t1) = 0.0
         xi1 = Xis1(it1)
         xi2 = Xis2(it1)
         azro = A0(it1)
         eta = Yb(b)
         zeta = Zb(b)
!
!     CHECK TO SEE IF CALCULATIONS ARE TO BE MADE
!
         IF ( Dy==eta .AND. Dz==zeta ) GOTO 40
         ASSIGN 20 TO jdzdy
         lhs = 0
         GOTO 160
 20      D(1,t1) = dzyr
         D(2,t1) = dzyi
!
!     SKIP IF NO SYMMETRY
!
 40      IF ( delta==0.0 ) GOTO 120
         eta = -Yb(b)
!
!     CHECK TO SEE IF CALCULATIONS ARE TO BE MADE
!
         IF ( Dy==eta .AND. Dz==zeta ) GOTO 80
         lhs = 1
         ASSIGN 60 TO jdzdy
         GOTO 160
 60      D(1,t1) = D(1,t1) + delta*dzyr
         D(2,t1) = D(2,t1) + delta*dzyi
 80      IF ( epslon==0.0 ) CYCLE
!
!     CALC. ONLY IF DELTA AND EPSLON  NOT EQUAL ZERO
!
         eta = -Yb(b)
         zeta = -Zb(b)
!
!     CHECK TO SEE IF CALCULATIONS ARE TO BE MADE
!
         IF ( Dy==eta .AND. Dz==zeta ) GOTO 120
         ASSIGN 100 TO jdzdy
         GOTO 160
 100     D(1,t1) = D(1,t1) + epslon*delta*dzyr
         D(2,t1) = D(2,t1) + epslon*delta*dzyi
!
!     SKIP IF NO GROUND EFFECTS
!
 120     IF ( epslon==0.0 ) CYCLE
         eta = Yb(b)
         zeta = -Zb(b)
!
!     CHECK TO SEE IF CALCULATIONS ARE TO BE MADE
!
         IF ( Dy==eta .AND. Dz==zeta ) CYCLE
         lhs = 1
         ASSIGN 140 TO jdzdy
         GOTO 160
 140     D(1,t1) = D(1,t1) + epslon*dzyr
         D(2,t1) = D(2,t1) + epslon*dzyi
         CYCLE
!
!     CALL SEQUENCE TO DZY
!
 160     CALL dzy(Dx,Dy,Dz,Sgr,Cgr,xi1,xi2,eta,zeta,dar,azro,Kr,Refc,Beta,Fmach,lhs,Idzdy,dzyr,dzyi)
         lhs = 0
         GOTO jdzdy
!
      ENDDO
!
!     END OF LOOP FOR ELEMENT
!
!     200 IS END OF LOOP ON SLENDER BODY
!
   ENDDO
!
!     WRITE ROW ON TAPE, ROW NUMBER, NO. ELEMENTS, DATA
!
   CALL write(Ntape,D,2*t1,0)
   IF ( Iprnt/=0 ) WRITE (Npot,99002) Row , t1 , D
99002 FORMAT (' ROWDYZ - ROW NO.',I5,1H,,I10,' ELEMENTS',/(1X,6E12.4))
END SUBROUTINE rowdyz