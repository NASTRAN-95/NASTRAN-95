!*==rowdyz.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rowdyz(Nfb,Nlb,Row,Ntzys,D,Dx,Dy,Dz,Beta,Idzdy,Ntape,Sgr,Cgr,Iprnt,Yb,Zb,Ar,Nsbe,Xis1,Xis2,A0)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntzys
   INTEGER :: Nfb
   INTEGER :: Nlb
   INTEGER :: Row
   REAL , DIMENSION(2,Ntzys) :: D
   REAL :: Dx
   REAL :: Dy
   REAL :: Dz
   REAL :: Beta
   INTEGER :: Idzdy
   INTEGER :: Ntape
   REAL :: Sgr
   REAL :: Cgr
   INTEGER :: Iprnt
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Zb
   REAL , DIMENSION(1) :: Ar
   INTEGER , DIMENSION(1) :: Nsbe
   REAL , DIMENSION(1) :: Xis1
   REAL , DIMENSION(1) :: Xis2
   REAL , DIMENSION(1) :: A0
!
! Local variable declarations rewritten by SPAG
!
   REAL :: azro , dar , delta , dzyi , dzyr , epslon , eta , xi1 , xi2 , zeta
   INTEGER :: b , b1 , it1 , jdzdy , lhs , nsbeb , t , t1
   EXTERNAL dzy , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
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
               IF ( Dy==eta .AND. Dz==zeta ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ASSIGN 5 TO jdzdy
               lhs = 0
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
 5             D(1,t1) = dzyr
               D(2,t1) = dzyi
               spag_nextblock_1 = 2
            CASE (2)
!
!     SKIP IF NO SYMMETRY
!
               IF ( delta==0.0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               eta = -Yb(b)
!
!     CHECK TO SEE IF CALCULATIONS ARE TO BE MADE
!
               IF ( Dy==eta .AND. Dz==zeta ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               lhs = 1
               ASSIGN 10 TO jdzdy
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
 10            D(1,t1) = D(1,t1) + delta*dzyr
               D(2,t1) = D(2,t1) + delta*dzyi
               spag_nextblock_1 = 3
            CASE (3)
               IF ( epslon==0.0 ) CYCLE
!
!     CALC. ONLY IF DELTA AND EPSLON  NOT EQUAL ZERO
!
               eta = -Yb(b)
               zeta = -Zb(b)
!
!     CHECK TO SEE IF CALCULATIONS ARE TO BE MADE
!
               IF ( Dy==eta .AND. Dz==zeta ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ASSIGN 15 TO jdzdy
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
 15            D(1,t1) = D(1,t1) + epslon*delta*dzyr
               D(2,t1) = D(2,t1) + epslon*delta*dzyi
               spag_nextblock_1 = 4
            CASE (4)
!
!     SKIP IF NO GROUND EFFECTS
!
               IF ( epslon==0.0 ) CYCLE
               eta = Yb(b)
               zeta = -Zb(b)
!
!     CHECK TO SEE IF CALCULATIONS ARE TO BE MADE
!
               IF ( Dy==eta .AND. Dz==zeta ) CYCLE
               lhs = 1
               ASSIGN 20 TO jdzdy
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
 20            D(1,t1) = D(1,t1) + epslon*dzyr
               D(2,t1) = D(2,t1) + epslon*dzyi
               CYCLE
            CASE (5)
!
!     CALL SEQUENCE TO DZY
!
               CALL dzy(Dx,Dy,Dz,Sgr,Cgr,xi1,xi2,eta,zeta,dar,azro,Kr,Refc,Beta,Fmach,lhs,Idzdy,dzyr,dzyi)
               lhs = 0
               GOTO jdzdy
            END SELECT
         ENDDO SPAG_DispatchLoop_1
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
