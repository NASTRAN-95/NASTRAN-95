!*==opt2b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE opt2b(Ipr,Pr,Pl,Rr)
   USE c_blank
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ipr
   REAL , DIMENSION(1) :: Pr
   REAL , DIMENSION(1) :: Pl
   REAL , DIMENSION(1) :: Rr
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alph , ch , delp , eps , gama , pnew
   INTEGER :: i , icp , iprnt , irr , kpl , max , nmes , np
   INTEGER , DIMENSION(1) :: iy
   REAL , DIMENSION(1) :: y
   REAL , DIMENSION(8) :: z
   EXTERNAL page2
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Core(1),Z(1),Max) , (Eps,Z(2)) , (Gama,Z(3)) , (Iprnt,Z(7)) , (Iy(1),Y(1),Z(8))
!     EQUIVALENT ARE  (IPR,PR)
!
   nmes = 0
   ch = 1.0
!
   DO np = 1 , nprw , nwdsp
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            alph = Pr(np+4)
            i = 1
            icp = ntotl - 4
            SPAG_Loop_2_1: DO
               icp = icp + 4
               IF ( iy(icp)<=0 ) EXIT SPAG_Loop_2_1
               IF ( iy(icp)==np ) THEN
!
!     SPECIAL HANDLING OF TRIM6
!
                  alph = y(icp+i)
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
            spag_nextblock_1 = 2
         CASE (2)
!
            IF ( alph<0 ) THEN
!
!     NO CHANGE IN ALPH (-1.0 DETECTED)
!
               alph = -1.0E0
               IF ( np==iy(icp) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( alph==0 ) THEN
!
!     ZERO STRESS INPUT, CHANGE ALPH TO 0.0001
!
               IF ( iprnt/=0 .AND. nmes<100 ) THEN
                  nmes = nmes + 1
                  CALL page2(-2)
                  WRITE (outtap,99001) uwm , Ipr(np)
99001             FORMAT (A25,' 2303, FULLY-STRESSED DESIGN DETECTED ZERO STRESS ','FOR PROPERTY',I9,/5X,                           &
                         &'CHECK PROPERTY CARD OR UNLOADED ','ELEMENT(S)')
               ENDIF
               alph = 1.0E-4
            ENDIF
!
!     POSITIVE ALPHA, CALCULATE PNEW
!
            irr = (np+nwdsp)/nwdsp
            IF ( abs(gama-1.0)<1.0E-4 ) ch = 0.25*Rr(irr) + 0.75
            pnew = Pr(np+3)*((alph/(alph+(1.0-alph)*gama))**ch)
            IF ( Ipr(np+5)/=0 ) THEN
!
!     COMPARE TO LIMIT DATA
!
               kpl = Ipr(np+5)
               delp = pnew/Pr(np+2)
               IF ( delp>=Pl(kpl) ) THEN
                  kpl = kpl + 1
                  IF ( delp<=Pl(kpl) .OR. Pl(kpl)==0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!     RECALCULATE ALPHA, PNEW  BASED ON THE LIMIT
!
               pnew = Pr(np+2)*Pl(kpl)
               alph = -pnew*gama/(pnew*(1.0-gama)-Pr(np+3))
            ENDIF
            spag_nextblock_1 = 3
         CASE (3)
!
            Pr(np+4) = alph
            IF ( np==iy(icp) ) y(icp+i) = alph
            spag_nextblock_1 = 4
         CASE (4)
!
            IF ( np==iy(icp) ) THEN
               i = i + 1
               IF ( i<=3 ) THEN
                  alph = y(icp+i)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  icp = icp + 4
               ENDIF
            ENDIF
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
!
   ENDDO
!
END SUBROUTINE opt2b
