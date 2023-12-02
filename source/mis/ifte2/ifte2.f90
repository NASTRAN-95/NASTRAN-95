!*==ifte2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifte2(Tha,Rp,Cp)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Tha
   REAL :: Rp
   REAL :: Cp
!
! Local variable declarations rewritten by SPAG
!
   REAL :: d , rn , rps , sign , t1 , t2 , trm , tsq
   REAL , SAVE :: epsi , thao
   INTEGER :: i , it
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA thao , epsi/.1 , 1.E-9/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         IF ( abs(Tha)<thao ) THEN
!
!     EVALUATE SERIES
!
            rn = 1.0
            d = 1.0
            sign = -1.
            rps = 1.0
            tsq = Tha*Tha
            t1 = 3.
            t2 = 4.
            it = 1
         ELSE
            d = .5*Tha*Tha
            Rp = (1.-cos(Tha))/d
            Cp = (Tha-sin(Tha))/d
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO i = 1 , 50
            rn = rn*tsq
            d = d*t1*t2
            trm = rn/d*sign
            rps = rps + trm
            IF ( abs(trm)<epsi ) EXIT SPAG_Loop_1_1
            sign = -sign
            t1 = t1 + 2.
            t2 = t2 + 2.
         ENDDO SPAG_Loop_1_1
         IF ( it==2 ) THEN
            Cp = rps
         ELSE
            Rp = rps
            rn = Tha
            d = 3.0
            sign = -1.
            rps = Tha/3.
            t1 = 4.
            t2 = 5.
            it = 2
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifte2
