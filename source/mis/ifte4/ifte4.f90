!*==ifte4.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifte4(Tha,Rp,Cp)
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
!  EVALUATE SERIES
!
            rn = 1.0
            d = 1.0
            sign = -1.
            rps = 1.
            tsq = Tha*Tha
            t1 = 5.
            t2 = 6.
            it = 1
         ELSE
            d = Tha**4/24.
            Rp = ((.5*(Tha*Tha))-1.+cos(Tha))/d
            Cp = ((Tha**3/6.)-Tha+sin(Tha))/d
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
            d = 5.0
            sign = -1.
            rps = Tha/5.
            t1 = 6.
            t2 = 7.
            it = 2
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifte4
