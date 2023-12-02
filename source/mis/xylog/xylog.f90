!*==xylog.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xylog(V1,V2,Cycles)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: V1
   REAL :: V2
   INTEGER :: Cycles
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: power1 , power2
   REAL :: temp
!
! End of declarations rewritten by SPAG
!
!*****
!  THIS SUBROUTINE TAKES V1 AND V2 REGARDLESS OF THEIR VALUES
!  AND COMPUTES A LOG SCALE OF AT LEAST 1 CYCLE...
!*****
   IF ( V1>0.0E0 ) THEN
!
      IF ( V2<=0.0E0 ) THEN
!
!     V1 IS POSITIVE BUT V2 IS NEGATIVE OR 0
!
         V2 = V1*1.0E+5
!
      ELSEIF ( V2<=V1 ) THEN
         temp = V1
         V1 = V2
         V2 = temp
      ENDIF
!
!     RAISE V2 TO POWER OF 10,  LOWER V1 TO POWER OF 10
!
      power1 = 0
      CALL spag_block_2
      RETURN
   ELSEIF ( V2>0.0E0 ) THEN
!
!     V2 IS POSITIVE BUT V1 IS NEGATIVE OR 0
!
      V1 = V2*1.0E-5
      power1 = 0
      CALL spag_block_2
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     V1 AND V2 ARE BOTH NEGATIVE OR ZERO.  SET ARBITRARY LIMITS
!
      V1 = 1.0E-5
      V2 = 1.0E+5
      Cycles = 10
      RETURN
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!WKBR 9/93  50 IF( V1 .LT. 1.0E0      ) GO TO 70
      DO WHILE ( V1<0.99999 )
         V1 = V1*10.0E0
         IF ( V1<=0.0E0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         power1 = power1 - 1
      ENDDO
!WKBR 9/93  60 IF( V1 .LT. 10.0E0) GO TO 80
      DO WHILE ( V1>10.0001 )
         V1 = V1/10.0E0
         power1 = power1 + 1
      ENDDO
!
      V1 = 10.0E0**power1
!
      power2 = 1
      DO WHILE ( V2<=1.0E0 )
         V2 = V2*10.0E0
         IF ( V2<=0.0E0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         power2 = power2 - 1
      ENDDO
!WKBR 9/93 100 IF( V2 .LT. 10.00001E0) GO TO 120
      DO WHILE ( V2>10.0001 )
         V2 = V2/10.0E0
         power2 = power2 + 1
      ENDDO
!
      V2 = 10.0**power2
!
      Cycles = power2 - power1
   END SUBROUTINE spag_block_2
END SUBROUTINE xylog
