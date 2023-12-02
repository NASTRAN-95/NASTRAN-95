!*==hring.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hring(Points)
   IMPLICIT NONE
   USE C_SMA1ET
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Points
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , i1 , i2 , ir , is , it , j , jpoint , k , tint
   INTEGER , DIMENSION(15) , SAVE :: map
   REAL , SAVE :: pi23
   REAL :: t , temp
   EXTERNAL kqdmem , ktrmem , page2
!
! End of declarations rewritten by SPAG
!
!
!     HEAT CONDUCTIVITY SMA1 ROUITNE FOR TRIANGULAR (POINTS=3) AND
!     TRAPEZOIDAL (POINTS=4) RING ELEMENTS.
!     THIS ROUTINE IS SEPARATE FROM KTRAPR AND KTRIRG SO AS TO BE
!     IN OVERLAY WITH KTRMEM AND KQDMEM.
!
   !>>>>EQUIVALENCE (t,tint)
   DATA pi23/2.0943951024E0/
   DATA map/1 , 2 , 3 , 1 , 2 , 3 , 2 , 3 , 4 , 3 , 4 , 1 , 4 , 1 , 2/
!
!     ECPT LISTS
!
!     ECPT     TRIRG -------- TRMEM          TRAPRG ------- QDMEM
!     ===========================================================
!      1       EL-ID          EL-ID          EL-ID          EL-ID
!      2       SIL-1          SIL-1          SIL-1          SIL-1
!      3       SIL-2          SIL-2          SIL-2          SIL-2
!      4       SIL-3          SIL-3          SIL-3          SIL-3
!      5       THETA          THETA          SIL-4          SIL-4
!      6       MATID          MATID          THETA          THETA
!      7       CSID-1         T              MATID          MATID
!      8       X1             NS-MASS        CSID-1         T
!      9       Y1             CSID-1         X1             NS-MASS
!     10       Z1             X1             Y1             CSID-1
!     11       CSID-2         Y1             Z1             X1
!     12       X2             Z1             CSID-2         Y1
!     13       Y2             CSID-2         X2             Z1
!     14       Z2             X2             Y2             CSID-2
!     15       CSID-3         Y2             Z2             X2
!     16       X3             Z2             CSID-3         Y2
!     17       Y3             CSID-3         X3             Z2
!     18       Z3             X3             Y3             CSID-3
!     19       AVG-TEMP       Y3             Z3             X3
!     20                      Z3             CSID-4         Y3
!     21                      AVG-TEMP       X4             Z3
!     22                                     Y4             CSID-4
!     23                                     Z4             X4
!     24                                     AVG-TEMP       Y4
!     25                                                    Z4
!     26                                                    AVG-TEMP
!
!     GEOMETRY CHECKS  X  MUST BE .GT.0, AND Y  = 0  FOR I = 1,2,..,PTS.
!                       I                     I
!
   i1 = Points + 4
   i2 = i1 + 4*Points - 1
   DO i = i1 , i2 , 4
      IF ( Ecpt(i+1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( Ecpt(i+2)/=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
   ENDDO
!
!     POINT ORDERING CHECK.
!
   IF ( Points==4 ) THEN
      i1 = 4
      i2 = 15
   ELSE
      i1 = 1
      i2 = 3
   ENDIF
   jpoint = Points + 1
   DO i = i1 , i2 , 3
      ir = map(i)*4 + jpoint
      is = map(i+1)*4 + jpoint
      it = map(i+2)*4 + jpoint
      temp = (Ecpt(is)-Ecpt(ir))*(Ecpt(it+2)-Ecpt(is+2)) - (Ecpt(it)-Ecpt(is))*(Ecpt(is+2)-Ecpt(ir+2))
      IF ( temp<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
   ENDDO
!
!     TRAPEZOID TEST.
!
   IF ( Points/=4 ) THEN
      t = pi23*(Ecpt(8)+Ecpt(12)+Ecpt(16))
      CALL spag_block_2
      RETURN
   ELSE
      IF ( Ecpt(11)==Ecpt(15) ) THEN
         IF ( Ecpt(19)==Ecpt(23) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
      CALL page2(-2)
      WRITE (Outpt,99001) Swm , Ecpt(1)
99001 FORMAT (A27,' 2158, A TRAPRG ELEMENT =',I14,' DOES NOT HAVE SIDE 1-2 PARALLEL TO SIDE 3-4.')
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     THICKNESS OF TRMEM OR QDMEM TO BE CALLED BELOW.
!     QDMEM WILL SUBDIVIDE THICKNESS FOR SUB-TRIANGLES AND THUS
!     T IS SET = INTEGER 1 AS A FLAG TO QDMEM ROUTINE WHICH WILL
!     COMPUTE T FOR EACH.
!
      tint = 1
      tint = tint
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     CONVERT ECPT TO THAT OF A TRMEM OR QDMEM.
!
      j = 5*Points + 6
      k = 4*Points + 1
      DO i = 1 , k
         Ecpt(j) = Ecpt(j-2)
         j = j - 1
      ENDDO
      Ecpt(Points+4) = t
      Ecpt(Points+5) = 0.0
      IF ( Points==4 ) THEN
!
         CALL kqdmem
         RETURN
      ELSE
         CALL ktrmem(0)
         RETURN
      ENDIF
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!
!     BAD GEOMETRY FATAL ERROR.
!
      WRITE (Outpt,99002) Ufm , Ecpt(1)
99002 FORMAT (A23,' 2159, TRIRG OR TRAPRG ELEMENT =',I14,' POSSESSES ILLEGAL GEOMETRY.')
      Nogo = .TRUE.
   END SUBROUTINE spag_block_3
END SUBROUTINE hring
