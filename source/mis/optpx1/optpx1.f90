!*==optpx1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optpx1(Stor,Nogo,Nen,Loc1) !HIDESTARS (*,Stor,Nogo,Nen,Loc1)
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: Stor
   INTEGER :: Nogo
   INTEGER :: Nen
   INTEGER :: Loc1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i1 , i2 , i3 , l
   INTEGER , DIMENSION(1) :: iy
   INTEGER , DIMENSION(2) :: nam
   INTEGER , SAVE :: thru
   EXTERNAL bishel , mesage , page2 , sort
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PROCESS PID DATA ON PLIMIT CARD
!
   !>>>>EQUIVALENCE (Core(1),X(1)) , (X(7),Iy(1))
   DATA thru/4HTHRU/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nam(1) = Stor(1)
         nam(2) = Stor(2)
         IF ( Stor(6)==thru ) THEN
!
!     USER SPECIFIED BY USING THRU
!
            l = 8
            Stor(9) = Stor(8)
            Stor(8) = Stor(5)
         ELSE
!
!     USER SPECIFIED BY EXPLICIT ID-S
!
            CALL sort(0,0,1,1,Stor(5),5)
!
!     CREATE PSEUDO THRU RANGE
!     LOCATE FIRST NONZERO
!
            DO l = 5 , 9
               IF ( Stor(l)/=0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            CALL page2(-2)
            WRITE (Outtap,99001) Ufm , nam
99001       FORMAT (A23,' 2293, NO PID ENTRIES ON PLIMIT CARD (',2A4,2H).)
            Nogo = Nogo + 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     LOOP ON ENTRIES
!
         i1 = Stor(l)
         i3 = 1
         SPAG_Loop_1_1: DO
            i2 = Stor(l+1)
            IF ( l<9 ) THEN
               IF ( i2-i1<i3 ) GOTO 20
               IF ( i2-i1/=i3 ) EXIT SPAG_Loop_1_1
!
!     THRU CAN BE EXPANDED
!
               l = l + 1
               i3 = i3 + 1
            ELSEIF ( l==9 ) THEN
               EXIT SPAG_Loop_1_1
            ELSE
!
               CALL mesage(-7,0,nam)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     PUT OUT I1,I2
!
         Stor(1) = i1
         Stor(2) = Stor(l)
         IF ( Loc1+3+Nen>Ycor ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL bishel(*20,Stor,Nen,4,iy(Loc1))
         spag_nextblock_1 = 3
      CASE (3)
         l = l + 1
         IF ( l<=9 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     DUPLICATE ENTRIES FOUND
!
 20      CALL page2(-2)
         WRITE (Outtap,99002) Ufm , i1 , i2 , nam
99002    FORMAT (A23,' 2294, DUPLICATE',I8,' THRU',I8,' RANGE FOR ELEMENT',1X,2A4,' REJECTED PLIMIT. SCAN CONTINUED.')
         Nogo = Nogo + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     THIS PLIMIT FINISHED
!
         RETURN
      CASE (5)
!
!     INSUFFICIENT CORE
!
         Stor(1) = nam(1)
         Stor(2) = nam(2)
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optpx1
