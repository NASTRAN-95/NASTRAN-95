!*==xipfl.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xipfl
   USE c_passer
   USE c_xgpi2
   USE c_xgpi4
   USE c_xgpic
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iofl , ityp , j , k , k1 , k2 , k3 , l , ospnt
   INTEGER , DIMENSION(1) :: oscar
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
!
!     THE PURPOSE OF XIOFL IS TO GENERATE THE INPUT AND OUTPUT FILE
!     SECTIONS FOR AN OSCAR ENTRY.
!
!                  ** CONTROL CARD NAMES **
!                  ** DMAP CARD NAMES **
   !>>>>EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Os(5),Oscar(1))
!
!
!     SET INPUT FILE FLAG
!
         iofl = 1
         k3 = 0
         istopf = 0
         j = mpl(mplpnt)
         mplpnt = mplpnt + 1
         IF ( j/=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NO INPUT FILES - MAKE ONE NULL ENTRY IN OSCAR
!
         oscar(ospnt+6) = 1
         oscar(ospnt+7) = 0
         oscar(ospnt+8) = 0
         oscar(ospnt+9) = 0
         oscar(ospnt) = oscar(ospnt) + 4
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY xopfl
!     ===========
!
!     SET O/P FLAG
!
         iofl = 0
         k3 = 0
         istopf = 0
         j = mpl(mplpnt)
         mplpnt = mplpnt + 1
         IF ( j==0 ) THEN
!
!     THERE ARE NO O/P FILES - CHANGE OSCAR ENTRY TYPE CODE TO O FORMAT
!
            oscar(ospnt+2) = orf(2,andf(masklo,oscar(ospnt+2)))
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!
!     SCAN INPUT OR OUTPUT SECTION
!
         i = ospnt + oscar(ospnt)
         istopf = i
         oscar(i) = j
         oscar(ospnt) = 1 + oscar(ospnt)
         i = i + 1
         j = i + 3*(j-1)
         oscar(ospnt) = j + 3 - ospnt
!
!     ZERO I/O SECTION
!
         l = j + 2
         DO k = i , l
            oscar(k) = 0
         ENDDO
!
!     ENTER FILE NAME IN OSCAR FROM DMAP
!
         DO k = i , j , 3
            CALL xscndm
            IF ( irturn==1 .OR. irturn==4 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( irturn==3 .OR. irturn==5 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     OK IF NAME RETURNED FROM XSCNDM
!
            IF ( dmap(dmppnt)/=nblank ) THEN
!
!     ENTER NAME IN OSCAR AND INITIALIZE ORDNAL
!
               oscar(k) = dmap(dmppnt)
               oscar(k+1) = dmap(dmppnt+1)
               oscar(k+2) = 0
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         CALL xscndm
         IF ( irturn==2 .OR. irturn==3 .OR. irturn==5 ) THEN
         ELSEIF ( irturn==4 ) THEN
            irturn = 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( dmap(dmppnt+1)==islsh ) THEN
            irturn = 1
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     NORMAL EXIT IF DMAP OPERATOR IS /
!
!     ERROR EXIT
!     BLANK ITEM IN O/P SECTION OF TYPE O FORMAT IS OKAY
!
         IF ( j==0 .AND. iofl==0 .AND. dmap(dmppnt)==nblank ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k1 = 1 + (k-i)/3
         k2 = 1 + (j-i)/3
         IF ( k1<=k2 ) THEN
            irturn = 2
         ELSE
            IF ( k3/=1 ) THEN
               IF ( iofl==1 ) CALL xgpidg(62,ospnt,0,0)
               IF ( iofl==0 ) CALL xgpidg(63,ospnt,0,0)
               k3 = 1
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         RETURN
      CASE (6)
!
!
!     DELIMITER OR END OF INSTRUCTION ENCOUNTERED BEFORE ANTICIPATED -
!     CHECK FOR ILLEGAL INPUT FORMAT
!
         IF ( iofl/=1 .OR. dmap(dmppnt+1)/=islsh ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( icomon==0 ) THEN
            irturn = 2
         ELSE
            ityp = andf(oscar(ospnt+2),7)
            IF ( ityp/=2 ) THEN
!
!     FIRST INPUT FILE WAS NULL - SHIFT I/P SECTION BY ONE ENTRY AND
!     ZERO FIRST ENTRY
!     ISSUE WARNING MESSAGE
!
               CALL xgpidg(-1,ospnt,0,0)
               IF ( i/=j ) THEN
                  i = i + 3
                  j = j + 2
                  DO k = i , j
                     l = j - k + i
                     oscar(l) = oscar(l-3)
                  ENDDO
                  oscar(i-3) = 0
                  oscar(i-2) = 0
                  oscar(i-1) = 0
               ENDIF
            ENDIF
            irturn = 1
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xipfl
