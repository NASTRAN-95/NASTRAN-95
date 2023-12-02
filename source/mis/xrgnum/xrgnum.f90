!*==xrgnum.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xrgnum
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_XMSSG
   USE C_XRGDXX
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank
   INTEGER :: i , ifrcol , j , k , newnum
   INTEGER , DIMENSION(10) , SAVE :: nums
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     XRGNUM PROCESSES THE NUMBER ON A CARD OR FILE NAME TABLE ENTRY
!     THIS ROUTINE IS CALLED ONLY BY XRGDTB
!
!     WRITTEN BY  RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!       /SYSTEM/
!         OPTAPE       UNIT NUMBER FOR THE OUTPUT PRINT FILE
!       /XRGDXX/
!         ICHAR        CONTAINS THE CARD IMAGE IN 80A1 FORMAT
!         ICOL         CURRENT COLUMN BEING PROCESSED
!         RECORD       CONTAINS THE CARD IMAGE IN 20A4 FORMAT
!
!     OUTPUT
!       /XRGDXX/
!         ICOL         CURRENT COLUMN BEING PROCESSED
!         NUMBER       VALUE OF THE NUMBER IN INTEGER FORMAT
!
!     LOCAL VARIABLES
!         BLANK          CONTAINS THE VALUE 1H
!         IFRCOL         FIRST COLUMN TO BE EXAMINED BY XRGNUM
!         NEWNUM         INTEGER VALUE OF THE CHARACTER IN THE CURRENT
!                        COLUMN
!         NUMS           CONTAINS THE ALPHA VALUES 1,2,...0
!
!     THE CARD IS SCANED TO FIND THE VALUE OF THE NUMBER IN THE FIRST
!     FIELD OF THE CARD
!
!     MESSAGE 8030 MAY BE ISSUED
!
   DATA nums/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 1H0/
   DATA blank/1H /
!
   ifrcol = Icol
   Number = 0
   SPAG_Loop_1_1: DO WHILE ( Icol<80 )
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            IF ( Ichar(Icol)==blank ) THEN
               Icol = Icol + 1
               IF ( Number/=0 ) EXIT SPAG_Loop_1_1
               CYCLE
            ELSE
               DO k = 1 , 10
                  IF ( Ichar(Icol)==nums(k) ) THEN
                     newnum = mod(k,10)
                     Number = Number*10 + newnum
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               Number = 0
               j = 0
               k = 1
               WRITE (Optape,99001) Ufm , ifrcol , Record , j , (i,i=1,8) , k , (j,i=1,8)
99001          FORMAT (A23,' 8030, EXPECTED AN INTEGER NEAR COLUMN',I3,' IN THE FOLLOWING CARD',//20X,20A4,/,(20X,I1,I9,7I10))
               EXIT SPAG_Loop_1_1
            ENDIF
         CASE (2)
            Icol = Icol + 1
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO SPAG_Loop_1_1
END SUBROUTINE xrgnum
