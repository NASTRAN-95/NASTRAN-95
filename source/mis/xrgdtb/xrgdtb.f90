!*==xrgdtb.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xrgdtb(Lu)
   USE c_system
   USE c_xmssg
   USE c_xrgdxx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lu
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: astrsk , blank , coment , dollar
   INTEGER :: icolum , nbpc , nbpw , ncpw , optape
   EXTERNAL write , xdcode , xecode , xrgnum
!
! End of declarations rewritten by SPAG
!
!
!     XRGDTB PROCESSES THE CARD AND FILE NAME RESTART TABLES
!     THIS SUBROUTINE IS CALLED ONLY BY XRGDFM
!
!     WRITTEN BY  RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!       LU            FORTRAN UNIT NUMBER FOR THE RIGID FORMAT FILE
!     /SYSTEM/
!       OPTAPE        OUTPUT UNIT NUMBER FOR THE PRINT FILE
!     /XRGDXX/
!       ICHAR         ARRAY IN 80A1 FORMAT CONTAINING CARD IMAGE
!       ISCR          FILE NUMBER ON WHICH TABLES ARE WRITTEN
!       ITYPE         TYPE OF TABLE BEING PROCESSED-('CARD'OR'FILE')
!       LIMIT         LOWER/UPPER LIMITS FOR VALUES IN THE TABLE
!       RECORD        CARD IMAGE IN 20A4 FORMAT
!
!     OUTPUT
!     /XRGDXX/
!       ICOL          COLUMN WITHIN CARD BEING PROCESSED
!       ICOUNT        NUMBER OF ALPHA CHARACTERS WITHIN A NAME
!       IERROR        ERROR FLAG - NON-ZERO IF ERROR OCCURRED
!       NAME          NAME OF THE SUBROUTINE
!       NUMBER        VALUE OF NUMBER RETURNED BY XRGNUM
!
!     LOCAL VARIABLES
!       ASTRSK          CONTAINS THE VALUE 1H*
!       BLANK           CONTAINS THE VALUE 1H
!       COMENT          CONTAINS THE VALUE OF 4H$$$$
!       DOLLAR          CONTAINS THE VALUE OF 1H$
!       ICOLUM          COLUMN NUMBER OF THE NEXT CHARACTER WITHIN
!                       A NAME
!
!     FUNCTIONS
!       1. CALLS READ AND XDCODE FOR EACH CARD WITHIN THE TABLE.
!       2. CALLS XRGNUM TO PROCESS ALL NUMBERS
!       3. CALLS XECODE TO PROCESS ALL NAMES
!       4. ALL ENTRIES READ ARE EXPECTED TO BE IN THE FOLLOWING
!          FORMAT:    NNNN    NAME  NAME  NAME  NAME  NAME ...
!          WHERE NNNN IS ANY NUMBER.
!
!     SUBROUTINES CALLED - XRGNUM,XECODE,READ,WRITE
!
!     ERRORS  MESSAGES 8028,8034,8029,8036 MAY BE ISSUED
!
   !>>>>EQUIVALENCE (Ksystm(2),Optape) , (Ksystm(39),Nbpc) , (Ksystm(40),Nbpw) , (Ksystm(41),Ncpw)
   DATA blank/1H / , dollar/1H$/ , astrsk/1H*/
   DATA coment/4H$$$$/
   DO
!
      number = 0
      name(1) = 0
      READ (Lu,99001,ERR=100,END=100) record
99001 FORMAT (20A4)
      CALL xdcode
      IF ( record(1)/=coment ) THEN
         IF ( ichar(1)==dollar .AND. ichar(2)==astrsk ) THEN
            CALL write(iscr,0,0,1)
            RETURN
         ELSE
            icol = 1
            SPAG_Loop_2_1: DO
               IF ( ichar(icol)==blank .OR. icol>80 ) THEN
                  IF ( icol>=80 ) THEN
                     IF ( number==0 .OR. name(1)==0 ) THEN
                        WRITE (optape,99002) ufm , record
99002                   FORMAT (A23,' 8036, MISSING FIELDS ON THE FOLLOWING CARD',/20X,20A4)
                        ierror = 1
                     ENDIF
                     EXIT SPAG_Loop_2_1
                  ELSE
                     icol = icol + 1
                  ENDIF
               ELSEIF ( number/=0 ) THEN
                  icount = 1
                  SPAG_Loop_3_2: DO
                     icolum = icol + icount
                     IF ( ichar(icolum)/=blank .AND. icolum<=80 ) THEN
                        icount = icount + 1
                        IF ( icount>8 ) THEN
                           WRITE (optape,99003) ufm , record
99003                      FORMAT (A23,' 8029, THE FOLLOWING CARD CONTAINS NAMES THAT ARE','COMPRISED OF MORE THAN 8 CHARACTERS',   &
                                 & //20X,20A4)
                           ierror = 1
                           EXIT SPAG_Loop_2_1
                        ENDIF
                     ELSEIF ( icount/=0 ) THEN
                        CALL xecode
                        CALL write(iscr,name,2,0)
                        CALL write(iscr,number,1,0)
                        icol = icol + icount
                        EXIT SPAG_Loop_3_2
                     ENDIF
                  ENDDO SPAG_Loop_3_2
               ELSE
                  CALL xrgnum
                  IF ( number==0 ) THEN
                     WRITE (optape,99004) ufm , record
99004                FORMAT (A23,' 8028, EXPECTED TO FIND AN INTEGER IN THE FIRST ','FIELD OF THE FOLLOWING CARD',//20X,20A4)
                     ierror = 1
                     EXIT SPAG_Loop_2_1
                  ELSEIF ( number<limit(1) .OR. number>limit(2) ) THEN
                     WRITE (optape,99005) ufm , number , record , limit , itype
99005                FORMAT (A23,' 8029, THE VALUE',I4,' GIVEN IN THE FIRST FIELD OF',' THE FOLLOWING CARD',//20X,20A4,//5X,        &
                            &'IS OUTSIDE THE ','RANGE OF',I5,1H-,I4,6H FOR ',A4,8H' CARDS.)
                     ierror = 1
                     EXIT SPAG_Loop_2_1
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_2_1
         ENDIF
      ENDIF
   ENDDO
!
!     ERRORS
!
 100  WRITE (optape,99006) ufm , member
99006 FORMAT (A23,' 8027, UNEXPECTED EOF ENCOUNTERED ON FILE ',2A4,' IN SUBROUTINE XRGDTB.')
   ierror = 1
   CALL write(iscr,0,0,1)
END SUBROUTINE xrgdtb
