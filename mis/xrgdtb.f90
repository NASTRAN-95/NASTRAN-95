
SUBROUTINE xrgdtb(Lu)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ichar(80) , Icol , Icount , Idmap , Ierror , Ignore , Ind , Iphase , Irestr , Iscr , Istate , Itype , Ksystm(100) ,      &
         & Limit(2) , Member(2) , Name(2) , Nbpc , Nbpw , Ncpw , Nsubst , Num(2) , Number , Nument , Optape , Record(20)
   CHARACTER*23 Ufm
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
   COMMON /xrgdxx/ Irestr , Nsubst , Iphase , Icol , Number , Itype , Istate , Ierror , Num , Ind , Nument , Record , Ichar ,       &
                 & Limit , Icount , Idmap , Iscr , Name , Member , Ignore
!
! Dummy argument declarations
!
   INTEGER Lu
!
! Local variable declarations
!
   INTEGER astrsk , blank , coment , dollar , icolum
!
! End of declarations
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
   EQUIVALENCE (Ksystm(2),Optape) , (Ksystm(39),Nbpc) , (Ksystm(40),Nbpw) , (Ksystm(41),Ncpw)
   DATA blank/1H / , dollar/1H$/ , astrsk/1H*/
   DATA coment/4H$$$$/
   DO
!
      Number = 0
      Name(1) = 0
      READ (Lu,99001,ERR=200,END=200) Record
99001 FORMAT (20A4)
      CALL xdcode
      IF ( Record(1)/=coment ) THEN
         IF ( Ichar(1)==dollar .AND. Ichar(2)==astrsk ) THEN
            CALL write(Iscr,0,0,1)
            GOTO 99999
         ELSE
            Icol = 1
            DO
               IF ( Ichar(Icol)==blank .OR. Icol>80 ) THEN
                  IF ( Icol>=80 ) THEN
                     IF ( Number==0 .OR. Name(1)==0 ) THEN
                        WRITE (Optape,99002) Ufm , Record
99002                   FORMAT (A23,' 8036, MISSING FIELDS ON THE FOLLOWING CARD',/20X,20A4)
                        Ierror = 1
                     ENDIF
                     EXIT
                  ELSE
                     Icol = Icol + 1
                  ENDIF
               ELSEIF ( Number/=0 ) THEN
                  Icount = 1
                  DO
                     icolum = Icol + Icount
                     IF ( Ichar(icolum)/=blank .AND. icolum<=80 ) THEN
                        Icount = Icount + 1
                        IF ( Icount>8 ) THEN
                           WRITE (Optape,99003) Ufm , Record
99003                      FORMAT (A23,' 8029, THE FOLLOWING CARD CONTAINS NAMES THAT ARE','COMPRISED OF MORE THAN 8 CHARACTERS',   &
                                 & //20X,20A4)
                           Ierror = 1
                           GOTO 100
                        ENDIF
                     ELSEIF ( Icount/=0 ) THEN
                        CALL xecode
                        CALL write(Iscr,Name,2,0)
                        CALL write(Iscr,Number,1,0)
                        Icol = Icol + Icount
                        EXIT
                     ENDIF
                  ENDDO
               ELSE
                  CALL xrgnum
                  IF ( Number==0 ) THEN
                     WRITE (Optape,99004) Ufm , Record
99004                FORMAT (A23,' 8028, EXPECTED TO FIND AN INTEGER IN THE FIRST ','FIELD OF THE FOLLOWING CARD',//20X,20A4)
                     Ierror = 1
                     EXIT
                  ELSEIF ( Number<Limit(1) .OR. Number>Limit(2) ) THEN
                     WRITE (Optape,99005) Ufm , Number , Record , Limit , Itype
99005                FORMAT (A23,' 8029, THE VALUE',I4,' GIVEN IN THE FIRST FIELD OF',' THE FOLLOWING CARD',//20X,20A4,//5X,        &
                            &'IS OUTSIDE THE ','RANGE OF',I5,1H-,I4,6H FOR ',A4,8H' CARDS.)
                     Ierror = 1
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
 100  ENDDO
!
!     ERRORS
!
 200  WRITE (Optape,99006) Ufm , Member
99006 FORMAT (A23,' 8027, UNEXPECTED EOF ENCOUNTERED ON FILE ',2A4,' IN SUBROUTINE XRGDTB.')
   Ierror = 1
   CALL write(Iscr,0,0,1)
99999 RETURN
END SUBROUTINE xrgdtb
