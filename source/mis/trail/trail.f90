!*==trail.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trail
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_XFIAT
   USE C_XFIST
   USE C_XMSSG
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: db , i , index , iw , mask , n
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: modnam , return , store
   EXTERNAL andf , lshift , mesage , orf , rdtrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     MODULE TO INTERROGATE OR ALTER ANY VALUE OF A 6 WORD MATRIX
!     OR TABLE TRAILER
!
!     DMAP CALL
!
!        TRAILER  DB / /*OPT*/WORD/S,N,VALUE $
!
!     INPUT DATA BLOCKS
!
!        DB - DATA BLOCK FOR WHICH TRAILER IS TO BE ALTERED OR READ
!
!     PARAMETERS
!
!        OPT   - BCD,INPUT.
!                RETURN - VALUE OF SPECIFIED TRAILER WORD IS TO
!                         BE RETURNED
!                STORE  - VALUE OF SPECIFIED TRAILER WORD IS TO
!                         CHANGED
!        WORD  - INTEGER,INPUT. DESIRED WORD OF TRAILER
!        VALUE - INTEGER,INPUT OR OUTPUT. LOCATION WHERE VALUED WILL
!                RETURNED OR FROM WHICH REPLACEMENT VALUE WILL BE
!                TAKEN.  RETURNED NEGATIVE IF DB IS PURGED.
!
!     FOR MATRIX DATA BLOCKS, THE TRAILER POSITIONS ARE AS FOLLOWS
!
!        WORD 1 - NUMBER OF COLUMNS
!        WORD 2 - MUNBER OF ROWS
!        WORD 3 - MATRIX FORM
!        WORD 4 - TYPE OF ELEMENTS
!        WORD 5 - MAXIMUM NUMBER OF NON-ZERO WORDS IN ANY ONE COLUMN
!        WORD 6 - MATRIX DENSITY * 100
!
   DATA store/4HSTOR , 4HE   /
   DATA return/4HRETU , 4HRN  /
   DATA modnam/4HTRAI , 4HLER /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     GET TRAILER
!
         db = 101
         mcb(1) = db
         CALL rdtrl(mcb)
         IF ( mcb(1)>0 ) THEN
!
!     TEST ILLEGAL PARAMETER VALUES AND BRANCH ON OPT
!
            IF ( Word<1 .OR. Word>6 ) THEN
!
!     ERROR CONDITIONS
!
               WRITE (Nout,99001) Ufm , Word
99001          FORMAT (A23,' 2202.  PARAMETER, WORD, HAS ILLEGAL VALUE OF',I9)
!
               CALL mesage(-37,0,modnam)
               RETURN
            ELSEIF ( Opt(1)==return(1) .AND. Opt(2)==return(2) ) THEN
!
!     RETURN OPTION
!
               Value = mcb(Word+1)
               RETURN
            ELSE
               IF ( Opt(1)==store(1) .AND. Opt(2)==store(2) ) THEN
!
!     STORE OPTION
!
!     SEARCH FIST FOR THE FILE
!
                  n = Fist(2)*2 + 1
                  DO i = 3 , n , 2
                     IF ( Fist(i)==db ) THEN
                        index = Fist(i+1) + 1
                        GOTO 5
                     ENDIF
                  ENDDO
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
                  WRITE (Nout,99002) Ufm , Opt
99002             FORMAT (A23,' 2202.  PARAMETER, OPT, HAS ILLEGAL VALUE OF ',2A4)
                  CALL mesage(-37,0,modnam)
                  RETURN
               ENDIF
!
!     PACK THE TRAILER INFORMATION INTO THE REQUESTED WORD.
!     MAKE SURE THE NUMBER IS POSITIVE AND .LE. 16 BITS IF ICFIAT=8
!
 5             IF ( Value<0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Icfiat==11 ) THEN
!
!     ICFIAT = 11, TRAILER WORDS ARE NOT PACKED
!
                  iw = 2
                  IF ( Word>=4 ) iw = 4
                  Fiat(index+iw+Word) = Value
                  RETURN
               ELSE
                  IF ( Value>65535 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  iw = (Word+1)/2 + 2
                  IF ( Word==(Word/2*2) ) THEN
!
!     WORD IS EVEN
!
                     mask = lshift(65535,16)
                     Fiat(index+iw) = orf(andf(Fiat(index+iw),mask),Value)
                     RETURN
                  ELSE
!
!     WORD IS ODD
!
                     mask = 65535
                     Fiat(index+iw) = orf(andf(Fiat(index+iw),mask),lshift(Value,16))
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     PURGED DATA BLOCK
!
         Value = -1
         RETURN
      CASE (3)
!
         WRITE (6,99003) Ufm , Value
99003    FORMAT (A23,' 2202.  PARAMETER, VALUE, HAS ILLEGAL VALUE OF',I9)
         CALL mesage(-37,0,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trail
