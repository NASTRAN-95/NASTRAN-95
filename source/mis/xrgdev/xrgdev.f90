!*==xrgdev.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xrgdev
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_XMSSG
   USE C_XRGDXX
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , istr , j , k
   INTEGER , DIMENSION(5,7) , SAVE :: state
   EXTERNAL xrgdtp
!
! End of declarations rewritten by SPAG
!
!
!     PURPOSE - XRGDEV PROCESSES A FIELD FROM A ****CARD, ****FILE,
!               ****SBST, OR A ****RFMT CARD FROM THE RIGID FORMAT
!               DATA BASE
!
!     AUTHOR  - RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!      /SYSTEM/
!       NOUT    UNIT NUMBER FOR OUTPUT PRINT FILE
!      /XRGDXX/
!       ICOL    COLUMN CONTAINING THE FIRST CHARACTER OF THE FIELD
!       LIMIT   2 WORD ARRAY CONTAINING THE LOWER/UPPER LIMITS FOR
!               VALUES GIVEN IN THE FIELD
!       NUMBER  INTEGER VALUE FOR A ALPHA NUMBER WITHIN THE FIELD
!       RECORD  ARRAY IN 20A4 FORMAT CONTAINING THE CARD IMAGE
!
!     OUTPUT
!      /XRGDXX/
!       IERROR  ERROR FLAG IS NON-ZERO IF AN ERROR OCCURRED
!       NUM     2 WORD ARRAY CONTAINING THE VALUE(S) WITHIN THE CURRENT
!               FIELD
!
!     LOCAL VARIABLES
!       IND     INDEX TO THE ARRAY NUM
!       ISTATE  NEXT STATE (ROW = IN THE ABOVE DATA STATEMENT) TO BE
!               USED FOR SYNTAX VALIDATION BASED ON THE TYPE OF THE NEXT
!               CHARACTER IN THE FIELD
!       ISTR    COLUMN CONTAINING THE FIRST CHARACTER WITHIN THE FIEL
!       K       DO LOOP INDEX FOR SCANING CHARACTERS WITHIN THE FIELD
!       STATE   TABLE USED TO VALIDATE THE SYNTAX OF THE FIELD.  THE
!               NUMBER IN EACH ENTRY INDICATES THE ROW TO BE USED FOR
!               VALIDATING THE SYNTAX OF THE NEXT CHARACTER.  IF THE
!               VALUE IS 0 THEN A SYNTAX ERROR OCCURRED.
!
!     FUNCTIONS
!     XRGDEV SCANS THE FIELD FOR SYNTAX ERRORS AND FOR PLACING THE NUMBE
!     INTO THE NUM ARRAY.  VALID FIELDS ARE OF THE FORM 'NNN,' OR
!     'NNN-NNN,' WITH EMBEDDED BLANKS ALLOWED AND NUMBERS MAY BE OF
!     ANY VALUE THAT IS WITHIN THE LIMITS OF THE ARRAY LIMIT.
!
!     SUBROUTINES CALLED - XRGDTP
!
!     CALLING SUBROUTINES - XRGSUB,XRGDCF
!
!     ERRORS
!       ERROR MESSAGES 8021 AND 8022 ARE GIVEN FOR SYNTAX OR VALUE RANGE
!       ERRORS.
!
!                   NUMBER  ,      -    BLANK    OTHER
   DATA state/1 , 2 , 3 , 6 , 0 , 1 , 0 , 0 , 2 , 0 , 4 , 0 , 0 , 3 , 0 , 4 , 2 , 0 , 5 , 0 , 0 , 2 , 0 , 5 , 0 , 0 , 2 , 3 , 6 ,   &
      & 0 , 1 , 0 , 0 , 7 , 0/
!
   IF ( Icol<=80 ) THEN
      Istate = 7
      Ind = 1
      Num(1) = 0
      istr = Icol
      SPAG_Loop_1_1: DO k = istr , 80
         Icol = k
         CALL xrgdtp
         Istate = state(Itype,Istate)
         IF ( Istate/=0 ) THEN
            IF ( Istate==2 ) EXIT SPAG_Loop_1_1
            IF ( Istate==3 ) THEN
               Ind = 2
               Num(2) = 0
            ELSEIF ( Istate/=5 .AND. Istate/=6 .AND. Istate/=7 ) THEN
               Num(Ind) = Num(Ind)*10 + Number
            ENDIF
         ELSE
            Ierror = 1
            j = 0
            WRITE (Nout,99001) Ufm , k , Record , j , (i,i=1,8) , Ierror , (j,i=1,8)
99001       FORMAT (A23,' 8020, SYNTAX ERROR NEAR COLUMN ',I3,' IN THE FOLLOWING CARD- ',/20X,20A4,/,(20X,I1,I9,7I10))
            RETURN
         ENDIF
      ENDDO SPAG_Loop_1_1
      IF ( Ind/=2 ) THEN
         Num(2) = Num(1)
      ELSEIF ( Num(2)<=Num(1) ) THEN
         Ierror = 1
         WRITE (Nout,99002) Ufm , Num(1) , Num(2) , Record
99002    FORMAT (A23,' 8021, NON-INCREASING RANGE ',I3,1H-,I3,' IN THE FOLLOWING CARD -',/20X,20A4)
      ENDIF
      IF ( Num(1)<Limit(1) .OR. Num(2)>Limit(2) ) THEN
         WRITE (Nout,99003) Ufm , Limit , Record
99003    FORMAT (A23,' 8022, NUMBERS ARE OUT OF THE RANGE ',I3,1H-,I3,' IN THE FOLLOWING CARD - ',/20X,20A4)
         Ierror = 1
      ENDIF
   ENDIF
END SUBROUTINE xrgdev
