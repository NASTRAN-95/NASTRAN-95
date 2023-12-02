!*==xrgdev.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xrgdev
   USE c_system
   USE c_xmssg
   USE c_xrgdxx
   IMPLICIT NONE
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
   IF ( icol<=80 ) THEN
      istate = 7
      ind = 1
      num(1) = 0
      istr = icol
      SPAG_Loop_1_1: DO k = istr , 80
         icol = k
         CALL xrgdtp
         istate = state(itype,istate)
         IF ( istate/=0 ) THEN
            IF ( istate==2 ) EXIT SPAG_Loop_1_1
            IF ( istate==3 ) THEN
               ind = 2
               num(2) = 0
            ELSEIF ( istate/=5 .AND. istate/=6 .AND. istate/=7 ) THEN
               num(ind) = num(ind)*10 + number
            ENDIF
         ELSE
            ierror = 1
            j = 0
            WRITE (nout,99001) ufm , k , record , j , (i,i=1,8) , ierror , (j,i=1,8)
99001       FORMAT (A23,' 8020, SYNTAX ERROR NEAR COLUMN ',I3,' IN THE FOLLOWING CARD- ',/20X,20A4,/,(20X,I1,I9,7I10))
            RETURN
         ENDIF
      ENDDO SPAG_Loop_1_1
      IF ( ind/=2 ) THEN
         num(2) = num(1)
      ELSEIF ( num(2)<=num(1) ) THEN
         ierror = 1
         WRITE (nout,99002) ufm , num(1) , num(2) , record
99002    FORMAT (A23,' 8021, NON-INCREASING RANGE ',I3,1H-,I3,' IN THE FOLLOWING CARD -',/20X,20A4)
      ENDIF
      IF ( num(1)<limit(1) .OR. num(2)>limit(2) ) THEN
         WRITE (nout,99003) ufm , limit , record
99003    FORMAT (A23,' 8022, NUMBERS ARE OUT OF THE RANGE ',I3,1H-,I3,' IN THE FOLLOWING CARD - ',/20X,20A4)
         ierror = 1
      ENDIF
   ENDIF
END SUBROUTINE xrgdev
