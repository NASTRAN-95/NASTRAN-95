
SUBROUTINE page2(Lines)
!
!     2ND MASTER PAGING ROUTINE FOR NASTRAN
!
!     IABS(LINES) = NO. OF LINES TO BE ADDED FOR OUTPUT
!     IF CURRENT PAGE CAN NOT ACCOMODATE THE INCOMING LINES, A NEW PAGE
!     IS INITIATED WITH PROPER HEADINGS.
!
!     IF LINES IS NEGATIVE, A 6-LINE HEADER IS PRINTED.
!     IF LINES IS POSITIVE, A 3-LINE HEADER IS PRINTED AND FOLLOWED BY
!        3 BLANK LINES.
!
!     ENTRY POINT PAGE3 -
!     A 3-LINE HEADER IS PRINTED, NO BLANK LINES FOLLOWED. LINES CAN BE
!     NEGATIVE OR POSITIVE.
!
!     SIMPLIFIED BY G.CHAN/UNISYS, AND PAGE3 ADDED  12/92
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Crdate(3) , Date(3) , Dum15(15) , Dum6(6) , Dum8(8) , Head1(32) , Head2(32) , Head3(32) , Ibuf , Label(32) , Line ,      &
         & Mach(4) , Maxlin , Nout , Ofp , Page , St , Subtit(32) , Sym , Title(32) , Titlex(18) , Tline
   CHARACTER*7 Machos
   CHARACTER*11 Mchnam
   COMMON /chmach/ Mchnam , Machos
   COMMON /machin/ Mach
   COMMON /output/ Title , Subtit , Label , Head1 , Head2 , Head3
   COMMON /system/ Ibuf , Nout , Dum6 , Sym , St , Page , Line , Tline , Maxlin , Date , Dum15 , Ofp , Dum8 , Crdate
!
! Dummy argument declarations
!
   INTEGER Lines
!
! Local variable declarations
!
   CHARACTER*30 ahead
   INTEGER fchar , flag , i , in , ll , name(2) , ncmnam , ncmos
   CHARACTER*3 month(12)
!
! End of declarations
!
   EQUIVALENCE (Titlex(1),Title(1))
   DATA month/'JAN' , 'FEB' , 'MAR' , 'APR' , 'MAY' , 'JUN' , 'JUL' , 'AUG' , 'SEP' , 'OCT' , 'NOV' , 'DEC'/
   DATA name/4H PAG , 4HE2  /
!
   flag = 2
!
 100  IF ( Lines/=0 ) THEN
      ll = iabs(Lines)
      IF ( Sym-Line<ll .OR. Ofp/=0 ) THEN
!
         Page = Page + 1
         Tline = Tline + Line
         Line = 0
         IF ( Tline>Maxlin ) THEN
!
!     MAX LINES EXCEEDED.  BUMP MAXLINES BY 3000 AND CALL MESAGE
!
            Maxlin = Maxlin + 3000
            CALL mesage(-19,Tline,name)
         ELSE
            in = Date(1)
!
!   ASSEMBLE PAGE HEADING
!
            ahead = ' '
            ncmnam = index(Mchnam,' ') - 1
            IF ( ncmnam<=-1 ) ncmnam = 11
            ncmos = index(Machos,' ') - 1
            IF ( ncmos<=-1 ) ncmos = 7
            fchar = (18-ncmnam-ncmos)/2 + 1
            WRITE (ahead(fchar:fchar+1),99001) Crdate(3)
99001       FORMAT (A2)
            fchar = fchar + 3
            ahead(fchar:30) = Mchnam(1:ncmnam)//' '//Machos(1:ncmos)//' NASTRAN'
!
            WRITE (Nout,99002) Titlex , ahead , month(in) , Date(2) , Date(3) , Page
99002       FORMAT (1H1,4X,17A4,A2,' /',A30,'/ ',A3,1X,I2,', ',I2,' / PAGE',I6)
            WRITE (Nout,99004) Subtit
            WRITE (Nout,99005) Label
            Line = Line + 4
            IF ( flag>=0 ) THEN
               IF ( Lines>0 ) THEN
!
                  WRITE (Nout,99003)
99003             FORMAT (///)
                  Line = Line + 4
               ELSE
!
                  WRITE (Nout,99005) (Head1(i),i=1,32)
                  WRITE (Nout,99004) (Head2(i),i=1,32)
                  WRITE (Nout,99004) (Head3(i),i=1,32)
                  Line = Line + 4
               ENDIF
            ENDIF
            Line = Line + ll
         ENDIF
      ELSE
         Line = Line + ll
      ENDIF
   ENDIF
!
   Ofp = 0
   RETURN
!
!
   ENTRY page3(Lines)
!     ===================
!
   flag = -3
   GOTO 100
99004 FORMAT (5X,31A4,A3)
99005 FORMAT (/5X,31A4,A3)
!
END SUBROUTINE page2
