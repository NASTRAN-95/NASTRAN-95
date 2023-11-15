
SUBROUTINE page
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Crdate(3) , Date(3) , Iofp , Ipage , Itline , Label(32) , Line , Loadn , Mach(4) , Maxlin , Method , Mpcn(3) , Otpe ,    &
         & Sym , Titlex(18)
   REAL Dum15(15) , Head1(32) , Head2(32) , Head3(32) , Spcn , St , Subtit(32) , Sysbuf , Title(32) , X(8)
   CHARACTER*7 Machos
   CHARACTER*11 Mchnam
   COMMON /chmach/ Mchnam , Machos
   COMMON /machin/ Mach
   COMMON /output/ Title , Subtit , Label , Head1 , Head2 , Head3
   COMMON /system/ Sysbuf , Otpe , Mpcn , Spcn , Method , Loadn , Sym , St , Ipage , Line , Itline , Maxlin , Date , Dum15 , Iofp , &
                 & X , Crdate
!
! Local variable declarations
!
   CHARACTER*30 ahead
   INTEGER fchar , i , in , iout , name(2) , ncmnam , ncmos
   CHARACTER*3 month(12)
!
! End of declarations
!
!
!     MASTER PAGING ROUTINE FOR NASTRAN.
!
   EQUIVALENCE (Titlex(1),Title(1))
   DATA month/'JAN' , 'FEB' , 'MAR' , 'APR' , 'MAY' , 'JUN' , 'JUL' , 'AUG' , 'SEP' , 'OCT' , 'NOV' , 'DEC'/
   DATA name/4HPAGE , 4H    /
!
   iout = 1
 100  Ipage = Ipage + 1
   Itline = Itline + Line
   Line = 0
   IF ( Itline>Maxlin ) THEN
!
!     MAX LINES EXCEEDED.  BUMP MAXLINES BY 3000 AND CALL MESAGE
!
      Maxlin = Maxlin + 3000
      CALL mesage(-19,Itline,name)
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
99001 FORMAT (A2)
      fchar = fchar + 3
      ahead(fchar:30) = Mchnam(1:ncmnam)//' '//Machos(1:ncmos)//' NASTRAN'
!
      WRITE (Otpe,99002) Titlex , ahead , month(in) , Date(2) , Date(3) , Ipage
99002 FORMAT (1H1,4X,17A4,A2,' /',A30,'/ ',A3,1X,I2,', ',I2,' / PAGE',I6)
      WRITE (Otpe,99003) Subtit
      WRITE (Otpe,99004) Label
      Line = Line + 4
      IF ( iout/=0 ) THEN
         WRITE (Otpe,99004) (Head1(i),i=1,32)
         WRITE (Otpe,99003) (Head2(i),i=1,32)
         WRITE (Otpe,99003) (Head3(i),i=1,32)
         Line = Line + 4
      ENDIF
   ENDIF
   RETURN
!
!
   ENTRY page1
!     ===========
!
   iout = 0
   GOTO 100
99003 FORMAT (5X,31A4,A3)
99004 FORMAT (1H0,4X,31A4,A3)
END SUBROUTINE page
