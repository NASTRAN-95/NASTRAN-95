
SUBROUTINE gigtkg
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(8) , Scr1 , Spline , Z(1)
   INTEGER Gsize , Ii , Incr , Iti , Ito , Iz(1) , Ksize , Nn , Out , Scr2 , Scr3 , Sysbuf
   CHARACTER*23 Ufm
   COMMON /gicom / Spline , Dum , Ksize , Gsize , Scr1 , Scr2 , Scr3
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Out
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER buf1 , buf2 , ctype , i , icm , iss , j , jj , jjj , k , kcol , kst , nam(2) , ncore , nwr , sdtab(6,5) , trl(7)
   INTEGER korsz
   REAL write
   EXTERNAL write
!
! End of declarations
!
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA nam/4HGIGT , 4HKG  /
   DATA sdtab/9 , 9 , 0 , 9 , 1 , 9 , 9 , 0 , 1 , 9 , 2 , 3 , 9 , 9 , 0 , 9 , 9 , 9 , 9 , 9 , 0 , 9 , 1 , 2 , 9 , 9 , 0 , 9 , 1 , 2/
!
   ncore = korsz(Z) - 2*Sysbuf
   buf1 = ncore
   buf2 = buf1 + Sysbuf
   Iti = 1
   Ito = 1
   Ii = 1
   Incr = 1
   trl(1) = Scr2
   trl(2) = 0
   trl(3) = Gsize
   trl(4) = 2
   trl(5) = 1
   trl(6) = 0
   trl(7) = 0
!
!     BUILD A G BY K MATRIX PUT OUT SPLINE3 COLUMNS WHEN NECESSARY
!
   CALL gopen(Scr2,Z(buf1),1)
   CALL gopen(Scr3,Z(buf2),0)
   iss = Gsize + 1
   ncore = ncore - iss
   kcol = 0
   DO i = 1 , Ksize
      IF ( kcol<i ) THEN
         CALL read(*100,*150,Scr3,Z(iss),ncore,0,nwr)
         GOTO 200
      ENDIF
 50   IF ( kcol==i ) THEN
!
!     BUILD COLUMN FOR SPLINE CARD
!
         DO j = 1 , Gsize
            Z(j) = 0.0
         ENDDO
         Nn = Gsize
         jj = iss + 4
         jjj = iss + nwr - 19
         DO j = jj , jjj , 3
            k = Iz(j) + Iz(j+1) - 1
            Z(k) = Z(j+2)
         ENDDO
         CALL pack(Z,Scr2,trl)
      ELSE
         Nn = 1
         Z(1) = 0.0
         CALL pack(Z,Scr2,trl)
      ENDIF
      CYCLE
 100  kcol = Ksize + 1
      GOTO 50
 150  kst = Iz(iss+2)
      ctype = Iz(iss+nwr-9)
      icm = Iz(iss+3)
      k = sdtab(icm,ctype)
      IF ( k==9 ) GOTO 300
      kcol = kst + k
      GOTO 50
   ENDDO
   CALL close(Scr2,1)
   CALL close(Scr3,1)
   CALL wrttrl(trl)
   GOTO 99999
!
!     ERROR MESSAGES
!
 200  CALL mesage(-8,ncore,nam)
 300  WRITE (Out,99001) Ufm , Iz(iss) , ctype , icm
99001 FORMAT (A23,' 2263, SPLINE3',I9,' FOR CAERO',I1,' HAS ILLEGAL COMPONENT',I6)
   CALL mesage(-37,0,nam)
99999 RETURN
END SUBROUTINE gigtkg
