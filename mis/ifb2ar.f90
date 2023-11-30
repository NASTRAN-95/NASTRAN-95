
SUBROUTINE ifb2ar(Type,Ifb,Ar,L)
   IMPLICIT NONE
   INTEGER Ifb , L , Type
   INTEGER Ar(1)
   CHARACTER*12 c12
   CHARACTER*8 c8
   CHARACTER*7 fmt(10) , fmtx
   CHARACTER*10 fmty , fnt(9)
   INTEGER i , ia , j , k , sub(2) , zero(2)
   REAL ra , x , xl
!
!     THIS ROUTINE STORES IN ARRAY AR(L+1) THE BCD VALUE OF IFB, AND
!     UPDATE THE L COUNTER
!
!     IF TYPE=1, IFB IS AN INTEGER, AND 8 DIGITS ARE USED IN AR, AND
!                L IS INCREASED BY 2 (INTEGER IS RIGHT ADJUSTED)
!     IF TYPE=2, IFB IS A REAL NUMBER, 12 DIGITS ARE USED IN AR, AND
!                L IS INCREASED BY 3
!     IF TYPE=3, IFB IS A BCD WORD, 4 LETTERS ARE USE IN AR, AND
!                L IS INCREASED BY 1
!
   !>>>>EQUIVALENCE (ia,ra)
   DATA fmt/'(F12.9)' , '(F12.8)' , '(F12.7)' , '(F12.6)' , '(F12.5)' , '(F12.4)' , '(F12.3)' , '(F12.2)' , '(F12.1)' , '(F12.0)'/
   DATA fnt/'(1X,F11.8)' , '(1X,F11.7)' , '(1X,F11.6)' , '(1X,F11.5)' , '(1X,F11.4)' , '(1X,F11.3)' , '(1X,F11.2)' , '(1X,F11.1)' , &
       &'(1X,F11.0)'/
   DATA zero/4H     , 4H 0.0/
   DATA sub/4HIFB2 , 4HAR  /
!
   k = -1
   j = Type + 1
   IF ( j==1 .OR. j==3 ) GOTO 400
   IF ( j==2 ) GOTO 200
   IF ( j==4 ) GOTO 300
 100  k = k + 1
   IF ( k<0 ) THEN
      CALL mesage(-37,0,sub)
   ELSEIF ( k/=0 ) THEN
      GOTO 300
   ENDIF
!
!     INTEGER, RIGHT ADJUSTED
!
 200  WRITE (c8,99001,ERR=400) Ifb
99001 FORMAT (I8)
   READ (c8,99002) Ar(L+1) , Ar(L+2)
99002 FORMAT (2A4)
   L = L + 2
   RETURN
!
!     BCD WORD
!
 300  Ar(L+1) = Ifb
   L = L + 1
   RETURN
!
!     REAL NUMBER
!
 400  ia = Ifb
   x = abs(ra)
   IF ( x<1.0E-36 ) THEN
      Ar(L+1) = zero(1)
      Ar(L+2) = zero(1)
      Ar(L+3) = zero(2)
      L = L + 3
      GOTO 99999
   ELSE
      xl = alog10(x)
      IF ( xl>-4.0 .AND. xl<10.0 ) THEN
         IF ( xl<=1.0 ) THEN
            fmtx = fmt(1)
            WRITE (c12,fmtx) ra
            GOTO 500
         ELSE
            i = xl
            IF ( ra<0. ) i = i + 1
            IF ( i>0 .AND. i<=9 ) THEN
               IF ( ra>0. .AND. xl>0. ) THEN
                  fmty = fnt(i)
                  WRITE (c12,fmty) ra
               ELSE
                  fmtx = fmt(i)
                  WRITE (c12,fmtx) ra
               ENDIF
               GOTO 500
            ENDIF
         ENDIF
      ENDIF
      WRITE (c12,99003,ERR=100) ra
99003 FORMAT (1P,E12.5)
   ENDIF
 500  READ (c12,99004) (Ar(L+j),j=1,3)
99004 FORMAT (3A4)
   L = L + 3
99999 RETURN
END SUBROUTINE ifb2ar