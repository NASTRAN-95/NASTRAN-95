
SUBROUTINE line10(X1,Y1,X2,Y2,Penden,Opt)
   IMPLICIT NONE
   INTEGER Opt , Penden
   REAL X1 , X2 , Y1 , Y2
   INTEGER a(6) , line , optx
!
!     X1,Y1  = STARTING POINT OF THE LINE
!     X2,Y2  = TERMINAL POINT OF THE LINE
!     PENDEN = PEN NUMBER OR LINE DENSITY
!     OPT    = -1 TO INITIATE  THE LINE MODE
!            = +1 TO TERMINATE THE LINE MODE
!            = 0 TO DRAW A LINE
!
   DATA optx , line/ - 1 , 5/
!
   IF ( optx>=0 ) optx = Opt
   IF ( Opt<0 ) THEN
   ELSEIF ( Opt==0 ) THEN
      a(1) = line
      a(2) = Penden
      a(3) = ifix(X1+.1)
      a(4) = ifix(Y1+.1)
      a(5) = ifix(X2+.1)
      a(6) = ifix(Y2+.1)
      IF ( optx/=0 ) THEN
!
!     INITIATE THE LINE MODE.
!
         a(1) = a(1) + 10
         optx = 0
      ENDIF
!
!     DRAW THE LINE.
!
      CALL wplt10(a,0)
   ELSE
!
!     TERMINATE THE LINE MODE.
!
      CALL wplt10(a,1)
      optx = -1
   ENDIF
!
END SUBROUTINE line10