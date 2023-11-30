
SUBROUTINE invert(Ia,Ib,Scr1)
   IMPLICIT NONE
   INTEGER Fa(7) , Fb(7) , Nx , Prec , Scrfil
   REAL Z(1)
   COMMON /invtrx/ Fa , Fb , Scrfil , Nx , Prec
   COMMON /zzzzzz/ Z
   INTEGER Ia , Ib , Scr1
   INTEGER korsz
   INTEGER name(2)
!
!     DRIVER  FOR  INVTR
!
!     INVERTS  LOWER OR UPPER TRIANGLE  IA  ONTO  IB
!
!     SCR1 WILL BE USED  ONLY IF  IA  IS AN  UPPER  TRIANGLE
!
!
!
   DATA name/4HINVE , 4HRT  /
!
!     FILL  MATRIX  CONTROL  BLOCKS  FOR  A  AND  B
!
   Fa(1) = Ia
   CALL rdtrl(Fa)
   Fb(1) = Ia
   CALL rdtrl(Fb)
   Fb(1) = Ib
   Scrfil = Scr1
   Prec = Fa(5)
   Nx = korsz(Z)
   CALL invtr(*100,Z,Z)
   CALL wrttrl(Fb)
   RETURN
 100  DO
!
!     SINGULAR  MATRIX
!
      CALL mesage(-5,Fa,name)
   ENDDO
END SUBROUTINE invert
