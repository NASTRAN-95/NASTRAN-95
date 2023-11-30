
SUBROUTINE dtranp
   IMPLICIT NONE
   REAL Core(1)
   INTEGER Ia(7) , Iat(7) , Ibuf , Iscr(8) , Ixx , Lcore , Nout , Nscr
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Ixx
   COMMON /system/ Ibuf , Nout
   COMMON /trnspx/ Ia , Iat , Lcore , Nscr , Iscr
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Core
   INTEGER i , in1 , in2
   INTEGER korsz
!
!     DRIVER OF MATRIX TRANSPOSE MODULE
!
!     TRNSP    IA/IAT/C,N,IXX  $
!
!     THE DIAGONALS OF THE LOWER OR UPPER TRIANGULAR MATRICES ARE
!     REPLACED BY UNITY (1.0) IF IXX IS ONE. (DEFAULT IS ZERO)
!
   DATA in1 , in2/101 , 201/
!
   Ia(1) = in1
   CALL rdtrl(Ia(1))
   IF ( Ia(1)>0 ) THEN
      Iat(1) = in2
      Iat(2) = Ia(3)
      Iat(3) = Ia(2)
      Iat(4) = Ia(4)
      Iat(5) = Ia(5)
      Iat(6) = 0
      Iat(7) = 0
      Lcore = korsz(Core)
      Nscr = 8
      DO i = 1 , Nscr
         Iscr(i) = 300 + i
      ENDDO
      IF ( Ixx==1 ) Ixx = -123457890
      CALL trnsp(Core(1))
      CALL wrttrl(Iat(1))
   ELSE
      WRITE (Nout,99001) Uwm
99001 FORMAT (A25,' FROM TRNSP, MISSING INPUT DATA BLOCK FOR MATRIX ','TRANSPOSE')
   ENDIF
!
END SUBROUTINE dtranp