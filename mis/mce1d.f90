
SUBROUTINE mce1d
   IMPLICIT NONE
   REAL A(1) , B(1) , Rg , Scr1 , Scr2 , Scr3 , Skp(53) , U , Uset , Z(1)
   DOUBLE PRECISION Ad(2) , Bd(2) , Zd(1)
   INTEGER Eol , Eor , Gm , I , Ipr , J , L , Mcb(7) , Ncol , Rm , Rn , Sysbuf , Type
   COMMON /blank / Uset , Rg , Gm , Scr1 , Scr2 , Scr3 , Rm , Rn , L , U , Mcb
   COMMON /system/ Sysbuf , Skp , Ipr
   COMMON /zblpkx/ Bd , J
   COMMON /zntpkx/ Ad , I , Eol , Eor
   COMMON /zzzzzz/ Zd
   INTEGER bcd(2) , k , mcb1(7) , mcb2(7) , n , n1 , ncol1 , nz , rdp
   INTEGER korsz
!
!     MCE1D SOLVES FOR GM IN THE MATRIX EQUATION RM*GM = -RN
!     WHERE RM IS A DIAGONAL MATRIX.
!
   !>>>>EQUIVALENCE (Mcb(2),Ncol) , (Ad(1),A(1)) , (Mcb(5),Type) , (Zd(1),Z(1)) , (Bd(1),B(1)) , (mcb1(2),ncol1)
   DATA bcd , rdp/4HMCE1 , 4HD    , 2/
!
!     OPEN RM MATRIX,SKIP HEADER RECORD AND READ MATRIX CONTROL BLOCK
!
   nz = korsz(Z)
   n = nz - Sysbuf
   CALL gopen(Rm,Z(n+1),0)
   Mcb(1) = Rm
   CALL rdtrl(Mcb)
!
!     FORM -RM
!
   Ncol = Mcb(2)
   DO k = 1 , Ncol
      CALL intpk(*100,Rm,0,rdp,0)
      CALL zntpki
      IF ( I/=k ) GOTO 200
      Zd(k) = -Ad(1)
   ENDDO
   CALL close(Rm,1)
!
!     OPEN RN MATRIX,SKIP HEADER RECORD AND READ MATRIX CONTROL BLOCK
!
   CALL gopen(Rn,Z(n+1),0)
   mcb1(1) = Rn
   CALL rdtrl(mcb1)
!
!     SET UP MATRIX CONTROL BLOCK BLOCK FOR GM
!
   CALL makmcb(mcb2,Gm,mcb1(3),mcb1(4),Ipr)
!
!     OPEN OUTPUT FILE FOR GM AND WRITE HEADER RECORD
!
   n1 = n - Sysbuf
   CALL gopen(Gm,Z(n1+1),1)
!
!     FORM GM = -RM(-1)*RN
!
   ncol1 = mcb1(2)
   DO k = 1 , ncol1
      CALL bldpk(rdp,Ipr,Gm,0,0)
      CALL intpk(*50,Rn,0,rdp,0)
      DO
         CALL zntpki
         J = I
         Bd(1) = Ad(1)/Zd(J)
         CALL zblpki
         IF ( Eol/=0 ) EXIT
      ENDDO
 50   CALL bldpkn(Gm,0,mcb2)
   ENDDO
!
!     CLOSE GM AND RM FILES AND WRITE TRAILER FOR GM
!
   CALL close(Gm,1)
   CALL close(Rn,1)
   CALL wrttrl(mcb2)
   RETURN
!
!     CALL MESSAGE WRITER IF FATAL ERROR DETECTED
!
 100  L = -5
   GOTO 300
 200  L = -16
 300  CALL mesage(L,Rm,bcd)
END SUBROUTINE mce1d