
SUBROUTINE gravl3(Nvect,Gvect,Sr1,Iharm)
   IMPLICIT NONE
   REAL B(4) , Z(1)
   INTEGER Ihalf , Ii , Isystm(175) , Ix(25) , Jhalf , Luset , Mach , Mn , Sysbuf
   COMMON /blank / Luset
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /system/ Sysbuf , Ix , Mn
   COMMON /zblpkx/ B , Ii
   COMMON /zzzzzz/ Z
   INTEGER Iharm , Nvect , Sr1
   REAL Gvect(1)
   INTEGER andf , korsz , rshift
   REAL cosph , costh , g , gxy , sinph , sinth
   INTEGER i , ibuf , il , iloop , m , mcb(7) , n
   EXTERNAL andf , rshift
!
!     BUILD GRAVITY LOADS FOR AXISYMMETRIC SHELL
!
!     DEFINITION OF VARIABLES
!
!     NVECT    NUMBER OF GRAVITY LOADS
!     GVECT    ARRAY OF G VECTORS
!     SR1      FILE TO PUT ACCELERATION VECTOR ON
!     IHARM    SINE OR COSINE SET FLAG -- 1 = SINE SET
!     LUSET    LENGTH OF G SET
!     MCB      MATRIX CONTROL BLOCK FOR SR1
!     M        NUMBER OF RINGS
!     N        NUMBER OF HARMONICS
!     IL       POINTER IN GVECT ARRAY
!
   EQUIVALENCE (Sysbuf,Isystm(1))
!
!     INITIALIZE STUFF
!
   ibuf = korsz(Z) - Sysbuf + 1
   CALL gopen(Sr1,Z(ibuf),1)
   CALL makmcb(mcb,Sr1,Luset,2,1)
   il = 1
   n = Mn
   m = Isystm(161)
!
!     BUILD NVECT GRAVITY VECTORS
!
   DO iloop = 1 , Nvect
      CALL bldpk(1,1,mcb(1),0,0)
!
!     COMPUTE VALUES
!
      sinth = 0.0
      sinph = 0.0
      cosph = 1.0
      g = sqrt(Gvect(il)*Gvect(il)+Gvect(il+1)*Gvect(il+1)+Gvect(il+2)*Gvect(il+2))
      costh = Gvect(il+2)/g
      IF ( Gvect(il)/=0.0 .OR. Gvect(il+1)/=0.0 ) THEN
         gxy = sqrt(Gvect(il)*Gvect(il)+Gvect(il+1)*Gvect(il+1))
         sinth = gxy/g
         sinph = Gvect(il+1)/gxy
         cosph = Gvect(il)/gxy
      ENDIF
      IF ( Iharm==2 ) THEN
!
!     COSINE SET
!
         B(1) = g*costh
         Ii = Luset - m*n*6 + 3
!
!     LOAD ZERO HARMONIC
!
         DO i = 1 , m
            CALL zblpki
            Ii = Ii + 6
         ENDDO
!
!     LOAD 2-D HARMONIC
!
         Ii = Ii - 2
         B(1) = g*sinth*cosph
         DO i = 1 , m
            CALL zblpki
            Ii = Ii + 1
            B(1) = -B(1)
            CALL zblpki
            B(1) = -B(1)
            Ii = Ii + 5
         ENDDO
      ELSE
!
!     SINE SET
!
         B(1) = g*sinth*sinph
         Ii = Luset - m*(n-1)*6 + 1
         DO i = 1 , m
            CALL zblpki
            Ii = Ii + 1
            CALL zblpki
            Ii = Ii + 5
         ENDDO
      ENDIF
!
!     END OF COLUMN
!
      CALL bldpkn(mcb(1),0,mcb(1))
      il = il + 3
   ENDDO
   CALL close(mcb(1),1)
   CALL wrttrl(mcb)
!
END SUBROUTINE gravl3
