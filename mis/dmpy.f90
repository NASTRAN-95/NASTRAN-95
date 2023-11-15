
SUBROUTINE dmpy(Z,Zd)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Ad(2) , Xd(2)
   INTEGER Clsrew , Eol , Eor , Filea(7) , Fileb(7) , Filec(7) , Flag , I , Incr , Ix , Nx , Nz , One , Rd , Rdrew , Sign , Sysbuf ,&
         & Type , Wrt , Wrtrew
   COMMON /dmpyx / Filea , Fileb , Filec , Nz , Flag , Sign
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Type , One , Nx , Incr
   COMMON /zblpkx/ Xd , Ix
   COMMON /zntpkx/ Ad , I , Eol , Eor
!
! Dummy argument declarations
!
   INTEGER Z(1)
   DOUBLE PRECISION Zd(1)
!
! Local variable declarations
!
   INTEGER buf1 , buf2 , j , k , kl , kr , ptype , qtype , rcc
!
! End of declarations
!
!
!     DMPY WILL PRE OR POST MULTIPLY AN ARBITRARY MATRIX BY A DIAGONAL
!     MATRIX.
!
!     FILEA = MATRIX CONTROL BLOCK FOR DIAGONAL MATRIX.
!     FILEB = MATRIX CONTROL BLOCK FOR ARBITRARY MATRIX.
!     FILEC = MATRIX CONTROL BLOCK FOR PRODUCT MATRIX.
!     Z     = ADDRESS OF A BLOCK OF CORE FOR WORKING SPACE. ZD IS SAME
!             BLOCK.
!     NZ    = LENGTH OF THIS BLOCK.
!     FLAG .EQ. 0 FOR PRE-MULTIPLICATION BY DIAGONAL.
!     FLAG .NE. 0 FOR POST-MULTIPLICATION BY DIAGONAL.
!     SIGN .EQ. +1 FOR POSITIVE PRODUCT.
!     SIGN .EQ. -1 FOR NEGATIVE PRODUCT.
!
!
!
!
!     PERFORM GENERAL INITIALIZATION
!
   buf1 = Nz - Sysbuf + 1
   buf2 = buf1 - Sysbuf
   One = 1
   Incr = 1
   Filec(2) = 0
   Filec(6) = 0
   Filec(7) = 0
   Nx = Filea(3)
!
!     COMPUTE TYPE OF C MATRIX.
!     RCC = 1 FOR REAL, = 2 FOR COMPLEX
!     QTYPE = 2 FOR RDP, = 4 FOR CDP
!
   rcc = 0
   IF ( Filea(5)>2 .OR. Fileb(5)>2 ) rcc = 2
   qtype = rcc + 2
   IF ( rcc==0 ) rcc = 1
   Type = qtype*Sign
   ptype = Filec(5)
!
!     OPEN PRODUCT MATRIX AND WRITE HEADER RECORD.
!
   CALL gopen(Filec(1),Z(buf1),Wrtrew)
!
!     UNPACK DIAGONAL MATRIX IN CORE AND OPEN ARBITRARY MATRIX.
!
   CALL gopen(Filea(1),Z(buf2),Rdrew)
   CALL unpack(*300,Filea,Z)
   CALL close(Filea(1),Clsrew)
   CALL gopen(Fileb(1),Z(buf2),Rdrew)
!
!     PERFORM MATRIX MULTIPLICATION.
!
   j = 1
 100  kr = (j-1)*rcc + 1
   CALL bldpk(qtype,ptype,Filec(1),0,0)
   CALL intpk(*200,Fileb(1),0,qtype,0)
   DO
      CALL zntpki
      kl = (I-1)*rcc + 1
      k = kl
      IF ( Flag/=0 ) k = kr
      Xd(1) = Zd(k)*Ad(1)
      IF ( rcc/=1 ) THEN
         Xd(1) = Xd(1) - Zd(k+1)*Ad(2)
         Xd(2) = Zd(k)*Ad(2) + Zd(k+1)*Ad(1)
      ENDIF
      Ix = I
      CALL zblpki
      IF ( Eol/=0 ) EXIT
   ENDDO
 200  CALL bldpkn(Filec(1),0,Filec)
   j = j + 1
   IF ( j>Fileb(2) ) GOTO 400
   GOTO 100
 300  DO
!
!     CODE FOR NULL DIAGONAL MATRIX.
!
      CALL bldpkn(Filec(1),0,Filec)
      IF ( Filec(2)>=Fileb(2) ) EXIT
   ENDDO
!
!     CLOSE FILES AND RETURN.
!
 400  CALL close(Filea(1),Clsrew)
   CALL close(Fileb(1),Clsrew)
   CALL close(Filec(1),Clsrew)
END SUBROUTINE dmpy
