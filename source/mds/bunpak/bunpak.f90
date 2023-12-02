!*==bunpak.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bunpak(Ig,I,Nj,Jg)
!
!     THIS ROUTINE WORKS SIMILARLY AS BUNPK EXCEPT IT UNPACKS A WHOLE
!     (I-TH) ROW OF GRID NUMBERS (1 THRU NJ) FROM IG TABLE, AND SAVES
!     THE UNPACKED DATA IN JG ARRAY.
!     (BUNPK UNPACKS ONLY AN ELEMENT OF GRID NUMBER IN IG TABLE)
!
!     THIS ROUTINE GREATLY INCREASES BANDIT INTERNAL EFFICIENCY
!     WRITTEN BY G.CHAN/UNISYS,    MAY 1988
!
   USE c_bandb
   USE c_bands
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER(INT16) , DIMENSION(1) :: Ig
   INTEGER :: I
   INTEGER :: Nj
   INTEGER , DIMENSION(1) :: Jg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: n , n1
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL errtrc
!
! End of declarations rewritten by SPAG
!
!
!DC   NEXT 2 LINES FOR CDC AND UNIVAC ONLY
!     EXTERNAL         ANDF,     RSHIFT
!     INTEGER          ANDF,     RSHIFT  ,IG(1)
!
!     NEXT LINE FOR IBM, VAX, AND MACHINES THAT HAVE INTEGER*2
!
   DATA nam/4HUNPA , 4HK   /
!
   IF ( Nj>maxdeg ) THEN
      WRITE (nout,99001) Nj , maxdeg
99001 FORMAT ('0 *** BUNPAK .GT. MAXDEG',2I7)
      CALL errtrc(nam)
   ENDIF
!
   ipass = ipass + Nj
   n1 = I
!
!     ********************************************
!     UNIVAC AND CDC MACHINES
!     ********************************************
!
!     DO 40 N=1,NJ,NW
!     N2 = IG(N1)
!     N3 = N+NW-1
!     DO 30 M=1,NW
!     JG(N3) = ANDF(N2,MASK)
!     IF (M .EQ. NW) GO TO 40
!     N2 = RSHIFT(N2,NBIT)
!  30 N3 = N3-1
!  40 N1 = N1+II1
!     RETURN
!
!     ********************************************
!     IBM AND VAX MACHINES
!     ********************************************
!
   DO n = 1 , Nj
      Jg(n) = Ig(n1)
      n1 = n1 + ii1
   ENDDO
END SUBROUTINE bunpak
