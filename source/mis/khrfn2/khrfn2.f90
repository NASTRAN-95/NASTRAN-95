!*==khrfn2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION khrfn2(Word,J,Izb)
   USE c_system
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: khrfn2
   INTEGER , DIMENSION(1) :: Word
   INTEGER :: J
   INTEGER :: Izb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank
   INTEGER :: i , ij
   EXTERNAL khrfn1
!
! End of declarations rewritten by SPAG
!
!
!     CHARACTER FUNCTION KHRFN2 RECIEVES THE J-TH BYTE OF WORD
!     LEFT ADJUSTED IF J IS .GE. ZERO, OR RIGHT ADJUSTED IF J .LT. ZERO
!     ZERO FILL IF IZB IS ZERO, OTHERWISE, BLANK FILL
!
   DATA blank/4H    /
!
   i = 1
   khrfn2 = blank
   IF ( Izb==0 ) khrfn2 = 0
   IF ( J<0 ) i = ncpw
   ij = iabs(J)
   khrfn2 = khrfn1(khrfn2,i,Word(1),ij)
END FUNCTION khrfn2
