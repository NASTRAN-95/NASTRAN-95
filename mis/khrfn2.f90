
INTEGER FUNCTION khrfn2(Word,J,Izb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dummy(40)
   INTEGER Ncpw
   COMMON /system/ Dummy , Ncpw
!
! Dummy argument declarations
!
   INTEGER Izb , J
   INTEGER Word(1)
!
! Local variable declarations
!
   INTEGER blank , i , ij
   INTEGER khrfn1
!
! End of declarations
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
   IF ( J<0 ) i = Ncpw
   ij = iabs(J)
   khrfn2 = khrfn1(khrfn2,i,Word(1),ij)
END FUNCTION khrfn2
