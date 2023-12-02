!*==bunpk.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION bunpk(Ig,I,J)
   USE c_bandb
   USE c_bands
   USE iso_fortran_env
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: bunpk
   INTEGER(INT16) , DIMENSION(1) :: Ig
   INTEGER :: I
   INTEGER :: J
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: loc , n1
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     UNPACK INTEGER GRID NO. FROM IG TABLE.   SEE BPACK FOR PACKING
!     USE APPROP. PORTION OF THIS ROUTINE FOR DIFFERENT TYPE OF MACHINE.
!
!
!
   ipass = ipass + 1
   loc = J - 1
!
!     ********************************************
!     UNIVAC AND CDC MACHINES
!     ********************************************
!     INTEGER          RSHIFT,   ANDF
!
!     N1 =II1*(LOC/NW)+I
!     N2 =MOD(LOC,NW)*NBIT+NBIT
!     LOC=RSHIFT(IG(N1),NBPW-N2)
!     BUNPK=ANDF(LOC,MASK)
!     RETURN
!
!     ********************************************
!     IBM AND VAX MACHINES
!     (IG IS SET TO INTEGER*2 IN BPACK AND BUNPK, ELSEWHERE INTEGER*4)
!     INTEGER*2        IG(1)
!     ********************************************
!
   n1 = ii1*loc + I
   bunpk = Ig(n1)
END FUNCTION bunpk
