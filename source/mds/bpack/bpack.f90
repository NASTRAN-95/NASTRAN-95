!*==bpack.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bpack(Ig,I,J,L)
!
   USE c_bandb
   USE c_bands
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER(INT16) , DIMENSION(1) :: Ig
   INTEGER :: I
   INTEGER :: J
   INTEGER :: L
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: loc , n1
!
! End of declarations rewritten by SPAG
!
!
!DC   NEXT 2 LINES FOR CDC AND UNIVAC ONLY
!     EXTERNAL         ORF,      LSHIFT
!     INTEGER          IG(1)
!
!     NEXT LINE FOR IBM, VAX, AND MACHINES THAT HAVE INTEGER*2
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     PACK INTERNAL GRID NO. INTO IG TABLE.  SEE BUNPK FOR UNPACKING
!     TABLE IG IS PACKED COLUMN-WISE.
!     USE APPROP. PORTION OF THIS ROUTINE FOR DIFFERENT TYPE OF MACHINE.
!     IPASS=COUNTER ON NUMBER OF CALLS TO PACK/UNPACK
!
!     NOTE - THIS ROUTINE DOES NOT CHECK NOR ZERO OUT THE PACKING SLOT
!            BEFORE PACKING.
!            L IS ASSUMED TO BE A POSITIVE INTEGER, NBIT BITS OR LESS
!
   ipass = ipass + 1
   loc = J - 1
!
!     ********************************************
!     UNIVAC AND CDC MACHINES
!     (IG SHOULD BE IN INTEGER*4 HERE)
!     ********************************************
!
!     N1 =II1*(LOC/NW)+I
!     N2 =MOD(LOC,NW)*NBIT+NBIT
!     LOC=ORF(IG(N1),LSHIFT(L,NBPW-N2))
!     IG(N1)=LOC
!
!     RETURN
!
!     ********************************************
!     IBM AND VAX MACHINES
!     (IG IS SET TO INTEGER*2 IN BPACK AND BUNPK, ELSEWHERE INTEGER*4)
!     INTEGER*2     IG(1)
!     ********************************************
!
   n1 = ii1*loc + I
   Ig(n1) = L
END SUBROUTINE bpack
