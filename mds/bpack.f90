
SUBROUTINE bpack(Ig,I,J,L)
!
   IMPLICIT NONE
   INTEGER Dum1b , Dum3b(3) , Dum4s(4) , Dum5s(5) , Ii1 , Ipass , Mask , Nbit , Nbpw , Nw
   COMMON /bandb / Nbit , Dum3b , Ipass , Nw , Dum1b , Nbpw
   COMMON /bands / Dum4s , Ii1 , Dum5s , Mask
   INTEGER I , J , L
   INTEGER*2 Ig(1)
   INTEGER loc , n1
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
   Ipass = Ipass + 1
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
   n1 = Ii1*loc + I
   Ig(n1) = L
END SUBROUTINE bpack
