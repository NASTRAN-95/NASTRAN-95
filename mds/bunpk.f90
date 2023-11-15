
INTEGER FUNCTION bunpk(Ig,I,J)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum1b , Dum3b(3) , Dum4s(4) , Dum5s(5)
   INTEGER Ii1 , Ipass , Mask , Nbit , Nbpw , Nw
   COMMON /bandb / Nbit , Dum3b , Ipass , Nw , Dum1b , Nbpw
   COMMON /bands / Dum4s , Ii1 , Dum5s , Mask
!
! Dummy argument declarations
!
   INTEGER I , J
   INTEGER*2 Ig(1)
!
! Local variable declarations
!
   INTEGER loc , n1
!
! End of declarations
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     UNPACK INTEGER GRID NO. FROM IG TABLE.   SEE BPACK FOR PACKING
!     USE APPROP. PORTION OF THIS ROUTINE FOR DIFFERENT TYPE OF MACHINE.
!
!
!
   Ipass = Ipass + 1
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
   n1 = Ii1*loc + I
   bunpk = Ig(n1)
END FUNCTION bunpk
