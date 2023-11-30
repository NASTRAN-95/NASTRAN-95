
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
   IMPLICIT NONE
   INTEGER Dum3b(3) , Dum4(4) , Dum4s(4) , Ibuf , Ii1 , Ipass , Mask , Maxdeg , Nbit , Nout , Nw
   COMMON /bandb / Nbit , Dum3b , Ipass , Nw
   COMMON /bands / Dum4s , Ii1 , Maxdeg , Dum4 , Mask
   COMMON /system/ Ibuf , Nout
   INTEGER I , Nj
   INTEGER*2 Ig(1)
   INTEGER Jg(1)
   INTEGER n , n1 , nam(2)
!
!DC   NEXT 2 LINES FOR CDC AND UNIVAC ONLY
!     EXTERNAL         ANDF,     RSHIFT
!     INTEGER          ANDF,     RSHIFT  ,IG(1)
!
!     NEXT LINE FOR IBM, VAX, AND MACHINES THAT HAVE INTEGER*2
!
   DATA nam/4HUNPA , 4HK   /
!
   IF ( Nj>Maxdeg ) THEN
      WRITE (Nout,99001) Nj , Maxdeg
99001 FORMAT ('0 *** BUNPAK .GT. MAXDEG',2I7)
      CALL errtrc(nam)
   ENDIF
!
   Ipass = Ipass + Nj
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
      n1 = n1 + Ii1
   ENDDO
END SUBROUTINE bunpak
