
INTEGER FUNCTION khrfn4(Word)
   IMPLICIT NONE
   INTEGER Word(1)
   CHARACTER*1 c1(4) , c2(4)
   INTEGER w1 , w2
!
!     REVERSE BYTES FOR SORTING (USED MAINLY BY THE VAX MACHINE)
!
   EQUIVALENCE (c1(1),w1) , (c2(1),w2)
!
   w1 = Word(1)
   c2(1) = c1(4)
   c2(2) = c1(3)
   c2(3) = c1(2)
   c2(4) = c1(1)
   khrfn4 = w2
!
!     CDC VERSION
!     ===========
!
!     CHARACTER*1 WORD(10),C2(10)
!
!     C2(1)=WORD(4)
!     C2(2)=WORD(3)
!     C2(3)=WORD(2)
!     C2(4)=WORD(1)
!     DO 10 J=5,10
! 10  C2(J)=WORD(J)
!     KHRFN4=ISWAP(C2)
!
END FUNCTION khrfn4
