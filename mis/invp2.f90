
SUBROUTINE invp2() !HIDESTARS (*)
   IMPLICIT NONE
   DOUBLE PRECISION Det , Detc , Detdet , Mindd
   INTEGER Dum(14) , Filea(7) , Filel(7) , Fileu(7) , Ia(7) , Ichl , Ij(8) , Ik(5) , Il(7) , Ipowr , Iscr1 , Iscr2 , Iscr3 ,        &
         & Isr3fl , Iu(7) , Ksystm(63) , Lowtri , Mind , Mz , Nz , Option , Prec , Q(1) , Rdp , Scr1(7) , Scr2(7) , Scr3 , Scr4 ,   &
         & Scr5 , Scr6 , Scr7 , Scr8 , Sr1fil , Sr2fil , Switch , Uprtri
   REAL Dumm(12) , Power , Scrx , Scrxx , Z(1)
   COMMON /dcompx/ Ia , Il , Iu , Iscr1 , Iscr2 , Iscr3 , Detdet , Ipowr , Mz , Mind
   COMMON /invpwx/ Dum , Scr1 , Scr2 , Scrx , Scrxx , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , Scr8
   COMMON /invpxx/ Dumm , Switch
   COMMON /names / Ij , Rdp , Ik , Lowtri , Uprtri
   COMMON /reigkr/ Option
   COMMON /sfact / Filea , Filel , Fileu , Sr1fil , Sr2fil , Nz , Det , Detc , Power , Isr3fl , Mindd , Ichl
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER i , opt2
   INTEGER korsz
!
!     INVP2 INITIALIZES THEN CALLS EITHER SDCOMP OR DECOMP DEPENDING ON
!     THE OPTION SELECTED ON THE EIGR CARD
!
   !>>>>EQUIVALENCE (Q(1),Z(1))
   !>>>>EQUIVALENCE (Ksystm(55),Prec)
   DATA opt2/4HUINV/
!
   Filea(1) = Scr1(1)
   IF ( Switch==1 ) THEN
      Filel(1) = Scr7
      Fileu(1) = Scr8
   ELSE
      Filel(1) = Scr2(1)
      Fileu(1) = Scr3
   ENDIF
   Sr1fil = Scr4
   Sr2fil = Scr5
   Isr3fl = Scr6
   Ichl = 0
   Filea(2) = Dum(2)
   Filea(3) = Dum(3)
   Filea(4) = Dum(4)
   Filea(5) = Prec
   Filea(6) = 0
   Filea(7) = 0
   Filel(5) = Prec
   IF ( Option==opt2 ) THEN
!
!     UNSYMMETRIC DECOMPOSITION SELECTED.
!
      DO i = 1 , 21
         Ia(i) = Filea(i)
      ENDDO
      Iscr1 = Scr4
      Iscr2 = Scr5
      Iscr3 = Scr6
      Mz = korsz(Q)
      CALL decomp(*100,Q,Q,Q)
      Il(3) = Il(2)
      Il(4) = Lowtri
      CALL wrttrl(Il)
      Iu(3) = Iu(2)
      Iu(4) = Uprtri
      Iu(5) = Il(5)
      CALL wrttrl(Iu)
      GOTO 99999
   ELSE
!
!     SYMMETRIC DECOMPOSITION SELECTED.
!
      Nz = korsz(Z)
      CALL sdcomp(*100,Z,Z,Z)
      Filel(3) = Filel(2)
      Filel(4) = Lowtri
      CALL wrttrl(Filel)
      RETURN
   ENDIF
 100  RETURN 1
99999 RETURN
END SUBROUTINE invp2