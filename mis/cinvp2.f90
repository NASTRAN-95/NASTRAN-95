
SUBROUTINE cinvp2(*)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cdp , Dumm(36) , Filea(7) , Filel(7) , Fileu(7) , Ib , Ij(10) , Nz , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 ,    &
         & Scr8 , Scr9 , Sr1fil , Sr2fil , Sr3fil , Switch , Sysbuf
   DOUBLE PRECISION Det(2) , Mindia
   REAL Dum(4) , Power , Z(1)
   COMMON /cdcmpx/ Filea , Filel , Fileu , Sr1fil , Sr2fil , Sr3fil , Det , Power , Nz , Mindia , Ib
   COMMON /cinvpx/ Dumm , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , Scr8 , Scr9
   COMMON /cinvxx/ Dum , Switch
   COMMON /names / Ij , Cdp
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER ioff
   INTEGER korsz
!
! End of declarations
!
!
!     CINVP2 INITIALIZES AND CALLS CDCOMP FOR CINVPR
!
!
   ioff = Fileu(7)
   Filea(1) = Scr1
   IF ( Switch==0 ) THEN
      Filel(1) = Scr3
      Fileu(1) = Scr4
   ELSE
      Filel(1) = Scr8
      Fileu(1) = Scr9
      IF ( Switch<0 ) Filea(1) = -Filea(1)
      IF ( Switch/=-204 ) Switch = 1
   ENDIF
   Sr1fil = Scr5
   Sr2fil = Scr6
   Sr3fil = Scr7
   Filea(2) = Dumm(3)
   Filea(3) = Dumm(3)
   Filea(4) = Dumm(4)
   Filea(5) = Cdp
   Filea(6) = 0
   Filea(7) = 0
   Filel(5) = Cdp
   Nz = korsz(Z)
   IF ( Switch==-204 ) Nz = Nz - 2*Sysbuf
   Ib = 0
   CALL cdcomp(*100,Z,Z,Z)
   IF ( Switch/=0 ) Fileu(7) = ioff
   RETURN
!
 100  RETURN 1
END SUBROUTINE cinvp2
