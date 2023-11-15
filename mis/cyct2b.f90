
SUBROUTINE cyct2b(Input,Outpt,Ncol,Iz,Mcb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Iik , Incr , Incr1 , Ita , Itb , Itc , Jj , Jjk
   COMMON /packx / Ita , Itb , Ii , Jj , Incr
   COMMON /unpakx/ Itc , Iik , Jjk , Incr1
!
! Dummy argument declarations
!
   INTEGER Input , Ncol , Outpt
   INTEGER Iz(4) , Mcb(7)
!
! Local variable declarations
!
   INTEGER i , izero
   REAL zero
!
! End of declarations
!
!
!     THE PURPOSE OF THIS SUBROUTINE IS TO COPY NCOL COLUMNS FROM
!     INPUT TO OUTPUT USING CORE AT IZ -- MCB IS THE TRAILER
!
   EQUIVALENCE (zero,izero)
   DATA zero/0.0/
!
!
   Ita = iabs(Itc)
   Itb = Ita
   Incr = Incr1
   DO i = 1 , Ncol
      Iik = 0
      CALL unpack(*100,Input,Iz)
      Ii = Iik
      Jj = Jjk
 50   CALL pack(Iz,Outpt,Mcb)
      CYCLE
!
!     NULL COLUMN
!
 100  Ii = 1
      Jj = 1
      Iz(1) = izero
      Iz(2) = izero
      Iz(3) = izero
      Iz(4) = izero
      GOTO 50
   ENDDO
!
END SUBROUTINE cyct2b
