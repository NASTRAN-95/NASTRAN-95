!*==gkam1b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gkam1b(Usetd,Scr1,Scr2,Phidh,Phidh1,Modes,Core,Lhset,Noue,Scr3)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_PARMEG
   USE C_PATX
   USE C_SYSTEM
   USE C_ZBLPKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Usetd
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Phidh
   INTEGER :: Phidh1
   INTEGER :: Modes
   INTEGER , DIMENSION(1) :: Core
   INTEGER :: Lhset
   INTEGER :: Noue
   INTEGER :: Scr3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nz
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL bldpk , bldpkn , calcv , close , gopen , korsz , makmcb , merge , rdtrl , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
!
!
!
! ----------------------------------------------------------------------
!
   Lhset = Lhset + Noue
!
!     BUILD  MERGE  VECTOR
!
   Uset = Usetd
   Lc = korsz(Core(Modes))
   Lcore = Lc
   CALL calcv(Scr1,Ud,Ua,Ue,Core(Modes))
!
!     BUILD  EXE  IDENTY   MATRIX
!
   nz = Lc - Sysbuf
   CALL gopen(Scr2,Core(nz+1),1)
   CALL makmcb(mcb,Scr2,Noue,6,1)
   A(1) = 1.0
   DO i = 1 , Noue
      CALL bldpk(1,1,Scr2,0,0)
      Ii = i
      CALL zblpki
      CALL bldpkn(Scr2,0,mcb)
   ENDDO
   CALL close(Scr2,1)
   CALL wrttrl(mcb)
!
!     SET  UP  FOR  MERGE
!
   Irule = 0
   Ia22(1) = Scr2
   CALL rdtrl(Ia22)
   Ia(1) = Phidh
   Ia(2) = Lhset
   Ia(3) = N1 + N2 + N3
   Ia(4) = 2
   Ia(5) = 1
   Ia21(1) = 0
   Ia12(1) = 0
   Ia11(1) = Phidh1
   CALL makmcb(Core(Modes),Scr3,Lhset,2,1)
   CALL rdtrl(Ia11)
!
!     BUILD  VECTOR IN CORE
!
   CALL gopen(Scr3,Core(nz+1),1)
   CALL bldpk(1,1,Scr3,0,0)
   Ii = Modes - 1
   DO i = 1 , Noue
      Ii = Ii + 1
      CALL zblpki
   ENDDO
   CALL bldpkn(Scr3,0,Core(Modes))
   CALL close(Scr3,1)
   CALL wrttrl(Core(Modes))
   CALL merge(Scr3,Scr1,Core(Modes))
   CALL wrttrl(Ia)
END SUBROUTINE gkam1b
