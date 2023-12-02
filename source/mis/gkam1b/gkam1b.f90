!*==gkam1b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gkam1b(Usetd,Scr1,Scr2,Phidh,Phidh1,Modes,Core,Lhset,Noue,Scr3)
   USE c_bitpos
   USE c_parmeg
   USE c_patx
   USE c_system
   USE c_zblpkx
   IMPLICIT NONE
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
   uset = Usetd
   lc = korsz(Core(Modes))
   lcore = lc
   CALL calcv(Scr1,ud,ua,ue,Core(Modes))
!
!     BUILD  EXE  IDENTY   MATRIX
!
   nz = lc - sysbuf
   CALL gopen(Scr2,Core(nz+1),1)
   CALL makmcb(mcb,Scr2,Noue,6,1)
   a(1) = 1.0
   DO i = 1 , Noue
      CALL bldpk(1,1,Scr2,0,0)
      ii = i
      CALL zblpki
      CALL bldpkn(Scr2,0,mcb)
   ENDDO
   CALL close(Scr2,1)
   CALL wrttrl(mcb)
!
!     SET  UP  FOR  MERGE
!
   irule = 0
   ia22(1) = Scr2
   CALL rdtrl(ia22)
   ia(1) = Phidh
   ia(2) = Lhset
   ia(3) = n1 + n2 + n3
   ia(4) = 2
   ia(5) = 1
   ia21(1) = 0
   ia12(1) = 0
   ia11(1) = Phidh1
   CALL makmcb(Core(Modes),Scr3,Lhset,2,1)
   CALL rdtrl(ia11)
!
!     BUILD  VECTOR IN CORE
!
   CALL gopen(Scr3,Core(nz+1),1)
   CALL bldpk(1,1,Scr3,0,0)
   ii = Modes - 1
   DO i = 1 , Noue
      ii = ii + 1
      CALL zblpki
   ENDDO
   CALL bldpkn(Scr3,0,Core(Modes))
   CALL close(Scr3,1)
   CALL wrttrl(Core(Modes))
   CALL merge(Scr3,Scr1,Core(Modes))
   CALL wrttrl(ia)
END SUBROUTINE gkam1b
