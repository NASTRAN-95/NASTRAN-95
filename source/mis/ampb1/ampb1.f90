!*==ampb1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampb1(Ipvct,Noh,Noe)
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ipvct
   INTEGER :: Noh
   INTEGER :: Noe
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibuf1
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL bldpk , bldpkn , close , gopen , korsz , makmcb , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE BUILDS A PARTITIONING VECTOR WHICH WILL APPEND NOE
!       TERM(OR COLUMNS)
!
!
!
!-----------------------------------------------------------------------
!
   ibuf1 = korsz(z) - sysbuf + 1
   CALL gopen(Ipvct,z(ibuf1),1)
   CALL makmcb(mcb,Ipvct,Noh+Noe,2,1)
   CALL bldpk(1,1,Ipvct,0,0)
   ii = Noh
   DO i = 1 , Noe
      a(1) = 1.0
      ii = ii + 1
      CALL zblpki
   ENDDO
   CALL bldpkn(Ipvct,0,mcb)
   CALL close(Ipvct,1)
   CALL wrttrl(mcb)
END SUBROUTINE ampb1
