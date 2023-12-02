!*==dsopen.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsopen(Dsname,Iunit,Iop)
   USE I_DSIOF
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   CHARACTER*80 Dsname
   INTEGER Iop , Iunit
   INTEGER iccer
!      print *,' dsopen,iunit,iop,dsname=',iunit,iop,dsname
   IF ( Iop/=1 ) CALL dsopff(Dsname,Iunit,iccer)
   IF ( Iop==1 ) CALL dsocff(Dsname,Iunit,iccer)
   numopn = numopn + 1
END SUBROUTINE dsopen
