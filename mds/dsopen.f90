
SUBROUTINE dsopen(Dsname,Iunit,Iop)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
!
! Dummy argument declarations
!
   CHARACTER*80 Dsname
   INTEGER Iop , Iunit
!
! Local variable declarations
!
   INTEGER iccer
!
! End of declarations
!
!      print *,' dsopen,iunit,iop,dsname=',iunit,iop,dsname
   IF ( Iop/=1 ) CALL dsopff(Dsname,Iunit,iccer)
   IF ( Iop==1 ) CALL dsocff(Dsname,Iunit,iccer)
   Numopn = Numopn + 1
END SUBROUTINE dsopen
