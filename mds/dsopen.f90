
SUBROUTINE dsopen(Dsname,Iunit,Iop)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   CHARACTER*80 Dsname
   INTEGER Iop , Iunit
   INTEGER iccer
!      print *,' dsopen,iunit,iop,dsname=',iunit,iop,dsname
   IF ( Iop/=1 ) CALL dsopff(Dsname,Iunit,iccer)
   IF ( Iop==1 ) CALL dsocff(Dsname,Iunit,iccer)
   Numopn = Numopn + 1
END SUBROUTINE dsopen