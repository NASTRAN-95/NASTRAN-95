
SUBROUTINE ampb1(Ipvct,Noh,Noe)
   IMPLICIT NONE
   REAL A(4) , Z(1)
   INTEGER Ii , Sysbuf
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ A , Ii
   COMMON /zzzzzz/ Z
   INTEGER Ipvct , Noe , Noh
   INTEGER i , ibuf1 , mcb(7)
   INTEGER korsz
!
!     THIS ROUTINE BUILDS A PARTITIONING VECTOR WHICH WILL APPEND NOE
!       TERM(OR COLUMNS)
!
!
!
!-----------------------------------------------------------------------
!
   ibuf1 = korsz(Z) - Sysbuf + 1
   CALL gopen(Ipvct,Z(ibuf1),1)
   CALL makmcb(mcb,Ipvct,Noh+Noe,2,1)
   CALL bldpk(1,1,Ipvct,0,0)
   Ii = Noh
   DO i = 1 , Noe
      A(1) = 1.0
      Ii = Ii + 1
      CALL zblpki
   ENDDO
   CALL bldpkn(Ipvct,0,mcb)
   CALL close(Ipvct,1)
   CALL wrttrl(mcb)
END SUBROUTINE ampb1