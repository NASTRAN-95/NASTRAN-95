!*==hess2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hess2(Nrow,Iden,Ipv)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nrow
   INTEGER :: Iden
   INTEGER :: Ipv
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibuf1 , nz , sysbuf
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL close , gopen , korsz , makmcb , pack , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     HESS2  WILL GENERATE  AN IDENTITY MATRIX AND A PARTIIONING VECTOR
!
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf)
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
! ----------------------------------------------------------------------
!
   CALL makmcb(mcb,Iden,Nrow,8,1)
   nz = korsz(Z)
   ibuf1 = nz - sysbuf
   CALL gopen(Iden,iz(ibuf1),1)
   It1 = 1
   It2 = 1
   Incr = 1
   Z(1) = -1.0
   DO i = 1 , Nrow
      Ii = i
      Jj = i
      CALL pack(Z,Iden,mcb)
   ENDDO
   CALL close(Iden,1)
   CALL wrttrl(mcb)
!
!     BUILD PARTITIONING VECTOR
!
   CALL makmcb(mcb,Ipv,2*Nrow,2,1)
   CALL gopen(Ipv,iz(ibuf1),1)
   DO i = 1 , Nrow
      Z(i) = 1.0
   ENDDO
   Ii = Nrow + 1
   Jj = 2*Nrow
   CALL pack(Z,Ipv,mcb)
   CALL wrttrl(mcb)
   CALL close(Ipv,1)
END SUBROUTINE hess2
