!*==hess2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hess2(Nrow,Iden,Ipv)
   USE c_packx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   nz = korsz(z)
   ibuf1 = nz - sysbuf
   CALL gopen(Iden,iz(ibuf1),1)
   it1 = 1
   it2 = 1
   incr = 1
   z(1) = -1.0
   DO i = 1 , Nrow
      ii = i
      jj = i
      CALL pack(z,Iden,mcb)
   ENDDO
   CALL close(Iden,1)
   CALL wrttrl(mcb)
!
!     BUILD PARTITIONING VECTOR
!
   CALL makmcb(mcb,Ipv,2*Nrow,2,1)
   CALL gopen(Ipv,iz(ibuf1),1)
   DO i = 1 , Nrow
      z(i) = 1.0
   ENDDO
   ii = Nrow + 1
   jj = 2*Nrow
   CALL pack(z,Ipv,mcb)
   CALL wrttrl(mcb)
   CALL close(Ipv,1)
END SUBROUTINE hess2
