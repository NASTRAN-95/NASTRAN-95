
SUBROUTINE hess2(Nrow,Iden,Ipv)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ii , Incr , It1 , It2 , Iz(1) , Jj , Ksystm(65) , Sysbuf
   REAL Z(1)
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Iden , Ipv , Nrow
!
! Local variable declarations
!
   INTEGER i , ibuf1 , mcb(7) , nz
   INTEGER korsz
!
! End of declarations
!
!
!     HESS2  WILL GENERATE  AN IDENTITY MATRIX AND A PARTIIONING VECTOR
!
!
!
   EQUIVALENCE (Ksystm(1),Sysbuf)
   EQUIVALENCE (Z(1),Iz(1))
!
! ----------------------------------------------------------------------
!
   CALL makmcb(mcb,Iden,Nrow,8,1)
   nz = korsz(Z)
   ibuf1 = nz - Sysbuf
   CALL gopen(Iden,Iz(ibuf1),1)
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
   CALL gopen(Ipv,Iz(ibuf1),1)
   DO i = 1 , Nrow
      Z(i) = 1.0
   ENDDO
   Ii = Nrow + 1
   Jj = 2*Nrow
   CALL pack(Z,Ipv,mcb)
   CALL wrttrl(mcb)
   CALL close(Ipv,1)
END SUBROUTINE hess2
