!*==gfsh.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsh(Nuy,H)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nuy
   INTEGER :: H
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibuf , nuy1 , nz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(2) :: rz
!
! End of declarations rewritten by SPAG
!
!
!     ROUTINE TO CALCULTE THE H TRANSFORMATION MATRIX USED WHEN NO
!     SPC'S ARE ON THE FLUID
!
!
!
!     OPEN CORE
!
!
!     SYSTEM COMMON
!
!
!     PACK - UNPACK COMMON BLOCKS
!
!
   !>>>>EQUIVALENCE (Z(1),Rz(1))
!
   DATA name/4HGFSH , 4H    /
!
!     ALLOCATE CORE
!
   nz = korsz(Z(1))
   ibuf = nz - Sysbuf
   nz = ibuf - 1
   IF ( nz<Nuy ) THEN
!
!     ERRORS
!
      CALL mesage(-8,0,name)
      GOTO 99999
   ENDIF
   nuy1 = Nuy - 1
   CALL makmcb(mcb,H,nuy1,2,2)
   Ti1 = 1
   To1 = 2
   I1 = 1
   N1 = nuy1
   Incr1 = 1
!
   DO i = 1 , Nuy
      rz(i) = -1.0/float(Nuy)
   ENDDO
   CALL gopen(H,Z(ibuf),1)
   DO i = 1 , Nuy
      rz(i) = float(nuy1)/float(Nuy)
      CALL pack(rz(2),H,mcb)
      rz(i) = -1.0/float(Nuy)
   ENDDO
   CALL close(H,1)
   CALL wrttrl(mcb)
   RETURN
99999 END SUBROUTINE gfsh
