!*==gfsh.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsh(Nuy,H)
   IMPLICIT NONE
   USE c_packx
   USE c_system
   USE c_zzzzzz
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
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
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
   nz = korsz(z(1))
   ibuf = nz - sysbuf
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
   ti1 = 1
   to1 = 2
   i1 = 1
   n1 = nuy1
   incr1 = 1
!
   DO i = 1 , Nuy
      rz(i) = -1.0/float(Nuy)
   ENDDO
   CALL gopen(H,z(ibuf),1)
   DO i = 1 , Nuy
      rz(i) = float(nuy1)/float(Nuy)
      CALL pack(rz(2),H,mcb)
      rz(i) = -1.0/float(Nuy)
   ENDDO
   CALL close(H,1)
   CALL wrttrl(mcb)
99999 END SUBROUTINE gfsh
