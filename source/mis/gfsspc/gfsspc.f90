!*==gfsspc.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsspc(Nuy,Pvec)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_ZBLPKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nuy
   INTEGER :: Pvec
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibuf , nuy1 , nz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
!     ROUTINE TO CALCULATE A PARTITIONING VECTOR TO REMOVE FIRST
!     ROW AND COLUMN OF FLUID STIFFNESS MATRIX IF NO SPC'S ARE ON
!     THE FLUID
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
   DATA name/4HGFSS , 4HPC  /
!
!     ALLOCATE CORE
!
   nz = korsz(Z(1))
   ibuf = nz - Sysbuf
   nz = ibuf - 1
   IF ( nz<0 ) THEN
!
!     ERRORS
!
      CALL mesage(-8,0,name)
      GOTO 99999
   ENDIF
!
   nuy1 = Nuy - 1
   CALL makmcb(mcb,Pvec,Nuy,2,1)
   CALL gopen(Pvec,Z(ibuf),1)
   CALL bldpk(1,1,Pvec,0,0)
   A(1) = 1.0
   Irow = 1
   CALL zblpki
   CALL bldpkn(Pvec,0,mcb)
   CALL close(Pvec,1)
   CALL wrttrl(mcb)
   RETURN
99999 END SUBROUTINE gfsspc
