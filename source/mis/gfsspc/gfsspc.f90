!*==gfsspc.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsspc(Nuy,Pvec)
   IMPLICIT NONE
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
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
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
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
   nz = korsz(z(1))
   ibuf = nz - sysbuf
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
   CALL gopen(Pvec,z(ibuf),1)
   CALL bldpk(1,1,Pvec,0,0)
   a(1) = 1.0
   irow = 1
   CALL zblpki
   CALL bldpkn(Pvec,0,mcb)
   CALL close(Pvec,1)
   CALL wrttrl(mcb)
99999 END SUBROUTINE gfsspc
