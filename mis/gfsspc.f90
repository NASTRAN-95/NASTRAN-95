
SUBROUTINE gfsspc(Nuy,Pvec)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4)
   INTEGER Irow , Sysbuf , Z(1)
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ A , Irow
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Nuy , Pvec
!
! Local variable declarations
!
   INTEGER ibuf , mcb(7) , name(2) , nuy1 , nz
   INTEGER korsz
!
! End of declarations
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
99999 RETURN
END SUBROUTINE gfsspc
