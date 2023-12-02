!*==gfsmt.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsmt(Mt,Mmat,Mrow)
   IMPLICIT NONE
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mt
   INTEGER :: Mmat
   INTEGER :: Mrow
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibuf1 , ibuf2 , icnt , mr , nrow , nz
   INTEGER , DIMENSION(15) , SAVE :: inblk , outblk
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL*8 :: val
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
!     ROUTINE TO ADD 1.0 TO ROW MROW AND COLUMN MROW OF MT TO PREVENT
!     SINGULARITIES IN THE MASS MATRIX FOR GIVINS
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
   !>>>>EQUIVALENCE (Val,A(1))
!
   DATA name/4HGFSM , 4HT   /
   DATA inblk/15*0/ , outblk/15*0/
!
!
   mcb(1) = Mt
   CALL rdtrl(mcb)
   nrow = mcb(2)
!
!     ALLOCATE BUFFERS
!
   nz = korsz(z(1))
   ibuf1 = nz - sysbuf
   ibuf2 = ibuf1 - sysbuf
   nz = ibuf2 - 1
   IF ( nz<100 ) GOTO 200
!
!     OPEN FILES
!
   CALL makmcb(mcb,Mmat,nrow,1,2)
   inblk(1) = Mt
   outblk(1) = Mmat
   CALL gopen(Mt,z(ibuf1),0)
   CALL gopen(Mmat,z(ibuf2),1)
!
!     COPY RECORDS UP TO MROW
!
   IF ( Mrow/=1 ) THEN
      mr = Mrow - 1
      DO i = 1 , mr
         CALL cpystr(inblk,outblk,0,0)
      ENDDO
   ENDIF
!
!    PACK OUT COLUMN MROW WITH A 1.0 IN ROW MROW.  THE COLUMN IS NULL
!     IN MT SO IT IS SKIPPED
!
   CALL bldpk(2,2,Mmat,0,0)
   irow = Mrow
   val = 1.0D0
   CALL zblpki
   CALL bldpkn(Mmat,0,mcb)
!
   IF ( Mrow<nrow ) THEN
      CALL fwdrec(*100,Mt)
!
!     BLAST OUT REST OF FILE
!
      CALL cpyfil(Mt,Mmat,z,nz,icnt)
   ENDIF
!
!     CLOSE FILES
!
   CALL close(Mt,1)
   CALL close(Mmat,1)
!
!     COPY TRAILER OVER.  THE DENSITY WILL BE SLIGHTLY OFF BECAUSE
!     OF THE NEW TERM BUT IT:S CLOSE
!
   mcb(1) = Mt
   CALL rdtrl(mcb)
   mcb(1) = Mmat
   CALL wrttrl(mcb)
   RETURN
!
!     ERRORS
!
 100  CALL mesage(-2,Mt,name)
 200  CALL mesage(-8,0,name)
END SUBROUTINE gfsmt
