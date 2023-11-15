
SUBROUTINE gfsmt(Mt,Mmat,Mrow)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4)
   INTEGER Irow , Sysbuf , Z(1)
   DOUBLE PRECISION Val
   COMMON /system/ Sysbuf
   COMMON /zblpkx/ A , Irow
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Mmat , Mrow , Mt
!
! Local variable declarations
!
   INTEGER i , ibuf1 , ibuf2 , icnt , inblk(15) , mcb(7) , mr , name(2) , nrow , nz , outblk(15)
   INTEGER korsz
!
! End of declarations
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
   EQUIVALENCE (Val,A(1))
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
   nz = korsz(Z(1))
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   nz = ibuf2 - 1
   IF ( nz<100 ) GOTO 200
!
!     OPEN FILES
!
   CALL makmcb(mcb,Mmat,nrow,1,2)
   inblk(1) = Mt
   outblk(1) = Mmat
   CALL gopen(Mt,Z(ibuf1),0)
   CALL gopen(Mmat,Z(ibuf2),1)
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
   Irow = Mrow
   Val = 1.0D0
   CALL zblpki
   CALL bldpkn(Mmat,0,mcb)
!
   IF ( Mrow<nrow ) THEN
      CALL fwdrec(*100,Mt)
!
!     BLAST OUT REST OF FILE
!
      CALL cpyfil(Mt,Mmat,Z,nz,icnt)
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
