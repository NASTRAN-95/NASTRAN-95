!*==mtrxi.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mtrxi(File,Name,Item,Dumbuf,Itest)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SOF
   USE C_SYS
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: Item
   REAL :: Dumbuf
   INTEGER :: Itest
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: buf
   INTEGER :: eof , i , idisp , ijump , in , inxt , iopt , itm , itrail , next , oldbuf
   INTEGER , SAVE :: idle , ifetch , ird
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   INTEGER , DIMENSION(7) :: trail
   EXTERNAL andf , chkopn , close , errmkn , fname , fnxt , ittype , locfx , open , rshift , sfetch , sofio , wrtblk , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COPIES MATRIX ITEM OF SUBSTRUCTURE NAME FROM THE SOF TO THE
!     NASTRAN FILE
!     ITEST VALUES RETURNED ARE
!        1 - NORMAL RETURN
!        2 - ITEM PSEUDO-EXISTS ONLY ON THE SOF
!        3 - ITEM DOES NOT EXIST ON THE SOF
!        4 - NAME DOES NOT EXIT
!        5 - ITEM IS NOT ONE OF THE ALLOWABLE MATRIX ITEMS
!        6 - THE NASTRAN FILE HAS BEEN PURGED
!
   !>>>>EQUIVALENCE (Buf(1),Nstrn)
   DATA nmsbr/4HMTRX , 4HI   /
   DATA ird/1/ , idle/0/ , ifetch/ - 1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK IF ITEM IS ONE OF THE FOLLOWING ALLOWABLE NAMES.
!     KMTX, MMTX, PVEC, POVE, UPRT, HORG, UVEC, QVEC, PAPP, POAP, LMTX
!
         CALL chkopn(nmsbr(1))
         itm = ittype(Item)
         IF ( itm/=1 ) THEN
!
!     INVALID ITEM NAME
!
            Itest = 5
            RETURN
         ELSE
!
!     MAKE SURE BUFFER IS DOUBLE WORD ALIGNED, OPEN NASTRAN FILE, AND
!     ADUST SOF BUFFER TO COINCIDE WITH GINO
!     ALSO DETERMINE PLACEMENT OF MATRIX NAME IN FIRST BUFFER
!
            idisp = locfx(buf(Io-2)) - locfx(Nstrn)
            IF ( andf(idisp,1)/=0 ) Io = Io + 1
            iopt = 1
            CALL open(*60,File,buf(Io-2),iopt)
            oldbuf = Io
!
            in = 4
            IF ( Mach<=2 ) in = 7
!IBMD 6/93 IF (BUF(IO-2) .EQ. FILE) GO TO 40
!IBMD 6/93 IO = IO + 1
!IBMD 6/93 IF (BUF(IO-2) .NE. FILE) GO TO 1010
!
!     OPEN ITEM TO READ AND FETCH FIRST BLOCK FROM SOF
!
            CALL sfetch(Name(1),Item,ifetch,Itest)
            IF ( Itest/=1 ) THEN
!
!     ERROR IN SFETCH CALL
!
               CALL close(File,1)
               Io = oldbuf
               RETURN
            ELSE
!
!     INSERT CORRECT MATRIX NAME INTO BUFFER
!
               CALL fname(File,buf(Io+in))
!
!     WRITE BLOCK ON NASTRAN FILE
!
               ASSIGN 20 TO ijump
               eof = 0
            ENDIF
         ENDIF
 20      IF ( buf(Io+1)<=0 ) THEN
!
!     LAST DATA BLOCK HAS BEEN READ FROM SOF
!
            itrail = buf(Io+1)
            buf(Io+1) = Iolbn
            IF ( itrail<0 ) THEN
               trail(1) = File
               DO i = 2 , 7
                  trail(i) = buf(Io+Blksiz-7+i)
               ENDDO
               CALL wrttrl(trail)
            ENDIF
            eof = 1
            CALL wrtblk(File,eof)
            CALL close(File,1)
            IF ( itrail/=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     TRAILER IS STORED ON NEXT SOF BLOCK - READ IT
!
            ASSIGN 40 TO ijump
         ELSE
            CALL wrtblk(File,eof)
         ENDIF
!
!     READ NEXT SOF BLOCK
!
         CALL fnxt(Iopbn,inxt)
         IF ( mod(Iopbn,2)==1 ) THEN
            next = andf(buf(inxt),Jhalf)
         ELSE
            next = andf(rshift(buf(inxt),Ihalf),Jhalf)
         ENDIF
         IF ( next==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Iopbn = next
         Iolbn = Iolbn + 1
         CALL sofio(ird,Iopbn,buf(Io-2))
         GOTO ijump
!
!     WRITE TRAILER OF NASTRAN DATA BLOCK
!
 40      trail(1) = File
         DO i = 2 , 7
            trail(i) = buf(Io+Blksiz-7+i)
         ENDDO
         CALL wrttrl(trail)
         spag_nextblock_1 = 2
      CASE (2)
         Itest = 1
         Io = oldbuf
         RETURN
!
!     ERROR RETURNS
!
!
!     NASTRAN FILE PURGED
!
 60      Itest = 6
         Iomode = idle
         RETURN
      CASE (3)
!
!     BUFFER ALIGNMENT ERROR
!
!IBMD 6/93 1010 CALL SOFCLS
!IBMD 6/93 CALL MESAGE (-8,0,NMSBR)
!
!     SOF CHAINING ERROR
!
         CALL errmkn(19,9)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mtrxi
