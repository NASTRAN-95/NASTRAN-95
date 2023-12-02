!*==mtrxo.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mtrxo(File,Name,Item,Dumbuf,Itest)
   USE c_machin
   USE c_sof
   USE c_sys
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
   INTEGER :: first , i , idisp , imdi , iopt , itm , j , left , oldbuf
   INTEGER , SAVE :: idle , ifetch , iwrt
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   INTEGER , DIMENSION(7) :: trail
   EXTERNAL andf , chkopn , close , fmdi , getblk , ittype , locfx , lshift , mesage , open , orf , rdblk , rdtrl , retblk ,        &
          & sfetch , sofcls , sofio
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COPIES MATRIX ITEM OF SUBSTRUCTURE NAME FROM THE NASTRAN FILE
!     TO THE SOF
!     ITEST VALUES RETURNED ARE
!        1 - ITEM ALREADY EXISTS ON THE SOF
!        2 - THE ITEM WAS PESUDO WRITTEN
!        3 - NORMAL RETURN
!        4 - NAME DOES NOT EXIST
!        5 - ITEM IS NOT ONE OF THE ALLOWABLE MATIX ITEMS
!        6 - NASTRAN FILE HAS BEEN PURGED
!
   !>>>>EQUIVALENCE (Buf(1),Nstrn)
   DATA nmsbr/4HMTRX , 4HO   /
   DATA iwrt/2/
   DATA ifetch/ - 2/
   DATA idle/0/
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
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( File>0 ) THEN
!
!     CHECK IF NASTRAN FILE HAS BEEN PURGED
!
            trail(1) = File
            CALL rdtrl(trail)
            IF ( trail(1)<=0 ) GOTO 40
!
!     OPEN ITEM TO WRITE AND FETCH FIRST BLOCK FOR SOF
!
            Itest = 3
            CALL sfetch(Name(1),Item,ifetch,Itest)
            IF ( Itest/=3 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     OPEN NASTRAN FILE
!     MAKE SURE BUFFER IS  DOUBLE WORD ALIGNED.
!
            idisp = locfx(buf(io-2)) - locfx(nstrn)
            IF ( andf(idisp,1)/=0 ) io = io + 1
            iopt = 0
            CALL open(*40,File,buf(io-2),iopt)
!
!     ADJUST SOF BUFFER TO COINCIDE WITH GINO BUFFER
!
            oldbuf = io
!
!IBMD 6/93 IF (MACH .GT. 2) GO TO 50
!IBMD 6/93 IF (BUF(IO-2) .EQ. FILE) GO TO 50
!IBMD 6/93 IO = IO + 1
!IBMD 6/93 IF (BUF(IO-2) .NE. FILE) GO TO 1000
!
!     BEGIN COPYING DATA TO SOF
!
!     FIRST CHECK IF CALL TO OPEN OBTAINED ONLY BLOCK IN FILE
!
            first = 1
            CALL rdblk(*20,File,first,left)
            first = 0
            DO
!
!     WRITE OUT BLOCK IN BUFFER TO SOF AND OBTAIN A FREE SOF BLOCK
!
               CALL sofio(iwrt,iopbn,buf(io-2))
               CALL getblk(iopbn,j)
               IF ( j==-1 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               iopbn = j
               iolbn = iolbn + 1
!
!     OBTAIN A NEW BLOCK FROM THE GINO FILE
!
               CALL rdblk(*20,File,first,left)
            ENDDO
         ELSE
!
!     THE MATRIX ITEM IS TO BE PESUDO WRITTEN
!
            Itest = 2
            CALL sfetch(Name(1),Item,ifetch,Itest)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THE LAST BUFFER OF THE GINO FILE HAS BEEN FOUND - DETERMINE
!     IF SUFFICIENT SPACE IN BUFFER REMAINS FOR TRAILER
!
 20      IF ( left<6 ) THEN
!
!     INSUFFICIENT SPACE - OBTAIN NEW SOF BLOCK
!     SET BLOCK NUMBER OF CURRENT BLOCK TO ZERO TO INDICATE TRAILER
!     IS STORED IN NEXT BLOCK
!
            buf(io+1) = 0
            CALL sofio(iwrt,iopbn,buf(io-2))
            CALL getblk(iopbn,j)
            IF ( j==-1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iopbn = j
            iolbn = iolbn + 1
         ENDIF
!
!     STORE TRAILER IN LAST SIX WORDS OF BLOCK
!     SET BLOCK NUMBER NEGATIVE TO INDICATE LAST BLOCK AND
!     WRITE OUT FINAL BLOCK TO SOF
!
         DO i = 2 , 7
            buf(io+blksiz-7+i) = trail(i)
         ENDDO
         buf(io+1) = -iolbn
         CALL sofio(iwrt,iopbn,buf(io-2))
!
!     CLOSE FILE AND UPDATE MDI
!
         CALL close(File,1)
         CALL fmdi(iosind,imdi)
         buf(imdi+ioitcd) = orf(andf(ioblk,jhalf),lshift(iolbn,ihalf))
         mdiup = .TRUE.
!
!     RETURN
!
         Itest = 3
         io = oldbuf
         iomode = idle
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
      CASE (3)
!
!     THERE ARE NO MORE FREE BLOCKS ON THE SOF.
!     RETURN THE BLOCKS THAT HAVE BEEN USED SO FAR BY THE ITEM BEING
!     WRITTEN, CLOSE THE SOF AND ISSUE A FATAL MESSAGE
!
         CALL retblk(ioblk)
         CALL sofcls
!
!     ERROR RETURNS
!
!
!     BUFFER ALIGNMENT ERROR
!
!IBMD 6/93 1000 CALL SOFCLS
!IBMD 6/93      CALL MESAGE (-8,0,NMSBR)
!IBMD 6/93      GO TO 100
!
!     NO MORE FREE BLOCKS ON THE SOF
!
         WRITE (nout,99001) ufm
99001    FORMAT (A23,' 6223, THERE ARE NO MORE FREE BLOCKS AVAILABLE ON ','THE SOF.')
         CALL mesage(-37,0,nmsbr)
!
!     GINO FILE PURGED
!
 40      Itest = 6
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mtrxo
