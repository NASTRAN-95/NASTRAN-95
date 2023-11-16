
SUBROUTINE mtrxi(File,Name,Item,Dumbuf,Itest)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Blksiz , Buf(1) , Ihalf , Io , Iolbn , Iomode , Iopbn , Jhalf , Mach , Nstrn
   REAL Ditdum(6)
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /sof   / Ditdum , Io , Iopbn , Iolbn , Iomode
   COMMON /sys   / Blksiz
   COMMON /zzzzzz/ Nstrn
!
! Dummy argument declarations
!
   REAL Dumbuf
   INTEGER File , Item , Itest
   INTEGER Name(2)
!
! Local variable declarations
!
   INTEGER andf , ittype , locfx , rshift
   INTEGER eof , i , idisp , idle , ifetch , ijump , in , inxt , iopt , ird , itm , itrail , next , nmsbr(2) , oldbuf , trail(7)
   EXTERNAL andf , rshift
!
! End of declarations
!
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
   EQUIVALENCE (Buf(1),Nstrn)
   DATA nmsbr/4HMTRX , 4HI   /
   DATA ird/1/ , idle/0/ , ifetch/ - 1/
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
      idisp = locfx(Buf(Io-2)) - locfx(Nstrn)
      IF ( andf(idisp,1)/=0 ) Io = Io + 1
      iopt = 1
      CALL open(*400,File,Buf(Io-2),iopt)
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
         GOTO 99999
      ELSE
!
!     INSERT CORRECT MATRIX NAME INTO BUFFER
!
         CALL fname(File,Buf(Io+in))
!
!     WRITE BLOCK ON NASTRAN FILE
!
         ASSIGN 100 TO ijump
         eof = 0
      ENDIF
   ENDIF
 100  IF ( Buf(Io+1)<=0 ) THEN
!
!     LAST DATA BLOCK HAS BEEN READ FROM SOF
!
      itrail = Buf(Io+1)
      Buf(Io+1) = Iolbn
      IF ( itrail<0 ) THEN
         trail(1) = File
         DO i = 2 , 7
            trail(i) = Buf(Io+Blksiz-7+i)
         ENDDO
         CALL wrttrl(trail)
      ENDIF
      eof = 1
      CALL wrtblk(File,eof)
      CALL close(File,1)
      IF ( itrail/=0 ) GOTO 300
!
!     TRAILER IS STORED ON NEXT SOF BLOCK - READ IT
!
      ASSIGN 200 TO ijump
   ELSE
      CALL wrtblk(File,eof)
   ENDIF
!
!     READ NEXT SOF BLOCK
!
   CALL fnxt(Iopbn,inxt)
   IF ( mod(Iopbn,2)==1 ) THEN
      next = andf(Buf(inxt),Jhalf)
   ELSE
      next = andf(rshift(Buf(inxt),Ihalf),Jhalf)
   ENDIF
   IF ( next==0 ) GOTO 500
   Iopbn = next
   Iolbn = Iolbn + 1
   CALL sofio(ird,Iopbn,Buf(Io-2))
   GOTO ijump
!
!     WRITE TRAILER OF NASTRAN DATA BLOCK
!
 200  trail(1) = File
   DO i = 2 , 7
      trail(i) = Buf(Io+Blksiz-7+i)
   ENDDO
   CALL wrttrl(trail)
 300  Itest = 1
   Io = oldbuf
   RETURN
!
!     ERROR RETURNS
!
!
!     NASTRAN FILE PURGED
!
 400  Itest = 6
   Iomode = idle
   RETURN
!
!     BUFFER ALIGNMENT ERROR
!
!IBMD 6/93 1010 CALL SOFCLS
!IBMD 6/93 CALL MESAGE (-8,0,NMSBR)
!
!     SOF CHAINING ERROR
!
 500  CALL errmkn(19,9)
   RETURN
!
99999 RETURN
END SUBROUTINE mtrxi
