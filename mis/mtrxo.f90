
SUBROUTINE mtrxo(File,Name,Item,Dumbuf,Itest)
   IMPLICIT NONE
   INTEGER Blksiz , Buf(1) , Ihalf , Io , Ioblk , Ioitcd , Iolbn , Iomode , Iopbn , Ioptr , Iosind , Jhalf , Mach , Nbuf , Nout ,   &
         & Nstrn
   REAL Ditdum(6) , Sofdum(20)
   LOGICAL Mdiup
   CHARACTER*23 Ufm
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /sof   / Ditdum , Io , Iopbn , Iolbn , Iomode , Ioptr , Iosind , Ioitcd , Ioblk , Sofdum , Mdiup
   COMMON /sys   / Blksiz
   COMMON /system/ Nbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Nstrn
   REAL Dumbuf
   INTEGER File , Item , Itest
   INTEGER Name(2)
   INTEGER andf , ittype , locfx , lshift , orf
   INTEGER first , i , idisp , idle , ifetch , imdi , iopt , itm , iwrt , j , left , nmsbr(2) , oldbuf , trail(7)
   EXTERNAL andf , lshift , orf
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
   EQUIVALENCE (Buf(1),Nstrn)
   DATA nmsbr/4HMTRX , 4HO   /
   DATA iwrt/2/
   DATA ifetch/ - 2/
   DATA idle/0/
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
      GOTO 200
   ELSEIF ( File>0 ) THEN
!
!     CHECK IF NASTRAN FILE HAS BEEN PURGED
!
      trail(1) = File
      CALL rdtrl(trail)
      IF ( trail(1)<=0 ) GOTO 400
!
!     OPEN ITEM TO WRITE AND FETCH FIRST BLOCK FOR SOF
!
      Itest = 3
      CALL sfetch(Name(1),Item,ifetch,Itest)
      IF ( Itest/=3 ) GOTO 200
!
!     OPEN NASTRAN FILE
!     MAKE SURE BUFFER IS  DOUBLE WORD ALIGNED.
!
      idisp = locfx(Buf(Io-2)) - locfx(Nstrn)
      IF ( andf(idisp,1)/=0 ) Io = Io + 1
      iopt = 0
      CALL open(*400,File,Buf(Io-2),iopt)
!
!     ADJUST SOF BUFFER TO COINCIDE WITH GINO BUFFER
!
      oldbuf = Io
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
      CALL rdblk(*100,File,first,left)
      first = 0
      DO
!
!     WRITE OUT BLOCK IN BUFFER TO SOF AND OBTAIN A FREE SOF BLOCK
!
         CALL sofio(iwrt,Iopbn,Buf(Io-2))
         CALL getblk(Iopbn,j)
         IF ( j==-1 ) GOTO 300
         Iopbn = j
         Iolbn = Iolbn + 1
!
!     OBTAIN A NEW BLOCK FROM THE GINO FILE
!
         CALL rdblk(*100,File,first,left)
      ENDDO
   ELSE
!
!     THE MATRIX ITEM IS TO BE PESUDO WRITTEN
!
      Itest = 2
      CALL sfetch(Name(1),Item,ifetch,Itest)
      GOTO 200
   ENDIF
!
!     THE LAST BUFFER OF THE GINO FILE HAS BEEN FOUND - DETERMINE
!     IF SUFFICIENT SPACE IN BUFFER REMAINS FOR TRAILER
!
 100  IF ( left<6 ) THEN
!
!     INSUFFICIENT SPACE - OBTAIN NEW SOF BLOCK
!     SET BLOCK NUMBER OF CURRENT BLOCK TO ZERO TO INDICATE TRAILER
!     IS STORED IN NEXT BLOCK
!
      Buf(Io+1) = 0
      CALL sofio(iwrt,Iopbn,Buf(Io-2))
      CALL getblk(Iopbn,j)
      IF ( j==-1 ) GOTO 300
      Iopbn = j
      Iolbn = Iolbn + 1
   ENDIF
!
!     STORE TRAILER IN LAST SIX WORDS OF BLOCK
!     SET BLOCK NUMBER NEGATIVE TO INDICATE LAST BLOCK AND
!     WRITE OUT FINAL BLOCK TO SOF
!
   DO i = 2 , 7
      Buf(Io+Blksiz-7+i) = trail(i)
   ENDDO
   Buf(Io+1) = -Iolbn
   CALL sofio(iwrt,Iopbn,Buf(Io-2))
!
!     CLOSE FILE AND UPDATE MDI
!
   CALL close(File,1)
   CALL fmdi(Iosind,imdi)
   Buf(imdi+Ioitcd) = orf(andf(Ioblk,Jhalf),lshift(Iolbn,Ihalf))
   Mdiup = .TRUE.
!
!     RETURN
!
   Itest = 3
   Io = oldbuf
   Iomode = idle
 200  RETURN
!
!     THERE ARE NO MORE FREE BLOCKS ON THE SOF.
!     RETURN THE BLOCKS THAT HAVE BEEN USED SO FAR BY THE ITEM BEING
!     WRITTEN, CLOSE THE SOF AND ISSUE A FATAL MESSAGE
!
 300  CALL retblk(Ioblk)
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
   WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 6223, THERE ARE NO MORE FREE BLOCKS AVAILABLE ON ','THE SOF.')
   CALL mesage(-37,0,nmsbr)
!
!     GINO FILE PURGED
!
 400  Itest = 6
   GOTO 200
!
END SUBROUTINE mtrxo
