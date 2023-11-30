
SUBROUTINE pla2
   IMPLICIT NONE
   REAL A(4) , Z(1)
   INTEGER Bufsz , Eol , Idummy , Incpk , Incupk , Index , Ipack , Iunpk , Iunpkb , Jpack , Junpk , Placnt , Typea , Typeb
   COMMON /blank / Placnt
   COMMON /packx / Typea , Typeb , Ipack , Jpack , Incpk
   COMMON /system/ Bufsz
   COMMON /unpakx/ Iunpkb , Iunpk , Junpk , Incupk
   COMMON /zntpkx/ A , Index , Eol , Idummy
   COMMON /zzzzzz/ Z
   INTEGER buffr1 , buffr2 , clsnrw , clsrw , eor , i , ifile , iloop , inblk(15) , inrw , iopt , izmax , jfile , ktype , left ,    &
         & mcb(7) , name(2) , nrecs , ofile , oublk(15) , outnrw , outrw
   REAL dummy(2)
   INTEGER korsz
!*****
! THIS ROUTINE IS THE SECOND FUNCTIONAL MODULE UNIQUE TO THE PIECE-WISE
! LINEAR ANALYSIS (PLA) RIGID FORMAT FOR THE DISPLACEMENT APPROACH.
!
! DMAP CALL...
!
! PLA2   DELTAUGV,DELTAPG,DELTAQG/UGV1,PGV1,QG1/V,N,PLACOUNT/ $
!
! CONCERNING DELTAUGV AND UGV1, THE ROUTINE WORKS AS FOLLOWS...
! DELTAUGV IS THE CURRENT INCREMENTAL DISPLACEMENT VECTOR IN THE PLA
! RIGID FORMAT LOOP AND UGV1 IS AN APPENDED FILE OF DISPLACEMENT VECTORS
! IF PLACOUNT .EQ. 1, THAT IS, THIS IS THE FIRST TIME PLA2 HAS BEEN
! CALLED IN THE PLA LOOP, THEN DELTAUGV IS COPIED ONTO UGV1.  IF
! PLACOUNT .GT. 1, THE PREVIOUS, OR LAST, DISPLACEMENT VECTOR IS READ
! INTO CORE FROM THE UGV1 DATA BLOCK, AND THE UGV1 FILE IS CLOSED WITH-
! OUT REWIND, THEN OPENED WITHOUT REWIND TO WRITE.  THE DELTAUGV VECTOR
! IS READ AN ELEMENT AT A TIME USING SUBROUTINE ZNTPKI AND ADDED TO
! THE VECTOR IN CORE.  THEN THE NEW DISPLACEMENT VECTOR IS WRITTEN ONTO
! THE UGV1 FILE.
!
! THEN THE PLA DMAP LOOP COUNTER PLACOUNT IS INCREMENTED.
!
! DELTAPG IS THE CURRENT INCREMENTAL LOAD VECTOR AND PGV1 IS THE
! CORRESPONDING MATRIX OF RUNNING SUM LOAD VECTORS.  PROCESSING IS
! SIMILAR TO THE ABOVE.  NOTE THAT NEITHER DATA BLOCK, LIKE THE TWO
! DISCUSSED ABOVE, CAN BE PURGED.
!
! DELTAQG IS THE CURRENT INCREMENTAL VECTOR OF SINGLE POINT CONSTRAINT
! FORCES AND QG1 IS THE APPENDED FILE OF VECTORS OF SPCF.  THESE TWO
! DATA BLOCKS ARE PROCESSED IDENTICALLY TO DELTAUGV AND UGV1 EXCECT
! THAT NO FATAL ERROR EXISTS IF ONE OR THE OTHER HAS BEEN PURGED.
!*****
!
!
!
   DATA name/4HPLA2 , 4H    /
   DATA inrw , outrw , outnrw , clsrw , clsnrw , eor/0 , 1 , 3 , 1 , 2 , 1/
!
! INITIALIZE
!
   izmax = korsz(Z)
   buffr1 = izmax - Bufsz
   buffr2 = buffr1 - Bufsz
   left = buffr2 - 1
   iloop = 1
   ifile = 101
   ofile = 201
!
! OPEN INPUT FILE TO READ AND OUTPUT FILE TO WRITE (IF PLACNT .EQ. 1)
! OR TO READ (IF PLACNT .GT. 1)
!
 100  jfile = ifile
   inblk(1) = ifile
   oublk(1) = ofile
   DO i = 2 , 7
      mcb(i) = 0
   ENDDO
   mcb(1) = ofile
   IF ( Placnt==1 ) mcb(1) = ifile
   CALL rdtrl(mcb)
   CALL open(*300,ifile,Z(buffr1),inrw)
   CALL fwdrec(*600,ifile)
   iopt = inrw
   IF ( Placnt==1 ) iopt = outrw
   CALL open(*400,ofile,Z(buffr2),iopt)
   IF ( Placnt/=1 ) THEN
!
! THIS IS NOT THE FIRST PASS
!
      jfile = ofile
      CALL fwdrec(*600,ofile)
      nrecs = Placnt - 2
      IF ( nrecs>0 ) THEN
         DO i = 1 , nrecs
            CALL fwdrec(*600,ofile)
         ENDDO
      ENDIF
      mcb(1) = ofile
      CALL rdtrl(mcb)
      mcb(6) = 0
      mcb(7) = 0
      IF ( left<mcb(3) ) CALL mesage(-8,0,name)
      Iunpkb = 1
      Iunpk = 1
      Junpk = mcb(3)
      Incupk = 1
      CALL unpack(*700,ofile,Z)
      CALL close(ofile,clsnrw)
      CALL open(*500,ofile,Z(buffr2),outnrw)
!
! READ THE INCREMENTAL VECTOR.  INTPK INITIALIZES AND ZNTPKI RETURNS
! ONE ELEMENT AT A TIME
!
      ktype = 1
      CALL intpk(*800,ifile,0,ktype,0)
      DO
!
! READ AND ADD LOOP.
!
         CALL zntpki
         Z(Index) = Z(Index) + A(1)
         IF ( Eol/=0 ) THEN
!
! ADDITION HAS BEEN COMPLETED
!
            CALL close(ifile,clsrw)
!
! WRITE VECTOR ON OUTPUT FILE IN PACKED FORMAT.
!
            Typea = 1
            Typeb = 1
            Ipack = 1
            Jpack = mcb(3)
            Incpk = 1
            CALL pack(Z,ofile,mcb)
            CALL close(ofile,clsrw)
            EXIT
         ENDIF
      ENDDO
   ELSE
!
! THIS IS THE FIRST TIME THROUGH THE PLA LOOP.  COPY THE VECTOR ON THE
! INPUT FILE ONTO THE OUTPUT FILE.
!
      CALL fname(ofile,dummy)
      CALL write(ofile,dummy,2,eor)
      CALL cpystr(inblk,oublk,0,0)
      CALL close(ofile,clsrw)
      CALL close(ifile,clsrw)
   ENDIF
!
! WRITE TRAILER
!
   mcb(1) = ofile
   CALL wrttrl(mcb)
 200  iloop = iloop + 1
   IF ( iloop>3 ) THEN
!
! INCREMENT THE PLA DMAP LOOP COUNTER
!
      Placnt = Placnt + 1
      RETURN
   ELSE
      ifile = ifile + 1
      ofile = ofile + 1
      GOTO 100
   ENDIF
 300  IF ( iloop==1 .OR. iloop==2 ) CALL mesage(-30,127,ifile)
   GOTO 200
 400  IF ( iloop==1 .OR. iloop==2 ) CALL mesage(-30,128,ofile)
   GOTO 200
!
! FATAL ERRORS
!
 500  CALL mesage(-1,jfile,name)
 600  CALL mesage(-2,jfile,name)
 700  CALL mesage(-30,129,iloop+200)
 800  CALL mesage(-30,130,iloop+100)
END SUBROUTINE pla2
