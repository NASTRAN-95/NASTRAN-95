!*==pla2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla2
   USE c_blank
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buffr1 , buffr2 , i , ifile , iloop , iopt , izmax , jfile , ktype , left , nrecs , ofile
   INTEGER , SAVE :: clsnrw , clsrw , eor , inrw , outnrw , outrw
   REAL , DIMENSION(2) :: dummy
   INTEGER , DIMENSION(15) :: inblk , oublk
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , cpystr , fname , fwdrec , intpk , korsz , mesage , open , pack , rdtrl , unpack , write , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! INITIALIZE
!
         izmax = korsz(z)
         buffr1 = izmax - bufsz
         buffr2 = buffr1 - bufsz
         left = buffr2 - 1
         iloop = 1
         ifile = 101
         ofile = 201
         spag_nextblock_1 = 2
      CASE (2)
!
! OPEN INPUT FILE TO READ AND OUTPUT FILE TO WRITE (IF PLACNT .EQ. 1)
! OR TO READ (IF PLACNT .GT. 1)
!
         jfile = ifile
         inblk(1) = ifile
         oublk(1) = ofile
         DO i = 2 , 7
            mcb(i) = 0
         ENDDO
         mcb(1) = ofile
         IF ( placnt==1 ) mcb(1) = ifile
         CALL rdtrl(mcb)
         CALL open(*20,ifile,z(buffr1),inrw)
         CALL fwdrec(*80,ifile)
         iopt = inrw
         IF ( placnt==1 ) iopt = outrw
         CALL open(*40,ofile,z(buffr2),iopt)
         IF ( placnt/=1 ) THEN
!
! THIS IS NOT THE FIRST PASS
!
            jfile = ofile
            CALL fwdrec(*80,ofile)
            nrecs = placnt - 2
            IF ( nrecs>0 ) THEN
               DO i = 1 , nrecs
                  CALL fwdrec(*80,ofile)
               ENDDO
            ENDIF
            mcb(1) = ofile
            CALL rdtrl(mcb)
            mcb(6) = 0
            mcb(7) = 0
            IF ( left<mcb(3) ) CALL mesage(-8,0,name)
            iunpkb = 1
            iunpk = 1
            junpk = mcb(3)
            incupk = 1
            CALL unpack(*100,ofile,z)
            CALL close(ofile,clsnrw)
            CALL open(*60,ofile,z(buffr2),outnrw)
!
! READ THE INCREMENTAL VECTOR.  INTPK INITIALIZES AND ZNTPKI RETURNS
! ONE ELEMENT AT A TIME
!
            ktype = 1
            CALL intpk(*120,ifile,0,ktype,0)
            SPAG_Loop_1_1: DO
!
! READ AND ADD LOOP.
!
               CALL zntpki
               z(index) = z(index) + a(1)
               IF ( eol/=0 ) THEN
!
! ADDITION HAS BEEN COMPLETED
!
                  CALL close(ifile,clsrw)
!
! WRITE VECTOR ON OUTPUT FILE IN PACKED FORMAT.
!
                  typea = 1
                  typeb = 1
                  ipack = 1
                  jpack = mcb(3)
                  incpk = 1
                  CALL pack(z,ofile,mcb)
                  CALL close(ofile,clsrw)
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
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
         spag_nextblock_1 = 3
      CASE (3)
         iloop = iloop + 1
         IF ( iloop>3 ) THEN
!
! INCREMENT THE PLA DMAP LOOP COUNTER
!
            placnt = placnt + 1
            RETURN
         ELSE
            ifile = ifile + 1
            ofile = ofile + 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      IF ( iloop==1 .OR. iloop==2 ) CALL mesage(-30,127,ifile)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      IF ( iloop==1 .OR. iloop==2 ) CALL mesage(-30,128,ofile)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
! FATAL ERRORS
!
 60      CALL mesage(-1,jfile,name)
 80      CALL mesage(-2,jfile,name)
 100     CALL mesage(-30,129,iloop+200)
 120     CALL mesage(-30,130,iloop+100)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pla2
