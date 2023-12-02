!*==ta1h.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ta1h
   IMPLICIT NONE
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_system
   USE c_ta1ab
   USE c_ta1com
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: blk
   INTEGER , DIMENSION(50) :: buf
   INTEGER :: buf1 , buf2 , buf3 , elemid , flag , i , idcntr , idptr , iecpt0 , ielem , igr1 , igr2 , ii , infile , ix , izero ,   &
            & j , k , khr , kk , ktwo24 , l , last , length , lj , ll , llx , lx , m , maxdof , maxel , mm , n , n2 , n21 , ndx1 ,  &
            & ndx2 , neq1 , neq2 , ngrids , noect , op , oufile , outpt , scri , scro , sysbuf , tempid
   REAL , DIMENSION(50) :: bufr
   REAL :: file
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(3) :: out
   INTEGER , SAVE :: two24
   REAL , DIMENSION(1) :: zz
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     FOR LEVEL 16 A MAJOR REVISION HAS BEEN MADE TO TA1B. THE ECPT AND
!     GPCT ARE NO LONGER CONTSTRUCTED BUT, INSTEAD, THE GPECT IS BUILT.
!     THE GPECT IS ESSENTIALLY A TRUNCATED VERSION OF THE OLD ECPT. IT
!     CONTAINS ONE LOGICAL RECORD FOR EACH GRID OR SCALAR POINT IN THE
!     MODEL. EACH LOGICAL RECORD CONTAINS THE CONNECTION DATA FOR EACH
!     ELEMENT CONNECTED TO THE GRID POINT.
!
!ZZ   COMMON /ZZTAA2/ Z(1)
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(10),Tempid) , (buf(1),bufr(1)) , (Z(1),Zz(1)) , (blk(2),n)
   DATA nam/4HTA1H , 4H    / , two24/4194304/
!
!     PERFORM GENERAL INITIALIZATION
!
   n2 = 2*nelem - 1
   n21 = n2 + 1
   buf1 = korsz(z) - sysbuf - 2
   buf2 = buf1 - sysbuf
   buf3 = buf2 - sysbuf
!WKBR spr 93012      NEQ1 = NSIL + 1
   neq1 = nsil + 2
   neq2 = 0
!
!     THE GRID POINT COUNTER(GPC)HAS ONE ENTRY PER GRID OR SCALAR POINT
!     IN THE STRUCTURE. EACH ENTRY CONTAINS THE NUMBER OF STRUCTURAL
!     ELEMENTS CONNECTED TO THE POINT.
!
   DO i = 1 , nsil
      z(i+1) = 0
   ENDDO
!
!     OPEN THE ECT. INITIALIZE TO LOOP THRU BY ELEMENT TYPE.
!
   file = ect
   CALL gopen(ect,z(buf1),rdrew)
   noect = 1
!
!     IGNORE PLOTEL AND REACT ELEMENTS. OTHERWISE, LOCATE AN ELEMENT
!     TYPE. IF PRESENT, READ ALL ELEMENTS OF THAT TYPE AND INCREMENT
!     THE GPC ENTRY FOR EACH POINT TO WHICH THE ELEMENT IS CONNECTED.
!
 100  CALL ectloc(*200,ect,buf,i)
   noect = 0
   lx = elem(i+12)
   mm = lx + elem(i+9) - 1
   m = elem(i+5)
   DO
      CALL read(*1500,*100,ect,buf,m,0,flag)
      DO l = lx , mm
         k = buf(l)
         IF ( k/=0 ) z(k+1) = z(k+1) + 1
      ENDDO
   ENDDO
 200  IF ( noect/=0 ) GOTO 1800
!
!     REPLACE ENTRIES IN THE GPC BY A RUNNING SUM
!     THUS CREATING POINTERS INTO ECPT0
!     QUEUE WARNING MESSAGES FOR GRID PTS. WITH NO ELEMENTS CONNECTED.
!
   z(1) = 1
   maxel = 0
   DO i = 1 , nsil
      maxel = max0(maxel,z(i+1))
      IF ( z(i+1)/=0 ) GOTO 400
!
      j = 0
      IF ( neq2<0 ) GOTO 350
      IF ( neq2/=0 ) GOTO 300
      neq2 = -1
      z(neq1) = eqexin
      CALL rdtrl(z(neq1))
      IF ( z(neq1)<=0 ) GOTO 350
      file = eqexin
      CALL gopen(eqexin,z(buf2),rdrew)
      CALL read(*1400,*250,eqexin,z(neq1),buf3,1,neq2)
 250  CALL close(eqexin,clsrew)
      CALL sort(0,0,2,2,z(neq1),neq2)
 300  j = z((i-1)*2+neq1)
!
 350  buf(1) = i
      buf(2) = j
      CALL mesage(30,15,buf)
 400  z(i+1) = z(i) + z(i+1)
   ENDDO
!
!     DETERMINE BAND OF ENTRIES IN ECPT0 WHICH WILL FIT IN CORE
!     NDX1 = POINTER IN GPC TO 1ST  ENTRY FOR CURRENT PASS.
!     NDX2 = POINTER IN GPC TO LAST ENTRY FOR CURRENT PASS.
!
   ndx1 = 1
   ndx2 = nsil
   llx = 1
   iecpt0 = nsil + 2
   length = buf1 - iecpt0
   op = wrtrew
 500  DO WHILE ( z(ndx2+1)-z(ndx1)+2>length )
      ndx2 = ndx2 - 1
   ENDDO
!
!     PASS THE ECT. FOR EACH GRID PT IN RANGE ON THIS PASS,
!     STORE ELEMENT POINTER = 2**K * J + WORD POSITION IN ECT RECORD
!     WHERE K=22 FOR LEVEL 16 AND J = ENTRY NBR OF ELEMENT IN /GPTA1/
!     (WHICH IS SAME AS ELEMENT TYPE AS OF LEVEL 15)
!
   file = ect
   CALL gopen(ect,z(buf1),rdrew)
   izero = z(ndx1)
 600  CALL ectloc(*700,ect,buf,i)
   j = (i-1)/incr + 1
   idcntr = two24*j
   m = elem(i+5)
   lx = elem(i+12)
   mm = lx + elem(i+9) - 1
   DO
      CALL read(*1500,*600,ect,buf,m,0,flag)
      DO l = lx , mm
         k = buf(l)
         IF ( k>=ndx1 .AND. k<=ndx2 ) THEN
            ix = z(k) - izero + iecpt0
            z(ix) = idcntr
            z(k) = z(k) + 1
         ENDIF
      ENDDO
      idcntr = idcntr + m
   ENDDO
!
!     WRITE ECPT0 AND TEST FOR ADDITIONAL PASSES
!     ECPT0 CONTAINS ONE LOGICAL RECORD FOR EACH GRID OR SCALAR POINT.
!     EACH LOGICAL RECORD CONTAINS N PAIRS OF(-1,ELEMENT POINTER)WHERE
!     N= NUMBER OF ELEMENTS CONNECTED TO THE PIVOT.
!     IF NO ELEMENTS CONNECTED TO POINT, RECORD IS ONE WORD = 0.
!
 700  file = scr1
   CALL open(*1400,scr1,z(buf1),op)
   elemid = 1
   buf(1) = -1
   lj = iecpt0 - 1
   DO i = ndx1 , ndx2
      m = z(i) - llx
      IF ( m/=0 ) THEN
         DO j = 1 , m
            lj = lj + 1
            buf(2) = z(lj)
            CALL write(scr1,buf,2,0)
         ENDDO
         CALL write(scr1,0,0,1)
      ELSE
         CALL write(scr1,0,1,1)
      ENDIF
      llx = z(i)
   ENDDO
   IF ( ndx2>=nsil ) THEN
!
!     READ AS MUCH OF ECT AS CORE CAN HOLD
!     FIRST N21 CELLS OF CORE CONTAIN A POINTER TABLE WHICH HAS TWO
!     ENTRIES PER ELEMENT TYPE. 1ST ENTRY HAS POINTER TO 1ST WORD OF
!     ECT DATA IN CORE FOR AN ELEMENT TYPE  2ND ENTRY HAS WORD POSITION
!     IN ECT RECORD OF THAT TYPE FOR LAST ENTRY READ ON PREVIOUS PASS.
!
      CALL close(scr1,clsrew)
      scri = scr1
      scro = scr2
      file = ect
      CALL gopen(ect,z(buf1),rdrew)
      DO j = 1 , n21
         z(j) = 0
      ENDDO
      l = n21 + 1
   ELSE
      CALL close(scr1,cls)
      ndx1 = ndx2 + 1
      ndx2 = nsil
      op = wrt
      GOTO 500
   ENDIF
 800  CALL ectloc(*1000,ect,buf,ielem)
   i = 2*((ielem-1)/incr+1) - 1
   z(i) = l
   ll = 0
   m = elem(ielem+5)
   last = buf3 - m
 900  DO WHILE ( l<=last )
      CALL read(*1500,*800,ect,z(l),m,0,flag)
      z(l) = elemid
      elemid = elemid + 1
      l = l + m
      ll = ll + m
   ENDDO
!
!     PASS ECPT0 ENTRIES LINE BY LINE
!     ATTACH EACH REFERENCED ECT ENTRY WHICH IS NOW IN CORE
!
 1000 file = scri
   CALL open(*1400,scri,z(buf2),rdrew)
   CALL open(*1400,scro,z(buf3),wrtrew)
 1100 DO
      CALL read(*1300,*1200,scri,buf,1,0,flag)
      IF ( buf(1)<0 ) THEN
         CALL read(*1500,*1600,scri,buf(2),1,0,flag)
         khr = buf(2)/two24
         ktwo24 = khr*two24
         k = 2*khr - 1
         idptr = buf(2) - ktwo24
         kk = z(k) + idptr - z(k+1)
         IF ( z(k)==0 .OR. kk>last ) THEN
            CALL write(scro,buf,2,0)
         ELSE
            j = (khr-1)*incr + 1
            mm = elem(j+5)
            buf(1) = mm
            buf(2) = andf(z(kk),two24-1) + ktwo24
            CALL write(scro,buf,2,0)
            CALL write(scro,z(kk+1),mm-1,0)
         ENDIF
      ELSEIF ( buf(1)==0 ) THEN
         CALL write(scro,0,1,1)
         CALL fwdrec(*1500,scri)
      ELSE
         CALL read(*1500,*1600,scri,buf(2),buf(1),0,flag)
         CALL write(scro,buf,buf(1)+1,0)
      ENDIF
   ENDDO
 1200 CALL write(scro,0,0,1)
   GOTO 1100
!
!     TEST FOR COMPLETION OF STEP
!     IF INCOMPLETE, SET FOR NEXT PASS
!
 1300 CALL close(scri,clsrew)
   CALL close(scro,clsrew)
   IF ( ielem==0 ) THEN
!
!     READ THE SIL INTO CORE. OPEN ECPT0 AND GPECT.
!     WRITE HEADER RECORD ON GPECT - 3RD WORD = NO OF ENTRIES IN /GPTA1/
!
      file = sil
      CALL gopen(sil,z(buf1),rdrew)
      CALL fread(sil,z,nsil,1)
      z(nsil+1) = luset + 1
      CALL close(sil,clsrew)
      infile = scro
      oufile = gpect
      maxdof = 0
      file = infile
      CALL open(*1400,infile,z(buf1),rdrew)
      CALL open(*1400,oufile,z(buf2),wrtrew)
      CALL fname(oufile,buf)
      buf(3) = nelem
      CALL write(oufile,buf,3,1)
!
!     PASS ECPT0 LINE BY LINE. FOR EACH LINE -
!     1. CONVERT GRID NBRS TO SIL VALUES
!     2. SORT SIL NBRS AND DISCARD MISSING ONES
!     3. WRITE LINE ON GPECT
!
      DO ll = 1 , nsil
!
!     WRITE SIL AND DOF FOR PIVOT
!
         buf(1) = z(ll)
         buf(2) = z(ll+1) - z(ll)
         CALL write(oufile,buf,2,0)
         DO
!
!     READ AN ECT LINE FROM ECPT0. SET POINTERS AS A FUNCTION OF ELEM
!     TYPE.
!
            CALL read(*1500,*1320,infile,buf,1,0,flag)
            IF ( buf(1)<0 ) GOTO 1700
            IF ( buf(1)==0 ) THEN
!
!     HERE IF NO ELEMENTS CONNECTED TO PIVOT.
!
               CALL write(oufile,0,0,1)
               CALL fwdrec(*1600,infile)
               GOTO 1350
            ELSE
               CALL read(*1500,*1600,infile,buf(2),buf(1),0,flag)
               khr = buf(2)/two24
               ielem = (khr-1)*incr + 1
               ngrids = elem(ielem+9)
               igr1 = elem(ielem+12) + 1
               igr2 = igr1 + ngrids - 1
               maxel = 0
!
!     CONVERT GRID NUMBERS TO SIL VALUES. DISCARD ANY MISSING (ZERO)
!     GRID POINTS THEN SORT LIST ON SIL VALUE
!
               DO ii = igr1 , igr2
                  k = buf(ii)
                  IF ( k/=0 ) THEN
                     buf(ii) = z(k)
                     maxel = max0(maxel,z(k+1)-z(k))
                  ELSE
                     buf(ii) = 2147483647
                     ngrids = ngrids - 1
                  ENDIF
               ENDDO
               CALL sort(0,0,1,1,buf(igr1),elem(ielem+9))
               maxdof = max0(maxdof,ngrids*maxel)
!
!     WRITE A LINE ON GPECT.
!     - NUMBER OF WORDS IN ENTRY (NOT INCLUDING THIS WORD)
!       ELEMENT ID
!       ELEMENT TYPE
!       THE SORTED SIL LIST FOR THE GRID POINTS
!
               out(1) = -(ngrids+2)
               out(2) = buf(2) - khr*two24
               out(3) = elem(ielem+2)
               CALL write(oufile,out,3,0)
               CALL write(oufile,buf(igr1),ngrids,0)
            ENDIF
         ENDDO
!
!     HERE WHEN ALL ELEMENTS COMPLETE FOR CURRENT PIVOT
!
 1320    CALL write(oufile,0,0,1)
 1350 ENDDO
!
!     CLOSE FILES, WRITE TRAILER AND RETURN.
!
      CALL close(infile,clsrew)
      CALL close(oufile,clsrew)
      buf(1) = oufile
      buf(2) = nelem
      buf(3) = nsil
      buf(4) = maxel
      buf(5) = maxdof
      buf(6) = 0
      buf(7) = 0
      CALL wrttrl(buf)
      RETURN
   ELSE
      k = scri
      scri = scro
      scro = k
      l = n21 + 1
      DO j = 1 , n21
         z(j) = 0
      ENDDO
      z(i) = l
      z(i+1) = ll
      GOTO 900
   ENDIF
!
!     FATAL ERROR MESAGES
!
 1400 j = -1
   CALL mesage(j,file,nam)
   GOTO 99999
 1500 j = -2
   CALL mesage(j,file,nam)
   GOTO 99999
 1600 j = -3
   CALL mesage(j,file,nam)
   GOTO 99999
 1700 buf(1) = 0
   buf(2) = 0
   CALL mesage(-30,14,buf)
 1800 buf(1) = 0
   buf(2) = 0
   CALL mesage(-30,13,buf)
   buf(1) = tempid
   buf(2) = 0
   n = 44
   CALL mesage(-30,n,buf)
   CALL mesage(j,file,nam)
99999 END SUBROUTINE ta1h
