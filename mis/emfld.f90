
SUBROUTINE emfld
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Hluset , Ii , Incr , Incur , Iz(1) , Last , Ne(1) , Nelems , Nn , Otpe , Sysbuf , Typout
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Z(1)
   COMMON /blank / Hluset
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /system/ Sysbuf , Otpe
   COMMON /unpakx/ Typout , Ii , Nn , Incur
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , casecc , cstm , dit , elid , eltype , estfld , file , geom1 , hccen , hcount , &
         & hest , hoef1 , hoeh1 , i , iany , ibuf(150) , icoord(4) , idum(2) , iel , ielid , iex , ifield , ipts , isub , itype ,   &
         & iwords , j , kcount , lcc , lcore , lsym , mcb(7) , mpt , n , nall , nall2 , nam(2) , ncount , ncstm , nelx , nelx2 ,    &
         & nextp , nextr , nextz , ngrids , nncr , nrows , nrows2 , nwords , oldcas , oldeid , remfl , strspt , subcas
   REAL coef , coord(4) , hc(3) , hex1 , hex2 , hex3 , hm(3) , hmg(3) , rbuf(150) , ta(9) , temp(3) , zn(2)
   INTEGER korsz , numtyp
!
! End of declarations
!
!
!                                            SEE T01191A     ===========
!     COMPUTES TOTAL MAGNETIC FIELD STRENGTH AND INDUCTION FOR
!     EACH ELEMENT IN BASIC COORDINATES BY ADDING HM AND HC
!
!     EMFLD    HOEF1,HEST,CASECC,HCFLD,MPT,DIT,REMFLD,GEOM1,CSTM,HCCEN/
!              HOEH1/V,N,HLUSET $
!
   EQUIVALENCE (rbuf(1),ibuf(1)) , (coord(1),icoord(1)) , (Z(1),Iz(1))
   DATA hoef1 , hest , casecc , mpt , dit/101 , 102 , 103 , 105 , 106/
   DATA remfl , geom1 , cstm , hccen/107 , 108 , 109 , 110/
   DATA estfld , hoeh1/301 , 201/
   DATA nam/4HEMFL , 4HD   / , zn/4HHOEH , 4H1   /
   DATA hex1 , hex2 , hex3/4HHEX1 , 4HHEX2 , 4HHEX3/
!
!     CHECK TO SEE IF HOEF1 EXISTS. IF NOT, THEN NO MAG. FIELD REQUESTS
!
   mcb(1) = hoef1
   CALL rdtrl(mcb)
   IF ( mcb(1)<0 ) GOTO 99999
   mcb(1) = hccen
   CALL rdtrl(mcb)
   Nn = mcb(3)
   IF ( mcb(1)<=0 ) THEN
      Nn = 0
      mcb(1) = remfl
      CALL rdtrl(mcb)
      IF ( mcb(1)<=0 ) THEN
         WRITE (Otpe,99001) Uwm
99001    FORMAT (A25,', DATA BLOCKS HCFLD AND REMFL ARE PURGED IN EM ','PROBLEM. ALL RESULTS ARE ZERO')
         GOTO 99999
      ENDIF
   ENDIF
!
   mcb(1) = hest
   CALL rdtrl(mcb)
   nelx = 3*mcb(2)
!
   Typout = 1
   Ii = 1
   Incur = 1
!
!     CREATE ESTFLD WHICH LOOKS LIKE HEST BUT CONTAINS ONLY TYPE, ID,
!     NUMBER OF SILS,SILS,3 X 3 MATERAIL MATRIX,AND 3 X 3 TRANSFORMATION
!     MATRIX FROM LOCAL TO BASIC,BFIELD,AND COORDS OF STRESS POINT FOR
!     NON-RECTANGULAR BFIELD
!
   CALL estmag(hest,estfld,mpt,dit,geom1,iany,kcount)
!
!     KCOUNT SHOULD BE NUMBER OF TERMS IN ROW OF HCCEN
!
   IF ( Nn==0 ) Nn = kcount
   IF ( Nn/=kcount ) THEN
!
!     FATAL ERROR MESSAGES
!
      WRITE (Otpe,99002) Sfm
99002 FORMAT (A25,', ROW COUNT ON HCCEN IN EMFLD IS NOT CONSISTENT')
      CALL mesage(-61,0,0)
      GOTO 1700
   ELSE
      nrows = Nn
!
!     NOW FETCH HC AT EACH POINT FROM HCFLD
!
      lcore = korsz(Z)
      buf1 = lcore - Sysbuf + 1
      buf2 = buf1 - Sysbuf
      buf3 = buf2 - Sysbuf
      buf4 = buf3 - Sysbuf
      buf5 = buf4 - Sysbuf
      buf6 = buf5 - Sysbuf
      lcore = buf6 - 1
      IF ( lcore>0 ) THEN
!
         ncount = 0
         oldcas = 0
         hcount = 0
!
!     COPY HEADER FROM HOEF1 TO HOEH1
!
         file = hoeh1
         CALL open(*1700,hoeh1,Z(buf5),1)
         file = hoef1
         CALL open(*1700,hoef1,Z(buf4),0)
         CALL read(*1800,*100,hoef1,Z,lcore,0,iwords)
      ENDIF
      GOTO 2000
   ENDIF
 100  Z(1) = zn(1)
   i = 2
   Z(i) = zn(2)
   CALL write(hoeh1,Z,iwords,1)
!
!     OPEN CSTM FOR NON-BASIC COORDINATE SYSTEM
!
   ncstm = 0
   IF ( iany==0 ) GOTO 300
   CALL gopen(cstm,Z(buf1),0)
   CALL read(*1800,*200,cstm,Z,lcore,0,ncstm)
   GOTO 2000
 200  CALL close(cstm,1)
   CALL pretrs(Z(1),ncstm)
!
 300  nncr = ncstm + nrows
   nall = nncr + nelx
   CALL gopen(casecc,Z(buf1),0)
   CALL gopen(hccen,Z(buf2),0)
   CALL gopen(estfld,Z(buf3),0)
   CALL gopen(remfl,Z(buf6),0)
!
!     READ ID RECORD FROM HOEF1. COPY TO HOEH1, EXCEPT CHANGE NUMBER OF
!     WORDS FROM +9 TO -9 AS AN INDICATOR FOR TITLES IN OFP. (+9 IS FOR
!     HEAT TRANSFER) ALSO PICK UP SUBCASE NUMBER AND ELEMENT TYPE. IF
!     SAME SUBCASE AS PREVIUUS ONE, USE SAME HCFLD VECTOR. IF NOT,
!     CREATE A NEW ONE
!
 400  CALL read(*1600,*1900,hoef1,ibuf,146,1,iwords)
   ibuf(10) = -9
   CALL write(hoeh1,ibuf,146,1)
   eltype = ibuf(3)
   subcas = ibuf(4)
   oldeid = 0
   IF ( subcas==oldcas ) GOTO 1100
   oldcas = subcas
   ncount = 0
   hcount = 0
   CALL rewind(estfld)
   file = estfld
   CALL fwdrec(*1800,estfld)
!
!     IF THIS SUBCASE IS NOT A SUBCOM, UNPACK NEXT COLUMN OF HCFLD. IF
!     IT IS A SUBCOM, BCKREC HCFLD THE SAME NUMBER OF RECORDS AS THERE
!     ARE FACTORS ON THE SUBSEQ AND COMBINE VECTORS TO PRODUCE ONE
!     VECTOR.
!
   IF ( lcore<16 ) GOTO 2000
   DO
      file = casecc
      CALL read(*1800,*1900,casecc,Z(ncstm+1),16,0,iwords)
      IF ( Iz(ncstm+1)==subcas ) THEN
!
!     MATCH ON SUBCASE ID. SEE HOW LONG THE RECORD IS
!
         IF ( Iz(ncstm+16)==0 ) GOTO 700
!
!     SUBCOM UNLESS IZ(16).LT.0. IN WHICH CASE IT IS A REPEAT SUBCASE
!
         IF ( Iz(ncstm+16)>0 ) THEN
!
!     SUBCOM. GET NUMBER OF FACTORS AND BCKREC THAT MANY RECORDS ON
!     HCFLD
!
!     OPEN CORE (AFTER NCSTM WORDS OF CSTM)
!             1 - NEXTZ   CASECC
!             NEXTZ+1 - NEXTZ+NROWS   COLUMN OF HCCEN
!             NEXTZ+NROWS+1 - NEXTZ+2*NROWS=NEXTP  HCCEN COMBINATION
!             NEXTP+1 - NEXTP+NELX  COLUMN OF REMFL
!             NEXTP+NELX+1 - NEXTP+2*NELX  REMFL COMBINATION
!
            CALL read(*1800,*500,casecc,Z(ncstm+17),lcore,0,iwords)
            GOTO 2000
         ELSE
            CALL bckrec(hccen)
            CALL bckrec(remfl)
            GOTO 700
         ENDIF
      ELSE
         CALL fwdrec(*1800,casecc)
         file = hccen
         CALL fwdrec(*1800,hccen)
         file = remfl
         CALL fwdrec(*1800,remfl)
      ENDIF
   ENDDO
 500  lcc = Iz(ncstm+166)
   lsym = Iz(ncstm+lcc)
   DO i = 1 , lsym
      CALL bckrec(hccen)
      CALL bckrec(remfl)
   ENDDO
   nextz = iwords + 16 + ncstm
   nrows2 = 2*nrows
   nextr = nextz + nrows
   nelx2 = 2*nelx
   nall2 = nrows2 + nelx2
   nextp = nextz + nrows2
   isub = nextp + nelx
   IF ( nextz+nall2>lcore ) GOTO 2000
!
!     SET UP FOR SUBSEQ
!
   DO i = 1 , nall2
      Z(nextz+i) = 0.
   ENDDO
   DO i = 1 , lsym
      coef = Z(ncstm+lcc+i)
      IF ( coef==0. ) THEN
!
!     COEF = 0.
!
         file = hccen
         CALL fwdrec(*1800,hccen)
         file = remfl
         CALL fwdrec(*1800,remfl)
         CYCLE
      ELSE
         Nn = nrows
         CALL unpack(*550,hccen,Z(nextz+1))
         DO j = 1 , nrows
            Z(nextr+j) = Z(nextr+j) + coef*Z(nextz+j)
         ENDDO
      ENDIF
 550  Nn = nelx
      CALL unpack(*600,remfl,Z(nextp+1))
      DO j = 1 , nelx
         Z(isub+j) = Z(isub+j) + coef*Z(nextp+j)
      ENDDO
 600  ENDDO
!
!     MOVE THE VECTOR IN CORE
!
   DO i = 1 , nrows
      Z(ncstm+i) = Z(nextr+i)
   ENDDO
   DO i = 1 , nelx
      Z(nncr+i) = Z(isub+i)
   ENDDO
   GOTO 1100
!
!     NOT A SUBCOM
!     UNPACK A COLUMN OF HCFLD. FIRST SKIP TO NEXT RECORD ON CASECC
!
 700  file = casecc
   CALL fwdrec(*1800,casecc)
   Nn = nrows
   CALL unpack(*800,hccen,Z(ncstm+1))
   GOTO 900
 800  DO i = 1 , nrows
      Z(ncstm+i) = 0.
   ENDDO
 900  Nn = nelx
   CALL unpack(*1000,remfl,Z(nncr+1))
   GOTO 1100
 1000 DO i = 1 , nelx
      Z(nncr+i) = 0.
   ENDDO
!
!     HCFLD VECTOR IS IN Z(NCSTM+1)-Z(NCSTM+NROWS=NNCR) AND REMFL IS IN
!     Z(NNCR+1)-Z(NNCR+NELX). MATCH ELEMENT TYPE ON HOEF1 WITH ESTFLD
!
 1100 file = estfld
 1200 CALL read(*1800,*1900,estfld,iel,1,0,iwords)
   iex = 3
   IF ( iel==66 .OR. iel==67 ) iex = 63
   IF ( iel==65 ) iex = 27
   ipts = iex/3
!
!     SINCE IS2D8 HAS 9 POINTS ON HCCEN BUT ONLY ONE ON HEOF1 AND ESTFLD
!     RESET IPTS
!
   IF ( iel==80 ) ipts = 9
   IF ( iel/=eltype ) THEN
      DO
!
!     NO MATCH. SKIP TO NEXT RECORD, BUT KEEP UP WITH NCOUNT
!
         CALL read(*1800,*1200,estfld,idum,2,0,iwords)
         CALL fread(estfld,idum,-(idum(2)+19+iex),0)
         ncount = ncount + 1
         hcount = hcount + ipts
      ENDDO
   ENDIF
!
!     MATCH ON ELEMENT TYPE. FIND A MATCH ON ELEMENT ID
!
 1300 file = hoef1
   CALL read(*1800,*1400,hoef1,rbuf,9,0,iwords)
   elid = ibuf(1)/10
   file = estfld
!
!     NEXT STATEMENT IS FOR ISOPARAMETRICS WHICH HAVE MULTIPLE POINTS
!     ON HOEF1, BUT ONLY ONE SET OF INFO ON ESTFLD(BUT MULTIPLE COORDS
!     FOR NON-BASIC COORDINATE SYSTEMS). IF MATERIAL IS ALLOWED TO BE
!     TEMPERATURE-DEPENDENT AT SOME LATER DATE IN MAGNETICS PROBLEMS,
!     THEN ESTFLD WILL HAVE MULTIPLE INFO. WRIITEN IN ESTMAG AND THIS
!     STATEMENT CAN BE DELETED
!
   IF ( oldeid==0 ) THEN
      DO
!
         CALL read(*1800,*1900,estfld,Iz(nall+1),2,0,iwords)
         ncount = ncount + 1
         hcount = hcount + ipts
         ielid = Iz(nall+1)
         ngrids = Iz(nall+2)
         nwords = ngrids + 19 + iex
         IF ( nall+nwords>lcore ) GOTO 2000
         CALL read(*1800,*1900,estfld,Z(nall+1),nwords,0,iwords)
!
         IF ( elid==ielid ) EXIT
      ENDDO
   ENDIF
!
!     MATCH ON ELEMENT ID. PICK UP HM FROM HOEF1(IN ELEMENT COORDS)
!     PICK UP 3 X 3 TRANSFORMATION MATRIX FROM ESTFLD TO CONVERT ELEMENT
!     SYSTEM TO BASIC. THEN MULTIPLY
!
   hm(1) = rbuf(4)
   hm(2) = rbuf(5)
   hm(3) = rbuf(6)
!WKBNB 8/94 ALPHA-VMS
   itype = numtyp(hm(2))
   IF ( itype<=1 ) hm(2) = 0.
   itype = numtyp(hm(3))
   IF ( itype<=1 ) hm(3) = 0.
!WKBNE 8/94 ALPHA-VMS
   CALL gmmats(Z(nall+ngrids+10),3,3,0,hm,3,1,0,hmg)
!
!     PICK UP HC FROM HCCEN VECTOR. FOR ALL EXCEPT ISOPARAMETRICS,HCOUNT
!     POINTS TO THE Z COMPONENT OF PROPER HC WHICH STARTS AT Z(NCSTM+1)
!
   IF ( rbuf(2)/=hex1 .AND. rbuf(2)/=hex2 .AND. rbuf(2)/=hex3 ) THEN
      strspt = 1
!
!     NEXT LINE IS FOR IS2D8 WHICH HAS 9 POINTS ON HCCEN BUT ONE ON
!     ESTFLD
!
      IF ( iel==80 ) strspt = 9
   ELSE
!
!     ISOPARAMETRIC SOLIDS
!
      IF ( oldeid/=elid ) THEN
         oldeid = elid
         strspt = 0
      ENDIF
      strspt = strspt + 1
      IF ( strspt>=21 ) oldeid = 0
      IF ( rbuf(2)==hex1 .AND. strspt>=9 ) oldeid = 0
   ENDIF
   isub = ncstm + 3*(hcount-ipts+strspt-1)
   hc(1) = Z(isub+1)
   hc(2) = Z(isub+2)
   hc(3) = Z(isub+3)
!
   DO i = 1 , 3
      rbuf(i+3) = hmg(i) + hc(i)
   ENDDO
!
!     TO GET INDUCTION B, MULTIPLY H BY MATERIALS
!
   CALL gmmats(Z(nall+ngrids+1),3,3,0,rbuf(4),3,1,0,rbuf(7))
!
!     ADD IN REMANENCE Z(NNCR+1)-Z(NNCR+NELX)
!
   isub = nncr + 3*ncount - 3
   rbuf(7) = rbuf(7) + Z(isub+1)
   rbuf(8) = rbuf(8) + Z(isub+2)
   rbuf(9) = rbuf(9) + Z(isub+3)
!
!     CHECK FOR REQUEST FOR NON-BASIC COORD. SYSTEM. TA TRANSFORMS TO
!     BASIC
!
   ifield = Iz(nall+ngrids+19)
   IF ( ifield/=0 ) THEN
      icoord(1) = ifield
!
!     NEXT LINE IS FOR IS2D8 WHICH ONLY ONE POINT ON ESTFLD
!
      IF ( iel==80 ) strspt = 1
      isub = nall + ngrids + 19 + 3*strspt - 3
      coord(2) = Z(isub+1)
      coord(3) = Z(isub+2)
      coord(4) = Z(isub+3)
      CALL transs(coord,ta)
      CALL gmmats(ta,3,3,1,rbuf(7),3,1,0,temp)
      DO i = 1 , 3
         rbuf(i+6) = temp(i)
      ENDDO
   ENDIF
!
!
!     WRITE OUT TO HOEH1
!
   CALL write(hoeh1,rbuf,9,0)
!
!     GET ANOTHER ELEMENT OF THIS TYPE IN THIS SUBCASE
!
   GOTO 1300
!
!     END OF ELEMENTS OF PRESENT TYPE AND/OR SUBCASE ON HOEF1
!
 1400 CALL write(hoeh1,0,0,1)
   file = estfld
   DO
!
!     SKIP RECORD BUT KEEP UP WITH NCOUNT
!
      CALL read(*1800,*1500,estfld,idum,2,0,iwords)
      CALL fread(estfld,idum,-(idum(2)+19+iex),0)
      ncount = ncount + 1
      hcount = hcount + ipts
   ENDDO
 1500 file = hoef1
   GOTO 400
!
!     EOF ON HOEF1 - ALL DONE
!
 1600 CALL close(casecc,1)
   CALL close(hccen,1)
   CALL close(estfld,1)
   CALL close(hoef1,1)
   CALL close(remfl,1)
   CALL close(hoeh1,1)
   mcb(1) = hoef1
   CALL rdtrl(mcb)
   mcb(1) = hoeh1
   CALL wrttrl(mcb)
   GOTO 99999
 1700 n = -1
   GOTO 2100
 1800 n = -2
   GOTO 2100
 1900 n = -3
   GOTO 2100
 2000 n = -8
   file = 0
 2100 CALL mesage(n,file,nam)
!
99999 END SUBROUTINE emfld
