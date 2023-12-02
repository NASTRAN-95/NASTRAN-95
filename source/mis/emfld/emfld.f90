!*==emfld.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emfld
   USE c_blank
   USE c_gpta1
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , elid , eltype , file , hcount , i , iany , iel , ielid , iex , ifield ,     &
            & ipts , isub , itype , iwords , j , kcount , lcc , lcore , lsym , n , nall , nall2 , ncount , ncstm , nelx , nelx2 ,   &
            & nextp , nextr , nextz , ngrids , nncr , nrows , nrows2 , nwords , oldcas , oldeid , strspt , subcas
   INTEGER , SAVE :: casecc , cstm , dit , estfld , geom1 , hccen , hest , hoef1 , hoeh1 , mpt , remfl
   REAL :: coef
   REAL , DIMENSION(4) :: coord
   REAL , DIMENSION(3) :: hc , hm , hmg , temp
   REAL , SAVE :: hex1 , hex2 , hex3
   INTEGER , DIMENSION(150) :: ibuf
   INTEGER , DIMENSION(4) :: icoord
   INTEGER , DIMENSION(2) :: idum
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL , DIMENSION(150) :: rbuf
   REAL , DIMENSION(9) :: ta
   REAL , DIMENSION(2) , SAVE :: zn
   EXTERNAL bckrec , close , estmag , fread , fwdrec , gmmats , gopen , korsz , mesage , numtyp , open , pretrs , rdtrl , read ,    &
          & rewind , transs , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!                                            SEE T01191A     ===========
!     COMPUTES TOTAL MAGNETIC FIELD STRENGTH AND INDUCTION FOR
!     EACH ELEMENT IN BASIC COORDINATES BY ADDING HM AND HC
!
!     EMFLD    HOEF1,HEST,CASECC,HCFLD,MPT,DIT,REMFLD,GEOM1,CSTM,HCCEN/
!              HOEH1/V,N,HLUSET $
!
   !>>>>EQUIVALENCE (rbuf(1),ibuf(1)) , (coord(1),icoord(1)) , (Z(1),Iz(1))
   DATA hoef1 , hest , casecc , mpt , dit/101 , 102 , 103 , 105 , 106/
   DATA remfl , geom1 , cstm , hccen/107 , 108 , 109 , 110/
   DATA estfld , hoeh1/301 , 201/
   DATA nam/4HEMFL , 4HD   / , zn/4HHOEH , 4H1   /
   DATA hex1 , hex2 , hex3/4HHEX1 , 4HHEX2 , 4HHEX3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK TO SEE IF HOEF1 EXISTS. IF NOT, THEN NO MAG. FIELD REQUESTS
!
         mcb(1) = hoef1
         CALL rdtrl(mcb)
         IF ( mcb(1)<0 ) RETURN
         mcb(1) = hccen
         CALL rdtrl(mcb)
         nn = mcb(3)
         IF ( mcb(1)<=0 ) THEN
            nn = 0
            mcb(1) = remfl
            CALL rdtrl(mcb)
            IF ( mcb(1)<=0 ) THEN
               WRITE (otpe,99001) uwm
99001          FORMAT (A25,', DATA BLOCKS HCFLD AND REMFL ARE PURGED IN EM ','PROBLEM. ALL RESULTS ARE ZERO')
               RETURN
            ENDIF
         ENDIF
!
         mcb(1) = hest
         CALL rdtrl(mcb)
         nelx = 3*mcb(2)
!
         typout = 1
         ii = 1
         incur = 1
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
         IF ( nn==0 ) nn = kcount
         IF ( nn/=kcount ) THEN
!
!     FATAL ERROR MESSAGES
!
            WRITE (otpe,99002) sfm
99002       FORMAT (A25,', ROW COUNT ON HCCEN IN EMFLD IS NOT CONSISTENT')
            CALL mesage(-61,0,0)
            GOTO 220
         ELSE
            nrows = nn
!
!     NOW FETCH HC AT EACH POINT FROM HCFLD
!
            lcore = korsz(z)
            buf1 = lcore - sysbuf + 1
            buf2 = buf1 - sysbuf
            buf3 = buf2 - sysbuf
            buf4 = buf3 - sysbuf
            buf5 = buf4 - sysbuf
            buf6 = buf5 - sysbuf
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
               CALL open(*220,hoeh1,z(buf5),1)
               file = hoef1
               CALL open(*220,hoef1,z(buf4),0)
               CALL read(*240,*20,hoef1,z,lcore,0,iwords)
            ENDIF
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      z(1) = zn(1)
         i = 2
         z(i) = zn(2)
         CALL write(hoeh1,z,iwords,1)
!
!     OPEN CSTM FOR NON-BASIC COORDINATE SYSTEM
!
         ncstm = 0
         IF ( iany==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(cstm,z(buf1),0)
         CALL read(*240,*40,cstm,z,lcore,0,ncstm)
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(cstm,1)
         CALL pretrs(z(1),ncstm)
         spag_nextblock_1 = 2
      CASE (2)
!
         nncr = ncstm + nrows
         nall = nncr + nelx
         CALL gopen(casecc,z(buf1),0)
         CALL gopen(hccen,z(buf2),0)
         CALL gopen(estfld,z(buf3),0)
         CALL gopen(remfl,z(buf6),0)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ ID RECORD FROM HOEF1. COPY TO HOEH1, EXCEPT CHANGE NUMBER OF
!     WORDS FROM +9 TO -9 AS AN INDICATOR FOR TITLES IN OFP. (+9 IS FOR
!     HEAT TRANSFER) ALSO PICK UP SUBCASE NUMBER AND ELEMENT TYPE. IF
!     SAME SUBCASE AS PREVIUUS ONE, USE SAME HCFLD VECTOR. IF NOT,
!     CREATE A NEW ONE
!
         CALL read(*200,*260,hoef1,ibuf,146,1,iwords)
         ibuf(10) = -9
         CALL write(hoeh1,ibuf,146,1)
         eltype = ibuf(3)
         subcas = ibuf(4)
         oldeid = 0
         IF ( subcas==oldcas ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         oldcas = subcas
         ncount = 0
         hcount = 0
         CALL rewind(estfld)
         file = estfld
         CALL fwdrec(*240,estfld)
!
!     IF THIS SUBCASE IS NOT A SUBCOM, UNPACK NEXT COLUMN OF HCFLD. IF
!     IT IS A SUBCOM, BCKREC HCFLD THE SAME NUMBER OF RECORDS AS THERE
!     ARE FACTORS ON THE SUBSEQ AND COMBINE VECTORS TO PRODUCE ONE
!     VECTOR.
!
         IF ( lcore<16 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
            file = casecc
            CALL read(*240,*260,casecc,z(ncstm+1),16,0,iwords)
            IF ( iz(ncstm+1)==subcas ) THEN
!
!     MATCH ON SUBCASE ID. SEE HOW LONG THE RECORD IS
!
               IF ( iz(ncstm+16)==0 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     SUBCOM UNLESS IZ(16).LT.0. IN WHICH CASE IT IS A REPEAT SUBCASE
!
               IF ( iz(ncstm+16)>0 ) THEN
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
                  CALL read(*240,*60,casecc,z(ncstm+17),lcore,0,iwords)
                  spag_nextblock_1 = 8
               ELSE
                  CALL bckrec(hccen)
                  CALL bckrec(remfl)
                  spag_nextblock_1 = 4
               ENDIF
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL fwdrec(*240,casecc)
               file = hccen
               CALL fwdrec(*240,hccen)
               file = remfl
               CALL fwdrec(*240,remfl)
            ENDIF
         ENDDO
 60      lcc = iz(ncstm+166)
         lsym = iz(ncstm+lcc)
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
         IF ( nextz+nall2>lcore ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SET UP FOR SUBSEQ
!
         DO i = 1 , nall2
            z(nextz+i) = 0.
         ENDDO
         DO i = 1 , lsym
            coef = z(ncstm+lcc+i)
            IF ( coef==0. ) THEN
!
!     COEF = 0.
!
               file = hccen
               CALL fwdrec(*240,hccen)
               file = remfl
               CALL fwdrec(*240,remfl)
               CYCLE
            ELSE
               nn = nrows
               CALL unpack(*70,hccen,z(nextz+1))
               DO j = 1 , nrows
                  z(nextr+j) = z(nextr+j) + coef*z(nextz+j)
               ENDDO
            ENDIF
 70         nn = nelx
            CALL unpack(*80,remfl,z(nextp+1))
            DO j = 1 , nelx
               z(isub+j) = z(isub+j) + coef*z(nextp+j)
            ENDDO
 80      ENDDO
!
!     MOVE THE VECTOR IN CORE
!
         DO i = 1 , nrows
            z(ncstm+i) = z(nextr+i)
         ENDDO
         DO i = 1 , nelx
            z(nncr+i) = z(isub+i)
         ENDDO
         spag_nextblock_1 = 6
      CASE (4)
!
!     NOT A SUBCOM
!     UNPACK A COLUMN OF HCFLD. FIRST SKIP TO NEXT RECORD ON CASECC
!
         file = casecc
         CALL fwdrec(*240,casecc)
         nn = nrows
         CALL unpack(*100,hccen,z(ncstm+1))
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     DO i = 1 , nrows
            z(ncstm+i) = 0.
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
         nn = nelx
         CALL unpack(*120,remfl,z(nncr+1))
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 120     DO i = 1 , nelx
            z(nncr+i) = 0.
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
!
!     HCFLD VECTOR IS IN Z(NCSTM+1)-Z(NCSTM+NROWS=NNCR) AND REMFL IS IN
!     Z(NNCR+1)-Z(NNCR+NELX). MATCH ELEMENT TYPE ON HOEF1 WITH ESTFLD
!
         file = estfld
 140     CALL read(*240,*260,estfld,iel,1,0,iwords)
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
               CALL read(*240,*140,estfld,idum,2,0,iwords)
               CALL fread(estfld,idum,-(idum(2)+19+iex),0)
               ncount = ncount + 1
               hcount = hcount + ipts
            ENDDO
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     MATCH ON ELEMENT TYPE. FIND A MATCH ON ELEMENT ID
!
         file = hoef1
         CALL read(*240,*160,hoef1,rbuf,9,0,iwords)
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
            SPAG_Loop_1_1: DO
!
               CALL read(*240,*260,estfld,iz(nall+1),2,0,iwords)
               ncount = ncount + 1
               hcount = hcount + ipts
               ielid = iz(nall+1)
               ngrids = iz(nall+2)
               nwords = ngrids + 19 + iex
               IF ( nall+nwords>lcore ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*240,*260,estfld,z(nall+1),nwords,0,iwords)
!
               IF ( elid==ielid ) EXIT SPAG_Loop_1_1
            ENDDO SPAG_Loop_1_1
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
         CALL gmmats(z(nall+ngrids+10),3,3,0,hm,3,1,0,hmg)
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
         hc(1) = z(isub+1)
         hc(2) = z(isub+2)
         hc(3) = z(isub+3)
!
         DO i = 1 , 3
            rbuf(i+3) = hmg(i) + hc(i)
         ENDDO
!
!     TO GET INDUCTION B, MULTIPLY H BY MATERIALS
!
         CALL gmmats(z(nall+ngrids+1),3,3,0,rbuf(4),3,1,0,rbuf(7))
!
!     ADD IN REMANENCE Z(NNCR+1)-Z(NNCR+NELX)
!
         isub = nncr + 3*ncount - 3
         rbuf(7) = rbuf(7) + z(isub+1)
         rbuf(8) = rbuf(8) + z(isub+2)
         rbuf(9) = rbuf(9) + z(isub+3)
!
!     CHECK FOR REQUEST FOR NON-BASIC COORD. SYSTEM. TA TRANSFORMS TO
!     BASIC
!
         ifield = iz(nall+ngrids+19)
         IF ( ifield/=0 ) THEN
            icoord(1) = ifield
!
!     NEXT LINE IS FOR IS2D8 WHICH ONLY ONE POINT ON ESTFLD
!
            IF ( iel==80 ) strspt = 1
            isub = nall + ngrids + 19 + 3*strspt - 3
            coord(2) = z(isub+1)
            coord(3) = z(isub+2)
            coord(4) = z(isub+3)
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
!
!     GET ANOTHER ELEMENT OF THIS TYPE IN THIS SUBCASE
!
         CALL write(hoeh1,rbuf,9,0)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     END OF ELEMENTS OF PRESENT TYPE AND/OR SUBCASE ON HOEF1
!
 160     CALL write(hoeh1,0,0,1)
         file = estfld
         DO
!
!     SKIP RECORD BUT KEEP UP WITH NCOUNT
!
            CALL read(*240,*180,estfld,idum,2,0,iwords)
            CALL fread(estfld,idum,-(idum(2)+19+iex),0)
            ncount = ncount + 1
            hcount = hcount + ipts
         ENDDO
 180     file = hoef1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     EOF ON HOEF1 - ALL DONE
!
 200     CALL close(casecc,1)
         CALL close(hccen,1)
         CALL close(estfld,1)
         CALL close(hoef1,1)
         CALL close(remfl,1)
         CALL close(hoeh1,1)
         mcb(1) = hoef1
         CALL rdtrl(mcb)
         mcb(1) = hoeh1
         CALL wrttrl(mcb)
         RETURN
 220     n = -1
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 240     n = -2
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 260     n = -3
         spag_nextblock_1 = 9
      CASE (8)
         n = -8
         file = 0
         spag_nextblock_1 = 9
      CASE (9)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE emfld
