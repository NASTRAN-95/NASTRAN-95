
SUBROUTINE sdr3a(Ofpfil)
   IMPLICIT NONE
   INTEGER Ibufsz , L , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /system/ Ibufsz , L
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   INTEGER Ofpfil(6)
   INTEGER ahead , buff(10) , buff10 , buff9 , core , entrys(85) , eof , eor , file , full , group , i , iamt , ibuff , icore ,     &
         & id(146) , idata , idtemp(146) , ieor , ierror , ifile(6) , ihd , ihd2 , ihead , iname(2) , infile , inprwd , iretrn ,    &
         & itype , j , k , layers , n , n1 , n2 , ndata , nentry , nfile , nfiles , noeor , nofrq , npoint , nrecs , nscrat ,       &
         & ntypes , nwds , nwords , ofile(6) , oufile , outrwd , ovrlap , recpt , recs , rwd , scrtch(8) , total1 , total2 ,        &
         & trail(7) , vector(50) , vinbk , vperbk , words , wperbk
   INTEGER korsz
!
!     SORT-2  MODULE
!
   !>>>>EQUIVALENCE (nwds,id(10))
!
!     IF THE NUMBER OF SCRATCH FILES CHANGE, ONE SHOULD SET NSCRAT EQUAL
!     TO THE NEW NUMBER AND INCREASE THE DATA BELOW
!
!     NFILES BELOW EQUALS THE NUMBER OF INPUT FILES AND ALSO EQUALS
!     THE NUMBER OF OUTPUT FILES.  IF NFILES CHANGES, CHANGE THE DATA
!     BELOW TO CONFORM...
!     ALSO CHANGE DIMENSIONS OF BUFF,IFILE,OFILE,SCRTCH, AS REQUIRED...
!
   DATA ifile/101 , 102 , 103 , 104 , 105 , 106/
   DATA ofile/201 , 202 , 203 , 204 , 205 , 206/
   DATA scrtch/301 , 302 , 303 , 304 , 305 , 306 , 307 , 308/
   DATA trail/0 , 1 , 2 , 3 , 4 , 5 , 6/
   DATA eor , noeor , rwd , inprwd , outrwd/1 , 0 , 1 , 0 , 1/
!
   nfiles = 6
   nscrat = 8
   DO i = 1 , 6
      Ofpfil(i) = 0
   ENDDO
   DO i = 1 , 146
      idtemp(i) = 0
   ENDDO
!
!     BUFFERS AND OPEN CORE
!
   core = korsz(Z)
!
   buff(1) = core - Ibufsz + 1
   DO i = 2 , 10
      buff(i) = buff(i-1) - Ibufsz
   ENDDO
   buff9 = buff(9)
   buff10 = buff(10)
   core = buff(10) - 1
   IF ( core<1 ) THEN
      DO i = 1 , 5
         Ofpfil(i) = 22
      ENDDO
      WRITE (L,99001) Uwm
99001 FORMAT (A25,' 986, INSUFFICIENT CORE FOR SDR3.')
   ELSE
!
!     OPEN SCRATCH FILES FOR OUTPUT
!
      ierror = 0
      DO i = 1 , nscrat
         ibuff = buff(i)
         CALL open(*20,scrtch(i),Z(ibuff),outrwd)
         CYCLE
 20      ierror = 1
         WRITE (L,99002) Uwm , i
99002    FORMAT (A25,' 985, SDR3 FINDS SCRATCH',I1,' PURGED.')
      ENDDO
!
!     EXECUTE FOR NFILES FILES
!
      DO file = 1 , nfiles
         eof = 0
         infile = ifile(file)
         oufile = ofile(file)
!
         CALL open(*750,infile,Z(buff9),inprwd)
         CALL open(*720,oufile,Z(buff10),outrwd)
         CALL fwdrec(*400,infile)
!
!     HEADER RECORD FOR OUFILE
!
         CALL fname(oufile,iname(1))
         CALL write(oufile,iname(1),2,eor)
!
!     WRITE SOME JUNK IN TRAILER FOR NOW
!
         trail(1) = oufile
         CALL wrttrl(trail(1))
         nofrq = 0
!
!     PROCEED WITH TRANSPOSE OF DATA = SORT-2
!
!     GROUP WILL BE THE NUMBER OF THE FIRST REC IN THE PRESENT GROUP OF
!     DATA BLOCKS BEING OPERATED ON, LESS 1
!
         nrecs = 1
!
 40      ASSIGN 140 TO iretrn
!
 60      CALL read(*100,*600,infile,id(1),146,eor,iamt)
         IF ( id(1)/10==1 ) nofrq = 1
         idata = 1
!
 80      icore = core
         recs = 0
         group = nrecs
!
!     READ FIRST DATA BLOCK INTO CORE
!
!
!     INSUFFICIENT CORE,  IF FALL HERE, TO DO SORT II ON THIS FILE..
!
         CALL read(*420,*120,infile,Z(1),icore,noeor,iamt)
!
!     INSUFFICIENT CORE
!
         n = 19
         GOTO 740
 100     CALL close(infile,rwd)
         CALL close(oufile,rwd)
         CYCLE
!
 120     IF ( iamt==0 ) GOTO 340
         entrys(1) = iamt/nwds
!
!     SET UP IN-CORE ENTRY BLOCKS
!     SPOT FOR TRANSPOSE HEADING DATA IS AT Z(ICORE-ENTRYS(1)+1)
!
         ihd2 = icore + 1
         icore = icore - entrys(1)
         ihead = icore
         IF ( icore<iamt ) THEN
            n = 20
            GOTO 740
         ELSE
!
!     NOTATION - W PER BK = WORDS PER ENTRY BLOCK
!                V PER BK = VECTORS PER ENTRY BLOCK
!                V IN  BK = VECTORS NOW IN ENTRY BLOCKS
!
            wperbk = icore/entrys(1)
            vperbk = wperbk/nwds
            wperbk = vperbk*nwds
            IF ( vperbk<1 ) THEN
               n = 21
               GOTO 740
            ELSE
!
!     DISTRIBUTE FIRST DATA BLOCK TO INCORE ENTRY BLOCKS (BOTTOM TO TOP)
!
               nentry = entrys(1)
               total1 = wperbk*entrys(1) + 1
               total2 = nwds*entrys(1) + 1
               DO i = 1 , nentry
                  n1 = total1 - wperbk*i
                  n2 = total2 - nwds*i
                  ihd = ihd2 - i
                  Z(ihd) = Z(n2)
                  Z(n1) = id(5)
!
!     SAVE TRANSPOSE HEADING
!
                  DO j = 2 , nwds
                     n1 = n1 + 1
                     n2 = n2 + 1
                     Z(n1) = Z(n2)
                  ENDDO
               ENDDO
!
               vinbk = 1
               GOTO iretrn
            ENDIF
         ENDIF
!
 140     ntypes = 1
 160     CALL read(*200,*620,infile,idtemp(1),146,eor,iamt)
         IF ( (id(2)==idtemp(2) .AND. id(3)==idtemp(3) .AND. id(5)/=idtemp(5)) .OR. (id(5)/=idtemp(5)) .OR. (id(4)/=idtemp(4)) )    &
            & GOTO 220
!
         ntypes = ntypes + 1
         nwords = idtemp(10)
!
!     WILL READ DATA AND COUNT ENTRYS
!
         IF ( ntypes>30 ) THEN
            WRITE (L,99003) Ufm , ntypes
99003       FORMAT (A23,' 3129, SDR3 CAN ONLY PROCESS 30 ELEMENT TYPES, ','PROBLEM HAS',I5)
            CALL mesage(-61,0,0)
            GOTO 720
         ELSE
            entrys(ntypes) = 0
            DO
               CALL read(*440,*160,infile,idtemp(1),nwords,noeor,iamt)
               entrys(ntypes) = entrys(ntypes) + 1
            ENDDO
         ENDIF
!
 180     ahead = 2*ntypes - 2
         IF ( ndata/=1 ) GOTO 240
         GOTO 300
!
!     AT THIS POINT IT IS KNOWN HOW MANY TYPES ARE IN THE PRESENT GROUP
!     OF DATA BLOCKS AND ALSO HOW MANY ENTRYS IN EACH TYPE
!
 200     IF ( ntypes==1 ) eof = 1
 220     itype = 1
         ndata = 1
         idata = 1
!
!     POSITION TO READ 2-ND ID OF TYPE(ITYPE) IF NOT JUST READ
!
         IF ( ntypes==1 ) GOTO 280
         CALL rewind(infile)
         ahead = group + 2*ntypes
 240     DO i = 1 , ahead
            CALL fwdrec(*460,infile)
         ENDDO
!
 260     CALL read(*300,*640,infile,idtemp(1),146,eor,iamt)
!
!     CHECK FOR BREAK POINT
!
 280     IF ( nofrq/=1 ) THEN
            IF ( id(4)/=idtemp(4) ) GOTO 320
         ENDIF
         IF ( eof==1 ) GOTO 320
         IF ( itype==1 ) ndata = ndata + 1
         idata = idata + 1
         nentry = entrys(itype)
!
!     CHECK TO SEE IF THERE IS ENOUGH ROOM IN EACH OF THE INCORE
!     ENTRY BLOCKS FOR ANOTHER VECTOR
!     IF NOT DO SCRATCH FILE  OPERATIONS
!
         IF ( vinbk>=vperbk ) THEN
!
!     NOT ENOUGH ROOM THUS DUMP CORE ENTRY BLOCKS ONTO SCRATCH FILES
!
            IF ( ierror==1 ) THEN
!
!     ATTEMPT TO USE SCRATCH FILES 1 OR MORE OF WHICH ARE PURGED.
!
               Ofpfil(file) = 1
               GOTO 100
            ELSE
               npoint = 1
               nfile = nscrat
               DO i = 1 , nentry
                  nfile = nfile + 1
                  IF ( nfile>nscrat ) nfile = 1
                  CALL write(scrtch(nfile),Z(npoint),wperbk,eor)
                  npoint = npoint + wperbk
               ENDDO
               recs = recs + nentry
!
!     IN CORE ENTRY BLOCKS ARE NOW EMPTY
!
               vinbk = 0
            ENDIF
         ENDIF
!
!     DISTRIBUTE DATA TO INCORE ENTRY BLOCKS
!
         npoint = vinbk*nwds + 1
         DO i = 1 , nentry
            ieor = i/nentry
            CALL read(*480,*660,infile,Z(npoint),nwds,ieor,iamt)
            Z(npoint) = idtemp(5)
            npoint = npoint + wperbk
         ENDDO
         vinbk = vinbk + 1
!
         IF ( ntypes/=1 ) THEN
            IF ( itype/=1 ) THEN
               IF ( idata==ndata ) GOTO 320
            ENDIF
!
!     NOW POSITION AHEAD TO READ NEXT ID FOR TYPE(ITYPE)
!
            ahead = 2*ntypes - 2
            DO i = 1 , ahead
               CALL fwdrec(*500,infile)
            ENDDO
         ENDIF
         GOTO 260
!
!     ONE DATA TYPE IN THIS GROUP IS COMPLETE
!
!     OUTPUT IS IN CORE, AND ON SCRATCH FILES IF RECS IS NOT 0
!
!     NOW DUMP SCRATCHES AND (OR JUST) CORE ONTO FINAL OUTPUT TAPE
!
!     ID WILL BE WRITTEN BEFORE EACH ENTRY INSERTING INTO IT THE NEW
!     HEADER VALUE REPLACING FREQUENCY OR TIME ETC
!
 300     eof = 1
 320     IF ( recs/=0 ) THEN
!
            layers = recs/nentry
!
!     CLOSE SCRATCH FILES AND OPEN AS INPUT FILES
!
            DO i = 1 , nscrat
               CALL close(scrtch(i),rwd)
               ibuff = buff(i)
               CALL open(*380,scrtch(i),Z(ibuff),inprwd)
            ENDDO
!
!     COMPUTE OVERLAPS PER LAYER
!
            ovrlap = (nentry-1)/nscrat
!
!     COMPUTE HOW MANY TAPES HAVE ALL THE OVERLAPS
!
            full = nentry - ovrlap*nscrat
         ENDIF
!
!
!     WRITE FINAL FILE THEN
!
         nfile = 0
         id(2) = id(2) + 2000
         DO i = 1 , nentry
            nfile = nfile + 1
            IF ( nfile>nscrat ) nfile = 1
!
            npoint = ihead + i
            id(5) = Z(npoint)
            CALL write(oufile,id(1),146,eor)
!
!     ANYTHING ON SCRATCH FILES IS NOW WRITTEN
!
            IF ( recs/=0 ) THEN
!
               DO j = 1 , layers
!
!     FORWARD REC IF NECESSARY
!
                  IF ( j>1 ) THEN
!
                     recpt = ovrlap
                     IF ( nfile>full ) recpt = recpt - 1
                     IF ( recpt>0 ) THEN
                        DO k = 1 , recpt
                           CALL fwdrec(*560,scrtch(nfile))
                        ENDDO
                     ENDIF
!
!     AHEAD TO FIRST PART IF NECESSARY
!
                  ELSEIF ( layers/=1 ) THEN
                     ahead = (i-1)/nscrat
!
                     IF ( ahead/=0 ) THEN
                        DO k = 1 , ahead
                           CALL fwdrec(*540,scrtch(nfile))
                        ENDDO
                     ENDIF
                  ENDIF
!
!     COPY RECORD FROM SCRTCH TO OUTFILE
!
                  DO k = 1 , vperbk
                     ieor = k/vperbk
                     CALL read(*580,*680,scrtch(nfile),vector(1),nwds,ieor,iamt)
                     CALL write(oufile,vector(1),nwds,noeor)
                  ENDDO
               ENDDO
               IF ( layers>1 ) CALL rewind(scrtch(nfile))
            ENDIF
!
!     COPY INCORE VECTORS TO OUTFILE
!
            words = vinbk*nwds
            npoint = wperbk*i - wperbk + 1
            CALL write(oufile,Z(npoint),words,eor)
         ENDDO
         IF ( recs/=0 ) THEN
!
!     CLOSE SCRTCH FILES AND OPEN AS OUTPUT FILES
!
            DO i = 1 , nscrat
               CALL close(scrtch(i),rwd)
               ibuff = buff(i)
               CALL open(*360,scrtch(i),Z(ibuff),outrwd)
            ENDDO
         ENDIF
!
         IF ( itype==ntypes ) THEN
!
!     THIS GROUP IS ABSOLUTELY COMPLETE AND WE ARE AT BREAK POINT
!
            IF ( eof==1 ) GOTO 100
            nrecs = nrecs + 2*ndata*ntypes
            IF ( ntypes>1 ) GOTO 40
         ELSE
!
            itype = itype + 1
            CALL rewind(infile)
            ahead = group + itype*2 - 2
            DO i = 1 , ahead
               CALL fwdrec(*520,infile)
            ENDDO
            ASSIGN 180 TO iretrn
            eof = 0
            GOTO 60
         ENDIF
 340     DO i = 1 , 146
            id(i) = idtemp(i)
         ENDDO
         GOTO 80
!
!
!     ERROR CONDITIONS FOR THIS DATA BLOCK
!
!     FORMAT OF INPUT DATA BLOCK MAY BE INCORRECT (N=TRACEBACK CODE)
!
 360     n = 23
         GOTO 700
 380     n = 3
         GOTO 700
 400     n = 4
         GOTO 700
 420     n = 5
         GOTO 700
 440     n = 6
         GOTO 700
 460     n = 7
         GOTO 700
 480     n = 8
         GOTO 700
 500     n = 9
         GOTO 700
 520     n = 10
         GOTO 700
 540     n = 11
         GOTO 700
 560     n = 12
         GOTO 700
 580     n = 13
         GOTO 700
 600     n = 14
         GOTO 700
 620     n = 15
         GOTO 700
 640     n = 16
         GOTO 700
 660     n = 17
         GOTO 700
 680     n = 18
 700     Ofpfil(file) = n
         WRITE (L,99004) Uwm , file
99004    FORMAT (A25,' 982, FORMAT OF SDR3 INPUT DATA BLOCK ',I3,' DOES NOT PERMIT SUCCESSFUL SORT-2 PROCESSING.')
         GOTO 100
!
!     CORRESPONDING OUTPUT FILE IS PURGED.
!
 720     Ofpfil(file) = 2
         WRITE (L,99005) Uwm , file
99005    FORMAT (A25,' 984,  SDR3 FINDS OUTPUT DATA-BLOCK',I4,' PURGED.')
         GOTO 100
 740     WRITE (L,99006) Uwm , file
99006    FORMAT (A25,' 983, SDR3 HAS INSUFFICIENT CORE TO PERFORM SORT-2',' ON INPUT DATA BLOCK',I4,/5X,                            &
                &'OR DATA-BLOCK IS NOT IN CORRECT FORMAT.')
         Ofpfil(file) = n
         GOTO 100
!
 750  ENDDO
!
!     CLOSE SCRATCH FILES
!
      DO i = 1 , nscrat
         CALL close(scrtch(i),rwd)
!
      ENDDO
   ENDIF
END SUBROUTINE sdr3a