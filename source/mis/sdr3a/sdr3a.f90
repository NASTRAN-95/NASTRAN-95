!*==sdr3a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr3a(Ofpfil)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: Ofpfil
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ahead , buff10 , buff9 , core , eof , file , full , group , i , iamt , ibuff , icore , idata , ieor , ierror , ihd ,  &
            & ihd2 , ihead , infile , iretrn , itype , j , k , layers , n , n1 , n2 , ndata , nentry , nfile , nfiles , nofrq ,     &
            & npoint , nrecs , nscrat , ntypes , nwds , nwords , oufile , ovrlap , recpt , recs , total1 , total2 , vinbk , vperbk ,&
            & words , wperbk
   INTEGER , DIMENSION(10) :: buff
   INTEGER , DIMENSION(85) :: entrys
   INTEGER , SAVE :: eor , inprwd , noeor , outrwd , rwd
   INTEGER , DIMENSION(146) :: id , idtemp
   INTEGER , DIMENSION(6) , SAVE :: ifile , ofile
   INTEGER , DIMENSION(2) :: iname
   INTEGER , DIMENSION(8) , SAVE :: scrtch
   INTEGER , DIMENSION(7) , SAVE :: trail
   INTEGER , DIMENSION(50) :: vector
   EXTERNAL close , fname , fwdrec , korsz , mesage , open , read , rewind , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               eof = 0
               infile = ifile(file)
               oufile = ofile(file)
!
               CALL open(*150,infile,Z(buff9),inprwd)
               CALL open(*145,oufile,Z(buff10),outrwd)
               CALL fwdrec(*70,infile)
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
               spag_nextblock_1 = 2
            CASE (2)
!
               ASSIGN 35 TO iretrn
               spag_nextblock_1 = 3
            CASE (3)
!
               CALL read(*25,*120,infile,id(1),146,eor,iamt)
               IF ( id(1)/10==1 ) nofrq = 1
               idata = 1
               spag_nextblock_1 = 4
            CASE (4)
!
               icore = core
               recs = 0
               group = nrecs
!
!     READ FIRST DATA BLOCK INTO CORE
!
!
!     INSUFFICIENT CORE,  IF FALL HERE, TO DO SORT II ON THIS FILE..
!
               CALL read(*75,*30,infile,Z(1),icore,noeor,iamt)
!
!     INSUFFICIENT CORE
!
               n = 19
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
 25            CALL close(infile,rwd)
               CALL close(oufile,rwd)
               CYCLE
!
 30            IF ( iamt==0 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
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
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
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
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
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
 35            ntypes = 1
 40            CALL read(*50,*125,infile,idtemp(1),146,eor,iamt)
               IF ( (id(2)==idtemp(2) .AND. id(3)==idtemp(3) .AND. id(5)/=idtemp(5)) .OR. (id(5)/=idtemp(5)) .OR. (id(4)/=idtemp(4))&
                  & ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
               ntypes = ntypes + 1
               nwords = idtemp(10)
!
!     WILL READ DATA AND COUNT ENTRYS
!
               IF ( ntypes>30 ) THEN
                  WRITE (L,99003) Ufm , ntypes
99003             FORMAT (A23,' 3129, SDR3 CAN ONLY PROCESS 30 ELEMENT TYPES, ','PROBLEM HAS',I5)
                  CALL mesage(-61,0,0)
                  GOTO 145
               ELSE
                  entrys(ntypes) = 0
                  DO
                     CALL read(*80,*40,infile,idtemp(1),nwords,noeor,iamt)
                     entrys(ntypes) = entrys(ntypes) + 1
                  ENDDO
               ENDIF
!
 45            ahead = 2*ntypes - 2
               IF ( ndata==1 ) GOTO 55
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
!
!     AT THIS POINT IT IS KNOWN HOW MANY TYPES ARE IN THE PRESENT GROUP
!     OF DATA BLOCKS AND ALSO HOW MANY ENTRYS IN EACH TYPE
!
 50            IF ( ntypes==1 ) eof = 1
               spag_nextblock_1 = 5
            CASE (5)
               itype = 1
               ndata = 1
               idata = 1
!
!     POSITION TO READ 2-ND ID OF TYPE(ITYPE) IF NOT JUST READ
!
               IF ( ntypes==1 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL rewind(infile)
               ahead = group + 2*ntypes
               spag_nextblock_1 = 6
            CASE (6)
               DO i = 1 , ahead
                  CALL fwdrec(*85,infile)
               ENDDO
               spag_nextblock_1 = 7
            CASE (7)
!
               CALL read(*55,*130,infile,idtemp(1),146,eor,iamt)
               spag_nextblock_1 = 8
            CASE (8)
!
!     CHECK FOR BREAK POINT
!
               IF ( nofrq/=1 ) THEN
                  IF ( id(4)/=idtemp(4) ) THEN
                     spag_nextblock_1 = 9
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               IF ( eof==1 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
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
                     GOTO 25
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
                  CALL read(*90,*135,infile,Z(npoint),nwds,ieor,iamt)
                  Z(npoint) = idtemp(5)
                  npoint = npoint + wperbk
               ENDDO
               vinbk = vinbk + 1
!
               IF ( ntypes/=1 ) THEN
                  IF ( itype/=1 ) THEN
                     IF ( idata==ndata ) THEN
                        spag_nextblock_1 = 9
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
!
!     NOW POSITION AHEAD TO READ NEXT ID FOR TYPE(ITYPE)
!
                  ahead = 2*ntypes - 2
                  DO i = 1 , ahead
                     CALL fwdrec(*95,infile)
                  ENDDO
               ENDIF
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
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
 55            eof = 1
               spag_nextblock_1 = 9
            CASE (9)
               IF ( recs/=0 ) THEN
!
                  layers = recs/nentry
!
!     CLOSE SCRATCH FILES AND OPEN AS INPUT FILES
!
                  DO i = 1 , nscrat
                     CALL close(scrtch(i),rwd)
                     ibuff = buff(i)
                     CALL open(*65,scrtch(i),Z(ibuff),inprwd)
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
                                 CALL fwdrec(*110,scrtch(nfile))
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
                                 CALL fwdrec(*105,scrtch(nfile))
                              ENDDO
                           ENDIF
                        ENDIF
!
!     COPY RECORD FROM SCRTCH TO OUTFILE
!
                        DO k = 1 , vperbk
                           ieor = k/vperbk
                           CALL read(*115,*140,scrtch(nfile),vector(1),nwds,ieor,iamt)
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
                     CALL open(*60,scrtch(i),Z(ibuff),outrwd)
                  ENDDO
               ENDIF
!
               IF ( itype==ntypes ) THEN
!
!     THIS GROUP IS ABSOLUTELY COMPLETE AND WE ARE AT BREAK POINT
!
                  IF ( eof==1 ) GOTO 25
                  nrecs = nrecs + 2*ndata*ntypes
                  IF ( ntypes>1 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSE
!
                  itype = itype + 1
                  CALL rewind(infile)
                  ahead = group + itype*2 - 2
                  DO i = 1 , ahead
                     CALL fwdrec(*100,infile)
                  ENDDO
                  ASSIGN 45 TO iretrn
                  eof = 0
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 10
            CASE (10)
               DO i = 1 , 146
                  id(i) = idtemp(i)
               ENDDO
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
!
!
!     ERROR CONDITIONS FOR THIS DATA BLOCK
!
!     FORMAT OF INPUT DATA BLOCK MAY BE INCORRECT (N=TRACEBACK CODE)
!
 60            n = 23
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 65            n = 3
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 70            n = 4
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 75            n = 5
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 80            n = 6
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 85            n = 7
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 90            n = 8
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 95            n = 9
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 100           n = 10
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 105           n = 11
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 110           n = 12
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 115           n = 13
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 120           n = 14
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 125           n = 15
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 130           n = 16
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 135           n = 17
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
 140           n = 18
               spag_nextblock_1 = 11
            CASE (11)
               Ofpfil(file) = n
               WRITE (L,99004) Uwm , file
99004          FORMAT (A25,' 982, FORMAT OF SDR3 INPUT DATA BLOCK ',I3,' DOES NOT PERMIT SUCCESSFUL SORT-2 PROCESSING.')
               GOTO 25
!
!     CORRESPONDING OUTPUT FILE IS PURGED.
!
 145           Ofpfil(file) = 2
               WRITE (L,99005) Uwm , file
99005          FORMAT (A25,' 984,  SDR3 FINDS OUTPUT DATA-BLOCK',I4,' PURGED.')
               GOTO 25
            CASE (12)
               WRITE (L,99006) Uwm , file
99006          FORMAT (A25,' 983, SDR3 HAS INSUFFICIENT CORE TO PERFORM SORT-2',' ON INPUT DATA BLOCK',I4,/5X,                      &
                      &'OR DATA-BLOCK IS NOT IN CORRECT FORMAT.')
               Ofpfil(file) = n
               GOTO 25
            END SELECT
         ENDDO SPAG_DispatchLoop_1
!
 150  ENDDO
!
!     CLOSE SCRATCH FILES
!
      DO i = 1 , nscrat
         CALL close(scrtch(i),rwd)
!
      ENDDO
   ENDIF
END SUBROUTINE sdr3a
