!*==fa1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fa1
   IMPLICIT NONE
   USE C_BLANK
   USE C_OUTPUT
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aero , flfact , fluter , imeth , ns
   INTEGER , SAVE :: bhh , bxhh , casecc , flist , fsave , khh , kxhh , mhh , mxhh , nmd , qhhl , scr1
   REAL , DIMENSION(12) :: block
   REAL :: bref , kfreq , rho , rref
   INTEGER :: buff , buff1 , fmethd , i , i165 , ibuf , ico , idum , iep , ifile , izx , lcc , lfl , ncore , neiw , nloops , np ,   &
            & nrho , nwr , pk , pm , pr , sk , sl , sm , smeth , sr
   REAL , DIMENSION(3) :: dlt
   INTEGER , DIMENSION(10) :: flut
   INTEGER , DIMENSION(12) :: iblock
   INTEGER , DIMENSION(4) , SAVE :: method
   REAL , DIMENSION(8) :: rec
   INTEGER , DIMENSION(8) :: rec0
   INTEGER , DIMENSION(10) , SAVE :: trl
   REAL , DIMENSION(1) :: z
   EXTERNAL close , cyct2b , fa1k , fa1ke , fa1pke , fa1pki , fname , gopen , klock , korsz , locate , mesage , open , pack ,       &
          & preloc , rdtrl , read , ssg2c , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     FA1 IS THE DRIVER FOR PART ONE OF FLUTTER ANALYSIS
!
   !>>>>EQUIVALENCE (rec0(1),rec(1))
   !>>>>EQUIVALENCE (block(1),iblock(1))
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA khh/101/ , bhh/102/ , mhh/103/ , qhhl/104/ , casecc/105/ , flist/106/ , fsave/201/ , kxhh/202/ , bxhh/203/ , mxhh/204/
   DATA scr1/301/ , ns/4HFA1  , 4H    /
   DATA imeth/4HS    , 4HL   /
   DATA nmd/4/ , method/4HK    , 4HKE   , 4HPK   , 4HINV /
   DATA trl/90 , 1006 , 7*0 , 6/
   DATA aero/3202 , 32/ , flfact/4102 , 41/ , fluter/3902 , 39/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         DO i = 1 , 12
            iblock(i) = 0
         ENDDO
         ncore = korsz(Iz)
         buff = ncore - Sysbuf - 1
         buff1 = buff - Sysbuf
         IF ( Floop/=0 ) THEN
            ifile = fsave
            CALL open(*280,fsave,Iz(buff+1),0)
            CALL read(*300,*300,fsave,Iz(1),8,1,nwr)
            CALL close(fsave,1)
            izx = 0
            fmethd = Iz(izx+3)
            smeth = Iz(izx+4)
            bref = z(izx+6)
            rref = z(izx+7)
            neiw = Iz(izx+8)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     FIRST TIME THROUGH FIND FMETHOD ON CASECC
!
            ifile = casecc
            CALL gopen(casecc,Iz(buff+1),0)
            CALL read(*300,*20,casecc,Iz,buff,1,nwr)
         ENDIF
 20      lcc = nwr
         CALL close(casecc,1)
!
!     GET DATA FOR REC0 OF FSAVE
!
         CALL fname(fsave,rec0)
         ifile = flist
         CALL preloc(*280,Iz(buff+1),flist)
         CALL locate(*260,Iz(buff+1),aero,idum)
         CALL read(*300,*300,flist,rec0(4),4,1,nwr)
         rec(6) = rec(6)*0.5
         CALL locate(*40,Iz(buff+1),flfact,idum)
         CALL read(*300,*60,flist,Iz(lcc+1),buff,1,nwr)
!
!     ERROR MESSAGES
!
         CALL mesage(-8,0,ns)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      nwr = 0
 60      lfl = nwr + lcc
         CALL locate(*240,Iz(buff+1),fluter,idum)
         SPAG_Loop_1_1: DO
            CALL read(*300,*240,flist,flut,10,0,nwr)
            i165 = 165
            IF ( flut(1)==Iz(i165) ) THEN
               CALL close(flist,1)
               rec0(8) = flut(9)
               iep = flut(10)
               DO i = 1 , nmd
                  IF ( flut(2)==method(i) ) EXIT SPAG_Loop_1_1
               ENDDO
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         rec0(3) = i
         fmethd = i
         IF ( i==3 ) THEN
!
!     PK METHOD HAS LINEAR SPLINE ONLY
!
            rec0(4) = 2
            smeth = 2
         ELSEIF ( i==4 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            rec0(4) = 0
            IF ( flut(7)==imeth(1) ) rec0(4) = 1
            IF ( flut(7)==imeth(2) ) rec0(4) = 2
            IF ( rec0(4)==0 ) THEN
               WRITE (Out,99001) Ufm , flut(7)
99001          FORMAT (A23,' 2267, INTERPOLATION METHOD ',A4,' UNKNOWN')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
               smeth = rec0(4)
            ENDIF
         ENDIF
!
!     BUILD RECORDS 0,1,2,3 OF SAVE
!
         ifile = fsave
         CALL open(*280,fsave,Iz(buff+1),1)
         CALL write(fsave,rec0,8,1)
         bref = rec(6)
         rref = rec(7)
         neiw = rec0(8)
!
!     BUILD M,K,RHO LIST FOR FLUTTER LOOP
!
         sr = 0
         sm = 0
         sk = 0
         i = lcc
         IF ( i==lfl ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_8: DO
            i = i + 1
            IF ( Iz(i)==flut(4) ) sr = i
            IF ( Iz(i)==flut(5) ) sm = i
            IF ( Iz(i)==flut(6) ) sk = i
            DO
               i = i + 1
               IF ( i>=lfl ) THEN
                  IF ( sr==0 .OR. sm==0 .OR. sk==0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  nrho = 0
                  pr = sr
                  DO
                     pr = pr + 1
                     IF ( Iz(pr)==-1 ) THEN
                        nloops = 0
                        IF ( fmethd/=3 ) THEN
!
!     ALGORITHM FOR BUILDING ELEMENTS OF FSAVE FOR K AND KE METHODS
!
!
!     OUTER LOOP ON MACH NUMBER
!
                           pm = sm
                           SPAG_Loop_4_2: DO
                              pm = pm + 1
                              IF ( Iz(pm)==-1 ) EXIT SPAG_Loop_4_2
                              dlt(1) = z(pm)
!
!     CENTER LOOP ON KFREQ
!
                              pk = sk
                              SPAG_Loop_5_3: DO
                                 pk = pk + 1
                                 IF ( Iz(pk)==-1 ) EXIT SPAG_Loop_5_3
                                 dlt(2) = z(pk)
!
!     INNER LOOP ON RHO
!
                                 pr = sr
                                 SPAG_Loop_6_4: DO
                                    pr = pr + 1
                                    IF ( Iz(pr)==-1 ) EXIT SPAG_Loop_6_4
                                    dlt(3) = z(pr)
                                    nloops = nloops + 1
                                    CALL write(fsave,dlt,3,0)
                                 ENDDO SPAG_Loop_6_4
                              ENDDO SPAG_Loop_5_3
                           ENDDO SPAG_Loop_4_2
                        ELSE
!
!     J.PETKAS/LOCKHEED      3/91
!     19 LINES OF OLD CODE FOR BUILDING ELEMENTS OF FSAVE FOR PK METHOD
!     WERE IN ERROR, AND ARE NOW REPLACED BY NEXT 29 NEW LINES
!
                           pm = sm
                           SPAG_Loop_4_5: DO
                              pm = pm + 1
                              IF ( Iz(pm)==-1 ) EXIT SPAG_Loop_4_5
                              dlt(1) = z(pm)
!
!     CENTER LOOP ON RHO
!
                              pr = sr
                              SPAG_Loop_5_6: DO
                                 pr = pr + 1
                                 IF ( Iz(pr)==-1 ) EXIT SPAG_Loop_5_6
                                 dlt(3) = z(pr)
!
!     INNER LOOP ON VELOCITY
!
                                 pk = sk
                                 SPAG_Loop_6_7: DO
                                    pk = pk + 1
                                    IF ( Iz(pk)==-1 ) EXIT SPAG_Loop_6_7
                                    dlt(2) = z(pk)
                                    nloops = nloops + 1
                                    CALL write(fsave,dlt,3,0)
                                 ENDDO SPAG_Loop_6_7
                              ENDDO SPAG_Loop_5_6
                           ENDDO SPAG_Loop_4_5
                        ENDIF
                        CALL write(fsave,0,0,1)
!
!     PICK UP M AND K FROM QHHL
!
                        ifile = qhhl
                        CALL open(*280,qhhl,Iz(buff1+1),0)
                        CALL read(*300,*80,qhhl,Iz(lcc+1),buff1,1,nwr)
                        CALL mesage(-8,0,ns)
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        nrho = nrho + 1
                     ENDIF
                  ENDDO
               ELSEIF ( Iz(i)==-1 ) THEN
                  CYCLE SPAG_Loop_1_8
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_8
         ENDDO SPAG_Loop_1_8
 80      lfl = nwr + lcc
         sl = lcc + 5
         CALL close(qhhl,1)
         rec0(1) = qhhl
         CALL rdtrl(rec0)
         np = min0(Iz(sl-1),rec0(2)/rec0(3))
         lfl = min0(lfl,2*np+sl-1)
         np = lfl - sl + 1
         CALL write(fsave,Iz(sl),np,1)
         np = np/2
!
!     WRITE CASECC RECORD AND TRAILER
!
         CALL write(fsave,Iz(1),lcc,1)
         CALL close(fsave,1)
         rec0(1) = fsave
         rec0(2) = Floop
         rec0(3) = nloops
         rec0(4) = np
         rec0(5) = lcc
         rec0(6) = 0
         rec0(7) = nrho
         CALL wrttrl(rec0)
         spag_nextblock_1 = 2
      CASE (2)
         rec0(1) = fsave
         CALL rdtrl(rec0)
!
!     START OF LOOPING BUMP LOOP COUNTER SET TIME AND GO
!
         Floop = Floop + 1
         nloops = rec0(3)
         CALL klock(Tstart)
         IF ( fmethd==2 ) THEN
!
!     KE METHOD DO INCORE EIGNVALUE EXTRACTION
!
            rec0(1) = bhh
            CALL rdtrl(rec0)
            IF ( rec0(1)>0 .AND. rec0(7)>0 ) THEN
               WRITE (Out,99002) Ufm , flut(2)
99002          FORMAT (A23,', FLUTTER METHOD ',A4,' NOT IMPLEMENTED WITH B ','MATRIX')
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
               rec0(1) = khh
               CALL rdtrl(rec0)
               ico = rec0(2)*rec0(2)*4 + 4
               DO
                  CALL fa1k(smeth,kfreq,rho,scr1,ico)
                  CALL fa1ke(scr1,kfreq,bref,rho,rref,Floop,nloops)
                  IF ( Floop>=nloops ) GOTO 220
                  Floop = Floop + 1
               ENDDO
            ENDIF
         ELSEIF ( fmethd==3 ) THEN
            SPAG_Loop_1_9: DO
!
!     PK METHOD  LINEAR INTERPOLATION  AND INCORE LOOP FOR
!     EIGENVALUE CONVERGENCE
!
               CALL fa1pki(fsave,qhhl)
               CALL fa1pke(khh,bhh,mhh,bxhh,fsave,nloops,bref,rref,neiw,iep)
               IF ( Floop>=nloops ) THEN
!
!     PHID  - KXHH   CLAMAD - BXHH
!
                  ibuf = buff1 - Sysbuf
                  trl(1) = scr1
                  CALL rdtrl(trl)
                  IF ( trl(2)==0 ) GOTO 220
                  CALL open(*220,scr1,z(ibuf),0)
                  CALL read(*200,*100,scr1,rec,6,1,nwr)
                  EXIT SPAG_Loop_1_9
               ELSE
                  Floop = Floop + 1
               ENDIF
            ENDDO SPAG_Loop_1_9
         ELSEIF ( fmethd==4 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     K METHOD BUILD PROPER QHH ON SCR1
!
            CALL fa1k(smeth,kfreq,rho,scr1,0)
!
!     COPY KHH TO KXHH
!
            CALL gopen(khh,Iz(buff+1),0)
            CALL gopen(kxhh,Iz(buff1+1),1)
            rec0(1) = khh
            CALL rdtrl(rec0)
            rec0(1) = kxhh
            Iout = rec0(5)
            Incr = 1
            i = rec0(2)
            rec0(2) = 0
            rec0(6) = 0
            rec0(7) = 0
            CALL cyct2b(khh,kxhh,i,z,rec0)
            CALL close(khh,1)
            CALL close(kxhh,1)
            CALL wrttrl(rec0)
!
!     BUILD BXHH = (K/B)BHH
!
            rec0(1) = bhh
            CALL rdtrl(rec0)
            IF ( rec0(1)>0 ) THEN
               iblock(2) = 1
               block(3) = kfreq/bref
               CALL ssg2c(bhh,0,bxhh,0,block(2))
            ENDIF
!
!                2  2
!     MXHH  =  (K /B ) MHH  + (RHO*RREF/2.0) QHH
!
            iblock(2) = 1
            block(3) = (kfreq*kfreq)/(bref*bref)
            iblock(8) = 1
            block(9) = rho*rref/2.0
            CALL ssg2c(mhh,scr1,mxhh,0,block(2))
            GOTO 220
         ENDIF
 100     CALL read(*200,*120,scr1,z,ibuf,1,nwr)
 120     Nn = nwr/2
         CALL gopen(kxhh,z(buff),1)
         CALL gopen(bxhh,z(buff1),1)
         CALL write(bxhh,trl(1),50,0)
         CALL write(bxhh,Hdg,96,1)
         trl(1) = kxhh
         trl(2) = 0
         trl(3) = Nn
         trl(4) = 2
         trl(5) = 3
         Iti = 3
         Ito = 3
         Ij = 1
         Incr1 = 1
 140     CALL write(bxhh,rec,6,0)
         CALL pack(z,kxhh,trl)
         CALL read(*180,*160,scr1,rec,6,1,nwr)
 160     CALL read(*180,*140,scr1,z,ibuf,1,nwr)
 180     CALL write(bxhh,0,0,1)
         CALL close(bxhh,1)
         CALL close(kxhh,1)
         CALL wrttrl(trl)
         trl(1) = bxhh
         trl(2) = 1006
         trl(7) = 0
         CALL wrttrl(trl)
 200     CALL close(scr1,1)
!
!     THE END
!
 220     rec0(1) = fsave
         CALL rdtrl(rec0)
         rec0(2) = Floop
         CALL wrttrl(rec0)
         IF ( Floop==nloops ) Floop = -1
         Icead = 1
         IF ( fmethd==2 ) Icead = -1
         IF ( fmethd==3 ) Icead = -1
         RETURN
      CASE (3)
         WRITE (Out,99003) Ufm , flut(4) , flut(5) , flut(6)
99003    FORMAT (A23,', ONE OR MORE OF THE FOLLOWING FLFACT SETS WERE NOT',' FOUND - ',3I9)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 240     i165 = 165
         WRITE (Out,99004) Ufm , Iz(i165)
99004    FORMAT (A23,' 2268, FMETHOD SET',I9,' NOT FOUND')
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 260     CALL mesage(-7,0,ns)
 280     CALL mesage(-1,ifile,ns)
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (Out,99005) Ufm , flut(2)
99005    FORMAT (A23,' 2269, FLUTTER METHOD ',A4,' NOT IMPLEMENTED')
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 300     CALL mesage(-3,ifile,ns)
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(-61,0,ns)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE fa1
