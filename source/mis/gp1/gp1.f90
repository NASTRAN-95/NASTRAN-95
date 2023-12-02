!*==gp1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp1
   USE c_blank
   USE c_condas
   USE c_gpta1
   USE c_names
   USE c_setup
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(34) :: a
   REAL , DIMENSION(34) :: aa
   REAL , DIMENSION(3) :: ab , ac , ai , aj , ak , ar , ax
   REAL :: axi , length , r , rcth , rsth
   INTEGER :: axic , buf1 , buf2 , buf3 , file , flag , geomp , gpfl , i , iaxif , icfiat , icsdt , icsgp , idseq1 , idseq2 ,       &
            & idseq3 , ierr , ifail , ifl , igpdt , ii , ij , ijk , ilist , ilist0 , imax , incr , iout , irmndr , isubs , itherm , &
            & itype , j , j1 , jerr , jj , k , khi , khr , kk , klo , kn , l , last , lmt1 , m , m8 , maxa1 , maxdof , mult , n ,   &
            & n1 , n2 , nam , nbpw , ncode , ncore , ncsdt , ncsgp , ndx , neqex , nerr , nlist , nn , nogmp1 , nogo , nogrid ,     &
            & nolist , nosclr , noseq , nread , nskip , nwds , nz , offset , solv , solvp , sysbuf , type
   INTEGER , SAVE :: bgpdt , cstm , eqexin , geom1 , geom2 , gpdt , gpl , large , scr1 , scr2 , sil
   INTEGER , DIMENSION(6) , SAVE :: cord
   INTEGER , DIMENSION(12) , SAVE :: cordij
   INTEGER , DIMENSION(2) , SAVE :: gp1ah , grid , scalpt , seqgp
   CHARACTER(29) , SAVE :: lvl1 , lvl2
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER , DIMENSION(2) :: spoint
   REAL , DIMENSION(1) :: zz
   EXTERNAL bisloc , close , delset , diag36 , ectloc , fname , fwdrec , korsz , locate , mesage , open , page2 , preloc , read ,   &
          & rewind , rshift , sorti , sswtch , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     GP1  BUILDS THE FOLLOWING DATA BLOCKS--
!       1. GRID POINT LIST (GPL)
!       2. EXTERNAL INTERNAL GRID POINT EQUIVALENCE TABLE (EQEXIN)
!       3. GRID POINT DEFINITION TABLE (GPDT)
!       4. COORDINATE SYSTEM TRANSFORMATION MATRICES (CSTM)
!       5. BASIC GRID POINT DEFINITION TABLE (BGPDT)
!       6. SCALAR INDEX LIST (SIL)
!
!     THE FOLLOWING CARDS ARE READ BY GP1--
!       1. GRID
!       2. CELASI, CDAMPI, CMASSI  (I=1,2,3,4)
!       3. SPOINT
!       4. SEQGP   (SEQEP IS PROCESSED IN DPD1)
!       5. CORDIJ  (I=1,2,  J=R,S,C)
!
!     IMPORTANT
!     =========
!     REVISED  7/89 BY G.CHAN/UNISYS, TO ALLOW GRID, SCALAR AND EXTRA
!     POINT EXTERNAL ID UP TO 8 DIGITS FOR ALL 32-BIT MACHINES
!     PREVIOUSLY, ID OF 2000000 IS THE UPPER LIMIT FOR IBM AND VAX
!
!     REVISED  8/89 BY G.CHAN/UNISYS, AS PART OF THE EFFORT TO ALLOW A
!     NASTRAN JOB TO EXCEED 65535 LIMIT.
!     NORMALLY, IF GRID POINTS OR SCALAR POINTS DO NOT HAVE VERY LARGE
!     EXTERNAL ID NUMBERS, THEIR ID NOS. ARE MULTIPLIED BY 1000, SO THAT
!     999 ADDITIONAL POINTS CAN SQUEEZE IN VIA SEQGP CARDS. (NOTE - A
!     7- OR 8-DIGIT ID NO., TIMES 1000, EXCEEDS A 32-BIT WORD COMPUTER
!     HARDWARE LIMIT). THIS MULTIPLY FACTOR IS NOW ADJUSTABLE, 1000,100,
!     OR 10, SO THAT ADDITIONAL DIGITS CAN BE USED FOR THE EXTERNAL GRID
!     OR SCALAR POINTS IN CASE THERE ARE LIMITTED SEQGP CARDS PRESENT.
!     THIS VARIABLE MULTIPLIER (10,100, OR 1000) IS ALSO RECORDED IN THE
!     3RD WORD OF THE HEADER RECORD OF THE GPL DATA BLOCK FOR LATER USE.
!     THE ACTUAL FACTOR OF THE MULTIPLIER IS ALSO MACHINE DEPENDENT.
!     UNIVAC, A 36-BIT MACHINE, CAN HAVE A MULTIPLIER OF 100 OR 1000.
!     OTHER 60- OR 64- BIT MACHINES, THE MULTIPLIER REMAINS AT 1000
!     IF THE MULTIPLIER IS 1000, THE SEQGP AND SEQEP CARDS, AS BEFORE,
!     CAN HAVE 4 SEQID LEVELS, SUCH AS XXX.X.X.X
!     IF THE MULTIPLIER IS 100, SEQGP AND SEQEP CARDS ARE LIMITED TO
!     3 SEQID LEVELS, XXX.X.X
!     FINALLY, IF MULTIPLIER IS 10, SEQGP AND SEQEP ARE LIMITED TO XXX.X
!
!     SPECIAL CONSIDERATION FOR THE AXISYM. AND HYDROELAS. PROBLEMS - 10
!     IS USED FOR THE MULTIPLIER, AND THEREFOR A ONE SEQID LEVEL IS
!     AVAILABLE. PREVIOUSLY, SEQGP CARDS WERE NOT USED IN AXISYM. AND
!     HYDROELAS. PROBLEMS, AND NO USER WARNING MESSAGE PRINTED
!
!     NO ADJUSTABLE MULTIPLY FACTOR FOR SUBRSTRUCTURING (MULT=1000,
!     SEE ALSO SGEN)
!
!     THE 65535 LIMITATION INVOLVES ONLY A SAMLL CHANGE IN STA 973
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iout) , (Ksystm(24),Icfiat) , (Ksystm(27),Axic) , (Ksystm(38),Iaxif) ,               &
!>>>>    & (Ksystm(40),Nbpw) , (Ksystm(56),Itherm) , (Ksystm(69),Isubs)
   !>>>>EQUIVALENCE (Z(1),Zz(1)) , (a(1),aa(1)) , (a(4),ab(1)) , (a(7),ac(1)) , (a(10),ai(1)) , (a(13),aj(1)) , (a(16),ak(1)) ,          &
!>>>>    & (a(19),ax(1)) , (a(22),ar(1)) , (Nocstm,Ifl) , (geomp,geom1) , (mcb(2),kn)
   !>>>>EQUIVALENCE (igpdt,icsdt)
   DATA geom1/101/ , geom2/102/ , gpl/201/ , eqexin/202/ , gpdt/203/ , cstm/204/ , bgpdt/205/ , sil/206/ , scr1/301/ , scr2/302/
   DATA gp1ah/4HGP1  , 4H    / , cord/6 , 6 , 6 , 13 , 13 , 13/ , grid/4501 , 45/ , seqgp/5301 , 53/ , cordij/1701 , 17 , 1801 ,    &
      & 18 , 1901 , 19 , 2001 , 20 , 2101 , 21 , 2201 , 22/ , scalpt/5551 , 49/
   DATA mcb/7*0/ , large/100000000/ , lvl1/'3  I.E.  XXX.X.X.X TO XXX.X.X'/ , lvl2/'2  I.E.  XXX.X.X.X TO XXX.X  '/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM GENERAL INITIALIZATION
!
         CALL delset
         nz = korsz(z)
         buf1 = nz - sysbuf - 2
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         nogo = 0
         nocstm = 0
         nogpdt = -1
         nogmp1 = 1
         maxa1 = 0
         mult = 1000
         axi = 0
         IF ( axic/=0 .OR. iaxif/=0 ) axi = 1
         IF ( axi/=0 ) mult = 10
         IF ( isubs/=0 ) mult = 1000
         imax = large
         IF ( nbpw==32 ) imax = 2147483
         IF ( nbpw==36 ) imax = 34359738
!         2147483=2**31/1000   34359738=2**35/1000
!
!     READ SCALAR ELEMENT CONNECTION CARDS (IF PRESENT).
!     EXTRACT SCALAR POINTS AND WRITE THEM ON SCR2.
!
         file = scr2
         CALL open(*560,scr2,z(buf2),wrtrew)
         nosclr = 0
         m8 = -8
         a(11) = -1
         DO k = 12 , 16
            a(k) = 0
         ENDDO
         CALL preloc(*60,z(buf1),geom2)
         i = 1
         DO i = 1 , lastx , incrx
            kk = elem(i+10)
            IF ( kk/=0 ) THEN
               CALL locate(*20,z(buf1),elem(i+3),flag)
               nn = elem(i+5)
               DO
                  CALL read(*580,*20,geom2,a,nn,0,flag)
                  DO k = 3 , 4
                     IF ( .NOT.(a(k)==0 .OR. (kk==1 .AND. a(k+2)/=0)) ) THEN
                        a(10) = a(k)
                        nosclr = 1
                        CALL write(scr2,a(10),1,0)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
 20      ENDDO
!
!     COPY SCALAR POINTS DEFINED ON SPOINT CARDS (IF PRESENT) ONTO SCR2.
!
         CALL locate(*60,z(buf1),scalpt,flag)
         nosclr = 1
         CALL read(*580,*40,geom2,z,buf2-1,1,n)
         CALL mesage(m8,0,gp1ah)
 40      CALL write(scr2,z,n,0)
!
!     CLOSE FILES. IF SCALAR POINTS PRESENT, SORT LIST.
!     THEN DISCARD DUPLICATES AND WRITE UNIQUE LIST ON SCR2.
!
 60      CALL write(scr2,0,0,1)
         CALL close(scr2,clsrew)
         CALL close(geom2,clsrew)
         IF ( nosclr==0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nfile(1) = gpdt
         nfile(2) = bgpdt
         nfile(3) = sil
         CALL open(*560,scr2,z(buf1),rdrew)
         CALL sorti(scr2,0,1,1,z,buf1-1)
         CALL close(scr2,clsrew)
         file = nfile(6)
         CALL open(*560,file,z(buf1),rdrew)
         CALL open(*560,scr2,z(buf2),wrtrew)
         last = -1
         DO
            CALL read(*580,*80,file,a(10),1,0,flag)
            IF ( a(10)/=last ) THEN
               CALL write(scr2,a(10),1,0)
               last = a(10)
            ENDIF
         ENDDO
 80      CALL write(scr2,0,0,1)
         CALL close(scr2,clsrew)
         CALL close(file,clsrew)
         CALL open(*560,scr2,z(buf3),rdrew)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ GRID ENTRIES (IF PRESENT).
!     MERGE GRID AND SCALAR NOS.
!     CREATING LIST IN CORE OF EXTERNAL NO., MULT * EXTERNAL NO.
!     WRITE 7-WORD GRID AND SCALAR ENTRIES ON SCR1.
!
         a(1) = large
         a(10) = large
         file = scr1
         IF ( maxa1==0 ) CALL open(*560,scr1,z(buf2),wrtrew)
         i = -1
         nogrid = 0
         IF ( maxa1==0 ) CALL preloc(*140,z(buf1),geom1)
         CALL locate(*160,z(buf1),grid,flag)
         nogrid = 1
         CALL read(*580,*600,geom1,a,8,0,flag)
         CALL write(scr1,a,7,0)
         spag_nextblock_1 = 3
      CASE (3)
         IF ( nosclr==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*580,*600,scr2,a(10),1,0,flag)
         CALL write(scr1,a(10),7,0)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( nogrid==0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nosclr/=0 ) THEN
            IF ( a(1)<a(10) ) THEN
            ELSEIF ( a(1)==a(10) ) THEN
               spoint(1) = a(1)
               spoint(2) = 0
               ierr = 12
               CALL mesage(-30,ierr,spoint)
               GOTO 600
            ELSE
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     GRID NO. .LT. SCALAR NO.
!
         i = i + 2
         z(i) = a(1)
!
!     GRID POINT EXTERNAL ID * MULT IS LIMITED TO COMPUTER MAXIMUM
!     INTEGER SIZE
!
         IF ( a(1)<=imax .OR. axi/=0 ) THEN
            z(i+1) = mult*a(1)
         ELSE
            IF ( a(1)>maxa1 ) maxa1 = a(1)
         ENDIF
         CALL read(*580,*100,geom1,a,8,0,flag)
         CALL write(scr1,a,7,0)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     nogrid = 0
         a(1) = large
         IF ( nosclr==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     SCALAR NO. .LT. GRID NO.
!
         i = i + 2
         z(i) = a(10)
!
!     SCALAR POINT EXTERNAL ID * MULT IS LIMITED TO COMPUTER MAXIMUM
!     INTEGER SIZE
!
         IF ( a(10)<=imax .OR. axi/=0 ) THEN
            z(i+1) = mult*a(10)
         ELSE
            IF ( a(10)>maxa1 ) maxa1 = a(10)
         ENDIF
         CALL read(*580,*120,scr2,a(10),1,0,flag)
         CALL write(scr1,a(10),7,0)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 120     nosclr = 0
         a(10) = large
         IF ( nogrid/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     LIST COMPLETE ONLY IF MAXA1 .LE. ZERO
!
!     IF MAXA1 IS .GT. ZERO, SOME LARGE GRID OR SCALAR POINTS HAD BEEN
!     LEFT OUT IN LIST. MAXA1 IS THE LARGEST GRID OR SCALAR POINT
!     EXTERNAL ID.  RESET MULT AND REPEAT COMPILING LIST
!
         IF ( maxa1>0 ) THEN
            IF ( isubs/=0 ) THEN
!
               WRITE (iout,99001) ufm
99001          FORMAT (A23,' 2140B, EXTERNAL GRID OR SCALAR POINT ID TOO BIG')
               CALL mesage(-61,0,0)
            ELSE
               CALL rewind(scr1)
               CALL rewind(geom1)
               IF ( nosclr/=0 ) CALL rewind(scr2)
               mult = 100
               IF ( maxa1>imax*10 ) mult = 10
               imax = (imax/mult)*1000
               maxa1 = -1
!WKBR CALL PAGE (-3)
               CALL page2(-3)
               IF ( mult==100 ) WRITE (iout,99014) uwm , lvl1
               IF ( mult==10 ) WRITE (iout,99014) uwm , lvl2
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
         n = i
         neqex = n
         n1 = n + 1
         n2 = n + 2
         igpdt = n2
         ilist = n2
         kn = n1/2
         CALL close(scr1,clsrew)
         CALL close(scr2,clsrew)
!
!     READ THE SEQGP TABLE (IF PRESENT)
!     FOR EACH ENTRY, FIND MATCH IN THE SORTED EXTERNAL GRID POINTS
!     AND REPLACE SEQUENCE NO. WITH SEQGP NO.
!
         noseq = 0
         nogpdt = 1
         IF ( nogmp1==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ASSIGN 220 TO ndx
         spoint(2) = 0
         ierr = 1
         ASSIGN 200 TO nerr
         CALL locate(*240,z(buf1),seqgp,flag)
         noseq = 1
         ifail = 0
         DO
            CALL read(*580,*180,geomp,z(n2),buf1-1,1,flag)
            ifail = ifail + 1
         ENDDO
!
!     NO GRID CARDS PRESENT-- TEST FOR ANY SCALAR PTS.
!
 140     nogmp1 = 0
 160     IF ( nosclr/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ABNORMAL EXIT FROM GP1
!
         CALL close(scr1,clsrew)
         CALL close(geom1,clsrew)
         nocstm = -1
         RETURN
 180     IF ( ifail/=0 ) THEN
            nwds = (ifail-1)*(buf1-1) + flag
            WRITE (iout,99002) ufm , nwds
99002       FORMAT (A23,' 3135, UNABLE TO PROCESS SEQGP DATA IN SUBROUTINE ','GP1 DUE TO INSUFFICIENT CORE.',//5X,                  &
                   &'ADDITIONAL CORE REQUIRED =',I10,7H  WORDS)
            CALL mesage(-61,0,0)
         ENDIF
!
!     CHECK FOR MULTIPLE REFERENCES TO GRID (OR SCALAR) POINT ID NOS.
!     AND SEQUENCE ID NOS. ON SEQGP CARDS
!
         k = n2
         kk = n2 + flag - 1
         jj = kk - 2
         SPAG_Loop_1_2: DO
            DO i = k , jj , 2
               IF ( z(i)>=0 .AND. i<kk ) THEN
                  ii = i + 2
                  ifail = 0
                  DO j = ii , kk , 2
                     IF ( z(i)==z(j) ) THEN
                        IF ( ifail==0 ) THEN
                           ifail = 1
                           nogo = 1
                           IF ( k/=n2 ) THEN
                              idseq1 = z(i)/1000
                              irmndr = z(i) - 1000*idseq1
                              IF ( irmndr/=0 .AND. mult>=10 ) THEN
                                 idseq2 = irmndr/100
                                 irmndr = irmndr - 100*idseq2
                                 IF ( irmndr/=0 .AND. mult>=100 ) THEN
                                    idseq3 = irmndr/10
                                    irmndr = irmndr - 10*idseq3
                                    IF ( irmndr/=0 ) THEN
                                       WRITE (iout,99003) ufm , idseq1 , idseq2 , idseq3 , irmndr
99003                                  FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,1H.,I1,         &
                                         &'  ON SEQGP CARDS.')
                                    ELSE
                                       WRITE (iout,99004) ufm , idseq1 , idseq2 , idseq3
99004                                  FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,1H.,I1,4X,             &
                                         &'ON SEQGP CARDS.')
                                    ENDIF
                                 ELSE
                                    WRITE (iout,99005) ufm , idseq1 , idseq2
99005                               FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,1H.,I1,5X,'ON SEQGP CARDS.')
                                 ENDIF
                              ELSEIF ( axi/=0 ) THEN
                                 IF ( axi==1 ) WRITE (iout,99015) ufm
                                 axi = 2
                                 nogo = 1
                              ELSE
                                 WRITE (iout,99006) ufm , idseq1
99006                            FORMAT (A23,' 3137, MULTIPLE REFERENCES TO SEQUENCE ID NO.',I6,6X,' ON SEQGP CARDS.')
                              ENDIF
                           ELSE
                              WRITE (iout,99007) ufm , z(i)
99007                         FORMAT (A23,' 3136, MULTIPLE REFERENCES TO GRID (OR SCALAR) POINT',' ID NO.',I9,'  ON SEQGP CARDS.')
                           ENDIF
                        ENDIF
                        z(j) = -z(j)
                     ENDIF
                  ENDDO
               ENDIF
!
               IF ( jj>=kk .AND. mult/=1000 ) THEN
                  l = z(i)
                  IF ( mult<=10 ) THEN
                     IF ( mult==1 ) THEN
                        IF ( axi==0 ) CALL mesage(-37,0,nam)
                        IF ( mod(l,1000)/=0 ) THEN
                           IF ( axi==1 ) WRITE (iout,99015) ufm
                           axi = 2
                           nogo = 1
                        ENDIF
                        CYCLE
                     ELSEIF ( mod(l,100)==0 ) THEN
                        z(i) = l/100
                        CYCLE
                     ENDIF
                  ELSEIF ( mod(l,10)==0 ) THEN
                     z(i) = l/10
                     CYCLE
                  ENDIF
                  IF ( maxa1/=0 ) THEN
                     maxa1 = 0
                     nogo = 1
                     WRITE (iout,99008) ufm
99008                FORMAT (A23,' 2140B, ILLEGAL DATA IN SEQGP CARD, POSSIBLY CAUSED',' BY LARGE GRID OR SCALAR POINTS')
                  ENDIF
               ENDIF
            ENDDO
!
            IF ( k/=n2 ) THEN
!
               DO i = n2 , kk , 2
                  IF ( z(i)<0 ) z(i) = -z(i)
               ENDDO
               IF ( nogo/=1 ) THEN
!
!     CHECK TO SEE IF ANY SEQUENCE ID NO. ON SEQGP CARDS IS THE SAME
!     AS A GRID (OR SCALAR) POINT ID NO. THAT HAS NOT BEEN RESEQUENCED
!
                  SPAG_Loop_2_1: DO i = k , kk , 2
                     IF ( z(i)>=0 ) THEN
                        idseq1 = z(i)/mult
                        irmndr = z(i) - mult*idseq1
                        IF ( irmndr==0 ) THEN
                           DO j = n2 , kk , 2
                              IF ( idseq1==z(j) ) CYCLE SPAG_Loop_2_1
                           ENDDO
                           DO j = 1 , n1 , 2
                              IF ( idseq1==z(j) ) GOTO 182
                           ENDDO
                        ENDIF
                        CYCLE
 182                    nogo = 1
                        WRITE (iout,99009) ufm , idseq1
99009                   FORMAT (A23,' 3138, SEQUENCE ID NO.',I6,' ON SEQGP CARDS IS THE ','SAME AS A',/5X,                          &
                               &'GRID (OR SCALAR) POINT ID NO. THAT HAS ','NOT BEEN RESEQUENCED.')
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
               i = -1
               EXIT SPAG_Loop_1_2
            ELSE
               jj = kk
               k = k + 1
            ENDIF
         ENDDO SPAG_Loop_1_2
 200     i = i + 2
         IF ( i<=flag ) THEN
            a(1) = z(n2+i-1)
            a(2) = z(n2+i)
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
!
!     SORT THE CORE TABLE BY INTERNAL GRID PT NO
!     THUS FORMING THE GPL (EXTERNAL GRID PT NOS IN SORT BY INTERNAL NO)
!
         ELSEIF ( nogo/=0 ) THEN
            CALL mesage(-61,0,0)
            GOTO 560
         ELSE
            CALL sorti(0,0,2,2,z,n1)
            GOTO 240
         ENDIF
 220     z(2*k) = a(2)
         GOTO 200
!
!     CLOSE GEOM1. WRITE THE GPL. FIRST RECORD IS A SINGE ENTRIED LIST
!     OF EXTERNAL GRID NOS. IN INTERNAL SORT. SECOND RECORD IS A DOUBLE
!     ENTRIED LIST OF EXTERAL GRID NO., SEQUENCE NO. (SORT IS INTERNAL).
!     ADD THE MULTIPLIER, MULT, TO THE 3RD WORD OF GPL HEADER RECORD
!
 240     IF ( nogmp1/=0 ) CALL close(geom1,clsrew)
         spag_nextblock_1 = 8
      CASE (8)
         CALL fname(gpl,a)
         file = gpl
         CALL open(*560,gpl,z(buf1),wrtrew)
         a(3) = mult
         CALL write(gpl,a,3,1)
         DO i = 1 , n , 2
            CALL write(gpl,z(i),1,0)
         ENDDO
         CALL write(gpl,0,0,1)
         CALL write(gpl,z,n1,1)
         CALL close(gpl,clsrew)
         mcb(1) = gpl
         CALL wrttrl(mcb)
!
!     FORM INTERNAL INDEX FOR EACH EXTERNAL GRID PT. NO.
!
         i = 2
         z(i) = 1
         IF ( n/=1 ) THEN
            DO i = 3 , n , 2
               z(i+1) = z(i-1) + 1
            ENDDO
!
!     TEST TO SEE IF EXTERNAL GRID PT NOS ARE STILL IN EXTERNAL SORT
!     I.E., IF NO SEQGP TABLE, THEN SORT IS MAINTAINED
!     OTHERWISE, SORT ON EXTERNAL GRID NO.
!
            IF ( noseq/=0 ) CALL sorti(0,0,2,1,z,n1)
         ENDIF
!
!     DETERMINE IF THE GPDT CAN BE HELD IN CORE
!     NWDS= TOTAL NO OF WORDS IN THE GPDT
!     M= MAX NO OF ENTRIES CORE CAN HOLD WITH ONE BUFFER OPEN
!     IF NWDS/7.LE.M,CORE WILL HOLD THE GPDT
!     OTHERWISE THE FILE SORT ROUTINE WILL BE USED
!
         nwds = 7*kn
         m = (buf1-n1)/7
         gpfl = 0
         IF ( kn>m ) gpfl = 7
         file = scr1
!
!     READ THE GRID AND SPOINT TABLES FROM SCR1
!     REPLACE THE EXTERNAL GRID PT NO WITH THE INTERNAL INDEX
!     IF CORE WILL HOLD THE GPDT, USE THE INTERNAL INDEX AS A POINTER
!     OTHERWISE, WRITE THE UNSORTED GPDT ON SCR2
!
         CALL open(*560,scr1,z(buf1),rdrew)
         file = scr2
         IF ( gpfl/=0 ) CALL open(*560,scr2,z(buf2),wrtrew)
         file = scr1
         ASSIGN 280 TO ndx
         ierr = 2
         ASSIGN 260 TO nerr
 260     CALL read(*580,*300,scr1,a,7,0,flag)
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 280     IF ( gpfl/=0 ) THEN
            CALL write(scr2,a,7,0)
         ELSE
            j = n1 + 7*(a(1)-1)
            DO k = 1 , 7
               i = j + k
               z(i) = a(k)
            ENDDO
         ENDIF
         GOTO 260
 300     IF ( nogo/=0 ) THEN
            CALL mesage(-61,0,0)
            GOTO 560
         ELSE
            CALL close(scr1,clsrew)
!
!     OPEN OUTPUT FILE FOR GPDT AND WRITE HEADER DATA
!     IF GPDT IS IN CORE, WRITE IT OUT
!
            file = gpdt
            CALL fname(gpdt,a)
            CALL open(*560,gpdt,z(buf1),wrtrew)
            CALL write(gpdt,a,2,1)
            IF ( gpfl/=0 ) THEN
!
!     IF GPDT NOT IN CORE, CALL SORT
!
               nfile(1) = scr1
               nfile(2) = cstm
               nfile(3) = bgpdt
               CALL close(scr2,clsrew)
               file = scr2
               CALL open(*560,scr2,z(buf2),rdrew)
               CALL sorti(scr2,gpdt,7,1,z(igpdt),buf2-igpdt)
               CALL close(scr2,clsrew)
            ELSE
               CALL write(gpdt,z(igpdt),nwds,1)
            ENDIF
            CALL close(gpdt,clsrew)
            mcb(1) = gpdt
            CALL wrttrl(mcb)
!
!     READ THE CORDIJ TABLES INTO CORE (IF PRESENT)
!
            ifl = -1
            m = icsdt
            nolist = 0
            IF ( nogmp1==0 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ndx = buf1 - 15
            ncore = buf1 - 15
            DO i = icsdt , buf1
               z(i) = 0
            ENDDO
            file = geomp
            CALL preloc(*560,z(buf1),geomp)
            DO i = 1 , 6
               ij = i + i - 1
               CALL locate(*310,z(buf1),cordij(ij),flag)
               ifl = 1
               DO
                  CALL read(*580,*310,geomp,z(m),cord(i),0,flag)
                  m = m + 16
                  IF ( m>ncore ) CALL mesage(-8,0,gp1ah)
               ENDDO
 310        ENDDO
            CALL close(geomp,clsrew)
            m = m - 16
            ncsdt = m
!
!     TEST FOR PRESENCE OF ANY CORDIJ TABLES
!
            IF ( ifl==-1 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     REPLACE EXTERNAL GRID PT NO IN CORD1J ENTRIES (IF ANY)
!     WITH CORRESPONDING INTERNAL INDEX
!     SAVE A TABLE OF GRID PTS REFERENCED ON CORD1J ENTRIES
!
            jj = icsdt
            ilist = ncsdt + 16
            ii = ilist - 1
            ncore = buf1 - 3
            ierr = 3
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         IF ( z(jj+2)/=1 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nolist = 1
         ASSIGN 320 TO ndx
         ASSIGN 340 TO nerr
         a(1) = z(jj+3)
         spoint(2) = z(jj+1)
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 320     z(jj+3) = a(1)
         z(ii+1) = a(1)
 340     ASSIGN 360 TO ndx
         ASSIGN 380 TO nerr
         a(1) = z(jj+4)
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 360     z(jj+4) = a(1)
         z(ii+2) = a(1)
 380     ASSIGN 400 TO ndx
         ASSIGN 420 TO nerr
         a(1) = z(jj+5)
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 400     z(jj+5) = a(1)
         z(ii+3) = a(1)
 420     ii = ii + 3
         IF ( ii>ncore ) CALL mesage(-8,0,gp1ah)
         spag_nextblock_1 = 10
      CASE (10)
         jj = jj + 16
         IF ( jj<=ncsdt ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nogo/=0 ) THEN
            CALL mesage(-61,0,0)
            GOTO 560
         ELSE
!
!     IF ANY CORD1J ENTRIES, PASS THE GPDT AND CREATE A TABLE OF THE
!     REFERENCED GRID PTS. THIS TABLE IS CALLED CSGP
!
            IF ( nolist/=0 ) THEN
               nlist = ii
               icsgp = nlist + 1
               CALL sorti(0,0,1,1,z(ilist),icsgp-ilist)
               z(icsgp) = 0
               jj = ilist
               DO kk = ilist , nlist
                  IF ( z(kk+1)/=z(kk) ) THEN
                     z(jj) = z(kk)
                     jj = jj + 1
                  ENDIF
               ENDDO
               nlist = jj - 1
               icsgp = jj
               file = gpdt
               CALL open(*560,gpdt,z(buf1),rdrew)
               CALL fwdrec(*580,gpdt)
               ncore = buf1 - 5
               i = ilist
               SPAG_Loop_1_3: DO
                  CALL read(*580,*600,gpdt,z(jj),7,0,flag)
                  IF ( z(jj)==z(i) ) THEN
                     jj = jj + 5
                     IF ( jj>ncore ) CALL mesage(-8,0,gp1ah)
                     i = i + 1
                     IF ( i>nlist ) THEN
                        ncsgp = jj - 5
                        CALL close(gpdt,clsrew)
                        EXIT SPAG_Loop_1_3
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_3
            ENDIF
!
!     LOOP THRU THE CSDT SOLVING AS MANY COORDINATE SYSTEMS AS POSSIBLE
!     ON EACH PASS.
!
            nn = (ncsdt-icsdt)/16 + 1
            solv = 0
            solvp = 0
            ii = icsdt
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         IF ( z(ii+2)<2 ) THEN
!
!     *****  TYPE = 1 *****
!     CHECK TO SEE IF EACH OF THE 3 REFERENCE GRID PTS IS IN BASIC SYS
!     IF SO,CALCULATE THE TRANSFORMATION TO BASIC AND SET COORD SYSTEM
!     AS SOLVED, IF NOT CONTINUE TO NEXT COORDINATE SYSTEM
!
            i = 0
         ELSEIF ( z(ii+2)==2 ) THEN
!
!     ***** TYPE = 2 *****
!     CHECK THE DEFINING LOCAL COORDINATE SYSTEM
!     IF BASIC, SOLVE AS IN TYPE=1
!     IF NOT BASIC, FIND THE REFERENCED COORD SYSTEM AND TEST IF THAT
!     SYSTEM IS SOLVED. IF YES, CALCULATE THE TRANSFORMATION TO BASIC
!     IF NO, CONTINUE THRU THE CSDT
!
            IF ( z(ii+3)/=0 ) THEN
               i = icsdt
               DO WHILE ( z(i)/=z(ii+3) )
                  i = i + 16
                  IF ( i>ncsdt ) THEN
                     spoint(1) = z(ii)
                     spoint(2) = z(ii+3)
                     ierr = 4
                     CALL mesage(-30,ierr,spoint)
                     GOTO 600
                  ENDIF
               ENDDO
               IF ( z(i+2)/=3 .OR. z(i+3)/=0 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = 0
               ASSIGN 440 TO ndx
               spag_nextblock_1 = 12
            ELSE
               DO i = 1 , 9
                  k = ii + i
                  aa(i) = zz(k+3)
               ENDDO
               spag_nextblock_1 = 28
            ENDIF
            CYCLE
         ELSE
!
!     ***** TYPE = 3 *****
!     CHECK THE DEFINING LOCAL COORDINATE SYSTEM
!     IF BASIC, CONTINUE THRU CSDT
!     IF NOT BASIC, ERROR CONDITION
!
            IF ( z(ii+3)==0 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL mesage(-30,ierr,spoint)
            GOTO 600
         ENDIF
         DO
            k = ii + i
            j = icsgp - 1
            DO WHILE ( z(j+1)/=z(k+3) )
               j = j + 5
               IF ( j>=ncsgp ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( z(j+2)/=0 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = i*3
            aa(k+1) = zz(j+3)
            aa(k+2) = zz(j+4)
            aa(k+3) = zz(j+5)
            i = i + 1
            IF ( i>2 ) THEN
               spag_nextblock_1 = 28
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 12
      CASE (12)
         l = k + ii
         ax(1) = zz(l+4)
         ax(2) = zz(l+5)
         ax(3) = zz(l+6)
         IF ( z(i+1)<2 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( z(i+1)/=2 ) THEN
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 26
         CYCLE SPAG_DispatchLoop_1
 440     aa(k+1) = ar(1)
         aa(k+2) = ar(2)
         aa(k+3) = ar(3)
         k = k + 3
         IF ( k<=6 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 28
      CASE (13)
!
!     TEST FOR COMPLETION OF PASS THRU CSDT
!
         ii = ii + 16
         IF ( ii<=ncsdt ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     LOOP THRU THE CSGP (IFPRESENT) AND TRANSFORM ALL
!     POSSIBLE GRID PTS TO BASIC
!
         IF ( nolist==0 ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         jj = icsgp
         spag_nextblock_1 = 14
      CASE (14)
         IF ( z(jj+1)==0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = icsdt
         DO WHILE ( z(jj+1)/=z(i) )
            i = i + 16
            IF ( i>ncsdt ) THEN
               ierr = 6
               spoint(1) = z(jj)
               spoint(2) = z(jj+1)
               CALL mesage(-30,ierr,spoint)
               GOTO 600
            ENDIF
         ENDDO
         IF ( z(i+2)/=3 .OR. z(i+3)/=0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ax(1) = zz(jj+2)
         ax(2) = zz(jj+3)
         ax(3) = zz(jj+4)
         ASSIGN 460 TO ndx
         IF ( z(i+1)<2 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( z(i+1)/=2 ) THEN
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 26
         CYCLE SPAG_DispatchLoop_1
 460     zz(jj+2) = ar(1)
         zz(jj+3) = ar(2)
         zz(jj+4) = ar(3)
         zz(jj+1) = 0
         spag_nextblock_1 = 15
      CASE (15)
         jj = jj + 5
         IF ( jj<=ncsgp ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
!
!     TEST TO SEE IF ALL COORDINATE SYSTEMS SOLVED
!     IF NOT, TEST TO SEE IF ANY NEW SOLUTIONS ON LAST PASS
!     IF NONE, INCONSISTANT DEFINITION OF COORDINATE SYSTEMS
!     OTHERWISE LOOP BACK THRU THE CSDT
!
         IF ( solv==nn ) THEN
!
!     WRITE THE CSTM
!
            CALL fname(cstm,a)
            file = cstm
            CALL open(*560,cstm,z(buf1),wrtrew)
            CALL write(cstm,a,2,1)
            DO ii = icsdt , ncsdt , 16
               CALL write(cstm,z(ii),2,0)
               CALL write(cstm,z(ii+4),12,0)
            ENDDO
            CALL close(cstm,clsrew)
            nocstm = nn
            mcb(3) = nn
            mcb(1) = cstm
            CALL wrttrl(mcb)
         ELSEIF ( solv==solvp ) THEN
            spoint(1) = 0
            spoint(2) = 0
            ierr = 5
            CALL mesage(-30,ierr,spoint)
            GOTO 600
         ELSE
            solvp = solv
            ii = icsdt
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
!
!     OPEN EQEXIN AND WRITE HEADER RECORD.
!     THEN WRITE FIRST RECORD (PAIRS OF EXTERNAL GRID NO., INTERNAL NO.
!     IN EXTERNAL SORT).
!
         file = eqexin
         CALL open(*560,eqexin,z(buf1),wrtrew)
         CALL fname(eqexin,a)
         CALL write(eqexin,a,2,1)
         CALL write(eqexin,z,n1,1)
         CALL close(eqexin,cls)
!
!     A LIST OF DEGREES OF FREEDOM FOR EACH GRID OR SCALAR POINT IS
!     FORMED BEGINNING AT Z(ILIST) BY READING GEOM2 AND USING THE
!     CONNECTION INFORMATION IN CONJUNCTION WITH THE ELEM TABLE IN
!     /GPTA1/.
!
         file = geom2
         ilist0 = ilist - 1
         nlist = ilist + (neqex+1)/2
         IF ( nlist>=buf3 ) CALL mesage(-8,0,gp1ah)
         DO i = ilist , nlist
            z(i) = 0
         ENDDO
         jerr = 0
         CALL open(*500,geom2,z(buf1),rdrew)
         spag_nextblock_1 = 18
      CASE (18)
         CALL fwdrec(*580,geom2)
 480     CALL ectloc(*500,geom2,a,i)
!
!     ELEMENT TYPE LOCATED--PREPARE TO PROCESS EACH ELEMENT
!
         IF ( elem(i+9)==0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j1 = elem(i+12)
         nread = j1 + elem(i+9) - 1
         nskip = -(elem(i+5)-nread)
         maxdof = elem(i+24)
         itype = elem(i+2)
         DO
!
!     READ CONNECTION DATA FOR ELEMENT AND LOCATE EXT. GRID NBR IN
!     EQEXIN UPDATE DOF LIST FOR EACH GRID NBR
!
            CALL read(*580,*480,geom2,a,nread,0,m)
            DO i = j1 , nread
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( a(i)==0 ) CYCLE
                     CALL bisloc(*482,a(i),z,2,kn,k)
                     j = ilist0 + z(k+1)
                     IF ( itype>=76 .AND. itype<=79 ) THEN
!
!     FLUID ELEMENT (CFHEX1,CFHEX2,CFWEDGE,CFTETRA)
!
                        IF ( z(j)>0 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
                        z(j) = -1
                     ELSE
!
!     STRUCTURE ELEMENT AND OTHERS
!
                        IF ( z(j)<0 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        z(j) = max0(z(j),maxdof)
                     ENDIF
                     CYCLE
 482                 WRITE (iout,99010) ufm , a(1) , a(i)
99010                FORMAT (A23,' 2007, ELEMENT',I8,' REFERENCES UNDEFINED GRID ','POINT',I8)
                     jerr = jerr + 1
                  CASE (2)
                     WRITE (iout,99011) ufm , a(i)
99011                FORMAT (A23,' 8011, GRID POINT',I8,' HAS BOTH STRUCTURE AND ','FLUID ELEMENTS CONNECTED')
                     jerr = jerr + 1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            CALL read(*580,*480,geom2,a,nskip,0,m)
         ENDDO
!
!     END-OF-FILE ON GEOM2---IF FATAL ERRORS, TERMINATE
!
 500     IF ( jerr/=0 ) CALL mesage(-61,a,z)
!
!     OPEN BGPDT AND SIL. WRITE HEADER RECORDS. OPEN GPDT. SKIP HEADER.
!
         offset = rshift(kn,5)
         CALL fname(bgpdt,a)
         CALL fname(sil,a(3))
         file = bgpdt
         CALL open(*560,bgpdt,z(buf1),wrtrew)
         file = sil
         CALL open(*560,sil,z(buf2),wrtrew)
         file = gpdt
         CALL open(*560,gpdt,z(buf3),rdrew)
         CALL fwdrec(*580,gpdt)
         CALL write(bgpdt,a,2,1)
         CALL write(sil,a(3),2,1)
         luset = 1
         spag_nextblock_1 = 19
      CASE (19)
!
!     READ AN ENTRY FROM THE GPDT.
!     TEST FOR DEFINING COORDINATE SYSTEM.
!
         CALL read(*580,*540,gpdt,a,7,0,flag)
         IF ( a(2)<0 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( a(2)==0 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COORDINATE SYSTEM NOT BASIC--
!     USE CSDT IN CORE TO TRANSFORM TO BASIC.
!
         IF ( nocstm/=-1 ) THEN
            i = icsdt
            DO WHILE ( z(i)/=a(2) )
               i = i + 16
               IF ( i>ncsdt ) THEN
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            ax(1) = aa(3)
            ax(2) = aa(4)
            ax(3) = aa(5)
            ASSIGN 520 TO ndx
            IF ( z(i+1)<2 ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( z(i+1)/=2 ) THEN
               spag_nextblock_1 = 27
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
         ierr = 6
         spoint(1) = a(1)
         spoint(2) = a(2)
         CALL mesage(-30,ierr,spoint)
         GOTO 600
 520     aa(3) = ar(1)
         aa(4) = ar(2)
         aa(5) = ar(3)
         spag_nextblock_1 = 21
      CASE (21)
!
!     GRID POINT NOW BASIC--
!     STORE DISPLACEMENT SYSTEM COORD. SYSTEM ID AND SET TYPE.
!     MAKE SURE DISPLACEMENT COORD. SYSTEM IS DEFINED.
!
         a(2) = a(6)
         type = 1
         khr = ilist0 + a(1)
         incr = z(khr)
!
!     IF INCR NEGATIVE - SPECIAL HYDROELASTIC GRID POINT WITH SINGLE
!     DEGREE OF FREEDOM
!
         IF ( incr>=0 ) THEN
            IF ( incr==0 ) incr = 6
!
!     ///////////////////////////////
!
!     TEMP PATCH
!
            incr = max0(incr,6)
!
!     ///////////////////////////////
!
            IF ( a(2)==0 .AND. itherm==0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     IF A(2) WHICH EQUALS A(6) IS EQUAL TO -1 THEN A FLUID GRID POINT
!     AS CREATED BY IFP4 IS AT HAND AND HAS ONLY 1 DEGREE OF FREEDOM
!     ..... IF -HEAT- PROBLEM THEN ALL GRIDS HAVE 1 DEGREE OF FREEDOM.
!
            IF ( a(2)/=(-1) .AND. itherm<=0 ) THEN
               IF ( nocstm/=-1 ) THEN
                  DO ijk = icsdt , ncsdt , 16
                     IF ( a(2)==z(ijk) ) THEN
                        spag_nextblock_1 = 23
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
               nogo = 1
               CALL mesage(30,104,a(2))
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     SCALAR POINT-- SET TYPE.
!
         a(2) = 0
         a(6) = 0
         spag_nextblock_1 = 22
      CASE (22)
         type = 2
         incr = 1
         spag_nextblock_1 = 23
      CASE (23)
!
!     WRITE ENTRY ON BGPDT AND SIL.
!
         CALL write(bgpdt,a(2),4,0)
         CALL write(sil,luset,1,0)
!
!     REPLACE INTERNAL NO. IN EQEXIN WITH CODED SIL NO.
!     THEN INCREMENT SIL NO.
!
         ncode = 10*luset + type
         IF ( noseq/=0 ) THEN
            ncode = -ncode
            lmt1 = max0(2*(a(1)-offset),2)
            DO k = lmt1 , n1 , 2
               IF ( z(k)==a(1) ) THEN
                  spag_nextblock_1 = 24
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            DO k = 2 , lmt1 , 2
               IF ( z(k)==a(1) ) THEN
                  spag_nextblock_1 = 24
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            CALL mesage(-30,2,a)
         ELSE
            k = 2*a(1)
            IF ( z(k)/=a(1) ) CALL mesage(-30,2,a)
         ENDIF
         spag_nextblock_1 = 24
      CASE (24)
         z(k) = ncode
         luset = luset + incr
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE BGPDT AND SIL. WRITE TRAILERS.
!
 540     CALL close(bgpdt,clsrew)
         CALL close(sil,clsrew)
         CALL close(gpdt,clsrew)
         luset = luset - 1
!    2147483647   = 2**31-1
         IF ( luset>2147483647 ) THEN
            WRITE (iout,99012) ufm , luset
99012       FORMAT (A23,' 3175, TOTAL NUMBER OF DEGREES OF FREEDOM IN THE ','PROBLEM (',I11,                                        &
                   &' ) EXCEEDS 2,147,483,647 (I.E., ''2**31 - 1)')
99013       FORMAT (A29,' 3175, PROBLEM SIZE,',I8,' DOF''S, EXCEEDS THE OLD ','LIMIT OF 65535.',/5X,'GOOD NEWS, JOB WILL CONTINUE')
            CALL mesage(-61,0,0)
         ENDIF
         mcb(1) = bgpdt
         mcb(3) = 0
         CALL wrttrl(mcb)
         mcb(1) = sil
         mcb(3) = luset
         CALL wrttrl(mcb)
!
!     IF GRID NOS. ARE RESEQUENCED, SWITCH SIGN ON CODED SIL NO.
!     WRITE SECOND RECORD OF EQEXIN. CLOSE FILE AND WRITE TRAILER.
!
         IF ( noseq/=0 ) THEN
            DO k = 2 , n1 , 2
               z(k) = -z(k)
            ENDDO
         ENDIF
         file = eqexin
         CALL open(*560,eqexin,z(buf1),wrt)
         CALL write(eqexin,z,n1,1)
         CALL close(eqexin,clsrew)
         mcb(1) = eqexin
         mcb(3) = 0
         CALL wrttrl(mcb)
         CALL sswtch(36,k)
         IF ( k==1 ) CALL diag36(z,buf1,gpl,sil,eqexin)
         IF ( nogo/=0 ) CALL mesage(-61,0,0)
         RETURN
      CASE (25)
!
!     ===============================================================
!
!     INTERNAL SUBROUTINE TO TRANSFORM A RECTANGULAR GRID PT TO BASIC
!     I POINTS TO THE CSDT ENTRY WHERE THE TRANSFORMATION IS DEFINED
!     THE GRID PT TO BE TRANSFORMED IS STORED AT AX(1,2,3)
!     THE TRANSFORMED GRID PT WILL BE STORED AT AR(1,2,3)
!
         ar(1) = zz(i+7)*ax(1) + zz(i+8)*ax(2) + zz(i+9)*ax(3) + zz(i+4)
         ar(2) = zz(i+10)*ax(1) + zz(i+11)*ax(2) + zz(i+12)*ax(3) + zz(i+5)
         ar(3) = zz(i+13)*ax(1) + zz(i+14)*ax(2) + zz(i+15)*ax(3) + zz(i+6)
         GOTO ndx
      CASE (26)
!
!     INTERNAL SUBROUTINE TO TRANSFORM A CYLINDRICAL GRID PT TO BASIC
!     R,THETA,Z IS STORED AX(1,2,3)
!
         r = ax(1)
         ax(2) = degra*ax(2)
         ax(1) = r*cos(ax(2))
         ax(2) = r*sin(ax(2))
         spag_nextblock_1 = 25
      CASE (27)
!
!
!     INTERNAL SUBROUTINE TO TRANSFORM A SPHERICAL GRID PT TO BASIC
!     RHO,THETA,PHI IS STORED AT AX(1,2,3)
!
         ax(2) = degra*ax(2)
         ax(3) = degra*ax(3)
         rsth = ax(1)*sin(ax(2))
         rcth = ax(1)*cos(ax(2))
         ax(1) = rsth*cos(ax(3))
         ax(2) = rsth*sin(ax(3))
         ax(3) = rcth
         spag_nextblock_1 = 25
      CASE (28)
!
!
!     INTERNAL SUBROUTINE TO CALCULATE THE 3X3 TRANSFORMATION MATRIX
!     AND 3X1 TRANSLATION VECTOR GIVEN THREE POINTS IN THE BASIC SYSTEM
!     THE RESULTS ARE STORED BACK IN THE CSDT
!
!     STORE R0 = A IN THE CSDT
!
         zz(ii+4) = aa(1)
         zz(ii+5) = aa(2)
         zz(ii+6) = aa(3)
!
!     FORM B - A
!
         DO i = 1 , 3
            ak(i) = ab(i) - aa(i)
         ENDDO
!
!     FORM K = (B - A)/LENGTH(B - A)
!     FORM C - A
!
         length = sqrt(ak(1)**2+ak(2)**2+ak(3)**2)
         DO i = 1 , 3
            ak(i) = ak(i)/length
            ac(i) = ac(i) - aa(i)
         ENDDO
!
!     FORM K X (C - A)
!
         aj(1) = ak(2)*ac(3) - ak(3)*ac(2)
         aj(2) = ak(3)*ac(1) - ak(1)*ac(3)
         aj(3) = ak(1)*ac(2) - ak(2)*ac(1)
!
!     FORM J = (K X (C-A))/LENGTH(K X (C-A))
!
         length = sqrt(aj(1)**2+aj(2)**2+aj(3)**2)
         DO i = 1 , 3
            aj(i) = aj(i)/length
         ENDDO
!
!     FORM I = J X K
!
         ai(1) = aj(2)*ak(3) - aj(3)*ak(2)
         ai(2) = aj(3)*ak(1) - aj(1)*ak(3)
         ai(3) = aj(1)*ak(2) - aj(2)*ak(1)
!
!     STORE 3X3 ROTATION MATRIX = ((IX,JX,KX),(IY,JY,KY),(IZ,JZ,KZ))
!     IN THE CSDT
!
         zz(ii+7) = ai(1)
         zz(ii+8) = aj(1)
         zz(ii+9) = ak(1)
         zz(ii+10) = ai(2)
         zz(ii+11) = aj(2)
         zz(ii+12) = ak(2)
         zz(ii+13) = ai(3)
         zz(ii+14) = aj(3)
         zz(ii+15) = ak(3)
!
!     SET WD 3 OF CSDT = 3 AND WD 4 = 0 TO INDICATE  SOLVED SYSTEM
!     INCREMENT SOLVED SYSTEM COUNT
!
         z(ii+2) = 3
         z(ii+3) = 0
         solv = solv + 1
         spag_nextblock_1 = 13
      CASE (29)
!
!
!     INTERNAL SUBROUTINE TO PERFORM BINARY SEARCH ON FIRST ENTRY
!     OF A DOUBLE ENTRIED TABLE STORED AT Z(1) THRU Z(N+1)
!
         klo = 1
         khi = kn
         spag_nextblock_1 = 30
      CASE (30)
         k = (klo+khi+1)/2
         DO
            IF ( a(1)<z(2*k-1) ) THEN
               khi = k
            ELSEIF ( a(1)==z(2*k-1) ) THEN
               a(1) = z(2*k)
               GOTO ndx
            ELSE
               klo = k
            ENDIF
            IF ( khi-klo<1 ) THEN
               CALL mesage(30,ierr,a(1))
               nogo = 1
               GOTO nerr
            ELSEIF ( khi-klo==1 ) THEN
               IF ( k==klo ) THEN
                  k = khi
               ELSE
                  k = klo
               ENDIF
               klo = khi
            ELSE
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!
!     FATAL ERROR MESAGES
!
 560     ndx = -1
         CALL mesage(ndx,file,gp1ah)
         spag_nextblock_1 = 31
         CYCLE SPAG_DispatchLoop_1
 580     ndx = -2
         CALL mesage(ndx,file,gp1ah)
         spag_nextblock_1 = 31
         CYCLE SPAG_DispatchLoop_1
 600     ndx = -3
         CALL mesage(ndx,file,gp1ah)
         spag_nextblock_1 = 31
      CASE (31)
         spoint(1) = z(k+3)
         spoint(2) = z(ii)
         ierr = 3
         CALL mesage(-30,ierr,spoint)
         GOTO 600
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99014 FORMAT (A25,' 2140A, DUE TO THE PRESENCE OF ONE OR MORE GRID OR ','SCALAR POINTS WITH VERY LARGE EXTERNAL ID''S, THE SEQGP',  &
            & /5X,'AND SEQEP CARDS, IF USED, ARE FORCED TO REDUCE FROM ','ALLOWABLE 4 SEQID LEVELS TO ',A29,/)
99015 FORMAT (A23,' 3137A, SEQGP CARDS WITH MORE THAN ONE SEQID LEVEL ','ARE ILLEGAL FOR AXISYSM. OR HYDROELAS. PROBLEM')
END SUBROUTINE gp1
