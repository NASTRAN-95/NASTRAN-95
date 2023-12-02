!*==rmg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rmg
USE C_BLANK
USE C_DCOMPX
USE C_GFBSX
USE C_GPTA1
USE C_NAMES
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZBLPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: ai
   INTEGER , DIMENSION(20) :: block , block2
   INTEGER , DIMENSION(10) :: buf
   INTEGER :: buf1 , buf2 , buf3 , core , dcol , dirgg , dnrgg , dx , eltype , estwds , file , flag , i , i1 , i2 , iadd , icol ,   &
            & icolum , icount , ideflt , idum , idx , idx1 , idx2 , idxm8 , ieltab , ieol , iirow , index , inxcol , ipos , iprec , &
            & irad , iretrn , irgg , isgg , isil , j , jcol , k , kk , ksil , l , lentry , lost , max , mcbsav , meltab , myradm ,  &
            & n , ncol , ndx , ne , neltab , nrgg , nsgg , nsil , number , numtst , outpt , precis , radchk , rcol , rx , sysbuf ,  &
            & tset , words
   REAL :: check , defalt , sumfa , temp1 , temp2 , value
   REAL(REAL64) , DIMENSION(2) :: di , do
   LOGICAL :: double , lrad , nogo
   REAL(REAL64) :: dsumfa , dtemp2 , dvalue
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(100) :: ecpt
   INTEGER , SAVE :: eor , est , gptt , hbdytp , kgg , kggx , matpol , noeor , qge , rgg , scrt1 , scrt2 , scrt3 , scrt4 , scrt5 ,  &
                   & scrt6
   INTEGER , DIMENSION(16) :: idata
   INTEGER , DIMENSION(7) :: mcb , mcb1 , mcb2 , mcb3
   INTEGER , DIMENSION(2) :: name
   INTEGER , DIMENSION(2) , SAVE :: radlst , radmtx , radtyp , subr
   REAL , DIMENSION(10) :: rbuf
   REAL , DIMENSION(16) :: rdata
   REAL , DIMENSION(2) :: rz
   EXTERNAL bldpk , bldpkn , close , cpyfil , decomp , delset , fname , fwdrec , gfbs , gopen , hbdy , intpk , intpki , korsz ,     &
          & locate , makmcb , mesage , open , pack , preloc , rdtrl , read , ssg2b , tranp1 , unpack , write , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
   INTEGER :: spag_nextblock_5
!
!     RADIATION MATRIX GENERATOR MODULE.
!
!     DMAP CALLING SEQUENCE
!
!     RMG    EST,MATPOOL,GPTT,KGGX/RGG,QGE,KGG/C,Y,TABS/C,Y,SIGMA/
!            V,N,NLR/V,N,LUSET $
!
!     THIS MODULE COMPUTES AND OUTPUTS DATA IN SINGLE OR DOUBLE
!     PRECISION BASED ON -PRECIS-.
!
   !>>>>EQUIVALENCE (Z(1),Rz(1),Dz(1)) , (buf(1),rbuf(1)) , (Do(1),Ao(1)) , (di(1),ai(1)) , (idata(1),rdata(1)) , (defalt,ideflt) ,      &
!>>>>    & (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(10),Tset) , (Ksystm(55),Iprec) , (Ksystm(57),Myradm) , (Ksystm(58),Radchk)
!
!     MYRADM  = 1  IMPLIES SYMMETRIC SCRIPT-AF INPUT
!     RADCHK NE 0  REQUESTS DIAGNOSTIC PRINTOUT OF AREAS AND VIEW FACTOR
!     MYRADM  = 2  IMPLIES UNSYMMETRIC SCRIPT-AF INPUT
!
   DATA subr/4HRMG  , 4H    /
   DATA radtyp/4H     , 4H  UN/
   DATA radlst/2014 , 20/
   DATA radmtx/3014 , 30/
   DATA hbdytp/52/
   DATA noeor/0/ , eor/1/
   DATA est , matpol , gptt , kggx , rgg , qge , kgg/101 , 102 , 103 , 104 , 201 , 202 , 203/
   DATA scrt1 , scrt2 , scrt3 , scrt4 , scrt5 , scrt6/301 , 302 , 303 , 304 , 305 , 306/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DEFINITION OF CORE AND BUFFER POINTERS
!
         CALL delset
         scrt1 = 301
         precis = 2
         IF ( iprec/=2 ) precis = 1
         core = korsz(Z)
         buf1 = core - sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         buf3 = buf2 - sysbuf - 2
         core = buf3 - 1
         IF ( core<100 ) CALL mesage(-8,0,subr)
         nogo = .FALSE.
         double = .FALSE.
         IF ( precis==2 ) double = .TRUE.
         IF ( myradm==1 .OR. myradm==2 ) WRITE (outpt,99001) Uwm , radtyp(myradm)
99001    FORMAT (A25,' 2358, ',A4,'SYMMETRIC SCRIPT-AF MATRIX (HREE) ','ASSUMED IN RADMTX')
!
!     OPEN MATPOOL DATA BLOCK.
!
         file = matpol
         CALL preloc(*240,Z(buf1),matpol)
!
!     LOCATE RADLST DATA
!
         CALL locate(*220,Z(buf1),radlst,flag)
!
!     BUILD ELEMENT DATA TABLE.  -LENTRY- WORDS PER ELEMENT ID PRESENT
!     IN RADLST.
!
!     EACH ENTRY CONTAINS THE FOLLOWING OR MORE
!
!     WORD  1 = ELEMENT ID OF HBDY ELEMENT
!     WORD  2 = DIAGONAL MATRIX ELEMENT A-SUB-I
!     WORD  3 = DIAGONAL MATRIX ELEMENT E-SUB-I
!     WORD  4 = ELEMENT FA SUM (USED FOR RADMTX CHECK)
!     WORD  5 = SIL-1
!     WORD  6 = SIL-2
!     WORD  7 = SIL-3
!     WORD  8 = SIL-4
!     WORD  9 = GIJ-1  (GIJ TERMS MAY BE 2 WORDS EACH IF DOUBLE PREC)
!     WORD 10 = GIJ-2
!     WORD 11 = GIJ-3
!     WORD 12 = GIJ-4
!
!
         lentry = 8 + 4*precis
         ieltab = 1
         idxm8 = ieltab - lentry - 1
         neltab = ieltab - 1
         SPAG_Loop_1_1: DO
            IF ( neltab+lentry>core ) CALL mesage(-8,0,subr)
            CALL read(*260,*280,matpol,Z(neltab+1),1,noeor,words)
            IF ( Z(neltab+1)<=0 ) THEN
!
!     ALL RADLST DATA NOW IN CORE.
!     (POSITION TO END OF RECORD ON  MATPOOL)
!
               CALL read(*260,*20,matpol,buf,1,eor,words)
               WRITE (outpt,99002) Swm
99002          FORMAT (A27,' 3071, EXTRA DATA IN RADLST RECORD OF MATPOOL DATA ','BLOCK IGNORED.')
               EXIT SPAG_Loop_1_1
            ELSE
               Z(neltab+2) = 0
               Z(neltab+3) = 0
               neltab = neltab + lentry
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     LOCATE RADMTX DATA
!
 20      ne = (neltab-ieltab+1)/lentry
         CALL locate(*40,Z(buf1),radmtx,flag)
         lrad = .TRUE.
!
!     READ IN RADMTX DATA.  FOR LOWER TRIANGLE COLUMNS PRESENT
!     ENTRY WORDS 2 AND 3 IN -ELTAB- WILL BE USED TO STORE FIRST
!     AND LAST LOCATIONS OF LOWER TRIANGLE COLUMN.  ZEROS IMPLY COLUMN
!     IS NULL.
!
         irad = neltab + 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ COLUMN INDEX
!
         CALL read(*260,*60,matpol,index,1,noeor,words)
!
!     MAXIMUM NUMBER OF INPUT TERMS FOR THIS COLUMN. (LOWER TRIANGLE)
!
         max = ne - index + 1
         IF ( myradm==2 ) max = ne
!
!     SET -IDX- TO ELTAB ENTRY
!
         idx = idxm8 + index*lentry
!
!     READ IN COLUMN ELEMENTS IF ANY
!
         n = 0
         SPAG_Loop_1_2: DO
            CALL read(*260,*280,matpol,Z(irad),1,noeor,words)
            IF ( Z(irad)==-1 ) EXIT SPAG_Loop_1_2
            n = n + 1
            irad = irad + 1
            IF ( irad>core ) CALL mesage(-8,0,subr)
            IF ( n>max ) THEN
!
!     TOO MANY COLUMN ELEMENTS INPUT
!
               irad = irad - 1
               DO
!
!     SKIP TO END OF COLUMN
!
                  CALL read(*260,*280,matpol,idum,1,noeor,words)
                  IF ( idum==-1 ) THEN
                     WRITE (outpt,99003) Uwm , index , ne
99003                FORMAT (A25,' 3072, TOO MANY MATRIX VALUES INPUT VIA RADMTX BULK',' DATA FOR COLUMN',I9,1H.,/5X,               &
                            &'EXTRA VALUES IGNORED AS ','MATRIX SIZE IS DETERMINED TO BE OF SIZE',I9,                               &
                            &' FROM RADLST COUNT OF ELEMENT ID-S.')
                     EXIT SPAG_Loop_1_2
                  ENDIF
               ENDDO
            ENDIF
         ENDDO SPAG_Loop_1_2
!
!     ALL DATA FOR LOWER TRIANGLE PORTION OF COLUMN IS IN CORE.
!     (BACK UP OVER ANY ZEROS)
!
         SPAG_Loop_1_3: DO WHILE ( n>0 )
            IF ( Z(irad-1)/=0 ) THEN
!
!     SET FIRST AND LAST POINTERS
!
               Z(idx+2) = irad - n
               Z(idx+3) = irad - 1
!
!     GO READ NEXT COLUMN
!
               EXIT SPAG_Loop_1_3
            ELSE
               n = n - 1
               irad = irad - 1
            ENDIF
         ENDDO SPAG_Loop_1_3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     NULL RADMTX ASSUMED
!
 40      lrad = .FALSE.
!
!     RADMTX IS COMPLETELY IN CORE IN TEMPORARY SPECIAL PACKED FORM.
!
!     NOW PACK OUT EACH COLUMN OF MATRIX F TO SCRATCH 1
!
 60      CALL close(matpol,Clsrew)
         IF ( myradm==1 .OR. myradm==2 ) scrt1 = 303
         CALL gopen(scrt1,Z(buf1),Wrtrew)
         CALL makmcb(mcb1,scrt1,ne,Sqr,precis)
         DO jcol = 1 , ne
!
!     INITIALIZE PACKING OF COLUMN -JCOL-
!
            CALL bldpk(1,precis,scrt1,0,0)
!
!     PACK OUT ELEMENTS OF COLUMN -JCOL-
!
            inxcol = idxm8 + jcol*lentry
!
!     SET FA SUM TO ZERO FOR CURRENT COLUMN.
!
            sumfa = 0.0
            IF ( lrad ) THEN
               DO Irow = 1 , ne
!
!     LOCATE ELEMENT ROW-IROWK, COL-JCOL.
!
                  IF ( Irow>=jcol .OR. myradm==2 ) THEN
!
!     HERE IF BELOW OR ON DIAGONAL.
!     ELEMENT DESIRED IS IN COLUMN -JCOL- IN POSITION (IROW-JCOL+I1)
!
                     idx = inxcol
                     i1 = Z(idx+2)
                     IF ( i1<=0 ) CYCLE
                     i2 = Z(idx+3)
                     ipos = Irow - jcol + i1
                     IF ( myradm==2 ) ipos = Irow + i1 - 1
                  ELSE
!
!     HERE IF ABOVE THE DIAGONAL
!     ELEMENT DESIRED IS IN COLUMN -IROW- IN CORE AND POSITION
!     (JCOL-IROW+1) OF THE LOWER TRIANGLE PORTION.
!
                     idx = idxm8 + Irow*lentry
                     i1 = Z(idx+2)
                     IF ( i1<=0 ) CYCLE
                     i2 = Z(idx+3)
                     ipos = jcol - Irow + i1
                  ENDIF
                  IF ( ipos<=i2 ) THEN
                     IF ( rz(ipos)<0 ) THEN
                        WRITE (outpt,99004) Uwm , jcol , Irow , rz(ipos)
99004                   FORMAT (A25,' 2359, COL',I6,', ROW',I6,' OF RADMTX IS NEGATIVE (',E14.6,').')
                     ELSEIF ( rz(ipos)==0 ) THEN
                        CYCLE
                     ENDIF
                     Ao(1) = rz(ipos)
                     IF ( myradm==1 .OR. myradm==2 ) Ao(1) = -Sigma*rz(ipos)
                     IF ( .NOT.(jcol==Irow .AND. (myradm==1 .OR. myradm==2)) ) sumfa = sumfa + rz(ipos)
                     CALL zblpki
                  ENDIF
!
               ENDDO
            ENDIF
!
!     COMPLETE COLUMN
!
            CALL bldpkn(scrt1,0,mcb1)
!
!     SAVE COLUMN FA SUM IN ELTAB FOR AWHILE.
!
            rz(inxcol+4) = sumfa
!
         ENDDO
!
!     PACKED MATRIX IS COMPLETE
!
         CALL wrttrl(mcb1)
         CALL close(scrt1,Clsrew)
!/////
!     CALL DMPFIL (-SCRT1,Z(NELTAB+1),CORE-NELTAB-2)
!     CALL BUG (10HF-MATRIX    ,210,0,1)
!/////
!
!     OUTPUT OF ELEMENT-ID LIST TO QGE HEADER RECORD IS PERFORMED AT
!     THIS TIME.
!
         file = qge
         CALL open(*300,qge,Z(buf1),Wrtrew)
         CALL fname(qge,name)
         CALL write(qge,name,2,noeor)
         DO i = ieltab , neltab , lentry
            CALL write(qge,Z(i),1,noeor)
         ENDDO
         CALL write(qge,0,0,eor)
         CALL close(qge,Cls)
!
!     OPEN EST AND PROCESS EST ELEMENT DATA OF ONLY THE HBDY ELEMENTS
!     WHOSE ELEMENT ID-S ARE IN THE RADLST.  I.E. NOW IN THE RDLST TABLE
!
         file = est
         CALL gopen(est,Z(buf1),Rdrew)
         SPAG_Loop_1_4: DO
!
!     READ ELEMENT TYPE
!
            CALL read(*80,*280,est,eltype,1,noeor,words)
            IF ( eltype/=hbdytp ) THEN
!
!     LOCATE HBDY ELEMENT TYPE RECORD
!
               CALL fwdrec(*260,est)
            ELSE
!
!     NOW POSITIONED TO READ EST DATA FOR HBDY ELEMENT.
!
               j = (eltype-1)*Incr
               estwds = Elem(j+12)
               lost = 0
               EXIT SPAG_Loop_1_4
            ENDIF
         ENDDO SPAG_Loop_1_4
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_5: DO
!
!     READ EST FOR ONE ELEMENT
!
            CALL read(*260,*80,est,ecpt,estwds,noeor,words)
!
!     FIND ID IN LIST
!
            DO i = ieltab , neltab , lentry
               IF ( ecpt(1)==Z(i) ) EXIT SPAG_Loop_1_5
            ENDDO
         ENDDO SPAG_Loop_1_5
!
!     ELEMENT ID IS IN LIST
!
         CALL hbdy(ecpt,ecpt,1,rdata,idata)
!
!     ON RETURN TAKE ELEMENT OUTPUTS AND PLANT THEM IN ALL ENTRIES
!     HAVING THIS SAME ID.
!
         iadd = 4*precis + 7
         DO j = ieltab , neltab , lentry
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( ecpt(1)/=Z(j) ) CYCLE
!
!     CHECK TO SEE IF SUM FA/A EQUALS 1.0 FOR THIS ELEMENT.
!
                  IF ( rdata(2)>1.0E-10 ) THEN
                     check = rz(j+3)/rdata(2)
                     IF ( myradm==1 .OR. myradm==2 ) check = check/rdata(3)
                     IF ( check<=0.99 ) lost = lost + 1
                     IF ( check<1.01 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ELSE
                     check = 9999999.
                  ENDIF
                  WRITE (outpt,99005) Ufm , Z(j) , check , rdata(2)
99005             FORMAT (A23,' 2360, TOTAL VIEW FACTOR (FA/A), FOR ELEMENT',I9,' IS',1P,E14.6,', (ELEMENT AREA IS ',1P,E14.5,').')
                  nogo = .TRUE.
                  spag_nextblock_2 = 2
               CASE (2)
                  IF ( check<1.01 .AND. radchk/=0 ) WRITE (outpt,99006) Uim , Z(j) , check , rdata(2)
99006             FORMAT (A29,' 2360, TOTAL VIEW FACTOR (FA/A), FOR ELEMENT',I9,' IS ',1P,E14.6,', (ELEMENT AREA IS ',1P,E14.5,')')
                  Z(j) = idata(1)
                  Z(j+1) = idata(2)
                  Z(j+2) = idata(3)
                  Z(j+3) = idata(4)
                  Z(j+4) = idata(5)
                  Z(j+5) = idata(6)
                  Z(j+6) = idata(7)
                  Z(j+7) = idata(8)
                  IF ( double ) THEN
                     dx = j/2 + 1
                     dz(dx+4) = rdata(9)
                     dz(dx+5) = rdata(10)
                     dz(dx+6) = rdata(11)
                     dz(dx+7) = rdata(12)
                  ELSE
                     rz(j+8) = rdata(9)
                     rz(j+9) = rdata(10)
                     rz(j+10) = rdata(11)
                     rz(j+11) = rdata(12)
                  ENDIF
                  Z(j) = -Z(j)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     ALL ELEMENTS PROCESSED.
!
 80      CALL close(est,Clsrew)
         IF ( lost>0 ) WRITE (outpt,99007) Uim , lost
99007    FORMAT (A29,' 2361, ',I4,' ELEMENTS HAVE A TOTAL VIEW FACTOR (FA','/A) LESS THAN 0.99 , ENERGY MAY BE LOST TO SPACE.')
!
!     CHECK TO SEE IF ALL ELEMENTS WERE PROCESSED.
!
!/////
!     CALL BUG (4HELTB ,270,Z(IELTAB),NELTAB-IELTAB+1)
!/////
         DO i = ieltab , neltab , lentry
            IF ( Z(i)<=0 ) THEN
               Z(i) = -Z(i)
            ELSE
               nogo = .TRUE.
               WRITE (outpt,99008) Ufm , Z(i)
99008          FORMAT (A23,' 3073, NO -HBDY- ELEMENT SUMMARY DATA IS PRESENT ','FOR ELEMENT ID =',I9,/5X,                           &
                      &'WHICH APPEARS ON A -RADLST- BULK DATA CARD.')
            ENDIF
         ENDDO
         IF ( nogo ) CALL mesage(-61,0,0)
         IF ( myradm/=1 .AND. myradm/=2 ) THEN
!
!     FORMATION OF THE Y MATRIX.  MATRIX F IS STORED ON SCRATCH 1
!
!         Y    = -F  (1.0 - E )  +  A
!          IJ      IJ        J       I
!
!         A  IS ADDED IN ONLY TO THE DIAGONAL TERMS I.E. I = J
!          I
!
!     MATRIX Y WILL BE STORED ON SCRATCH 2.
!
!
!     OPEN SCRATCH 1 FOR MATRIX F COLUMN UNPACKING.
!
            CALL gopen(scrt1,Z(buf1),Rdrew)
!
!     OPEN SCRATCH 2 FOR MATRIX Y COLUMN PACKING
!
            CALL gopen(scrt2,Z(buf2),Wrtrew)
            CALL makmcb(mcb2,scrt2,ne,Sqr,precis)
         ENDIF
!
!     SET UP VECTOR CORE (INSURE EVEN BOUNDARY)
!
         icol = mod(neltab,2) + neltab + 1
         rcol = icol
         dcol = icol/2 + 1
         ncol = icol + precis*ne - 1
         IF ( ncol>core ) CALL mesage(-8,0,subr)
         IF ( myradm==1 .OR. myradm==2 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         meltab = ieltab - lentry - 1
!
!     SETUP /PACKX/ FOR PACKING COLUMNS OF Y (SCRATCH 2)
!
         Pkin = precis
         Pkout = precis
         Pkirow = 1
         Pknrow = ne
         Pkincr = 1
!
!     SETUP /UNPAKX/ FOR UNPACKING COLUMNS OF F (SCRATCH 1)
!
         Unout = precis
         Unirow = 1
         Unnrow = ne
         Unincr = 1
         DO i = 1 , ne
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  meltab = meltab + lentry
                  rx = rcol
                  dx = dcol
!
!     UNPACK A COLUMN OF F INTO CORE.
!
                  CALL unpack(*82,scrt1,Z(icol))
                  spag_nextblock_3 = 2
                  CYCLE SPAG_DispatchLoop_3
 82               DO j = icol , ncol
                     Z(j) = 0
                  ENDDO
                  spag_nextblock_3 = 2
               CASE (2)
!
!     COMPUTE THE Y-COLUMN
!
                  DO Irow = 1 , ne
                     IF ( double ) THEN
!
!     DOUBLE PRECISION COMPUTATION
!
                        dz(dx) = -dz(dx)*(1.0D0-dble(rz(meltab+3)))
                        IF ( Irow==i ) dz(dx) = dz(dx) + dble(rz(meltab+2))
                        dx = dx + 1
                     ELSE
!
!     REAL COMPUTATION
!
                        rz(rx) = -rz(rx)*(1.0E0-rz(meltab+3))
                        IF ( Irow==i ) rz(rx) = rz(rx) + rz(meltab+2)
                        rx = rx + 1
                     ENDIF
                  ENDDO
!
!     PACK COLUMN OUT
!
                  mcbsav = mcb2(6)
                  mcb2(6) = 0
                  CALL pack(Z(icol),scrt2,mcb2)
                  IF ( mcb2(6)<=0 ) THEN
                     nogo = .TRUE.
                     WRITE (outpt,99009) Ufm , i
99009                FORMAT (A23,' 3074, COLUMN',I9,' OF THE Y MATRIX IS NULL.')
                  ENDIF
                  mcb2(6) = max0(mcb2(6),mcbsav)
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
!
         ENDDO
         IF ( nogo ) CALL mesage(-61,0,subr)
         CALL close(scrt1,Clsrew)
         CALL wrttrl(mcb2)
         CALL close(scrt2,Clsrew)
!/////
!     CALL DMPFIL (-SCRT2,Z(ICOL),CORE-ICOL-1)
!     CALL BUG (10HY-MATRIX    ,400,0,1)
!/////
!
!     NOW SOLVING FOR MATRIX X ON SCRATCH-3
!
!     (Y) (X) = (F)
!
!     F IS ON SCRATCH 1
!     Y IS ON SCRATCH 2
!
!
!     SETUP /DCOMPX/
!
         Ia(1) = scrt2
         Il(1) = 201
         Iu(1) = 203
         Il(5) = precis
         Isr1 = scrt4
         Isr2 = scrt5
         Isr3 = scrt6
         CALL rdtrl(Ia)
         Nzz = korsz(Z(icol))
         Ib = 0
         Ibbar = 0
         CALL decomp(*100,Z(icol),Z(icol),Z(icol))
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     WRITE (outpt,99010) Ufm
99010    FORMAT (A23,' 3075, INTERMEDIATE MATRIX Y IS SINGULAR.')
         CALL mesage(-61,0,subr)
         spag_nextblock_1 = 4
      CASE (4)
!
!     SETUP /GFBSX/
!
         Jl(5) = Il(5)
         Ju(7) = Iu(7)
         Jl(1) = 201
         Ju(1) = 203
         Jb(1) = scrt1
         Jx(1) = scrt3
         Ipr = precis
!//// WHAT ABOUT IDET
         Isgn = 1
         Nzzz = Nzz
         Jl(3) = ne
         Jx(5) = precis
         CALL rdtrl(Jb(1))
         CALL gfbs(Z(icol),Z(icol))
         Jx(3) = ne
         Jx(4) = Sqr
         CALL wrttrl(Jx)
         spag_nextblock_1 = 5
      CASE (5)
!/////
!     CALL DMPFIL (-SCRT3,Z(ICOL),CORE-ICOL-1)
!     CALL BUG (10HX-MATRIX     ,438,0,1)
!/////
!
!     FORMATION OF THE R MATRIX (TO BE STORED ON SCRATCH 1)
!
!          R    =(-SIGMA*E *A *E *X  ) + (SIGMA*E *A )
!           IJ            J  I  I  IJ            J  I
!
!     (TERM2 IS ADDED IN ONLY WHEN I = J)
!
!     IF MYRADM = 1 OR 2    , RADMTX MULTIPLIED BY -SIGMA IS ON SCRT3
!     MATRIX X IS ON SCRATCH 3
!
!
!     OPEN SCRATCH 3 FOR MATRIX X COLUMN UNPACKING.
!
         CALL gopen(scrt3,Z(buf3),Rdrew)
!
!     THE FOLLOWING CARD IS NEEDED IF DIRECT SCRIPT-F INPUT IS USED
!
         scrt1 = 301
!
!     OPEN SCRATCH 1 FOR MATRIX R COLUMN PACKING.
!
         file = scrt1
         CALL gopen(scrt1,Z(buf1),Wrtrew)
         CALL makmcb(mcb1,scrt1,ne,Sqr,precis)
         meltab = ieltab - lentry - 1
!
!     SETUP /PACKX/ FOR PACKING COLUMNS OF R (SCRATCH 1)
!
         Pkin = precis
         Pkout = precis
         Pkirow = 1
         Pknrow = ne
         Pkincr = 1
!
!     SETUP /UNPAKX/ FOR UNPACKING COLUMNS OF X (SCRATCH 3)
!
         Unout = precis
         Unirow = 1
         Unnrow = ne
         Unincr = 1
!
         idx1 = ieltab - lentry
         DO icolum = 1 , ne
            spag_nextblock_4 = 1
            SPAG_DispatchLoop_4: DO
               SELECT CASE (spag_nextblock_4)
               CASE (1)
                  dsumfa = 0.
                  sumfa = 0.
                  meltab = meltab + lentry
!
!     COMPUTE CONSTANT FOR COLUMN
!
!     TEMP1 = SIGMA*E
!                    J
!
                  temp1 = Sigma*rz(meltab+3)
                  rx = rcol
                  dx = dcol
!
!     UNPACK A COLUMN OF X INTO CORE.
!
                  CALL unpack(*102,scrt3,Z(icol))
                  spag_nextblock_4 = 2
                  CYCLE SPAG_DispatchLoop_4
 102              DO j = icol , ncol
                     Z(j) = 0
                  ENDDO
                  spag_nextblock_4 = 2
               CASE (2)
!
!     COMPUTE THE R-COLUMN
!
                  idx2 = idx1
                  DO Irow = 1 , ne
                     idx2 = idx2 + lentry
                     IF ( double ) THEN
!
!     DOUBLE PRECISON COMPUTATION
!
                        IF ( myradm/=1 .AND. myradm/=2 ) THEN
                           dtemp2 = dble(temp1)*dble(rz(idx2+1))
                           dz(dx) = -dtemp2*dble(rz(idx2+2))*dz(dx)
                           IF ( Irow==icolum ) dz(dx) = dz(dx) + dtemp2
                        ENDIF
                        IF ( Irow/=icolum ) dsumfa = dsumfa + dz(dx)
                        dx = dx + 1
                     ELSE
!
!     REAL COMPUTATION.
!
                        IF ( myradm/=1 .AND. myradm/=2 ) THEN
                           temp2 = temp1*rz(idx2+1)
                           rz(rx) = -temp2*rz(idx2+2)*rz(rx)
                           IF ( Irow==icolum ) rz(rx) = rz(rx) + temp2
                        ENDIF
                        IF ( Irow/=icolum ) sumfa = sumfa + rz(rx)
                        rx = rx + 1
                     ENDIF
!
                  ENDDO
!
!     PACK COLUMN OF R OUT
!
                  IF ( myradm==1 .OR. myradm==2 ) THEN
                     IF ( double ) dz(dx-1-ne+icolum) = -dsumfa
                     IF ( .NOT.double ) rz(icolum+rx-1-ne) = -sumfa
                  ENDIF
                  CALL pack(Z(icol),scrt1,mcb1)
                  EXIT SPAG_DispatchLoop_4
               END SELECT
            ENDDO SPAG_DispatchLoop_4
         ENDDO
         CALL wrttrl(mcb1)
         CALL close(scrt1,Clsrew)
         CALL close(scrt3,Clsrew)
!/////
!     CALL DMPFIL (-SCRT1,Z(ICOL),CORE-ICOL-1)
!     CALL BUG (10HR-MATRIX    ,490,0,1)
!/////
!
!     ALL OF THE HBDY ELEMENTS OF THE RADLST HAVE
!     HAD THEIR G TERMS COMPUTED, THESE G TERMS MAY BE INSERTED INTO
!     THE FULL MATRIX G.
!
!     GOING THROUGH THE RADLST TABLE WE HAVE EACH ELEMENT ENTRY FORMING
!     A COLUMN OF G WITH THE G TERMS OF THE RESPECTIVE ENTRY BEING
!     ENTERED INTO THE COLUMN AT THE SIL LOCATIONS.  (THE SILS WERE
!     PLACED IN THE RADLST ENTRY EARLIER)
!
!
!     AS THE X MATRIX STORED ON SCRATCH 3 IS NO LONGER NEEDED
!     WE WILL USE SCRATCH 3 FOR THE G MATRIX NOW.
!
         CALL gopen(scrt3,Z(buf3),Wrtrew)
         CALL makmcb(mcb3,scrt3,Luset,2,precis)
!
!     LOOP ON THE RADLST TABLE
!
         DO i = ieltab , neltab , lentry
!
!     BEGIN PACKING A COLUMN OUT
!
            CALL bldpk(precis,precis,scrt3,0,0)
!
!     PACK 1 TO 4 TERMS OUT.
!
            i1 = i + 4
            i2 = i + 7
            SPAG_Loop_2_6: DO j = 1 , 4
!
!     PICKING THE SMALLEST SIL NOT ZERO FOR THE NEXT TERM OUT
!
               isil = 0
               DO l = i1 , i2
                  IF ( Z(l)>0 ) THEN
                     IF ( isil>0 ) THEN
                        IF ( Z(l)>isil ) CYCLE
                     ENDIF
                     isil = Z(l)
                     k = l
                  ENDIF
               ENDDO
!
!     ZERO SIL IMPLYS OUT OF VALUES
!
               IF ( isil<=0 ) EXIT SPAG_Loop_2_6
!
!     PACK OUT TERM  (MAY BE SINGLE OR DOUBLE PRECISON)
!
               Irow = Z(k)
               Z(k) = 0
!
!     RESET K TO GIJ TERM PTR.
!
               kk = k + 4
               IF ( double ) kk = kk + k - i1
               Ao(1) = rz(kk)
               Ao(2) = rz(kk+1)
               CALL zblpki
            ENDDO SPAG_Loop_2_6
!
!     COMPLETE THE COLUMN
!
            CALL bldpkn(scrt3,0,mcb3)
         ENDDO
!
!     G MATRIX IS COMPLETE ON SCRATCH 3.
!
         CALL wrttrl(mcb3)
         CALL close(scrt3,Clsrew)
!/////
!     CALL DMPFIL (-SCRT3,Z(ICOL),CORE-ICOL-1)
!     CALL BUG (10HG-MATRIX     ,570,0,1)
!/////
!
!     FORM OUTPUT MATRIX  (Q  ) = (G)(R )
!                           GE         E
!
!
!     ALL CORE AT THIS POINT IS AVAILABLE THUS OPEN CORE FOR SSG2B
!     WHICH IS IN /SSGB2/ MAY BE AT THE SAME LEVEL AS
!     /RMGZZZ/.  SSG2B IS THE DRIVER FOR MPYAD.
!
         CALL ssg2b(scrt3,scrt1,0,scrt5,0,precis,1,scrt2)
!
!                                        T
!     FORM OUTPUT MATRIX  (R  ) = (Q  )(G )
!                           GG      GE
!
!
!     THE MATRIX G IS FIRST TRANSPOSED.
!
!     MATRIX G IS ON SCRATCH-3.  MATRIX G TRANSPOSE WILL BE ON SCRATCH-2
!
!     OPEN CORE /DTRANX/ FOR TRANP1 MAY BE AT SAME LEVEL AS /RMGZZZ/.
!
         CALL tranp1(scrt3,scrt2,4,scrt4,scrt6,scrt1,rgg,0,0,0,0)
!/////
!     CALL DMPFIL (-SCRT2,Z(ICOL),CORE-ICOL-1)
!     CALL BUG (10HG-TRANSP    ,570,0,1)
!/////
!
!     SSG2B MAY BE CALLED NOW TO COMPUTE (R  )
!                                          GG
!
         CALL ssg2b(scrt5,scrt2,0,rgg,0,precis,1,scrt1)
!
!     QGE WAS PLACED ON SCRT5.  NOW COPY IT TO QGE (WHERE THE HEADER
!     RECORD HAS BEEN SPECIALLY PREPARED EARLIER) .
!
         file = qge
         CALL open(*300,qge,Z(buf1),Wrt)
         file = scrt5
         CALL gopen(scrt5,Z(buf2),Rdrew)
         CALL cpyfil(scrt5,qge,Z,core,icount)
         mcb(1) = scrt5
         CALL rdtrl(mcb)
         mcb(1) = qge
         CALL wrttrl(mcb)
         CALL close(scrt5,Clsrew)
         CALL close(qge,Clsrew)
!
!                    1      3
!     FORM  S   = 4(U  + T )  THIS IS ACTUALLY A DIAGONAL MATRIX.
!            GG      G    A
!
!     NOW ALLOCATE S   DIAGONAL MATRIX SPACE AND STORE -TABS- EVERYWHERE
!                   GG
!
!
         isgg = 1
         nsgg = precis*Luset
         IF ( nsgg>core ) CALL mesage(-8,0,subr)
         IF ( double ) THEN
!
!     DOUBLE PRECISION VECTOR
!
            dx = isgg/2 + 1
            ndx = dx + Luset - 1
            DO i = dx , ndx
               dz(i) = Tabs
            ENDDO
         ELSE
!
!     REAL VECTOR
!
            DO i = isgg , nsgg
               rz(i) = Tabs
            ENDDO
         ENDIF
!
!     IF -TSET- IS SPECIFIED THEN THAT SET OF TEMPERATURES IS ADDED TO
!     THE UG VECTOR IN CORE.
!
         IF ( tset<=0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TSET IS REQUESTED
!
         file = gptt
         CALL open(*300,gptt,Z(buf1),Rdrew)
!
!     DETERMINE NUMBER OF RECORDS IN ELEMENT TEMPERATURE SECTION TO
!     SKIP OVER. (FIRST SKIP THE NAME IN HEADER)
!
         CALL read(*260,*280,gptt,buf,2,noeor,flag)
!
!     LOOK FOR REQUESTED TSET POINTERS AND REPOSITION GPTT.
!
         number = 0
         numtst = -1
         DO
            CALL read(*260,*120,gptt,buf,3,noeor,flag)
            IF ( buf(3)>number ) number = buf(3)
            IF ( tset==buf(1) ) THEN
!
!     BUF(1)=SET-ID, BUF(2)=-1 OR DEFAULT TEMP, BUF(3)=GPTT DATA RECORD.
!
               defalt = rbuf(2)
               numtst = buf(3)
            ENDIF
         ENDDO
!
!     CHECK FOR TSET NOT FOUND.
!
 120     IF ( numtst==-1 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ADD SKIP COUNTS (EL. RECORDS + DUPE HEADER + TEMP SET -1)
!
         number = number + numtst
!
!     NO NEED TO DO FURTHER I/O IF TSET IS ALL DEFAULT TEMPS.
!
         IF ( numtst==0 ) number = 0
         IF ( number>0 ) THEN
            DO i = 1 , number
               CALL fwdrec(*260,gptt)
            ENDDO
         ENDIF
!
!     TEMPERATURE DATA IS IN PAIRS OF INTERNAL ID AND TEMPERATURE.
!
!
!     AT THIS POINT THE GRID POINT TEMPERATUE DATA IS ADDED INTO THE SGG
!     DIAGONAL HELD IN CORE.
!
         nsil = 1
         rx = isgg - 1
         dx = isgg/2
         ASSIGN 140 TO iretrn
         IF ( number<=0 ) THEN
!
!     ADD DEFAULT TEMPERATURE (IF ONE EXISTS) TO THOSE POINTS NOT HAVING
!     AN EXPLICIT TEMPERATURE DEFINED.
!
            buf(1) = Luset + 1
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 140     CALL read(*260,*160,gptt,buf,2,noeor,flag)
         spag_nextblock_1 = 6
      CASE (6)
         IF ( buf(1)<nsil ) THEN
            WRITE (outpt,99011) Sfm
99011       FORMAT (A25,' 3076, GPTT DATA IS NOT IN SORT BY INTERNAL ID.')
            CALL mesage(-61,0,subr)
            buf(1) = Luset + 1
         ELSEIF ( buf(1)==nsil ) THEN
            value = rbuf(2)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         IF ( ideflt/=-1 ) THEN
            value = defalt
            ASSIGN 180 TO iretrn
            isil = nsil
            ksil = buf(1) - 1
            nsil = buf(1)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
            WRITE (outpt,99012) Ufm , nsil
99012       FORMAT (A23,' 3077, THERE IS NO GRID POINT TEMPERATURE DATA OR ','DEFAULT TEMPERATURE DATA FOR SIL POINT',I9,/5X,       &
                   &'AND POSSIBLY OTHER POINTS.')
            CALL mesage(-61,0,subr)
            value = rbuf(2)
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         isil = nsil
         ksil = buf(1)
         nsil = buf(1) + 1
         spag_nextblock_1 = 9
      CASE (9)
         DO i = isil , ksil
            IF ( i>Luset ) GOTO 200
            IF ( double ) THEN
               dx = dx + 1
               dz(dx) = dz(dx) + dble(value)
            ELSE
               rx = rx + 1
               rz(rx) = rz(rx) + value
            ENDIF
         ENDDO
         GOTO iretrn
 160     ASSIGN 200 TO iretrn
         buf(1) = Luset
         value = defalt
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 180     ASSIGN 140 TO iretrn
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     ALL TEMPERATURE DATA HAS BEEN ADDED IN.
!
 200     CALL close(gptt,Clsrew)
         spag_nextblock_1 = 10
      CASE (10)
!/////
!     CALL BUG (4HTMPS,890,Z(ISGG),NSGG-ISGG+1)
!/////
!
!     NOW CUBE EACH TERM AND THEN MULTIPLY EACH TERM BY 4.0
!
         IF ( double ) THEN
!
!     DOUBLE PRECISION COMPUTATION
!
            dx = isgg/2 + 1
            ndx = dx + Luset - 1
            DO i = dx , ndx
               dz(i) = 4.0D0*(dz(i)**3)
            ENDDO
         ELSE
!
!     REAL COMPUTATION
!
            DO i = isgg , nsgg
               rz(i) = 4.0*(rz(i)**3)
            ENDDO
         ENDIF
!
!     ALLOCATION OF CORE FOR A COLUMN OF MATRIX RGG.
!
         irgg = nsgg + 1
         nrgg = irgg + precis*Luset - 1
         dirgg = irgg/2 + 1
         dnrgg = dirgg + Luset - 1
         IF ( nrgg>core ) CALL mesage(-8,0,subr)
!
!                                   X
!     FORM OUTPUT MATRIX  (K  ) = (K  ) + (R  )(S  )
!                           GG      GG      GG   GG
!
!
!     THE DIAGONAL MATRIX (S  ) RESIDES IN CORE FROM Z(ISGG) TO Z(NSGG)
!                           GG
!
!     Z(IRGG) TO Z(NRGG) WILL BE USED TO HOLD A COLUMN OF R.
!
!       X
!     (K  ) WILL BE UNPACKED INCREMENTALLY AND ADDED INTO THE COLUMN
!       GG
!
!     OF R, AFTER THAT COLUMN OF R HAS BEEN MULTIPLIED BY THE RESPECTIVE
!
!     DIAGONAL ELEMENT OF (S  ).
!                           GG
!
!/////
!     CALL BUG (4HSGG  ,829,Z(ISGG),NSGG-ISGG+1)
!/////
         CALL gopen(rgg,Z(buf1),Rdrew)
         CALL gopen(kggx,Z(buf2),Rdrew)
         CALL gopen(kgg,Z(buf3),Wrtrew)
         CALL makmcb(mcb1,kgg,Luset,Sqr,precis)
!
!     SET UP /PACKX/ FOR PACKING COLUMN OF KGG OUT.
!
         Pkin = precis
         Pkout = precis
         Pkirow = 1
         Pknrow = Luset
         Pkincr = 1
         rx = isgg - 1
         dx = isgg/2
!
!     LOOP THROUGH -LUSET- COLUMNS TO BE OUTPUT.
!
         DO i = 1 , Luset
            spag_nextblock_5 = 1
            SPAG_DispatchLoop_5: DO
               SELECT CASE (spag_nextblock_5)
               CASE (1)
                  IF ( double ) THEN
                     dx = dx + 1
                     dvalue = dz(dx)
                  ELSE
                     rx = rx + 1
                     value = rz(rx)
                  ENDIF
!
!     UNPACK A COLUMN OF R
!
                  DO j = irgg , nrgg
                     Z(j) = 0
                  ENDDO
!
!     -UNPACK- CAN NOT BE USED HERE DUE TO UNPACKING OF KGGX BELOW.
!
                  CALL intpk(*202,rgg,block2,precis,1)
                  SPAG_Loop_2_7: DO
                     CALL intpki(ai,iirow,rgg,block2,ieol)
                     IF ( double ) THEN
                        k = dirgg - 1 + iirow
                        dz(k) = dz(k) + di(1)
                        IF ( ieol>0 ) EXIT SPAG_Loop_2_7
                     ELSE
                        k = irgg - 1 + iirow
                        rz(k) = rz(k) + ai(1)
                        IF ( ieol>0 ) EXIT SPAG_Loop_2_7
                     ENDIF
                  ENDDO SPAG_Loop_2_7
!
!     MULTIPLY RGG COLUMN BY DIAGONAL ELEMENT OF SGG.
!
 202              IF ( double ) THEN
!
!     DOUBLE PRECISION COMPUTATION
!
                     DO j = dirgg , dnrgg
                        dz(j) = dz(j)*dvalue
                     ENDDO
                  ELSE
!
!     REAL COMPUTATION
!
                     DO j = irgg , nrgg
                        rz(j) = rz(j)*value
                     ENDDO
                  ENDIF
!
!     INCREMENTAL UNPACK OF A COLUMN OF KGGX.
!     ADD TO MODIFIED COLUMN OF RGG IN CORE, AND THEN
!     BLAST PACK OUT FURTHER MODIFIED COLUMN AS A COLUMN OF KGG.
!
!     START UNPACKING COLUMN OF KGGX
!
                  CALL intpk(*204,kggx,block,precis,1)
                  spag_nextblock_5 = 2
               CASE (2)
                  SPAG_Loop_2_8: DO
                     CALL intpki(ai,iirow,kggx,block,ieol)
!
!     ADD VALUE IN
!
                     IF ( iirow>Luset ) EXIT SPAG_Loop_2_8
                     IF ( double ) THEN
!
!     DOUBLE PRECISION ADD IN
!
                        k = dirgg - 1 + iirow
                        dz(k) = dz(k) + di(1)
                        IF ( ieol>0 ) GOTO 204
                     ELSE
!
!     REAL ADD IN
!
                        k = irgg - 1 + iirow
                        rz(k) = rz(k) + ai(1)
                        EXIT SPAG_Loop_2_8
                     ENDIF
                  ENDDO SPAG_Loop_2_8
                  IF ( ieol<=0 ) THEN
                     spag_nextblock_5 = 2
                     CYCLE SPAG_DispatchLoop_5
                  ENDIF
!
!     PACK OUT COMPLETED COLUMN.
!
 204              CALL pack(Z(irgg),kgg,mcb1)
                  EXIT SPAG_DispatchLoop_5
               END SELECT
            ENDDO SPAG_DispatchLoop_5
         ENDDO
         CALL wrttrl(mcb1)
         CALL close(kgg,Clsrew)
         CALL close(kggx,Clsrew)
         CALL close(rgg,Clsrew)
!
!     ALL PROCESSING COMPLETED.
!
         Nlr = +1
         RETURN
 220     CALL close(matpol,Clsrew)
 240     Nlr = -1
         RETURN
!
!     ERROR CONDITIONS
!
!
!     END OF FILE
!
 260     j = -2
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
!     END OF RECORD
!
 280     j = -3
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
!     UNDEFINED FILE
!
 300     j = -1
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(j,file,subr)
         spag_nextblock_1 = 12
      CASE (12)
!
!     GPTT DATA MISSING FOR SET -TSET-.
!
         WRITE (outpt,99013) Ufm , tset
99013    FORMAT (A23,' 3078, NO GPTT DATA IS PRESENT FOR TEMPERATURE SET ',I8,1H.)
         CALL mesage(-61,0,subr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!     NO HBDY ELEMENTS
!
!
END SUBROUTINE rmg
