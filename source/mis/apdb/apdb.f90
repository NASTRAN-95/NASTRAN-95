!*==apdb.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apdb
   IMPLICIT NONE
   USE C_APDBUG
   USE C_BLANK
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: acpt , aerob , bgpdt , cstm , edt , eqexin , flist , pvect , scr1 , sine
   INTEGER , DIMENSION(3) , SAVE :: aero , flfact , fluttr , mkaer1 , mkaer2 , strml1 , strml2
   INTEGER , DIMENSION(7) :: buf
   INTEGER :: file , flag , i , ibuf1 , ibuf2 , ibuf3 , igdp , intrl , ip1 , ip2 , ip3 , ip4 , ipos , ipos1 , ipos2 , ipos3 , ipv , &
            & isilc , isln , j , jloc , kn , kode , last , left , macmax , macmin , maxsl , mthd , n1 , n2 , n3 , n4 , nbg1 , nbg2 ,&
            & nbgl , ncol , ncstm1 , ncstm2 , ncstml , neq1 , neq2 , neq21 , neq22 , neq2l , neql , next , nline , nlines , nogo ,  &
            & ns , nsl , nsl1a , nsl1b , nsl1l , nsl2a , nsl2b , nsl2l , nstns , nstnsx , nx
   LOGICAL , SAVE :: first
   INTEGER , DIMENSION(7) , SAVE :: itrl
   INTEGER , DIMENSION(6) :: iz
   LOGICAL :: lmkaer
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(6,2) , SAVE :: name1
   INTEGER , DIMENSION(100) :: pstrm
   REAL :: xsign , ytip1 , ytip2
   EXTERNAL apdb1 , apdb2 , bisloc , bug1 , close , corwds , fname , gopen , korsz , locate , makmcb , mesage , open , pack ,       &
          & preloc , rdtrl , read , sswtch , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     AERODYNAMIC POOL DISTRIBUTOR AND GEOMETRY INTERPOLATOR FOR
!     COMPRESSOR BLADES (AERODYNAMIC THEORY 6) AND SWEPT TURBOPROP
!     BLADES (AERODYNAMIC THEORY 7).
!
!     THIS IS THE DMAP DRIVER FOR APDB
!
!     DMAP CALLING SEQUENCE
!
!     APDB     EDT,USET,BGPDT,CSTM,EQEXIN,GM,GO / AEROB,ACPT,FLIST,
!              GTKA,PVECT / V,N,NK/V,N,NJ/V,Y,MINMACH/V,Y,MAXMACH/
!              V,Y,IREF/V,Y,MTYPE/V,N,NEIGV/V,Y,KINDEX $
!
!     INPUT  DATA BLOCKS CSTM, GM AND GO MAY BE PURGED
!     OUTPUT DATA BLOCK  PVECT MAY BE PURGED
!     PARAMETERS NK AND NJ ARE OUTPUT, THE OTHERS ARE INPUT
!
!
!     NAMES  -VALUE =  2   0    3    1      1      2     3
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Minmac,Macmin) , (Maxmac,Macmax)
   DATA aero/3202 , 32 , 0/ , mkaer1/3802 , 38 , 0/ , mkaer2/3702 , 37 , 0/
   DATA fluttr/3902 , 39 , 0/ , flfact/4102 , 41 , 0/
   DATA strml1/3292 , 92 , 0/ , strml2/3293 , 93 , 0/
   DATA edt , bgpdt , cstm , eqexin/101 , 103 , 104 , 105/
   DATA aerob , acpt , flist , pvect/201 , 202 , 203 , 205/
   DATA name/4HAPDB , 4H    / , scr1/301/
   DATA itrl/7*0/ , first/.TRUE./ , sine/4HSINE/
   DATA name1(1,1) , name1(1,2)/4HAERO , 4H    /
   DATA name1(2,1) , name1(2,2)/4HMKAE , 4HRO  /
   DATA name1(3,1) , name1(3,2)/4HFLFA , 4HCT  /
   DATA name1(4,1) , name1(4,2)/4HFLUT , 4HTER /
   DATA name1(5,1) , name1(5,2)/4HSTRE , 4HAML1/
   DATA name1(6,1) , name1(6,2)/4HSTRE , 4HAML2/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Debug = .FALSE.
         CALL sswtch(20,j)
         IF ( j==1 ) Debug = .TRUE.
!
!     SELECT AERODYNAMIC THEORY
!
!     COMPRESSOR BLADES (AERODYNAMIC THEORY 6).
!     SWEPT TURBOPROPS  (AERODYNAMIC THEORY 7).
!
!     AT PRESENT THE USER SELECTS THE THEORY VIA THE NASTRAN CARD.
!     SET SYSTEM(93)=0  FOR THEORY 6 OR SYSTEM(93)=1 FOR THEORY 7.
!     NOTE - THE DEFAULT IS THEORY 6 (SYSTEM(93)=0).
!
!     FOR EXAMPLE, TO SELECT THEORY 7, USE THE FOLLOWING CARD -
!     NASTRAN SYSTEM(93)=1
!
         IF ( Nsys(91)==0 ) mthd = 6
         IF ( Nsys(91)==1 ) mthd = 7
!
         IF ( Debug ) CALL bug1('BLANK COMM',1,Nk,9)
         nogo = 0
         maxsl = 100
         ibuf1 = korsz(Z) - Sysbuf
         ibuf2 = ibuf1 - Sysbuf
         ibuf3 = ibuf2 - Sysbuf
         last = ibuf3 - Sysbuf - 1
         IF ( last>0 ) THEN
            left = corwds(Z(1),Z(last))
!
!     CREATE AEROB DATA BLOCK
!
            CALL gopen(aerob,Z(ibuf2),Wrtrew)
!
!     READ AERO CARD VALUES - BREF, SYMXZ AND SYMXY
!
            file = edt
            CALL preloc(*420,Z(ibuf1),edt)
            CALL locate(*320,Z(ibuf1),aero,flag)
            CALL read(*440,*460,edt,Z(1),6,1,flag)
            IF ( Debug ) CALL bug1('AERO CARD ',2,Z,6)
            iz(1) = iz(5)
            iz(2) = iz(6)
            CALL write(aerob,Z,3,1)
!
!     READ IN MKAERO1 CARDS
!
            lmkaer = .FALSE.
            next = 1
            CALL locate(*40,Z(ibuf1),mkaer1,flag)
            CALL read(*440,*20,edt,Z(next),left,1,nx)
         ENDIF
!
!     NOT ENOUGH CORE
!
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 20      n1 = next
         IF ( Debug ) CALL bug1('MKAERO1   ',10,Z(n1),nx)
         lmkaer = .TRUE.
         spag_nextblock_1 = 2
      CASE (2)
         n2 = n1 + 7
         SPAG_Loop_1_1: DO i = n1 , n2
            IF ( iz(i)==-1 ) EXIT SPAG_Loop_1_1
            buf(1) = iz(i)
            n3 = n2 + 1
            n4 = n3 + 7
            SPAG_Loop_2_2: DO j = n3 , n4
               IF ( iz(j)==-1 ) EXIT SPAG_Loop_2_2
               buf(2) = iz(j)
               CALL write(aerob,buf,2,0)
            ENDDO SPAG_Loop_2_2
         ENDDO SPAG_Loop_1_1
         IF ( n4-next+1<nx ) THEN
            n1 = n1 + 16
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ IN MKAERO2 CARDS
!
 40      CALL locate(*80,Z(ibuf1),mkaer2,flag)
         CALL read(*440,*60,edt,Z(next),left,1,nx)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 60      CALL write(aerob,Z(next),nx,0)
         IF ( Debug ) CALL bug1('MKAERO2   ',70,Z(next),nx)
         lmkaer = .TRUE.
 80      CALL write(aerob,0,0,1)
         CALL close(aerob,Clsrew)
         IF ( .NOT.lmkaer ) THEN
!
!     NO MKAERO1 OR MKAERO2 CARDS FOUND
!
            kode = 2
            WRITE (Iout,99001) Ufm , name1(kode,1) , name1(kode,2)
            CALL mesage(-37,0,name)
            RETURN
         ELSE
            itrl(1) = aerob
            itrl(2) = 1
            CALL wrttrl(itrl)
!
!     CREATE FLIST TABLE
!
            CALL open(*100,flist,Z(ibuf2),Wrtrew)
            CALL fname(flist,iz(next))
            CALL write(flist,iz(next),2,1)
            CALL locate(*320,Z(ibuf1),aero,flag)
            CALL read(*440,*120,edt,Z(next),left,1,nx)
            ip1 = -8
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FLIST CAN BE PURGED IF THE APPROACH IS NOT AERO
!
 100     IF ( iabs(Nsys(19))/=4 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = flist
         GOTO 420
 120     CALL write(flist,aero,3,0)
         CALL write(flist,Z(next),nx,1)
         IF ( Debug ) CALL bug1('FLIST AERO',90,Z(next),nx)
         CALL locate(*340,Z(ibuf1),flfact,flag)
         CALL read(*440,*140,edt,Z(next),left,1,nx)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     CALL write(flist,flfact,3,0)
         CALL write(flist,Z(next),nx,1)
         IF ( Debug ) CALL bug1('FLIST FLFA',100,Z(next),nx)
         CALL locate(*360,Z(ibuf1),fluttr,flag)
         CALL read(*440,*160,edt,Z(next),left,1,nx)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 160     CALL write(flist,fluttr,3,0)
         CALL write(flist,Z(next),nx,1)
         IF ( Debug ) CALL bug1('FLIST FLUT',110,Z(next),nx)
         CALL close(flist,Clsrew)
         itrl(1) = edt
         CALL rdtrl(itrl)
         itrl(1) = flist
         CALL wrttrl(itrl)
         spag_nextblock_1 = 3
      CASE (3)
!
!     CREATE ACPT TABLE
!
         CALL gopen(acpt,Z(ibuf2),Wrtrew)
!
!     STORE EXTERNAL NODE NUMBER, INTERNAL NODE NUMBER AND BASIC
!     COORDINATES OF ALL NODES ON BLADE ON SCR1
!
         CALL gopen(scr1,Z(ibuf3),Wrtrew)
!
!     READ STREAML1 AND STREAML2 CARDS. STORE IN-CORE
!
         nsl1a = next
         CALL locate(*380,Z(ibuf1),strml1,flag)
         CALL read(*440,*180,edt,Z(nsl1a),left,1,nsl1l)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 180     nsl1b = nsl1a + nsl1l - 1
         IF ( Debug ) CALL bug1('STREAML1  ',120,Z(nsl1a),nsl1l)
         nsl2a = nsl1b + 1
         left = corwds(Z(nsl2a),Z(last))
         CALL locate(*400,Z(ibuf1),strml2,flag)
         CALL read(*440,*200,edt,Z(nsl2a),left,1,nsl2l)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 200     nsl2b = nsl2a + nsl2l - 1
         IF ( Debug ) CALL bug1('STREAML2  ',130,Z(nsl2a),nsl2l)
         CALL close(edt,Clsrew)
!
!     INPUT CHECKS  (ALL ARE THEORY DEPENDENT RESTRICTIONS)
!     STREAML1 - ALL CARDS MUST HAVE THE SAME NUMBER OF NODES
!     STREAML2 - THERE MUST BE AT LEAST THREE(3) STREAML2 CARDS.
!                (THIS IS A THEORY DEPENDENT RESTRICTION,
!                SEE AMG MODULE - COMPRESSOR BLADE CODE FOR AJJL)
!              - NSTNS MUST BE THE SAME FOR ALL STREAML2 CARDS
!                AND MUST EQUAL THE NO. OF NODES ON THE STRAML1 CARD
!
!     COUNT THE NUMBER OF STREAML2 CARDS
!
         nlines = nsl2l/10
         IF ( Debug ) CALL bug1('NLINES    ',131,nlines,1)
         IF ( nlines<3 ) THEN
            nogo = 1
            WRITE (Iout,99002) Ufm , nlines
99002       FORMAT (A23,' - APDB MODULE - THE NO. OF STREAML2 CARDS INPUT =',I3,/40X,                                               &
                   &'THERE MUST BE AT LEAST THREE(3) STREAML2 CARDS',' INPUT.')
         ENDIF
         IF ( nlines>maxsl ) THEN
!
!     MAXIMUM NUMBER OF STREAML2 CARDS EXCEEDED FOR
!     LOCAL ARRAY PSTRM. SEE ERROR MESSAGE FOR FIX.
!
            WRITE (Iout,99003) Ufm , maxsl
99003       FORMAT (A23,' - APDB MODULE - MAXIMUM NUMBER OF STREAML2 CARDS ','EXCEEDED FOR LOCAL ARRAY PSTRM.',/40X,                &
                   &'UPDATE VARABLE MAXSL AND ARRAY PSTRM IN ROUTINE APDB.',/40X,'CURRENT VALUE OF MAXSL AND DIMENSION OF PSTRM =', &
                  & I4)
            CALL mesage(-37,0,name)
            RETURN
         ELSE
!
!     LOCATE STREAML1 CARDS THAT CORRESPOND TO STREAML2 CARDS BY
!     MATCHING SLN VALUES
!
            nline = 0
            DO isln = nsl2a , nsl2b , 10
               nline = nline + 1
               pstrm(nline) = -iz(isln)
            ENDDO
!
!     LOCATE SLN AND COUNT THE NUMBER OF COMPUTING STATIONS
!
            ipos = nsl1a
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_3: DO ns = ipos , nsl1b
            IF ( iz(ns)==-1 ) EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
!
!     CHECK FOR VALID SLN
!
         DO nline = 1 , nlines
            IF ( iz(ipos)==-pstrm(nline) ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         pstrm(nline) = ipos
         nstnsx = ns - ipos - 1
         IF ( first ) THEN
            nstns = nstnsx
            first = .FALSE.
!
!     ALL NSTNSX MUST BE THE SAME
!
         ELSEIF ( nstnsx/=nstns ) THEN
            nogo = 2
            WRITE (Iout,99004) Ufm , iz(ipos)
99004       FORMAT (A23,' - APDB MODULE - ILLEGAL NO. OF NODES ON STREAML1 ','CARD WITH SLN =',I8,/40X,                             &
                   &'ALL STREAML1 CARDS MUST HAVE THE SAME NUMBER OF NODES.')
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         ipos = ns + 1
         IF ( ipos<nsl1b ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     IS THERE A STREAML1 CARD FOR EVERY STREAML2 CARD
!
         DO nline = 1 , nlines
            IF ( pstrm(nline)<=0 ) THEN
               nogo = 3
               isln = -pstrm(nline)
               WRITE (Iout,99005) Ufm , isln
99005          FORMAT (A23,' - APDB MODULE - NO STREAML1 CARD FOR THE STREAML2',' WITH SLN =',I8)
            ENDIF
         ENDDO
         IF ( nogo>0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ BGPDT
!
         nbg1 = nsl2b + 1
         left = corwds(Z(nbg1),Z(last))
         file = bgpdt
         CALL gopen(bgpdt,Z(ibuf1),Rdrew)
         CALL read(*440,*220,bgpdt,Z(nbg1),left,1,nbgl)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 220     CALL close(bgpdt,Clsrew)
         IF ( Debug ) CALL bug1('BGPDT     ',200,Z(nbg1),nbgl)
         nbg2 = nbg1 + nbgl - 1
!
!     READ EQEXIN (RECORD 1)
!
         neq1 = nbg2 + 1
         left = corwds(Z(neq1),Z(last))
         file = eqexin
         CALL gopen(eqexin,Z(ibuf1),Rdrew)
         CALL read(*440,*240,eqexin,Z(neq1),left,1,neql)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 240     neq2 = neq1 + neql - 1
         IF ( Debug ) CALL bug1('EQEXIN R1 ',210,Z(neq1),neql)
!
!     READ EQEXIN (RECORD 2)
!
         neq21 = neq2 + 1
         left = corwds(Z(neq21),Z(last))
         CALL read(*440,*260,eqexin,Z(neq21),left,1,neq2l)
         ip1 = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 260     neq22 = neq2 + neq2l - 1
         IF ( Debug ) CALL bug1('EQEXIN R2 ',212,Z(neq21),neq2l)
         CALL close(eqexin,Clsrew)
!
!     WRITE ACPT
!
!     KEY WORD = 6 FOR COMPRESSOR BLADES, I.E. METHOD ID = 6
!     KEY WORD = 7 FOR SWEPT TURBOPROPS , I.E. METHOD ID = 7
!
!     WRITE CONSTANT PARAMETERS, WORDS 1 - 6
!
         buf(1) = mthd
         buf(2) = Iref
         buf(3) = macmin
         buf(4) = macmax
         buf(5) = nlines
         buf(6) = nstns
         CALL write(acpt,buf,6,0)
         IF ( Debug ) CALL bug1('ACPT WRT 1',216,buf,6)
!
!     WRITE STREAMLINE DATA
!
         kn = neql/2
         nline = 0
         DO nsl = nsl2a , nsl2b , 10
!
!     MAKE SURE NSTNS ON ALL STREAML2 CARDS IS THE SAME
!
            IF ( iz(nsl+1)/=nstns ) THEN
               WRITE (Iout,99006) Uwm , iz(nsl)
99006          FORMAT (A25,' - APDB MODULE - STREAML2 WITH SLN =',I8,/42X,'NSTNS INCONSISTENT WITH NO. OF NODES ON STREAML2 CARD ', &
                      &'FOR BLADE ROOT.',/42X,'CORRECT VALUE OF NSTNS WILL BE ','SUBSTITUTED ON STREAML2 CARD.')
               iz(nsl+1) = nstns
            ENDIF
!
!     WRITE STREAML2 DATA
!
            CALL write(acpt,Z(nsl),10,0)
            IF ( Debug ) CALL bug1('ACPT WRT 2',217,Z(nsl),10)
!
!     WRITE BASIC X, Y AND Z FOR EACH NODE ON STREAML1 CARD
!
            nline = nline + 1
            ipos = pstrm(nline)
            ipos1 = ipos + 1
            ipos2 = ipos + nstns
            DO igdp = ipos1 , ipos2
!
!     LOCATE INTERNAL NUMBER THAT CORRESOONDS TO THIS EXTERNAL NODE
!
               CALL bisloc(*265,iz(igdp),iz(neq1),2,kn,jloc)
!
!     PICK-UP BASIC GRID DATA FOR THIS NODE
!
               intrl = iz(neq1+jloc)
               isilc = iz(neq21+jloc)
               jloc = nbg1 + (intrl-1)*4
               buf(1) = iz(igdp)
               buf(2) = intrl
               buf(3) = isilc
               buf(4) = iz(jloc)
               buf(5) = iz(jloc+1)
               buf(6) = iz(jloc+2)
               buf(7) = iz(jloc+3)
!
!     TEST FOR SCALAR POINT (CID = -1)
!
               IF ( buf(4)<0 ) THEN
                  nogo = 6
                  WRITE (Iout,99007) Ufm , iz(ipos) , iz(igdp)
99007             FORMAT (A23,' - APDB MODULE - STREAML1 CARD WITH SLN =',I8,' REFERENCES A SCALAR POINT WITH EXTERNAL ID =',I8,    &
                        & /40X,'SCALAR POINTS ARE ILLEGAL. USE A GRID POINT.')
               ENDIF
               CALL write(acpt,buf(5),3,0)
               CALL write(scr1,buf,7,0)
               IF ( Debug ) CALL bug1('ACPT WRT 3',227,buf,7)
!
!-----DETERMINE DIRECTION OF BLADE ROTATION VIA Y-COORDINATES AT TIP
!-----STREAMLINE. USE COORDINATES OF FIRST 2 NODES ON STREAMLINE.
!
               IF ( nline==nlines .AND. igdp==ipos1 ) ytip1 = Z(jloc+2)
               IF ( nline==nlines .AND. igdp==ipos1+1 ) ytip2 = Z(jloc+2)
               CYCLE
!
!     STREAML1 REFERNCES AN EXTERNAL ID THAT DOES NOT EXIST
!
 265           nogo = 5
               WRITE (Iout,99008) Ufm , iz(ipos) , iz(igdp)
99008          FORMAT (A23,' - APDB MODULE - STREAML1 CARD WITH SLN =',I8,' REFERENCES NON-EXISTENT EXTERNAL NODE =',I8)
!
            ENDDO
         ENDDO
!
         xsign = 1.0
         IF ( ytip2<ytip1 ) xsign = -1.0
         IF ( Debug ) CALL bug1('XSIN      ',240,xsign,1)
         CALL write(acpt,0,0,1)
         CALL write(scr1,0,0,1)
         CALL close(acpt,Clsrew)
         CALL close(scr1,Clsrew)
         itrl(1) = acpt
         itrl(2) = 1
         itrl(3) = 0
         itrl(4) = 0
         itrl(5) = 0
         itrl(6) = 0
         itrl(7) = 0
         CALL wrttrl(itrl)
         IF ( nogo>0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SET OUTPUT PARAMETERS NK AND NJ FOR APPROPRIATE THEORY.
!
!     COMPRESSOR BLADES (THEORY 6) - NK = NJ = NSTNS*NLINES.
!     SWEPT TURBOPROPS  (THEORY 7) - NK = NJ = 2*NSTNS*NLINES.
!
         IF ( mthd==6 ) Nk = nstns*nlines
         IF ( mthd==7 ) Nk = 2*nstns*nlines
         Nj = Nk
         IF ( Debug ) CALL bug1('BLANK COM ',241,Nk,9)
!
!     CREATE PVECT PARTITIONING VECTOR     (PVECT MAY BE PURGED)
!     PVECT IS A COLUMN PARTITIONING VECTOR TO BE USED BY MODULE PARTN
!     TO PARTITION OUT EITHER THE SINE OR COSINE COLUMNS OF MATRIX
!     PHIA WHICH IS OUTPUT BY THE CYCT2 MODULE  WHEN DOING A CYCLIC
!     NORMAL MODES ANALYSIS
!     PARAMETER MTYPE=SINE OR COSINE (DEFAULT IS COSINE)
!
!     OPEN PVECT AND WRITE HEADER
!
         CALL open(*280,pvect,Z(ibuf2),Wrtrew)
!
!     TEST FOR VALID NEIGV AND KINDEX
!
         IF ( Neigv<=0 .OR. Kindex<0 ) THEN
!
!     NEIGV OR KINDEX INVALID
!
            WRITE (Iout,99009) Ufm , Neigv , Kindex
!
99009       FORMAT (A23,' - APDB MODULE - INVALID PARAMETER NEIGV OR KINDEX',' INPUT.',/40X,                                        &
                   &'DATA BLOCK PVECT (FILE 205) CANNOT BE GENERATED.',/40X,7HNEIGV =,I8,10H, KINDEX =,I8)
            CALL mesage(-37,0,name)
            RETURN
         ELSE
!
            CALL fname(pvect,buf)
            CALL write(pvect,buf,2,1)
!
!     PVECT IS TO BE GENERATED
!
            left = left - neq2
            ncol = Neigv
            IF ( Kindex>0 ) ncol = 2*ncol
            ipos1 = neq2 + 1
            ipos2 = neq2 + ncol
            DO ipv = ipos1 , ipos2
               Z(ipv) = 0.0
            ENDDO
            IF ( Kindex/=0 ) THEN
               ipos3 = ipos1
               IF ( Mtype(1)/=sine ) ipos3 = ipos1 + 1
               DO ipv = ipos3 , ipos2 , 2
                  Z(ipv) = 1.0
               ENDDO
            ENDIF
            Typin = 1
            Typout = 1
            Ii = 1
            Nn = ncol
            Incr = 1
            CALL makmcb(itrl,pvect,ncol,2,1)
            CALL pack(Z(ipos1),pvect,itrl)
            IF ( Debug ) CALL bug1('PVECT     ',260,Z(ipos1),ncol)
            CALL close(pvect,Clsrew)
            CALL wrttrl(itrl)
         ENDIF
!
!     GENERATE GTKA TRANSFORMATION MATRIX
!
!     READ CSTM INTO CORE
!
 280     ncstm1 = 1
         ncstml = 0
         file = cstm
         itrl(1) = cstm
         CALL rdtrl(itrl)
         IF ( itrl(1)==cstm ) THEN
            left = corwds(Z(ncstm1),Z(last))
            CALL gopen(cstm,Z(ibuf1),Rdrew)
            CALL read(*440,*300,cstm,Z(ncstm1),left,1,ncstml)
            ip1 = -8
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 300     ncstm2 = ncstm1 + ncstml - 1
         IF ( Debug ) CALL bug1('CSTM      ',300,Z(ncstm1),ncstml)
         CALL close(cstm,Clsrew)
!
!     ALLOCATE WORK STORAGE
!
         ip1 = ncstm2 + 1
         ip2 = ip1 + nstns
         ip3 = ip2 + nstns
         ip4 = ip3 + nstns
         next = ip4 + 4*nstns
         left = left - next + 1
         IF ( left<=0 ) THEN
            ip1 = -8
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     GENERATE GTKA TRANSFORMATION MATRIX FOR APPROPRIATE THEORY.
!
!     COMPRESSOR BLADES (AERODYNAMIC THEORY 6).
!
            IF ( mthd==6 ) CALL apdb1(ibuf1,ibuf2,next,left,nstns,nlines,xsign,ncstml,Z(ncstm1),Z(ip1),Z(ip2),Z(ip3),Z(ip4))
!
!     SWEPT TURBOPROPS (AERODYNAMIC THEORY 7).
!
            IF ( mthd==7 ) CALL apdb2(ibuf1,ibuf2,next,left,nstns,nlines,xsign,ncstml,Z(ncstm1),Z(ip1),Z(ip2),Z(ip3),Z(ip4))
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ERROR MESSAGES
!
!     NO AERO CARD FOUND
 320     kode = 1
         WRITE (Iout,99001) Ufm , name1(kode,1) , name1(kode,2)
         CALL mesage(-37,0,name)
         RETURN
!
!     NO FLFACT CARD FOUND
!
 340     kode = 3
         WRITE (Iout,99001) Ufm , name1(kode,1) , name1(kode,2)
         CALL mesage(-37,0,name)
         RETURN
!
!     NO FLUTTER CARD FOUND
!
 360     kode = 4
         WRITE (Iout,99001) Ufm , name1(kode,1) , name1(kode,2)
         CALL mesage(-37,0,name)
         RETURN
!
!     NO STREAML1 CARD FOUND
!
 380     kode = 5
         WRITE (Iout,99001) Ufm , name1(kode,1) , name1(kode,2)
         CALL mesage(-37,0,name)
         RETURN
!
!     NO STREAML2 CARD FOUND
!
 400     kode = 6
         WRITE (Iout,99001) Ufm , name1(kode,1) , name1(kode,2)
         CALL mesage(-37,0,name)
         RETURN
!
!     DATA SET NOT IN FIST
!
 420     ip1 = -1
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     E-O-F ENCOUNTERED
!
 440     ip1 = -2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     E-O-L ENCOUNTERED
!
 460     ip1 = -3
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(ip1,file,name)
         spag_nextblock_1 = 8
      CASE (8)
!
         IF ( nogo/=0 ) CALL mesage(-37,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99001 FORMAT (A23,' - MODULE APDB - BULK DATA CARD ',2A4,' MISSING FROM INPUT DECK.')
END SUBROUTINE apdb
