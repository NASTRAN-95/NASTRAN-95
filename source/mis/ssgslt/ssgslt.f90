!*==ssgslt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssgslt(Slt,Newslt,Est)
!
!     THIS SUBROUTINE OF THE SSG1 MODULE COPIES THE SLT TO ANOTHER
!     FILE.  IN THE COPYING PROCESS ANY -QVOL-, -QBDY1-, -QBDY2-, OR
!     -QVECT- EXTERNAL LOAD TYPE DATA FOUND WILL BE ALTERED SO AS TO
!     REPLACE THEIR ELEMENT ID REFERENCES WITH THE APPROPRIATE SILS, AND
!     MISC. CONSTANTS.  THE EXTERNAL LOADS WILL BE PREPARED AS USUAL FOR
!     THESE AND OTHER LOAD CARD TYPES VIA SUBROUTINE EXTERN.
!
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Slt
   INTEGER :: Newslt
   INTEGER :: Est
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: any , bgcore , bgopen , nogo
   REAL :: area , hc1 , hc2 , hc3 , q0 , xx , yy , zz
   INTEGER , SAVE :: bgpdt , cbar , chbdy , chexa1 , chexa2 , cihex1 , cihex2 , cihex3 , conrod , cqdmem , cqdmm1 , cqdmm2 ,        &
                   & cquad1 , cquad2 , cquad4 , crod , ctetra , ctria1 , ctria2 , ctria3 , ctrirg , ctrmem , ctrprg , ctube ,       &
                   & cwedge , eor , noeor , ntypes
   INTEGER , DIMENSION(50) :: buf
   INTEGER :: buf1 , buf2 , buf3 , core , dum , elid , eltype , entrys , estwds , grid1 , i , i1 , i2 , iarea , id , idbdy1 ,       &
            & idbdy2 , idptr , idqvec , idqvol , idx , ieor , ifirst , iflag , igrid , incnt , index , iretrn , isub , isub1 ,      &
            & itype , iwords , j , j1 , j2 , jcor , jcore , jcore1 , jcoren , jcorex , jindex , jpoint , jtype , k , k1 , k2 ,      &
            & kcore , kk , l , ncore , nel , nels , next , npts , nrecs , nwords , outcnt , outpt , points , qbdy1s , qbdy2s ,      &
            & qvects , sysbuf
   INTEGER , DIMENSION(100) :: ecpt
   INTEGER , DIMENSION(7) :: mcb
   REAL , SAVE :: piovr4
   REAL , DIMENSION(50) :: rbuf
   REAL , DIMENSION(100) :: recpt
   REAL , DIMENSION(1) :: rz
   INTEGER , DIMENSION(2) , SAVE :: subr
   INTEGER , DIMENSION(25,4) , SAVE :: type
   EXTERNAL axloop , bisloc , close , dipole , fname , fread , fwdrec , geloop , gopen , hbdy , korsz , mesage , open , rdtrl ,     &
          & read , rewind , sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (ecpt(1),recpt(1)) , (buf(1),rbuf(1)) , (Z(1),Rz(1))
   DATA subr/4HSSGS , 4HLT  / , noeor , eor/0 , 1/
   DATA bgpdt/102/
   DATA piovr4/0.7853981634E0/
   DATA cbar/34/
   DATA crod/1/
   DATA conrod/10/
   DATA ctube/3/
   DATA ctrmem/9/
   DATA ctria1/6/
   DATA ctria2/17/
   DATA cqdmem/16/
   DATA cqdmm1/62/
   DATA cqdmm2/63/
   DATA cquad4/64/
   DATA ctria3/83/
   DATA cquad1/19/
   DATA cquad2/18/
   DATA ctrirg/36/
   DATA ctrprg/37/
   DATA ctetra/39/
   DATA cwedge/40/
   DATA chexa1/41/
   DATA chexa2/42/
   DATA chbdy/52/
   DATA cihex1/65/
   DATA cihex2/66/
   DATA cihex3/67/
!
   DATA ntypes/25/
!
!          SLT            NEWSLT         FLAG FOR       DATA
!          WORDS-IN       WORDS-OUT      SPEC-PROC      CORE-LOCAT
!          ==========     ==========     ==========     ==========
!     FORCE
   DATA type(1,1)/6/ , type(1,2)/6/ , type(1,3)/0/ , type(1,4)/0/
!     MOMENT
   DATA type(2,1)/6/ , type(2,2)/6/ , type(2,3)/0/ , type(2,4)/0/
!     FORCE1
   DATA type(3,1)/4/ , type(3,2)/4/ , type(3,3)/0/ , type(3,4)/0/
!     MOMNT1
   DATA type(4,1)/4/ , type(4,2)/4/ , type(4,3)/0/ , type(4,4)/0/
!     FORCE2
   DATA type(5,1)/6/ , type(5,2)/6/ , type(5,3)/0/ , type(5,4)/0/
!     MOMNT2
   DATA type(6,1)/6/ , type(6,2)/6/ , type(6,3)/0/ , type(6,4)/0/
!     SLOAD
   DATA type(7,1)/2/ , type(7,2)/2/ , type(7,3)/0/ , type(7,4)/0/
!     GRAV
   DATA type(8,1)/5/ , type(8,2)/5/ , type(8,3)/0/ , type(8,4)/0/
!     PLOAD
   DATA type(9,1)/5/ , type(9,2)/5/ , type(9,3)/0/ , type(9,4)/0/
!     RFORCE
   DATA type(10,1)/6/ , type(10,2)/6/ , type(10,3)/0/ , type(10,4)/0/
!     PRESAX
   DATA type(11,1)/6/ , type(11,2)/6/ , type(11,3)/0/ , type(11,4)/0/
!     QHBDY
   DATA type(12,1)/7/ , type(12,2)/7/ , type(12,3)/0/ , type(12,4)/0/
!     QVOL
   DATA type(13,1)/2/ , type(13,2)/12/ , type(13,3)/1/ , type(13,4)/0/
!     QBDY1
   DATA type(14,1)/2/ , type(14,2)/10/ , type(14,3)/1/ , type(14,4)/0/
!     QBDY2
   DATA type(15,1)/5/ , type(15,2)/10/ , type(15,3)/1/ , type(15,4)/0/
!     QVECT
   DATA type(16,1)/5/ , type(16,2)/19/ , type(16,3)/1/ , type(16,4)/0/
!     PLOAD3
   DATA type(17,1)/38/ , type(17,2)/38/ , type(17,3)/0/ , type(17,4)/0/
!     PLOAD1
   DATA type(18,1)/7/ , type(18,2)/7/ , type(18,3)/0/ , type(18,4)/0/
!     PLOADX
   DATA type(19,1)/5/ , type(19,2)/5/ , type(19,3)/0/ , type(19,4)/0/
!     SPCFLD  (WORDS OUT IS A DUMMY VALUE-IT WILL REALLY BE 3*NROWSP)
   DATA type(20,1)/5/ , type(20,2)/4/ , type(20,3)/0/ , type(20,4)/0/
!     CEMLOOP
   DATA type(21,1)/12/ , type(21,2)/12/ , type(21,3)/0/ , type(21,4)/0/
!     GEMLOOP  ,BOTH INPUT AND OUTPUT ARE DUMMY.
   DATA type(22,1)/5/ , type(22,2)/4/ , type(22,3)/0/ , type(22,4)/0/
!     MDIPOLE (OUTPUT VALUE IS A DUMMY)
   DATA type(23,1)/9/ , type(23,2)/5/ , type(23,3)/0/ , type(23,4)/0/
!     REMFLUX    (OUTPUT VALUE IS A DUMMY)
   DATA type(24,1)/5/ , type(24,2)/5/ , type(24,3)/0/ , type(24,4)/0/
!     PLOAD4
   DATA type(25,1)/11/ , type(25,2)/11/ , type(25,3)/0/ , type(25,4)/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!                     SLT                         NEWSLT
!     CARD=TYPE       WORDS IN                    WORDS OUT
!     =========       ========                    =========
!
!     QVOL=13         1 = QV                      1 = NUM-POINTS(1 TO 8)
!                     2 = ELEMENT ID              2 = ELEMENT ID
!                                                 3 THRU 10 = 8 SILS
!                                                 11 = COEFICIENT
!                                                 12 = TYPE 1 = 1 DIMEN
!                                                           2 = 2 DIMEN
!                                                           3 = BELL-EL
!                                                           4 = SOLID
!
!     QBDY1=14        1 = Q0                      1 = TYPE (1 TO 5)
!                     2 = ELEMENT ID              2 = ELEMENT ID
!                                                 3 THRU  6 = 4 SILS
!                                                 7 THRU 10 = 4 COEFS.
!
!     QBDY2=15        1 = ELEMENT ID              1 = ELEMENT ID
!                     2 = Q01                     2 = TYPE (1 TO 5)
!                     3 = Q02                     3 THRU  6 = 4 SILS
!                     4 = Q03                     7 THRU 10 = 4 COEFS.
!                     5 = Q04
!
!     QVECT=16        1 = Q0                      1 THRU 4 = 4 SILS
!                     2 = E1                      5 = ELEMENT ID
!                     3 = E2                      6 = TYPE (1 TO 5)
!                     4 = E3                      7 THRU 10 = 4 COEFS.
!                     5 = ELEMENT ID              11 = E1
!                                                 12 = E2
!                                                 13 = E3
!                                                 14 THRU 16 = V1 VECTOR
!                                                 17 THRU 19 = V2 VECTOR
!
!
!
!     SPCFLD=20       1 = CID                     1 THRU 3*NROWSP=
!                     2 = HCX                     TOTAL HC VALUES AT
!                     3 = HCY                     THE GRID POINTS
!                     4 = HCZ
!                     5 = GRID ID OR -1
!
!
!     CEMLOOP=21                                  SAME AS FOR
!     GEMLOOP=22                                  SPCFLD
!
!
!
!     MDIPOLE=23      1  =CID                     SAME AS
!                     2-4=LOCATION OF DIPOLE      SPCFLD
!                     5-7=DIPOLE MOMENT
!                     8  =MIN. DISTANCE
!                     9  =MAX. DISTANCE
!
!     REMFLUX=24      SAME INPUT AS               1 THRU 3*(NO. OF
!                     SPCFLD EXCEPT               ELEMENTS)= TOTAL
!                     WORD 5 IS ELEMENT ID        REMANENT FLUX DENSITY
!                                                 FOR EACH ELEMNT IN
!                                                 ORDER ON EST
!
!     THE ELEMENT ID MUST REMAIN IN THE SAME LOCATION ON OUTPUT.
!
!
!     SET UP CORE AND BUFFERS. (PG BUFFER IS OPEN IN SSG1)
!
         bgcore = .FALSE.
         bgopen = .FALSE.
         nogo = .FALSE.
         buf1 = korsz(z) - 2*sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         buf3 = buf2 - sysbuf - 2
         core = buf3 - 1
         IF ( core<100 ) CALL mesage(-8,0,subr)
!
!     OPEN SLT, AND NEWSLT.  COPY HEADER RECORD ACROSS.
!
         CALL open(*300,Slt,z(buf1),rdrew)
         CALL open(*320,Newslt,z(buf2),wrtrew)
         CALL read(*340,*20,Slt,z,core,eor,iwords)
         CALL mesage(-8,0,subr)
 20      CALL fname(Newslt,z)
         CALL write(Newslt,z,iwords,eor)
!
!     READ TRAILER OF SLT AND GET COUNT OF LOAD SET RECORDS.
!
         mcb(1) = Slt
         CALL rdtrl(mcb)
         nrecs = mcb(2)
         mcb(1) = Newslt
         CALL wrttrl(mcb)
!
!     PROCESSING OF LOAD SET RECORDS IF ANY.
!
         IF ( nrecs<=0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = 1
         spag_nextblock_1 = 2
      CASE (2)
         any = .FALSE.
         ncore = nrecs + 2
         ifirst = 0
!
!     ZERO OUT EXISTANCE FLAGS
!
         DO k = 1 , ntypes
            type(k,4) = 0
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ CARD TYPE AND COUNT OF CARDS
!
         CALL read(*340,*40,Slt,buf,2,noeor,iwords)
         spag_nextblock_1 = 4
      CASE (4)
         itype = buf(1)
         entrys = buf(2)
!
!     CHECK FOR KNOWN TYPE
!
         IF ( itype>ntypes .OR. itype<=0 ) THEN
            WRITE (outpt,99001) sfm , itype
99001       FORMAT (A25,' 3094, SLT LOAD TYPE',I9,' IS NOT RECOGNIZED.')
            CALL mesage(-61,0,subr)
         ENDIF
!
!     CHECK FOR SPECIAL PROCESSING.
!
         incnt = type(itype,1)
!
!     IF TYPE IS CEMLOOP,SPCFLD,MDIPOLE, OR GEMLOOP,GO TO 800 FOR
!     SPECIAL PROCESSING. GO TO 1000 FOR REMFLUX PROCESSING
!
         IF ( itype>=20 .AND. itype<=23 ) THEN
!
!     SPECIAL PROCESSING FOR SPCFLD,CEMLOOP,MDIPOLE, AND GEMLOOP. SET UP
!     A VECTOR FOR ALL BUT SPCFLD CARDS, COMPUTE FIELD AT EACH POINT
!     IN BGPDT. WHEN FINISHED, ALL THE E AND M CARD TYPES WILL BE
!     ACCUMULATED INTO ONE SPCFLD-LIKE CARD WITH FIELD VALUSS AT EACH
!     POINT
!
            IF ( ifirst/=1 ) THEN
               ifirst = 1
               jcore1 = ncore + 1
               jcoren = ncore + 3*nrowsp
               IF ( jcoren>core ) CALL mesage(-8,0,subr)
!
               DO j1 = jcore1 , jcoren
                  rz(j1) = 0.
               ENDDO
            ENDIF
            DO
!
!     ALL E AND M CARDS WILL BE COMBINED INTO ONE LOGICAL CARD OF
!     TYPE=20, 3*NROWSP VALUES HCX,HCY,HCZ AT EACH POINT IN THE MODEL.
!     FOR CEMLOOP AND GEMLOOP, WE MUST PICK UP BGPDT FOREACH POINT AND
!     COMPUTE FIELD FOR EACH LOOP
! *** 10/1/80 WE MUST ALSO FIND HC AT INTEGRATION POINTS AND CENTROIDS.
!     SO ALSO COPY SLT INFO TO NEWSLT FOR USE IN EANDM
!
!
!     1ST OCCURRENCE OF A CARD TYPE. CHECK ON TYPE
!
               jtype = itype - 19
               IF ( jtype==2 .OR. jtype==3 .OR. jtype==4 ) THEN
!
!     CEMLOOP,GEMLOOP, OR MDIPOLE
!     CHECK FOR ENOUGH CORE TO READ IN BGPDT. IF NOT, READ ONE POINT AT
!     A TIME
!
                  IF ( .NOT.(bgopen) ) THEN
!
!     IF MODCOM(9) IS NOT SET TO NONZERO, THEN WE WILL NOT COMPUTE HCFLD
!     AT GRID POINTS FOR COILS, ETC.(ONLY SPCFLD) SINCE IT TAKES TIME
!     AND IS NOT NEEDED IN ANY SUBSEQUENT COMPUTATION. (ONLY SPCFLD INFO
!     IS NEEDED LATER. ALL OTHER HC INFO IS COMPUTED LATER) IF MODCOM(9)
!     IS SET TO NONZERO, HCFLD IS COMPUTED AT THE POINTS FOR ALL LOAD
!     TYPES AND CAN BE PRINTED FOR INFORMATIONAL PURPOSES IF DESIRED.
!
                     IF ( ksystm(65)/=0 ) THEN
                        CALL gopen(bgpdt,z(buf3),0)
                        mcb(1) = bgpdt
                        CALL rdtrl(mcb)
                        npts = mcb(2)
                        bgcore = .TRUE.
                        bgopen = .TRUE.
                        IF ( jcoren+4*npts>core ) bgcore = .FALSE.
                        next = jcoren + 4*npts
                        IF ( .NOT.bgcore ) next = jcoren
                        IF ( bgcore ) CALL fread(bgpdt,z(jcoren+1),4*npts,0)
                     ENDIF
                  ENDIF
                  CALL write(Newslt,buf,2,0)
!
                  DO j1 = 1 , entrys
!
!     READ CEMLOOP, GEMLOOP, OR MDIPOLE ENTRY
!
                     iwords = 12
                     IF ( itype==22 ) iwords = 48
                     IF ( itype==23 ) iwords = 9
                     CALL fread(Slt,buf,iwords,0)
                     CALL write(Newslt,buf,iwords,0)
                     IF ( ksystm(65)/=0 ) THEN
!
!
!     DO THIS LOOP FOR ALL POINTS
!
                        DO kk = 1 , npts
                           IF ( bgcore ) THEN
                              jcor = jcoren + 4*kk
                              IF ( z(jcor-3)==-1 ) CYCLE
                              xx = rz(jcor-2)
                              yy = rz(jcor-1)
                              zz = rz(jcor)
                           ELSE
                              CALL fread(bgpdt,buf,4,0)
                              IF ( buf(1)==-1 ) CYCLE
                              xx = rbuf(2)
                              yy = rbuf(3)
                              zz = rbuf(4)
                           ENDIF
                           IF ( itype==21 ) THEN
                              CALL axloop(rbuf,buf,xx,yy,zz,hc1,hc2,hc3)
                           ELSEIF ( itype==23 ) THEN
                              CALL dipole(rbuf,buf,xx,yy,zz,hc1,hc2,hc3)
                           ELSE
                              CALL geloop(rbuf,buf,xx,yy,zz,hc1,hc2,hc3)
                           ENDIF
                           isub = ncore + 3*kk - 2
                           rz(isub) = rz(isub) + hc1
                           rz(isub+1) = rz(isub+1) + hc2
                           rz(isub+2) = rz(isub+2) + hc3
!
!     GO BACK FOR ANOTHER POINT
!
                        ENDDO
                        IF ( .NOT.(bgcore) ) THEN
                           CALL rewind(bgpdt)
                           CALL fwdrec(*400,bgpdt)
                        ENDIF
                     ENDIF
!
!     GET ANOTHER LOOP OR DIPOLE
!
                  ENDDO
               ELSE
!
!     SPCFLD
!
                  buf(1) = 20
                  buf(2) = 1
                  CALL write(Newslt,buf,2,0)
                  DO j1 = 1 , entrys
!
!     READ ONE SPCFLD CARD
!
                     CALL fread(Slt,buf,5,0)
                     IF ( buf(5)/=-1 ) THEN
!
                        isub = ncore + 3*buf(5) - 2
                        rz(isub) = rz(isub) + rbuf(2)
                        rz(isub+1) = rz(isub+1) + rbuf(3)
                        rz(isub+2) = rz(isub+2) + rbuf(4)
                     ELSE
!
!     BUF(1)=CID WHICH IS ASSUMED TO BE 0 FOR NOW
!
!     ALL GRIDS GET HC
!
                        DO j2 = jcore1 , jcoren , 3
                           rz(j2) = rz(j2) + rbuf(2)
                           rz(j2+1) = rz(j2+1) + rbuf(3)
                           rz(j2+2) = rz(j2+2) + rbuf(4)
                        ENDDO
                     ENDIF
                  ENDDO
!
!     DONE WITH ALL SPCFLD CARDS IN THIS LOAD SET. CHECK FOR OTHER CARD
!     TYPES IN THIS LOAD SET
!
                  CALL write(Newslt,rz(jcore1),3*nrowsp,0)
               ENDIF
!
!     CHECK IF NEXT CARD TYPE IS 21 ,22, OR 23. CARD TYPES ON SLT ARE
!     IN INCREASING CARD TYPE). IF SO, STAY HERE. OTHERWISE, WRITE OUT
!     ALL CARD TYPES GENERATING AN SPCFLD-TYPE CARD AND GOING ONTO
!     HCFLDS MUST HAVE CONSECUTIVE TYPE NUMBERS FOR THIS SPECIAL
!     PROCESSING THE GENERATED SPCFLD AND GO BACK TO NORMAL PROCESSING
!
               CALL read(*340,*420,Slt,buf,2,noeor,iwords)
               itype = buf(1)
               entrys = buf(2)
               IF ( buf(1)<20 .OR. buf(1)>23 ) THEN
                  ieor = 0
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            GOTO 420
         ELSEIF ( itype==24 ) THEN
!
!     REMFLUX PROCESSING. CREATE A VECTOR OF ORDER 3N,N=NUMBER OF
!     ELEMENTS IN MODEL,N IS 1ST TRAILER WORD OF EST. THE VECTOR
!     CONTAINS TOTAL BX,BY,BZ FROM ALL REMFLUX CARDS FOR EACH ELEMENT
!     IN THE ORDER OF ELEMENTS ON EST
!
            CALL gopen(Est,z(buf3),0)
            mcb(1) = Est
            CALL rdtrl(mcb)
            nel = mcb(2)
            jcore1 = ncore + 1
            jcoren = ncore + 3*nel
            jcorex = jcoren + 5*entrys
            IF ( jcorex>core ) CALL mesage(-8,0,subr)
!
            nels = 0
            DO j1 = jcore1 , jcoren
               rz(j1) = 0.
            ENDDO
!
!     READ ALL REMFLUX CARDS
!
            CALL fread(Slt,rz(jcoren+1),5*entrys,0)
            GOTO 440
         ELSE
            outcnt = type(itype,2)
            iflag = type(itype,3)
!
            IF ( iflag<=0 ) THEN
!
!     NO SPECIAL PROCESSING OF THIS LOAD TYPE THUS JUST COPY IT ACROSS.
!
               CALL write(Newslt,buf,2,noeor)
               DO j = 1 , entrys
                  CALL fread(Slt,buf,incnt,0)
                  CALL write(Newslt,buf,outcnt,noeor)
               ENDDO
            ELSE
!
!     OK BRING DATA INTO CORE.
!
               jcore = ncore + 1
               type(itype,4) = jcore
               z(jcore) = itype
               z(jcore+1) = entrys
               jcore = jcore + 2
               ncore = jcore + entrys*outcnt - 1
               IF ( ncore>core ) CALL mesage(-8,0,subr)
               DO j = jcore , ncore
                  z(j) = 1
               ENDDO
               kcore = jcore
!
!     READ IN THE LOAD ENTRIES.
!
               DO j = jcore , ncore , outcnt
                  CALL fread(Slt,z(j),incnt,0)
               ENDDO
               id = 2
               IF ( itype==15 ) id = 1
               IF ( itype==16 ) id = 5
               CALL sort(0,0,outcnt,id,z(kcore),ncore-kcore+1)
               any = .TRUE.
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ALL DATA NOW IN CORE FOR THIS LOAD SET.
!
 40      IF ( .NOT.any ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THE EST IS NOW PROCESSED FOR ELEMENT TYPES CHECKED BELOW.
!
         CALL gopen(Est,z(buf3),rdrew)
 60      SPAG_Loop_1_1: DO
!
!     READ ELEMENT TYPE
!
            CALL read(*240,*360,Est,eltype,1,noeor,iwords)
            IF ( eltype==cbar ) THEN
!
!     BAR
!
               estwds = 42
               grid1 = 2
               points = 2
               iarea = 17
               itype = 1
            ELSE
               IF ( eltype/=crod ) THEN
                  IF ( eltype/=conrod ) THEN
                     IF ( eltype==ctube ) THEN
!
!     TUBE
!
                        estwds = 16
                        grid1 = 2
                        points = 2
                        iarea = 5
                        itype = 1
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        IF ( eltype==ctrmem ) EXIT SPAG_Loop_1_1
                        IF ( eltype==ctria1 ) THEN
!
!     TRIA1
!
                           estwds = 27
                           grid1 = 2
                           points = 3
                           iarea = 7
                           itype = 2
                           spag_nextblock_1 = 7
                           CYCLE SPAG_DispatchLoop_1
                        ELSE
                           IF ( eltype==ctria2 ) EXIT SPAG_Loop_1_1
                           IF ( eltype==ctria3 ) THEN
!
!     TRIA3
!
                              estwds = 39
                              grid1 = 2
                              points = 3
                              iarea = 7
                              itype = 2
                              spag_nextblock_1 = 7
                              CYCLE SPAG_DispatchLoop_1
                           ELSE
                              IF ( eltype==cqdmem ) THEN
                                 spag_nextblock_1 = 5
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( eltype==cqdmm1 ) THEN
                                 spag_nextblock_1 = 5
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( eltype==cqdmm2 ) THEN
                                 spag_nextblock_1 = 5
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( eltype==cquad1 ) THEN
!
!     QUAD1
!
                                 estwds = 32
                                 grid1 = 2
                                 points = 4
                                 iarea = 8
                                 itype = 2
                                 spag_nextblock_1 = 7
                                 CYCLE SPAG_DispatchLoop_1
                              ELSE
                                 IF ( eltype==cquad2 ) THEN
                                    spag_nextblock_1 = 5
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 IF ( eltype==cquad4 ) THEN
!
!     QUAD4
!
                                    estwds = 45
                                    grid1 = 2
                                    points = 4
                                    iarea = 8
                                    itype = 2
                                    spag_nextblock_1 = 7
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSEIF ( eltype==ctrirg ) THEN
!
!     TRIRG
!
                                    estwds = 19
                                    grid1 = 2
                                    points = 3
                                    iarea = 0
                                    itype = 3
                                    spag_nextblock_1 = 7
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSEIF ( eltype==ctrprg ) THEN
!
!     TRAPRG
!
                                    estwds = 24
                                    grid1 = 2
                                    points = 4
                                    iarea = 0
                                    itype = 3
                                    spag_nextblock_1 = 7
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSEIF ( eltype==ctetra ) THEN
!
!     TETRA
!
                                    estwds = 23
                                    grid1 = 3
                                    points = 4
                                    iarea = 0
                                    itype = 4
                                    spag_nextblock_1 = 7
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSEIF ( eltype==cwedge ) THEN
!
!     WEDGE
!
                                    estwds = 33
                                    grid1 = 3
                                    points = 6
                                    iarea = 0
                                    itype = 4
                                    spag_nextblock_1 = 7
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSE
                                    IF ( eltype==chexa1 ) THEN
                                       spag_nextblock_1 = 6
                                       CYCLE SPAG_DispatchLoop_1
                                    ENDIF
                                    IF ( eltype==chexa2 ) THEN
                                       spag_nextblock_1 = 6
                                       CYCLE SPAG_DispatchLoop_1
                                    ENDIF
                                    IF ( eltype==cihex1 ) THEN
!
!     IHEX1
!
                                       estwds = 55
                                       grid1 = 3
                                       points = 8
                                       iarea = 0
                                       itype = 4
                                       spag_nextblock_1 = 7
                                       CYCLE SPAG_DispatchLoop_1
                                    ELSEIF ( eltype==cihex2 ) THEN
!
!     IHEX2 AND IHEX3 ARE NOT IMPLEMENTED DUE TO
!        1. ECPT ARRAY TOO SMALL IN THIS ROUTINE
!        2. QVOL ROUTINE CAN NOT HANDLE SOLID ELEMENTS HAVING MORE THAN
!           8 GRID POINTS
!
!     IHEX2
!
                                       estwds = 127
                                       grid1 = 3
                                       points = 20
                                       iarea = 0
                                       itype = 4
                                       CALL fwdrec(*380,Est)
                                       CYCLE
                                    ELSEIF ( eltype==cihex3 ) THEN
!
!     IHEX3
!
                                       estwds = 199
                                       grid1 = 3
                                       points = 32
                                       iarea = 0
                                       itype = 4
                                       CALL fwdrec(*380,Est)
                                       CYCLE
                                    ELSEIF ( eltype==chbdy ) THEN
!
!     HBDY ELEMENTS OF EST FILE.  DO QBDY1, QBDY2, AND QVECT REFERENCES.
!
!
!     BUF(3) IS SET TO 0 AS A FLAG TO TELL IF HBDY HAS BEEN CALLED FOR
!     THIS ELEMENT.
!
                                       IF ( type(14,4)+type(15,4)+type(16,4)==0 ) THEN
                                         CALL fwdrec(*380,Est)
                                         CYCLE
                                       ELSE
                                         idbdy1 = type(14,4) + 3
                                         idbdy2 = type(15,4) + 2
                                         idqvec = type(16,4) + 6
                                         qbdy1s = z(idbdy1-2)
                                         qbdy2s = z(idbdy2-1)
                                         qvects = z(idqvec-5)
                                         estwds = 53
                                         GOTO 120
                                       ENDIF
                                    ELSE
                                       CALL fwdrec(*380,Est)
                                       CYCLE
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
!
!     ROD AND CONROD
!
               estwds = 17
               grid1 = 2
               points = 2
               iarea = 5
               itype = 1
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDDO SPAG_Loop_1_1
!
!     TRMEM AND TRIA2
!
         estwds = 21
         grid1 = 2
         points = 3
         iarea = 7
         itype = 2
         spag_nextblock_1 = 7
      CASE (5)
!
!     QDMEM AND QUAD2
!
         estwds = 26
         grid1 = 2
         points = 4
         iarea = 8
         itype = 2
         spag_nextblock_1 = 7
      CASE (6)
!
!     HEXA1 AND HEXA2
!
         estwds = 43
         grid1 = 3
         points = 8
         iarea = 0
         itype = 4
         spag_nextblock_1 = 7
      CASE (7)
!
!     MISC. ELEMENTS OF EST FILE.  DO QVOL REFERENCES.
!
         IF ( type(13,4)==0 ) THEN
            CALL fwdrec(*380,Est)
            GOTO 60
         ELSE
            idqvol = type(13,4) + 3
            entrys = z(idqvol-2)
            iwords = 12
            j1 = idqvol
            j2 = j1 + entrys*iwords
            idptr = 2
            ASSIGN 100 TO iretrn
         ENDIF
 80      CALL read(*380,*60,Est,ecpt,estwds,noeor,iflag)
!
!     LOOK FOR THIS ELEMENT ID AMONG QVOL DATA.
!
         CALL bisloc(*80,ecpt(1),z(idqvol),12,entrys,jpoint)
!
!     MATCH FOUND ON ID. COMPUTE ZERO POINTER TO ZERO WORD OF QVOL ENTRY
!
         index = idqvol + jpoint - 3
!
!     INTERNAL ROUTINE TO FIND THE START AND END OF ENTRYS HAVING THE
!     SAME ID IN A GIVEN CARD-TYPE SET.
!
!
!     BACK UP TO FIRST ENTRY OF THIS ID.
!
         jindex = index + idptr - iwords
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
!
!     IF COEFICIENT IS NOT AN INTEGER 1 ENTRY HAS BEEN ALTERED BEFORE.
!
 100     DO index = k1 , k2 , iwords
            IF ( z(index+11)==1 ) THEN
!
!     ALTER ENTRY IN PLACE (NOTE THE CONVERSION TABLE ABOVE)
!
!     GET AREA FACTOR FROM ECPT AND REVISE ENTRY.
!
               IF ( iarea==0 ) THEN
                  area = 1.0
               ELSE
                  area = recpt(iarea)
                  IF ( eltype==ctube ) area = piovr4*(area**2-(area-2.*recpt(6))**2)
               ENDIF
               i1 = index + 3
               i2 = index + 10
               DO j = i1 , i2
                  z(j) = 0
               ENDDO
               i2 = i1 + points - 1
               igrid = grid1
               DO j = i1 , i2
                  z(j) = ecpt(igrid)
                  igrid = igrid + 1
               ENDDO
               rz(index+11) = rz(index+1)*area
               z(index+1) = points
               z(index+12) = itype
            ELSE
               WRITE (outpt,99002) uwm , eltype , ecpt(1) , z(i+2)
99002          FORMAT (A25,' 3095, ELEMENT TYPE',I9,' WITH ID =',I9,', REFERENCED BY A QVOL CARD IN LOAD SET',I9,1H,,/5X,           &
                      &'IS NOT BEING USED FOR INTERNAL HEAT GENERATION IN THIS ',                                                   &
                      &'LOAD SET BECAUSE ANOTHER ELEMENT TYPE WITH THE SAME ID',/5X,'HAS ALREADY BEEN USED.')
            ENDIF
         ENDDO
         GOTO 80
!
!     READ AN HBDY ELEMENT ECPT FROM THE EST.
!
 120     CALL read(*380,*60,Est,ecpt,estwds,noeor,iflag)
!
!     LOOK FOR ID AMONG QBDY1 DATA
!
         IF ( type(14,4)==0 ) GOTO 160
         iwords = 10
         j1 = idbdy1
         j2 = j1 + qbdy1s*iwords
         idptr = 2
         ASSIGN 140 TO iretrn
         CALL bisloc(*160,ecpt(1),z(idbdy1),10,qbdy1s,jpoint)
!
!     MATCH FOUND.  CHECK FOR PREVIOUS REFERENCE.
!
         index = idbdy1 + jpoint - 3
         jindex = index + idptr - iwords
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 140     DO index = k1 , k2 , iwords
            IF ( z(index+10)==1 ) THEN
!
!     ALTER ENTRY FOR OUTPUT.  GET AREA FACTORS FOR HBDY ELEMENT.
!
               CALL hbdy(ecpt,ecpt,2,rbuf,buf)
               z(index+3) = buf(3)
               z(index+4) = buf(4)
               z(index+5) = buf(5)
               z(index+6) = buf(6)
               rz(index+7) = rbuf(7)*rz(index+1)
               rz(index+8) = rbuf(8)*rz(index+1)
               rz(index+9) = rbuf(9)*rz(index+1)
               rz(index+10) = rbuf(10)*rz(index+1)
               z(index+1) = ecpt(2)
            ELSE
               WRITE (outpt,99004) ufm , ecpt(1)
               nogo = .TRUE.
               GOTO 300
            ENDIF
         ENDDO
!
!     LOOK FOR ID AMONG QBDY2 DATA.
!
 160     IF ( type(15,4)==0 ) GOTO 200
         iwords = 10
         j1 = idbdy2
         j2 = j1 + qbdy2s*iwords
         idptr = 1
         ASSIGN 180 TO iretrn
         CALL bisloc(*200,ecpt(1),z(idbdy2),10,qbdy2s,jpoint)
!
!     MATCH FOUND.  CHECK FOR PREVIOUS REFERENCE.
!
         index = idbdy2 + jpoint - 2
         jindex = index + idptr - iwords
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 180     DO index = k1 , k2 , iwords
            IF ( z(index+10)==1 ) THEN
!
!     ALTER ENTRY FOR OUTPUT.  GET AREA FACTORS FOR HBDY ELEMENT.
!
               CALL hbdy(ecpt,ecpt,2,rbuf,buf)
               rz(index+7) = rbuf(7)*rz(index+2)
               rz(index+8) = rbuf(8)*rz(index+3)
               rz(index+9) = rbuf(9)*rz(index+4)
               rz(index+10) = rbuf(10)*rz(index+5)
               z(index+3) = buf(3)
               z(index+4) = buf(4)
               z(index+5) = buf(5)
               z(index+6) = buf(6)
               z(index+2) = ecpt(2)
            ELSE
               WRITE (outpt,99004) ufm , ecpt(1)
               nogo = .TRUE.
               GOTO 300
            ENDIF
         ENDDO
!
!     LOOK FOR ID AMONG QVECT DATA
!
 200     IF ( type(16,4)==0 ) GOTO 120
         iwords = 19
         j1 = idqvec
         j2 = j1 + qvects*iwords
         idptr = 5
         ASSIGN 220 TO iretrn
         CALL bisloc(*120,ecpt(1),z(idqvec),19,qvects,jpoint)
!
!     MATCH FOUND.  CHECK FOR PREVIOUS REFERENCE.
!
         index = idqvec + jpoint - 6
         jindex = index + idptr - iwords
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 220     DO index = k1 , k2 , iwords
            IF ( z(index+19)==1 ) THEN
!
!     ALTER ENTRY FOR OUTPUT.  GET AREA FACTORS FOR HBDY ELEMENT.
!
               CALL hbdy(ecpt,ecpt,3,rbuf,buf)
               rz(index+11) = rz(index+2)
               rz(index+12) = rz(index+3)
               rz(index+13) = rz(index+4)
               rz(index+14) = rbuf(11)
               rz(index+15) = rbuf(12)
               rz(index+16) = rbuf(13)
               rz(index+17) = rbuf(14)
               rz(index+18) = rbuf(15)
               rz(index+19) = rbuf(16)
               q0 = rz(index+1)
               z(index+1) = buf(3)
               z(index+2) = buf(4)
               z(index+3) = buf(5)
               z(index+4) = buf(6)
               z(index+6) = ecpt(2)
               rz(index+7) = rbuf(7)*q0
               rz(index+8) = rbuf(8)*q0
               rz(index+9) = rbuf(9)*q0
               rz(index+10) = rbuf(10)*q0
            ELSE
               WRITE (outpt,99004) ufm , ecpt(1)
               nogo = .TRUE.
               GOTO 300
            ENDIF
         ENDDO
         GOTO 120
!
!     EST HAS BEEN PASSED FOR ALL ELEMENTS.  NOW OUTPUT DATA TO NEWSLT.
!
 240     CALL close(Est,clsrew)
         DO j = 13 , 16
            jcore = type(j,4)
            IF ( jcore>0 ) THEN
               nwords = z(jcore+1)*type(j,2) + 2
!
!     INSURE THAT ALL ENTRYS WERE MODIFIED.
!     CHECK WORD 7 FOR NO INTEGER 1 IN TYPES 14,15, AND 16.
!     CHECK WORD 11 FOR NO INTEGER 1 IN TYPE 13.
!
               k = 8
               IF ( j==13 ) k = 12
               i1 = jcore + k
               i2 = i1 + nwords - 3
               outcnt = type(j,2)
               DO l = i1 , i2 , outcnt
                  IF ( z(l)==1 ) THEN
                     k = j - 12
                     IF ( k==2 ) THEN
                        id = l - 5
                     ELSEIF ( k==3 ) THEN
                        id = l - 6
                     ELSEIF ( k==4 ) THEN
                        id = l - 2
                     ELSE
                        id = l - 9
                     ENDIF
                     WRITE (outpt,99003) ufm , z(id)
99003                FORMAT (A23,' 3096, ELEMENT ID =',I9,' AS REFERENCED ON A QVOL, ','QBDY1, QBDY2, OR QVECT LOAD CARD,',/5X,     &
                            &'COULD NOT BE ','FOUND AMONG ACCEPTABLE ELEMENTS FOR THAT LOAD TYPE.')
                     nogo = .TRUE.
                  ENDIF
               ENDDO
               CALL write(Newslt,z(jcore),nwords,noeor)
            ENDIF
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
!
!     COMPLETE THIS LOAD SET RECORD ON -NEWSLT-.
!
         CALL write(Newslt,0,0,eor)
         i = i + 1
         IF ( i<=nrecs ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         DO
!
!     COPY BALANCE OF DATA ON -SLT- TO -NEWSLT- WHATEVER IT BE.
!
            CALL read(*280,*260,Slt,z,core,noeor,iwords)
            CALL write(Newslt,z,core,noeor)
         ENDDO
 260     CALL write(Newslt,z,iwords,eor)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     NEWSLT IS COMPLETE.
!
 280     CALL close(Slt,clsrew)
         CALL close(Newslt,clsrew)
 300     IF ( nogo ) CALL mesage(-61,0,subr)
         RETURN
!
!     FATAL FILE ERRORS
!
 320     CALL mesage(-1,Newslt,subr)
 340     CALL mesage(-2,Slt,subr)
 360     CALL mesage(-3,Est,subr)
 380     CALL mesage(-2,Est,subr)
 400     CALL mesage(-2,bgpdt,subr)
         RETURN
      CASE (10)
         SPAG_Loop_1_2: DO WHILE ( jindex>=j1 )
            IF ( z(jindex)/=ecpt(1) ) EXIT SPAG_Loop_1_2
            jindex = jindex - iwords
         ENDDO SPAG_Loop_1_2
         k1 = jindex + iwords - idptr
!
!     FIND LAST ENTRY OF THIS ID.
!
         jindex = k1 + iwords + idptr
         SPAG_Loop_1_3: DO WHILE ( jindex<j2 )
            IF ( z(jindex)/=ecpt(1) ) EXIT SPAG_Loop_1_3
            jindex = jindex + iwords
         ENDDO SPAG_Loop_1_3
         k2 = jindex - iwords - idptr
         GOTO iretrn
 420     ieor = 1
         spag_nextblock_1 = 11
      CASE (11)
         buf(1) = -20
         buf(2) = 1
         CALL write(Newslt,buf,2,0)
         CALL write(Newslt,rz(jcore1),3*nrowsp,0)
         IF ( bgopen ) CALL close(bgpdt,1)
         IF ( ieor==1 ) GOTO 40
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
 440     CALL read(*460,*360,Est,eltype,1,0,iwords)
         idx = (eltype-1)*incr
         estwds = ne(idx+12)
         DO
            CALL read(*380,*440,Est,elid,1,0,iwords)
            nels = nels + 1
            isub = ncore + 3*nels - 2
            CALL fread(Est,dum,-estwds+1,0)
!
!     CHECK FOR THIS ELID AMONG THE REMFLUX CARDS
!
            DO j1 = 1 , entrys
               isub1 = jcoren + 5*j1
               IF ( z(isub1)/=-1 ) THEN
                  IF ( elid/=z(isub1) ) CYCLE
               ENDIF
!
!     MATCH-STORE THIS PERM MAG
!
               rz(isub) = rz(isub) + rz(isub1-3)
               rz(isub+1) = rz(isub+1) + rz(isub1-2)
               rz(isub+2) = rz(isub+2) + rz(isub1-1)
!
!     READ ANOTHER ELEMENT ID
!
            ENDDO
         ENDDO
!
!     EST EXHAUSTED
!
 460     CALL close(Est,1)
         buf(1) = 24
         buf(2) = 1
         CALL write(Newslt,buf,2,0)
         CALL write(Newslt,rz(jcore1),3*nel,0)
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99004 FORMAT (A23,' 2362, CHBDY CARDS WITH DUPLICATE IDS FOUND IN EST,',' CHBDY ID NUMBER =',I9)
END SUBROUTINE ssgslt
