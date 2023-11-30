
SUBROUTINE scan
   IMPLICIT NONE
   REAL Amax , Amin , Core(1) , Eofnrw , Rd , Rdrew , Rew , Skp(83) , Wrt , Wrtrew
   LOGICAL Debug , Force , Iopen , Jopen , Layerd , Stress
   INTEGER E(1) , Ibeg , Ibuf , Icomp , Icompx , Idupl , Iel , Ielt(2) , Iend , Imax , Imin , Inc , Incr , Infile , Intra , Iopt ,  &
         & Iset , Isort , Itrl3 , Last , Lbeg , Lcore , Lend , Lloop , Nelem , Norew , Nout , Ntop , Oel , Osubc , Oufile , Quad4 , &
         & Subc , Tria3 , Z(166)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Ielt , Icomp , Ntop , Amax , Amin , Ibeg , Iend , Icompx
   COMMON /gpta1 / Nelem , Last , Incr , E
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /system/ Ibuf , Nout , Skp , Intra
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /xscanx/ Infile , Oufile , Lcore , Lbeg , Lend , Iopen , Jopen , Iel , Iopt , Iset , Isort , Itrl3 , Subc , Osubc , Oel , &
                 & Debug , Lloop , Quad4 , Tria3 , Stress , Force , Layerd
   COMMON /zzzzzz/ Core
   INTEGER casecc , eor , file , i , ib , ibuf1 , ibuf2 , ibuf3 , ie , ii , ioef , ioes , iol1 , iol2 , irf , j , jelt(2) , jmp ,   &
         & kk , l , l1 , le , lencc , ll , ll2 , llc , lll1 , ls , lsem , lx , nam(2) , ncase , nn , nscan , nxx , nz , oefi(2) ,   &
         & oesfi(2) , oesi(2) , scr1 , sorf
   INTEGER khrfn1 , khrfn2 , khrfn3 , korsz
   LOGICAL kopen , lopen
!
!     THIS IS THE MAIN DRIVER FOR THE OUTPUT SCAN MODULE - SCAN
!
!     THIS SCAN MODULE CAN BE CALLED DIRECTLY FROM ALL RIGID FORMATS, OR
!     BY USER DMAP ALTER. THE CALLING INSTRUCTIONS ARE
!
!     (THREE INPUT FILES IF CALLED BY RIGID FORMAT VIA SCAN INPUT CARDS)
!     (1) FORCE AND STRESS SCAN -
!     SCAN  CASECC,OESI,OEFI/OESFI/*RF*  $    (WHERE I=1, OR 2)
!           OR
!     SCAN  CASECC,OESI,OEFI/OESFI/*OLI* $    FOR ON-LINE SCAN
!
!         . IF INPUT FILES ARE OES1, OEF1, SORT1 TYPE DATA ARE SCANNED
!         . IF INPUT FILES ARE OES2, OEF2, SORT2 TYPE DATA ARE SCANNED
!
!     (ONE INPUT FILE ONLY IF CALLED BY USER VIA DMAP ALTER)
!     (2) STRESS SCAN -
!     SCAN, ,OESI, /OESFI/C,N,ELEMENT/C,N,ICOMP/C,N,NTOP/C,N,AMAX/
!           C,N,AMIN/C,N,IBEG/C,N,IEND/C,N,ICOMPX $
!     OR (3) FORCE SCAN -
!     SCAN, ,,OEFI /OESFI/C,N,ELEMENT/C,N,ICOMP/C,N,NTOP/C,N,AMAX/
!           C,N,AMIN/C,N,IBEG/C,N,IEND/C,N,ICOMPX $
!
!         . FOR SORT1 TYPE DATA, OESI AND OEFI ARE OES1 AND OEF1, AND
!           IBEG AND IEND ARE RANGE OF ELEMENT IDS TO BE SCANNED
!         . FOR SORT2 TYPE DATA, OESI AND OEFI ARE OES2 AND OEF2, AND
!           IBEG AND IEND ARE RANGE OF SUBCASE IDS TO BE SCANNED
!         . IF IBEG AND IEND ARE NOT GIVEN, ALL IDS IMPLY
!
!         . OESB1, OESC1, OEFB1, AND OEFC1 CAN BE USED IN LIEU OF OES1
!           AND OEF1. SIMILARLY, OESC2 AND OEFC2  FOR OES2 AND OEF2
!
!     INPUT  FILES  - CASECC, OES1, OEF1, (OR OES2, OEF2)
!                     (OESB1, OESC1, OEFB1, OEFC1, OESB2, OEFB2 CAN BE
!                     USED INSTEAD)
!     OUTPUT FILE   - OESF1 (OR OESF2)
!     SCRATCH FILE  - SCR1
!
!     THIS SCAN MODULE SHOULD BE FOLLOWED BY OFP TO PRINT SCAN RESULTS
!     OFP  OESFI,,,,, //S,N,CARDNO $
!
!     PARAMETERS -
!
!           ELEMENT - ELEMENT NAME IN BCD.  E.G. BAR, CBAR, QUAD2, ETC.
!           ICOMP   - THE OUTPUT FIELD NO. (BY COLUMN, 1 THRU 31) OF
!                     OUTPUT LISTING.
!           ICOMPX  - OUTPUT FIELD NO. CONTINUATION (FROM 32 THRU 62)
!           NTOP    - TOP N VALUES TO BE OUTPUT.  DEFAULT=20
!      AMAX-AMIN    - SCAN VALUES OUTSIDE THIS MAX-MIN RANGE, DEFAULT=0.
!      IBEG,IEND    - SEE EXPLANATION ABOVE
!
!     DEFINITION OF SOME LOCAL VARIABLES
!
!           DEBUG   - USED FOR LOCAL DEBUG
!           S OR F  - STRESS OR FORCE SCAN FLAG
!           NSCAN   - NO. OF SCAN INPUT CARDS IN CASECC
!           SUBC    - CURRENT SUBCASE ID
!           NZ      - TOP OF OPEN CORE, JUST BELOW GINO BUFFERS
!           LCORE   - AVAILABLE CORE FOR STRSCN ROUTINE
!           IOPEN   - INPUT  FILE STATUS FLAG, .T. FOR OPEN, .F. NOT
!           JOPEN   - OUTPUT FILE STATUS FLAG, .T. FOR OPEN, .F. NOT
!           KOPEN   - SCR1   FILE STATUS FLAG, .T. FOR OPEN, .F. NOT
!           LOPEN   - CASECC FILE STATUS FLAG, .T. FOR OPEN, .F. NOT
!           ISET    - SCAN ONLY BY THE SPECIFIED SET OF ELEM. IDS
!                   - ALL IS IMPLIED IF ISET IS NOT GIVEN
!                   - USED ONLY IF SCAN IS CALLED FROM RIGID FORMAT
!      IDUPL,INC    - SET UP COMPONENT FIELDS TO BE REPEATEDLY SCANNED
!                     IDUPL TIMES, WITH FIELD INCREMENT BY INC (RF ONLY)
!      LBEG,LEND    - A LIST OF TO-BE-SCANNED ELEMENT IDS, STORED IN
!                     Z(LBEG) THRU Z(LEND).
!                   - NO SUCH LIST EXISTS IF LBEG.GT.LEND OR LBEG=LEND=0
!           IOPT    - DATA SCAN BY AMAX AND AMIN IF IOPT=1, BY NTOP IF 2
!           ISORT   - SET TO 1 (BY STRSCN) IF DATA TYPE IS IN SORT1
!                     FORMAT, AND SET TO 2 IF SORT2
!
!     WRITTEN BY G.CHAN/SPERRY      OCTOBER 1984
!
!     THIS ROUTINE OPENS AND CLOSES ALL INPUT AND OUTPUT FILES.
!     IT SETS UP THE SCANNING PARAMETERS AND CALL STRSCN TO SCAN THE
!     OUTPUT STRESS OR FORCE DATA
!
!     THE SCAN INPUT CARDS OPERATE VERY SIMILARY TO THE ELEMENT STRESS
!     OR FORCE CARDS. THEY CAN BE PLACED ABOVE ALL SUBCASES, OR INSIDE
!     ANY SUBCASE LEVEL, OR BOTH
!     HOWEVER, UNLIKE THE STRESS OR FORCE CARDS, MULTI-SCAN CARDS ARE
!     ALLOWED, AND THEY DO NOT EXCLUDE ONE ANOTHER.
!
!     MODIFIED IN 10/1989, TO ALLOW SETS TO BE DEFINED BEFORE OR AFTER
!     SCAN CARDS IN CASE CONTROL SECTION
!     (CURRENTLY, THIS MODIFICATION IS OK, BUT IFP1/IFP1H DO NOT ALLOW
!     SET TO BE DEFINED AFTER SCAN. IN FACT, IFP1 DOES NOT ALLOW SET TO
!     BE DEFINED AFTER ANY GUY WHO USES THE SET)
!
!WKBI  1/4/94 SPR93010 & 93011
!WKBI  1/4/94 SPR93010 & 93011
!RLBR 12/29/93 SPR 93010 & 93011
!     INTEGER         CASECC,   OESI,     OEFI,     OESFI,    SCR1,
!RLBNB 12/29/93 SPR 93010 & 93011
!RLBNE 12/29/93 SPR 93010 & 93011
 
!WKBR 1/4/94 SPR93010 & 93011     3                DEBUG
   !>>>>EQUIVALENCE (Imax,Amax) , (Imin,Amin) , (Idupl,Ibeg) , (Inc,Iend) , (Core(1),Z(1))
!RLBDB 12/29/93 SPR 93010 & 93011
!     DATA            CASECC,   OESI,     OEFI,     OESFI,    SCR1    /
!    1                101,      102,      103,      201,      301     /
!RLBDE 12/29/93 SPR 93010 & 93011
!RLBNB 12/29/93 SPR 93010 & 93011
   DATA casecc , oesi(1) , oefi(1) , oesi(2) , oefi(2) , oesfi(1) , oesfi(2) , scr1/101 , 102 , 103 , 104 , 105 , 201 , 202 , 301/
!RLBNE 12/29/93 SPR 93010 & 93011
   DATA nam , llc , eor , irf/4HSCAN , 4H     , 4HC    , 1 , 4HRF  /
   DATA iol1 , iol2/4HOL1  , 4HOL2 /
!
   Debug = .FALSE.
!WKBNB 1/4/94 SPR93011 & 93010
   Quad4 = 0
   Tria3 = 0
!
!     ALLOCATE OPEN CORE
!
!RLBNB 12/29/93 SPR 93010 & 93011
   Lloop = 1
   jelt(1) = Ielt(1)
   jelt(2) = Ielt(2)
!RLBNB 12/29/93 SPR 93010 & 93011
 100  nz = korsz(Z)
   ibuf1 = nz - Ibuf + 1
   ibuf2 = ibuf1 - Ibuf
   ibuf3 = ibuf2 - Ibuf
   nz = ibuf3 - 1
   Lcore = ibuf2 - 1
   Iopen = .FALSE.
   Jopen = .FALSE.
   kopen = .FALSE.
   lopen = .FALSE.
!
!     OPEN CASECC AND CHECK SCAN DATA
!
   Iset = 0
   IF ( Ielt(1)/=irf ) Iset = -2
   IF ( Ielt(1)==iol1 .OR. Ielt(1)==iol2 ) Iset = -3
   IF ( Iset/=-2 ) THEN
      file = casecc
      CALL open(*1100,casecc,Z(ibuf1),Rdrew)
      lopen = .TRUE.
      CALL fwdrec(*1200,casecc)
      IF ( Iset/=-3 ) THEN
         DO
            CALL read(*300,*300,casecc,Z(1),200,1,l)
            lencc = Z(166)
            nscan = Z(lencc-1)
            IF ( nscan/=0 ) EXIT
         ENDDO
      ENDIF
   ENDIF
!
!     CHECK THE PRESENCE OF STRESS AND/OR FORCE FILE.
!     QUIT IF BOTH ARE PURGED
!
   ioes = 1
   ioef = 1
!RLBDB 12/29/93 SPR 93010 & 93011
!     Z( 1) = OESI
!     Z(11) = OEFI
!RLBDE 12/29/93 SPR 93010 & 93011
!RLBNB 12/29/93 SPR 93010 & 93011
   Z(1) = oesi(Lloop)
   Z(11) = oefi(Lloop)
!RLBNE 12/29/93 SPR 93010 & 93011
   CALL rdtrl(Z(1))
   CALL rdtrl(Z(11))
   IF ( Z(1)<0 ) ioes = 0
   IF ( Z(11)<0 ) ioef = 0
   IF ( ioes+ioef==0 .AND. Iset/=-3 ) GOTO 1000
!
!     OPEN OUTPUT FILE OESFI
!
!RLBDB 12/29/93 SPR 93010 & 93011
!     FILE = OESFI
!     OUFILE = OESFI
!     CALL FNAME (OESFI,Z)
!     CALL OPEN  (*310,OESFI,Z(IBUF2),WRTREW)
!     CALL WRITE (OESFI,Z,2,EOR)
!RLBDE 12/29/93 SPR 93010 & 93011
!RLBNB 12/29/93 SPR 93010 & 93011
   file = oesfi(Lloop)
   Oufile = oesfi(Lloop)
   CALL fname(Oufile,Z)
   CALL open(*1100,Oufile,Z(ibuf2),Wrtrew)
   CALL write(Oufile,Z,2,eor)
!RLBNE 12/29/93 SPR 93010 & 93011
   Jopen = .TRUE.
   Itrl3 = 0
   lx = -1
   IF ( Ielt(1)==iol2 ) lx = -2
   IF ( Iset==-3 ) CALL onlins(*900,lx)
   IF ( Iset/=-2 ) THEN
!
!
!     SCAN IS CALLED BY RIGID FORMAT (ISET .GE. -1)
!     OR CALLED BY INTERACTIVE MODE  (ISET .EQ. -3)
!     =============================================
!
      ls = nz
!
!     OPEN SCR1 FILE, SEPERATE SCAN DATA FROM SET DATA IN CASECC, AND
!     SAVE THE COMPLETE SCAN DATA IN SCR1 FILE.
!
      file = scr1
      CALL open(*1100,scr1,Z(ibuf3),Wrtrew)
      kopen = .TRUE.
      nscan = 0
      ncase = 0
      nxx = nz
      IF ( Intra<=0 ) GOTO 400
      nxx = 198
      l = lx
      IF ( lx<=0 ) GOTO 400
      GOTO 600
   ELSE
!
!     SCAN CALLED BY USER VIA DMAP ALTER (ISET=-2)
!     ============================================
!
      ls = Lcore
      Lbeg = 0
      Lend = 0
!
!     CHECK USER DMAP ERROR, SET IOPT FLAG, AND INITIALIZE ISCAN ARRAY
!     FOR COMPONENT SPECIFIED.
!
      IF ( ioes+ioef>1 ) THEN
!
!     ERROR MESSAGES
!
         WRITE (Nout,99001)
!
99001    FORMAT (//5X,48HONLY ONE INPUT FILE ALLOWED FROM SCAN DMAP ALTER)
      ELSEIF ( Amin>Amax ) THEN
         WRITE (Nout,99002)
99002    FORMAT (//5X,21HAMAX-AMIN RANGE ERROR)
      ELSEIF ( Icomp<=1 ) THEN
         WRITE (Nout,99003)
99003    FORMAT (//5X,35HFIELD COMPONENT SPECIFICATION ERROR)
      ELSEIF ( (Amax==0. .AND. Amin==0.) .AND. Ntop==0 ) THEN
         WRITE (Nout,99004)
99004    FORMAT (//5X,30HNO AMAX-AMIN OR NTOP SPECIFIED)
      ELSEIF ( (Amax/=0. .OR. Amin/=0.) .AND. Ntop/=0 ) THEN
         WRITE (Nout,99005)
99005    FORMAT (//5X,46HSPECIFY EITHER AMAX-AMIN OR NTOP, BUT NOT BOTH,/5X,21H(NTOP=20  BY DEFAULT))
      ELSEIF ( (Ibeg==0 .AND. Iend/=0) .OR. Ibeg>Iend .OR. (Ibeg/=0 .AND. Iend==0) ) THEN
         WRITE (Nout,99006) Sfm , Ielt , Ibeg , Iend
99006    FORMAT (A25,' - SCANNING ',2A4,' ELEMENT. IBEG-IEND OUT OF RANGE','.  SCAN ABORTED')
      ELSE
         IF ( Ibeg==0 .AND. Iend==0 ) Ibeg = -1
         Iopt = 1
         IF ( Ntop>0 ) Iopt = 2
!
!     DETERMINE ELEMENT TYPE, DROP THE FIRST LETTER C IF NECESSARY
!
         Z(1) = irf
         Z(2) = irf
         IF ( khrfn2(Ielt(1),1,1)==llc ) THEN
            Z(1) = khrfn3(nam(2),Ielt(1),1,1)
            Z(1) = khrfn1(Z(1),4,Ielt(2),1)
            Z(2) = khrfn3(nam(2),Ielt(2),1,1)
         ENDIF
         DO i = 1 , Last , Incr
            IF ( Ielt(1)==E(i) .AND. Ielt(2)==E(i+1) ) GOTO 200
            IF ( Z(1)==E(i) .AND. Z(2)==E(i+1) ) GOTO 200
         ENDDO
         WRITE (Nout,99007) Ielt
99007    FORMAT (//5X,22HELEMENT MIS-SPELLED - ,2A4)
      ENDIF
      GOTO 1700
   ENDIF
 200  Iel = E(i+2)
!
!     SPECIAL HANDLING OF THE QUAD4 AND TRIA3 ELEMENT, STRESS ONLY
!     (THE 2ND, 3RD, 9TH, AND 13TH WORDS IN OES1/OES1L FILES ARE
!     NOT PRINTED. THE 9TH AND 13TH WORDS MAY BE BLANKS OR ASTERISKS)
!
   IF ( (Iel/=64 .AND. Iel/=83) .OR. ioes==0 ) THEN
   ENDIF
!WKBD 1/3/94 SPR93011 & 93011      ICOMP = ICOMP + 2
!WKBD 1/3/94 SPR93010 & 93011      IF (ICOMP .GT. 8) ICOMP = ICOMP + 1
!
!     OPEN INPUT FILE
!
!RLBDB 12/29/93 SPR 93010 & 93011
!75   INFILE = OESI
!     IF (IOES .EQ. 0) INFILE = OEFI
!RLBDE 12/29/93 SPR 93010 & 93011
!RLBNB 12/29/93 SPR 93010 & 93011
   Infile = oesi(Lloop)
   Stress = .TRUE.
   Force = .FALSE.
   IF ( ioes==0 ) THEN
      Stress = .FALSE.
      Force = .TRUE.
      Infile = oefi(Lloop)
   ENDIF
!RLBNE 12/29/93 SPR 93010 & 93011
   file = Infile
   CALL open(*200,Infile,Z(ibuf1),Rdrew)
   Iopen = .TRUE.
!
! ... NEXT I/O OPERATION ON INFILE WILL BE IN SUBROUTINE STRSCN
!
!     ALL SET TO GO
!
   j = 1
   IF ( ioes==0 ) j = 2
   CALL strscn(j)
   GOTO 900
!
 300  CALL close(casecc,Rew)
   lopen = .FALSE.
   RETURN
 400  file = casecc
   CALL rewind(casecc)
   CALL fwdrec(*1200,casecc)
!
!     READ CASECC AND PROCESS ALL SUBCASES
!
 500  CALL read(*800,*600,casecc,Z(1),nxx,1,l)
   IF ( nxx>=200 ) GOTO 1400
 600  ncase = ncase + 1
   lencc = Z(166)
   nscan = Z(lencc-1)
   lsem = Z(lencc)
   Subc = Z(1)
!
!     PICK UP ALL THE SET ID'S AND THEIR LOCATIONS IN Z ARRAY, Z(L1)
!     THRU Z(LL). SORT, AND CHECK DUPLICATE
!
   jmp = 0
   ii = lencc + lsem
   l1 = l + 1
   ll = l
   DO
      ii = ii + jmp
      IF ( ii>=l ) THEN
         lll1 = ll - l1 + 1
         ll2 = lll1/2
         IF ( Debug ) WRITE (Nout,99008) (Z(i),i=l1,ll)
99008    FORMAT (' ...SET/@125',/,(10X,I8,' @',I6))
!
         jmp = 0
         ii = lencc + lsem
         kk = nz
         IF ( ll2>1 ) THEN
            CALL sort(0,0,2,1,Z(l1),lll1)
            j = l1 + 2
            DO i = j , ll , 2
               IF ( Z(i)==Z(i-2) ) WRITE (Nout,99009) Uwm , Z(i)
99009          FORMAT (A25,' FROM SCAN, DUPLICATE SET',I9)
            ENDDO
         ENDIF
         EXIT
      ELSE
         jmp = Z(ii+2) + 2
         IF ( Z(ii+1)<10000000 .OR. jmp/=8 ) THEN
            Z(ll+1) = Z(ii+1)
            Z(ll+2) = ii
            ll = ll + 2
         ENDIF
      ENDIF
   ENDDO
 700  DO
!
!     PROCESS THE SCAN CARDS
!
!     PICK UP SCAN 8 WORD ARRAY, AND PICK UP SET DATA
!     WRITE TO SCR1 A RECORD (OF EACH SUBCASE) OF THE SCAN INPUT DATA
!     IN REVERSE ORDER (FIRST SCAN CARD LAST, AS SET UP BY CASECC)
!
      ii = ii + jmp
      IF ( ii>=l ) THEN
!
!     AT THE END OF EACH SUBCASE, WE COMPUTE THE TOTAL LENGTH OF THIS
!     SCAN DATA ARRAY, AND WRITE THE ARRAY OUT TO SCR1.  ONE RECORD PER
!     SUBCASE
!
         kk = kk - 2
         IF ( kk<ll ) THEN
!
            CALL mesage(8,0,nam)
            GOTO 99999
         ELSE
            ie = nz - kk
            Z(kk+1) = Subc
            Z(kk+2) = ie - 2
            CALL write(scr1,Z(kk+1),ie,1)
            l = kk + 1
            nn = 200
            IF ( Debug ) WRITE (Nout,99016) nn , (Z(j),j=l,nz)
            IF ( Intra>0 .AND. lx>=200 ) EXIT
            GOTO 500
         ENDIF
      ELSE
         jmp = Z(ii+2) + 2
         IF ( Z(ii+1)>=10000000 .AND. jmp==8 ) THEN
            ie = 0
            Iset = Z(ii+4)
            IF ( Iset/=-1 ) THEN
               IF ( lll1<=0 ) GOTO 1500
               CALL bisloc(*1500,Iset,Z(l1),2,ll2,i)
               ib = Z(i+l1) + 2
               ie = Z(ib)
               IF ( Debug ) WRITE (Nout,99010) Iset , i , ib , ie
99010          FORMAT (' @145, SET',I8,' FOUND.  I,IB,IE =',3I6)
               kk = kk - ie
               DO i = 1 , ie
                  Z(kk+i) = Z(ib+i)
               ENDDO
            ENDIF
            kk = kk - 9
            DO i = 1 , 8
               Z(kk+i) = Z(ii+i)
            ENDDO
            Z(kk+9) = 0
            Idupl = Z(kk+8)
            IF ( Idupl/=0 ) THEN
!WKBD 1/3/94 SPR93010 & 93011      INC = IDUPL/100
!WKBD 1/3/94 SPR93010 & 93011      Z(KK+8) = MOD(IDUPL,100)
!WKBNB 1/3/94 SPR93010 & 93011
               Inc = mod(Idupl,100)
               Z(kk+8) = Idupl/100
!WKBNE 1/3/94 SPR93010 & 93011
               Z(kk+9) = Inc
            ENDIF
            Z(kk+2) = Z(kk+2) + 1 + ie
!
!     HERE AT THE TAIL END OF OPEN CORE, WE ACCUMULATE ANOTHER RECORD
!     OF A SCAN DATA SET
!        WORD 1,  10000000 FOR STRESS, OR 20000000 FOR FORCE
!             2,  NO. OF WORDS OF THIS DATA SET (SCAN + SET)
!                 (FIRST 2 WORDS NOT INCLUDED)
!             3,  ELEMENT TYPE NUMERIC CODE
!             4,  SET-ID, OR -1
!             5,  COMPONENT CODE, ICOMP
!             6,  NTOP, OR AMAX
!             7,  -1,   OR AMIN
!             8,  COMPONENT - DUPLICATION, OR ZERO
!             9,  COMPONENT - INCREMENT,   OR ZERO
!        10-END,  SET DATA
!     REPEAT FOR ANOTHER SCAN CARD
!
!
!     SPECIAL HANDLING OF THE QUAD4 AND TRIA3 ELEMENT, STRESS ONLY
!     (THE 2ND, 3RD, 9TH,  AND 13TH WORDS IN OES1/OES1L FILES ARE
!     NOT PRINTED. THE 9TH AND 13TH WORDS MAY BE BLANKS OR ASTERISKS)
!WKBI 12/93 SPR93010 & 93011
!     ABOVE IS TRUE ONLY FOR LAMINATED QUAD4 AND TRIA3)
!
!WKBD 12/31/93 SPR93010 & 93011
!     IF ((Z(KK+3).NE.64 .AND. Z(KK+3).NE.83) .OR. Z(KK+1).NE.10000000)
            IF ( (Z(kk+3)/=64 .AND. Z(kk+3)/=83) .OR. Z(kk+8)==0 ) THEN
!WKBDB 1/3/94 SPR93010 & 93011
!      Z(KK+5) = Z(KK+5) + 2
!      IF (Z(KK+5) .GT. 8) Z(KK+5) = Z(KK+5) + 1
!      IF (Z(KK+9) .NE. 0) Z(KK+9) = Z(KK+9) + 2
!WKBDE 1/3/94 SPR93010 & 93011
            ENDIF
         ENDIF
      ENDIF
   ENDDO
!
!     THUS, END OF THE PREPARATION PHASE.  CLOSE CASECC AND SCR1
!
 800  CALL close(casecc,Rew)
   CALL close(scr1,Rew)
   kopen = .FALSE.
   lopen = .FALSE.
!
!     NOW, SET UP 2 LOOPS FOR STRESS (10000000) AND FORCE (20000000)
!     OUTPUT SCAN
!
   sorf = 30000000
   DO
      sorf = sorf - 10000000
      IF ( Debug ) WRITE (Nout,99011) sorf
99011 FORMAT (///,18H PROCESSING SERIES,I15/1X,8(4H====),/)
      IF ( Iopen ) CALL close(Infile,Rew)
      Iopen = .FALSE.
      IF ( sorf/=10000000 .OR. ioes/=0 ) THEN
         IF ( sorf/=20000000 .OR. ioef/=0 ) THEN
            IF ( sorf<=0 ) EXIT
!
!     OPEN INPUT FILES
!
!RLBDB 12/29/93 SPR 93010 & 93011
!     INFILE = OESI
!     IF (SORF .GE. 20000000) INFILE=OEFI
!RLBDE 12/29/93 SPR 93010 & 93011
!RLBNB 12/29/93 SPR 93010 & 93011
            Infile = oesi(Lloop)
            Stress = .TRUE.
            Force = .FALSE.
            IF ( sorf>=20000000 ) THEN
               Stress = .FALSE.
               Force = .TRUE.
               Infile = oefi(Lloop)
            ENDIF
!RLBNE 12/29/93 SPR 93010 & 93011
            file = Infile
            CALL open(*1100,Infile,Z(ibuf1),Rdrew)
            Iopen = .TRUE.
! ... NEXT I/O OPERATION ON INFILE WILL BE IN SUBROUTINE STRSCN
!
!     NOW, LOAD THE SCAN DATA PREVIOUSLY SAVED IN SCR1, TO THE TAIL END
!     OF THE OPEN CORE.
!     ONE OR MORE SCAN CARDS MAY BE PRESENT IN  ONE SUBCASE
!     SET UP POINTERS IN FRONT OF THE SCAN DATA, SO THAT FIRST SCAN
!     INPUT CARD WILL BE PROCESS FIRST, SECOND CARD SECOND, ETC.
!     NOTE - USE SUBCASE 1 SCAN DATA IF OUTPUT IS SORT 2 TYPE
!            (IF SUBCASE 1 DOES NOT HAVE SCAN DATA, USE NEXT SUBCASE)
!
            file = scr1
            IF ( .NOT.kopen ) CALL open(*1100,scr1,Z(ibuf3),Rdrew)
            IF ( kopen ) CALL rewind(scr1)
            kopen = .TRUE.
            Isort = 0
            Osubc = 0
            Oel = 0
!
            DO ii = 1 , ncase
               IF ( Isort==2 ) EXIT
               CALL read(*1200,*1300,scr1,Z(1),2,0,l)
               j = Z(2)
               IF ( j==0 ) THEN
                  CALL fwdrec(*1200,scr1)
               ELSE
                  Subc = Z(1)
                  ls = nz - j
                  CALL read(*1200,*1300,scr1,Z(ls+1),j,1,l)
                  le = ls
                  i = ls
                  DO
                     Z(ls) = i
                     ls = ls - 1
                     i = i + Z(i+2) + 2
                     IF ( i>=nz ) THEN
                        Lcore = ls
                        j = ls + 1
                        kk = 230
                        IF ( Debug ) WRITE (Nout,99016) kk , Subc , (Z(i),i=j,nz)
!
!     NOW IS THE TIME TO SET THE SCAN PARAMETERS FOR EACH SCAN CARD
!     WITHIN A SUBCASE, AND CALL STRSCN TO SCAN THE OUTPUT DATA
!
                        i = ls
                        DO
                           i = i + 1
                           IF ( i>le ) GOTO 810
                           ib = Z(i)
                           IF ( Z(ib+1)==sorf ) THEN
                              jmp = Z(ib+2)
                              Iel = Z(ib+3)
! ONLY QUAD4 (=64) AND TRIA3 (=83) ARE VALID FOR LLOOP=2
                              IF ( Lloop/=2 .OR. Iel==64 .OR. Iel==83 ) THEN
                                 Iset = Z(ib+4)
                                 Icomp = Z(ib+5)
                                 Ntop = Z(ib+6)
                                 Imax = Z(ib+6)
                                 Imin = Z(ib+7)
                                 Idupl = Z(ib+8)
                                 Inc = Z(ib+9)
                                 Iopt = 1
                                 IF ( Imin==-1 ) Iopt = 2
                                 IF ( Iopt/=2 ) Ntop = 0
                                 Lbeg = Lcore
                                 Lend = Lcore - 1
                                 IF ( Iset/=-1 ) THEN
                                    Lbeg = ib + 10
                                    Lend = ib + jmp + 2
                                 ENDIF
                                 j = (Iel-1)*Incr
                                 Ielt(1) = E(j+1)
                                 Ielt(2) = E(j+2)
                                 IF ( Debug ) WRITE (Nout,99012) Ielt , (Z(ib+j),j=3,9) , Iopt , Lbeg , Lend , ii , Subc
99012                            FORMAT (/5X,16HDEBUG/SCAN255 - ,2A4,/5X,12I9)
                                 CALL strscn(sorf/10000000)
                                 IF ( Iopt<0 ) GOTO 1600
                              ENDIF
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
!
!     GO BACK TO PROCESS NEXT INPUT FILE
!
 810        ENDDO
         ENDIF
      ENDIF
   ENDDO
!
!     ALL SCAN DONE.  WRITE OUTPUT FILE TRAILERS AND CLOSE ALL FILES
!
 900  IF ( Itrl3>0 ) THEN
!RLBR 12/29/93 SPR 93010 & 93011
!     Z(1) = OESFI
      Z(1) = oesfi(Lloop)
      Z(2) = 1
      Z(3) = Itrl3
      DO i = 4 , 7
         Z(i) = 0
      ENDDO
      CALL wrttrl(Z(1))
   ENDIF
!
 1000 IF ( Iopen ) CALL close(Infile,Rew)
   IF ( Jopen ) CALL close(Oufile,Rew)
   IF ( kopen ) CALL close(scr1,Rew)
   IF ( lopen ) CALL close(casecc,Rew)
!RLBNE 12/29/93 SPR 93010 & 93011
   IF ( Lloop==2 ) THEN
      IF ( Quad4==-1 ) WRITE (Nout,99017) 'QUAD4'
      IF ( Tria3==-1 ) WRITE (Nout,99017) 'TRIA3'
!RLBNE 12/29/93 SPR 93010 & 93011
      RETURN
   ELSE
      Lloop = 2
      Ielt(1) = jelt(1)
      Ielt(2) = jelt(2)
      GOTO 100
   ENDIF
!
!     FILE ERRORS
!
 1100 j = -1
   CALL mesage(j,file,nam)
   GOTO 1400
 1200 j = -2
   CALL mesage(j,file,nam)
   GOTO 1400
 1300 j = -3
   CALL mesage(j,file,nam)
 1400 DO
      j = -8
      CALL mesage(j,file,nam)
   ENDDO
 1500 WRITE (Nout,99013) Uwm , Iset
99013 FORMAT (A25,' FROM SCAN, SET',I9,' NOT FOUND')
   GOTO 700
 1600 WRITE (Nout,99014) Iopt
99014 FORMAT (//5X,44HUSER ERROR.  ILLEGAL INPUT FILE SENT TO SCAN,I6)
 1700 WRITE (Nout,99015) Swm
99015 FORMAT (A27,' FROM SCAN.  CASE ABORTED ***')
   GOTO 900
99016 FORMAT (/,11H SCAN/DEBUG,I3,(/2X,13I9))
99017 FORMAT (//' SCAN MODULE DID NOT FIND ELEMENT ',A5,' IN USER OUTPUT REQUESTS.',/,                                              &
             &' POSSIBLY WRONG COMPONENT SPECIFIED FOR LAYERED OR ','NON-LAYERED CASE',//)
99999 RETURN
END SUBROUTINE scan