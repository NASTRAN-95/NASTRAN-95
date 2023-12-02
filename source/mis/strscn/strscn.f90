!*==strscn.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strscn(Sorf)
   USE c_blank
   USE c_names
   USE c_output
   USE c_system
   USE c_xscanx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Sorf
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: any
   REAL :: bmax , bmin , zk
   CHARACTER(100) :: chead
   INTEGER , SAVE :: eor , iblank , noeor
   CHARACTER(12) :: field
   INTEGER :: i , icase , idelm , idupl , iend , ih1 , ih2 , ii , il1 , il2 , inc , istr , iwds , izn , izn1 , j , jdupl , jj ,     &
            & jnc , k , kk , lbuf0 , lbuf1 , lbuf2 , lbuf3 , len , mm , nn , nrew , ns , nscan , ntop , numfld , nwds , nwds1
   INTEGER , DIMENSION(50) :: id
   INTEGER , DIMENSION(25) :: ihead
   INTEGER , DIMENSION(10) :: iscan
   INTEGER , DIMENSION(2) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   CHARACTER(12) , DIMENSION(6) :: scnfld
   INTEGER , DIMENSION(3) , SAVE :: sortx
   REAL , SAVE :: t24
   EXTERNAL bckrec , fname , fornam , fwdrec , mesage , read , rewind , sort , sortf , strnam , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE PERFORMS STRESS AND FORCE OUTPUT SCAN.
!
!     ACKNOWLEDGEMENT -
!
!     THIS ROUTINE WAS WRITTEN ORIGINALLY BY LOCKHEED/GEORGIA FOR USER
!     DMAP-ALTER APPLICATION. IT WAS GREATLY MODIFIED BY G.CHAN/SPERRY
!     SO THAT USERS CAN SPECIFY THE OUTPUT SCAN PARAMETERS FROM THE
!     CASE CONTROL SECTION VIA THE SCAN INPUT CARD(S).  ONLY A VERY
!     SMALL PORTION OF THE ORIGINAL PROGRAM REMAINS.  THE DMAP-ALTER
!     APPLICATION IS STILL AVAILABLE TO THE USER
!
!     THIS ROUTINE IS CALLED ONLY BY SCAN
!     IT DOES NOT OPEN NOR CLOSE ANY FILE
!
!     SCAN PARAMETER -
!
!          S OR F  - STRESS (1) OR FORCE (2) SCAN FLAG
!          INFILE  - INPUT FILE, EITHER STRESS OR FORCE OUTPUT FILE
!          OUFILE  - OUTPUT FILE FROM SCAN OPERATION, TO BE PRINTED
!                    AGAIN BY OFP
!          IOPT    - OPTION 1, SCAN BY AMAX-AMIN (.GT.AMAX AND .LT.AMIN)
!                    OPTION 2, SCAN BY NTOP-
!                  . TOP N LARGEST (TENSION) AND SMALLEST (COMPRESSION)
!                    IN STRESS SCAN.
!                  . TOP N LARGEST ONLY IF NO COMPRESSION STRESS PRESENT
!                  . TOP N SMALLEST ONLY IF NO TENSION STRESS PRESENT
!                  . TOP N VALUES SCAN FOR FORCES IF TOP N IS POSITIVE
!                  . LEAST N VALUES SCAN FOR FORCES OR MARGIN (STRESS)
!                    IF TOP N IS NEGATIVE
!                  - IOPT IS INITIALIZED IN SCAN
!                  - STRSCN WILL SET IOPT TO A NEGATIVE NUMBER IF INPUT
!                    FILE IS NOT A STRESS OR FORCE FILE
!          ISET    - A LIST OF ELEMENT IDS TO BE SCANNED
!          IEL     - ELEMENT TYPE (CODE) TO BE SCANNED
!          IELT    - ELEMENT NAME IN 2 BCD WORDS
!          ICASE   - USED LOCALLY FOR SUBCASE NUMBER.
!          SUBC    - CURRENT SUBCASE NO. USED IN SCAN AND STRSCN
!          OSUBC   - SUBCASE NO. PROCESSED PREVIOUSLY
!          ISORT   - SET LOCALLY TO 1 IF INPUT FILE DATA IS IN SORT1
!                    TYPE FORMAT, TO 2 IF IN SORT2
!          DEBUG   - LOCAL DEBUG FLAG, SET BY SCAN
!          OEL     - ELEMENT TYPE PROCESSED PREVIOUSLY
!
!     SEE SUBROUTINE SCAN FOR MORE PARAMETER DEFINITIONS
!
! *** IF SCAN IS CALLED BY USER VIA DMAP ALTER, WE HAVE
!
!          ISET      =-2
!          LBEG=LEND = 0, NOT USED
!          LCSE1 AND = BEGINNING AND ENDING POINTERS TO AN ELEM. LIST
!          LCSE2       (SORT1, ALL SUBCASES), OR A SUBCASE LIST (SORT2,
!                      ALL ELEMS) IF THEY ARE GIVEN. OTHERWISE, LCSE1=-1
!                      AND LCSE2=0
!
! *** IF SCAN IS CALLED BY RIGID FORMAT, WE HAVE
!
!          ISET      =-1  IMPLIES THAT ALL ELEMENTS ARE TE BE SCANNED,
!                         AND LBEG .GT. LEND
!          ISET      = N  IMPLIES THAT ELEM. ID SET N IS REQUESTED. THE
!                         SET DATA IS STORED IN IZ(LBEG) THRU IZ(LEND)
!          ISET      = 0  NOT DEFINED
!          LCSE1 AND =    ARE COMPONENT DUPLICATION FLAG (IDUPL) AND
!          LCSE2          INCREMENT FLAG (INC)
!                    = 0  IMPLIES NO DUPLICATION/INCR SET BY COMPONENT
!          LCSE1     =-2  SET AND USE LOCALLY IF SORT2 AND ELEM. SET ARE
!                         INVOLVED.
!          LBEG AND  =    ARE BEGINNING AND ENDING POINTERS TO THE ELEM.
!          LEND           ID SET, ALL ELEMS. (LBEG .GT. LEND IF ISET=-1)
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   !>>>>EQUIVALENCE (chead,ihead(1))
   DATA nam , sortx/4HSTRS , 4HCN   , 4HSORT , 4H1    , 4H2   /
   DATA t24 , eor , noeor , iblank/1.E+24 , 1 , 0 , 1H /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! *** SET ISCAN ARRAY FROM COMPONENT SPECIFICATION
!
         chead = ' '
         nscan = 0
         ntop = iabs(topn)
!      PRINT *,' ENTERRING STRSCN,NTOP,ICOMP=',NTOP,ICOMP
         DO i = 1 , 30
            j = 2**(i-1)
            IF ( mod(icomp,2*j)>=j ) THEN
               nscan = nscan + 1
               IF ( i==1 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               iscan(nscan) = i
            ENDIF
            IF ( icompx/=0 ) THEN
               IF ( mod(icompx,2*j)>=j ) THEN
                  nscan = nscan + 1
                  iscan(nscan) = i + 31
               ENDIF
            ENDIF
         ENDDO
!      PRINT *,' AFTER 20, ICOMP=',ICOMP
         j = 2*j
         IF ( icomp>=j ) THEN
            nscan = nscan + 1
            iscan(nscan) = 31
         ENDIF
         IF ( icompx>=j ) THEN
            nscan = nscan + 1
            iscan(nscan) = 62
         ENDIF
         IF ( icompx/=0 ) CALL sort(0,0,1,1,iscan,nscan)
!      DEBUG = .TRUE.
!      PRINT *,' AFTER 26,NSCAN=',NSCAN
!      PRINT *,' AFTER 26,ISCAN=',(ISCAN(KB),KB=1,NSCAN)
         IF ( debug ) THEN
            WRITE (nout,99001) iopen , jopen , ielt , iel , iset , icomp , icompx , lcse1 , lcse2 , isort , subc , itrl3 , lbeg ,   &
                             & lend , nscan
99001       FORMAT (//2X,12HDEBUG/STRSCN,/,2(2X,L1),2X,2A4,13I8)
            IF ( iopt==2 ) WRITE (nout,99002) ntop , (iscan(j),j=1,nscan)
99002       FORMAT (5X,I9,31I3)
            IF ( iopt==1 ) WRITE (nout,99003) amax , amin , (iscan(j),j=1,nscan)
99003       FORMAT (5X,2E10.3,31I3)
            IF ( lend>lbeg ) WRITE (nout,99004) iset , (iz(j),j=lbeg,lend)
99004       FORMAT (/5X,3HSET,I8,(/5X,15I7))
            IF ( nscan>10 ) THEN
!
! *** FILE ERRORS
!
               WRITE (nout,99005) ielt
99005          FORMAT (//5X,34HTOO MANY COMPONENTS SPECIFIED FOR ,2A4)
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( nscan==0 ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
! *** INITIALIZATION
!
         idupl = lcse1
         inc = lcse2
         jnc = 0
         IF ( iset+1<0 ) THEN
         ELSEIF ( iset+1==0 ) THEN
            lcse1 = 0
            lcse2 = 0
         ELSE
            lcse1 = iz(lbeg)
            lcse2 = iabs(iz(lend))
         ENDIF
         ns = -1
         IF ( lcse1>lcse2 ) THEN
            WRITE (nout,99016) iset , lcse1 , lcse2 , lbeg , lend , ns , (iz(j),j=lbeg,lend)
         ELSEIF ( .NOT.iopen .OR. .NOT.jopen ) THEN
            WRITE (nout,99006) iopen , jopen
!
99006       FORMAT (//5X,52HSYSTEM ERROR/STRSCN.  INPUT OR OUTPUT FILE NOT REA  DY,2(2X,L1))
         ELSE
!
            lbuf1 = 1
            lbuf3 = 0
            lbuf0 = lbuf1 - 1
            il2 = 0
            nrew = 0
            icase = -1
            any = .FALSE.
            IF ( osubc==0 ) CALL fwdrec(*80,infile)
            IF ( iset/=-2 .AND. isort/=2 .AND. subc==osubc ) THEN
               DO i = 1 , 3
                  CALL bckrec(infile)
               ENDDO
               CALL fwdrec(*80,infile)
            ENDIF
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
! *** READ INPUT FILE ID RECORD AND SET ISORT FLAG FOR SORT1 OR SORT2
!     DATA TYPE
!     AT THIS TIME, ISORT MAY BE ALREADY SET BY PREVIOUS SCAN, OR ZERO
!
 20      IF ( isort==2 .OR. nrew>=2 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nrew = nrew + 1
         CALL rewind(infile)
         CALL fwdrec(*80,infile)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*20,*40,infile,id,50,0,iwds)
            CALL read(*80,*100,infile,head,96,1,iwds)
            isort = 1
            IF ( id(2)>=2000 ) isort = 2
            IF ( id(2)>=3000 ) THEN
!
! *** JOB DONE
!
               iopt = -id(2)
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
! *** SYNCHRONIZE SUBCASE ID (WHICH MAY NOT BE IN ASCENDING ORDER)
!
               IF ( iset/=-2 .AND. isort/=2 .AND. id(4)/=subc ) EXIT SPAG_Loop_1_1
!
! *** SYNCHRONIZE ELEMENT TYPE (WHICH MAY NOT BE IN ASCENDING ORDER)
!
               IF ( id(3)/=iel ) THEN
                  CALL fwdrec(*80,infile)
               ELSE
                  oel = iel
                  nrew = 0
!
! *** POSITION DATA BLOCK FOR FIRST CASE AND BEGIN SCAN
!
                  i = 140
                  IF ( debug ) WRITE (nout,99018) i , iset , isort , icase , lcse1 , lcse2 , subc
                  nwds = id(10)
!WKBNB 1/3/94 SPR93010 & 93011
                  layerd = .FALSE.
! FOR LAYERED QUAD4 AND TRIA3 IEL WILL BE EITHER 64 OR 83 RESPECTIVELY
! AND ID(10) WILL BE 10 (10 IS THE NUMBER OF WORDS PER LINE TO BE PRINTED).
                  IF ( (iel==64 .OR. iel==83) .AND. id(10)==10 ) layerd = .TRUE.
                  IF ( layerd ) THEN
! TO DETERMINE THE NUMBER OF WORDS PER EACH ELEMENT, WILL NEED TO DETERMINE
! HOW MANY LAYERS ARE PRESENT (Z(LBUF1+1)) AND ALLOW 10 WORDS PER LAYER
! PLUS A THREE WORD HEADER AND TWO EXTRA WORDS ON THE END.
                     CALL read(*80,*60,infile,iz(lbuf1),3,0,iwds)
                     nwds = 3 + 10*iz(lbuf1+1) + 2
!      PRINT *,' COMPUTED NWDS=',NWDS
                     CALL bckrec(infile)
                  ENDIF
!WKBNE 1/3/94 SPR93010 & 93011
                  nwds1 = nwds + 1
                  lbuf2 = lbuf1 + nwds1
                  lbuf3 = lbuf2
                  ih1 = lbuf2
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
 40      CALL rewind(infile)
         nrew = nrew + 1
         IF ( nrew>2 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
            CALL fwdrec(*80,infile)
            CALL read(*80,*40,infile,id,10,1,iwds)
            IF ( id(4)==subc ) THEN
               CALL bckrec(infile)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         ih2 = lbuf2 + nwds1*ntop - 1
         il1 = ih2 + 1
         il2 = ih2 + nwds1*ntop
         IF ( il2>lcore ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ii = 0
         jj = 0
         kk = 0
         mm = ih1
         nn = il1
         idelm = -1
         icase = 0
         any = .FALSE.
         IF ( lcse1==-2 ) lcse1 = 0
         IF ( lcse1>-2 ) ns = lbeg
         spag_nextblock_1 = 4
      CASE (4)
         ns = ns - 1
         ns = min0(ns,lbeg-1)
         IF ( isort==2 .AND. idelm/=-1 ) THEN
            CALL fwdrec(*80,infile)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_2: DO
            CALL read(*80,*60,infile,iz(lbuf1),nwds,0,iwds)
!
! *** CHECK WHETHER THIS ELEMENT IS NEEDED FOR SCAN
!     WALK THROUGH SET ARRAY IF IT IS NECESSARY TO DO SO (R.F. ONLY)
!     CHECK SUBCASE NO. INSTEAD OF ELEM. ID IF THIS IS A USER DAMP ALTER
!     RUN WITH SORT2 TYPE DATA
!
            IF ( iset/=-2 ) THEN
               IF ( iset==-1 .OR. lcse1==-2 ) EXIT SPAG_Loop_1_2
               idelm = iz(lbuf1)/10
               IF ( isort==2 ) idelm = id(5)/10
               DO
                  ns = ns + 1
                  IF ( ns>lend ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  izn = iz(ns)
                  IF ( izn>=0 ) THEN
                     IF ( idelm<izn ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( idelm==izn ) THEN
                        IF ( isort==2 ) lcse1 = -2
                        EXIT SPAG_Loop_1_2
                     ENDIF
                  ELSE
                     izn = iabs(izn)
                     IF ( idelm==izn ) THEN
                        IF ( isort==2 ) lcse1 = -2
                        EXIT SPAG_Loop_1_2
                     ELSE
                        izn1 = iz(ns-1)
                        IF ( izn1<=0 .OR. izn1>izn ) THEN
                           WRITE (nout,99016) iset , lcse1 , lcse2 , lbeg , lend , ns , (iz(j),j=lbeg,lend)
                           spag_nextblock_1 = 11
                           CYCLE SPAG_DispatchLoop_1
                        ELSEIF ( idelm<=izn ) THEN
                           IF ( idelm<izn1 ) THEN
                              WRITE (nout,99016) iset , lcse1 , lcse2 , lbeg , lend , ns , (iz(j),j=lbeg,lend)
                              spag_nextblock_1 = 11
                              CYCLE SPAG_DispatchLoop_1
                           ELSE
                              ns = ns - 1
                              IF ( isort==2 ) lcse1 = -2
                              EXIT SPAG_Loop_1_2
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               IF ( lcse1<=-1 ) EXIT SPAG_Loop_1_2
               icase = iz(lbuf1)
               IF ( isort==1 ) icase = icase/10
               IF ( icase>=lcse1 ) THEN
                  IF ( icase<=lcse2 ) EXIT SPAG_Loop_1_2
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_2
!
! *** MAKE SURE DEVICE CODE IS SET TO PRINT (SORT1 ONLY)
!     SET UP COMPONENT DUPLICATION/INC LOOP IF THEY ARE VALID
!
         IF ( isort==1 ) iz(lbuf1) = (iz(lbuf1)/10)*10 + 1
         i = 200
         IF ( debug ) WRITE (nout,99018) i , iz(lbuf1) , icase , lcse1 , lcse2 , idupl , inc , ns , isort , nwds , iset , subc ,    &
                           & iopt , any
         jdupl = 1
         jnc = 0
         IF ( iset/=-2 .AND. idupl>0 ) THEN
            jdupl = idupl
            jnc = inc
         ENDIF
!
! *** PICKUP MAX AND MIN OF CURRENT ELEMENT DATA
!     SAVE THESE MAX, MIN AS KEYS FOR SORTING LATER
!
         bmax = -t24
         bmin = t24
! QUAD4 (=64) AND TRIA3 (=83) WILL HAVE JDUPL NE 0 FOR LAMINATED
! CASE (I.E., WHEN LAYERD IS TRUE) FOR STRESS CASES
         IF ( (iel==64 .OR. iel==83) .AND. jdupl==49 .AND. .NOT.layerd .AND. stress ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( (iel==64 .OR. iel==83) .AND. jdupl/=49 .AND. layerd .AND. stress ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!WKBNB 1/3/94 SPR93010 & 93011
! SET QUAD4 OR TRIA3 TO FALSE TO INDICATE TO SUBROUTINE SCAN THAT
! DATA FOR THESE ELEMENTS HAS BEEN FOUND IN EITHER OES1 OR OES1L FILES.
         IF ( iel==64 ) quad4 = 1
         IF ( iel==83 ) tria3 = 1
! IF JDUPL IS 49 THAN THIS IS A QUAD4 OR TRIA3 LAYERED ELEMENT, GET
! VALUE AFTER ELEMENT ID IN RECORD TO DETERMINE THE NUMBER OF LAYERS IN
! IN THE ELEMENT.
         IF ( jdupl==49 ) jdupl = iz(lbuf0+2)
!WKBNE 1/3/94 SPR93010 & 93011
!      PRINT *,' BEFORE 230,JDUPL,JNC,NSCAN=',JDUPL,JNC,NSCAN
!      PRINT *,' BEFORE 230,ISCAN=',(ISCAN(KB),KB=1,NSCAN)
         DO j = 1 , nscan
            i = iscan(j)
            IF ( i<=nwds ) THEN
               kk = 0
               DO k = 1 , jdupl
!      WRITE(6,77777)Z(LBUF0+I+KK)
!77777 FORMAT(' HEX OF Z=',Z8)
                  zk = z(lbuf0+i+kk)
                  IF ( zk>bmax ) bmax = zk
                  IF ( zk<bmin ) bmin = zk
                  kk = kk + jnc
               ENDDO
            ENDIF
         ENDDO
!
         IF ( iopt==2 ) THEN
!
! *** OPTION TWO (IOPT=2)
!     ===================
!
!     TOP AND BOTTOM N VALUES FOR STRESSES
!     TOP VALUE SCAN FOR FORCES IF TOPN IS POSITIVE
!     BOTTEM VALUE SCAN FOR FORCES AND MARGIN ETC. IF TOPN IS NEGATIVE
!
!     II AND JJ ARE TOP AND BOTTOM ARRAY COUNTERS
!     MM IS POINTER TO THE SMALLEST OF THE TOP VALUSES
!     NN IS POINTER TO THE BIGGEST OF THE BOTTOM VALUSES
!
!     WHEN TOP AND BOTTOM ARRAYS ARE FILLED UP COMPLETELY WITH SCANNED
!     DATA (II=JJ=NTOP), IH1 AND IH2 ARE BEGINNING AND ENDING POINTERS
!     TO THE TOP VALUES, SIMILARY, IL1 AND IL2 ARE FOR THE BOTTOM VALUES
!
!     REMEMBER, SORF=1 FOR STRESS SCAN, SORF=2 FOR FORCE SCAN
!               NTOP=IABS(TOPN)
!
            any = .TRUE.
            IF ( Sorf/=2 .OR. topn>0 ) THEN
               IF ( .NOT.((Sorf==1 .AND. bmax<0.0) .OR. (ii>=ntop .AND. bmax<z(mm))) ) THEN
                  DO i = 1 , nwds
                     z(mm+i) = z(lbuf0+i)
                  ENDDO
                  z(mm) = bmax
                  IF ( ii<ntop ) THEN
                     ii = ii + 1
                     mm = mm + nwds1
                     IF ( ii<ntop ) GOTO 50
                  ENDIF
                  mm = ih1
                  bmax = +t24
                  DO i = ih1 , ih2 , nwds1
                     IF ( z(i)<=bmax ) THEN
                        bmax = z(i)
                        mm = i
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
!
 50         IF ( Sorf/=2 .OR. topn<0 ) THEN
               IF ( .NOT.((Sorf==1 .AND. bmin>0 .AND. topn>0) .OR. (jj>=ntop .AND. bmin>z(nn))) ) THEN
                  DO i = 1 , nwds
                     z(nn+i) = z(lbuf0+i)
                  ENDDO
                  z(nn) = bmin
                  IF ( jj<ntop ) THEN
                     jj = jj + 1
                     nn = nn + nwds1
                     IF ( jj<ntop ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
                  nn = il1
                  bmin = -t24
                  DO i = il1 , il2 , nwds1
                     IF ( z(i)>=bmin ) THEN
                        bmin = z(i)
                        nn = i
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
            spag_nextblock_1 = 5
         ELSE
!
! *** OPTION ONE (IOPT=1, BY MAX-MIN)
!     ===============================
!
!     LBUF2 AND LBUF3 ARE BEGINNING AND ENDING POINTERS TO THE SCANNED
!     DATA ARRAY
!
            IF ( bmax<amax .AND. bmin>amin ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( lbuf3+nwds1>lcore ) THEN
               WRITE (nout,99017) ielt
               spag_nextblock_1 = 11
            ELSE
               any = .TRUE.
               DO i = 1 , nwds
                  z(lbuf3+i) = z(lbuf0+i)
               ENDDO
               z(lbuf3) = bmax
               IF ( bmin<=amin ) z(lbuf3) = bmin
               lbuf3 = lbuf3 + nwds1
               spag_nextblock_1 = 5
            ENDIF
         ENDIF
      CASE (6)
!
! *** ELEM. ID LIST, OR SUBCASE LIST, HAS BEEN EXHAULSTED
!     (NOTE - SHOULD RETURN WITHOUT FWDREC HERE.  IF STRSCN IS CALLED
!             AGAIN, FWDREC WILL BE DONE AT 90)
!
         i = 330
         IF ( debug ) WRITE (nout,99018) i , idelm , icase , isort , ns , lbeg , lend , lcse1 , lcse2
         IF ( any ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
!
! *** EOR READ (FROM 160)
!
 60      id(11) = 0
         IF ( .NOT.(any) ) THEN
            id(11) = 1
            id(10) = 1
            nwds = 1
            il2 = ih1 + 1
            iz(il2) = 01
            IF ( isort==2 ) iz(il2) = 0
            iz(2) = 1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
! *** THIS ELEMENT TYPE IS DONE.  BEGIN OUTPUT PROCEDURE
!     MAKE SURE DEVICE CODE IS SET TO PRINT, ALWAYS
!     ADD SCAN HEADER TO LABEL LINE
!
         id(1) = (id(1)/10)*10 + 1
         IF ( isort==2 ) id(5) = (id(5)/10)*10 + 1
         CALL write(oufile,id(1),50,noeor)
!
! *** INTERNAL ROUTINE TO SYNTHESIZE THE COMPONENTS FOR HEADING
!
         IF ( jnc<=0 ) THEN
         ENDIF
         numfld = 0
!      PRINT *,' STRSCN,INC,IDUPL,NSCAN=',INC,IDUPL,NSCAN
!      PRINT *,' ISCAN=',(ISCAN(KB),KB=1,NSCAN)
         SPAG_Loop_1_3: DO i = 1 , nscan
!      PRINT *,' STRSCN CALLING STRNAM,I,ISCAN=',I,ISCAN(I)
            IF ( stress ) CALL strnam(iel,iscan(i),field)
            IF ( force ) CALL fornam(iel,iscan(i),field)
            IF ( field/=' ' ) THEN
               IF ( numfld/=0 ) THEN
                  DO k = 1 , numfld
                     IF ( field==scnfld(k) ) CYCLE SPAG_Loop_1_3
                  ENDDO
               ENDIF
               IF ( numfld>=6 ) EXIT SPAG_Loop_1_3
               numfld = numfld + 1
               scnfld(numfld) = field
            ENDIF
         ENDDO SPAG_Loop_1_3
         IF ( numfld==1 ) chead(1:19) = 'SCANNED BY FIELD:  '
         IF ( numfld/=1 ) chead(1:19) = 'SCANNED BY FIELDS: '
         istr = 20
         SPAG_Loop_1_4: DO i = 1 , numfld
            len = index(scnfld(i),' ')
            iend = istr + len - 1
            IF ( iend>51 ) THEN
               chead(istr:51) = ',...'
               EXIT SPAG_Loop_1_4
            ELSE
               IF ( i==1 ) chead(istr:iend) = scnfld(i)(1:len)
               IF ( i>1 ) chead(istr:iend+2) = ', '//scnfld(numfld)(1:len)
               istr = iend
               IF ( i>1 ) istr = iend + 2
            ENDIF
         ENDDO SPAG_Loop_1_4
         IF ( iset>0 ) THEN
            WRITE (chead(52:68),99007) iset
99007       FORMAT (' SET:',I8)
         ENDIF
         IF ( iopt==1 ) THEN
            WRITE (chead(69:100),99008) amin , amax
99008       FORMAT ('EXCLUDING TO ',2(F8.1))
         ELSE
            WRITE (chead(69:100),99009) ntop
99009       FORMAT ('TOP AND BOTTOM  ',I4,' VALUES')
         ENDIF
         head(73) = iblank
         DO i = 1 , 25
            head(i+64) = ihead(i)
         ENDDO
         head(95) = sortx(1)
         head(96) = sortx(2)
         IF ( isort==2 ) head(96) = sortx(3)
         CALL write(oufile,head,96,eor)
!
         kk = 1
         j = 2
         IF ( any ) THEN
!
! *** (IOPT=2 ONLY) IF TOP AND BOTTOM ARRAYS ARE NOT FULL (I.E. II AND/
!     OR JJ ARE  .LT. NTOP), WE NEED TO SQUEEZE OUT SOME EMPTY CELLS IN
!     THE SPACE FROM Z(IH1) THRU Z(IL2) BEFORE SORTING THE SCANNED DATA
!
            IF ( iopt/=2 ) THEN
               il2 = lbuf3
            ELSEIF ( ii+jj/=2*ntop ) THEN
               kk = (ntop-ii)*nwds1
               il2 = ih2 + jj*nwds1
               DO i = il1 , il2
                  z(i-kk) = z(i)
               ENDDO
               il2 = ih1 + (ii+jj)*nwds1 - 1
            ENDIF
!
! *** MOVE MAX-MIN KEYS BEHIND IL2 SPACE AND BEGIN A 2-COLUMN SORT
!     THUS AVOID MASSIVE DATA TRANSFER DURING SORTING IF THE ORIGINAL
!     MULTI-COLUMNS SCANNED DATA WERE USED.
!
            kk = (il2-ih1+1)/nwds1
            IF ( il2+2*kk>lcore ) THEN
               IF ( iopt>1 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               WRITE (nout,99017) ielt
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
               i = ih1
               j = il2 - 2
               k = 0
               SPAG_Loop_1_5: DO
                  j = j + 2
                  k = k + 1
                  z(j+1) = z(i)
                  iz(j+2) = k
                  i = i + nwds1
                  IF ( i>=il2 ) THEN
                     k = 2*kk
                     CALL sortf(0,0,2,1,z(il2+1),k)
!
! *** BEGIN OUTPUT SCANNED DATA
!
                     j = j + 2
                     IF ( debug ) WRITE (nout,99010) j , kk , (z(il2+i),iz(il2+i+1),i=1,k,2)
99010                FORMAT (/9X,17HDEBUG/STRSCN 450-,2I7,(/15X,E11.3,I5))
                     EXIT SPAG_Loop_1_5
                  ENDIF
               ENDDO SPAG_Loop_1_5
            ENDIF
         ENDIF
         DO k = 1 , kk
            i = ih1 + (iz(j)-1)*nwds1
            CALL write(oufile,iz(i+1),nwds,noeor)
            j = j - 2
         ENDDO
         CALL write(oufile,0,0,eor)
         itrl3 = itrl3 + 2
         j = kk*nwds
         IF ( debug ) WRITE (nout,99011) j , itrl3 , ii , jj
99011    FORMAT (/,I9,37H WORDS WRITTEN TO OUTPUT FILE, RECORD,I5,9X,2I5)
         IF ( .NOT.any ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!*** EOF ON INPUT FILE (FROM 100)
!     NEXT ACTION WILL BE LOOP-BACK FOR MORE OR RETURN TO SCAN
!
!           R.F. (ISET.NE.-2)        I     USER DMAP ALTER (ISET=-2)
!     -------------------------------+----------------------------------
!     SORT1 - RETURN TO SCAN FOR     I  SORT1 - LOOP BACK FOR NEXT SUB-
!             NEXT SUBCASE           I          CASE, DISREGARDING THE
!                                    I          ELEM ID LIST
!     SORT2 - LOOP BACK FOR NEXT     I  SORT2 - LOOP BACK FOR NEXT ELEM,
!             ELEM. IF NO ELEM. LIST I          DISREGARDING THE SUBCASE
!           - IF ELEM. LIST EXISTS,  I          LIST
!             LOOP BACK ONLY IF MORE I
!             ELEM. TO BE PROCESSED  I
!             OTHERWISE, RETURN      I
!
         IF ( il2<=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         il2 = -1
         nrew = 0
         IF ( iset==-2 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( isort==1 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( lend<=lbeg ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ns<lend ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (9)
!
! *** COULD NOT FIND ELEMENT OR SUBCASE
!
         IF ( il2/=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
!WKBNB 1/4/94 SPR93010 & 93011
         IF ( iel==64 .AND. quad4==0 ) quad4 = -1
         IF ( iel==83 .AND. tria3==0 ) tria3 = -1
         IF ( iel/=64 .AND. iel/=83 ) THEN
!WKBNE 1/4/94 SPR93010 & 93011
            CALL fname(infile,z(1))
            WRITE (nout,99012) ielt , z(1) , z(2) , nrew
99012       FORMAT (//5X,8HELEMENT ,2A4,32H, OR SUBCASE, NOT IN DATA BLOCK ,2A4,I7,8H REWINDS)
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         DO i = 1 , 16
            head(i+73) = iblank
         ENDDO
         head(95) = iblank
         head(96) = iblank
         osubc = subc
         RETURN
 80      IF ( .NOT.any ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = -2
         CALL mesage(j,infile,nam)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 100     j = -3
         CALL mesage(j,infile,nam)
         spag_nextblock_1 = 11
      CASE (12)
         j = (lcore-lbuf2+1)/(2*nwds1)
         WRITE (nout,99013) ielt , ntop , j
99013    FORMAT (//5X,45HINSUFFICIENT CORE TO PROCESS OUTPUT SCAN FOR ,2A4,/5X,                                                     &
                &89HLARGE TOPN VALUE REQUIRES EXCESSIVE CORE REQUIREMENT. TOP N IS AUTOMATICALLY REDUCED FROM,I5,3H TO,I5)
         ntop = j
         spag_nextblock_1 = 3
      CASE (13)
         WRITE (nout,99014) icomp , icompx , ielt
99014    FORMAT (//5X,40HFIELD COMPONENT ERROR, CASE ABORT/STRSCN,5X,2I9,1X,2A4)
         spag_nextblock_1 = 11
      CASE (14)
         IF ( debug ) WRITE (nout,99015) ielt , subc
99015    FORMAT (//5X,37HNO APPLICABLE ELEMT OR SUBCASE/STRSCN,3X,2A4,I8)
         CALL mesage(30,220,ielt)
         spag_nextblock_1 = 8
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99016 FORMAT (//5X,23HSYSTEM ERROR/STRSCN 740,7X,6I7,/,(5X,12I10))
99017 FORMAT (//5X,40HINSUFFICIENT CORE TO PROCESS OUTPUT SCAN,/5X,56HSMALL VALUES OF AMAX-AMIN REQUIRE LARGE CORE REQUIREMENT)
99018 FORMAT (/9X,12HDEBUG/STRSCN,I4,1H-,/2X,I9,11I7,3X,L1)
END SUBROUTINE strscn
