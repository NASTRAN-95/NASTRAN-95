!*==strscn.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strscn(Sorf)
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_XSCANX
   USE C_ZZZZZZ
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
         ntop = iabs(Topn)
!      PRINT *,' ENTERRING STRSCN,NTOP,ICOMP=',NTOP,ICOMP
         DO i = 1 , 30
            j = 2**(i-1)
            IF ( mod(Icomp,2*j)>=j ) THEN
               nscan = nscan + 1
               IF ( i==1 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               iscan(nscan) = i
            ENDIF
            IF ( Icompx/=0 ) THEN
               IF ( mod(Icompx,2*j)>=j ) THEN
                  nscan = nscan + 1
                  iscan(nscan) = i + 31
               ENDIF
            ENDIF
         ENDDO
!      PRINT *,' AFTER 20, ICOMP=',ICOMP
         j = 2*j
         IF ( Icomp>=j ) THEN
            nscan = nscan + 1
            iscan(nscan) = 31
         ENDIF
         IF ( Icompx>=j ) THEN
            nscan = nscan + 1
            iscan(nscan) = 62
         ENDIF
         IF ( Icompx/=0 ) CALL sort(0,0,1,1,iscan,nscan)
!      DEBUG = .TRUE.
!      PRINT *,' AFTER 26,NSCAN=',NSCAN
!      PRINT *,' AFTER 26,ISCAN=',(ISCAN(KB),KB=1,NSCAN)
         IF ( Debug ) THEN
            WRITE (Nout,99001) Iopen , Jopen , Ielt , Iel , Iset , Icomp , Icompx , Lcse1 , Lcse2 , Isort , Subc , Itrl3 , Lbeg ,   &
                             & Lend , nscan
99001       FORMAT (//2X,12HDEBUG/STRSCN,/,2(2X,L1),2X,2A4,13I8)
            IF ( Iopt==2 ) WRITE (Nout,99002) ntop , (iscan(j),j=1,nscan)
99002       FORMAT (5X,I9,31I3)
            IF ( Iopt==1 ) WRITE (Nout,99003) Amax , Amin , (iscan(j),j=1,nscan)
99003       FORMAT (5X,2E10.3,31I3)
            IF ( Lend>Lbeg ) WRITE (Nout,99004) Iset , (iz(j),j=Lbeg,Lend)
99004       FORMAT (/5X,3HSET,I8,(/5X,15I7))
            IF ( nscan>10 ) THEN
!
! *** FILE ERRORS
!
               WRITE (Nout,99005) Ielt
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
         idupl = Lcse1
         inc = Lcse2
         jnc = 0
         IF ( Iset+1<0 ) THEN
         ELSEIF ( Iset+1==0 ) THEN
            Lcse1 = 0
            Lcse2 = 0
         ELSE
            Lcse1 = iz(Lbeg)
            Lcse2 = iabs(iz(Lend))
         ENDIF
         ns = -1
         IF ( Lcse1>Lcse2 ) THEN
            WRITE (Nout,99006) Iset , Lcse1 , Lcse2 , Lbeg , Lend , ns , (iz(j),j=Lbeg,Lend)
         ELSEIF ( .NOT.Iopen .OR. .NOT.Jopen ) THEN
            WRITE (Nout,99007) Iopen , Jopen
!
99007       FORMAT (//5X,52HSYSTEM ERROR/STRSCN.  INPUT OR OUTPUT FILE NOT REA  DY,2(2X,L1))
         ELSE
!
            lbuf1 = 1
            lbuf3 = 0
            lbuf0 = lbuf1 - 1
            il2 = 0
            nrew = 0
            icase = -1
            any = .FALSE.
            IF ( Osubc==0 ) CALL fwdrec(*80,Infile)
            IF ( Iset/=-2 .AND. Isort/=2 .AND. Subc==Osubc ) THEN
               DO i = 1 , 3
                  CALL bckrec(Infile)
               ENDDO
               CALL fwdrec(*80,Infile)
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
 20      IF ( Isort==2 .OR. nrew>=2 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nrew = nrew + 1
         CALL rewind(Infile)
         CALL fwdrec(*80,Infile)
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*20,*40,Infile,id,50,0,iwds)
            CALL read(*80,*100,Infile,Head,96,1,iwds)
            Isort = 1
            IF ( id(2)>=2000 ) Isort = 2
            IF ( id(2)>=3000 ) THEN
!
! *** JOB DONE
!
               Iopt = -id(2)
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
! *** SYNCHRONIZE SUBCASE ID (WHICH MAY NOT BE IN ASCENDING ORDER)
!
               IF ( Iset/=-2 .AND. Isort/=2 .AND. id(4)/=Subc ) EXIT SPAG_Loop_1_1
!
! *** SYNCHRONIZE ELEMENT TYPE (WHICH MAY NOT BE IN ASCENDING ORDER)
!
               IF ( id(3)/=Iel ) THEN
                  CALL fwdrec(*80,Infile)
               ELSE
                  Oel = Iel
                  nrew = 0
!
! *** POSITION DATA BLOCK FOR FIRST CASE AND BEGIN SCAN
!
                  i = 140
                  IF ( Debug ) WRITE (Nout,99018) i , Iset , Isort , icase , Lcse1 , Lcse2 , Subc
                  nwds = id(10)
!WKBNB 1/3/94 SPR93010 & 93011
                  Layerd = .FALSE.
! FOR LAYERED QUAD4 AND TRIA3 IEL WILL BE EITHER 64 OR 83 RESPECTIVELY
! AND ID(10) WILL BE 10 (10 IS THE NUMBER OF WORDS PER LINE TO BE PRINTED).
                  IF ( (Iel==64 .OR. Iel==83) .AND. id(10)==10 ) Layerd = .TRUE.
                  IF ( Layerd ) THEN
! TO DETERMINE THE NUMBER OF WORDS PER EACH ELEMENT, WILL NEED TO DETERMINE
! HOW MANY LAYERS ARE PRESENT (Z(LBUF1+1)) AND ALLOW 10 WORDS PER LAYER
! PLUS A THREE WORD HEADER AND TWO EXTRA WORDS ON THE END.
                     CALL read(*80,*60,Infile,iz(lbuf1),3,0,iwds)
                     nwds = 3 + 10*iz(lbuf1+1) + 2
!      PRINT *,' COMPUTED NWDS=',NWDS
                     CALL bckrec(Infile)
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
 40      CALL rewind(Infile)
         nrew = nrew + 1
         IF ( nrew>2 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
            CALL fwdrec(*80,Infile)
            CALL read(*80,*40,Infile,id,10,1,iwds)
            IF ( id(4)==Subc ) THEN
               CALL bckrec(Infile)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (3)
         ih2 = lbuf2 + nwds1*ntop - 1
         il1 = ih2 + 1
         il2 = ih2 + nwds1*ntop
         IF ( il2>Lcore ) THEN
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
         IF ( Lcse1==-2 ) Lcse1 = 0
         IF ( Lcse1>-2 ) ns = Lbeg
         spag_nextblock_1 = 4
      CASE (4)
         ns = ns - 1
         ns = min0(ns,Lbeg-1)
         IF ( Isort==2 .AND. idelm/=-1 ) THEN
            CALL fwdrec(*80,Infile)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_2: DO
            CALL read(*80,*60,Infile,iz(lbuf1),nwds,0,iwds)
!
! *** CHECK WHETHER THIS ELEMENT IS NEEDED FOR SCAN
!     WALK THROUGH SET ARRAY IF IT IS NECESSARY TO DO SO (R.F. ONLY)
!     CHECK SUBCASE NO. INSTEAD OF ELEM. ID IF THIS IS A USER DAMP ALTER
!     RUN WITH SORT2 TYPE DATA
!
            IF ( Iset/=-2 ) THEN
               IF ( Iset==-1 .OR. Lcse1==-2 ) EXIT SPAG_Loop_1_2
               idelm = iz(lbuf1)/10
               IF ( Isort==2 ) idelm = id(5)/10
               DO
                  ns = ns + 1
                  IF ( ns>Lend ) THEN
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
                        IF ( Isort==2 ) Lcse1 = -2
                        EXIT SPAG_Loop_1_2
                     ENDIF
                  ELSE
                     izn = iabs(izn)
                     IF ( idelm==izn ) THEN
                        IF ( Isort==2 ) Lcse1 = -2
                        EXIT SPAG_Loop_1_2
                     ELSE
                        izn1 = iz(ns-1)
                        IF ( izn1<=0 .OR. izn1>izn ) THEN
                           WRITE (Nout,99006) Iset , Lcse1 , Lcse2 , Lbeg , Lend , ns , (iz(j),j=Lbeg,Lend)
                           spag_nextblock_1 = 11
                           CYCLE SPAG_DispatchLoop_1
                        ELSEIF ( idelm<=izn ) THEN
                           IF ( idelm<izn1 ) THEN
                              WRITE (Nout,99006) Iset , Lcse1 , Lcse2 , Lbeg , Lend , ns , (iz(j),j=Lbeg,Lend)
                              spag_nextblock_1 = 11
                              CYCLE SPAG_DispatchLoop_1
                           ELSE
                              ns = ns - 1
                              IF ( Isort==2 ) Lcse1 = -2
                              EXIT SPAG_Loop_1_2
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               IF ( Lcse1<=-1 ) EXIT SPAG_Loop_1_2
               icase = iz(lbuf1)
               IF ( Isort==1 ) icase = icase/10
               IF ( icase>=Lcse1 ) THEN
                  IF ( icase<=Lcse2 ) EXIT SPAG_Loop_1_2
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_2
!
! *** MAKE SURE DEVICE CODE IS SET TO PRINT (SORT1 ONLY)
!     SET UP COMPONENT DUPLICATION/INC LOOP IF THEY ARE VALID
!
         IF ( Isort==1 ) iz(lbuf1) = (iz(lbuf1)/10)*10 + 1
         i = 200
         IF ( Debug ) WRITE (Nout,99018) i , iz(lbuf1) , icase , Lcse1 , Lcse2 , idupl , inc , ns , Isort , nwds , Iset , Subc ,    &
                           & Iopt , any
         jdupl = 1
         jnc = 0
         IF ( Iset/=-2 .AND. idupl>0 ) THEN
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
         IF ( (Iel==64 .OR. Iel==83) .AND. jdupl==49 .AND. .NOT.Layerd .AND. Stress ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( (Iel==64 .OR. Iel==83) .AND. jdupl/=49 .AND. Layerd .AND. Stress ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!WKBNB 1/3/94 SPR93010 & 93011
! SET QUAD4 OR TRIA3 TO FALSE TO INDICATE TO SUBROUTINE SCAN THAT
! DATA FOR THESE ELEMENTS HAS BEEN FOUND IN EITHER OES1 OR OES1L FILES.
         IF ( Iel==64 ) Quad4 = 1
         IF ( Iel==83 ) Tria3 = 1
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
                  zk = Z(lbuf0+i+kk)
                  IF ( zk>bmax ) bmax = zk
                  IF ( zk<bmin ) bmin = zk
                  kk = kk + jnc
               ENDDO
            ENDIF
         ENDDO
!
         IF ( Iopt==2 ) THEN
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
            IF ( Sorf/=2 .OR. Topn>0 ) THEN
               IF ( .NOT.((Sorf==1 .AND. bmax<0.0) .OR. (ii>=ntop .AND. bmax<Z(mm))) ) THEN
                  DO i = 1 , nwds
                     Z(mm+i) = Z(lbuf0+i)
                  ENDDO
                  Z(mm) = bmax
                  IF ( ii<ntop ) THEN
                     ii = ii + 1
                     mm = mm + nwds1
                     IF ( ii<ntop ) GOTO 50
                  ENDIF
                  mm = ih1
                  bmax = +t24
                  DO i = ih1 , ih2 , nwds1
                     IF ( Z(i)<=bmax ) THEN
                        bmax = Z(i)
                        mm = i
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
!
 50         IF ( Sorf/=2 .OR. Topn<0 ) THEN
               IF ( .NOT.((Sorf==1 .AND. bmin>0 .AND. Topn>0) .OR. (jj>=ntop .AND. bmin>Z(nn))) ) THEN
                  DO i = 1 , nwds
                     Z(nn+i) = Z(lbuf0+i)
                  ENDDO
                  Z(nn) = bmin
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
                     IF ( Z(i)>=bmin ) THEN
                        bmin = Z(i)
                        nn = i
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
! *** OPTION ONE (IOPT=1, BY MAX-MIN)
!     ===============================
!
!     LBUF2 AND LBUF3 ARE BEGINNING AND ENDING POINTERS TO THE SCANNED
!     DATA ARRAY
!
            IF ( bmax<Amax .AND. bmin>Amin ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( lbuf3+nwds1>Lcore ) THEN
               WRITE (Nout,99008) Ielt
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
               any = .TRUE.
               DO i = 1 , nwds
                  Z(lbuf3+i) = Z(lbuf0+i)
               ENDDO
               Z(lbuf3) = bmax
               IF ( bmin<=Amin ) Z(lbuf3) = bmin
               lbuf3 = lbuf3 + nwds1
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
      CASE (6)
!
! *** ELEM. ID LIST, OR SUBCASE LIST, HAS BEEN EXHAULSTED
!     (NOTE - SHOULD RETURN WITHOUT FWDREC HERE.  IF STRSCN IS CALLED
!             AGAIN, FWDREC WILL BE DONE AT 90)
!
         i = 330
         IF ( Debug ) WRITE (Nout,99018) i , idelm , icase , Isort , ns , Lbeg , Lend , Lcse1 , Lcse2
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
            IF ( Isort==2 ) iz(il2) = 0
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
         IF ( Isort==2 ) id(5) = (id(5)/10)*10 + 1
         CALL write(Oufile,id(1),50,noeor)
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
            IF ( Stress ) CALL strnam(Iel,iscan(i),field)
            IF ( Force ) CALL fornam(Iel,iscan(i),field)
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
         IF ( Iset>0 ) THEN
            WRITE (chead(52:68),99009) Iset
99009       FORMAT (' SET:',I8)
         ENDIF
         IF ( Iopt==1 ) THEN
            WRITE (chead(69:100),99010) Amin , Amax
99010       FORMAT ('EXCLUDING TO ',2(F8.1))
         ELSE
            WRITE (chead(69:100),99011) ntop
99011       FORMAT ('TOP AND BOTTOM  ',I4,' VALUES')
         ENDIF
         Head(73) = iblank
         DO i = 1 , 25
            Head(i+64) = ihead(i)
         ENDDO
         Head(95) = sortx(1)
         Head(96) = sortx(2)
         IF ( Isort==2 ) Head(96) = sortx(3)
         CALL write(Oufile,Head,96,eor)
!
         kk = 1
         j = 2
         IF ( any ) THEN
!
! *** (IOPT=2 ONLY) IF TOP AND BOTTOM ARRAYS ARE NOT FULL (I.E. II AND/
!     OR JJ ARE  .LT. NTOP), WE NEED TO SQUEEZE OUT SOME EMPTY CELLS IN
!     THE SPACE FROM Z(IH1) THRU Z(IL2) BEFORE SORTING THE SCANNED DATA
!
            IF ( Iopt/=2 ) THEN
               il2 = lbuf3
            ELSEIF ( ii+jj/=2*ntop ) THEN
               kk = (ntop-ii)*nwds1
               il2 = ih2 + jj*nwds1
               DO i = il1 , il2
                  Z(i-kk) = Z(i)
               ENDDO
               il2 = ih1 + (ii+jj)*nwds1 - 1
            ENDIF
!
! *** MOVE MAX-MIN KEYS BEHIND IL2 SPACE AND BEGIN A 2-COLUMN SORT
!     THUS AVOID MASSIVE DATA TRANSFER DURING SORTING IF THE ORIGINAL
!     MULTI-COLUMNS SCANNED DATA WERE USED.
!
            kk = (il2-ih1+1)/nwds1
            IF ( il2+2*kk>Lcore ) THEN
               IF ( Iopt>1 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               WRITE (Nout,99008) Ielt
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSE
               i = ih1
               j = il2 - 2
               k = 0
               SPAG_Loop_1_5: DO
                  j = j + 2
                  k = k + 1
                  Z(j+1) = Z(i)
                  iz(j+2) = k
                  i = i + nwds1
                  IF ( i>=il2 ) THEN
                     k = 2*kk
                     CALL sortf(0,0,2,1,Z(il2+1),k)
!
! *** BEGIN OUTPUT SCANNED DATA
!
                     j = j + 2
                     IF ( Debug ) WRITE (Nout,99012) j , kk , (Z(il2+i),iz(il2+i+1),i=1,k,2)
99012                FORMAT (/9X,17HDEBUG/STRSCN 450-,2I7,(/15X,E11.3,I5))
                     EXIT SPAG_Loop_1_5
                  ENDIF
               ENDDO SPAG_Loop_1_5
            ENDIF
         ENDIF
         DO k = 1 , kk
            i = ih1 + (iz(j)-1)*nwds1
            CALL write(Oufile,iz(i+1),nwds,noeor)
            j = j - 2
         ENDDO
         CALL write(Oufile,0,0,eor)
         Itrl3 = Itrl3 + 2
         j = kk*nwds
         IF ( Debug ) WRITE (Nout,99013) j , Itrl3 , ii , jj
99013    FORMAT (/,I9,37H WORDS WRITTEN TO OUTPUT FILE, RECORD,I5,9X,2I5)
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
         IF ( Iset==-2 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Isort==1 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Lend<=Lbeg ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ns<Lend ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
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
         IF ( Iel==64 .AND. Quad4==0 ) Quad4 = -1
         IF ( Iel==83 .AND. Tria3==0 ) Tria3 = -1
         IF ( Iel/=64 .AND. Iel/=83 ) THEN
!WKBNE 1/4/94 SPR93010 & 93011
            CALL fname(Infile,Z(1))
            WRITE (Nout,99014) Ielt , Z(1) , Z(2) , nrew
99014       FORMAT (//5X,8HELEMENT ,2A4,32H, OR SUBCASE, NOT IN DATA BLOCK ,2A4,I7,8H REWINDS)
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         DO i = 1 , 16
            Head(i+73) = iblank
         ENDDO
         Head(95) = iblank
         Head(96) = iblank
         Osubc = Subc
         RETURN
 80      IF ( .NOT.any ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = -2
         CALL mesage(j,Infile,nam)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 100     j = -3
         CALL mesage(j,Infile,nam)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         j = (Lcore-lbuf2+1)/(2*nwds1)
         WRITE (Nout,99015) Ielt , ntop , j
99015    FORMAT (//5X,45HINSUFFICIENT CORE TO PROCESS OUTPUT SCAN FOR ,2A4,/5X,                                                     &
                &89HLARGE TOPN VALUE REQUIRES EXCESSIVE CORE REQUIREMENT. TOP N IS AUTOMATICALLY REDUCED FROM,I5,3H TO,I5)
         ntop = j
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         WRITE (Nout,99016) Icomp , Icompx , Ielt
99016    FORMAT (//5X,40HFIELD COMPONENT ERROR, CASE ABORT/STRSCN,5X,2I9,1X,2A4)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         IF ( Debug ) WRITE (Nout,99017) Ielt , Subc
99017    FORMAT (//5X,37HNO APPLICABLE ELEMT OR SUBCASE/STRSCN,3X,2A4,I8)
         CALL mesage(30,220,Ielt)
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99006 FORMAT (//5X,23HSYSTEM ERROR/STRSCN 740,7X,6I7,/,(5X,12I10))
99008 FORMAT (//5X,40HINSUFFICIENT CORE TO PROCESS OUTPUT SCAN,/5X,56HSMALL VALUES OF AMAX-AMIN REQUIRE LARGE CORE REQUIREMENT)
99018 FORMAT (/9X,12HDEBUG/STRSCN,I4,1H-,/2X,I9,11I7,3X,L1)
END SUBROUTINE strscn
