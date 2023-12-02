!*==xosgen.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xosgen
!
!     THE PURPOSE OF THIS ROUTINE IS TO GENERATE THE OSCAR ARRAY.
!
!          ... DESCRIPTION OF PROGRAM VARIABLES ...
!     IENDF  = FLAG SIGNALING END OF DMAP SEQUENCE.
!     LDEF   = SCRATCH USED IN SCANNING LBLTBL TABLE.
!     LBLTOP = TOP OF LBLTBL ARRAY.
!     LBLBOT = BOTTOM OF LBLTBL ARRAY.
!     LSTLBL = POINTER TO LAST LABEL ENTRY MADE IN LBLTBL.
!     LSTPAR = POINTER TO LAST PARAMETER NAME ENTRY MADE IN LBLTBL.
!     NAMTBL = NAME CONVERSION TABLE FOR TYPE E NAMES.
!     IEXFLG = FLAG INDICATING LAST OSCAR ENTRY WAS EXIT.
!     IOSPNT = POINTER TO NEXT AVAILABLE WORD IN OSCAR ENTRY.
!     NOSPNT = POINTER TO DATA BLOCK NAME COUNT IN OSCAR ENTRY.
!     NTYPEE = TABLE CONTAINING TYPE E DMAP NAMES
!     IPRCFO = POINTER TO LAST TYPE F OR O OSCAR ENTRY.
!     NDIAG1 = NAME OF THE DIAGNOSTIC O/P PROCESSOR
!     ITYPE  = TABLE FOR TRANSLATING TYPE CODES TO WORD LENGTH
!     VARFLG = FLAG INDICATING VARIABLE FOUND IN EQUIV OR PURGE
!              INSTRUCTION.
!
   IMPLICIT NONE
   USE c_autocm
   USE c_autosm
   USE c_moddmp
   USE c_passer
   USE c_system
   USE c_xceitb
   USE c_xfiat
   USE c_xgpi2
   USE c_xgpi3
   USE c_xgpi4
   USE c_xgpi5
   USE c_xgpi6
   USE c_xgpi7
   USE c_xgpic
   USE c_xgpid
   USE c_xgpie
   USE c_xoldpt
   USE c_xvps
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) , SAVE :: cdcomp , declar , fparam
   INTEGER , DIMENSION(1) :: dmpcrd , lbltbl , med , oscar
   INTEGER :: fcode , i , i1 , idlhss , idpbuf , ilevel , index , iold , ion , iosdav , iospnt , iprime , ivrept , ix , iy , j ,    &
            & j1 , jx , k , k1 , k2 , kdh , kk , l , lblbot , lblerr , lbltop , ldef , lookup , loscar , lstlbl , lstlsv , lstpar , &
            & m , n1 , n2 , n3 , nospnt , nxpurg , os2b4 , osbot , ospnt , osprc , varflg
   INTEGER , SAVE :: iendf , iprcfo , nceit1 , nceit2 , nfile , nlblt1 , nlblt2 , nvps , xchk
   INTEGER , DIMENSION(6) , SAVE :: itype
   INTEGER , DIMENSION(12) , SAVE :: namtbl
   INTEGER , DIMENSION(5,2) , SAVE :: nskip
   INTEGER , DIMENSION(5) :: os
   INTEGER , DIMENSION(2) , SAVE :: prechk , xdmap
   LOGICAL :: skip
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!WKBR COMMON /XGPI3 / PVT(2)
!WKBR COMMON /XCEITB/ CEITBL(2)
!
!     EQUIVALENCE     (NTYPEE(1),NTIME ), (NTYPEE(2),NSAVE )
!    1                (NTYPEE(3),NOUTPT), (NTYPEE(4),NCHKPT)
!    2                (NTYPEE(5),NPURGE), (NTYPEE(6),NEQUIV)
!    3                (NTYPEE(7),NCPW  ), (NTYPEE(8),NBPC  )
!    4                (NTYPEE(9),NWPC  )
   !>>>>EQUIVALENCE (namtbl(9),nxpurg)
   !>>>>EQUIVALENCE (oscar(1),dmpcrd(1),lbltbl(1),med(1),Os(5)) , (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt)
!
   DATA xchk/4HXCHK/
   DATA itype/1 , 1 , 2 , 2 , 2 , 4/
   DATA iprcfo/0/ , iendf/0/
   DATA nfile/4HFILE/
   DATA nvps/4HVPS /
   DATA prechk/4HPREC , 4HHK  / , xdmap/4HXDMA , 4HP   /
   DATA nceit1/4HCEIT/ , nceit2/4HBL  /
   DATA nlblt1/4HLBLT/ , nlblt2/4HBL  /
   DATA declar/4HBEGI , 4HLABE , 4HFILE/
   DATA fparam/4HTAPE , 4HAPPE , 4HSAVE/
   DATA namtbl/4HXTIM , 4HE    , 4HXSAV , 4HE    , 4HXUOP , 4H     , 4HXCHK , 4H     , 4HXPUR , 4HGE   , 4HXEQU , 4HIV  /
   DATA nskip/10*0/ , cdcomp/4HCOMP , 4HON   , 4HOFF /
!
!     INITIALIZE
!
   ifirst = 0
   osbot = 1
   nwords = 0
   lookup = 0
   preflg = 0
   ivrept = 0
   ilevel = 0
   skip = .FALSE.
   ospnt = osbot
   oscar(osbot) = 0
   oscar(osbot+1) = 1
!
!     FOR RESTART ALLOW CHECKPOINT AND JUMP ENTRIES TO BE INSERTED IN
!     OSCAR BY XGPI.
!
   IF ( start/=icst ) oscar(osbot+1) = 3
!
!     ALLOCATE 50 WORDS IN OPEN CORE FOR LBLTBL AND SET LBLTBL
!     PARAMETERS.
!
   lblbot = loscar
   lbltop = loscar - 50
   loscar = lbltop - 1
   lstlbl = lbltop - 4
   lstpar = lblbot + 1
!
!     INITIALIZE DMPCRD ARRAY FOR RIGID FORMAT
!
   icrdtp = loscar
!
!     ****************************************
!     PREPARE TO PROCESS NEXT DMAP INSTRUCTION
!     ****************************************
!
 100  dmpcnt = dmpcnt + 1
   IF ( iapp/=idmapp ) THEN
      medpnt = med(medtp+1)*(dmpcnt-1) + medtp + 2
      IF ( med(medtp)<dmpcnt .AND. iapp/=idmapp ) THEN
!
!     DMAP SEQUENCE DOES NOT CORRESPOND TO MED TABLE
!
         CALL xgpidg(39,0,0,0)
!
!     RETURN WHEN XGPI HAS BEEN DISASTERED.
!
         nogo = 2
         GOTO 99999
      ENDIF
   ENDIF
   newcrd = -1
   insert = 0
!
!     SEE IF DMAP INSTRUCTION IS TO BE DELETED OR INSERTED
!
   IF ( alter(1)/=0 .AND. alter(1)<=dmpcnt ) THEN
      IF ( alter(1)<=dmpcnt .AND. alter(2)>=dmpcnt ) THEN
!
!     SET INSERT FLAG TO NO PRINT
!
         insert = -2
         GOTO 400
      ELSE
         IF ( alter(2)/=0 ) THEN
!
!     JUST FINISHED DELETING, SET INSERT AND ALTER FOR INSERTING
!
            alter(1) = alter(2)
            alter(2) = 0
         ENDIF
         IF ( alter(1)==dmpcnt-1 ) THEN
            insert = 1
            dmpcnt = dmpcnt - 1
            GOTO 200
         ENDIF
      ENDIF
   ENDIF
!
!     GET NEXT DMAP INSTRUCTION
!     FOR RIGID FORMAT SEE IF OSCAR ENTRY IS PART OF SUBSET
!
   IF ( iapp/=idmapp ) THEN
      i = med(medtp+1)
      DO j = 1 , i
         k = medpnt + j - 1
         IF ( med(k)/=0 ) GOTO 200
      ENDDO
      insert = -2
      GOTO 400
   ENDIF
!
!     CHECK FOR CONDITIONAL COMPILATION END
!
 200  IF ( ilevel>0 ) THEN
      DO i = 1 , ilevel
         IF ( iabs(nskip(i,1))<99999 ) nskip(i,1) = nskip(i,1) - 1
      ENDDO
      IF ( nskip(ilevel,1)==-1 ) THEN
         skip = .FALSE.
         ilevel = ilevel - 1
      ELSE
         IF ( skip ) insert = insert - 2
      ENDIF
   ENDIF
!
   IF ( lookup==1 .AND. preflg/=0 ) THEN
      preflg = -preflg
      CALL autock(ospnt)
   ENDIF
   modnam = 1
   lookup = 0
   CALL xscndm
   modnam = 0
   IF ( irturn==1 .OR. irturn==3 ) THEN
!
!     NO MACRO INSTRUCTION NAME ON DMAP CARD.
!
      CALL xgpidg(12,0,dmpcnt,0)
   ELSEIF ( irturn==4 ) THEN
      GOTO 100
   ELSEIF ( irturn==5 ) THEN
      GOTO 3700
   ELSEIF ( .NOT.skip ) THEN
!
!     FIND MPL ENTRY AND BRANCH ON TYPE
!
      mplpnt = 1
      modidx = 1
      IF ( dmap(dmppnt)==prechk(1) .AND. dmap(dmppnt+1)==prechk(2) ) THEN
         DO
!
!     PROCESS PRECHK CARD
!
            index = 3
            CALL xscndm
            IF ( irturn==1 .OR. irturn==3 .OR. irturn==4 .OR. irturn==5 ) THEN
!
!     DMAP FORMAT ERROR
!
               CALL xgpidg(16,ospnt,0,0)
               EXIT
!
!     TEST FOR  ALL  OPTION OR BLANK
!
            ELSEIF ( dmap(dmppnt)/=nblank ) THEN
               preflg = 1
               nnames = 0
               IF ( dmap(dmppnt)==namopt(23) ) GOTO 3200
               IF ( dmap(dmppnt)/=nend ) GOTO 3100
               preflg = 0
               GOTO 3300
            ENDIF
         ENDDO
      ELSEIF ( dmap(dmppnt)==xdmap(1) .AND. dmap(dmppnt+1)==xdmap(2) ) THEN
!
!     PROCESS XDMAP INSTRUCTION
!
         iold = diag14
         DO
            CALL xscndm
            IF ( irturn==1 .OR. irturn==3 ) THEN
               CALL xgpidg(16,ospnt,0,0)
               EXIT
            ELSEIF ( irturn==2 ) THEN
               IF ( dmap(dmppnt)==nblank ) CYCLE
!
!     HAVE LOCATED AN XDMAP OPTION
!
               DO k = 1 , 22 , 2
                  IF ( dmap(dmppnt)==namopt(k) .AND. dmap(dmppnt+1)==namopt(k+1) ) GOTO 205
               ENDDO
!
!     ILLEGAL OPTION ON XDMAP CARD
!
               CALL xgpidg(56,0,0,0)
               EXIT
 205           kk = k/2 + 1
               IF ( kk==1 .OR. kk==9 .OR. kk==11 ) THEN
               ELSEIF ( kk==3 ) THEN
!
!     CODE TO PROCESS  ERR  OPTION
!
                  CALL xscndm
                  IF ( irturn==2 .OR. irturn==3 .OR. irturn==4 ) THEN
                     CALL xgpidg(16,ospnt,0,0)
                     EXIT
                  ELSEIF ( irturn==5 ) THEN
                     GOTO 3700
                  ELSEIF ( dmap(dmppnt+1)/=iequl ) THEN
                     CALL xgpidg(16,ospnt,0,0)
                     EXIT
                  ELSE
                     CALL xscndm
                     IF ( irturn==1 .OR. irturn==2 .OR. irturn==4 ) THEN
                        CALL xgpidg(16,ospnt,0,0)
                        EXIT
                     ELSEIF ( irturn==5 ) THEN
                        GOTO 3700
                     ELSE
                        iflg(2) = dmap(dmppnt+1)
                        IF ( iflg(2)<0 .OR. iflg(2)>2 ) THEN
                           CALL xgpidg(56,0,0,0)
                           EXIT
                        ENDIF
                     ENDIF
                  ENDIF
               ELSEIF ( kk==4 ) THEN
                  IF ( diag14/=1 ) THEN
                     iflg(3) = 1
                     diag14 = 2
                  ENDIF
               ELSEIF ( kk==5 ) THEN
                  IF ( diag14/=1 ) THEN
                     iflg(3) = 0
                     diag14 = 0
                  ENDIF
               ELSEIF ( kk==6 ) THEN
                  IF ( diag17/=1 ) THEN
                     iflg(4) = 1
                     diag17 = 2
                  ENDIF
               ELSEIF ( kk==7 ) THEN
                  IF ( diag17/=1 ) THEN
                     iflg(4) = 0
                     diag17 = 0
                  ENDIF
               ELSEIF ( kk==8 ) THEN
                  IF ( diag25/=1 ) THEN
                     iflg(5) = 1
                     diag25 = 1
                  ENDIF
               ELSEIF ( kk==10 ) THEN
                  IF ( diag4/=1 ) THEN
                     iflg(6) = 1
                     diag4 = 1
                  ENDIF
               ELSE
                  iflg(1) = 0
               ENDIF
            ELSEIF ( irturn==5 ) THEN
               GOTO 3700
            ELSE
               index = 2
               IF ( iold==0 .OR. ifirst==0 ) GOTO 1700
               IF ( start/=icst ) WRITE (optape,99001) iplus , iplus
99001          FORMAT (A1,2X,A1)
               EXIT
            ENDIF
         ENDDO
      ELSEIF ( dmap(dmppnt)==cdcomp(1) .AND. (dmap(dmppnt+1)==cdcomp(2) .OR. dmap(dmppnt+1)==cdcomp(3)) ) THEN
!
!     PROCESS CONDCOMP INSTRUCTION
!
         IF ( ilevel>=5 ) THEN
            CALL xgpidg(16,ospnt,0,0)
         ELSE
            ion = 0
            IF ( dmap(dmppnt+1)==cdcomp(2) ) ion = 1
            CALL xscndm
            IF ( irturn==1 .OR. irturn==4 ) THEN
               CALL xgpidg(16,ospnt,0,0)
            ELSEIF ( irturn==3 ) THEN
!
!     INSTRUCTION COUNT GIVEN FOR END
!
               IF ( dmap(dmppnt+1)<0 ) THEN
                  CALL xgpidg(16,ospnt,0,0)
               ELSE
                  nskip(ilevel+1,1) = dmap(dmppnt+1)
                  GOTO 3400
               ENDIF
            ELSEIF ( irturn==5 ) THEN
               GOTO 3700
            ELSE
!
!     LABEL SPECIFIED FOR END
!
               nskip(ilevel+1,1) = dmap(dmppnt)
               nskip(ilevel+1,2) = dmap(dmppnt+1)
               GOTO 3400
            ENDIF
         ENDIF
      ELSE
         DO WHILE ( mpl(mplpnt+1)/=dmap(dmppnt) .OR. mpl(mplpnt+2)/=dmap(dmppnt+1) )
!
!     CHECK FOR ERROR IN MPL TABLE
!
            IF ( mpl(mplpnt)<1 .OR. mpl(mplpnt)>lmpl ) THEN
               CALL xgpidg(49,0,0,0)
               nogo = 2
               GOTO 99999
            ELSE
               mplpnt = mpl(mplpnt) + mplpnt
               modidx = 1 + modidx
               IF ( mplpnt>=lmpl ) THEN
!
!     NO MPL ENTRY FOR THIS DMAP MACRO INSTRUCTION
!
                  CALL xgpidg(13,0,dmppnt,dmpcnt)
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
!
!     GET FORMAT TYPE FROM MPL AND BRANCH
!
         i = mpl(mplpnt+3)
         IF ( i<1 .OR. i>5 ) THEN
!
!     MPL TABLE INCORRECT
!
            CALL xgpidg(49,0,0,0)
            nogo = 2
            GOTO 99999
         ELSEIF ( i==1 .OR. i==2 ) THEN
!
!     ********************************************
!     GENERATE OSCAR ENTRY WITH TYPE F OR O FORMAT
!     ********************************************
!
!     GENERATE LINK HEADER SECTION
!
            CALL xlnkhd
            iprcfo = ospnt
!
!     GENERATE I/P FILE SECTION
!
            CALL xipfl
!
!     DIAGNOSTIC MESSAGES -
!
!     DMAP INPUT FILE ERROR
!
            IF ( irturn==2 ) CALL xgpidg(-10,ospnt,0,0)
!
!     SAVE POINTER TO O/P FILE SECTION
!
            j = ospnt + oscar(ospnt)
!
!     GENERATE O/P FILE SECTION
!
            CALL xopfl
!
!     DMAP OUTPUT FILE ERROR
!
            IF ( irturn==2 ) CALL xgpidg(-11,ospnt,0,0)
!
!     NUMBER OF SCRATCH FILES TO OSCAR
!
            i = ospnt + oscar(ospnt)
            oscar(i) = mpl(mplpnt)
!
!     INCREMENT OSCAR WORD COUNT AND MPLPNT
!
            oscar(ospnt) = 1 + oscar(ospnt)
            mplpnt = 1 + mplpnt
!
!     GENERATE PARAMETER SECTION
!
            CALL xparam
            IF ( irturn==2 ) GOTO 3700
!
!     CONTINUE COMPILATION
!     ZERO INTERNAL CHECKPOINT FLAG IN OSCAR ENTRY FOR TYPE F ENTRY
!
            IF ( andf(oscar(ospnt+2),maskhi)/=2 ) THEN
               i = ospnt + oscar(ospnt)
               oscar(i) = 0
               oscar(ospnt) = 1 + oscar(ospnt)
            ENDIF
            IF ( nwords/=0 ) THEN
               CALL autosv
               nwords = 0
            ENDIF
            IF ( preflg/=0 .AND. istopf/=0 ) CALL autock(istopf)
         ELSEIF ( i==3 ) THEN
!
!     ***************************************
!     GENERATE OSCAR ENTRY WITH TYPE C FORMAT
!     ***************************************
!
!     GENERATE LINK HEADER SECTION
!
            CALL xlnkhd
!
!     UPDATE OSCAR ENTRY WORD COUNT TO INCLUDE VALUE SECTION.
!
            oscar(ospnt) = 7
!
!     CHECK FOR END CARD
!
            IF ( oscar(ospnt+3)==nend ) THEN
               oscar(ospnt+3) = nexit
               iendf = 1
!
!     SET EXECUTE FLAG IN OSCAR FOR END
!
               oscar(ospnt+5) = orf(isgnon,oscar(ospnt+5))
            ENDIF
!
!     GET NEXT ENTRY IN DMAP
!
            CALL xscndm
            IF ( irturn==1 ) THEN
               CALL xgpidg(16,ospnt,0,0)
            ELSEIF ( irturn==3 .OR. irturn==4 ) THEN
!
!     EXIT DMAP INSTRUCTION, SET EXECUTE FLAG AND OSCAR VALUE SECTION.
!
               IF ( oscar(ospnt+3)/=nexit ) THEN
                  CALL xgpidg(16,ospnt,0,0)
               ELSE
                  IF ( dmap(dmppnt)/=intgr ) dmap(dmppnt+1) = 0
                  dmap(dmppnt) = intgr
                  dmap(dmppnt+2) = rshift(iallon,1)
                  GOTO 900
               ENDIF
            ELSEIF ( irturn==5 ) THEN
               GOTO 3700
            ELSE
!
!     IF NEXT DMAP ENTRY IS BCD IT SHOULD BE LABEL NAME FOR BRANCH
!     DMAP INSTRUCTION.
!
               IF ( oscar(ospnt+3)==nexit ) THEN
                  CALL xgpidg(16,ospnt,0,0)
                  GOTO 300
               ELSE
!
!     SEARCH LABEL TABLE FOR LABEL NAME
!
                  IF ( lstlbl>=lbltop ) THEN
                     DO j = lbltop , lstlbl , 4
                        IF ( dmap(dmppnt)==lbltbl(j) .AND. dmap(dmppnt+1)==lbltbl(j+1) ) GOTO 210
                     ENDDO
                  ENDIF
!
!     NAME NOT FOUND IN TABLE
!
                  ldef = 0
                  GOTO 500
               ENDIF
!
!     NOW SEE IF LABEL HAS BEEN REFERENCED
!
 210           IF ( lbltbl(j+3)==0 ) GOTO 700
               ldef = lbltbl(j+2)
               GOTO 500
            ENDIF
         ELSEIF ( i==4 ) THEN
!
!     ***************************************
!     GENERATE OSCAR ENTRY WITH TYPE E FORMAT
!     ***************************************
!
!     PREFIX MODULE NAME WITH AN X
!
            DO i = 1 , 6
               IF ( ntypee(i)==dmap(dmppnt) ) EXIT
            ENDDO
            i = 2*i - 1
            dmap(dmppnt) = namtbl(i)
            dmap(dmppnt+1) = namtbl(i+1)
!
!     GENERATE LINK HEADER FOR OSCAR
!
            IF ( i==9 .OR. i==11 ) lookup = 1
            os2b4 = osprc
            CALL xlnkhd
!
!     BRANCH ON DMAP NAME AND GENERATE VALUE/OUTPUT SECTION OF OSCAR
!
            i = (i+1)/2
            IF ( i==2 ) THEN
!
!     XSAVE ENTRY, ENTER POINTERS IN VALUE SECTION OF OSCAR.
!
               i = ospnt + oscar(ospnt)
               oscar(i) = 0
               k = i - 1
               DO
!
!     GET PARAMETER NAME FROM DMAP.
!
                  CALL xscndm
                  IF ( irturn==1 .OR. irturn==3 ) THEN
!
!     ILLEGAL CHARACTERS IN DMAP SAVE PARAMETER NAME LIST
!
                     CALL xgpidg(20,ospnt,oscar(i)+1,0)
                  ELSEIF ( irturn==4 ) THEN
!
!
!     END OF SAVE PARAMETER NAME LIST, INCREMENT OSCAR WORD COUNT.
!
                     oscar(ospnt) = oscar(ospnt) + 2*oscar(i) + 1
!
!     GET PARAMETER VALUE DISPLACEMENT IN COMMON FROM PRECEDING
!     OSCAR ENTRY.
!
                     iosdav = osprc
                     IF ( oscar(osprc+3)==xchk ) osprc = os2b4
                     IF ( andf(oscar(osprc+2),maskhi)>2 ) THEN
!
!     SAVE OUT OF POSITION
!
                        CALL xgpidg(61,ospnt,0,0)
                        ospnt = iosdav
                        osprc = os2b4
                     ELSE
!
!     J = OSCAR POINTER TO BEGINNING OF PARAMETER SECTION.
!
                        j = osprc + 6 + 3*oscar(osprc+6) + 1
                        IF ( andf(oscar(osprc+2),maskhi)==1 ) j = j + 1 + 3*oscar(j)
                        j = j + 1
!
!     N1 = PARAMETER COUNT,N2=PARAMETER DISPLACEMENT IN COMMON,
!     N3 = OSCAR POINTER TO PARAMETER ENTRIES IN PRECEDING OSCAR ENTRY.
!
                        n3 = j + 1
                        n1 = oscar(j)
                        n2 = 1
!
!     SCAN PARAMETER LIST OF PRECEDING OSCAR ENTRY
!
                        DO m = 1 , n1
                           l = andf(oscar(n3),nosgn)
                           IF ( oscar(n3)>0 ) THEN
!
!     CONSTANT PARAMETER, INCREMENT N2, N3
!
                              n3 = n3 + l + 1
                              GOTO 216
                           ELSE
                              n3 = n3 + 1
!
!     VARIABLE PARAMETER, COMPARE VPS POINTER WITH XSAVE VPS POINTERS.
!
                              i1 = i + 1
                              DO k1 = i1 , k , 2
                                 IF ( oscar(k1)==l ) GOTO 212
                              ENDDO
                              GOTO 214
                           ENDIF
 212                       oscar(k1+1) = n2
 214                       l = andf(vps(l-1),maskhi)
 216                       n2 = n2 + l
!
!     PARAMETER SECTION SCANNED, CHECK EXSAVE PARAMETER LIST FOR
!     PARAMETERS NOT FOUND IN PRECEDING OSCAR.
!
                        ENDDO
!
!     CHECK FOR XSAVE PARAMETERS NOT ON PRECEDING DMAP CARD
!
                        i1 = i + 2
                        k = k + 1
                        DO k1 = i1 , k , 2
                           IF ( oscar(k1)<=0 .AND. oscar(k1-1)/=0 ) THEN
                              j = oscar(k1-1)
                              CALL xgpidg(21,ospnt,vps(j-3),vps(j-2))
                           ENDIF
                        ENDDO
                     ENDIF
                     EXIT
                  ELSEIF ( irturn==5 ) THEN
                     GOTO 3700
                  ELSE
!
!     FIND PARAMETER IN VPS AND ENTER POINTER TO VALUE IN OSCAR.
!
                     k = k + 2
                     oscar(i) = oscar(i) + 1
                     oscar(k) = 0
                     oscar(k+1) = 0
                     j = 3
                     DO WHILE ( vps(j)/=dmap(dmppnt) .OR. vps(j+1)/=dmap(dmppnt+1) )
                        l = andf(vps(j+2),maskhi)
                        j = j + l + 3
                        IF ( j>=vps(2) ) THEN
!
!     XSAVE PARAMETER NAME NOT ON PRECEDING DMAP CARD
!
                           CALL xgpidg(21,ospnt,dmap(dmppnt),dmap(dmppnt+1))
                           GOTO 220
!
!     PARAMETER NOT IN VPS - ERROR
!
                        ENDIF
                     ENDDO
!
!     PARAMETER FOUND IN VPS
!
                     oscar(k) = j + 3
!
!     SEE IF PARAMETER WAS ALREADY SAVED
!
                     j = i + 1
                     j1 = k - 2
                     IF ( j1>=j ) THEN
                        DO l = j , j1 , 2
                           IF ( oscar(l)==oscar(k) ) GOTO 218
                        ENDDO
                     ENDIF
                     CYCLE
!
!     PARAMETER DUPLICATED
!
 218                 k = k - 2
                     oscar(i) = oscar(i) - 1
!
!     DUPLICATE PARAMETER NAMES (WARNING)
!
                     CALL xgpidg(-2,ospnt,dmap(dmppnt),dmap(dmppnt+1))
                  ENDIF
 220           ENDDO
            ELSEIF ( i==3 .OR. i==4 .OR. i==5 .OR. i==6 ) THEN
               GOTO 1000
            ELSE
!
!     EXTIME ENTRY, CHECK ESTIM IN CONTROL FILE
!
               oscar(ospnt+5) = andf(oscar(ospnt+5),nosgn)
               IF ( iestim/=0 ) THEN
!
!     GET TIME SEGMENT NAME
!
                  CALL xscndm
                  IF ( irturn==1 .OR. irturn==3 .OR. irturn==4 ) THEN
!
!     TIME SEGMENT NAME INCORRECT - WARNING ONLY
!
                     CALL xgpidg(-17,ospnt,0,0)
                  ELSEIF ( irturn==5 ) THEN
                     GOTO 3700
                  ELSE
                     i = iestim + ictlfl(iestim) - 1
                     j = iestim + 1
                     DO k = j , i , 2
                        IF ( dmap(dmppnt)==ictlfl(k) .AND. dmap(dmppnt+1)==ictlfl(k+1) ) oscar(ospnt+5) = orf(oscar(ospnt+5),isgnon)
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF ( i==5 ) THEN
!
!     *******************************
!     DMAP INSTRUCTION IS DECLARATIVE
!     *******************************
!
!     PUT DUMMY ENTRY IN OSCAR FOR DIAGNOSTIC USE.
!
            j = osbot + oscar(osbot)
            oscar(j+3) = dmap(dmppnt)
            oscar(j+4) = dmap(dmppnt+1)
            oscar(j+5) = dmpcnt
            CALL xlnkhd
!
!     NOW PROCESS INSTRUCTION
!
            DO j = 1 , 3
               IF ( dmap(dmppnt)==declar(j) ) THEN
                  IF ( j==1 ) EXIT
                  IF ( j==2 ) GOTO 2000
                  IF ( j==3 ) GOTO 2400
               ENDIF
            ENDDO
!
!     BEGIN DECLARATIVE - PREPARE TO PROCESS NEXT DMAP INSTRUCTION
!
            index = 1
            GOTO 1700
         ENDIF
      ENDIF
   ELSE
!
!     CHECK LABELS EVEN IF CONDITIONAL COMPILATION
!
      IF ( dmap(dmppnt)/=declar(2) ) GOTO 400
      GOTO 2000
   ENDIF
!
!     *****************************************************
!     RETURN HERE AFTER DMAP INSTRUCTION HAS BEEN PROCESSED
!     *****************************************************
!
!     CHECK FOR FATAL ERROR
!
 300  IF ( nogo==2 ) GOTO 3700
!
!     CHECK FOR END OF DMAP SEQUENCE.
!
   IF ( iendf/=0 ) THEN
!
!     ***********************************************************
!     DMAP INSTRUCTIONS ALL PROCESSED - PREPARE OSCAR FOR PHASE 2
!     ***********************************************************
!
!     CHECK FOR DISCREPENCY BETWEEN RIGID FORMAT AND MED TABLE.
!
      IF ( med(medtp)/=dmpcnt .AND. iapp/=idmapp ) THEN
         CALL xgpidg(39,0,0,0)
         nogo = 2
         GOTO 99999
      ELSE
!
!     USE LBLTBL PARAMETER NAMES TO UPDATE VALUE SECTIONS OF TYPE C AND
!     E OSCAR ENTRIES.
!
         DO WHILE ( lstpar<lblbot )
!
!     FIND PARAMETER NAME IN VPS
!
            k = 3
            DO WHILE ( lbltbl(lstpar)/=vps(k) .OR. lbltbl(lstpar+1)/=vps(k+1) )
               k = k + andf(vps(k+2),maskhi) + 3
               IF ( k>=vps(2) ) THEN
!
!     SEARCH PVT TABLE FOR PARAMETER. IF FOUND ENTER PARAMETER IN VPS.
!
                  k1 = 3
                  DO
                     length = andf(pvt(k1+2),nosgn)
                     length = itype(length)
                     IF ( lbltbl(lstpar)==pvt(k1) .AND. lbltbl(lstpar+1)==pvt(k1+1) ) THEN
                        k = vps(2) + 1
                        pvt(k1+2) = orf(pvt(k1+2),isgnon)
                        vps(2) = k + 2 + length
                        IF ( vps(2)>=vps(1) ) THEN
!
!     VPS TABLE OVERFLOWED
!
                           CALL xgpidg(14,nvps,nblank,0)
                           nogo = 2
                           GOTO 99999
                        ELSE
                           k2 = length + 3
                           DO m = 1 , k2
                              j = k + m - 1
                              j1 = k1 + m - 1
                              vps(j) = pvt(j1)
                           ENDDO
                           GOTO 310
                        ENDIF
                     ELSE
                        k1 = k1 + length + 3
                        IF ( k1>=pvt(2) ) THEN
!
!     PARAMETER NOT DEFINED FOR USE IN COND, PURGE OR EQUIV INSTRUCTIONS
!
                           CALL xgpidg(25,lbltbl(lstpar+3),lbltbl(lstpar),lbltbl(lstpar+1))
!
!     GET NEXT ENTRY FROM LBLTBL
!
                           lstpar = lstpar + 4
                           GOTO 320
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
!
!     NAME FOUND IN VPS, VPS POINTER TO OSCAR VALUE SECTION.
!
 310        i = lbltbl(lstpar+2)
            oscar(i) = k + 3
            lstpar = lstpar + 4
 320     ENDDO
!
!     USE LBLTBL ENTRIES TO LOAD SEQUENCE NOS. INTO VALUE SECTION OF
!     TYPE C OSCAR ENTRIES.
!
         lblerr = 0
         lstlsv = lstlbl
         GOTO 3500
      ENDIF
   ENDIF
 400  DO
!
!     CHECK FOR $ ENTRY IN DMAP AND GET NEXT DMAP INSTRUCTION
!
      CALL xscndm
      IF ( irturn==4 ) GOTO 100
      IF ( irturn==5 ) GOTO 3700
      IF ( nogo==0 .AND. insert>=0 ) THEN
         CALL xgpidg(16,ospnt,0,0)
         GOTO 300
      ENDIF
   ENDDO
!
!     MAKE NEW ENTRY IN LABEL TABLE, CHECK FOR TABLE OVERFLOW
!
 500  ASSIGN 600 TO irturn
   IF ( lstlbl+8>=lstpar ) GOTO 3900
 600  lstlbl = lstlbl + 4
   j = lstlbl
   lbltbl(j) = dmap(dmppnt)
   lbltbl(j+1) = dmap(dmppnt+1)
   lbltbl(j+2) = ldef
 700  lbltbl(j+3) = ospnt
!
!     GET NEXT ENTRY FROM DMAP, ENTRY IS $ FOR JUMP,NAME FOR COND,
!     VALUE FOR REPT.
!
   CALL xscndm
   IF ( irturn==1 ) THEN
      CALL xgpidg(16,ospnt,0,0)
      GOTO 300
   ELSEIF ( irturn==2 ) THEN
!
!     COND DMAP INSTRUCTION, ENTER PARAMETER NAME IN LABEL TABLE.
!
      IF ( oscar(ospnt+3)==nrept ) THEN
         ivrept = 1
         GOTO 900
      ELSEIF ( oscar(ospnt+3)/=ncond ) THEN
         CALL xgpidg(16,ospnt,0,0)
         GOTO 300
      ELSE
         ASSIGN 800 TO irturn
         IF ( lstpar-8<=lstlbl ) GOTO 3900
      ENDIF
   ELSEIF ( irturn==3 ) THEN
!
!     REPT DMAP INSTRUCTION, COUNT TO VALUE SECTION.
!
      IF ( oscar(ospnt+3)==nrept ) GOTO 900
      CALL xgpidg(16,ospnt,0,0)
      GOTO 300
   ELSEIF ( irturn==5 ) THEN
      GOTO 3700
   ELSE
!
!     DMAP INSTRUCTION IS JUMP
!
      oscar(ospnt+6) = 0
      IF ( oscar(ospnt+3)/=njump ) CALL xgpidg(16,ospnt,0,0)
      GOTO 300
   ENDIF
 800  lstpar = lstpar - 4
   lbltbl(lstpar) = dmap(dmppnt)
   lbltbl(lstpar+1) = dmap(dmppnt+1)
   lbltbl(lstpar+2) = ospnt + 6
   lbltbl(lstpar+3) = ospnt
   GOTO 300
!
!     ENTER LOOP COUNT IN CEITBL FOR REPT AND EXIT INSTRUCTIONS
!
 900  ceitbl(2) = ceitbl(2) + 4
   IF ( ceitbl(2)>ceitbl(1) ) THEN
!
!     CEITBL OVERFLOW, DISCONTINUE COMPILATION
!
      CALL xgpidg(14,nceit1,nceit2,dmpcnt)
      nogo = 2
      GOTO 99999
   ELSE
!
!     I = POINTER TO LOOP COUNT IN CEITBL ENTRY
!
      i = ceitbl(2) - 2
      IF ( ivrept==0 ) THEN
         ceitbl(i) = lshift(dmap(dmppnt+1),16)
      ELSE
!
!     PROCESS VARIABLE REPT INSTRUCTION - FIND PARAM IN VPS
!
         kdh = 3
         DO WHILE ( dmap(dmppnt)/=vps(kdh) .OR. dmap(dmppnt+1)/=vps(kdh+1) )
            kdh = kdh + andf(vps(kdh+2),maskhi) + 3
            IF ( kdh>=vps(2) ) THEN
!
!     CHECK PVT FOR PARAMETER
!
               kdh = 3
               DO
                  length = andf(pvt(kdh+2),nosgn)
                  length = itype(length)
                  IF ( dmap(dmppnt)/=pvt(kdh) .OR. dmap(dmppnt+1)/=pvt(kdh+1) ) THEN
                     kdh = kdh + length + 3
                     IF ( kdh>=pvt(2) ) THEN
!
!     VARIABLE REPT INSTRUCTION ERRORS
!
                        CALL xgpidg(58,0,0,0)
                        GOTO 300
                     ENDIF
                  ELSEIF ( length/=itype(1) ) THEN
                     CALL xgpidg(57,0,0,0)
                     GOTO 300
                  ELSE
                     ceitbl(i) = lshift(pvt(kdh+3),16)
                     GOTO 950
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
!
!     PARAMETER FOUND
!
         IF ( andf(rshift(vps(kdh+2),16),15)/=1 ) THEN
            CALL xgpidg(57,0,0,0)
            GOTO 300
         ELSE
            ceitbl(i) = lshift(kdh,16)
            ceitbl(i) = orf(ceitbl(i),isgnon)
         ENDIF
      ENDIF
!
!     FIRST WORD OF CEITBL ENTRY CONTAINS OSCAR RECORD NUMBERS OF
!     BEGINNING AND END OF LOOP
!
 950  ceitbl(i-1) = iseqn
      ivrept = 0
!
!     OSCAR VALUE SECTION CONTAINS POINTER TO LOOP COUNT IN CEITBL ENTRY
!
      oscar(ospnt+6) = i
      GOTO 300
   ENDIF
!
!     XUOP,XCHK,XPURGE,OR XEQUIV OSCAR ENTRY - GENERATE FILE NAME LIST.
!
 1000 nospnt = ospnt + oscar(ospnt)
   iprime = 1
   iospnt = nospnt + 1
   oscar(nospnt) = 0
 1100 DO
!
!     GET NEXT ENTRY FROM DMAP CARD
!
      CALL xscndm
      IF ( irturn==1 ) THEN
!
!     DMAP ENTRY IS OPERATOR, CHECK FOR / OPERATOR
!
         IF ( (dmap(dmppnt+1)/=islsh) .OR. (oscar(ospnt+3)/=nxequi .AND. oscar(ospnt+3)/=nxpurg) ) THEN
            CALL xgpidg(16,ospnt,0,0)
            GOTO 300
         ELSE
!
!     OSCAR ENTRY IS XEQUIV OR XPURGE
!
            varflg = 0
            IF ( oscar(ospnt+3)==nxpurg ) GOTO 1300
            IF ( oscar(nospnt)>=2 ) GOTO 1300
            CALL xgpidg(16,ospnt,0,0)
            GOTO 300
         ENDIF
      ELSEIF ( irturn==3 ) THEN
         CALL xgpidg(16,ospnt,0,0)
         GOTO 300
      ELSEIF ( irturn==4 ) THEN
         GOTO 1500
      ELSEIF ( irturn==5 ) THEN
         GOTO 3700
      ELSE
!
!     DMAP ENTRY IS DATA BLOCK NAME, STORE IN OSCAR
!
         oscar(iospnt) = dmap(dmppnt)
         oscar(iospnt+1) = dmap(dmppnt+1)
!
!     MAKE SURE FILE IS NOT BLANK
!
         IF ( oscar(iospnt)/=nblank ) THEN
!
!     FOR CHKPNT - MAKE SURE FILE IS NOT OUTPUT BY USER I/P PROCESSOR
!
            IF ( oscar(ospnt+3)==namtbl(7) ) THEN
               m = fiat(3)*icfiat - 2
               DO j = 4 , m , icfiat
                  IF ( oscar(iospnt)==fiat(j+1) .AND. oscar(iospnt+1)==fiat(j+2) ) GOTO 4100
               ENDDO
            ENDIF
            EXIT
         ENDIF
      ENDIF
   ENDDO
 1200 iospnt = iospnt + 2
   oscar(nospnt) = 1 + oscar(nospnt)
!
!     INSERT EXTRA WORD INTO OSCAR FOR EACH PRIMARY DATA BLOCK IN
!     EQUIV STATEMENT
!
   IF ( oscar(ospnt+3)==namtbl(11) .AND. oscar(ospnt+4)==namtbl(12) ) THEN
      IF ( iprime/=0 ) THEN
         oscar(iospnt) = 0
         iospnt = iospnt + 1
         iprime = 0
      ENDIF
   ENDIF
   GOTO 1100
!
!     GET PARAMETER NAME AND ENTER INTO LBLTBL
!
 1300 CALL xscndm
   IF ( irturn==1 ) GOTO 1600
   IF ( irturn==3 .OR. irturn==4 ) THEN
      CALL xgpidg(16,ospnt,0,0)
      GOTO 300
   ELSEIF ( irturn==5 ) THEN
      GOTO 3700
   ELSE
      varflg = 1
      IF ( dmap(dmppnt)==nblank ) THEN
         CALL xscndm
         IF ( irturn==2 .OR. irturn==3 .OR. irturn==4 ) THEN
            CALL xgpidg(16,ospnt,0,0)
            GOTO 300
         ELSEIF ( irturn==5 ) THEN
            GOTO 3700
         ELSE
            GOTO 1600
         ENDIF
      ELSE
         ASSIGN 1400 TO irturn
         IF ( lstpar-8<=lstlbl ) GOTO 3900
      ENDIF
   ENDIF
 1400 lstpar = lstpar - 4
   lbltbl(lstpar) = dmap(dmppnt)
   lbltbl(lstpar+1) = dmap(dmppnt+1)
   lbltbl(lstpar+2) = iospnt
   lbltbl(lstpar+3) = ospnt
   idlhss = 2*oscar(nospnt) + oscar(ospnt) + 2
   IF ( oscar(ospnt+3)==namtbl(11) ) idlhss = idlhss + 1
   oscar(ospnt) = idlhss
!
!     CHECK FOR POSSIBILITY OF ANOTHER DATA BLOCK NAME LIST.
!
   CALL xscndm
   IF ( irturn==1 ) GOTO 1000
   IF ( irturn==2 .OR. irturn==3 ) THEN
      CALL xgpidg(16,ospnt,0,0)
   ELSEIF ( irturn==4 ) THEN
   ELSEIF ( irturn==5 ) THEN
      GOTO 3700
   ELSE
      GOTO 1500
   ENDIF
   GOTO 300
!
!     END OF DMAP INSTRUCTION, INCREMENT OSCAR WORD COUNT IF NOT XEQUIV
!     OR XPURGE.
!
 1500 IF ( oscar(ospnt+3)/=nxequi .AND. oscar(ospnt+3)/=nxpurg ) THEN
      oscar(ospnt) = 2*oscar(nospnt) + oscar(ospnt) + 1
!
!     ELIMINATE ENTRY IF NOTHING CHECKPOINTED.
!
      IF ( oscar(nospnt)==0 ) osbot = osprc
   ELSE
      oscar(iospnt) = -1
      idlhss = 2*oscar(nospnt) + oscar(ospnt) + 2
      IF ( oscar(ospnt+3)==namtbl(11) ) idlhss = idlhss + 1
      oscar(ospnt) = idlhss
   ENDIF
   GOTO 300
 1600 IF ( (dmap(dmppnt+1)/=islsh) .OR. (oscar(ospnt+3)/=nxequi .AND. oscar(ospnt+3)/=nxpurg) ) THEN
      CALL xgpidg(16,ospnt,0,0)
      GOTO 300
   ELSE
      oscar(iospnt) = -1
      idlhss = 2*oscar(nospnt) + oscar(ospnt) + 2
      IF ( oscar(ospnt+3)==namtbl(11) ) idlhss = idlhss + 1
      oscar(ospnt) = idlhss
      GOTO 1000
   ENDIF
 1700 IF ( ifirst<=0 ) THEN
      IF ( diag14/=0 .OR. diag17/=0 ) THEN
         ifirst = 1
         CALL xgpimw(5,18,dmpcnt,ibuff)
         IF ( start/=icst ) CALL xgpimw(10,0,0,0)
      ENDIF
   ENDIF
 1800 IF ( index>1 ) GOTO 300
 1900 DO
      CALL xscndm
      IF ( irturn==1 .OR. irturn==2 .OR. irturn==3 ) THEN
      ELSEIF ( irturn==4 ) THEN
         GOTO 300
      ELSEIF ( irturn==5 ) THEN
         GOTO 3700
      ELSE
         EXIT
      ENDIF
   ENDDO
!
!     LABEL DECLARATIVE - GET LABEL NAME
!
 2000 CALL xscndm
   IF ( irturn==1 .OR. irturn==3 .OR. irturn==4 ) GOTO 3800
   IF ( irturn==5 ) GOTO 3700
!
!     CHECK IF LABEL IS FOR CONDITIONAL COMPILATION
!
   IF ( dmap(dmppnt)/=nskip(ilevel,1) .OR. dmap(dmppnt+1)/=nskip(ilevel,2) ) THEN
      IF ( skip ) GOTO 300
!
!     SCAN LABEL TABLE FOR LABEL NAME
!
      IF ( lstlbl>=lbltop ) THEN
         DO j = lbltop , lstlbl , 4
            IF ( dmap(dmppnt)==lbltbl(j) .AND. dmap(dmppnt+1)==lbltbl(j+1) ) GOTO 2300
         ENDDO
      ENDIF
!
!     NAME NOT IN LABEL TABLE, MAKE NEW ENTRY
!
      ASSIGN 2100 TO irturn
      IF ( lstlbl+8>=lstpar ) GOTO 3900
   ELSE
      ilevel = ilevel - 1
      skip = .FALSE.
      GOTO 300
   ENDIF
 2100 lstlbl = lstlbl + 4
   j = lstlbl
   lbltbl(j) = dmap(dmppnt)
   lbltbl(j+1) = dmap(dmppnt+1)
   lbltbl(j+3) = 0
 2200 lbltbl(j+2) = iseqn + 1
   GOTO 300
!
!     LABEL NAME FOUND IN LABEL TABLE, DEF ENTRY SHOULD BE ZERO
!
 2300 IF ( lbltbl(j+2)==0 ) GOTO 2200
!
!     LABEL IS MULTIPLY DEFINED
!
   CALL xgpidg(19,dmpcnt,dmppnt,0)
   GOTO 300
!
!     FILE DECLARATIVE
!     SET FILE NAME FLAG
!     DO NOT PROCESS FILE DECLARATION WHEN EXECUTE FLAG IS OFF ON
!     MODIFIED RESTART.
!
 2400 IF ( start==imst .AND. oscar(ospnt+5)>=0 ) GOTO 1900
   i = 1
 2500 DO
      CALL xscndm
      IF ( irturn==2 ) THEN
!
!     NAME ENCOUNTERED - TEST FILE NAME FLAG
!
         IF ( i==0 ) THEN
!
!     FILE PARAMETER FOUND - ENTER APPROPRIATE CODE IN FILE TABLE
!
            DO j = 1 , 3
               IF ( dmap(dmppnt)==fparam(j) ) THEN
                  IF ( j==1 ) GOTO 2700
                  IF ( j==2 ) GOTO 2800
                  IF ( j==3 ) GOTO 2900
               ENDIF
            ENDDO
            CALL xgpidg(16,ospnt,0,0)
            GOTO 300
         ELSE
!
!     FILE NAME - ENTER IN FILE TABLE
!
            fpnt = fpnt + 3
            IF ( fpnt>lfile-2 ) THEN
!
!     OVERFLOWED FILE TABLE
!
               CALL xgpidg(14,nfile,nblank,0)
               nogo = 2
               GOTO 99999
            ELSE
               file(fpnt) = dmap(dmppnt)
               file(fpnt+1) = dmap(dmppnt+1)
!
!     PUT FILE NAME INTO LABEL TABLE FOR DMAP XREF
!
               ASSIGN 2600 TO irturn
               IF ( lstlbl+8<lstpar ) EXIT
               GOTO 3900
            ENDIF
         ENDIF
      ELSEIF ( irturn==3 ) THEN
         GOTO 3800
      ELSEIF ( irturn==4 ) THEN
         GOTO 300
      ELSEIF ( irturn==5 ) THEN
         GOTO 3700
!
!     DELIMITER ENCOUNTERED
!
      ELSEIF ( dmap(dmppnt+1)==islsh ) THEN
!
!     DELIMITER IS /, TEST FILE NAME FLAG
!
         IF ( i/=0 ) GOTO 3800
         i = 1
      ELSE
         IF ( dmap(dmppnt+1)/=iequl ) GOTO 3800
!
!     DELIMITER IS =, TURN OFF FILE NAME FLAG
!
         i = 0
      ENDIF
   ENDDO
 2600 lstlbl = lstlbl + 4
   lbltbl(lstlbl) = file(fpnt)
   lbltbl(lstlbl+1) = file(fpnt+1)
   lbltbl(lstlbl+2) = iseqn
   lbltbl(lstlbl+3) = -1
   GOTO 2500
!
!     TAPE PARAM
!
 2700 fcode = itape
   GOTO 3000
!
!     APPEND PARAM
!
 2800 fcode = iappnd
   GOTO 3000
!
!     SAVE PARAM
!
 2900 fcode = isave
!
!     PUT CODE IN FILE TABLE
!
 3000 file(fpnt+2) = orf(file(fpnt+2),fcode)
   GOTO 2500
 3100 DO
!
!     LIST HAS BEEN FOUND, STORE IN /AUTOCM/
!
      nnames = nnames + 1
      IF ( nnames>50 ) THEN
!
!     PRECHK NAME LIST OVERFLOW
!
         CALL xgpidg(55,0,0,0)
         nogo = 2
         GOTO 99999
      ELSE
         prenam(2*nnames-1) = dmap(dmppnt)
         prenam(2*nnames) = dmap(dmppnt+1)
         CALL xscndm
         IF ( irturn==1 .OR. irturn==3 ) THEN
            CALL xgpidg(16,ospnt,0,0)
            GOTO 300
         ELSEIF ( irturn==2 ) THEN
         ELSEIF ( irturn==4 ) THEN
            GOTO 3300
         ELSEIF ( irturn==5 ) THEN
            GOTO 3700
         ELSE
            EXIT
         ENDIF
      ENDIF
   ENDDO
!
!     ALL  OPTION FOUND, LOOK FOR  EXCEPT
!
 3200 CALL xscndm
   IF ( irturn==1 .OR. irturn==3 ) THEN
      CALL xgpidg(16,ospnt,0,0)
      GOTO 300
   ELSEIF ( irturn==5 ) THEN
      GOTO 3700
   ELSEIF ( dmap(dmppnt)==namopt(25) .AND. dmap(dmppnt+1)==namopt(26) ) THEN
      preflg = 3
      CALL xscndm
      IF ( irturn==1 .OR. irturn==3 ) THEN
         CALL xgpidg(16,ospnt,0,0)
         GOTO 300
      ELSEIF ( irturn==2 ) THEN
         GOTO 3100
      ELSEIF ( irturn==4 ) THEN
      ELSEIF ( irturn==5 ) THEN
         GOTO 3700
      ELSE
         preflg = 0
      ENDIF
   ELSE
      preflg = 2
   ENDIF
 3300 IF ( icpflg/=0 ) THEN
      IF ( start/=icst ) CALL xgpimw(10,0,0,0)
      GOTO 1800
   ELSE
      preflg = 0
      GOTO 300
   ENDIF
!
!     GET LABEL AND LOOK FOR IT IN PVT
!
 3400 CALL xscndm
   IF ( irturn==1 .OR. irturn==3 .OR. irturn==4 ) THEN
      CALL xgpidg(16,ospnt,0,0)
      GOTO 300
   ELSEIF ( irturn==5 ) THEN
      GOTO 3700
   ELSE
      ilevel = ilevel + 1
      kdh = 3
      DO
         length = andf(pvt(kdh+2),nosgn)
         length = itype(length)
         IF ( dmap(dmppnt)==pvt(kdh) .AND. dmap(dmppnt+1)==pvt(kdh+1) ) THEN
!
!     CHECK IF VALUE IS FALSE
!
            pvt(kdh+2) = orf(pvt(kdh+2),isgnon)
            IF ( andf(pvt(kdh+2),nosgn)/=1 ) THEN
               CALL xgpidg(16,ospnt,0,0)
            ELSEIF ( pvt(kdh+3)>=0 .OR. ion/=1 ) THEN
               IF ( pvt(kdh+3)<0 .OR. ion/=0 ) skip = .TRUE.
            ENDIF
            GOTO 300
         ELSE
            kdh = kdh + length + 3
            IF ( kdh>=pvt(2) ) THEN
!
!     PARAMETER NOT FOUND - ASSUME FALSE VALUE
!
               IF ( ion/=0 ) skip = .TRUE.
               GOTO 300
            ENDIF
         ENDIF
      ENDDO
   ENDIF
 3500 DO WHILE ( lstlbl>=lbltop )
      IF ( lbltbl(lstlbl+2)/=0 ) GOTO 3600
!
!     CHECK FOR LABEL DEFINED
!
      DO j = lbltop , lstlbl , 4
         IF ( lbltbl(j)==lbltbl(lstlbl) .AND. lbltbl(j+1)==lbltbl(lstlbl+1) .AND. lbltbl(j+2)>0 ) GOTO 4000
      ENDDO
!
!     LABEL NOT DEFINED
!
      CALL xgpidg(26,lbltbl(lstlbl+3),lbltbl(lstlbl),lbltbl(lstlbl+1))
      nogo = 1
!
!     GET NEXT LBLTBL ENTRY.
!
      lstlbl = lstlbl - 4
   ENDDO
!
!     NORMAL RETURN -     DUMP LBLTBL ONTO SCRATCH FOR DMAP XREF
!                         THEN DELETE LBLTBL AND DMPCRD ARRARYS
!                         FROM OPEN CORE
!
   lstlbl = lstlsv
   GOTO 3700
!
!     IGNORE FILE NAMES IN LBLTBL USED FOR XREF
!
 3600 IF ( lbltbl(lstlbl+3)<0 ) THEN
   ELSEIF ( lbltbl(lstlbl+3)==0 ) THEN
!
!     LABEL NOT REFERENCED - WARNING ONLY
!
      CALL xgpidg(-27,lbltbl(lstlbl+2),lbltbl(lstlbl),lbltbl(lstlbl+1))
   ELSE
      i = lbltbl(lstlbl+3) + 6
      IF ( oscar(i-3)/=ncond .AND. oscar(i-3)/=njump ) THEN
         j = oscar(i)
!
!     LABEL NAME TO WORDS 3 AND 4 OF CEITBL ENTRY
!
         ceitbl(j+1) = lbltbl(lstlbl)
         ceitbl(j+2) = lbltbl(lstlbl+1)
!
!     OSCAR RECORD NO. OF BEGIN LOOP TO FIRST WORD OF CEITBL ENTRY
!
         ceitbl(j-1) = orf(lshift(lbltbl(lstlbl+2),16),ceitbl(j-1))
      ENDIF
      oscar(i) = orf(lshift(lbltbl(lstlbl+2),16),oscar(i))
      lstlbl = lstlbl - 4
      GOTO 3500
   ENDIF
   lstlbl = lstlbl - 4
   GOTO 3500
 3700 loscar = lblbot
   idpbuf = korsz(oscar) - 2*bufsz
   CALL close(nscr,1)
   lstlbl = lstlbl - lbltop + 4
   IF ( lstlbl<0 ) lstlbl = 0
   RETURN
 3800 j = osbot + oscar(osbot) + 6
   CALL xgpidg(16,j,0,0)
   GOTO 300
!
!     LBLTBL OVERFLOWED - ALLOCATE 50 MORE WORDS FOR IT.
!
 3900 icrdtp = icrdtp - 50
   IF ( icrdtp<oscar(osbot)+osbot ) THEN
!
!     LABEL TABLE OVERFLOW, DISCONTINUE COMPILATION
!
      CALL xgpidg(14,nlblt1,nlblt2,dmpcnt)
      nogo = 2
      GOTO 99999
   ELSE
      loscar = loscar - 50
!
!     MOVE LABEL NAME PORTION OF LBLTBL
!
      jx = lstlbl + 3
      DO ix = lbltop , jx
         iy = ix - 50
         lbltbl(iy) = lbltbl(ix)
      ENDDO
      lbltop = lbltop - 50
      lstlbl = lstlbl - 50
      GOTO irturn
   ENDIF
 4000 lbltbl(lstlbl+2) = lbltbl(j+2)
   GOTO 3600
!
!     WARNING - CANNOT CHECKPOINT USER INPUT
!
 4100 CALL xgpidg(-48,ospnt,oscar(iospnt),oscar(iospnt+1))
   GOTO 1200
99999 END SUBROUTINE xosgen
