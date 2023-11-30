
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
   INTEGER Alter(2) , Bcdcnt , Bufsz , Ceitbl(42) , Cnmtp , Core(1) , Diag14 , Diag17 , Diag25 , Diag4 , Dmap(1) , Dmpcnt , Dmppnt ,&
         & Dum(20) , Fiat(3) , File(1) , Fnmtp , Fpnt , Iallon , Iapp , Iappnd , Ibuff(20) , Icfiat , Icfpnt , Icftop , Ichar ,     &
         & Icold , Icpflg , Icrdtp , Icst , Ictlfl(1) , Idmapp , Idmpnt , Idsapp , Iequl , Iestim , Ifirst , Iflag , Iflg(6) ,      &
         & Ihapp , Imst , Insert , Intgr , Iplus , Irturn , Isavdw , Isave , Iseqn , Isgnon , Islsh , Istopf , Iswtch(3) , Itape ,  &
         & Iunst , Junk(54) , Lctlfl , Ldmap , Length , Lfile , Lmed , Lmpl , Loscar , Losgn , Maskhi , Masklo , Masks(1) , Medpnt ,&
         & Medtp , Modidx , Modnam , Mpl(1) , Mplpnt , Namopt(26) , Nbegin , Nblank , Ncond , Ndmap , Nend , Nestm1 , Nestm2
   INTEGER Newcrd , Nexit , Njump , Nmed , Nnames , Nogo , Nosgn , Nrept , Nscr , Nsol , Ntypee(9) , Nwords , Nxequi , Optape ,     &
         & Os(5) , Osbot , Ospnt , Osprc , Preflg , Prenam(100) , Pvt(200) , Savnam(100) , Seqno , Sol , Start , Subset , Vps(2) ,  &
         & Xx(4)
   COMMON /autocm/ Preflg , Nnames , Prenam
   COMMON /autosm/ Nwords , Savnam
   COMMON /moddmp/ Iflg , Namopt
   COMMON /passer/ Istopf , Modnam
   COMMON /system/ Bufsz , Optape , Nogo , Dum , Icfiat , Junk , Iswtch , Icpflg
   COMMON /xceitb/ Ceitbl
   COMMON /xfiat / Fiat
   COMMON /xgpi2 / Lmpl , Mplpnt , Mpl
   COMMON /xgpi3 / Pvt
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpi5 / Iapp , Start , Alter , Sol , Subset , Iflag , Iestim , Icftop , Icfpnt , Lctlfl , Ictlfl
   COMMON /xgpi6 / Medtp , Fnmtp , Cnmtp , Medpnt , Lmed , Iplus , Diag14 , Diag17 , Diag4 , Diag25 , Ifirst , Ibuff
   COMMON /xgpi7 / Fpnt , Lfile , File
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Nmed , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend , Njump ,&
                 & Ncond , Nrept , Ntypee , Maskhi , Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / Icst , Iunst , Imst , Ihapp , Idsapp , Idmapp , Isave , Itape , Iappnd , Intgr , Losgn
   COMMON /xgpie / Nscr
   COMMON /xoldpt/ Xx , Seqno
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Core
   INTEGER andf , korsz , lshift , orf , rshift
   INTEGER cdcomp(3) , declar(3) , dmpcrd(1) , fcode , fparam(3) , i , i1 , idlhss , idpbuf , iendf , ilevel , index , iold , ion , &
         & iosdav , iospnt , iprcfo , iprime , itype(6) , ivrept , ix , iy , j , j1 , jx , k , k1 , k2 , kdh , kk , l , lblbot ,    &
         & lblerr , lbltbl(1) , lbltop , ldef , lookup , lstlbl , lstlsv , lstpar , m , med(1) , n1 , n2 , n3 , namtbl(12) ,        &
         & nceit1 , nceit2 , nfile , nlblt1 , nlblt2 , nospnt , nskip(5,2) , nvps , nxpurg , os2b4 , oscar(1) , prechk(2) , varflg ,&
         & xchk , xdmap(2)
   LOGICAL skip
   EXTERNAL andf , lshift , orf , rshift
!WKBR COMMON /XGPI3 / PVT(2)
!WKBR COMMON /XCEITB/ CEITBL(2)
!
!     EQUIVALENCE     (NTYPEE(1),NTIME ), (NTYPEE(2),NSAVE )
!    1                (NTYPEE(3),NOUTPT), (NTYPEE(4),NCHKPT)
!    2                (NTYPEE(5),NPURGE), (NTYPEE(6),NEQUIV)
!    3                (NTYPEE(7),NCPW  ), (NTYPEE(8),NBPC  )
!    4                (NTYPEE(9),NWPC  )
   EQUIVALENCE (namtbl(9),nxpurg)
   EQUIVALENCE (oscar(1),dmpcrd(1),lbltbl(1),med(1),Os(5)) , (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt)
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
   Ifirst = 0
   Osbot = 1
   Nwords = 0
   lookup = 0
   Preflg = 0
   ivrept = 0
   ilevel = 0
   skip = .FALSE.
   Ospnt = Osbot
   oscar(Osbot) = 0
   oscar(Osbot+1) = 1
!
!     FOR RESTART ALLOW CHECKPOINT AND JUMP ENTRIES TO BE INSERTED IN
!     OSCAR BY XGPI.
!
   IF ( Start/=Icst ) oscar(Osbot+1) = 3
!
!     ALLOCATE 50 WORDS IN OPEN CORE FOR LBLTBL AND SET LBLTBL
!     PARAMETERS.
!
   lblbot = Loscar
   lbltop = Loscar - 50
   Loscar = lbltop - 1
   lstlbl = lbltop - 4
   lstpar = lblbot + 1
!
!     INITIALIZE DMPCRD ARRAY FOR RIGID FORMAT
!
   Icrdtp = Loscar
!
!     ****************************************
!     PREPARE TO PROCESS NEXT DMAP INSTRUCTION
!     ****************************************
!
 100  Dmpcnt = Dmpcnt + 1
   IF ( Iapp/=Idmapp ) THEN
      Medpnt = med(Medtp+1)*(Dmpcnt-1) + Medtp + 2
      IF ( med(Medtp)<Dmpcnt .AND. Iapp/=Idmapp ) THEN
!
!     DMAP SEQUENCE DOES NOT CORRESPOND TO MED TABLE
!
         CALL xgpidg(39,0,0,0)
!
!     RETURN WHEN XGPI HAS BEEN DISASTERED.
!
         Nogo = 2
         GOTO 99999
      ENDIF
   ENDIF
   Newcrd = -1
   Insert = 0
!
!     SEE IF DMAP INSTRUCTION IS TO BE DELETED OR INSERTED
!
   IF ( Alter(1)/=0 .AND. Alter(1)<=Dmpcnt ) THEN
      IF ( Alter(1)<=Dmpcnt .AND. Alter(2)>=Dmpcnt ) THEN
!
!     SET INSERT FLAG TO NO PRINT
!
         Insert = -2
         GOTO 400
      ELSE
         IF ( Alter(2)/=0 ) THEN
!
!     JUST FINISHED DELETING, SET INSERT AND ALTER FOR INSERTING
!
            Alter(1) = Alter(2)
            Alter(2) = 0
         ENDIF
         IF ( Alter(1)==Dmpcnt-1 ) THEN
            Insert = 1
            Dmpcnt = Dmpcnt - 1
            GOTO 200
         ENDIF
      ENDIF
   ENDIF
!
!     GET NEXT DMAP INSTRUCTION
!     FOR RIGID FORMAT SEE IF OSCAR ENTRY IS PART OF SUBSET
!
   IF ( Iapp/=Idmapp ) THEN
      i = med(Medtp+1)
      DO j = 1 , i
         k = Medpnt + j - 1
         IF ( med(k)/=0 ) GOTO 200
      ENDDO
      Insert = -2
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
         IF ( skip ) Insert = Insert - 2
      ENDIF
   ENDIF
!
   IF ( lookup==1 .AND. Preflg/=0 ) THEN
      Preflg = -Preflg
      CALL autock(Ospnt)
   ENDIF
   Modnam = 1
   lookup = 0
   CALL xscndm
   Modnam = 0
   IF ( Irturn==1 .OR. Irturn==3 ) THEN
!
!     NO MACRO INSTRUCTION NAME ON DMAP CARD.
!
      CALL xgpidg(12,0,Dmpcnt,0)
   ELSEIF ( Irturn==4 ) THEN
      GOTO 100
   ELSEIF ( Irturn==5 ) THEN
      GOTO 3700
   ELSEIF ( .NOT.skip ) THEN
!
!     FIND MPL ENTRY AND BRANCH ON TYPE
!
      Mplpnt = 1
      Modidx = 1
      IF ( Dmap(Dmppnt)==prechk(1) .AND. Dmap(Dmppnt+1)==prechk(2) ) THEN
         DO
!
!     PROCESS PRECHK CARD
!
            index = 3
            CALL xscndm
            IF ( Irturn==1 .OR. Irturn==3 .OR. Irturn==4 .OR. Irturn==5 ) THEN
!
!     DMAP FORMAT ERROR
!
               CALL xgpidg(16,Ospnt,0,0)
               EXIT
!
!     TEST FOR  ALL  OPTION OR BLANK
!
            ELSEIF ( Dmap(Dmppnt)/=Nblank ) THEN
               Preflg = 1
               Nnames = 0
               IF ( Dmap(Dmppnt)==Namopt(23) ) GOTO 3200
               IF ( Dmap(Dmppnt)/=Nend ) GOTO 3100
               Preflg = 0
               GOTO 3300
            ENDIF
         ENDDO
      ELSEIF ( Dmap(Dmppnt)==xdmap(1) .AND. Dmap(Dmppnt+1)==xdmap(2) ) THEN
!
!     PROCESS XDMAP INSTRUCTION
!
         iold = Diag14
         DO
            CALL xscndm
            IF ( Irturn==1 .OR. Irturn==3 ) THEN
               CALL xgpidg(16,Ospnt,0,0)
               EXIT
            ELSEIF ( Irturn==2 ) THEN
               IF ( Dmap(Dmppnt)==Nblank ) CYCLE
!
!     HAVE LOCATED AN XDMAP OPTION
!
               DO k = 1 , 22 , 2
                  IF ( Dmap(Dmppnt)==Namopt(k) .AND. Dmap(Dmppnt+1)==Namopt(k+1) ) GOTO 205
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
                  IF ( Irturn==2 .OR. Irturn==3 .OR. Irturn==4 ) THEN
                     CALL xgpidg(16,Ospnt,0,0)
                     EXIT
                  ELSEIF ( Irturn==5 ) THEN
                     GOTO 3700
                  ELSEIF ( Dmap(Dmppnt+1)/=Iequl ) THEN
                     CALL xgpidg(16,Ospnt,0,0)
                     EXIT
                  ELSE
                     CALL xscndm
                     IF ( Irturn==1 .OR. Irturn==2 .OR. Irturn==4 ) THEN
                        CALL xgpidg(16,Ospnt,0,0)
                        EXIT
                     ELSEIF ( Irturn==5 ) THEN
                        GOTO 3700
                     ELSE
                        Iflg(2) = Dmap(Dmppnt+1)
                        IF ( Iflg(2)<0 .OR. Iflg(2)>2 ) THEN
                           CALL xgpidg(56,0,0,0)
                           EXIT
                        ENDIF
                     ENDIF
                  ENDIF
               ELSEIF ( kk==4 ) THEN
                  IF ( Diag14/=1 ) THEN
                     Iflg(3) = 1
                     Diag14 = 2
                  ENDIF
               ELSEIF ( kk==5 ) THEN
                  IF ( Diag14/=1 ) THEN
                     Iflg(3) = 0
                     Diag14 = 0
                  ENDIF
               ELSEIF ( kk==6 ) THEN
                  IF ( Diag17/=1 ) THEN
                     Iflg(4) = 1
                     Diag17 = 2
                  ENDIF
               ELSEIF ( kk==7 ) THEN
                  IF ( Diag17/=1 ) THEN
                     Iflg(4) = 0
                     Diag17 = 0
                  ENDIF
               ELSEIF ( kk==8 ) THEN
                  IF ( Diag25/=1 ) THEN
                     Iflg(5) = 1
                     Diag25 = 1
                  ENDIF
               ELSEIF ( kk==10 ) THEN
                  IF ( Diag4/=1 ) THEN
                     Iflg(6) = 1
                     Diag4 = 1
                  ENDIF
               ELSE
                  Iflg(1) = 0
               ENDIF
            ELSEIF ( Irturn==5 ) THEN
               GOTO 3700
            ELSE
               index = 2
               IF ( iold==0 .OR. Ifirst==0 ) GOTO 1700
               IF ( Start/=Icst ) WRITE (Optape,99001) Iplus , Iplus
99001          FORMAT (A1,2X,A1)
               EXIT
            ENDIF
         ENDDO
      ELSEIF ( Dmap(Dmppnt)==cdcomp(1) .AND. (Dmap(Dmppnt+1)==cdcomp(2) .OR. Dmap(Dmppnt+1)==cdcomp(3)) ) THEN
!
!     PROCESS CONDCOMP INSTRUCTION
!
         IF ( ilevel>=5 ) THEN
            CALL xgpidg(16,Ospnt,0,0)
         ELSE
            ion = 0
            IF ( Dmap(Dmppnt+1)==cdcomp(2) ) ion = 1
            CALL xscndm
            IF ( Irturn==1 .OR. Irturn==4 ) THEN
               CALL xgpidg(16,Ospnt,0,0)
            ELSEIF ( Irturn==3 ) THEN
!
!     INSTRUCTION COUNT GIVEN FOR END
!
               IF ( Dmap(Dmppnt+1)<0 ) THEN
                  CALL xgpidg(16,Ospnt,0,0)
               ELSE
                  nskip(ilevel+1,1) = Dmap(Dmppnt+1)
                  GOTO 3400
               ENDIF
            ELSEIF ( Irturn==5 ) THEN
               GOTO 3700
            ELSE
!
!     LABEL SPECIFIED FOR END
!
               nskip(ilevel+1,1) = Dmap(Dmppnt)
               nskip(ilevel+1,2) = Dmap(Dmppnt+1)
               GOTO 3400
            ENDIF
         ENDIF
      ELSE
         DO WHILE ( Mpl(Mplpnt+1)/=Dmap(Dmppnt) .OR. Mpl(Mplpnt+2)/=Dmap(Dmppnt+1) )
!
!     CHECK FOR ERROR IN MPL TABLE
!
            IF ( Mpl(Mplpnt)<1 .OR. Mpl(Mplpnt)>Lmpl ) THEN
               CALL xgpidg(49,0,0,0)
               Nogo = 2
               GOTO 99999
            ELSE
               Mplpnt = Mpl(Mplpnt) + Mplpnt
               Modidx = 1 + Modidx
               IF ( Mplpnt>=Lmpl ) THEN
!
!     NO MPL ENTRY FOR THIS DMAP MACRO INSTRUCTION
!
                  CALL xgpidg(13,0,Dmppnt,Dmpcnt)
                  GOTO 300
               ENDIF
            ENDIF
         ENDDO
!
!     GET FORMAT TYPE FROM MPL AND BRANCH
!
         i = Mpl(Mplpnt+3)
         IF ( i<1 .OR. i>5 ) THEN
!
!     MPL TABLE INCORRECT
!
            CALL xgpidg(49,0,0,0)
            Nogo = 2
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
            iprcfo = Ospnt
!
!     GENERATE I/P FILE SECTION
!
            CALL xipfl
!
!     DIAGNOSTIC MESSAGES -
!
!     DMAP INPUT FILE ERROR
!
            IF ( Irturn==2 ) CALL xgpidg(-10,Ospnt,0,0)
!
!     SAVE POINTER TO O/P FILE SECTION
!
            j = Ospnt + oscar(Ospnt)
!
!     GENERATE O/P FILE SECTION
!
            CALL xopfl
!
!     DMAP OUTPUT FILE ERROR
!
            IF ( Irturn==2 ) CALL xgpidg(-11,Ospnt,0,0)
!
!     NUMBER OF SCRATCH FILES TO OSCAR
!
            i = Ospnt + oscar(Ospnt)
            oscar(i) = Mpl(Mplpnt)
!
!     INCREMENT OSCAR WORD COUNT AND MPLPNT
!
            oscar(Ospnt) = 1 + oscar(Ospnt)
            Mplpnt = 1 + Mplpnt
!
!     GENERATE PARAMETER SECTION
!
            CALL xparam
            IF ( Irturn==2 ) GOTO 3700
!
!     CONTINUE COMPILATION
!     ZERO INTERNAL CHECKPOINT FLAG IN OSCAR ENTRY FOR TYPE F ENTRY
!
            IF ( andf(oscar(Ospnt+2),Maskhi)/=2 ) THEN
               i = Ospnt + oscar(Ospnt)
               oscar(i) = 0
               oscar(Ospnt) = 1 + oscar(Ospnt)
            ENDIF
            IF ( Nwords/=0 ) THEN
               CALL autosv
               Nwords = 0
            ENDIF
            IF ( Preflg/=0 .AND. Istopf/=0 ) CALL autock(Istopf)
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
            oscar(Ospnt) = 7
!
!     CHECK FOR END CARD
!
            IF ( oscar(Ospnt+3)==Nend ) THEN
               oscar(Ospnt+3) = Nexit
               iendf = 1
!
!     SET EXECUTE FLAG IN OSCAR FOR END
!
               oscar(Ospnt+5) = orf(Isgnon,oscar(Ospnt+5))
            ENDIF
!
!     GET NEXT ENTRY IN DMAP
!
            CALL xscndm
            IF ( Irturn==1 ) THEN
               CALL xgpidg(16,Ospnt,0,0)
            ELSEIF ( Irturn==3 .OR. Irturn==4 ) THEN
!
!     EXIT DMAP INSTRUCTION, SET EXECUTE FLAG AND OSCAR VALUE SECTION.
!
               IF ( oscar(Ospnt+3)/=Nexit ) THEN
                  CALL xgpidg(16,Ospnt,0,0)
               ELSE
                  IF ( Dmap(Dmppnt)/=Intgr ) Dmap(Dmppnt+1) = 0
                  Dmap(Dmppnt) = Intgr
                  Dmap(Dmppnt+2) = rshift(Iallon,1)
                  GOTO 900
               ENDIF
            ELSEIF ( Irturn==5 ) THEN
               GOTO 3700
            ELSE
!
!     IF NEXT DMAP ENTRY IS BCD IT SHOULD BE LABEL NAME FOR BRANCH
!     DMAP INSTRUCTION.
!
               IF ( oscar(Ospnt+3)==Nexit ) THEN
                  CALL xgpidg(16,Ospnt,0,0)
                  GOTO 300
               ELSE
!
!     SEARCH LABEL TABLE FOR LABEL NAME
!
                  IF ( lstlbl>=lbltop ) THEN
                     DO j = lbltop , lstlbl , 4
                        IF ( Dmap(Dmppnt)==lbltbl(j) .AND. Dmap(Dmppnt+1)==lbltbl(j+1) ) GOTO 210
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
               IF ( Ntypee(i)==Dmap(Dmppnt) ) EXIT
            ENDDO
            i = 2*i - 1
            Dmap(Dmppnt) = namtbl(i)
            Dmap(Dmppnt+1) = namtbl(i+1)
!
!     GENERATE LINK HEADER FOR OSCAR
!
            IF ( i==9 .OR. i==11 ) lookup = 1
            os2b4 = Osprc
            CALL xlnkhd
!
!     BRANCH ON DMAP NAME AND GENERATE VALUE/OUTPUT SECTION OF OSCAR
!
            i = (i+1)/2
            IF ( i==2 ) THEN
!
!     XSAVE ENTRY, ENTER POINTERS IN VALUE SECTION OF OSCAR.
!
               i = Ospnt + oscar(Ospnt)
               oscar(i) = 0
               k = i - 1
               DO
!
!     GET PARAMETER NAME FROM DMAP.
!
                  CALL xscndm
                  IF ( Irturn==1 .OR. Irturn==3 ) THEN
!
!     ILLEGAL CHARACTERS IN DMAP SAVE PARAMETER NAME LIST
!
                     CALL xgpidg(20,Ospnt,oscar(i)+1,0)
                  ELSEIF ( Irturn==4 ) THEN
!
!
!     END OF SAVE PARAMETER NAME LIST, INCREMENT OSCAR WORD COUNT.
!
                     oscar(Ospnt) = oscar(Ospnt) + 2*oscar(i) + 1
!
!     GET PARAMETER VALUE DISPLACEMENT IN COMMON FROM PRECEDING
!     OSCAR ENTRY.
!
                     iosdav = Osprc
                     IF ( oscar(Osprc+3)==xchk ) Osprc = os2b4
                     IF ( andf(oscar(Osprc+2),Maskhi)>2 ) THEN
!
!     SAVE OUT OF POSITION
!
                        CALL xgpidg(61,Ospnt,0,0)
                        Ospnt = iosdav
                        Osprc = os2b4
                     ELSE
!
!     J = OSCAR POINTER TO BEGINNING OF PARAMETER SECTION.
!
                        j = Osprc + 6 + 3*oscar(Osprc+6) + 1
                        IF ( andf(oscar(Osprc+2),Maskhi)==1 ) j = j + 1 + 3*oscar(j)
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
                           l = andf(oscar(n3),Nosgn)
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
 214                       l = andf(Vps(l-1),Maskhi)
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
                              CALL xgpidg(21,Ospnt,Vps(j-3),Vps(j-2))
                           ENDIF
                        ENDDO
                     ENDIF
                     EXIT
                  ELSEIF ( Irturn==5 ) THEN
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
                     DO WHILE ( Vps(j)/=Dmap(Dmppnt) .OR. Vps(j+1)/=Dmap(Dmppnt+1) )
                        l = andf(Vps(j+2),Maskhi)
                        j = j + l + 3
                        IF ( j>=Vps(2) ) THEN
!
!     XSAVE PARAMETER NAME NOT ON PRECEDING DMAP CARD
!
                           CALL xgpidg(21,Ospnt,Dmap(Dmppnt),Dmap(Dmppnt+1))
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
                     CALL xgpidg(-2,Ospnt,Dmap(Dmppnt),Dmap(Dmppnt+1))
                  ENDIF
 220           ENDDO
            ELSEIF ( i==3 .OR. i==4 .OR. i==5 .OR. i==6 ) THEN
               GOTO 1000
            ELSE
!
!     EXTIME ENTRY, CHECK ESTIM IN CONTROL FILE
!
               oscar(Ospnt+5) = andf(oscar(Ospnt+5),Nosgn)
               IF ( Iestim/=0 ) THEN
!
!     GET TIME SEGMENT NAME
!
                  CALL xscndm
                  IF ( Irturn==1 .OR. Irturn==3 .OR. Irturn==4 ) THEN
!
!     TIME SEGMENT NAME INCORRECT - WARNING ONLY
!
                     CALL xgpidg(-17,Ospnt,0,0)
                  ELSEIF ( Irturn==5 ) THEN
                     GOTO 3700
                  ELSE
                     i = Iestim + Ictlfl(Iestim) - 1
                     j = Iestim + 1
                     DO k = j , i , 2
                        IF ( Dmap(Dmppnt)==Ictlfl(k) .AND. Dmap(Dmppnt+1)==Ictlfl(k+1) ) oscar(Ospnt+5) = orf(oscar(Ospnt+5),Isgnon)
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
            j = Osbot + oscar(Osbot)
            oscar(j+3) = Dmap(Dmppnt)
            oscar(j+4) = Dmap(Dmppnt+1)
            oscar(j+5) = Dmpcnt
            CALL xlnkhd
!
!     NOW PROCESS INSTRUCTION
!
            DO j = 1 , 3
               IF ( Dmap(Dmppnt)==declar(j) ) THEN
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
      IF ( Dmap(Dmppnt)/=declar(2) ) GOTO 400
      GOTO 2000
   ENDIF
!
!     *****************************************************
!     RETURN HERE AFTER DMAP INSTRUCTION HAS BEEN PROCESSED
!     *****************************************************
!
!     CHECK FOR FATAL ERROR
!
 300  IF ( Nogo==2 ) GOTO 3700
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
      IF ( med(Medtp)/=Dmpcnt .AND. Iapp/=Idmapp ) THEN
         CALL xgpidg(39,0,0,0)
         Nogo = 2
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
            DO WHILE ( lbltbl(lstpar)/=Vps(k) .OR. lbltbl(lstpar+1)/=Vps(k+1) )
               k = k + andf(Vps(k+2),Maskhi) + 3
               IF ( k>=Vps(2) ) THEN
!
!     SEARCH PVT TABLE FOR PARAMETER. IF FOUND ENTER PARAMETER IN VPS.
!
                  k1 = 3
                  DO
                     Length = andf(Pvt(k1+2),Nosgn)
                     Length = itype(Length)
                     IF ( lbltbl(lstpar)==Pvt(k1) .AND. lbltbl(lstpar+1)==Pvt(k1+1) ) THEN
                        k = Vps(2) + 1
                        Pvt(k1+2) = orf(Pvt(k1+2),Isgnon)
                        Vps(2) = k + 2 + Length
                        IF ( Vps(2)>=Vps(1) ) THEN
!
!     VPS TABLE OVERFLOWED
!
                           CALL xgpidg(14,nvps,Nblank,0)
                           Nogo = 2
                           GOTO 99999
                        ELSE
                           k2 = Length + 3
                           DO m = 1 , k2
                              j = k + m - 1
                              j1 = k1 + m - 1
                              Vps(j) = Pvt(j1)
                           ENDDO
                           GOTO 310
                        ENDIF
                     ELSE
                        k1 = k1 + Length + 3
                        IF ( k1>=Pvt(2) ) THEN
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
      IF ( Irturn==4 ) GOTO 100
      IF ( Irturn==5 ) GOTO 3700
      IF ( Nogo==0 .AND. Insert>=0 ) THEN
         CALL xgpidg(16,Ospnt,0,0)
         GOTO 300
      ENDIF
   ENDDO
!
!     MAKE NEW ENTRY IN LABEL TABLE, CHECK FOR TABLE OVERFLOW
!
 500  ASSIGN 600 TO Irturn
   IF ( lstlbl+8>=lstpar ) GOTO 3900
 600  lstlbl = lstlbl + 4
   j = lstlbl
   lbltbl(j) = Dmap(Dmppnt)
   lbltbl(j+1) = Dmap(Dmppnt+1)
   lbltbl(j+2) = ldef
 700  lbltbl(j+3) = Ospnt
!
!     GET NEXT ENTRY FROM DMAP, ENTRY IS $ FOR JUMP,NAME FOR COND,
!     VALUE FOR REPT.
!
   CALL xscndm
   IF ( Irturn==1 ) THEN
      CALL xgpidg(16,Ospnt,0,0)
      GOTO 300
   ELSEIF ( Irturn==2 ) THEN
!
!     COND DMAP INSTRUCTION, ENTER PARAMETER NAME IN LABEL TABLE.
!
      IF ( oscar(Ospnt+3)==Nrept ) THEN
         ivrept = 1
         GOTO 900
      ELSEIF ( oscar(Ospnt+3)/=Ncond ) THEN
         CALL xgpidg(16,Ospnt,0,0)
         GOTO 300
      ELSE
         ASSIGN 800 TO Irturn
         IF ( lstpar-8<=lstlbl ) GOTO 3900
      ENDIF
   ELSEIF ( Irturn==3 ) THEN
!
!     REPT DMAP INSTRUCTION, COUNT TO VALUE SECTION.
!
      IF ( oscar(Ospnt+3)==Nrept ) GOTO 900
      CALL xgpidg(16,Ospnt,0,0)
      GOTO 300
   ELSEIF ( Irturn==5 ) THEN
      GOTO 3700
   ELSE
!
!     DMAP INSTRUCTION IS JUMP
!
      oscar(Ospnt+6) = 0
      IF ( oscar(Ospnt+3)/=Njump ) CALL xgpidg(16,Ospnt,0,0)
      GOTO 300
   ENDIF
 800  lstpar = lstpar - 4
   lbltbl(lstpar) = Dmap(Dmppnt)
   lbltbl(lstpar+1) = Dmap(Dmppnt+1)
   lbltbl(lstpar+2) = Ospnt + 6
   lbltbl(lstpar+3) = Ospnt
   GOTO 300
!
!     ENTER LOOP COUNT IN CEITBL FOR REPT AND EXIT INSTRUCTIONS
!
 900  Ceitbl(2) = Ceitbl(2) + 4
   IF ( Ceitbl(2)>Ceitbl(1) ) THEN
!
!     CEITBL OVERFLOW, DISCONTINUE COMPILATION
!
      CALL xgpidg(14,nceit1,nceit2,Dmpcnt)
      Nogo = 2
      GOTO 99999
   ELSE
!
!     I = POINTER TO LOOP COUNT IN CEITBL ENTRY
!
      i = Ceitbl(2) - 2
      IF ( ivrept==0 ) THEN
         Ceitbl(i) = lshift(Dmap(Dmppnt+1),16)
      ELSE
!
!     PROCESS VARIABLE REPT INSTRUCTION - FIND PARAM IN VPS
!
         kdh = 3
         DO WHILE ( Dmap(Dmppnt)/=Vps(kdh) .OR. Dmap(Dmppnt+1)/=Vps(kdh+1) )
            kdh = kdh + andf(Vps(kdh+2),Maskhi) + 3
            IF ( kdh>=Vps(2) ) THEN
!
!     CHECK PVT FOR PARAMETER
!
               kdh = 3
               DO
                  Length = andf(Pvt(kdh+2),Nosgn)
                  Length = itype(Length)
                  IF ( Dmap(Dmppnt)/=Pvt(kdh) .OR. Dmap(Dmppnt+1)/=Pvt(kdh+1) ) THEN
                     kdh = kdh + Length + 3
                     IF ( kdh>=Pvt(2) ) THEN
!
!     VARIABLE REPT INSTRUCTION ERRORS
!
                        CALL xgpidg(58,0,0,0)
                        GOTO 300
                     ENDIF
                  ELSEIF ( Length/=itype(1) ) THEN
                     CALL xgpidg(57,0,0,0)
                     GOTO 300
                  ELSE
                     Ceitbl(i) = lshift(Pvt(kdh+3),16)
                     GOTO 950
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
!
!     PARAMETER FOUND
!
         IF ( andf(rshift(Vps(kdh+2),16),15)/=1 ) THEN
            CALL xgpidg(57,0,0,0)
            GOTO 300
         ELSE
            Ceitbl(i) = lshift(kdh,16)
            Ceitbl(i) = orf(Ceitbl(i),Isgnon)
         ENDIF
      ENDIF
!
!     FIRST WORD OF CEITBL ENTRY CONTAINS OSCAR RECORD NUMBERS OF
!     BEGINNING AND END OF LOOP
!
 950  Ceitbl(i-1) = Iseqn
      ivrept = 0
!
!     OSCAR VALUE SECTION CONTAINS POINTER TO LOOP COUNT IN CEITBL ENTRY
!
      oscar(Ospnt+6) = i
      GOTO 300
   ENDIF
!
!     XUOP,XCHK,XPURGE,OR XEQUIV OSCAR ENTRY - GENERATE FILE NAME LIST.
!
 1000 nospnt = Ospnt + oscar(Ospnt)
   iprime = 1
   iospnt = nospnt + 1
   oscar(nospnt) = 0
 1100 DO
!
!     GET NEXT ENTRY FROM DMAP CARD
!
      CALL xscndm
      IF ( Irturn==1 ) THEN
!
!     DMAP ENTRY IS OPERATOR, CHECK FOR / OPERATOR
!
         IF ( (Dmap(Dmppnt+1)/=Islsh) .OR. (oscar(Ospnt+3)/=Nxequi .AND. oscar(Ospnt+3)/=nxpurg) ) THEN
            CALL xgpidg(16,Ospnt,0,0)
            GOTO 300
         ELSE
!
!     OSCAR ENTRY IS XEQUIV OR XPURGE
!
            varflg = 0
            IF ( oscar(Ospnt+3)==nxpurg ) GOTO 1300
            IF ( oscar(nospnt)>=2 ) GOTO 1300
            CALL xgpidg(16,Ospnt,0,0)
            GOTO 300
         ENDIF
      ELSEIF ( Irturn==3 ) THEN
         CALL xgpidg(16,Ospnt,0,0)
         GOTO 300
      ELSEIF ( Irturn==4 ) THEN
         GOTO 1500
      ELSEIF ( Irturn==5 ) THEN
         GOTO 3700
      ELSE
!
!     DMAP ENTRY IS DATA BLOCK NAME, STORE IN OSCAR
!
         oscar(iospnt) = Dmap(Dmppnt)
         oscar(iospnt+1) = Dmap(Dmppnt+1)
!
!     MAKE SURE FILE IS NOT BLANK
!
         IF ( oscar(iospnt)/=Nblank ) THEN
!
!     FOR CHKPNT - MAKE SURE FILE IS NOT OUTPUT BY USER I/P PROCESSOR
!
            IF ( oscar(Ospnt+3)==namtbl(7) ) THEN
               m = Fiat(3)*Icfiat - 2
               DO j = 4 , m , Icfiat
                  IF ( oscar(iospnt)==Fiat(j+1) .AND. oscar(iospnt+1)==Fiat(j+2) ) GOTO 4100
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
   IF ( oscar(Ospnt+3)==namtbl(11) .AND. oscar(Ospnt+4)==namtbl(12) ) THEN
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
   IF ( Irturn==1 ) GOTO 1600
   IF ( Irturn==3 .OR. Irturn==4 ) THEN
      CALL xgpidg(16,Ospnt,0,0)
      GOTO 300
   ELSEIF ( Irturn==5 ) THEN
      GOTO 3700
   ELSE
      varflg = 1
      IF ( Dmap(Dmppnt)==Nblank ) THEN
         CALL xscndm
         IF ( Irturn==2 .OR. Irturn==3 .OR. Irturn==4 ) THEN
            CALL xgpidg(16,Ospnt,0,0)
            GOTO 300
         ELSEIF ( Irturn==5 ) THEN
            GOTO 3700
         ELSE
            GOTO 1600
         ENDIF
      ELSE
         ASSIGN 1400 TO Irturn
         IF ( lstpar-8<=lstlbl ) GOTO 3900
      ENDIF
   ENDIF
 1400 lstpar = lstpar - 4
   lbltbl(lstpar) = Dmap(Dmppnt)
   lbltbl(lstpar+1) = Dmap(Dmppnt+1)
   lbltbl(lstpar+2) = iospnt
   lbltbl(lstpar+3) = Ospnt
   idlhss = 2*oscar(nospnt) + oscar(Ospnt) + 2
   IF ( oscar(Ospnt+3)==namtbl(11) ) idlhss = idlhss + 1
   oscar(Ospnt) = idlhss
!
!     CHECK FOR POSSIBILITY OF ANOTHER DATA BLOCK NAME LIST.
!
   CALL xscndm
   IF ( Irturn==1 ) GOTO 1000
   IF ( Irturn==2 .OR. Irturn==3 ) THEN
      CALL xgpidg(16,Ospnt,0,0)
   ELSEIF ( Irturn==4 ) THEN
   ELSEIF ( Irturn==5 ) THEN
      GOTO 3700
   ELSE
      GOTO 1500
   ENDIF
   GOTO 300
!
!     END OF DMAP INSTRUCTION, INCREMENT OSCAR WORD COUNT IF NOT XEQUIV
!     OR XPURGE.
!
 1500 IF ( oscar(Ospnt+3)/=Nxequi .AND. oscar(Ospnt+3)/=nxpurg ) THEN
      oscar(Ospnt) = 2*oscar(nospnt) + oscar(Ospnt) + 1
!
!     ELIMINATE ENTRY IF NOTHING CHECKPOINTED.
!
      IF ( oscar(nospnt)==0 ) Osbot = Osprc
   ELSE
      oscar(iospnt) = -1
      idlhss = 2*oscar(nospnt) + oscar(Ospnt) + 2
      IF ( oscar(Ospnt+3)==namtbl(11) ) idlhss = idlhss + 1
      oscar(Ospnt) = idlhss
   ENDIF
   GOTO 300
 1600 IF ( (Dmap(Dmppnt+1)/=Islsh) .OR. (oscar(Ospnt+3)/=Nxequi .AND. oscar(Ospnt+3)/=nxpurg) ) THEN
      CALL xgpidg(16,Ospnt,0,0)
      GOTO 300
   ELSE
      oscar(iospnt) = -1
      idlhss = 2*oscar(nospnt) + oscar(Ospnt) + 2
      IF ( oscar(Ospnt+3)==namtbl(11) ) idlhss = idlhss + 1
      oscar(Ospnt) = idlhss
      GOTO 1000
   ENDIF
 1700 IF ( Ifirst<=0 ) THEN
      IF ( Diag14/=0 .OR. Diag17/=0 ) THEN
         Ifirst = 1
         CALL xgpimw(5,18,Dmpcnt,Ibuff)
         IF ( Start/=Icst ) CALL xgpimw(10,0,0,0)
      ENDIF
   ENDIF
 1800 IF ( index>1 ) GOTO 300
 1900 DO
      CALL xscndm
      IF ( Irturn==1 .OR. Irturn==2 .OR. Irturn==3 ) THEN
      ELSEIF ( Irturn==4 ) THEN
         GOTO 300
      ELSEIF ( Irturn==5 ) THEN
         GOTO 3700
      ELSE
         EXIT
      ENDIF
   ENDDO
!
!     LABEL DECLARATIVE - GET LABEL NAME
!
 2000 CALL xscndm
   IF ( Irturn==1 .OR. Irturn==3 .OR. Irturn==4 ) GOTO 3800
   IF ( Irturn==5 ) GOTO 3700
!
!     CHECK IF LABEL IS FOR CONDITIONAL COMPILATION
!
   IF ( Dmap(Dmppnt)/=nskip(ilevel,1) .OR. Dmap(Dmppnt+1)/=nskip(ilevel,2) ) THEN
      IF ( skip ) GOTO 300
!
!     SCAN LABEL TABLE FOR LABEL NAME
!
      IF ( lstlbl>=lbltop ) THEN
         DO j = lbltop , lstlbl , 4
            IF ( Dmap(Dmppnt)==lbltbl(j) .AND. Dmap(Dmppnt+1)==lbltbl(j+1) ) GOTO 2300
         ENDDO
      ENDIF
!
!     NAME NOT IN LABEL TABLE, MAKE NEW ENTRY
!
      ASSIGN 2100 TO Irturn
      IF ( lstlbl+8>=lstpar ) GOTO 3900
   ELSE
      ilevel = ilevel - 1
      skip = .FALSE.
      GOTO 300
   ENDIF
 2100 lstlbl = lstlbl + 4
   j = lstlbl
   lbltbl(j) = Dmap(Dmppnt)
   lbltbl(j+1) = Dmap(Dmppnt+1)
   lbltbl(j+3) = 0
 2200 lbltbl(j+2) = Iseqn + 1
   GOTO 300
!
!     LABEL NAME FOUND IN LABEL TABLE, DEF ENTRY SHOULD BE ZERO
!
 2300 IF ( lbltbl(j+2)==0 ) GOTO 2200
!
!     LABEL IS MULTIPLY DEFINED
!
   CALL xgpidg(19,Dmpcnt,Dmppnt,0)
   GOTO 300
!
!     FILE DECLARATIVE
!     SET FILE NAME FLAG
!     DO NOT PROCESS FILE DECLARATION WHEN EXECUTE FLAG IS OFF ON
!     MODIFIED RESTART.
!
 2400 IF ( Start==Imst .AND. oscar(Ospnt+5)>=0 ) GOTO 1900
   i = 1
 2500 DO
      CALL xscndm
      IF ( Irturn==2 ) THEN
!
!     NAME ENCOUNTERED - TEST FILE NAME FLAG
!
         IF ( i==0 ) THEN
!
!     FILE PARAMETER FOUND - ENTER APPROPRIATE CODE IN FILE TABLE
!
            DO j = 1 , 3
               IF ( Dmap(Dmppnt)==fparam(j) ) THEN
                  IF ( j==1 ) GOTO 2700
                  IF ( j==2 ) GOTO 2800
                  IF ( j==3 ) GOTO 2900
               ENDIF
            ENDDO
            CALL xgpidg(16,Ospnt,0,0)
            GOTO 300
         ELSE
!
!     FILE NAME - ENTER IN FILE TABLE
!
            Fpnt = Fpnt + 3
            IF ( Fpnt>Lfile-2 ) THEN
!
!     OVERFLOWED FILE TABLE
!
               CALL xgpidg(14,nfile,Nblank,0)
               Nogo = 2
               GOTO 99999
            ELSE
               File(Fpnt) = Dmap(Dmppnt)
               File(Fpnt+1) = Dmap(Dmppnt+1)
!
!     PUT FILE NAME INTO LABEL TABLE FOR DMAP XREF
!
               ASSIGN 2600 TO Irturn
               IF ( lstlbl+8<lstpar ) EXIT
               GOTO 3900
            ENDIF
         ENDIF
      ELSEIF ( Irturn==3 ) THEN
         GOTO 3800
      ELSEIF ( Irturn==4 ) THEN
         GOTO 300
      ELSEIF ( Irturn==5 ) THEN
         GOTO 3700
!
!     DELIMITER ENCOUNTERED
!
      ELSEIF ( Dmap(Dmppnt+1)==Islsh ) THEN
!
!     DELIMITER IS /, TEST FILE NAME FLAG
!
         IF ( i/=0 ) GOTO 3800
         i = 1
      ELSE
         IF ( Dmap(Dmppnt+1)/=Iequl ) GOTO 3800
!
!     DELIMITER IS =, TURN OFF FILE NAME FLAG
!
         i = 0
      ENDIF
   ENDDO
 2600 lstlbl = lstlbl + 4
   lbltbl(lstlbl) = File(Fpnt)
   lbltbl(lstlbl+1) = File(Fpnt+1)
   lbltbl(lstlbl+2) = Iseqn
   lbltbl(lstlbl+3) = -1
   GOTO 2500
!
!     TAPE PARAM
!
 2700 fcode = Itape
   GOTO 3000
!
!     APPEND PARAM
!
 2800 fcode = Iappnd
   GOTO 3000
!
!     SAVE PARAM
!
 2900 fcode = Isave
!
!     PUT CODE IN FILE TABLE
!
 3000 File(Fpnt+2) = orf(File(Fpnt+2),fcode)
   GOTO 2500
 3100 DO
!
!     LIST HAS BEEN FOUND, STORE IN /AUTOCM/
!
      Nnames = Nnames + 1
      IF ( Nnames>50 ) THEN
!
!     PRECHK NAME LIST OVERFLOW
!
         CALL xgpidg(55,0,0,0)
         Nogo = 2
         GOTO 99999
      ELSE
         Prenam(2*Nnames-1) = Dmap(Dmppnt)
         Prenam(2*Nnames) = Dmap(Dmppnt+1)
         CALL xscndm
         IF ( Irturn==1 .OR. Irturn==3 ) THEN
            CALL xgpidg(16,Ospnt,0,0)
            GOTO 300
         ELSEIF ( Irturn==2 ) THEN
         ELSEIF ( Irturn==4 ) THEN
            GOTO 3300
         ELSEIF ( Irturn==5 ) THEN
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
   IF ( Irturn==1 .OR. Irturn==3 ) THEN
      CALL xgpidg(16,Ospnt,0,0)
      GOTO 300
   ELSEIF ( Irturn==5 ) THEN
      GOTO 3700
   ELSEIF ( Dmap(Dmppnt)==Namopt(25) .AND. Dmap(Dmppnt+1)==Namopt(26) ) THEN
      Preflg = 3
      CALL xscndm
      IF ( Irturn==1 .OR. Irturn==3 ) THEN
         CALL xgpidg(16,Ospnt,0,0)
         GOTO 300
      ELSEIF ( Irturn==2 ) THEN
         GOTO 3100
      ELSEIF ( Irturn==4 ) THEN
      ELSEIF ( Irturn==5 ) THEN
         GOTO 3700
      ELSE
         Preflg = 0
      ENDIF
   ELSE
      Preflg = 2
   ENDIF
 3300 IF ( Icpflg/=0 ) THEN
      IF ( Start/=Icst ) CALL xgpimw(10,0,0,0)
      GOTO 1800
   ELSE
      Preflg = 0
      GOTO 300
   ENDIF
!
!     GET LABEL AND LOOK FOR IT IN PVT
!
 3400 CALL xscndm
   IF ( Irturn==1 .OR. Irturn==3 .OR. Irturn==4 ) THEN
      CALL xgpidg(16,Ospnt,0,0)
      GOTO 300
   ELSEIF ( Irturn==5 ) THEN
      GOTO 3700
   ELSE
      ilevel = ilevel + 1
      kdh = 3
      DO
         Length = andf(Pvt(kdh+2),Nosgn)
         Length = itype(Length)
         IF ( Dmap(Dmppnt)==Pvt(kdh) .AND. Dmap(Dmppnt+1)==Pvt(kdh+1) ) THEN
!
!     CHECK IF VALUE IS FALSE
!
            Pvt(kdh+2) = orf(Pvt(kdh+2),Isgnon)
            IF ( andf(Pvt(kdh+2),Nosgn)/=1 ) THEN
               CALL xgpidg(16,Ospnt,0,0)
            ELSEIF ( Pvt(kdh+3)>=0 .OR. ion/=1 ) THEN
               IF ( Pvt(kdh+3)<0 .OR. ion/=0 ) skip = .TRUE.
            ENDIF
            GOTO 300
         ELSE
            kdh = kdh + Length + 3
            IF ( kdh>=Pvt(2) ) THEN
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
      Nogo = 1
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
      IF ( oscar(i-3)/=Ncond .AND. oscar(i-3)/=Njump ) THEN
         j = oscar(i)
!
!     LABEL NAME TO WORDS 3 AND 4 OF CEITBL ENTRY
!
         Ceitbl(j+1) = lbltbl(lstlbl)
         Ceitbl(j+2) = lbltbl(lstlbl+1)
!
!     OSCAR RECORD NO. OF BEGIN LOOP TO FIRST WORD OF CEITBL ENTRY
!
         Ceitbl(j-1) = orf(lshift(lbltbl(lstlbl+2),16),Ceitbl(j-1))
      ENDIF
      oscar(i) = orf(lshift(lbltbl(lstlbl+2),16),oscar(i))
      lstlbl = lstlbl - 4
      GOTO 3500
   ENDIF
   lstlbl = lstlbl - 4
   GOTO 3500
 3700 Loscar = lblbot
   idpbuf = korsz(oscar) - 2*Bufsz
   CALL close(Nscr,1)
   lstlbl = lstlbl - lbltop + 4
   IF ( lstlbl<0 ) lstlbl = 0
   RETURN
 3800 j = Osbot + oscar(Osbot) + 6
   CALL xgpidg(16,j,0,0)
   GOTO 300
!
!     LBLTBL OVERFLOWED - ALLOCATE 50 MORE WORDS FOR IT.
!
 3900 Icrdtp = Icrdtp - 50
   IF ( Icrdtp<oscar(Osbot)+Osbot ) THEN
!
!     LABEL TABLE OVERFLOW, DISCONTINUE COMPILATION
!
      CALL xgpidg(14,nlblt1,nlblt2,Dmpcnt)
      Nogo = 2
      GOTO 99999
   ELSE
      Loscar = Loscar - 50
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
      GOTO Irturn
   ENDIF
 4000 lbltbl(lstlbl+2) = lbltbl(j+2)
   GOTO 3600
!
!     WARNING - CANNOT CHECKPOINT USER INPUT
!
 4100 CALL xgpidg(-48,Ospnt,oscar(iospnt),oscar(iospnt+1))
   GOTO 1200
99999 RETURN
END SUBROUTINE xosgen
