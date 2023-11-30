
SUBROUTINE xflord
!
!     THE PURPOSE OF THIS ROUTINE IS TO COMPUTE THE LTU (LAST TIME USED)
!     VALUE AND THE NTU (NEXT TIME USED) VALUE FOR THE INPUT AND OUTPUT
!     FILE SECTIONS OF THE OSCAR ENTRIES.
!
!          ... DESCRIPTION OF PROGRAM VARIABLES ...
!     LPTOP  = POINTER/SEQUENCE NUMBER OF FIRST ENTRY IN A DMAP LOOP.
!     LPBOT  = LAST ENTRY IN A LOOP.
!     IOPNT  = POINTER TO FILE NAME IN I/O SECTION OF OSCAR ENTRY.
!     LPORD  = POINTER TO IORDNL TABLE ENTRY CORRESPONDING TO LPTOP.
!     IORDNO = FILE ORDINAL NUMBER
!
   IMPLICIT NONE
   INTEGER Alter(2) , Bcdcnt , Bufsz , Core(1) , Diag14 , Dmap(1) , Dmpcnt , Dmppnt , Dpl(3) , Dum1(20) , Dum2(57) , Dum5(5) ,      &
         & File(1) , Fpnt , Iallon , Iapp , Iappnd , Icfiat , Icfpnt , Icftop , Ichar , Icold , Icpbot , Icpdpl(1) , Icpflg ,       &
         & Icptop , Icrdtp , Icst , Ictlfl(1) , Idmapp , Idmpnt , Idsapp , Iequl , Iestim , Ifiat(3) , Ifist(1) , Iflag , Ihapp ,   &
         & Imst , Insert , Intgr , Iorbot , Irturn , Isavdw , Isave , Iseqn , Isgnon , Islsh , Itape , Itmp(1) , Iunst , Lcpdpl ,   &
         & Lctlfl , Ldmap , Length , Lfile , Lmpl , Lordnl , Loscar , Losgn , Lptdic , Lstdpl , Maskhi , Masklo , Masks(1) ,        &
         & Maxdpl , Medtp , Modidx , Mpl(1) , Mplpnt , Nbegin , Nblank , Nbpc , Nchkpt , Ncond , Ncpw , Ndiag , Ndmap , Ndpfil ,    &
         & Nend
   INTEGER Nequiv , Nestm1 , Nestm2 , Newcrd , Nexit , Njump , Noflgs , Nogo , Nosgn , Noutpt , Npurge , Nrept , Nrlfl , Nsave ,    &
         & Nsol , Ntime , Nwpc , Nxequi , Optape , Os(5) , Osbot , Oscar(2) , Ospnt , Osprc , Ptdbot , Ptdic(1) , Ptdtop , Reuse ,  &
         & Seqno , Sol , Start , Subset , Two(4)
   COMMON /system/ Bufsz , Optape , Nogo , Dum1 , Icfiat , Dum2 , Icpflg
   COMMON /two   / Two
   COMMON /xdpl  / Dpl
   COMMON /xfiat / Ifiat
   COMMON /xfist / Ifist
   COMMON /xgpi2 / Lmpl , Mplpnt , Mpl
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpi5 / Iapp , Start , Alter , Sol , Subset , Iflag , Iestim , Icftop , Icfpnt , Lctlfl , Ictlfl
   COMMON /xgpi6 / Medtp , Dum5 , Diag14
   COMMON /xgpi7 / Fpnt , Lfile , File
   COMMON /xgpi8 / Icptop , Icpbot , Lcpdpl
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Ndiag , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend ,       &
                 & Njump , Ncond , Nrept , Ntime , Nsave , Noutpt , Nchkpt , Npurge , Nequiv , Ncpw , Nbpc , Nwpc , Maskhi ,        &
                 & Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / Icst , Iunst , Imst , Ihapp , Idsapp , Idmapp , Isave , Itape , Iappnd , Intgr , Losgn , Noflgs
   COMMON /xoldpt/ Ptdtop , Ptdbot , Lptdic , Nrlfl , Seqno
   COMMON /zzzzzz/ Core
   INTEGER and , compl , or
   INTEGER andf , complf , lshift , orf , rshift
   INTEGER dlyerr , i , i1 , i2 , idmpct , idpl , ifeq , ii , incrlp , iopnt , iordnl(1800) , iprime , irentr , j , j1 , j2 , k ,   &
         & k1 , k2 , khr , kk , kk1 , kxt , l , l1 , l2 , l3 , l4 , lpbot , lptop , lstbot , lstuse , ltu , m , m1 , m2 , n ,       &
         & ncpdp1 , ncpdp2 , ndatab , nnfind , nofind , nordn1 , nordn2 , nthpas , ntu , nwdh , nxvps , xnam(12)
   EXTERNAL andf , complf , lshift , orf , rshift
   EQUIVALENCE (Core(1),Loscar,Os(1)) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Ospnt,Os(4)) , (Oscar(1),Os(5),Ptdic(1)) ,                 &
    & (Dmap(1),Itmp(1)) , (Oscar(1),Icpdpl(1)) , (Lmpl,Lordnl) , (Mplpnt,Iorbot) , (Dpl(1),Ndpfil) , (Dpl(2),Maxdpl) ,              &
    & (Dpl(3),Lstdpl) , (Two(4),Reuse)
   DATA nordn1/4HIORD/ , nordn2/4HNL  / , nxvps/4HXVPS/ , ncpdp1/4HICPD/ , ncpdp2/4HPL  / , xnam/4HXTIM , 4HE    , 4HXSAV , 4HE    ,&
       &4HXUOP , 4H     , 4HXCHK , 4H     , 4HXPUR , 4HGE   , 4HXEQU , 4HIV  /
   DATA nthpas/0/ , dlyerr/0/
!
   or(i,j) = orf(i,j)
   and(i,j) = andf(i,j)
   compl(l) = complf(l)
!
!     USE AREA IN OPEN CORE BETWEEN PTDIC AND MED ARRAYS FOR STORING
!     MISSING FILE DATA
!
   Iflag = 0
   Icptop = Ptdbot + 3
   Icpbot = Icptop - 3
   Lcpdpl = Medtp - Icptop
   IF ( Start==Imst ) dlyerr = 1
   irentr = and(Maskhi,Seqno)
   idmpct = rshift(Seqno,16)
!
!     PREPARE FOR NTH PASS THRU OSCAR
!     *******************************
!
 100  IF ( Nogo>1 ) GOTO 99999
   Ospnt = 1
   Osprc = Ospnt
   Iorbot = 0
   ifeq = 0
!
!     INCREMENT NUMBER OF PASSES MADE THRU OSCAR
!
   nthpas = 1 + nthpas
!
!     ENTER DPL FILE NAMES IN IORDNL TABLE
!
   i = Lstdpl*3 + 1
   idpl = i
   IF ( Lstdpl/=0 ) THEN
      DO k = 4 , i , 3
         Iorbot = Iorbot + 4
         iordnl(Iorbot) = Dpl(k)
         iordnl(Iorbot+1) = Dpl(k+1)
         iordnl(Iorbot+2) = 0
         iordnl(Iorbot+3) = 0
      ENDDO
   ENDIF
!
!     ENTER FIAT NAMES IN IORDNL TABLE
!
   i = Ifiat(3)*Icfiat - 2
   DO k = 4 , i , Icfiat
      IF ( Ifiat(k+1)/=0 ) THEN
         Iorbot = Iorbot + 4
         Ifiat(k) = or(lshift(Iorbot,16),and(Ifiat(k),orf(Maskhi,Losgn)))
         iordnl(Iorbot) = Ifiat(k+1)
         iordnl(Iorbot+1) = Ifiat(k+2)
         iordnl(Iorbot+2) = 0
         iordnl(Iorbot+3) = 0
      ENDIF
   ENDDO
!
!     FOR UNMODIFIED RESTART BEGIN OSCAR PROCESSING AT RE-ENTRY POINT IF
!     THIS IS FIRST PASS THRU OSCAR
!
   IF ( nthpas<=1 ) THEN
      IF ( Start==Iunst .AND. irentr/=0 ) THEN
         DO j = 1 , irentr
            IF ( Oscar(Ospnt+1)>=irentr ) EXIT
            Osprc = Ospnt
            Ospnt = Ospnt + Oscar(Ospnt)
         ENDDO
      ENDIF
   ENDIF
!
!     GET NEXT OSCAR ENTRY
!     ********************
!
!     BRANCH ON OSCAR ENTRY TYPE IF EXECUTE FLAG IS UP
!
 200  IF ( Oscar(Ospnt+5)<0 ) THEN
      i = and(Oscar(Ospnt+2),Maskhi)
      lstbot = Iorbot
      IF ( i==1 ) THEN
!
!     PROCESS TYPE F OSCAR ENTRY
!     **************************
!
!     SCAN OSCAR OUTPUT FILE SECTION,ENTER NAMES IN IORDNL TABLE.
!
         k = Ospnt + 6
         k = Oscar(k)*3 + 2 + k
         i = Oscar(k-1)*3 - 3 + k
         iopnt = k
         ASSIGN 1500 TO Irturn
         GOTO 1000
      ELSEIF ( i==2 ) THEN
         GOTO 1600
      ELSEIF ( i==3 ) THEN
!
!     PROCESS TYPE C OSCAR ENTRY
!     **************************
!
!     CHECK FOR LOOPING
!
         lptop = rshift(Oscar(Ospnt+6),16)
         IF ( (Nexit/=Oscar(Ospnt+3)) .AND. (Oscar(Ospnt+1)>=lptop) ) THEN
!
!     FIND BEGINNING OF LOOP AND ADJUST IORDNL RANGES INSIDE LOOP.
!
            lpbot = Ospnt
            i = Oscar(Ospnt+1)
            Ospnt = 1
            j1 = Oscar(Ospnt+1)
            DO j = j1 , i
               IF ( Oscar(Ospnt+1)==lptop ) EXIT
               Ospnt = Oscar(Ospnt) + Ospnt
            ENDDO
            lptop = Ospnt
!
!     LOOP TOP FOUND - IF UNMODIFIED RESTART,EXECUTE ALL MODULES INSIDE
!     LOOP.
!
            IF ( Oscar(lptop+5)<0 .OR. Start/=Iunst ) THEN
!
!     EXTEND RANGE OF FILES DEFINED OUTSIDE OF LOOP IF USED INSIDE LOOP
!     GET FIRST/NEXT OSCAR ENTRY INSIDE LOOP
!
               Ospnt = lptop
               j1 = Oscar(lptop+1)
               j2 = Oscar(lpbot+1)
               DO j = j1 , j2
                  IF ( and(Oscar(Ospnt+2),Maskhi)<=2 ) THEN
!
!     GET FIRST/NEXT I/P FILE OF OSCAR ENTRY
!
                     k1 = Ospnt + 7
                     k2 = Oscar(k1-1)*3 - 3 + k1
                     DO k = k1 , k2 , 3
                        IF ( Oscar(k)==0 ) CYCLE
!
!     SEE IF FILE SAVE IS ON
!
                        IF ( Fpnt>=1 ) THEN
                           DO l = 1 , Fpnt , 3
                              IF ( Oscar(k)==File(l) .AND. Oscar(k+1)==File(l+1) ) THEN
                                 IF ( and(Isave,File(l+2))/=Isave ) EXIT
                                 GOTO 202
                              ENDIF
                           ENDDO
                        ENDIF
!
!     FILE SAVE FLAG NOT ON - SEE IF I/P FILE IS GENERATED INSIDE LOOP
!
                        l1 = Oscar(Ospnt+1)
!
!     GET FIRST/NEXT OSCAR ENTRY INSIDE LOOP
!
                        n = lptop
                        DO l = j1 , l1
                           IF ( and(Oscar(n+2),Maskhi)==1 .AND. Oscar(n+5)<0 ) THEN
!
!     GET FIRST/NEXT O/P FILE
!
                              m1 = Oscar(n+6)*3 + n + 8
                              m2 = Oscar(m1-1)*3 - 3 + m1
                              DO m = m1 , m2 , 3
                                 IF ( Oscar(m)/=0 ) THEN
                                    IF ( Oscar(m)==Oscar(k) .AND. Oscar(m+1)==Oscar(k+1) ) GOTO 204
                                 ENDIF
                              ENDDO
                           ENDIF
                           n = Oscar(n) + n
                        ENDDO
!
!     EXTEND I/P FILE RANGE TO END OF LOOP
!
 202                    n = rshift(Oscar(k+2),16)
                        iordnl(n+3) = lshift(i,16)
 204                 ENDDO
                     IF ( Start==Iunst ) THEN
!
!     FOR UNMODIFIED RESTART, MARK ALL OUTPUT FILES WITHIN THE
!     LOOP AND BEFORE THE RE-ENTRY POINT FOR REUSE
!
                        kk1 = k1
                        IF ( Oscar(kk1-6)<irentr ) THEN
                           IF ( and(Oscar(kk1-5),Maskhi)==1 ) THEN
                              k1 = k2 + 4
                              k2 = 3*Oscar(k1-1) - 3 + k1
                              DO k = k1 , k2 , 3
                                 IF ( Oscar(k)/=0 ) THEN
                                    nofind = -1
                                    CALL xfldef(Oscar(k),Oscar(k+1),nofind)
                                 ENDIF
                              ENDDO
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
                  Ospnt = Oscar(Ospnt) + Ospnt
               ENDDO
!
!     LOOP SCANNED, GET NEXT OSCAR ENTRY AFTER LOOP ENTRIES
!
               Ospnt = lpbot
            ELSE
!
!     MAKE SURE FIRST INSTRUCTION IN LOOP IS NOT CHKPNT
!
               IF ( Oscar(lptop+3)==xnam(7) .AND. Oscar(lptop+4)==xnam(8) ) THEN
!
!     CHKPNT IS FIRST INSTRUCTION IN LOOP
!
                  CALL xgpidg(47,lptop,0,0)
                  Oscar(lptop+5) = or(Oscar(lptop+5),Isgnon)
               ENDIF
!
!     EXECUTE FLAGS NOT ALL SET - SET FLAGS AND BEGIN OSCAR SCAN AGAIN
!
               j1 = Oscar(lptop+1)
               DO j = j1 , i
                  IF ( Oscar(Ospnt+3)/=xnam(7) .OR. Oscar(Ospnt+4)/=xnam(8) .OR. Icpflg/=0 ) THEN
                     IF ( Oscar(Ospnt+5)>=0 ) THEN
                        IF ( Iflag/=1 ) THEN
                           Iflag = 1
                           CALL page1
                           CALL xgpimw(11,idmpct,0,0)
                        ENDIF
                        CALL xgpimw(4,0,0,Oscar(Ospnt))
                        Oscar(Ospnt+5) = or(Isgnon,Oscar(Ospnt+5))
                     ENDIF
                  ENDIF
                  Ospnt = Oscar(Ospnt) + Ospnt
               ENDDO
               GOTO 100
            ENDIF
         ENDIF
      ELSEIF ( i==4 ) THEN
!
!     PROCESS TYPE E OSCAR ENTRY
!     **************************
!
!     BRANCH ON NAME
!
         DO i = 1 , 11 , 2
            IF ( Oscar(Ospnt+3)==xnam(i) ) THEN
               j = (i+1)/2
               IF ( j==1 .OR. j==2 .OR. j==5 ) GOTO 300
               IF ( j==3 .OR. j==4 ) EXIT
               IF ( j==6 ) GOTO 900
            ENDIF
         ENDDO
!
!     ENTRY IS XUOP OR XCHK - MAKE SURE FILES HAVE BEEN DEFINED OR
!     PREPURGED.
!
         i1 = Ospnt + 7
         i2 = Oscar(Ospnt+6)*2 + i1 - 2
         iopnt = i1
         GOTO 400
      ENDIF
   ENDIF
!
!     GET NEXT OSCAR ENTRY
!
 300  IF ( Ospnt<Osbot ) THEN
      IF ( Oscar(Ospnt+5)<0 .AND. and(Oscar(Ospnt+2),Maskhi)<=2 ) Osprc = Ospnt
      Ospnt = Ospnt + Oscar(Ospnt)
      GOTO 200
!
!     OSCAR HAS BEEN PROCESSED
!     ************************
!
   ELSEIF ( dlyerr==0 ) THEN
!
!     SET  NTU = LTU FOR LAST REFERENCE TO EACH FILE IN OSCAR.
!
      DO i = 4 , Iorbot , 4
         lstuse = and(iordnl(i+2),Maskhi)
         IF ( lstuse/=0 ) THEN
            ntu = or(and(Itape,iordnl(i+2)),rshift(iordnl(i+3),16))
            Oscar(lstuse) = or(ntu,and(Oscar(lstuse),Masklo))
         ENDIF
      ENDDO
!
!     SEARCH FILE TABLE FOR FILES WITH APPEND OR SAVE FLAG UP
!
      IF ( Fpnt>=1 ) THEN
         DO j = 1 , Fpnt , 3
            IF ( and(File(j+2),Iappnd)/=0 .OR. and(File(j+2),Isave)/=0 ) THEN
!
!     FOR RESTART, MARK APPEND AND SAVE FILES FOR REUSE
!
               nofind = -1
               CALL xfldef(File(j),File(j+1),nofind)
               IF ( and(File(j+2),Isave)==0 ) THEN
!
!     APPEND FLAG SET - FIND CORRESPONDING IORDNL ENTRY AND SET FLAG
!
                  DO i = 4 , Iorbot , 4
                     IF ( iordnl(i)==File(j) .AND. iordnl(i+1)==File(j+1) ) iordnl(i+3) = or(Iappnd,iordnl(i+3))
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
!     STORE LTU IN OSCAR FILE ENTRIES
!
      Ospnt = 1
      GOTO 2300
   ELSE
      dlyerr = 0
      GOTO 100
   ENDIF
 400  IF ( Iorbot>0 ) THEN
      DO j = 4 , Iorbot , 4
         IF ( Oscar(iopnt)==iordnl(j) .AND. Oscar(iopnt+1)==iordnl(j+1) ) THEN
            IF ( Start==Iunst .AND. j<=idpl ) THEN
               nnfind = -1
               CALL xfldef(Oscar(iopnt),Oscar(iopnt+1),nnfind)
            ENDIF
            GOTO 700
         ENDIF
      ENDDO
   ENDIF
!
!     FILE NOT IN ORDNAL TABLE - SEE IF IT IS IN PREVIOUS PURGE OR
!     EQUIV ENTRY
!
   k1 = 2
   k1 = Oscar(k1)
   k2 = Oscar(Ospnt+1) - 1
   kk = 1
   DO k = k1 , k2
      IF ( Oscar(kk+3)/=xnam(9) .AND. Oscar(kk+3)/=xnam(11) ) GOTO 600
!
!     PURGE OR EQUIV ENTRY FOUND - SEARCH FOR FILE NAME MATCH
!
      l1 = kk + 7
      l3 = kk + Oscar(kk)
!
!     GET FIRST/NEXT FILE LIST
!
 450  l2 = Oscar(l1-1)*2 + l1 - 2
      incrlp = 2
      IF ( Oscar(kk+3)==xnam(11) ) THEN
         l2 = l2 + 1
         incrlp = 3
      ENDIF
      DO l = l1 , l2 , incrlp
         IF ( Oscar(l)==Oscar(iopnt) .AND. Oscar(l+1)==Oscar(iopnt+1) ) GOTO 800
         IF ( l==l1+incrlp ) GOTO 500
      ENDDO
      GOTO 550
 500  l4 = l1 + incrlp
      incrlp = 2
      l4 = l4 + incrlp
      DO l = l4 , l2 , incrlp
         IF ( Oscar(l)==Oscar(iopnt) .AND. Oscar(l+1)==Oscar(iopnt+1) ) GOTO 800
      ENDDO
 550  l1 = l2 + 4
      IF ( l1<l3 ) GOTO 450
 600  kk = Oscar(kk) + kk
   ENDDO
!
!     FILE IS NOT PURGED OR DEFINED - SEE IF IT IS ON PROBLEM TAPE
!
   nofind = -1
   GOTO 2000
!
!     FILE IS IN ORDNAL TABLE - ENTER RANGE
!
 700  IF ( iordnl(j+3)>=0 ) iordnl(j+3) = lshift(Oscar(Ospnt+1),16)
 800  iopnt = iopnt + 2
   IF ( iopnt>i2 ) GOTO 300
   GOTO 400
!
!     PROCESS EQUIV INSTRUCTION
!
 900  l1 = Ospnt + 7
   nwdh = Oscar(Ospnt) - 6
   DO
      ndatab = Oscar(l1-1)
      iprime = 0
      DO khr = 1 , ndatab
!
!     CHECK FOR DATA BLOCK IN IORDNL
!
         IF ( Iorbot>0 ) THEN
            DO i = 4 , Iorbot , 4
               IF ( iordnl(i)==Oscar(l1) .AND. iordnl(i+1)==Oscar(l1+1) ) THEN
                  IF ( Start==Iunst .AND. i<=idpl ) THEN
                     nnfind = -1
                     CALL xfldef(Oscar(l1),Oscar(l1+1),nnfind)
                  ENDIF
                  GOTO 940
               ENDIF
            ENDDO
         ENDIF
!
!     FILE NOT IN IORDNL, SEE IF ON PTDIC OR REGEN
!
         IF ( Start/=Icst .AND. iprime==0 ) THEN
            nofind = 1
            CALL xfldef(Oscar(l1),Oscar(l1+1),nofind)
            IF ( nofind<0 ) GOTO 100
            IF ( nofind==0 ) GOTO 920
         ENDIF
         IF ( dlyerr==0 .AND. iprime==0 ) THEN
!
!     PRIMARY EQUIV FILE NOT DEFINED
!
            CALL xgpidg(32,Ospnt,Oscar(l1),Oscar(l1+1))
            GOTO 940
         ENDIF
!
!     PUT FILE IN IORDNL, FLAG FOURTH WORD FOR EQUIV
!
 920     Iorbot = Iorbot + 4
         IF ( Iorbot>=Lordnl ) GOTO 2400
         iordnl(Iorbot) = Oscar(l1)
         iordnl(Iorbot+1) = Oscar(l1+1)
         iordnl(Iorbot+2) = 0
         iordnl(Iorbot+3) = Isgnon
 940     IF ( iprime==0 ) THEN
            lstuse = and(Maskhi,iordnl(i+2))
            IF ( lstuse/=0 ) THEN
               ntu = or(Oscar(Ospnt+1),and(iordnl(i+2),Itape))
               Oscar(lstuse) = or(and(Oscar(lstuse),Masklo),ntu)
            ENDIF
            iordnl(i+2) = or(Oscar(l1+2),and(iordnl(i+2),Itape))
            iordnl(i+3) = lshift(Oscar(Ospnt+1),16)
            Oscar(l1+2) = or(and(Oscar(l1+2),Maskhi),lshift(i,16))
         ENDIF
         l1 = l1 + 2
         IF ( iprime==0 ) l1 = l1 + 1
         iprime = 1
      ENDDO
      nwdh = nwdh - 2*ndatab - 3
      IF ( nwdh<=0 ) GOTO 300
      l1 = l1 + 2
   ENDDO
!
!     GET FIRST/NEXT FILE NAME FROM OSCAR
!
 1000 IF ( Oscar(iopnt)==0 ) GOTO 1500
!
!     SEE IF FILE NAME IS ALREADY IN ORDNAL TABLE
!
   IF ( Iorbot>0 ) THEN
      DO k = 4 , Iorbot , 4
         IF ( iordnl(k)==Oscar(iopnt) .AND. iordnl(k+1)==Oscar(iopnt+1) ) THEN
            IF ( Start==Iunst .AND. k<=idpl ) THEN
               nnfind = -1
               CALL xfldef(Oscar(iopnt),Oscar(iopnt+1),nnfind)
            ENDIF
            GOTO 1100
         ENDIF
      ENDDO
   ENDIF
   GOTO 1300
 1100 IF ( iordnl(k+3)>=0 ) THEN
!
!     FILE APPEARS MORE THAN ONCE AS OUTPUT
!
!     SUPPRESS MESSAGE ONCE IF FILE IS INITIALLY UNDEFINED
!
      IF ( Icpbot>=Icptop ) THEN
         DO ii = Icptop , Icpbot , 3
            IF ( Oscar(iopnt)==Icpdpl(ii) .AND. Oscar(iopnt+1)==Icpdpl(ii+1) ) THEN
               IF ( Icpdpl(ii+2)>=0 ) EXIT
               Icpdpl(ii+2) = -Icpdpl(ii+2)
               GOTO 1200
            ENDIF
         ENDDO
      ENDIF
      CALL xgpidg(-45,Ospnt,Oscar(iopnt),Oscar(iopnt+1))
   ENDIF
 1200 kxt = k
   GOTO 1400
!
!     INCREMENT TO NEXT ORDNAL ENTRY AND ENTER FILE NAME AND LU POINTER.
!
 1300 Iorbot = Iorbot + 4
   IF ( Iorbot>=Lordnl ) GOTO 2400
   iordnl(Iorbot) = Oscar(iopnt)
   iordnl(Iorbot+1) = Oscar(iopnt+1)
!
!     SEE IF TAPE FLAG IS SET FOR THIS FILE
!
   kxt = Iorbot
 1400 lstuse = iopnt + 2
   IF ( and(Oscar(Ospnt+2),Maskhi)>2 ) lstuse = 0
   IF ( Fpnt>=1 ) THEN
      DO k = 1 , Fpnt , 3
         IF ( Oscar(iopnt)==File(k) .AND. Oscar(iopnt+1)==File(k+1) ) lstuse = or(lstuse,and(File(k+2),Itape))
      ENDDO
   ENDIF
   iordnl(kxt+2) = lstuse
   iordnl(kxt+3) = lshift(Oscar(Ospnt+1),16)
!
!     IORDNL POINTER  TO OSCAR IF TYPE F OR O FORMAT
!
   IF ( and(Oscar(Ospnt+2),Maskhi)<=2 ) Oscar(iopnt+2) = or(lshift(kxt,16),and(Oscar(iopnt+2),Maskhi))
   GOTO Irturn
!
!     O/P FILE PROCESSED  -  INCREMENT TO NEXT O/P FILE
!
 1500 iopnt = iopnt + 3
   IF ( iopnt<=i ) GOTO 1000
!
!     OUTPUT SECTION SCANNED, NOW SCAN INPUT FILE SECTION OF OSCAR.
!
!     PROCESS TYPE F OR O OSCAR ENTRY
!     *******************************
!
!     SCAN OSCAR INPUT FILE SECTION,ENTER RANGES IN IORDNL TABLE.
!
 1600 k = Ospnt + 7
   i = Oscar(k-1)*3 - 3 + k
   iopnt = k
!
!     GET FIRST/NEXT FILE NAME FROM OSCAR
!
 1700 IF ( Oscar(iopnt)==0 ) GOTO 2200
   nofind = 1
   ASSIGN 2200 TO Irturn
!
!     NOW SCAN IORDNAL TABLE FOR FILE NAME
!
   j1 = lstbot
   IF ( j1>0 ) THEN
      DO j = 4 , j1 , 4
         IF ( Oscar(iopnt)==iordnl(j) .AND. Oscar(iopnt+1)==iordnl(j+1) ) THEN
            IF ( Start==Iunst .AND. j<=idpl ) THEN
               nnfind = -1
               CALL xfldef(Oscar(iopnt),Oscar(iopnt+1),nnfind)
            ENDIF
            GOTO 1800
         ENDIF
      ENDDO
   ENDIF
   GOTO 1900
!
!     FOUND FILE IN IORDNL TABLE - ENTER NTU AND TAPE FLAG INTO
!     OSCAR ENTRY POINTED TO BY IORDNL ENTRY
!
 1800 lstuse = and(Maskhi,iordnl(j+2))
   IF ( lstuse/=0 ) THEN
      ntu = or(Oscar(Ospnt+1),and(iordnl(j+2),Itape))
      Oscar(lstuse) = or(and(Oscar(lstuse),Masklo),ntu)
   ENDIF
!
!     SET RANGE AND LASTUSE POINTER IN IORDNAL ENTRY
!
   nofind = -1
   iordnl(j+2) = or(iopnt+2,and(iordnl(j+2),Itape))
   iordnl(j+3) = lshift(Oscar(Ospnt+1),16)
!
!     LINK OSCAR I/P FILE TO IORDNL ENTRY
!
   Oscar(iopnt+2) = or(and(Oscar(iopnt+2),Maskhi),lshift(j,16))
!
!     I/P FILE PROCESSED - MAKE SURE IT WAS DEFINED
!
 1900 IF ( nofind<0 ) GOTO 2200
!
!     I/P FILE NOT DEFINED
!
 2000 IF ( Start/=Icold ) THEN
!
!     RESTART - SEE IF FILE IS ON PROBLEM TAPE OR CAN BE REGENERATED
!     BY RE-EXECUTING SOME MODULES.
!
      CALL xfldef(Oscar(iopnt),Oscar(iopnt+1),nofind)
      IF ( nofind<0 ) GOTO 100
      IF ( nofind==0 ) GOTO 2100
   ENDIF
!
!     ERROR - FILE NOT DEFINED(PUT OUT MESSAGE AT END OF XFLORD)
!     SEE IF FILE IS ALREADY IN ICPDPL TABLE
!
   IF ( dlyerr==0 ) THEN
      IF ( Icpbot>=Icptop ) THEN
         DO l = Icptop , Icpbot , 3
            IF ( Oscar(iopnt)==Icpdpl(l) .AND. Oscar(iopnt+1)==Icpdpl(l+1) ) GOTO 2100
         ENDDO
      ENDIF
!
!     ENTER FILE IN ICPDPL TABLE
!
      Icpbot = Icpbot + 3
      IF ( Icpbot+3-Icptop>Lcpdpl ) THEN
!
!     ICPDPL TABLE OVERFLOW
!
         CALL xgpidg(14,ncpdp1,ncpdp2,0)
         GOTO 99999
      ELSE
         Icpdpl(Icpbot) = Oscar(iopnt)
         Icpdpl(Icpbot+1) = Oscar(iopnt+1)
         Icpdpl(Icpbot+2) = -Ospnt
      ENDIF
   ENDIF
!
!     ENTER FILE IN ORDNAL TABLE IF NOT CHKPNT MODULE
!
 2100 IF ( Oscar(Ospnt+3)==xnam(7) ) GOTO 800
   GOTO 1300
!
!     CHECK FOR ANOTHER I/P FILE
!
 2200 iopnt = iopnt + 3
!
!     INPUT FILE SECTION SCANNED,GET NEXT OSCAR ENTRY.
!
   IF ( iopnt>i ) GOTO 300
   GOTO 1700
 2300 IF ( Oscar(Ospnt+5)<0 .AND. and(Oscar(Ospnt+2),Maskhi)<=2 ) THEN
      k = Ospnt + 7
      j = 1
      IF ( and(Oscar(Ospnt+2),Maskhi)==1 ) j = 2
      DO l = 1 , j
!
         i = Oscar(k-1)*3 - 3 + k
         DO iopnt = k , i , 3
            IF ( Oscar(iopnt)/=0 ) THEN
               j1 = rshift(Oscar(iopnt+2),16)
               ltu = and(Oscar(iopnt+2),or(Losgn,Maskhi))
               Oscar(iopnt+2) = or(ltu,iordnl(j1+3))
            ENDIF
         ENDDO
         k = i + 4
      ENDDO
   ENDIF
   IF ( Oscar(Ospnt+3)==xnam(11) ) THEN
      i = Oscar(Ospnt) - 6
      k = Ospnt + 7
      DO
         j1 = rshift(Oscar(k+2),16)
         ltu = and(Oscar(k+2),or(Losgn,Maskhi))
         Oscar(k+2) = or(ltu,iordnl(j1+3))
         i = i - 2*Oscar(k-1) - 3
         IF ( i<=0 ) EXIT
         k = k + 2*Oscar(k-1) + 3
      ENDDO
   ENDIF
   Ospnt = Ospnt + Oscar(Ospnt)
   IF ( Ospnt<=Osbot ) GOTO 2300
!
!     STORE LTU IN FIAT ENTRIES
!
   i = Ifiat(3)*Icfiat - 2
   DO k = 4 , i , Icfiat
      IF ( Ifiat(k+1)/=0 ) THEN
         j = rshift(and(Ifiat(k),Masklo),16)
!
!     SEE IF FILE HAS BEEN REFERENCED
!
         IF ( and(iordnl(j+3),compl(Iappnd))/=0 ) THEN
            ltu = and(Ifiat(k),or(or(Isgnon,Losgn),Maskhi))
            Ifiat(k) = or(ltu,iordnl(j+3))
         ELSE
!
!     FILE NOT USED - DROP IT FROM FIAT
!
            Ifiat(k) = and(Ifiat(k),or(Maskhi,Losgn))
            k1 = k + 1
            k2 = k + Icfiat - 3
            DO kk = k1 , k2
               Ifiat(kk) = 0
            ENDDO
         ENDIF
      ENDIF
   ENDDO
!
!     CHECK ICPDPL TABLE FOR UNDEFINED FILES
!
   IF ( Icpbot<Icptop ) THEN
!
!     NO UNDEFINED FILES - CHECK FOR RESTART
!
      IF ( Start/=Icst ) GOTO 2500
      GOTO 99999
   ELSE
      DO i = Icptop , Icpbot , 3
         CALL xgpidg(-22,iabs(Icpdpl(i+2)),Icpdpl(i),Icpdpl(i+1))
      ENDDO
!
!     IF DIAG 14 IS NOT ON, AND THERE ARE UNDEFINED FILES FROM USER'S
!     ALTER (DIAG14 IS SET TO 10 BY XGPI AT THIS TIME), SET DIAG14 TO 11
!     TO FLAG XGPI TO PRINT THE DMAP COMPILE LISTING.
!
!     IF DIAG 14 IS ON, THE DMAP LISTING IS ALREADY PRINTTED BY XSCNDM,
!     SHICH IS CALLED BY XOSGEN. XOSGEN IS CALLED BY XGPI BEFORE THIS
!     XFLORD IS CALLED (ALSO BY XGPI)
!
      IF ( Diag14==10 ) Diag14 = 11
      IF ( Start==Icst ) GOTO 99999
      GOTO 2500
   ENDIF
!
!     ERROR MESSAGES
!     **************
!
!     IORDNL TABLE OVERFLOW
!
 2400 CALL xgpidg(14,nordn1,nordn2,and(Oscar(Ospnt+5),Nosgn))
   GOTO 99999
!
!     RESTART - USE LAST XVPS ENTRY IN PTDIC FOR RESTART.
!     EXCLUDE FIRST NXVPS ENTRY
!
 2500 Ptdtop = Ptdtop + 3
   nofind = -1
   CALL xfldef(nxvps,Nblank,nofind)
   Ptdtop = Ptdtop - 3
!
!     OVERLAY PTDIC TABLE WITH ICPDPL TABLE
!
   Icptop = Ptdtop
   Icpbot = Icptop - 3
   Lcpdpl = Lptdic
!
!     SCAN PTDIC FOR REUSE FLAGS
!
   DO j = Ptdtop , Ptdbot , 3
      IF ( and(Ptdic(j+2),Reuse)/=0 ) THEN
!
!     REUSE FLAG UP - ENTER FILE IN ICPDPL
!
         Icpbot = Icpbot + 3
         Icpdpl(Icpbot) = Ptdic(j)
         Icpdpl(Icpbot+1) = Ptdic(j+1)
         Icpdpl(Icpbot+2) = Ptdic(j+2)
      ENDIF
   ENDDO
!
!     ORDER FILES IN ICPDPL BY REEL/FILE NUMBER
!
   IF ( Icpbot<Icptop ) GOTO 99999
!
!     DO NOT DISTURB EXISTING ORDER
!
   IF ( Icpbot==Icptop ) GOTO 2700
   k = Icptop
   l = k
 2600 DO WHILE ( and(Icpdpl(k+2),Noflgs)>and(Icpdpl(k+5),Noflgs) )
!
!     SWITCH
!
      DO m = 1 , 3
         j = k + m + 2
         Itmp(1) = Icpdpl(j)
         Icpdpl(j) = Icpdpl(j-3)
         Icpdpl(j-3) = Itmp(1)
      ENDDO
      k = k - 3
      IF ( k<Icptop ) EXIT
   ENDDO
   k = l + 3
   IF ( k<Icpbot ) THEN
      l = k
      GOTO 2600
   ENDIF
!
!     ENTER PURGED FILE IN FIAT IF THERE IS NO POSSIBLE WAY TO GENERATE
!     FILE
!
 2700 j1 = 2
   j1 = Oscar(j1)
   j2 = Oscar(Osbot+1)
   DO i = Icptop , Icpbot , 3
      IF ( and(Icpdpl(i+2),Maskhi)/=0 ) EXIT
      Ospnt = 1
      DO j = j1 , j2
         IF ( and(Maskhi,Oscar(Ospnt+2))<=2 .AND. Oscar(Ospnt+5)<0 ) THEN
!
!     SEE IF PURGED FILE IS IN I/P SECTION
!
            k1 = Ospnt + 7
            k2 = Oscar(k1-1)*3 - 3 + k1
            DO k = k1 , k2 , 3
               IF ( Oscar(k)==Icpdpl(i) .AND. Oscar(k+1)==Icpdpl(i+1) ) GOTO 2710
            ENDDO
!
!     PURGED FILE IS NOT IN I/P SECTION - SEARCH O/P SECTION FOR IT.
!
            IF ( and(Maskhi,Oscar(Ospnt+2))==1 ) THEN
               k1 = Oscar(Ospnt+6)*3 + Ospnt + 8
               k2 = Oscar(k1-1)*3 - 3 + k1
               DO k = k1 , k2 , 3
                  IF ( Oscar(k)==Icpdpl(i) .AND. Oscar(k+1)==Icpdpl(i+1) ) GOTO 2800
               ENDDO
            ENDIF
            GOTO 2720
!
!     PURGED FILE FIRST USED AS INPUT - THEREFORE IT CANNOT BE GENERATED
!     ENTER PURGED FILE IN FIAT
!
 2710       l = Ifiat(3)*Icfiat + 4
            Ifiat(3) = Ifiat(3) + 1
            Ifiat(l) = or(Maskhi,Oscar(k+2))
            Ifiat(l+1) = Oscar(k)
            Ifiat(l+2) = Oscar(k+1)
            EXIT
         ENDIF
 2720    Ospnt = Oscar(Ospnt) + Ospnt
      ENDDO
 2800 ENDDO
99999 RETURN
END SUBROUTINE xflord
