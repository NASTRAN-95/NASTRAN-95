!*==xflord.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE C_SYSTEM
   USE C_TWO
   USE C_XDPL
   USE C_XFIAT
   USE C_XFIST
   USE C_XGPI2
   USE C_XGPI4
   USE C_XGPI5
   USE C_XGPI6
   USE C_XGPI7
   USE C_XGPI8
   USE C_XGPIC
   USE C_XGPID
   USE C_XOLDPT
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: and , compl , or
   INTEGER , SAVE :: dlyerr , ncpdp1 , ncpdp2 , nordn1 , nordn2 , nthpas , nxvps
   INTEGER :: i , i1 , i2 , idmpct , idpl , ifeq , ii , incrlp , iopnt , iorbot , iprime , irentr , j , j1 , j2 , k , k1 , k2 ,     &
            & khr , kk , kk1 , kxt , l , l1 , l2 , l3 , l4 , lordnl , loscar , lpbot , lptop , lstbot , lstdpl , lstuse , ltu , m , &
            & m1 , m2 , maxdpl , n , ndatab , ndpfil , nnfind , nofind , ntu , nwdh , osbot , ospnt , osprc , reuse
   INTEGER , DIMENSION(1) :: icpdpl , itmp , ptdic
   INTEGER , DIMENSION(1800) :: iordnl
   INTEGER , DIMENSION(5) :: os
   INTEGER , DIMENSION(2) :: oscar
   INTEGER , DIMENSION(12) , SAVE :: xnam
   EXTERNAL andf , complf , lshift , orf , page1 , rshift , xfldef , xgpidg , xgpimw
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Core(1),Loscar,Os(1)) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Ospnt,Os(4)) , (Oscar(1),Os(5),Ptdic(1)) ,                 &
!>>>>    & (Dmap(1),Itmp(1)) , (Oscar(1),Icpdpl(1)) , (Lmpl,Lordnl) , (Mplpnt,Iorbot) , (Dpl(1),Ndpfil) , (Dpl(2),Maxdpl) ,              &
!>>>>    & (Dpl(3),Lstdpl) , (Two(4),Reuse)
   DATA nordn1/4HIORD/ , nordn2/4HNL  / , nxvps/4HXVPS/ , ncpdp1/4HICPD/ , ncpdp2/4HPL  / , xnam/4HXTIM , 4HE    , 4HXSAV , 4HE    ,&
       &4HXUOP , 4H     , 4HXCHK , 4H     , 4HXPUR , 4HGE   , 4HXEQU , 4HIV  /
   DATA nthpas/0/ , dlyerr/0/
!
   or(i,j) = orf(i,j)
   and(i,j) = andf(i,j)
   compl(l) = complf(l)
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
   INTEGER :: spag_nextblock_4
   INTEGER :: spag_nextblock_5
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
      CASE (2)
!
!     PREPARE FOR NTH PASS THRU OSCAR
!     *******************************
!
         IF ( Nogo>1 ) RETURN
         ospnt = 1
         osprc = ospnt
         iorbot = 0
         ifeq = 0
!
!     INCREMENT NUMBER OF PASSES MADE THRU OSCAR
!
         nthpas = 1 + nthpas
!
!     ENTER DPL FILE NAMES IN IORDNL TABLE
!
         i = lstdpl*3 + 1
         idpl = i
         IF ( lstdpl/=0 ) THEN
            DO k = 4 , i , 3
               iorbot = iorbot + 4
               iordnl(iorbot) = Dpl(k)
               iordnl(iorbot+1) = Dpl(k+1)
               iordnl(iorbot+2) = 0
               iordnl(iorbot+3) = 0
            ENDDO
         ENDIF
!
!     ENTER FIAT NAMES IN IORDNL TABLE
!
         i = Ifiat(3)*Icfiat - 2
         DO k = 4 , i , Icfiat
            IF ( Ifiat(k+1)/=0 ) THEN
               iorbot = iorbot + 4
               Ifiat(k) = or(lshift(iorbot,16),and(Ifiat(k),orf(Maskhi,Losgn)))
               iordnl(iorbot) = Ifiat(k+1)
               iordnl(iorbot+1) = Ifiat(k+2)
               iordnl(iorbot+2) = 0
               iordnl(iorbot+3) = 0
            ENDIF
         ENDDO
!
!     FOR UNMODIFIED RESTART BEGIN OSCAR PROCESSING AT RE-ENTRY POINT IF
!     THIS IS FIRST PASS THRU OSCAR
!
         IF ( nthpas<=1 ) THEN
            IF ( Start==Iunst .AND. irentr/=0 ) THEN
               SPAG_Loop_1_1: DO j = 1 , irentr
                  IF ( oscar(ospnt+1)>=irentr ) EXIT SPAG_Loop_1_1
                  osprc = ospnt
                  ospnt = ospnt + oscar(ospnt)
               ENDDO SPAG_Loop_1_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     GET NEXT OSCAR ENTRY
!     ********************
!
!     BRANCH ON OSCAR ENTRY TYPE IF EXECUTE FLAG IS UP
!
         IF ( oscar(ospnt+5)<0 ) THEN
            i = and(oscar(ospnt+2),Maskhi)
            lstbot = iorbot
            IF ( i==1 ) THEN
!
!     PROCESS TYPE F OSCAR ENTRY
!     **************************
!
!     SCAN OSCAR OUTPUT FILE SECTION,ENTER NAMES IN IORDNL TABLE.
!
               k = ospnt + 6
               k = oscar(k)*3 + 2 + k
               i = oscar(k-1)*3 - 3 + k
               iopnt = k
               ASSIGN 20 TO Irturn
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==2 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( i==3 ) THEN
!
!     PROCESS TYPE C OSCAR ENTRY
!     **************************
!
!     CHECK FOR LOOPING
!
               lptop = rshift(oscar(ospnt+6),16)
               IF ( (Nexit/=oscar(ospnt+3)) .AND. (oscar(ospnt+1)>=lptop) ) THEN
!
!     FIND BEGINNING OF LOOP AND ADJUST IORDNL RANGES INSIDE LOOP.
!
                  lpbot = ospnt
                  i = oscar(ospnt+1)
                  ospnt = 1
                  j1 = oscar(ospnt+1)
                  SPAG_Loop_1_2: DO j = j1 , i
                     IF ( oscar(ospnt+1)==lptop ) EXIT SPAG_Loop_1_2
                     ospnt = oscar(ospnt) + ospnt
                  ENDDO SPAG_Loop_1_2
                  lptop = ospnt
!
!     LOOP TOP FOUND - IF UNMODIFIED RESTART,EXECUTE ALL MODULES INSIDE
!     LOOP.
!
                  IF ( oscar(lptop+5)<0 .OR. Start/=Iunst ) THEN
!
!     EXTEND RANGE OF FILES DEFINED OUTSIDE OF LOOP IF USED INSIDE LOOP
!     GET FIRST/NEXT OSCAR ENTRY INSIDE LOOP
!
                     ospnt = lptop
                     j1 = oscar(lptop+1)
                     j2 = oscar(lpbot+1)
                     DO j = j1 , j2
                        IF ( and(oscar(ospnt+2),Maskhi)<=2 ) THEN
!
!     GET FIRST/NEXT I/P FILE OF OSCAR ENTRY
!
                           k1 = ospnt + 7
                           k2 = oscar(k1-1)*3 - 3 + k1
                           SPAG_Loop_2_4: DO k = k1 , k2 , 3
                              spag_nextblock_2 = 1
                              SPAG_DispatchLoop_2: DO
                                 SELECT CASE (spag_nextblock_2)
                                 CASE (1)
                                    IF ( oscar(k)==0 ) CYCLE
!
!     SEE IF FILE SAVE IS ON
!
                                    IF ( Fpnt>=1 ) THEN
                                       SPAG_Loop_3_3: DO l = 1 , Fpnt , 3
                                         IF ( oscar(k)==File(l) .AND. oscar(k+1)==File(l+1) ) THEN
                                         IF ( and(Isave,File(l+2))/=Isave ) EXIT SPAG_Loop_3_3
                                         spag_nextblock_2 = 2
                                         CYCLE SPAG_DispatchLoop_2
                                         ENDIF
                                       ENDDO SPAG_Loop_3_3
                                    ENDIF
!
!     FILE SAVE FLAG NOT ON - SEE IF I/P FILE IS GENERATED INSIDE LOOP
!
                                    l1 = oscar(ospnt+1)
!
!     GET FIRST/NEXT OSCAR ENTRY INSIDE LOOP
!
                                    n = lptop
                                    DO l = j1 , l1
                                       IF ( and(oscar(n+2),Maskhi)==1 .AND. oscar(n+5)<0 ) THEN
!
!     GET FIRST/NEXT O/P FILE
!
                                         m1 = oscar(n+6)*3 + n + 8
                                         m2 = oscar(m1-1)*3 - 3 + m1
                                         DO m = m1 , m2 , 3
                                         IF ( oscar(m)/=0 ) THEN
                                         IF ( oscar(m)==oscar(k) .AND. oscar(m+1)==oscar(k+1) ) CYCLE SPAG_Loop_2_4
                                         ENDIF
                                         ENDDO
                                       ENDIF
                                       n = oscar(n) + n
                                    ENDDO
                                    spag_nextblock_2 = 2
                                 CASE (2)
!
!     EXTEND I/P FILE RANGE TO END OF LOOP
!
                                    n = rshift(oscar(k+2),16)
                                    iordnl(n+3) = lshift(i,16)
                                    EXIT SPAG_DispatchLoop_2
                                 END SELECT
                              ENDDO SPAG_DispatchLoop_2
                           ENDDO SPAG_Loop_2_4
                           IF ( Start==Iunst ) THEN
!
!     FOR UNMODIFIED RESTART, MARK ALL OUTPUT FILES WITHIN THE
!     LOOP AND BEFORE THE RE-ENTRY POINT FOR REUSE
!
                              kk1 = k1
                              IF ( oscar(kk1-6)<irentr ) THEN
                                 IF ( and(oscar(kk1-5),Maskhi)==1 ) THEN
                                    k1 = k2 + 4
                                    k2 = 3*oscar(k1-1) - 3 + k1
                                    DO k = k1 , k2 , 3
                                       IF ( oscar(k)/=0 ) THEN
                                         nofind = -1
                                         CALL xfldef(oscar(k),oscar(k+1),nofind)
                                       ENDIF
                                    ENDDO
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                        ospnt = oscar(ospnt) + ospnt
                     ENDDO
!
!     LOOP SCANNED, GET NEXT OSCAR ENTRY AFTER LOOP ENTRIES
!
                     ospnt = lpbot
                  ELSE
!
!     MAKE SURE FIRST INSTRUCTION IN LOOP IS NOT CHKPNT
!
                     IF ( oscar(lptop+3)==xnam(7) .AND. oscar(lptop+4)==xnam(8) ) THEN
!
!     CHKPNT IS FIRST INSTRUCTION IN LOOP
!
                        CALL xgpidg(47,lptop,0,0)
                        oscar(lptop+5) = or(oscar(lptop+5),Isgnon)
                     ENDIF
!
!     EXECUTE FLAGS NOT ALL SET - SET FLAGS AND BEGIN OSCAR SCAN AGAIN
!
                     j1 = oscar(lptop+1)
                     DO j = j1 , i
                        IF ( oscar(ospnt+3)/=xnam(7) .OR. oscar(ospnt+4)/=xnam(8) .OR. Icpflg/=0 ) THEN
                           IF ( oscar(ospnt+5)>=0 ) THEN
                              IF ( Iflag/=1 ) THEN
                                 Iflag = 1
                                 CALL page1
                                 CALL xgpimw(11,idmpct,0,0)
                              ENDIF
                              CALL xgpimw(4,0,0,oscar(ospnt))
                              oscar(ospnt+5) = or(Isgnon,oscar(ospnt+5))
                           ENDIF
                        ENDIF
                        ospnt = oscar(ospnt) + ospnt
                     ENDDO
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ELSEIF ( i==4 ) THEN
!
!     PROCESS TYPE E OSCAR ENTRY
!     **************************
!
!     BRANCH ON NAME
!
               SPAG_Loop_1_5: DO i = 1 , 11 , 2
                  IF ( oscar(ospnt+3)==xnam(i) ) THEN
                     j = (i+1)/2
                     IF ( j==1 .OR. j==2 .OR. j==5 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( j==3 .OR. j==4 ) EXIT SPAG_Loop_1_5
                     IF ( j==6 ) THEN
                        spag_nextblock_1 = 8
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_5
!
!     ENTRY IS XUOP OR XCHK - MAKE SURE FILES HAVE BEEN DEFINED OR
!     PREPURGED.
!
               i1 = ospnt + 7
               i2 = oscar(ospnt+6)*2 + i1 - 2
               iopnt = i1
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     GET NEXT OSCAR ENTRY
!
         IF ( ospnt<osbot ) THEN
            IF ( oscar(ospnt+5)<0 .AND. and(oscar(ospnt+2),Maskhi)<=2 ) osprc = ospnt
            ospnt = ospnt + oscar(ospnt)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
!
!     OSCAR HAS BEEN PROCESSED
!     ************************
!
         ELSEIF ( dlyerr==0 ) THEN
!
!     SET  NTU = LTU FOR LAST REFERENCE TO EACH FILE IN OSCAR.
!
            DO i = 4 , iorbot , 4
               lstuse = and(iordnl(i+2),Maskhi)
               IF ( lstuse/=0 ) THEN
                  ntu = or(and(Itape,iordnl(i+2)),rshift(iordnl(i+3),16))
                  oscar(lstuse) = or(ntu,and(oscar(lstuse),Masklo))
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
                        DO i = 4 , iorbot , 4
                           IF ( iordnl(i)==File(j) .AND. iordnl(i+1)==File(j+1) ) iordnl(i+3) = or(Iappnd,iordnl(i+3))
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
!     STORE LTU IN OSCAR FILE ENTRIES
!
            ospnt = 1
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ELSE
            dlyerr = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (5)
         IF ( iorbot>0 ) THEN
            DO j = 4 , iorbot , 4
               IF ( oscar(iopnt)==iordnl(j) .AND. oscar(iopnt+1)==iordnl(j+1) ) THEN
                  IF ( Start==Iunst .AND. j<=idpl ) THEN
                     nnfind = -1
                     CALL xfldef(oscar(iopnt),oscar(iopnt+1),nnfind)
                  ENDIF
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     FILE NOT IN ORDNAL TABLE - SEE IF IT IS IN PREVIOUS PURGE OR
!     EQUIV ENTRY
!
         k1 = 2
         k1 = oscar(k1)
         k2 = oscar(ospnt+1) - 1
         kk = 1
         DO k = k1 , k2
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  IF ( oscar(kk+3)/=xnam(9) .AND. oscar(kk+3)/=xnam(11) ) THEN
                     spag_nextblock_3 = 5
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
!
!     PURGE OR EQUIV ENTRY FOUND - SEARCH FOR FILE NAME MATCH
!
                  l1 = kk + 7
                  l3 = kk + oscar(kk)
                  spag_nextblock_3 = 2
               CASE (2)
!
!     GET FIRST/NEXT FILE LIST
!
                  l2 = oscar(l1-1)*2 + l1 - 2
                  incrlp = 2
                  IF ( oscar(kk+3)==xnam(11) ) THEN
                     l2 = l2 + 1
                     incrlp = 3
                  ENDIF
                  DO l = l1 , l2 , incrlp
                     IF ( oscar(l)==oscar(iopnt) .AND. oscar(l+1)==oscar(iopnt+1) ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( l==l1+incrlp ) THEN
                        spag_nextblock_3 = 3
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                  ENDDO
                  spag_nextblock_3 = 4
                  CYCLE SPAG_DispatchLoop_3
               CASE (3)
                  l4 = l1 + incrlp
                  incrlp = 2
                  l4 = l4 + incrlp
                  DO l = l4 , l2 , incrlp
                     IF ( oscar(l)==oscar(iopnt) .AND. oscar(l+1)==oscar(iopnt+1) ) THEN
                        spag_nextblock_1 = 7
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  spag_nextblock_3 = 4
               CASE (4)
                  l1 = l2 + 4
                  IF ( l1<l3 ) THEN
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
                  spag_nextblock_3 = 5
               CASE (5)
                  kk = oscar(kk) + kk
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
!
!     FILE IS NOT PURGED OR DEFINED - SEE IF IT IS ON PROBLEM TAPE
!
         nofind = -1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
!     FILE IS IN ORDNAL TABLE - ENTER RANGE
!
         IF ( iordnl(j+3)>=0 ) iordnl(j+3) = lshift(oscar(ospnt+1),16)
         spag_nextblock_1 = 7
      CASE (7)
         iopnt = iopnt + 2
         IF ( iopnt<=i2 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
!
!     PROCESS EQUIV INSTRUCTION
!
         l1 = ospnt + 7
         nwdh = oscar(ospnt) - 6
         DO
            ndatab = oscar(l1-1)
            iprime = 0
            DO khr = 1 , ndatab
               spag_nextblock_4 = 1
               SPAG_DispatchLoop_4: DO
                  SELECT CASE (spag_nextblock_4)
                  CASE (1)
!
!     CHECK FOR DATA BLOCK IN IORDNL
!
                     IF ( iorbot>0 ) THEN
                        DO i = 4 , iorbot , 4
                           IF ( iordnl(i)==oscar(l1) .AND. iordnl(i+1)==oscar(l1+1) ) THEN
                              IF ( Start==Iunst .AND. i<=idpl ) THEN
                                 nnfind = -1
                                 CALL xfldef(oscar(l1),oscar(l1+1),nnfind)
                              ENDIF
                              spag_nextblock_4 = 3
                              CYCLE SPAG_DispatchLoop_4
                           ENDIF
                        ENDDO
                     ENDIF
!
!     FILE NOT IN IORDNL, SEE IF ON PTDIC OR REGEN
!
                     IF ( Start/=Icst .AND. iprime==0 ) THEN
                        nofind = 1
                        CALL xfldef(oscar(l1),oscar(l1+1),nofind)
                        IF ( nofind<0 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        IF ( nofind==0 ) THEN
                           spag_nextblock_4 = 2
                           CYCLE SPAG_DispatchLoop_4
                        ENDIF
                     ENDIF
                     IF ( dlyerr==0 .AND. iprime==0 ) THEN
!
!     PRIMARY EQUIV FILE NOT DEFINED
!
                        CALL xgpidg(32,ospnt,oscar(l1),oscar(l1+1))
                        spag_nextblock_4 = 3
                        CYCLE SPAG_DispatchLoop_4
                     ENDIF
                     spag_nextblock_4 = 2
                  CASE (2)
!
!     PUT FILE IN IORDNL, FLAG FOURTH WORD FOR EQUIV
!
                     iorbot = iorbot + 4
                     IF ( iorbot>=lordnl ) THEN
                        spag_nextblock_1 = 21
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     iordnl(iorbot) = oscar(l1)
                     iordnl(iorbot+1) = oscar(l1+1)
                     iordnl(iorbot+2) = 0
                     iordnl(iorbot+3) = Isgnon
                     spag_nextblock_4 = 3
                  CASE (3)
                     IF ( iprime==0 ) THEN
                        lstuse = and(Maskhi,iordnl(i+2))
                        IF ( lstuse/=0 ) THEN
                           ntu = or(oscar(ospnt+1),and(iordnl(i+2),Itape))
                           oscar(lstuse) = or(and(oscar(lstuse),Masklo),ntu)
                        ENDIF
                        iordnl(i+2) = or(oscar(l1+2),and(iordnl(i+2),Itape))
                        iordnl(i+3) = lshift(oscar(ospnt+1),16)
                        oscar(l1+2) = or(and(oscar(l1+2),Maskhi),lshift(i,16))
                     ENDIF
                     l1 = l1 + 2
                     IF ( iprime==0 ) l1 = l1 + 1
                     iprime = 1
                     EXIT SPAG_DispatchLoop_4
                  END SELECT
               ENDDO SPAG_DispatchLoop_4
            ENDDO
            nwdh = nwdh - 2*ndatab - 3
            IF ( nwdh<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            l1 = l1 + 2
         ENDDO
         spag_nextblock_1 = 9
      CASE (9)
!
!     GET FIRST/NEXT FILE NAME FROM OSCAR
!
         IF ( oscar(iopnt)==0 ) GOTO 20
!
!     SEE IF FILE NAME IS ALREADY IN ORDNAL TABLE
!
         IF ( iorbot>0 ) THEN
            DO k = 4 , iorbot , 4
               IF ( iordnl(k)==oscar(iopnt) .AND. iordnl(k+1)==oscar(iopnt+1) ) THEN
                  IF ( Start==Iunst .AND. k<=idpl ) THEN
                     nnfind = -1
                     CALL xfldef(oscar(iopnt),oscar(iopnt+1),nnfind)
                  ENDIF
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         IF ( iordnl(k+3)>=0 ) THEN
!
!     FILE APPEARS MORE THAN ONCE AS OUTPUT
!
!     SUPPRESS MESSAGE ONCE IF FILE IS INITIALLY UNDEFINED
!
            IF ( Icpbot>=Icptop ) THEN
               SPAG_Loop_1_6: DO ii = Icptop , Icpbot , 3
                  IF ( oscar(iopnt)==icpdpl(ii) .AND. oscar(iopnt+1)==icpdpl(ii+1) ) THEN
                     IF ( icpdpl(ii+2)>=0 ) EXIT SPAG_Loop_1_6
                     icpdpl(ii+2) = -icpdpl(ii+2)
                     spag_nextblock_1 = 11
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_1_6
            ENDIF
            CALL xgpidg(-45,ospnt,oscar(iopnt),oscar(iopnt+1))
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         kxt = k
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
!
!     INCREMENT TO NEXT ORDNAL ENTRY AND ENTER FILE NAME AND LU POINTER.
!
         iorbot = iorbot + 4
         IF ( iorbot>=lordnl ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iordnl(iorbot) = oscar(iopnt)
         iordnl(iorbot+1) = oscar(iopnt+1)
!
!     SEE IF TAPE FLAG IS SET FOR THIS FILE
!
         kxt = iorbot
         spag_nextblock_1 = 13
      CASE (13)
         lstuse = iopnt + 2
         IF ( and(oscar(ospnt+2),Maskhi)>2 ) lstuse = 0
         IF ( Fpnt>=1 ) THEN
            DO k = 1 , Fpnt , 3
               IF ( oscar(iopnt)==File(k) .AND. oscar(iopnt+1)==File(k+1) ) lstuse = or(lstuse,and(File(k+2),Itape))
            ENDDO
         ENDIF
         iordnl(kxt+2) = lstuse
         iordnl(kxt+3) = lshift(oscar(ospnt+1),16)
!
!     IORDNL POINTER  TO OSCAR IF TYPE F OR O FORMAT
!
         IF ( and(oscar(ospnt+2),Maskhi)<=2 ) oscar(iopnt+2) = or(lshift(kxt,16),and(oscar(iopnt+2),Maskhi))
         GOTO Irturn
!
!     O/P FILE PROCESSED  -  INCREMENT TO NEXT O/P FILE
!
 20      iopnt = iopnt + 3
         IF ( iopnt<=i ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
!
!     OUTPUT SECTION SCANNED, NOW SCAN INPUT FILE SECTION OF OSCAR.
!
!     PROCESS TYPE F OR O OSCAR ENTRY
!     *******************************
!
!     SCAN OSCAR INPUT FILE SECTION,ENTER RANGES IN IORDNL TABLE.
!
         k = ospnt + 7
         i = oscar(k-1)*3 - 3 + k
         iopnt = k
         spag_nextblock_1 = 15
      CASE (15)
!
!     GET FIRST/NEXT FILE NAME FROM OSCAR
!
         IF ( oscar(iopnt)==0 ) GOTO 40
         nofind = 1
         ASSIGN 40 TO Irturn
!
!     NOW SCAN IORDNAL TABLE FOR FILE NAME
!
         j1 = lstbot
         IF ( j1>0 ) THEN
            DO j = 4 , j1 , 4
               IF ( oscar(iopnt)==iordnl(j) .AND. oscar(iopnt+1)==iordnl(j+1) ) THEN
                  IF ( Start==Iunst .AND. j<=idpl ) THEN
                     nnfind = -1
                     CALL xfldef(oscar(iopnt),oscar(iopnt+1),nnfind)
                  ENDIF
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
!
!     FOUND FILE IN IORDNL TABLE - ENTER NTU AND TAPE FLAG INTO
!     OSCAR ENTRY POINTED TO BY IORDNL ENTRY
!
         lstuse = and(Maskhi,iordnl(j+2))
         IF ( lstuse/=0 ) THEN
            ntu = or(oscar(ospnt+1),and(iordnl(j+2),Itape))
            oscar(lstuse) = or(and(oscar(lstuse),Masklo),ntu)
         ENDIF
!
!     SET RANGE AND LASTUSE POINTER IN IORDNAL ENTRY
!
         nofind = -1
         iordnl(j+2) = or(iopnt+2,and(iordnl(j+2),Itape))
         iordnl(j+3) = lshift(oscar(ospnt+1),16)
!
!     LINK OSCAR I/P FILE TO IORDNL ENTRY
!
         oscar(iopnt+2) = or(and(oscar(iopnt+2),Maskhi),lshift(j,16))
         spag_nextblock_1 = 17
      CASE (17)
!
!     I/P FILE PROCESSED - MAKE SURE IT WAS DEFINED
!
         IF ( nofind<0 ) GOTO 40
         spag_nextblock_1 = 18
      CASE (18)
!
!     I/P FILE NOT DEFINED
!
         IF ( Start/=Icold ) THEN
!
!     RESTART - SEE IF FILE IS ON PROBLEM TAPE OR CAN BE REGENERATED
!     BY RE-EXECUTING SOME MODULES.
!
            CALL xfldef(oscar(iopnt),oscar(iopnt+1),nofind)
            IF ( nofind<0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nofind==0 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     ERROR - FILE NOT DEFINED(PUT OUT MESSAGE AT END OF XFLORD)
!     SEE IF FILE IS ALREADY IN ICPDPL TABLE
!
         IF ( dlyerr==0 ) THEN
            IF ( Icpbot>=Icptop ) THEN
               DO l = Icptop , Icpbot , 3
                  IF ( oscar(iopnt)==icpdpl(l) .AND. oscar(iopnt+1)==icpdpl(l+1) ) THEN
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
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
               RETURN
            ELSE
               icpdpl(Icpbot) = oscar(iopnt)
               icpdpl(Icpbot+1) = oscar(iopnt+1)
               icpdpl(Icpbot+2) = -ospnt
            ENDIF
         ENDIF
         spag_nextblock_1 = 19
      CASE (19)
!
!     ENTER FILE IN ORDNAL TABLE IF NOT CHKPNT MODULE
!
         IF ( oscar(ospnt+3)/=xnam(7) ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
!
!     CHECK FOR ANOTHER I/P FILE
!
 40      iopnt = iopnt + 3
!
!     INPUT FILE SECTION SCANNED,GET NEXT OSCAR ENTRY.
!
         IF ( iopnt<=i ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (20)
         IF ( oscar(ospnt+5)<0 .AND. and(oscar(ospnt+2),Maskhi)<=2 ) THEN
            k = ospnt + 7
            j = 1
            IF ( and(oscar(ospnt+2),Maskhi)==1 ) j = 2
            DO l = 1 , j
!
               i = oscar(k-1)*3 - 3 + k
               DO iopnt = k , i , 3
                  IF ( oscar(iopnt)/=0 ) THEN
                     j1 = rshift(oscar(iopnt+2),16)
                     ltu = and(oscar(iopnt+2),or(Losgn,Maskhi))
                     oscar(iopnt+2) = or(ltu,iordnl(j1+3))
                  ENDIF
               ENDDO
               k = i + 4
            ENDDO
         ENDIF
         IF ( oscar(ospnt+3)==xnam(11) ) THEN
            i = oscar(ospnt) - 6
            k = ospnt + 7
            SPAG_Loop_1_7: DO
               j1 = rshift(oscar(k+2),16)
               ltu = and(oscar(k+2),or(Losgn,Maskhi))
               oscar(k+2) = or(ltu,iordnl(j1+3))
               i = i - 2*oscar(k-1) - 3
               IF ( i<=0 ) EXIT SPAG_Loop_1_7
               k = k + 2*oscar(k-1) + 3
            ENDDO SPAG_Loop_1_7
         ENDIF
         ospnt = ospnt + oscar(ospnt)
         IF ( ospnt<=osbot ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
            IF ( Start==Icst ) RETURN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO i = Icptop , Icpbot , 3
               CALL xgpidg(-22,iabs(icpdpl(i+2)),icpdpl(i),icpdpl(i+1))
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
            IF ( Start/=Icst ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
      CASE (21)
!
!     ERROR MESSAGES
!     **************
!
!     IORDNL TABLE OVERFLOW
!
         CALL xgpidg(14,nordn1,nordn2,and(oscar(ospnt+5),Nosgn))
         RETURN
      CASE (22)
!
!     RESTART - USE LAST XVPS ENTRY IN PTDIC FOR RESTART.
!     EXCLUDE FIRST NXVPS ENTRY
!
         Ptdtop = Ptdtop + 3
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
            IF ( and(ptdic(j+2),reuse)/=0 ) THEN
!
!     REUSE FLAG UP - ENTER FILE IN ICPDPL
!
               Icpbot = Icpbot + 3
               icpdpl(Icpbot) = ptdic(j)
               icpdpl(Icpbot+1) = ptdic(j+1)
               icpdpl(Icpbot+2) = ptdic(j+2)
            ENDIF
         ENDDO
!
!     ORDER FILES IN ICPDPL BY REEL/FILE NUMBER
!
         IF ( Icpbot<Icptop ) RETURN
!
!     DO NOT DISTURB EXISTING ORDER
!
         IF ( Icpbot==Icptop ) THEN
            spag_nextblock_1 = 24
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = Icptop
         l = k
         spag_nextblock_1 = 23
      CASE (23)
         SPAG_Loop_1_8: DO WHILE ( and(icpdpl(k+2),Noflgs)>and(icpdpl(k+5),Noflgs) )
!
!     SWITCH
!
            DO m = 1 , 3
               j = k + m + 2
               itmp(1) = icpdpl(j)
               icpdpl(j) = icpdpl(j-3)
               icpdpl(j-3) = itmp(1)
            ENDDO
            k = k - 3
            IF ( k<Icptop ) EXIT SPAG_Loop_1_8
         ENDDO SPAG_Loop_1_8
         k = l + 3
         IF ( k<Icpbot ) THEN
            l = k
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 24
      CASE (24)
!
!     ENTER PURGED FILE IN FIAT IF THERE IS NO POSSIBLE WAY TO GENERATE
!     FILE
!
         j1 = 2
         j1 = oscar(j1)
         j2 = oscar(osbot+1)
         SPAG_Loop_1_9: DO i = Icptop , Icpbot , 3
            IF ( and(icpdpl(i+2),Maskhi)/=0 ) EXIT SPAG_Loop_1_9
            ospnt = 1
            SPAG_Loop_2_10: DO j = j1 , j2
               spag_nextblock_5 = 1
               SPAG_DispatchLoop_5: DO
                  SELECT CASE (spag_nextblock_5)
                  CASE (1)
                     IF ( and(Maskhi,oscar(ospnt+2))<=2 .AND. oscar(ospnt+5)<0 ) THEN
!
!     SEE IF PURGED FILE IS IN I/P SECTION
!
                        k1 = ospnt + 7
                        k2 = oscar(k1-1)*3 - 3 + k1
                        DO k = k1 , k2 , 3
                           IF ( oscar(k)==icpdpl(i) .AND. oscar(k+1)==icpdpl(i+1) ) GOTO 42
                        ENDDO
!
!     PURGED FILE IS NOT IN I/P SECTION - SEARCH O/P SECTION FOR IT.
!
                        IF ( and(Maskhi,oscar(ospnt+2))==1 ) THEN
                           k1 = oscar(ospnt+6)*3 + ospnt + 8
                           k2 = oscar(k1-1)*3 - 3 + k1
                           DO k = k1 , k2 , 3
                              IF ( oscar(k)==icpdpl(i) .AND. oscar(k+1)==icpdpl(i+1) ) EXIT SPAG_Loop_2_10
                           ENDDO
                        ENDIF
                        spag_nextblock_5 = 2
                        CYCLE SPAG_DispatchLoop_5
!
!     PURGED FILE FIRST USED AS INPUT - THEREFORE IT CANNOT BE GENERATED
!     ENTER PURGED FILE IN FIAT
!
 42                     l = Ifiat(3)*Icfiat + 4
                        Ifiat(3) = Ifiat(3) + 1
                        Ifiat(l) = or(Maskhi,oscar(k+2))
                        Ifiat(l+1) = oscar(k)
                        Ifiat(l+2) = oscar(k+1)
                        EXIT SPAG_Loop_2_10
                     ENDIF
                     spag_nextblock_5 = 2
                  CASE (2)
                     ospnt = oscar(ospnt) + ospnt
                     EXIT SPAG_DispatchLoop_5
                  END SELECT
               ENDDO SPAG_DispatchLoop_5
            ENDDO SPAG_Loop_2_10
         ENDDO SPAG_Loop_1_9
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xflord
