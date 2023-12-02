!*==ta1cps.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ta1cps
   USE c_blank
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_names
   USE c_system
   USE c_ta1com
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alfa1 , alfa12 , alfa2 , c , c2 , c4 , const , determ , detrmn , dit , eixx , eiyy , ex , exx , ey , eyy , gsube , rho , &
         & s , s2 , s4 , theta , thetar , ti , tlam , tref , zbarx , zbarxb , zbarxt , zbary , zbaryb , zbaryt , zg1 , zg2 , zg4 ,  &
         & zi , zk , zk1 , zoffs , zref , zx , zy
   INTEGER , SAVE :: blank , eoe , i1st , mem , mt2bit , pshbit , sym , symmem
   INTEGER :: buf0 , buf1 , buf2 , buf3 , buf4 , buf5 , elid , eoeloc , eof , eptwds , file , flag , ic , icore , icount , iept ,   &
            & ifinis , ii , iik , imat , ipc , ipc1 , ipc2 , ir , irec , ired , ising , istart , itype , jj , k , k1 , k2 , kk ,    &
            & kpc , kt721 , l40 , lamopt , len , ll , lpcomp , mat2pr , matwds , mid , mm , n , n1mat , n2mat , nlay , nn , npc ,   &
            & npc1 , npc2 , nwdpc , pcompr , pidloc , pos , pos1 , pshlpr , typc , typc1 , typc2
   REAL , DIMENSION(6) :: dum
   REAL , DIMENSION(3) :: dummy
   REAL , DIMENSION(2) :: e , ei , fi , fii , ri , zbar
   REAL , SAVE :: epsi
   REAL , DIMENSION(25) :: g , glay
   REAL , DIMENSION(3,3) :: g1 , g2 , g3bar , g4 , gbar , gd1 , gd2 , gd4 , gdbar
   REAL , DIMENSION(2,2) :: g3 , g3invd , gtrflx , trflx
   REAL , DIMENSION(9) :: g3br , g3i , g3iu , gbr , gd , gdbr , gdt , gt , t , u
   REAL , DIMENSION(17) :: gbendg , gmembd , gmembr , gtrshr , rpshel
   INTEGER , DIMENSION(17) :: ibendg , imembd , imembr , ipshel , itrshr
   INTEGER , DIMENSION(7) :: ieptx , imptx , ipcomp
   INTEGER , DIMENSION(6,3) :: index
   INTEGER , DIMENSION(3,3) :: indexx
   INTEGER , DIMENSION(3) , SAVE :: matnam , npcmp , npcmp1 , npcmp2 , pcbit , pshnam
   INTEGER , DIMENSION(2) , SAVE :: nam , nam2 , pcomp , pcomp1 , pcomp2
   INTEGER , DIMENSION(2) :: nam1
   LOGICAL , SAVE :: okuai
   REAL , DIMENSION(1) :: rz
   REAL , DIMENSION(6,6) :: stiff
   EXTERNAL andf , bckrec , close , filpos , fname , fwdrec , gmmats , invers , korsz , locate , lprops , mat , mesage , open ,     &
          & orf , page2 , preloc , premat , rdtrl , read , rewind , savpos , sswtch , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     G3 MATRIX CALCULATION WITH NEW FORMULATION
!
!     THIS ROUTINE IS CALLED IN TA1 IF PARAM COMPS IS SET TO -1
!     INDICATING PCOMP, PCOMP1 OR PCOMP2 BULK DATA ENTRIES ARE
!     PRESENT. IT'S PRIMARY FUNCTION IS TO -
!       1. CREATE FILE PCOMPS WHICH WILL CONTAIN THE ECHO OF THE
!          'PCOMPS' ENTRIES ALONG WITH INDIVIDUAL LAYER INTRINISIC
!          PROPERTY MATRICES.
!       2. CALCULATE OVERALL MATERIAL PROPERTIES IN THE FORM OF MAT2
!          ENTRIES AND WRITE TO FILE MPTX.
!       3. GENERATE EQUIVALENT PSHELL PROPERTY ENTRIES AND WRITE TO
!          FILE EPTX.
!
   !>>>>EQUIVALENCE (Z(1),Rz(1)) , (ipshel(1),rpshel(1)) , (imembr(1),gmembr(1)) , (ibendg(1),gbendg(1)) , (imembd(1),gmembd(1)) ,       &
!>>>>    & (itrshr(1),gtrshr(1))
!     DATA    MPT   / 107/
!     DATA    MPTX  / 206/
!     DATA    PCOMPS/ 207/
!     DATA    EPTX  / 208/
   DATA pcomp/5502 , 55/
   DATA pcomp1/5602 , 56/
   DATA pcomp2/5702 , 57/
   DATA npcmp/5502 , 55 , 280/
   DATA npcmp1/5602 , 56 , 281/
   DATA npcmp2/5702 , 57 , 282/
   DATA pshnam/5802 , 58 , 283/
   DATA matnam/203 , 2 , 78/
   DATA pcbit/55 , 56 , 57/
   DATA pshbit/58/
   DATA mt2bit/2/
   DATA i1st/1/
   DATA sym/1/
   DATA mem/2/
   DATA symmem/3/
   DATA eoe/ - 1/
   DATA nam/4HTA1C , 4HPS  /
   DATA nam2/4HPCOM , 4HPS  /
   DATA blank/4HBLNK/
   DATA okuai/.TRUE./
   DATA epsi/1.0E-15/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         buf0 = korsz(z) - sysbuf - 2
         buf1 = buf0 - sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         buf3 = buf2 - sysbuf - 2
         buf4 = buf3 - sysbuf - 2
         buf5 = buf4 - sysbuf - 2
!
!     PERFORM GENERAL INITILIZATION
!
         matwds = 0
         eof = 0
         elid = 0
         mat2pr = 0
         pshlpr = 0
         icount = 0
         rho = 0.0
!
!     OPEN EPTX AND WRITE HEADER RECORD
!
         file = eptx
         CALL open(*380,eptx,z(buf0),wrtrew)
         CALL fname(eptx,nam1)
         CALL write(eptx,nam1,2,1)
!
!     OPEN MPTX AND WRITE HEADER RECORD
!
         file = mptx
         CALL open(*380,mptx,z(buf1),wrtrew)
         CALL fname(mptx,nam1)
         CALL write(mptx,nam1,2,1)
!
!     OPEN MPT AND POSITION FILE
!
         file = mpt
         CALL open(*380,mpt,z(buf2),rdrew)
         CALL fwdrec(*380,mpt)
!
!     OPEN PCOMPS AND WRITE HEADER RECORD
!     WRITE TO IPCOMP(1), THE GINO FILE NAME OF PCOMPS
!
         file = pcomps
         CALL open(*380,pcomps,z(buf3),wrtrew)
         CALL write(pcomps,nam2,2,1)
!
         ipcomp(1) = pcomps
         DO ll = 2 , 7
            ipcomp(ll) = 0
         ENDDO
!
!     COPY ALL EPT ENTRIES UP TO PSHELL TYPE TO FILE EPTX
!     IF NONE FOUND, MUST CREATE ONE BEFORE THE LAST RECORD IN FILE
!
!     SET AVAILABLE CORE
!
         n = buf5 - 1
         iept = i1st
         file = ept
         CALL open(*380,ept,z(buf4),rdrew)
         CALL fwdrec(*380,ept)
         irec = 0
         DO
            CALL fwdrec(*20,ept)
            irec = irec + 1
         ENDDO
!
 20      CALL rewind(ept)
         CALL fwdrec(*380,ept)
         ired = 0
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*380,*40,ept,z(iept),n,1,eptwds)
         CALL mesage(-8,0,nam)
 40      IF ( z(iept)==4902 ) THEN
!
            pshlpr = 1
         ELSE
            ired = ired + 1
            IF ( ired/=irec ) THEN
               CALL write(eptx,z(iept),eptwds,1)
               eptwds = 0
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         CALL bckrec(ept)
         CALL savpos(ept,pos1)
         CALL close(ept,clsrew)
!
!     OPEN EPT
!
         file = ept
         CALL preloc(*380,z(buf4),ept)
!
!     COPY ALL MAT ENTRIES UP TO MAT2 TYPE TO FILE MPTX
!
!     SET AVAILABLE CORE
!
         n = buf5 - 1
         imat = i1st
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*80,*60,mpt,z(imat),n,1,matwds)
         CALL mesage(-8,0,nam)
 60      IF ( z(imat)>=203 ) THEN
            CALL bckrec(mpt)
            CALL savpos(mpt,pos)
            IF ( z(imat)==203 ) mat2pr = 1
            spag_nextblock_1 = 4
         ELSE
            CALL write(mptx,z(imat),matwds,1)
            matwds = 0
            spag_nextblock_1 = 3
         ENDIF
         CYCLE
!
!    SET END OF FILE FLAG
!
 80      eof = 1
         spag_nextblock_1 = 4
      CASE (4)
!
!     CLOSE MPT BEFORE CALLING PREMAT
!
         CALL close(mpt,1)
!
!     SET POINTERS AND PERFORM INITILIZATION
!
         ipc1 = 1
         npc = 0
         npc1 = 0
         npc2 = 0
         typc = 0
         typc1 = 0
         typc2 = 0
!
!     SET SIZE OF AVAILABLE CORE
!
         n = buf5 - 1
         ipc = 1
!
!     LOCATE PCOMP DATA AND READ INTO CORE
!
         CALL locate(*120,z(buf4),pcomp,flag)
!
         CALL read(*380,*100,ept,z(ipc),n,0,npc)
         CALL mesage(-8,0,nam)
 100     IF ( npc>0 ) typc = 1
         ipc1 = ipc + npc
         IF ( ipc1>=buf5 ) CALL mesage(-8,0,nam)
         n = n - npc
!
!     LOCATE PCOMP1 DATA AND READ INTO CORE
!
 120     CALL locate(*160,z(buf4),pcomp1,flag)
!
         ipc1 = ipc + npc
         CALL read(*200,*140,ept,z(ipc1),n,0,npc1)
         CALL mesage(-8,0,nam)
 140     IF ( npc1>0 ) typc1 = 1
         ipc2 = ipc1 + npc1
         IF ( ipc2>=buf5 ) CALL mesage(-8,0,nam)
         n = n - npc1
!
!     LOCATE PCOMP2 DATA AND READ INTO CORE
!
 160     CALL locate(*200,z(buf4),pcomp2,flag)
!
         ipc2 = ipc1 + npc1
         CALL read(*200,*180,ept,z(ipc2),n,0,npc2)
         CALL mesage(-8,0,nam)
 180     IF ( npc2>0 ) typc2 = 1
!
!     SET SIZE OF LPCOMP. NUMBER OF WORDS READ INTO CORE
!
 200     lpcomp = ipc + npc + npc1 + npc2
         IF ( lpcomp>=buf5 ) CALL mesage(-8,0,nam)
!
!     CLOSE EPT BEFORE PROCESSING PCOMPI
!
         CALL close(ept,1)
!
!     READ MATERIAL PROPERTY TABLE INTO CORE
!
         imat = lpcomp + 1
         n1mat = buf5 - imat
         CALL premat(z(imat),z(imat),z(buf5),n1mat,n2mat,mpt,dit)
         IF ( imat+n2mat>=buf5 ) CALL mesage(-8,0,nam)
         icore = imat + n2mat + 1
!
!     SET POINTERS
!
         itype = -1
         istart = 0
         ifinis = 0
!
!     PROCESS ALL 'PCOMP' ENTRY TYPES SEQUENTIALLY
!
!     PCOMP ENTRIES
!
         IF ( typc/=0 ) THEN
            itype = 0
            istart = ipc
            ifinis = ipc1 - 1
            nwdpc = 8
            kpc = 4
            pcompr = 1
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     PCOMP1 ENTRIES
!
         IF ( typc1/=0 ) THEN
            itype = 1
            istart = ipc1
            ifinis = ipc2 - 1
            nwdpc = 8
            kpc = 1
            pcompr = 1
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     PCOMP2 ENTRIES
!
         IF ( typc2/=0 ) THEN
            itype = 2
            istart = ipc2
            ifinis = lpcomp - 1
            nwdpc = 8
            kpc = 2
         ENDIF
!
!     CHECK IF NO PCOMP DATA HAS BEEN READ INTO CORE
!
         IF ( typc==0 .AND. typc1==0 .AND. typc2==0 ) THEN
            CALL page2(2)
            WRITE (nout,99001)
99001       FORMAT ('0*** SYSTEM FATAL ERROR.  PCOMP, PCOMP1 OR PCOMP2',' DATA NOT FOUND BY SUBROUTINE TA1CPS.')
            nogo = 1
            RETURN
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     SET INFLAG = 12, SO THAT FOR LAMINA REFERENCING MAT1 OR MAT2
!     PROPERTY ENTRY WILL BE RETURNED IN MAT2 FORMAT. EXECPT FOR
!     THOSE REFERENCING MAT8 PROPERTY, IN WHICH CASE THE ENTRY
!     IS MERELY ECHOED.
!
         inflag = 12
!
!     SET POINTERS
!
!     WRITE 3-WORD IDENTITY FOR PCOMP DATA
!
!     PCOMP TYPE
!
         IF ( itype==0 ) THEN
            CALL write(pcomps,npcmp,3,0)
!
!     PCOMP1 TYPE
!
         ELSEIF ( itype/=1 ) THEN
!
!     PCOMP2 TYPE
!
            CALL write(pcomps,npcmp2,3,0)
         ELSE
            CALL write(pcomps,npcmp1,3,0)
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     PROCESS ALL 'PCOMP' ENTRIES
!
         len = 0
         nlay = 0
         eoeloc = 0
         pidloc = 1
         tlam = 0.0
         rho = 0.0
         zk = 0.0
         zk1 = 0.0
         tref = 0.0
         gsube = 0.0
         alfa1 = 0.0
         alfa2 = 0.0
         alfa12 = 0.0
!
         SPAG_Loop_1_1: DO ii = istart , ifinis
            IF ( z(ii)==-1 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
         eoeloc = ii
         pidloc = istart
         len = eoeloc - pidloc
         nlay = (len-nwdpc)/kpc
         lamopt = z(pidloc+7)
!
!     DETERMINE LAMINATE THICKNESS
!
!     PCOMP DATA
!
         IF ( itype<=0 ) THEN
            DO k = 1 , nlay
               iik = (pidloc+5) + 4*k
               tlam = tlam + rz(iik)
            ENDDO
            IF ( lamopt==sym .OR. lamopt==symmem ) tlam = 2.0*tlam
!
!     PCOMP1 DATA
!
         ELSEIF ( itype>1 ) THEN
!
!     PCOMP2 DATA
!
            DO k = 1 , nlay
               iik = (pidloc+6) + 2*k
               tlam = tlam + rz(iik)
            ENDDO
            IF ( lamopt==sym .OR. lamopt==symmem ) tlam = 2.0*tlam
         ELSE
            iik = pidloc + 6
            tlam = rz(iik)*nlay
            IF ( lamopt==sym .OR. lamopt==symmem ) tlam = 2.0*tlam
         ENDIF
!
!     WRITE TO PCOMPS
!      1. PID
!      2. NLAY - NUMBER OF LAYERS
!      3. REMAINDER OF PCOMP ENTRY
!
         CALL write(pcomps,z(pidloc),1,0)
         CALL write(pcomps,nlay,1,0)
!
!     SET LEN TO THE NO. WORDS TO BE WRITTEN TO PCOMPS
!
         len = len - 1
         CALL write(pcomps,z(pidloc+1),len,0)
!
!     CALL MAT TO GET LAYER PROPERTIES AND WRITE TO PCOMPS
!     NOTE FOR PCOMP1 AND PCOMP2 ENTRIES THE PROPERTY MATRIX
!     IS ONLY WRITTEN TO PCOMPS ONCE. (ALL LAYER PER ENTRY HAVE
!     THE SAME MID.
!     SIMILARILY FOR PCOMP ENTRY, IF ALL LAYERS REFERENCE THE SAME
!     MID, THEN THE PROPERTY MATRIX IS ONLY WRITTEN ONCE TO PCOMPS.
!
!          ITYPE = 0 PCOMP  ENTRY
!          ITYPE = 1 PCOMP1 ENTRY
!          ITYPE = 2 PCOMP2 ENTRY
!
         mid = 0
!
!     INITIALIZE G1, G2, G3 AND G4 MATRICES
!
         DO ll = 1 , 3
            DO mm = 1 , 3
               g1(ll,mm) = 0.0
               gd1(ll,mm) = 0.0
               g2(ll,mm) = 0.0
               gd2(ll,mm) = 0.0
               g4(ll,mm) = 0.0
               gd4(ll,mm) = 0.0
            ENDDO
         ENDDO
!
         DO ll = 1 , 2
            fii(ll) = 0.0
            fi(ll) = 0.0
            ri(ll) = 0.0
            zbar(ll) = 0.0
            DO mm = 1 , 2
               g3(ll,mm) = 0.0
               gtrflx(ll,mm) = 0.0
               trflx(ll,mm) = 0.0
               g3invd(ll,mm) = 0.0
            ENDDO
         ENDDO
!
!     INTILIZISE ZBAR
!
         zbarx = 0.0
         zbary = 0.0
         zbarxt = 0.0
         zbarxb = 0.0
         zbaryt = 0.0
         zbaryb = 0.0
         zx = 0.0
         zy = 0.0
!
         eixx = 0.0
         eiyy = 0.0
!
!     LOOP OVER LAYERS
!
         DO k = 1 , nlay
            IF ( itype==0 ) matid = z(pidloc+4+4*k)
            IF ( itype==1 .OR. itype==2 ) matid = z(pidloc+5)
            IF ( k>=2 .AND. (itype==0 .AND. mid==matid) ) THEN
!
!     WRITE THE LAYER PROPERTY MATRIX G TO FILE PCOMPS
!
               CALL write(pcomps,glay(1),25,0)
            ELSEIF ( .NOT.(k>=2 .AND. (itype==1 .OR. itype==2)) ) THEN
!
               mid = matid
               CALL mat(elid)
!
!     CALL LPROPS TO GET LAYER PROPERTY MATRICES
!
               CALL lprops(g)
!
!     COPY G(25) TO GLAY(25), FOR WRITING TO PCOMPS
!
               DO kk = 1 , 25
                  glay(kk) = g(kk)
               ENDDO
!
!     NEX 20 LINES ARE NEW FROM 2/90 UAI CODE
!     COPY ALFA1, ALFA2 AND ALFA12 FROM GLAY(14 THRU 16)
!
               IF ( okuai ) THEN
                  alfa1 = glay(14)
                  alfa2 = glay(15)
                  alfa1 = glay(16)
!
!     IF PCOMP, COPY TREF AND GE FROM THE MAIN CARD TO MATERIAL
!     PROPERTY DATA. THIS IS DONE FOR THE FIRST LAYER
!
                  IF ( k/=1 ) THEN
                     IF ( itype>=1 ) THEN
                        tref = glay(24)
                        gsube = glay(25)
                     ELSE
                        tref = rz(pidloc+5)
                        gsube = rz(pidloc+6)
                        glay(24) = tref
                        glay(25) = gsube
                     ENDIF
                  ENDIF
               ENDIF
               CALL write(pcomps,glay(1),25,0)
            ENDIF
!
!     CALCULATE CONTRIBUTION OF EACH LAYER TO OVERALL PROPERTY
!     MATRICES G1, G2, G4
!
!     BUILD TRANSFORMATION MATRIX T
!
            IF ( itype==0 ) theta = rz(pidloc+6+4*k)
            IF ( itype==1 ) theta = rz(pidloc+7+k)
            IF ( itype==2 ) theta = rz(pidloc+7+2*k)
            c = abs(theta)
            IF ( c<0.000002 ) c = 0.0
            IF ( c>89.99998 .AND. c<90.00002 ) c = 90.0
            IF ( c>179.9998 .AND. c<180.0002 ) c = 180.0
            IF ( c>269.9998 .AND. c<270.0002 ) c = 270.0
            IF ( c>359.9998 .AND. c<360.0002 ) c = 360.0
            IF ( theta<0.0 ) c = -c
            thetar = c*degrad
!
            c = cos(thetar)
            IF ( abs(c)<epsi ) c = 0.0
            c2 = c*c
            c4 = c2*c2
            s = sin(thetar)
            IF ( abs(s)<epsi ) s = 0.0
            s2 = s*s
            s4 = s2*s2
!
            t(1) = c2
            t(2) = s2
            t(3) = c*s
            t(4) = s2
            t(5) = c2
            t(6) = -c*s
            t(7) = -2.0*c*s
            t(8) = 2.0*c*s
            t(9) = c2 - s2
!
!     CALCULATE GBAR = TT X G X T
!
!     MULTIPLY G X T AND WRITE TO GT
!
            CALL gmmats(g(1),3,3,0,t(1),3,3,0,gt(1))
!
!     MULTIPLY TT X GT AND WRITE TO GBR
!
            CALL gmmats(t(1),3,3,1,gt(1),3,3,0,gbr(1))
!
!     WRITE GBR IN TWO DIMENSIONED ARRAY GBAR
!
            DO ll = 1 , 3
               DO mm = 1 , 3
                  nn = mm + 3*(ll-1)
                  gbar(ll,mm) = gbr(nn)
               ENDDO
            ENDDO
!
!     PROCESSING FOR G3 MATRIX
!
!     CALCULATE GDBAR = TT X GD X T
!
!     DETERMINE GD MATRIX, WHICH IS EQUAL TO G MATRIX WITH POISSONS
!     RATIO=0.0
!        GD(1) ---- YOUNGS MODULUS IN X-DIRN
!        GD(5) ---- YOUNGS MODULUS IN Y-DIRN
!        GD(9) ---- INPLANE SHEAR MODULUS
!
            DO ll = 1 , 9
               gd(ll) = 0.0
            ENDDO
            const = 1.0 - (g(2)*g(4))/(g(5)*g(1))
            gd(1) = g(1)*const
            gd(5) = g(5)*const
            gd(9) = g(9)
!
!     MULTIPLY GD X T AND WRITE TO GDT
!
            CALL gmmats(gd(1),3,3,0,t(1),3,3,0,gdt(1))
!
!     MULTIPLY TT X GDT AND WRITE TO GDBR
            CALL gmmats(t(1),3,3,1,gdt(1),3,3,0,gdbr(1))
!
!     WRITE GDBR IN TWO DIMENSIONED ARRAY GDBAR
!
            DO ll = 1 , 3
               DO mm = 1 , 3
                  nn = mm + 3*(ll-1)
                  gdbar(ll,mm) = gdbr(nn)
               ENDDO
            ENDDO
!
!     *********************************************************
!     *   NOTE TO APPROXIMATE BEAM BEHAVIOUR THE CROSS AND    *
!     *   COUPLING TERMS IN THE GDBAR MATRIX NEED TO BE       *
!     *   DEGRADED I.E SET TO ZERO.                           *
!     *********************************************************
!
            gdbar(1,2) = 0.0
            gdbar(2,1) = 0.0
            gdbar(1,3) = 0.0
            gdbar(2,3) = 0.0
            gdbar(3,1) = 0.0
            gdbar(3,2) = 0.0
!
!     PERFORM INITIALIZATION
!
            zref = -tlam/2.0
            zk1 = zk
            IF ( k==1 ) zk1 = zref
            IF ( itype==0 ) zk = zk1 + rz(pidloc+5+4*k)
            IF ( itype==1 ) zk = zk1 + rz(pidloc+6)
            IF ( itype==2 ) zk = zk1 + rz(pidloc+6+2*k)
            zg1 = zk - zk1
            zg4 = -(zk**2-zk1**2)*0.5
            zg2 = (zk**3-zk1**3)*0.33333333
!
!     CALCULATE LAYER CONTRIBUTION TO G1, G2, DG2, AND G4 MATRICES
!
            DO ir = 1 , 3
               DO ic = 1 , 3
                  g1(ir,ic) = g1(ir,ic) + gbar(ir,ic)*zg1
                  gd1(ir,ic) = gd1(ir,ic) + gdbar(ir,ic)*zg1
                  IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
                     g2(ir,ic) = g2(ir,ic) + gbar(ir,ic)*zg2
                     gd2(ir,ic) = gd2(ir,ic) + gdbar(ir,ic)*zg2
                     IF ( lamopt/=sym ) THEN
                        g4(ir,ic) = g4(ir,ic) + gbar(ir,ic)*zg4
                        gd4(ir,ic) = gd4(ir,ic) + gdbar(ir,ic)*zg4
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
!
!     CHECK LAMINATION OPTION AND IF SYMM OR SYMM.MEMB CALCULATE
!     LAYER CONTRIBUTION TO THE MEMBRANE, BENDING AND THE
!     MEMEBRANE-BENDING MATRICES
!
            IF ( lamopt==sym .OR. lamopt==symmem ) THEN
!
               DO ir = 1 , 3
                  DO ic = 1 , 3
                     g1(ir,ic) = g1(ir,ic) + gbar(ir,ic)*zg1
                     gd1(ir,ic) = gd1(ir,ic) + gdbar(ir,ic)*zg1
                     IF ( lamopt/=symmem ) THEN
                        g2(ir,ic) = g2(ir,ic) + gbar(ir,ic)*zg2
                        gd2(ir,ic) = gd2(ir,ic) + gdbar(ir,ic)*zg2
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
!
!
!     **************************************************************
!     CALCULATION OF ZBARX AND ZBARY
!            NEUTRAL SURFACE LOCATION IN X- AND Y- DIRECTION
!
!           TI  -  THICKNESS OF LAYER K
!           ZI  -  DISTANCE FROM REFERENCE SURFACE TO MID OF LAMINA K
!        EX,EY  -  APPARENT ENGINEERING PROPERTY. I.E YOUNGS MODULUS
!                  IN THE LONGITUDINAL AND TRANSVERSE DIRECTIONS IN
!                  THE MATERIAL COORDINATE SYSTEM.
!     **************************************************************
!
!     INVERT GDBAR TO DETERMINE EX AND EY
!
            ising = -1
            CALL invers(3,gdbar,3,dummy,0,determ,ising,indexx)
!
!     THE YOUNGS MODULI EX AND EY IN THE MATERIAL COORD SYSTEM
!
            ex = 1.0/gdbar(1,1)
            ey = 1.0/gdbar(2,2)
!
            exx = ex
            eyy = ey
!
!     WRITE EXX AND EYY TO PCOMPS
!
            CALL write(pcomps,exx,1,0)
            CALL write(pcomps,eyy,1,0)
!
            IF ( lamopt/=sym ) THEN
!
               ti = zk - zk1
               zi = (zk+zk1)/2.0
!
               zbarxt = zbarxt + ex*ti*zi
               zbarxb = zbarxb + ex*ti
               zbaryt = zbaryt + ey*ti*zi
               zbaryb = zbaryb + ey*ti
            ENDIF
!
!     CALCULATE CONTRIBUTION TO OVERALL DENSITY RHO
!
            IF ( g(23)/=0. ) rho = rho + g(23)*zg1
!
!     PROCESS NEXT LAYER
!
         ENDDO
!
!     JUMP IF LAMOPT IS MEMBRANE OR SYMM.MEMBRANE
!
         IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
!
!     WRITE GD1, GD2 AND GD4 MATRICES TO STIFF MATRIX AND INVERT
!     TO DETERMINE THE OVERALL BENDING PROPERTY FOR THE LAMINATE.
!
            DO ll = 1 , 3
               DO mm = 1 , 3
                  stiff(ll,mm) = gd1(ll,mm)
                  stiff(ll,mm+3) = gd4(ll,mm)
                  stiff(ll+3,mm) = gd4(ll,mm)
                  stiff(ll+3,mm+3) = gd2(ll,mm)
               ENDDO
            ENDDO
!
!     INVERT STIFF
!
            ising = -1
            CALL invers(6,stiff,6,dum,0,determ,ising,index)
!
            ei(1) = 1.0/stiff(4,4)
            ei(2) = 1.0/stiff(5,5)
!
            eixx = ei(1)
            eiyy = ei(2)
         ENDIF
!
!     WRITE EIXX AND EIYY TO PCOMPS
!
         CALL write(pcomps,eixx,1,0)
         CALL write(pcomps,eiyy,1,0)
!
!     ***************************************************************
!     *   THE MEMBRANE, BENDING, AND MEMEBRANE-BENDING MATRICES     *
!     *   G1, G2, G3, AND G4  ARE GIVEN BY THE FOLLOWING            *
!     ***************************************************************
!
         DO ir = 1 , 3
            DO ic = 1 , 3
               g1(ir,ic) = (1.0/tlam)*g1(ir,ic)
               IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
                  g2(ir,ic) = (12.0/tlam**3)*g2(ir,ic)
                  IF ( lamopt/=sym ) g4(ir,ic) = (1.0/tlam**2)*g4(ir,ic)
               ENDIF
            ENDDO
         ENDDO
!
!     CALCULATE LOCATION OF NEUTRAL SURFACE ZBARX AND ZBARY
!     FOR LAMINATE
!
         IF ( lamopt/=sym .AND. lamopt/=mem .AND. lamopt/=symmem ) THEN
            zbarx = zbarxt/zbarxb
            zbary = zbaryt/zbaryb
            zbar(1) = zbarx
            zbar(2) = zbary
!
            zx = zbarx
            zy = zbary
         ENDIF
!
!     WRITE ZX AND ZY TO PCOMPS
!
         CALL write(pcomps,zx,1,0)
         CALL write(pcomps,zy,1,0)
!
!     CALCULATE OVERALL DENSITY RHO
!
         IF ( rho/=0. ) THEN
            IF ( lamopt==sym .OR. lamopt==symmem ) rho = 2.0*rho
            rho = rho/tlam
         ENDIF
!
!     ****************************************************************
!     *   CHECK IF TRANSVERSE FLEXIBILITY MATRIX NEEDS TO CALCULATED *
!     *   OTHERWISE JUMP TO PROCEED AS PER NORMAL.                   *
!     ****************************************************************
!
         IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
            IF ( g(10)/=0.0 ) THEN
!
!     LOOP OVER ALL THE LAYERS
!
               DO k = 1 , nlay
                  IF ( itype==0 ) matid = z(pidloc+4+4*k)
                  IF ( itype==1 .OR. itype==2 ) matid = z(pidloc+5)
                  IF ( k<2 .OR. (itype/=0 .OR. mid/=matid) ) THEN
                     IF ( .NOT.(k>=2 .AND. (itype==1 .OR. itype==2)) ) THEN
!
                        mid = matid
                        CALL mat(elid)
!
!     CALL LPROPS TO GET LAYER PROPERTY MATRICES
!
                        CALL lprops(g)
                     ENDIF
                  ENDIF
!
!     BUILD TRANSFORMATION MATRIX T
!
                  IF ( itype==0 ) theta = rz(pidloc+6+4*k)
                  IF ( itype==1 ) theta = rz(pidloc+7+k)
                  IF ( itype==2 ) theta = rz(pidloc+7+2*k)
                  c = abs(theta)
                  IF ( c<0.000002 ) c = 0.0
                  IF ( c>89.99998 .AND. c<90.00002 ) c = 90.0
                  IF ( c>179.9998 .AND. c<180.0002 ) c = 180.0
                  IF ( c>269.9998 .AND. c<270.0002 ) c = 270.0
                  IF ( c>359.9998 .AND. c<360.0002 ) c = 360.0
                  IF ( theta<0.0 ) c = -c
                  thetar = c*degrad
!
                  c = cos(thetar)
                  IF ( abs(c)<epsi ) c = 0.0
                  c2 = c*c
                  c4 = c2*c2
                  s = sin(thetar)
                  IF ( abs(s)<epsi ) s = 0.0
                  s2 = s*s
                  s4 = s2*s2
!
                  t(1) = c2
                  t(2) = s2
                  t(3) = c*s
                  t(4) = s2
                  t(5) = c2
                  t(6) = -c*s
                  t(7) = -2.0*c*s
                  t(8) = 2.0*c*s
                  t(9) = c2 - s2
!
!     PROCESSING FOR G3 MATRIX
!
!     CALCULATE GDBR = TT X GD X T
!
!     DETERMINE GD MATRIX, WHICH IS EQUAL TO G MATRIX WITH POISSONS
!     RATIO=0.0
!        GD(1) ---- YOUNGS MODULUS IN X-DIRN
!        GD(5) ---- YOUNGS MODULUS IN Y-DIRN
!        GD(9) ---- INPLANE SHEAR MODULUS
!
                  DO ll = 1 , 9
                     gd(ll) = 0.0
                  ENDDO
                  const = 1.0 - (g(2)*g(4))/(g(5)*g(1))
                  gd(1) = g(1)*const
                  gd(5) = g(5)*const
                  gd(9) = g(9)
!
!     MULTIPLY GD X T AND WRITE TO GDT
!
                  CALL gmmats(gd(1),3,3,0,t(1),3,3,0,gdt(1))
!
!     MULTIPLY TT X GDT AND WRITE TO GDBR
!
                  CALL gmmats(t(1),3,3,1,gdt(1),3,3,0,gdbr(1))
!
!     WRITE GBR TO GDBAR
!
                  DO ll = 1 , 3
                     DO mm = 1 , 3
                        nn = mm + 3*(ll-1)
                        gdbar(ll,mm) = gdbr(nn)
                     ENDDO
                  ENDDO
!
!     *************************************************************
!     *       NOTE TO APPROXIMATE BEAM BEHAVIOUR THE CROSS AND    *
!     *       COUPLING TERMS IN THE GDBAR MATRIX NEED TO BE       *
!     *       DEGRADED I.E SET TO ZERO.                           *
!     *************************************************************
!
                  gdbar(1,2) = 0.0
                  gdbar(2,1) = 0.0
                  gdbar(1,3) = 0.0
                  gdbar(2,3) = 0.0
                  gdbar(3,1) = 0.0
                  gdbar(3,2) = 0.0
!
!     INVERT GDBAR TO DETERMINE EX AND EY
!
                  ising = -1
                  CALL invers(3,gdbar,3,dummy,0,determ,ising,indexx)
!
!     THE YOUNGS MODULI EX AND EY IN THE MATERIAL COORD SYSTEM ARE
!
                  e(1) = 1.0/gdbar(1,1)
                  e(2) = 1.0/gdbar(2,2)
!
!     PERFORM INTILIZATION
!
                  zref = -tlam/2.0
                  zk1 = zk
                  IF ( k==1 ) zk1 = zref
                  IF ( itype==0 ) zk = zk1 + rz(pidloc+5+4*k)
                  IF ( itype==1 ) zk = zk1 + rz(pidloc+6)
                  IF ( itype==2 ) zk = zk1 + rz(pidloc+6+2*k)
!
!     BUILD TRANSFORMATION MATRIX U
!
                  u(1) = c
                  u(2) = s
                  u(3) = -s
                  u(4) = c
!
!     CALCULATE G3BAR = UT X G3I X U
!     G3I MATRIX  -  LAYER K TRANSFORMED G3, IN MATERIAL COORD-SYS
!
                  DO ll = 1 , 4
                     mm = ll + 9
                     g3i(ll) = g(mm)
                  ENDDO
!
!     MULTIPLY G3I X U AND WRITE TO G3IU
!
                  CALL gmmats(g3i(1),2,2,0,u(1),2,2,0,g3iu(1))
!
!     MULTIPLY UT X G3IU AND WRITE TO G3BR
!
                  CALL gmmats(u(1),2,2,1,g3iu(1),2,2,0,g3br(1))
!
!     WRITE G3BR IN TWO DIMENSIONED ARRAY G3BAR
!
                  DO ll = 1 , 2
                     DO mm = 1 , 2
                        nn = mm + 2*(ll-1)
                        g3bar(ll,mm) = g3br(nn)
                     ENDDO
                  ENDDO
!
!     INVERT G3BAR
!
                  detrmn = g3bar(1,1)*g3bar(2,2) - g3bar(1,2)*g3bar(2,1)
                  IF ( detrmn==0.0 ) THEN
                     spag_nextblock_1 = 11
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
                  g3invd(1,1) = g3bar(2,2)/detrmn
                  g3invd(1,2) = -g3bar(1,2)/detrmn
                  g3invd(2,1) = -g3bar(2,1)/detrmn
                  g3invd(2,2) = g3bar(1,1)/detrmn
!
!     G3 MATRIX CALC
!
                  zi = (zk+zk1)/2.0
                  ti = zk - zk1
!
                  DO ir = 1 , 2
                     ri(ir) = ((fi(ir)/e(ir))+(zbar(ir)-zk1)*ti-(ti*ti/3.0))*(fi(ir)/e(ir))
                     ri(ir) = ri(ir) + zbar(ir)*ti*ti*((zbar(ir)-2.0*zk1)/3.0-(ti/4.0))
                     ri(ir) = ri(ir) + ti*ti*((zk1*zk1)/3.0+(zk1*ti)/4.0+(ti*ti)/20.0)
                     ri(ir) = ri(ir)*e(ir)*e(ir)*ti
                  ENDDO
!
                  DO ir = 1 , 2
                     DO ic = 1 , 2
                        gtrflx(ir,ic) = gtrflx(ir,ic) + ri(ir)*g3invd(ir,ic)
                     ENDDO
                  ENDDO
!
                  DO ir = 1 , 2
                     fii(ir) = e(ir)*ti*(zbar(ir)-zi)
                     fi(ir) = fi(ir) + fii(ir)
                  ENDDO
!
!     PROCESS NEXT LAYER
!
               ENDDO
!
!
!    FALL HERE IF LAMOPT IS SYMM AND G3 CALCULATION IS REQUIRED
!
!
               IF ( lamopt==sym ) THEN
                  DO kk = 1 , nlay
                     k = nlay + 1 - kk
!
                     IF ( itype==0 ) matid = z(pidloc+4+4*k)
                     IF ( itype==1 .OR. itype==2 ) matid = z(pidloc+5)
                     IF ( k<2 .OR. (itype/=0 .OR. mid/=matid) ) THEN
                        IF ( .NOT.(k>=2 .AND. (itype==1 .OR. itype==2)) ) THEN
!
                           mid = matid
                           CALL mat(elid)
!
!     CALL LPROPS TO GET LAYER PROPERTY MATRICES
!
                           CALL lprops(g)
                        ENDIF
                     ENDIF
!
!     BUILD TRANSFORMATION MATRIX T
!
                     IF ( itype==0 ) theta = rz(pidloc+6+4*k)
                     IF ( itype==1 ) theta = rz(pidloc+7+k)
                     IF ( itype==2 ) theta = rz(pidloc+7+2*k)
                     c = abs(theta)
                     IF ( c<0.000002 ) c = 0.0
                     IF ( c>89.99998 .AND. c<90.00002 ) c = 90.0
                     IF ( c>179.9998 .AND. c<180.0002 ) c = 180.0
                     IF ( c>269.9998 .AND. c<270.0002 ) c = 270.0
                     IF ( c>359.9998 .AND. c<360.0002 ) c = 360.0
                     IF ( theta<0.0 ) c = -c
                     thetar = c*degrad
!
                     c = cos(thetar)
                     IF ( abs(c)<epsi ) c = 0.0
                     c2 = c*c
                     c4 = c2*c2
                     s = sin(thetar)
                     IF ( abs(s)<epsi ) s = 0.0
                     s2 = s*s
                     s4 = s2*s2
!
                     t(1) = c2
                     t(2) = s2
                     t(3) = c*s
                     t(4) = s2
                     t(5) = c2
                     t(6) = -c*s
                     t(7) = -2.0*c*s
                     t(8) = 2.0*c*s
                     t(9) = c2 - s2
!
!     PROCESSING FOR G3 MATRIX
!
!     CALCULATE GDBR = TT X GD X T
!
!     DETERMINE GD MATRIX, WHICH IS EQUAL TO G MATRIX WITH POISSONS
!     RATIO=0.0
!        GD(1) ---- YOUNGS MODULUS IN X-DIRN
!        GD(5) ---- YOUNGS MODULUS IN Y-DIRN
!        GD(9) ---- INPLANE SHEAR MODULUS
!
                     DO ll = 1 , 9
                        gd(ll) = 0.0
                     ENDDO
                     const = 1.0 - (g(2)*g(4))/(g(5)*g(1))
                     gd(1) = g(1)*const
                     gd(5) = g(5)*const
                     gd(9) = g(9)
!
!     MULTIPLY GD X T AND WRITE TO GDT
!
                     CALL gmmats(gd(1),3,3,0,t(1),3,3,0,gdt(1))
!
!     MULTIPLY TT X GDT AND WRITE TO GDBR
!
                     CALL gmmats(t(1),3,3,1,gdt(1),3,3,0,gdbr(1))
!
!     WRITE GBR TO GDBAR
!
                     DO ll = 1 , 3
                        DO mm = 1 , 3
                           nn = mm + 3*(ll-1)
                           gdbar(ll,mm) = gdbr(nn)
                        ENDDO
                     ENDDO
!
!     *************************************************************
!     *       NOTE TO APPROXIMATE BEAM BEHAVIOUR THE CROSS AND    *
!     *       COUPLING TERMS IN THE GDBAR MATRIX NEED TO BE       *
!     *       DEGRADED I.E SET TO ZERO.                           *
!     *************************************************************
!
                     gdbar(1,2) = 0.0
                     gdbar(2,1) = 0.0
                     gdbar(1,3) = 0.0
                     gdbar(2,3) = 0.0
                     gdbar(3,1) = 0.0
                     gdbar(3,2) = 0.0
!
!     INVERT GDBAR TO DETERMINE EX AND EY
!
                     ising = -1
                     CALL invers(3,gdbar,3,dummy,0,determ,ising,indexx)
!
!     THE YOUNGS MODULI EX AND EY IN THE MATERIAL COORD SYSTEM ARE
!
                     e(1) = 1.0/gdbar(1,1)
                     e(2) = 1.0/gdbar(2,2)
!
!     PERFORM INTILIZATION
!
                     zref = -tlam/2.0
                     zk1 = zk
                     IF ( itype==0 ) zk = zk1 + rz(pidloc+5+4*k)
                     IF ( itype==1 ) zk = zk1 + rz(pidloc+6)
                     IF ( itype==2 ) zk = zk1 + rz(pidloc+6+2*k)
!
!     BUILD TRANSFORMATION MATRIX U
!
                     u(1) = c
                     u(2) = s
                     u(3) = -s
                     u(4) = c
!
!     CALCULATE G3BAR = UT X G3I X U
!     G3I MATRIX  -  LAYER K TRANSFORMED G3, IN MATERIAL COORD-SYS
!
                     DO ll = 1 , 4
                        mm = ll + 9
                        g3i(ll) = g(mm)
                     ENDDO
!
!     MULTIPLY G3I X U AND WRITE TO G3IU
!
                     CALL gmmats(g3i(1),2,2,0,u(1),2,2,0,g3iu(1))
!
!     MULTIPLY UT X G3IU AND WRITE TO G3BR
!
                     CALL gmmats(u(1),2,2,1,g3iu(1),2,2,0,g3br(1))
!
!     WRITE G3BR IN TWO DIMENSIONED ARRAY G3BAR
!
                     DO ll = 1 , 2
                        DO mm = 1 , 2
                           nn = mm + 2*(ll-1)
                           g3bar(ll,mm) = g3br(nn)
                        ENDDO
                     ENDDO
!
!     INVERT G3BAR
!
                     detrmn = g3bar(1,1)*g3bar(2,2) - g3bar(1,2)*g3bar(2,1)
                     IF ( detrmn==0.0 ) THEN
                        spag_nextblock_1 = 11
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
                     g3invd(1,1) = g3bar(2,2)/detrmn
                     g3invd(1,2) = -g3bar(1,2)/detrmn
                     g3invd(2,1) = -g3bar(2,1)/detrmn
                     g3invd(2,2) = g3bar(1,1)/detrmn
!
!     THE CORRESSPONDING LAYER ON THE OTHER SIDE OF SYMMETRY
!
                     zi = (zk+zk1)/2.0
                     ti = zk - zk1
!
                     DO ir = 1 , 2
                        ri(ir) = (fi(ir)/e(ir)+(-zk1)*ti-ti*ti/3.0)*fi(ir)/e(ir) + (zk1*zk1/3.0+zk1*ti/4.0+ti*ti/20.0)*ti*ti
                        ri(ir) = ri(ir)*e(ir)*e(ir)*ti
                     ENDDO
!
                     DO ir = 1 , 2
                        DO ic = 1 , 2
                           gtrflx(ir,ic) = gtrflx(ir,ic) + ri(ir)*g3invd(ir,ic)
                        ENDDO
                     ENDDO
!
                     DO ir = 1 , 2
                        fii(ir) = e(ir)*ti*(zbar(ir)-zi)
                        fi(ir) = fi(ir) + fii(ir)
                     ENDDO
!
!     PROCESS NEXT LAYER
!
                  ENDDO
               ENDIF
!
               DO ir = 1 , 2
                  DO ic = 1 , 2
                     gtrflx(ir,ic) = gtrflx(ir,ic)*tlam/(ei(ir)**2)
                  ENDDO
               ENDDO
!
!     INVERT GTRFLX
!
               detrmn = gtrflx(1,1)*gtrflx(2,2) - gtrflx(1,2)*gtrflx(2,1)
               IF ( detrmn==0.0 ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
               g3(1,1) = gtrflx(2,2)/detrmn
               g3(1,2) = -gtrflx(1,2)/detrmn
               g3(2,1) = -gtrflx(2,1)/detrmn
               g3(2,2) = gtrflx(1,1)/detrmn
!
!     BECAUSE G3(1,2) IS NOT EQUAL TO G3(2,1) IN GENERAL
!     AN AVERAGE VALUE WILL BE USED FOR THE COUPLING TERMS
!
               g3(1,2) = (g3(1,2)+g3(2,1))/2.0
               g3(2,1) = g3(1,2)
            ENDIF
         ENDIF
!
!     *****************************************************
!     WRITE THE NEWLY GENERATED G1, G2, G3, AND G4 MATRICES
!     TO MPTX IN THE FORM OF MAT2 DATA ENTRIES
!     *****************************************************
!
!      NOTE - THE MID FOR THESE MATRICES ARE AS FOLLOWS-
!         1. MID1  -- PID + 100000000
!         2. MID2  -- PID + 200000000
!         3. MID3  -- PID + 300000000
!         4. MID4  -- PID + 400000000
!
!    INITIALIZE G1, G2, G3, AND G4 MATRICES
!
         DO jj = 1 , 17
            gmembr(jj) = 0.0
            gbendg(jj) = 0.0
            gtrshr(jj) = 0.0
            gmembd(jj) = 0.0
         ENDDO
!
         imembr(1) = 0
         ibendg(1) = 0
         itrshr(1) = 0
         imembd(1) = 0
!
!     START GENERATING G1 MEMBRANE MATRIX
!
         imembr(1) = z(pidloc) + 100000000
         gmembr(2) = g1(1,1)
         gmembr(3) = g1(1,2)
         gmembr(4) = g1(1,3)
         gmembr(5) = g1(2,2)
         gmembr(6) = g1(2,3)
         gmembr(7) = g1(3,3)
         gmembr(8) = rho
!
!     NEXT 5 LINES ARE NEW FROM 2/90 UAI CODE
!
         IF ( okuai ) THEN
            gmembr(9) = alfa1
            gmembr(10) = alfa2
            gmembr(11) = alfa12
            gmembr(12) = tref
            gmembr(13) = gsube
         ENDIF
!
         IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
!
!     START GENERATING G2 BENDING MATRIX
!
            ibendg(1) = z(pidloc) + 200000000
            gbendg(2) = g2(1,1)
            gbendg(3) = g2(1,2)
            gbendg(4) = g2(1,3)
            gbendg(5) = g2(2,2)
            gbendg(6) = g2(2,3)
            gbendg(7) = g2(3,3)
!
!     NEXT 3 LINES ARE NEW FROM 2/90 UAI CODE
!
            IF ( okuai ) THEN
!     GBENDG( 8) = ??
               gbendg(9) = alfa1
               gbendg(10) = alfa2
               gbendg(11) = alfa12
            ENDIF
!
!     START GENERATING G3 TRANSVERSE SHEAR FLEXIBILITY MATRIX
!
            itrshr(1) = z(pidloc) + 300000000
            gtrshr(2) = g3(1,1)
            gtrshr(3) = g3(1,2)
            gtrshr(4) = g3(2,1)
            gtrshr(5) = g3(2,2)
!
            IF ( lamopt/=sym ) THEN
!
!     START GENERATING G4 MEMBRANE-BENDING COUPLING MATRIX
!
               imembd(1) = z(pidloc) + 400000000
               gmembd(2) = g4(1,1)
               gmembd(3) = g4(1,2)
               gmembd(4) = g4(1,3)
               gmembd(5) = g4(2,2)
               gmembd(6) = g4(2,3)
               gmembd(7) = g4(3,3)
            ENDIF
         ENDIF
!
!
!     *******************************************************
!     GENERATE EQUIVALENT PSHELL BULK DATA ENTIES FOR EVERY
!     PCOMPI BULK DATA ENTRY. THIS IS NECESSARY FOR DEMG TO
!     FUNCTION CORRECTLY WHEN LAMINATED COMPOSITE ELEMENTS
!     ARE PRESENT.
!     *******************************************************
!
         ipshel(1) = z(pidloc)
         ipshel(2) = z(pidloc) + 100000000
         rpshel(3) = tlam
         ipshel(4) = z(pidloc) + 200000000
         rpshel(5) = 1.0
         ipshel(6) = z(pidloc) + 300000000
         rpshel(7) = 1.0
         rpshel(8) = rz(pidloc+2)
         rpshel(9) = -tlam/2.0
         rpshel(10) = tlam/2.0
         ipshel(11) = z(pidloc) + 400000000
         rpshel(12) = 0.0
         ipshel(13) = 0
         ipshel(14) = 0
         rpshel(15) = 0.0
         ipshel(16) = 0
         rpshel(17) = 0.0
!
         zoffs = rz(pidloc+1) + tlam/2.0
         IF ( z(pidloc)==blank ) zoffs = 0.0
         IF ( lamopt==mem .OR. lamopt==symmem ) zoffs = 0
         IF ( abs(zoffs)<=1.0E-3 ) zoffs = 0.0
         rpshel(14) = zoffs
!
         IF ( lamopt==mem .OR. lamopt==symmem ) THEN
            ipshel(4) = 0
            ipshel(6) = 0
            ipshel(11) = 0
            rpshel(14) = 0.0
         ENDIF
         IF ( lamopt==sym ) ipshel(11) = 0
!
!     UPDATE COUNTER ICOUNT TO INDICATE MAT2 AND PSHELL DATA IS BEING
!     WRITTEN SECOND TIME
!
         icount = icount + 1
!
         IF ( icount>1 ) GOTO 240
!
         IF ( pshlpr/=1 ) THEN
            CALL write(eptx,pshnam,3,0)
            GOTO 240
         ELSE
            icore = lpcomp + 1 + n2mat
            n = buf5 - icore
            CALL open(*380,ept,z(buf4),rdrew)
            CALL filpos(ept,pos1)
            CALL read(*240,*220,ept,z(icore),n,0,eptwds)
            CALL mesage(-8,0,nam)
         ENDIF
 220     CALL write(eptx,z(icore),eptwds,0)
 240     CALL write(eptx,ipshel(1),17,0)
!
         IF ( icount>1 ) GOTO 280
!
         IF ( mat2pr/=1 ) THEN
            CALL write(mptx,matnam,3,0)
            GOTO 280
         ELSE
            icore = lpcomp + 1 + n2mat
            n = buf5 - icore
            CALL open(*380,mpt,z(buf2),rdrew)
            CALL filpos(mpt,pos)
            CALL read(*280,*260,mpt,z(icore),n,0,matwds)
            CALL mesage(-8,0,nam)
         ENDIF
 260     CALL write(mptx,z(icore),matwds,0)
 280     CALL write(mptx,imembr(1),17,0)
         IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
            CALL write(mptx,ibendg(1),17,0)
            CALL write(mptx,itrshr(1),17,0)
            IF ( lamopt/=sym ) CALL write(mptx,imembd(1),17,0)
         ENDIF
         CALL sswtch(40,l40)
         IF ( l40/=0 ) THEN
!
!     WRITE THE NEWLY GENERATED PROPERTY MATRICES TO THE OUTPUT FILE
!
            CALL page2(2)
            WRITE (nout,99003) imembr(1) , (gmembr(ll),ll=2,16)
            IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
               CALL page2(2)
               WRITE (nout,99003) ibendg(1) , (gbendg(ll),ll=2,16)
               IF ( gtrshr(1)/=0.0 ) THEN
                  CALL page2(2)
                  WRITE (nout,99003) itrshr(1) , (gtrshr(ll),ll=2,16)
               ENDIF
               IF ( lamopt/=sym ) THEN
                  CALL page2(2)
                  WRITE (nout,99003) imembd(1) , (gmembd(ll),ll=2,16)
               ENDIF
            ENDIF
         ENDIF
!
!     UPDATE LOCATION OF NEXT PID
!
         pidloc = eoeloc + 1
         istart = pidloc
!
!     WRITE END OF ENTRY (EOE) TO PCOMPS BEFORE PROCESSING
!     NEXT PCOMP ENTRY
!
         CALL write(pcomps,eoe,1,0)
!
!     CHECK IF ALL 'PCOMP' TYPE ENTRIES HAVE BEEN PROCESSED
!
!
!     PROCESS NEXT 'PCOMP' ENTRY
!
         IF ( istart<ifinis ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype<1 ) THEN
!
            CALL write(pcomps,0,0,1)
            IF ( typc1>0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( typc2>0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( itype==1 ) THEN
!
            CALL write(pcomps,0,0,1)
            IF ( typc2>0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
!
            CALL write(pcomps,0,0,1)
         ENDIF
!
!     ALL 'PCOMP' TYPES PROCESSED
!     WRITE EOR ON MPTX AND EPTX
!
         CALL write(mptx,0,0,1)
         CALL write(eptx,0,0,1)
!
!     COPY REMAINDER OF EPT TO EPTX
!
         icore = 1
         n = buf5 - 1
         eptwds = 0
         IF ( pshlpr/=1 ) CALL open(*380,ept,z(buf4),rdrew)
         CALL filpos(ept,pos1)
         IF ( pshlpr==1 ) CALL fwdrec(*320,ept)
         spag_nextblock_1 = 9
      CASE (9)
         CALL read(*320,*300,ept,z(icore),n,1,eptwds)
         CALL mesage(-8,0,nam)
 300     CALL write(eptx,z(icore),eptwds,1)
         eptwds = 0
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     READ TRAILER FROM EPT AND WRITE TO EPTX
!
 320     DO kk = 1 , 7
            ieptx(kk) = 0
         ENDDO
         ieptx(1) = ept
!
         CALL rdtrl(ieptx)
         ieptx(1) = eptx
         kt721 = andf(pshbit,511)
         k1 = (kt721-1)/16 + 2
         k2 = kt721 - (k1-2)*16 + 16
         ieptx(k1) = orf(ieptx(k1),two(k2))
         CALL wrttrl(ieptx)
!
!     IF EOF ON MPT,THEN ALL MAT2 DATA COPIED TO MPTX
!
         IF ( eof==1 ) GOTO 360
!
!     OTHERWISE COPY REMAINDER OF MPT TO MPTX
!
         icore = 1
         n = buf5 - 1
         matwds = 0
         IF ( mat2pr/=1 ) CALL open(*380,mpt,z(buf2),rdrew)
         CALL filpos(mpt,pos)
         IF ( mat2pr==1 ) CALL fwdrec(*360,mpt)
         spag_nextblock_1 = 10
      CASE (10)
         CALL read(*360,*340,mpt,z(icore),n,1,matwds)
         CALL mesage(-8,0,nam)
 340     CALL write(mptx,z(icore),matwds,1)
         matwds = 0
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
!
!     READ TRAILER FROM MPT AND WRITE TO MPTX
!
 360     DO kk = 1 , 7
            imptx(kk) = 0
         ENDDO
         imptx(1) = mpt
!
         CALL rdtrl(imptx)
         imptx(1) = mptx
         kt721 = andf(mt2bit,511)
         k1 = (kt721-1)/16 + 2
         k2 = kt721 - (k1-2)*16 + 16
         imptx(k1) = orf(imptx(k1),two(k2))
         CALL wrttrl(imptx)
!
!     WRITE TO TRAILER OF PCOMPS
!
!     SET TRAILER BIT POSITION TO ZERO IF ENTRY TYPE DOES NOT EXIST
!
         IF ( typc==0 ) pcbit(1) = 0
         IF ( typc1==0 ) pcbit(2) = 0
         IF ( typc2==0 ) pcbit(3) = 0
!
         DO ll = 1 , 3
            kt721 = andf(pcbit(ll),511)
            k1 = (kt721-1)/16 + 2
            k2 = kt721 - (k1-2)*16 + 16
            ipcomp(k1) = orf(ipcomp(k1),two(k2))
         ENDDO
!
!     WHEN ICFIAT IS 11, A 65536 IS LEFT IN IPCOMP(2) ACCIDENTALLY
!     ZERO IT OUT
!
         IF ( icfiat==11 ) ipcomp(2) = 0
         CALL wrttrl(ipcomp)
!
!     CLOSE ALL FILES
!
         CALL close(pcomps,1)
         CALL close(eptx,1)
         CALL close(mptx,1)
         CALL close(mpt,1)
         CALL close(ept,1)
!
         RETURN
!
!     FATAL ERROR MESSAGES
!
 380     CALL mesage(-1,file,nam)
         RETURN
      CASE (11)
         CALL page2(4)
         WRITE (nout,99002) matid
99002    FORMAT ('0*** USER FATAL ERROR.  IMPROPER DATA PROVIDED FOR',' CALCULATION OF TRANSVERSE SHEAR FLEXIBILITY MATRIX',/,23X,  &
                &'FOR LAMINA REFERENCING MID ',I8,'.',/,23X,'CHECK DATA ON MAT BULK DATA ENTRY.')
         nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99003 FORMAT (/,' MAT2',7X,I9,7(1X,1P,E11.4),/,9X,8(1X,F11.1))
END SUBROUTINE ta1cps
