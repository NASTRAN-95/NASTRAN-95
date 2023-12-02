!*==ta1a.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ta1a
!
!     TA1A BUILDS THE ELEMENT SUMMARY TABLE (EST).
!     THE EST GROUPS ECT, EPT, BGPDT AND ELEMENT TEMP. DATA FOR EACH
!     SIMPLE ELEMENT OF THE STRUCTURE. THE EST CONTAINS ONE LOGICAL
!     RECORD PER SIMPLE ELEMENT TYPE.
!
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_system
   USE c_ta1acm
   USE c_ta1com
   USE c_ta1ett
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bar , hbdy , mem , qdmem2 , quad4 , sym , symmem , tria3
   INTEGER , DIMENSION(50) :: buf
   INTEGER :: buf1 , buf2 , buf3 , eoeloc , file , flag , i , id , iheat , ii , iip , iip1 , iip2 , ipc , ipc1 , ipc2 , iset ,      &
            & itabl , itmpid , itype , j , jscalr , jtemp , k , khi , kk , klo , kn , kpc , kpc2 , ksavew , kscalr , kx , l , l40 , &
            & lamopt , len , ll , locbgp , lpc , lpc1 , lpc2 , lpcomp , lstprp , lx , m , m1 , m8 , mm , mx , n , name , nbgp ,     &
            & nid , nlay , noept , nogo , nogox , noprop , nopshl , nout , npc , npc1 , npc2 , npcomp , npshel , ntemp , ntmpid ,   &
            & oldid , pidloc , sysbuf , tempid , typc , typc1 , typc2
   REAL , DIMENSION(50) :: bufr
   REAL :: deftmp , tlam , zoffs
   LOGICAL :: frstim , q4t3
   INTEGER , DIMENSION(34) :: gpsav
   INTEGER , DIMENSION(16) :: ipshel
   INTEGER , DIMENSION(2) , SAVE :: nam , pcomp , pcomp1 , pcomp2
   REAL , DIMENSION(16) :: rpshel
   REAL , DIMENSION(33) :: tgrid
   INTEGER , DIMENSION(4) , SAVE :: zeros
   REAL , DIMENSION(1) :: zz
   EXTERNAL close , ectloc , fname , fwdrec , korsz , locate , mesage , open , page2 , preloc , rdtrl , read , skprec , sort ,      &
          & sswtch , ta1etd , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(10),Tempid) , (Ksystm(56),Iheat) , (Idftmp,Deftmp) , (bufr(1),buf(1))&
!>>>>    & , (Z(1),Zz(1)) , (ipshel(1),rpshel(1))
   DATA nam/4HTA1A , 4H    /
   DATA zeros/4*0/
   DATA bar/34/
   DATA hbdy/52/
   DATA qdmem2/63/
   DATA quad4/64/
   DATA tria3/83/
   DATA pcomp/5502 , 55/
   DATA pcomp1/5602 , 56/
   DATA pcomp2/5702 , 57/
   DATA sym/1/
   DATA mem/2/
   DATA symmem/3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PERFORM GENERAL INITIALIZATION
!
         IF ( nelem>90 ) THEN
!
!     ARRAY IG IS FIRST DIMENSIONED IN TA1ABD
!
            WRITE (nout,99001) sfm
99001       FORMAT (A25,', IG ARRAY IN TA1A TOO SMALL')
            CALL mesage(-61,0,0)
            RETURN
         ELSE
            buf1 = korsz(z) - sysbuf - 2
            buf2 = buf1 - sysbuf - 3
            buf3 = buf2 - sysbuf
            frstim = .TRUE.
            lstprp = 0
            kscalr = 0
            itabl = 0
            nosimp = -1
            nogox = 0
            nogo = 0
            m8 = -8
            comps = 1
            nopshl = -1
            npshel = 0
            oldid = 0
            CALL sswtch(40,l40)
!
!     READ THE ELEMENT CONNECTION TABLE.
!     IF PROPERTY DATA IS DEFINED FOR THE ELEMENT, READ THE EPT INTO
!     CORE AND SORT IF REQUIRED. THEN FOR EACH ECT ENTRY, LOOK UP AND
!     ATTACH THE PROPERTY DATA. WRITE ECT+EPT ON SCR1.
!     IF PROPERTY DATA NOT DEFINED FOR ELEMENT, COPY ECT DATA ON SCR1.
!     IF NO SIMPLE ELEMENTS IN ECT, RETURN.
!
!     FOR THE PLATE AND SHELL ELEMENTS REFERENCING PCOMP, PCOMP1 OR
!     PCOMP2 BULK DATA ENTRIES, PROPERTY DATA IN THE FORM OF PSHELL
!     BULK DATA ENTRY IS CALLED AND WRITTEN TO SCR1
!
            file = ect
            CALL open(*220,ect,z(buf1),rdrew)
            CALL skprec(ect,1)
            file = scr1
            CALL open(*360,scr1,z(buf3),wrtrew)
            buf(1) = ept
            CALL rdtrl(buf)
            noept = buf(1)
            IF ( buf(1)>=0 ) CALL preloc(*20,z(buf2),ept)
         ENDIF
!
!     LOCATE, ONE AT A TIME, SIMPLE ELEMENT TYPE IN ECT. IF PRESENT,
!     WRITE POINTER ON  SCR1. SET POINTERS AND, IF DEFINED, LOCATE AND
!     READ ALL PROPERTY DATA FROM EPT.
!
 20      CALL ectloc(*100,ect,buf,i)
         id = -1
         eltype = elem(i+2)
         CALL write(scr1,i,1,0)
         q4t3 = .FALSE.
         IF ( eltype==quad4 .OR. eltype==tria3 ) q4t3 = .TRUE.
         IF ( elem(i+10)==0 ) kscalr = 1
         m = elem(i+5)
         mm = elem(i+8)
         IF ( mm==0 ) THEN
!
!     EPT DATA NOT DEFINED FOR ELEMENT. COPY ECT DATA ON SCR1.
!
            buf(1) = m
            m1 = m + 1
            DO
               CALL read(*380,*80,ect,buf(2),m,0,flag)
               CALL write(scr1,buf(1),m1,0)
               nosimp = nosimp + 1
            ENDDO
         ELSE
            mx = mm
            noprop = 0
            IF ( elem(i+6)/=lstprp ) THEN
               IF ( noept<0 ) THEN
                  spag_nextblock_1 = 17
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     LOCATE PROPERTY CARD
!
               ll = 0
               CALL locate(*40,z(buf2),elem(i+6),flag)
               noprop = 1
               lstprp = elem(i+6)
            ELSE
               IF ( eltype==qdmem2 ) noprop = 1
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO
            IF ( ll+mm>=buf3 ) CALL mesage(-8,0,nam)
            IF ( noprop==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL read(*380,*60,ept,z(ll+1),mm,0,flag)
            ll = ll + mm
         ENDDO
!
!     CHECK FOR QUAD4 AND TRIA3 ELEMENTS WITH ONLY PCOMP CARDS
!
!     SET POINTER FOR NO PSHELL DATA, AND
!     READ PCOMP, PCOMP1 AND PCOMP2 DATA INTO CORE
!
 40      IF ( .NOT.q4t3 ) THEN
!
            IF ( noprop==0 ) THEN
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 3
         ELSE
            nopshl = 1
            spag_nextblock_1 = 10
         ENDIF
         CYCLE
!
!     CHECK FOR QUAD4 AND TRIA3 ELEMENTS WITH BOTH PCOMP AND PSHELL
!     CARDS
!
!     IF LL.GT.0 HERE, PSHELL DATA IS PRESENT,
!     NEED TO CHECK THE PRESENCE OF PCOMP DATA, AND RESET NOPSHL POINTER
!     IF NECCESSARY
!
!     EVENTUALLY, WE WILL HAVE
!
!     NOPSHL =-1, LOGIC ERROR FOR QUAD4/TRIA3 PROPERTY DATA
!            = 0, ONLY PSHELL DATA PRESENT
!            = 1, ONLY PCOMP TYPE DATA PRESENT
!            = 2, BOTH PSHELL AND PCOMP DATA PRESENT (SEE STA.760)
!
 60      IF ( q4t3 ) THEN
            IF ( ll<=0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nopshl = 0
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     Z(1) THRU Z(LL) CONTAIN PROPERTY DATA
!
         IF ( mm<=4 ) CALL sort(0,0,mm,1,z(1),ll)
         kn = ll/mm
         IF ( nopshl==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     READ ECT DATA FOR ELEMENT. LOOK UP PROPERTY DATA IF CURRENT ELEM.
!     HAS A PROPERTY ID DIFFERNENT FROM THAT OF THE PREVIOUS ELEMENT.
!     WRITE ECT + EPT (OR NEW GENERATED PSHELL) DATA ON SCR1.
!
         CALL read(*380,*80,ect,buf,m,0,flag)
         nosimp = nosimp + 1
         IF ( buf(2)/=id ) noprop = 1
         id = buf(2)
         buf(2) = buf(1)
         buf(1) = m + mm - 2
         IF ( noprop/=0 ) THEN
            IF ( q4t3 .AND. nopshl==1 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            npshel = 0
!
!     **************************************************
!
!     INTERNAL BINARY SEARCH ROUTINE
!
            klo = 1
            khi = kn
            k = (klo+khi+1)/2
            SPAG_Loop_1_1: DO
               kx = (k-1)*mx + itabl
               IF ( id<z(kx+1) ) THEN
                  khi = k
               ELSEIF ( id==z(kx+1) ) THEN
                  EXIT SPAG_Loop_1_1
               ELSE
                  klo = k
               ENDIF
               IF ( khi-klo<1 ) THEN
                  IF ( q4t3 .AND. nopshl>=1 ) THEN
                     spag_nextblock_1 = 11
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( khi-klo==1 ) THEN
                  IF ( k==klo ) THEN
                     k = khi
                  ELSE
                     k = klo
                  ENDIF
                  klo = khi
               ELSE
                  k = (klo+khi+1)/2
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL write(scr1,buf(1),m,0)
         IF ( q4t3 ) THEN
            IF ( npshel==1 ) THEN
               CALL write(scr1,ipshel(1),mm-1,0)
               noprop = 0
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         CALL write(scr1,z(kx+2),mm-1,0)
         npshel = 0
         noprop = 0
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      CALL write(scr1,0,0,1)
         GOTO 20
!
!     HERE WHEN ALL ELEMENTS HAVE BEEN PROCESSED.
!     IF NONE FOUND, EXIT.
!
 100     IF ( noept>=0 ) CALL close(ept,clsrew)
         CALL close(scr1,clsrew)
         IF ( nosimp==-1 ) RETURN
         nosimp = nosimp + 1
!
!     READ THE BGPDT INTO CORE (UNLESS ALL SCALAR PROBLEM).
!     READ THE SIL INTO CORE.
!
         nbgp = 0
         IF ( kscalr==0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = bgpdt
         CALL open(*360,bgpdt,z(buf1),rdrew)
         CALL fwdrec(*380,bgpdt)
         CALL read(*380,*120,bgpdt,z(1),buf2,1,nbgp)
         CALL mesage(m8,0,nam)
 120     CALL close(bgpdt,clsrew)
         spag_nextblock_1 = 6
      CASE (6)
         file = sil
         CALL open(*360,sil,z(buf1),rdrew)
         CALL fwdrec(*380,sil)
         CALL read(*380,*140,sil,z(nbgp+1),buf2-nbgp,1,nsil)
         CALL mesage(m8,0,nam)
 140     CALL close(sil,clsrew)
!
!     IF TEMP DEPENDENT MATERIALS IN PROBLEM,
!     OPEN GPTT AND POSITION TO PROPER THERMAL RECORD
!
         record = .FALSE.
         itemp = tempid
         IF ( tempid==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file = gptt
         CALL open(*420,gptt,z(buf3),rdrew)
         itmpid = nbgp + nsil + 3
         CALL read(*380,*160,gptt,z(itmpid-2),buf2-itmpid,1,nid)
         CALL mesage(-8,0,nam)
 160     ntmpid = itmpid - 5 + nid
         DO i = itmpid , ntmpid , 3
            IF ( tempid==z(i) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         GOTO 420
      CASE (7)
         idftmp = z(i+1)
         IF ( idftmp/=-1 ) deftmp = zz(i+1)
         n = z(i+2)
         IF ( n/=0 ) THEN
            record = .TRUE.
            n = n - 1
            IF ( n/=0 ) THEN
               DO i = 1 , n
                  CALL fwdrec(*380,gptt)
               ENDDO
            ENDIF
!
!     READ SET ID AND VERIFY FOR CORRECTNESS
!
            CALL read(*380,*400,gptt,iset,1,0,flag)
            IF ( iset/=tempid ) THEN
               WRITE (nout,99002) sfm , iset , tempid
99002          FORMAT (A25,' 4020, TA1A HAS PICKED UP TEMPERATURE SET',I9,' AND NOT THE REQUESTED SET',I9)
               CALL mesage(-61,0,0)
            ENDIF
!
!     INITIALIZE /TA1ETT/ VARIABLES
!
            oldeid = 0
            oldel = 0
            eorflg = .FALSE.
            endid = .TRUE.
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
!     LOOP THRU THE ECT+EPT DATA
!     CONVERT INTERNAL GRID POINT INDICES TO SIL VALUES FOR EACH NON-
!     SCALER ELEMENT, ATTACH THE BGPDT DATA AND,
!     IF A TEMPERATURE PROBLEM, COMPUTE THE ELEMENT TEMP FROM THE GPTT
!     DATA OR SUBSTITUTE THE DEFAULT TEMP.
!     WRITE THE RESULT ON THE EST, ONE RECORD PER ELEMENT TYPE
!
         CALL open(*360,scr1,z(buf1),rdrew)
         CALL open(*360,est,z(buf2),wrtrew)
         CALL fname(est,buf)
         CALL write(est,buf,2,1)
         locbgp = 1
!
!     RESET SOME OF THE /TA1ACM/ VALUES IF IT IS A -HEAT- FORMULATION
!
         IF ( iheat>0 ) THEN
!
!     TRIARG ELEMENT (TYPE 36)
            ig(36) = 14
!
!     TRAPRG ELEMENT (TYPE 37)
            ig(37) = 14
!
!     REPLACE QDMEM1 ELEMENT (TYPE 62) BY QDMEM ELEMENT (TYPE 16)
            ig(62) = 14
!
!     REPLACE QDMEM2 ELEMENT (TYPE 63) BY QDMEM ELEMENT (TYPE 16)
            ig(63) = 14
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     READ POINTER FROM SCR1. WRITE ELEMENT TYPE ON EST.
!     SET POINTERS FOR CONVERSION OF GRID NOS TO SIL VALUES.
!
         CALL read(*200,*400,scr1,i,1,0,flag)
         eltype = elem(i+2)
         CALL write(est,eltype,1,0)
!
!     ELEMENT TYPE  USED TO INDEX INTO /TA1ACM/
!     AND SET USED  /OPEN CORE/  BLOCKS NEGATIVE
!
         ig(eltype) = -ig(eltype)
         name = elem(i)
         jscalr = elem(i+10)
         mm = elem(i+9)
         lx = elem(i+12)
         IF ( elem(i+8)==0 ) lx = lx + 1
         mm = lx + mm - 1
         jtemp = elem(i+13)
         ntemp = 1
         IF ( jtemp==4 ) ntemp = elem(i+14) - 1
         DO
!         IHEX1/2/3,TRIM6
!
!     READ ECT + EPT DATA FOR ELEMENT FROM SCR1.
!
            CALL read(*380,*180,scr1,buf,1,0,flag)
            CALL read(*380,*400,scr1,buf(2),buf(1),0,flag)
!
            IF ( nogo==0 .AND. nogox==0 ) THEN
               IF ( eltype==bar ) THEN
!
!     FOR BAR AND BEAM ELEMENTS, STORE COORDINATES AND
!     COORDINATE SYSTEM ID FOR ORIENTATION VECTOR.
!
                  kx = 4*(buf(3)-1) + locbgp
                  IF ( buf(8)==1 ) THEN
                     buf(8) = z(kx)
                  ELSE
                     buf(8) = buf(5)
                     IF ( buf(8)==0 ) THEN
                        buf(8) = z(kx)
                     ELSE
                        k = 4*(buf(8)-1) + locbgp
                        bufr(5) = zz(k+1) - zz(kx+1)
                        bufr(6) = zz(k+2) - zz(kx+2)
                        bufr(7) = zz(k+3) - zz(kx+3)
                        buf(8) = 0
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
!
!     SAVE INTERNAL GRID NOS, THEN CONVERT TO SIL NOS
!     AND WRITE ECT + EPT DATA ON EST.
!
            DO l = lx , mm
               gpsav(l) = 0
               IF ( buf(l)/=0 ) THEN
                  gpsav(l) = buf(l)
                  k = gpsav(l) + nbgp
                  buf(l) = z(k)
               ENDIF
            ENDDO
            CALL write(est,buf(2),buf(1),0)
!
!     IF NOT SCALAR ELEMENT, PICK UP BGPDT DATA AND WRITE ON EST.
!
            IF ( jscalr==0 ) THEN
               DO l = lx , mm
                  IF ( gpsav(l)==0 ) THEN
                     CALL write(est,zeros,4,0)
                  ELSE
                     k = (gpsav(l)-1)*4
                     CALL write(est,z(k+1),4,0)
                     IF ( z(k+1)<0 ) THEN
                        IF ( eltype/=hbdy .OR. l<=lx+3 ) THEN
                           nogo = 1
                           CALL mesage(30,131,buf(2))
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
!
!     ELEMENT TEMP. IS NOT USED IN CONM1 AND CONM2 (ELEM TYPES 29 30)
!
               tgrid(1) = 0.
               IF ( eltype/=29 .AND. eltype/=30 ) THEN
!
!     IF NOT SCALAR ELEMENT, COMPUTE AND WRITE ELEMENT TEMP ON EST.
!
                  CALL ta1etd(buf(2),tgrid,ntemp)
                  IF ( eltype==bar ) tgrid(1) = (tgrid(1)+tgrid(2))/2.0
               ENDIF
               CALL write(est,tgrid,ntemp,0)
            ENDIF
         ENDDO
!
!     CLOSE EST RECORD AND RETURN FOR ANOTHER ELEMENT TYPE.
!
 180     CALL write(est,0,0,1)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     ALL ELEMENTS HAVE BEEN PROCESSED-- CLOSE FILES, WRITE TRAILER AND
!     EXIT
!
 200     CALL close(scr1,clsrew)
         CALL close(est,clsrew)
         CALL close(gptt,clsrew)
         buf(1) = est
         buf(2) = nosimp
         IF ( nogox/=0 ) nogo = 1
         IF ( nogo/=0 ) CALL mesage(-61,0,0)
         DO i = 3 , 7
            buf(i) = 0
         ENDDO
!
!     PROCESS /TA1ACM/ LOAD EST TRAILER WITH FLAGS
!     TO THE USED /OPEN CORE/ BLOCKS
!
         DO i = 1 , nelem
            IF ( ig(i)<0 ) THEN
               k = ig(i)
               DO j = i , nelem
                  IF ( ig(j)==k ) ig(j) = -ig(j)
               ENDDO
               j = ig(i)
               IF ( j>48 ) CALL mesage(-61,i,j)
               k = (j-1)/16
               j = j - k*16
               buf(k+5) = buf(k+5) + ktwo(j+16)
            ENDIF
         ENDDO
         CALL wrttrl(buf)
 220     RETURN
      CASE (10)
!
!     **************************************************
!
!     PROCESSING FOR LAMINATED COMPOSITES
!
!     INTERNAL SUBROUTINE TO READ PCOMP, PCOMP1 AND PCOMP2 DATA INTO
!     CORE
!
!
!     INITIALIZE VARIABLES AND SET POINTERS
!
         npc = 0
         npc1 = 0
         npc2 = 0
         typc = 0
         typc1 = 0
         typc2 = 0
         n = buf3 - ll
!
!     LOCATE PCOMP DATA AND READ INTO CORE
!
         ipc = ll + 1
         CALL locate(*260,z(buf2),pcomp,flag)
         CALL read(*380,*240,ept,z(ipc),n,0,npc)
         CALL mesage(-8,0,nam)
 240     IF ( npc>0 ) typc = 1
         n = n - npc
!
!     LOCATE PCOMP1 DATA AND READ INTO CORE
!
 260     ipc1 = ipc + npc
         CALL locate(*300,z(buf2),pcomp1,flag)
         CALL read(*380,*280,ept,z(ipc1),n,0,npc1)
         CALL mesage(-8,0,nam)
 280     IF ( npc1>0 ) typc1 = 1
         n = n - npc1
!
!     LOCATE PCOMP2 DATA AND READ INTO CORE
!
 300     ipc2 = ipc1 + npc1
         CALL locate(*340,z(buf2),pcomp2,flag)
         CALL read(*380,*320,ept,z(ipc2),n,0,npc2)
         CALL mesage(-8,0,nam)
 320     IF ( npc2>0 ) typc2 = 1
!
!     SET SIZE OF LPCOMP. NUMBER OF WORDS READ INTO CORE
!
 340     lpcomp = ipc2 + npc2
         IF ( lpcomp-1>ll ) comps = -1
!
!     CHECK FOR NO PCOMP, PCOMP1 OR PCOMP2 DATA
!     SET NOPSHL TO 2 IF BOTH 'PCOMP' AND PSHELL DATA ARE PRESENT
!
         IF ( nopshl==1 .AND. comps==1 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nopshl==0 .AND. comps==-1 ) nopshl = 2
         spag_nextblock_1 = 4
      CASE (11)
!
!     ***************************************************************
!
!     INTERNAL SUBROUTINE TO LOCATE A PARTICULAR PROPERTY ID FROM THE
!     'PCOMP' DATA AND TO CONVERT THE DATA TO PSHELL DATA FORMAT
!
!
!     Z(LL+1) THRU Z(LPCOMP) CONTAIN 'PCOMP' DATA
!
!     SET POINTERS
!
         kpc = 4
         kpc2 = 2
         len = 0
         nlay = 0
         eoeloc = 0
         pidloc = 0
         itype = -1
!
!     SEARCH FOR PID IN PCOMP DATA
!
         IF ( typc/=0 ) THEN
            z(lpcomp+1) = ipc
            npcomp = 0
            n = 2
!
            lpc = ipc1 - 1
            DO iip = ipc , lpc
               IF ( z(iip)==-1 ) THEN
                  z(lpcomp+n) = iip
                  z(lpcomp+n+1) = iip + 1
                  IF ( iip==lpc ) z(lpcomp+n+1) = 0
                  n = n + 2
                  npcomp = npcomp + 1
               ENDIF
            ENDDO
            IF ( lpcomp+n-2>=buf3 ) CALL mesage(-8,0,nam)
!
!     LOCATE PARTICULAR PID
!
            DO iip = 1 , npcomp
               eoeloc = z(lpcomp+2*iip)
               pidloc = z(lpcomp+2*iip-1)
               IF ( z(pidloc)==id ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     SEARCH FOR PID IN PCOMP1 DATA
!
         IF ( typc1/=0 ) THEN
!
            z(lpcomp+1) = ipc1
            npcomp = 0
            n = 2
!
            lpc1 = ipc2 - 1
            DO iip1 = ipc1 , lpc1
               IF ( z(iip1)==-1 ) THEN
                  z(lpcomp+n) = iip1
                  z(lpcomp+n+1) = iip1 + 1
                  IF ( iip1==lpc1 ) z(lpcomp+n+1) = 0
                  npcomp = npcomp + 1
                  n = n + 2
               ENDIF
            ENDDO
            IF ( lpcomp+n-2>=buf3 ) CALL mesage(-8,0,nam)
!
!     LOCATE PARTICULAR PID
!
            DO iip1 = 1 , npcomp
               eoeloc = z(lpcomp+2*iip1)
               pidloc = z(lpcomp+2*iip1-1)
               IF ( z(pidloc)==id ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     SEARCH FOR PID IN PCOMP2 DATA
!
         IF ( typc2/=0 ) THEN
!
            z(lpcomp+1) = ipc2
            npcomp = 0
            n = 2
!
            lpc2 = lpcomp - 1
            DO iip2 = ipc2 , lpc2
               IF ( z(iip2)==-1 ) THEN
                  z(lpcomp+n) = iip2
                  z(lpcomp+n+1) = iip2 + 1
                  IF ( iip2==lpc2 ) z(lpcomp+n+1) = 0
                  npcomp = npcomp + 1
                  n = n + 2
               ENDIF
            ENDDO
            IF ( lpcomp+n-2>=buf3 ) CALL mesage(-8,0,nam)
!
!     LOCATE PARTICULAR PID
!
            DO iip2 = 1 , npcomp
               eoeloc = z(lpcomp+2*iip2)
               pidloc = z(lpcomp+2*iip2-1)
               IF ( z(pidloc)==id ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     CHECK IF PID HAS BEEN FOUND IN 'PCOMP' DATA
!
         IF ( itype<0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 15
      CASE (12)
!
         len = eoeloc - pidloc
         nlay = (len-8)/kpc
         itype = 0
         spag_nextblock_1 = 15
      CASE (13)
!
         len = eoeloc - pidloc
         nlay = len - 8
         itype = 1
         spag_nextblock_1 = 15
      CASE (14)
!
         len = eoeloc - pidloc
         nlay = (len-8)/kpc2
         itype = 2
         spag_nextblock_1 = 15
      CASE (15)
!
!     DETERMINE DATA TO BE WRITTEN IN THE FORM OF PSHELL AND
!     WRITE TO SCR1
!
!     ITYPE  = 0,  PCOMP  ENTRY
!            = 1,  PCOMP1 ENTRY
!            = 2,  PCOMP2 ENTRY
!
!     CALCULATE LAMINATE THICKNESS - TLAM
!
         tlam = 0.
!
!     NOTE - IF Z(PIDLOC+7) IS EQUAL TO SYM OR SYMMEM, THE OPTION
!            TO MODEL EITHER A SYMMETRICAL OR SYMMETRICAL-MEMBRANE
!            LAMINATE HAS BEEN EXERCISED.  THEREFORE, THE TOTAL
!            THICKNESS IS TLAM = 2.0*TLAM
!
!     SET LAMOPT
!
         lamopt = z(pidloc+7)
!
!     PCOMP DATA
!
         IF ( itype<=0 ) THEN
            DO k = 1 , nlay
               ii = (pidloc+5) + 4*k
               tlam = tlam + zz(ii)
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
               ii = (pidloc+6) + 2*k
               tlam = tlam + zz(ii)
            ENDDO
            IF ( lamopt==sym .OR. lamopt==symmem ) tlam = 2.0*tlam
         ELSE
            ii = pidloc + 6
            tlam = zz(ii)*nlay
            IF ( lamopt==sym .OR. lamopt==symmem ) tlam = 2.0*tlam
         ENDIF
!
!
!     CREATE NEW PSHELL DATA AND WRITE TO ARRAY IPSHEL
!     NOTE - PID IS NOT WRITTEN TO IPSHEL
!
!     IPSHEL DATA TO BE WRITTEN TO SCR1
!     ============================================================
!     IPSHEL( 1)     = MID1     MEMBRANE MATERIAL
!     IPSHEL( 2)     = T        DEFAULT MEMBRANE THICKNESS
!     IPSHEL( 3)     = MID2     BENDING MATERIAL
!     IPSHEL( 4)     = 12I/T**3 BENDING STIFFNESS PARAMETER
!     IPSHEL( 5)     = MID3     TRANVERSE SHEAR MATERIAL
!     IPSHEL( 6)     = TS/T     SHEAR THICKNESS FACTOR
!     IPSHEL( 7)     = NSM      NON-STRUCTURAL MASS
!     IPSHEL(8,9)    = Z1,Z2    FIBRE DISTANCES
!     IPSHEL(10)     = MID4     MEMBRANE-BENDING COUPLING MATERIAL
!     IPSHEL(11)     = MCSID OR THETAM   //DATA FROM PSHELL
!     IPSHEL(12)     = FLAGM               OVERRIDDEN BY EST(18-19)//
!     IPSHEL(13)     = INTEGRATION ORDER (SET TO 0)
!                     (THE INTEGRATION ORDER IS NOT USED IN THE PROGRAM,
!                      BUT THIS WORD IS REQUIRED BECAUSE OF THE DESIGN
!                      OF THE EST DATA FOR THE CQUAD4/TRIA3 ELEMENTS.)
!     IPSHEL(14)     = SCSID OR THETAS   //DATA FROM PSHELL
!     IPSHEL(15)     = FLAGS               OVERRIDDEN BY EST(20-21)//
!     IPSHEL(16)     = ZOFF
!
!     CALCULATE ZOFFS
!
         IF ( z(pidloc+1)/=0 ) zoffs = zz(pidloc+1) + 0.5*tlam
         IF ( z(pidloc+1)==0 ) zoffs = 0.0
         IF ( abs(zoffs)<=0.001 ) zoffs = 0.0
!
!     SET POINTER TO INDICATE NEW PSHELL DATA CREATED
!
         npshel = 1
!
!     INITIALIZE IPSHEL ARRAY
!
         DO kk = 1 , 16
            ipshel(kk) = 0
         ENDDO
!
         rpshel(4) = 1.0
         rpshel(6) = 1.0
!
         ipshel(1) = id + 100000000
         rpshel(2) = tlam
         IF ( lamopt/=mem .AND. lamopt/=symmem ) THEN
            ipshel(3) = id + 200000000
            ipshel(5) = id + 300000000
         ENDIF
         rpshel(7) = zz(pidloc+2)
         rpshel(9) = 0.5*tlam
         rpshel(8) = -rpshel(9)
         IF ( lamopt/=sym .AND. lamopt/=mem .AND. lamopt/=symmem ) ipshel(10) = id + 400000000
         ipshel(13) = 0
         rpshel(16) = zoffs
!
!     DO NOT WRITE TO OUTPUT FILE IF PREVIOUS ID IS SAME AS NEW ID.
!     OTHERWISE, WRITE THE NEWLY CREATED PSHELL BULK DATA ENTRY TO
!     OUTPUT FILE IF DIAG 40 IS TURNED ON
!
         IF ( oldid/=id ) THEN
            IF ( frstim ) THEN
               frstim = .FALSE.
               IF ( l40==0 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!WKBR CALL PAGE (3)
               CALL page2(3)
               WRITE (nout,99003)
99003          FORMAT (//9X,'THE INPUT PCOMP, PCOMP1 OR PCOMP2 BULK DATA',' ENTRIES HAVE BEEN REPLACED BY THE FOLLOWING PSHELL',    &
                      &' AND MAT2 ENTRIES.',//)
            ENDIF
            IF ( l40/=0 ) THEN
               WRITE (nout,99004) id , ipshel(1) , rpshel(2) , ipshel(3) , rpshel(4) , ipshel(5) , rpshel(6) , rpshel(7) , rpshel(8)&
                                & , rpshel(9) , ipshel(10) , rpshel(11) , rpshel(14) , rpshel(16)
99004          FORMAT (' PSHELL',I14,I12,1X,1P,E11.4,I12,1X,1P,E11.4,I12,2(1X,1P,E11.4),/9X,2(1X,1P,E11.4),I12,2(1X,F11.1),1X,1P,   &
                     & E11.4)
            ENDIF
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
!
!     SET OLDID TO ID
!
         oldid = id
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     FATAL ERROR MESSAGES
!
 360     j = -1
         CALL mesage(j,file,nam)
         GOTO 420
 380     j = -2
         CALL mesage(j,file,nam)
         GOTO 420
 400     j = -3
         CALL mesage(j,file,nam)
         GOTO 420
      CASE (17)
         buf(1) = elem(i)
         buf(2) = elem(i+1)
         nogox = 1
         CALL mesage(30,11,buf)
         kx = itabl
         lstprp = elem(i+6)
         spag_nextblock_1 = 2
      CASE (18)
         ksavew = buf(3)
         buf(3) = id
         nogo = 1
         CALL mesage(30,10,buf(2))
         kx = itabl
         buf(3) = ksavew
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 420     buf(1) = tempid
         buf(2) = 0
         CALL mesage(-30,44,buf)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE ta1a
