!*==pltmrg.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pltmrg
   USE c_blank
   USE c_names
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bar , bgp , casep , casess , els , eqex , eqss , gps , pcdb , pltp , plts , quad4 , scr1 , srd , tria3
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , elid , file , i , i5 , icore , indx , iss , isx , item , j , k , ksil , lcore , n ,&
            & ncore , ngp , ngpel , ngpset , nss , nwds , offset , rc
   INTEGER , DIMENSION(2) , SAVE :: casecc , subr
   LOGICAL :: ident
   INTEGER , DIMENSION(3) :: z
   EXTERNAL bisloc , close , clstab , fname , fwdrec , gmmats , korsz , mesage , open , rdtrl , read , sfetch , sjump , smsg ,      &
          & sofcls , sofopn , sort , suread , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     MODULE PLTMRG WRITES GINO DATA BLOCKS WHICH ARE USED AS INPUT TO
!     THE PLOT MODULE FOR PLOTTING A SUBSTRUCTURE.
!
!     APRIL 1974
!
   !>>>>EQUIVALENCE (Z(1),Rz(1))
   DATA plts , eqss , subr , casecc/4HPLTS , 4HEQSS , 4HPLTM , 4HRG   , 4HCASE , 4HCC  /
   DATA casess , pcdb , pltp , gps , els/101 , 102 , 201 , 202 , 203/ , bgp , casep , eqex , scr1 , srd/204 , 205 , 206 , 301 , 1/ ,&
      & bar , quad4 , tria3/2HBR , 2HQ4 , 2HT3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         ncore = korsz(z)
         buf1 = ncore - sysbuf + 1
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
         ncore = buf5 - 1
         ngptot = 0
         lsil = 0
         npset = -1
         IF ( ncore<=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sofopn(z(buf3),z(buf4),z(buf5))
!
!     STRIP SUBSTRUCTURE RECORDS FROM CASESS AND WRITE CASEP (CASECC)
!
         file = casess
         CALL open(*140,casess,z(buf1),rdrew)
         file = casep
         CALL open(*140,casep,z(buf2),wrtrew)
         CALL fname(casep,buf)
         CALL write(casep,buf,2,1)
         file = casess
         DO
            CALL read(*160,*180,casess,z,2,1,nwds)
            IF ( z(1)==casecc(1) .AND. z(2)==casecc(2) ) THEN
               CALL read(*40,*20,casess,z,ncore,1,nwds)
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
 20      CALL write(casep,z,nwds,1)
         CALL read(*40,*20,casess,z,ncore,1,nwds)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 40      CALL clstab(casep,rew)
         CALL close(casess,rew)
!
!     BASIC GRID POINT DATA
!
         nm(1) = name(1)
         nm(2) = name(2)
         item = plts
         CALL sfetch(name,plts,srd,rc)
         IF ( rc/=1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     READ SUBSTRUCTURE NAMES AND TRANSFORMATION DATA INTO OPEN CORE.
!
         CALL suread(z,3,nwds,rc)
         IF ( rc/=1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nss = z(3)
         IF ( 14*nss>ncore ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL suread(z,14*nss,nwds,rc)
         IF ( rc/=1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icore = 14*nss + 1
!
!     READ THE BASIC GRID POINT DATA FROM THE PLTS ITEM OF EACH BASIC
!     SUBSTRUCTURE COMPRISING THE PSEUDOSTRUCTURE TO BE PLOTTED.
!     TRANSFORM THE COORDINATES TO THE BASIC COORDINATE SYSTEM OF THE
!     PSEUDOSTRUCTURE AND WRITE THEM ON BGP (BGPDT).
!
         file = bgp
         CALL open(*140,bgp,z(buf1),wrtrew)
         CALL fname(bgp,buf)
         CALL write(bgp,buf,2,1)
         j = 1
         spag_nextblock_1 = 2
      CASE (2)
         nm(1) = z(j)
         nm(2) = z(j+1)
         ngp = 0
         CALL sfetch(nm,plts,srd,rc)
         IF ( rc==1 ) THEN
            i = 1
            CALL sjump(i)
            ident = .FALSE.
            DO i = 1 , 3
               IF ( z(j+i+1)/=0 ) GOTO 50
               IF ( z(j+i+5)/=0 ) GOTO 50
               IF ( z(j+i+9)/=0 ) GOTO 50
               IF ( abs(rz(j+4*i+1)-1.0)>1.0E-4 ) GOTO 50
            ENDDO
            ident = .TRUE.
 50         SPAG_Loop_1_1: DO
               CALL suread(buf,4,nwds,rc)
               IF ( rc==2 ) EXIT SPAG_Loop_1_1
               ngp = ngp + 1
               IF ( ident .OR. buf(1)<0 ) THEN
                  CALL write(bgp,buf,4,0)
               ELSE
                  buf(5) = z(j+2)
                  buf(6) = z(j+3)
                  buf(7) = z(j+4)
                  CALL gmmats(z(j+5),3,3,-2,buf(2),3,1,0,buf(5))
                  CALL write(bgp,buf,1,0)
                  CALL write(bgp,buf(5),3,0)
               ENDIF
            ENDDO SPAG_Loop_1_1
         ELSE
            CALL smsg(rc-2,plts,nm)
         ENDIF
         ngptot = ngptot + ngp
         z(j+2) = ngp
         j = j + 14
         IF ( j<icore ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL write(bgp,0,0,1)
         CALL close(bgp,rew)
         buf(1) = bgp
         buf(2) = ngptot
         DO i = 3 , 7
            buf(i) = 0
         ENDDO
         CALL wrttrl(buf)
!
!     ALLOCATE 5 WORDS PER COMPONENT BASIC SUBSTRUCTURE AT THE TOP OF
!     OPEN CORE.  THIS ARRAY IS HEREINAFTER REFERRED TO AS *SDATA*
!
!     SAVE THE BASIC SUBSTRUCTURE NAMES AND THE NUMBER OF STRUCTURAL
!     GRID POINTS IN EACH IN SDATA.  DO NOT SAVE SUBSTRUCTURES FOR
!     WHICH NO PLTS ITEM WAS FOUND.
!
         j = 1
         DO i = 1 , nss
            IF ( z(14*i-11)/=0 ) THEN
               z(j) = z(14*i-13)
               z(j+1) = z(14*i-12)
               z(j+2) = z(14*i-11)
               j = j + 5
            ENDIF
         ENDDO
         IF ( j<=1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nss = j/5
         isx = nss*5
         icore = j
         lcore = ncore - j + 1
!
!     COMPUTE EQEX (EQEXIN)
!
!
!     READ THE EQEXIN DATA FROM THE PLTS ITEM OF EACH BASIC SUBSTRUCTURE
!     USE THREE WORDS IN OPEN CORE FOR EACH GRID POINT   (1) EXTERNAL
!     ID, (2) INTERNAL ID, (3) SUBSTRUCTURE SEQUENCE NUMBER IN SDATA.
!     INCREMENT THE INTERNAL IDS BY THE NUMBER OF GRID POINTS ON THE
!     PRECEDING SUBSTRUCTURES.
!
         k = icore
         ngp = 0
         DO i = 1 , nss
            nm(1) = z(5*i-4)
            nm(2) = z(5*i-3)
            CALL sfetch(nm,plts,srd,rc)
            n = 2
            CALL sjump(n)
            rc = 3
            IF ( n<0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            n = z(5*i-2)
            DO j = 1 , n
               CALL suread(z(k),2,nwds,rc)
               IF ( rc/=1 ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               z(k+1) = z(k+1) + ngp
               z(k+2) = i
               k = k + 3
               IF ( k+2>ncore ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            ngp = ngp + n
         ENDDO
!
!     SORT ON EXTERNAL IDS AND WRITE RECORD 1 OF EQEX.
!
         CALL sort(0,0,3,1,z(icore),3*ngp)
         file = eqex
         CALL open(*140,eqex,z(buf1),wrtrew)
         CALL fname(eqex,buf)
         CALL write(eqex,buf,2,1)
         DO i = 1 , ngp
            CALL write(eqex,z(icore+3*i-3),2,0)
         ENDDO
         CALL write(eqex,0,0,1)
!
!     SAVE THE TABLE IN OPEN CORE ON SCR1 TO USE IN COMPUTING RECORD 2
!     OF EQEX
!
         file = scr1
         CALL open(*140,scr1,z(buf2),wrtrew)
         CALL write(scr1,z(icore),3*ngp,1)
         CALL close(scr1,rew)
         CALL open(*140,scr1,z(buf2),rdrew)
!
!     READ GROUP 0 OF THE EQSS ITEM OF THE SUBSTRUCTURE TO BE PLOTTED
!     INTO OPEN CORE AT ICORE.  READ THE EXTERNAL AND INTERNAL IDS FOR
!     EACH CONTRIBUTING BASIC SUBSTRUCTURE INTO OPEN CORE FOLLOWING
!     GROUP 0.  SAVE THE CORE POINTERS FOR EACH GROUP IN SDATA.
!
         nm(1) = name(1)
         nm(2) = name(2)
         item = eqss
         CALL sfetch(name,eqss,srd,rc)
         IF ( rc/=1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL suread(z(icore),lcore,nwds,rc)
         IF ( rc/=2 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = icore + nwds
         n = z(icore+2)
         iss = 1
         SPAG_Loop_1_2: DO i = 1 , n
            IF ( iss<=isx ) THEN
               IF ( z(icore+2*i+2)==z(iss) .AND. z(icore+2*i+3)==z(iss+1) ) THEN
                  z(iss+3) = k
                  DO WHILE ( k+2<=ncore )
                     CALL suread(z(k),3,nwds,rc)
                     k = k + 2
                     IF ( rc/=1 ) THEN
                        z(iss+4) = (k-z(iss+3))/2
                        iss = iss + 5
                        CYCLE SPAG_Loop_1_2
                     ENDIF
                  ENDDO
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            j = 1
            CALL sjump(j)
         ENDDO SPAG_Loop_1_2
!
!     READ SIL NUMBERS INTO OPEN CORE.
!
         ksil = k - 1
         n = z(icore+3)
         IF ( ksil+n+1>ncore ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 1 , n
            CALL suread(z(ksil+i),2,nwds,rc)
            IF ( rc/=1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         lsil = z(ksil+n)
         spag_nextblock_1 = 3
      CASE (3)
         DO
!
!     READ THE TABLE OF EXTERNAL ID (GP), INTERNAL ID (IP), AND SUB-
!     STRUCTURE NUMBER (SSN) FROM SCR1 ONE ENTRY AT A TIME.  LOCATE
!     THE GP IN THE EQSS DATA INDICATED BY SSN AND LOOK UP THE SIL
!     NUMBER.  WRITE GP AND SIL ON EQEX.  IF GP NOT FOUND, THEN SIL=-1.
!
            CALL read(*160,*80,scr1,buf,3,0,n)
            i = buf(3)
            j = z(5*i-1)
            i5 = 5*i
            CALL bisloc(*60,buf(1),z(j),2,z(i5),k)
            i = z(j+k) + ksil
            buf(2) = 10*z(i) + 1
            CALL write(eqex,buf,2,0)
         ENDDO
 60      buf(2) = -1
         CALL write(eqex,buf,2,0)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      CALL write(eqex,0,0,1)
         CALL close(eqex,rew)
         CALL close(scr1,rew)
         buf(1) = eqex
         buf(2) = ngptot
         DO i = 3 , 7
            buf(i) = 0
         ENDDO
         CALL wrttrl(buf)
!
!     INTERPRET PLOT SETS AND GENERATE PLTP (PLTPAR)
!
!
!     AT PRESENT, ONLY ONE PLOT SET (DEFINED IN PHASE 1) IS ALLOWED.
!
!     PHASE 2 PLOT SET DEFINITIONS ARE IGNORED.
!
!     COPY PCDB TO PLTP
!
         file = pcdb
         CALL open(*140,pcdb,z(buf1),rdrew)
         CALL fwdrec(*160,pcdb)
         file = pltp
         CALL open(*140,pltp,z(buf2),wrtrew)
         CALL fname(pltp,buf)
         CALL write(pltp,buf,2,1)
         spag_nextblock_1 = 4
      CASE (4)
         CALL read(*120,*100,pcdb,z(icore),lcore,1,nwds)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 100     CALL write(pltp,z(icore),nwds,1)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 120     CALL close(pcdb,rew)
         CALL close(pltp,rew)
         buf(1) = pcdb
         CALL rdtrl(buf)
         buf(1) = pltp
         CALL wrttrl(buf)
         DO i = 1 , nss
            z(5*i-1) = 0
            z(5*i) = 1
         ENDDO
         npset = 1
!
!     GPSETS
!
!
!     LOCATE THE GPSETS DATA OF THE PLTS ITEM OF EACH BASIC SUBSTRUCTURE
!     AND READ THE NUMBER OF GRID POINTS IN THE ELEMENT SET.  STORE THIS
!     AS THE FOURTH ENTRY IN SDATA
!
         n = 3
         ngpset = 0
         item = plts
         DO i = 1 , nss
            nm(1) = z(5*i-4)
            nm(2) = z(5*i-3)
            CALL sfetch(nm,plts,srd,rc)
            CALL sjump(n)
            rc = 3
            IF ( n<0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(z(5*i-1),1,nwds,rc)
            IF ( rc/=1 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ngpset = ngpset + z(5*i-1)
         ENDDO
!
!     WRITE RECORDS 0 AND 1 OF GPS AND FIRST WORD OF RECORD 2.
!
         file = gps
         CALL open(*140,gps,z(buf1),wrtrew)
         CALL fname(gps,buf)
         CALL write(gps,buf,2,1)
         CALL write(gps,1,1,1)
         CALL write(gps,ngpset,1,0)
!
!     READ GPSETS DATA FROM THE PLTS ITEM OF EACH BASIC SUBSTRUCTURE.
!     INCREMENT THE ABSOLUTE VALUE OF THE POINTERS IN IT BY THE NUMBER
!     OF GRID POINTS IN THE ELEMENT SETS OF THE PRECEDING BASIC
!     SUBSTRUCTURES.  WRITE THE RESULT ON GPS (GPSETS).
!
         n = 3
         ngpset = 0
         DO i = 1 , nss
            CALL sfetch(z(5*i-4),plts,srd,rc)
            CALL sjump(n)
            CALL suread(z(icore),lcore,nwds,rc)
            IF ( rc/=2 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nwds = nwds - 1
            DO j = 1 , nwds
               IF ( z(icore+j)<0 ) THEN
                  z(icore+j) = z(icore+j) - ngpset
               ELSEIF ( z(icore+j)/=0 ) THEN
                  z(icore+j) = z(icore+j) + ngpset
               ENDIF
            ENDDO
            CALL write(gps,z(icore+1),nwds,0)
            ngpset = ngpset + z(5*i-1)
         ENDDO
         CALL clstab(gps,rew)
!
!     ELSETS
!
!
!     READ THE ELSETS DATA FROM THE PLTS ITEM OF EACH BASIC SUBSTRUCTURE
!     INCREMENT ALL NON-ZERO GRID POINT CONNECTION INDICES BY THE NUMBER
!     OF STRUCTURAL GRID POINTS OF THE PRECEDING SUBSTRUCTURES.  WRITE
!     THE RESULT ON ELS (ELSETS).
!
!     NOTE   THE ELEMENT TYPES WILL BE SCRAMBLED.  LIKE ELEMENT TYPES
!            FROM THE CONTRIBUTING BASIC SUBSTRUCTURES WILL NOT BE
!            GROUPED TOGETHER.
!
!     NOTE   THE BAR HAS ADDITIONALLY 6 OFFSET DATA VALUES. QUAD4 AND
!            TRIA3 HAS 1 OFFSET DATA EACH
!
         file = els
         CALL open(*140,els,z(buf1),wrtrew)
         CALL fname(els,buf)
         CALL write(els,buf,2,1)
         ngp = 0
!
!     LOOP OVER BASIC SUBSTRUCTURES
!
         DO i = 1 , nss
            nm(1) = z(5*i-4)
            nm(2) = z(5*i-3)
            CALL sfetch(nm,plts,srd,rc)
            n = 4
            CALL sjump(n)
            rc = 3
            IF ( n<0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            SPAG_Loop_2_3: DO
!
!     LOOP OVER ELEMENT TYPES
!
               CALL suread(buf,2,n,rc)
               IF ( rc==2 ) THEN
                  ngp = ngp + z(5*i-2)
                  EXIT SPAG_Loop_2_3
               ELSE
                  IF ( rc/=1 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL write(els,buf,2,0)
                  ngpel = buf(2)
                  offset = 0
                  IF ( buf(1)==bar ) offset = 6
                  IF ( buf(1)==quad4 .OR. buf(1)==tria3 ) offset = 1
                  SPAG_Loop_3_4: DO
!
!     LOOP OVER ELEMENTS
!
                     CALL suread(elid,1,n,rc)
                     IF ( rc/=1 ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     CALL write(els,elid,1,0)
                     IF ( elid<=0 ) EXIT SPAG_Loop_3_4
                     CALL suread(indx,1,n,rc)
                     CALL write(els,indx,1,0)
                     CALL suread(z(icore),ngpel+offset,n,rc)
                     IF ( rc/=1 ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     LOOP OVER CONNECTIONS
!
                     k = icore
                     DO j = 1 , ngpel
                        IF ( z(k)/=0 ) z(k) = z(k) + ngp
                        k = k + 1
                     ENDDO
                     CALL write(els,z(icore),ngpel+offset,0)
                  ENDDO SPAG_Loop_3_4
               ENDIF
            ENDDO SPAG_Loop_2_3
         ENDDO
!
         CALL write(els,0,0,1)
         CALL clstab(els,rew)
!
!     NORMAL MODULE COMPLETION
!
         CALL sofcls
         RETURN
      CASE (5)
!
!     ABNORMAL MODULE COMPLETION
!
         IF ( rc==2 ) rc = 3
         CALL smsg(rc-2,item,nm)
         spag_nextblock_1 = 9
      CASE (6)
         CALL smsg(rc+4,item,nm)
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 140     n = 1
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 160     n = 2
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 180     n = 3
         spag_nextblock_1 = 8
      CASE (7)
         n = 8
         spag_nextblock_1 = 8
      CASE (8)
         CALL mesage(n,file,subr)
         CALL close(file,rew)
         spag_nextblock_1 = 9
      CASE (9)
         CALL sofcls
         npset = -1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pltmrg
