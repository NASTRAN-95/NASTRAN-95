
SUBROUTINE pltmrg
   IMPLICIT NONE
   INTEGER Buf(7) , Lsil , Name(2) , Ngptot , Nm(2) , Norew , Npset , Sysbuf , Z(3)
   REAL Rd , Rdrew , Rew , Rz(1) , Wrt , Wrtrew
   COMMON /blank / Name , Ngptot , Lsil , Npset , Nm , Buf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Rz
   INTEGER bar , bgp , buf1 , buf2 , buf3 , buf4 , buf5 , casecc(2) , casep , casess , elid , els , eqex , eqss , file , gps , i ,  &
         & i5 , icore , indx , iss , isx , item , j , k , ksil , lcore , n , ncore , ngp , ngpel , ngpset , nss , nwds , offset ,   &
         & pcdb , pltp , plts , quad4 , rc , scr1 , srd , subr(2) , tria3
   LOGICAL ident
   INTEGER korsz
!
!     MODULE PLTMRG WRITES GINO DATA BLOCKS WHICH ARE USED AS INPUT TO
!     THE PLOT MODULE FOR PLOTTING A SUBSTRUCTURE.
!
!     APRIL 1974
!
   EQUIVALENCE (Z(1),Rz(1))
   DATA plts , eqss , subr , casecc/4HPLTS , 4HEQSS , 4HPLTM , 4HRG   , 4HCASE , 4HCC  /
   DATA casess , pcdb , pltp , gps , els/101 , 102 , 201 , 202 , 203/ , bgp , casep , eqex , scr1 , srd/204 , 205 , 206 , 301 , 1/ ,&
      & bar , quad4 , tria3/2HBR , 2HQ4 , 2HT3/
!
!     INITIALIZE
!
   ncore = korsz(Z)
   buf1 = ncore - Sysbuf + 1
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   buf5 = buf4 - Sysbuf
   ncore = buf5 - 1
   Ngptot = 0
   Lsil = 0
   Npset = -1
   IF ( ncore<=0 ) GOTO 1600
   CALL sofopn(Z(buf3),Z(buf4),Z(buf5))
!
!     STRIP SUBSTRUCTURE RECORDS FROM CASESS AND WRITE CASEP (CASECC)
!
   file = casess
   CALL open(*1300,casess,Z(buf1),Rdrew)
   file = casep
   CALL open(*1300,casep,Z(buf2),Wrtrew)
   CALL fname(casep,Buf)
   CALL write(casep,Buf,2,1)
   file = casess
   DO
      CALL read(*1400,*1500,casess,Z,2,1,nwds)
      IF ( Z(1)==casecc(1) .AND. Z(2)==casecc(2) ) THEN
         CALL read(*200,*100,casess,Z,ncore,1,nwds)
         GOTO 1600
      ENDIF
   ENDDO
 100  CALL write(casep,Z,nwds,1)
   CALL read(*200,*100,casess,Z,ncore,1,nwds)
   GOTO 1600
 200  CALL clstab(casep,Rew)
   CALL close(casess,Rew)
!
!     BASIC GRID POINT DATA
!
   Nm(1) = Name(1)
   Nm(2) = Name(2)
   item = plts
   CALL sfetch(Name,plts,srd,rc)
   IF ( rc/=1 ) GOTO 1100
!
!     READ SUBSTRUCTURE NAMES AND TRANSFORMATION DATA INTO OPEN CORE.
!
   CALL suread(Z,3,nwds,rc)
   IF ( rc/=1 ) GOTO 1200
   nss = Z(3)
   IF ( 14*nss>ncore ) GOTO 1600
   CALL suread(Z,14*nss,nwds,rc)
   IF ( rc/=1 ) GOTO 1200
   icore = 14*nss + 1
!
!     READ THE BASIC GRID POINT DATA FROM THE PLTS ITEM OF EACH BASIC
!     SUBSTRUCTURE COMPRISING THE PSEUDOSTRUCTURE TO BE PLOTTED.
!     TRANSFORM THE COORDINATES TO THE BASIC COORDINATE SYSTEM OF THE
!     PSEUDOSTRUCTURE AND WRITE THEM ON BGP (BGPDT).
!
   file = bgp
   CALL open(*1300,bgp,Z(buf1),Wrtrew)
   CALL fname(bgp,Buf)
   CALL write(bgp,Buf,2,1)
   j = 1
 300  Nm(1) = Z(j)
   Nm(2) = Z(j+1)
   ngp = 0
   CALL sfetch(Nm,plts,srd,rc)
   IF ( rc==1 ) THEN
      i = 1
      CALL sjump(i)
      ident = .FALSE.
      DO i = 1 , 3
         IF ( Z(j+i+1)/=0 ) GOTO 350
         IF ( Z(j+i+5)/=0 ) GOTO 350
         IF ( Z(j+i+9)/=0 ) GOTO 350
         IF ( abs(Rz(j+4*i+1)-1.0)>1.0E-4 ) GOTO 350
      ENDDO
      ident = .TRUE.
 350  DO
         CALL suread(Buf,4,nwds,rc)
         IF ( rc==2 ) EXIT
         ngp = ngp + 1
         IF ( ident .OR. Buf(1)<0 ) THEN
            CALL write(bgp,Buf,4,0)
         ELSE
            Buf(5) = Z(j+2)
            Buf(6) = Z(j+3)
            Buf(7) = Z(j+4)
            CALL gmmats(Z(j+5),3,3,-2,Buf(2),3,1,0,Buf(5))
            CALL write(bgp,Buf,1,0)
            CALL write(bgp,Buf(5),3,0)
         ENDIF
      ENDDO
   ELSE
      CALL smsg(rc-2,plts,Nm)
   ENDIF
   Ngptot = Ngptot + ngp
   Z(j+2) = ngp
   j = j + 14
   IF ( j<icore ) GOTO 300
   CALL write(bgp,0,0,1)
   CALL close(bgp,Rew)
   Buf(1) = bgp
   Buf(2) = Ngptot
   DO i = 3 , 7
      Buf(i) = 0
   ENDDO
   CALL wrttrl(Buf)
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
      IF ( Z(14*i-11)/=0 ) THEN
         Z(j) = Z(14*i-13)
         Z(j+1) = Z(14*i-12)
         Z(j+2) = Z(14*i-11)
         j = j + 5
      ENDIF
   ENDDO
   IF ( j<=1 ) GOTO 1800
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
      Nm(1) = Z(5*i-4)
      Nm(2) = Z(5*i-3)
      CALL sfetch(Nm,plts,srd,rc)
      n = 2
      CALL sjump(n)
      rc = 3
      IF ( n<0 ) GOTO 1200
      n = Z(5*i-2)
      DO j = 1 , n
         CALL suread(Z(k),2,nwds,rc)
         IF ( rc/=1 ) GOTO 1200
         Z(k+1) = Z(k+1) + ngp
         Z(k+2) = i
         k = k + 3
         IF ( k+2>ncore ) GOTO 1600
      ENDDO
      ngp = ngp + n
   ENDDO
!
!     SORT ON EXTERNAL IDS AND WRITE RECORD 1 OF EQEX.
!
   CALL sort(0,0,3,1,Z(icore),3*ngp)
   file = eqex
   CALL open(*1300,eqex,Z(buf1),Wrtrew)
   CALL fname(eqex,Buf)
   CALL write(eqex,Buf,2,1)
   DO i = 1 , ngp
      CALL write(eqex,Z(icore+3*i-3),2,0)
   ENDDO
   CALL write(eqex,0,0,1)
!
!     SAVE THE TABLE IN OPEN CORE ON SCR1 TO USE IN COMPUTING RECORD 2
!     OF EQEX
!
   file = scr1
   CALL open(*1300,scr1,Z(buf2),Wrtrew)
   CALL write(scr1,Z(icore),3*ngp,1)
   CALL close(scr1,Rew)
   CALL open(*1300,scr1,Z(buf2),Rdrew)
!
!     READ GROUP 0 OF THE EQSS ITEM OF THE SUBSTRUCTURE TO BE PLOTTED
!     INTO OPEN CORE AT ICORE.  READ THE EXTERNAL AND INTERNAL IDS FOR
!     EACH CONTRIBUTING BASIC SUBSTRUCTURE INTO OPEN CORE FOLLOWING
!     GROUP 0.  SAVE THE CORE POINTERS FOR EACH GROUP IN SDATA.
!
   Nm(1) = Name(1)
   Nm(2) = Name(2)
   item = eqss
   CALL sfetch(Name,eqss,srd,rc)
   IF ( rc/=1 ) GOTO 1100
   CALL suread(Z(icore),lcore,nwds,rc)
   IF ( rc/=2 ) GOTO 1600
   k = icore + nwds
   n = Z(icore+2)
   iss = 1
   DO i = 1 , n
      IF ( iss<=isx ) THEN
         IF ( Z(icore+2*i+2)==Z(iss) .AND. Z(icore+2*i+3)==Z(iss+1) ) THEN
            Z(iss+3) = k
            DO WHILE ( k+2<=ncore )
               CALL suread(Z(k),3,nwds,rc)
               k = k + 2
               IF ( rc/=1 ) THEN
                  Z(iss+4) = (k-Z(iss+3))/2
                  iss = iss + 5
                  GOTO 400
               ENDIF
            ENDDO
            GOTO 1600
         ENDIF
      ENDIF
      j = 1
      CALL sjump(j)
 400  ENDDO
!
!     READ SIL NUMBERS INTO OPEN CORE.
!
   ksil = k - 1
   n = Z(icore+3)
   IF ( ksil+n+1>ncore ) GOTO 1600
   DO i = 1 , n
      CALL suread(Z(ksil+i),2,nwds,rc)
      IF ( rc/=1 ) GOTO 1200
   ENDDO
   Lsil = Z(ksil+n)
 500  DO
!
!     READ THE TABLE OF EXTERNAL ID (GP), INTERNAL ID (IP), AND SUB-
!     STRUCTURE NUMBER (SSN) FROM SCR1 ONE ENTRY AT A TIME.  LOCATE
!     THE GP IN THE EQSS DATA INDICATED BY SSN AND LOOK UP THE SIL
!     NUMBER.  WRITE GP AND SIL ON EQEX.  IF GP NOT FOUND, THEN SIL=-1.
!
      CALL read(*1400,*700,scr1,Buf,3,0,n)
      i = Buf(3)
      j = Z(5*i-1)
      i5 = 5*i
      CALL bisloc(*600,Buf(1),Z(j),2,Z(i5),k)
      i = Z(j+k) + ksil
      Buf(2) = 10*Z(i) + 1
      CALL write(eqex,Buf,2,0)
   ENDDO
 600  Buf(2) = -1
   CALL write(eqex,Buf,2,0)
   GOTO 500
 700  CALL write(eqex,0,0,1)
   CALL close(eqex,Rew)
   CALL close(scr1,Rew)
   Buf(1) = eqex
   Buf(2) = Ngptot
   DO i = 3 , 7
      Buf(i) = 0
   ENDDO
   CALL wrttrl(Buf)
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
   CALL open(*1300,pcdb,Z(buf1),Rdrew)
   CALL fwdrec(*1400,pcdb)
   file = pltp
   CALL open(*1300,pltp,Z(buf2),Wrtrew)
   CALL fname(pltp,Buf)
   CALL write(pltp,Buf,2,1)
 800  CALL read(*1000,*900,pcdb,Z(icore),lcore,1,nwds)
   GOTO 1600
 900  CALL write(pltp,Z(icore),nwds,1)
   GOTO 800
 1000 CALL close(pcdb,Rew)
   CALL close(pltp,Rew)
   Buf(1) = pcdb
   CALL rdtrl(Buf)
   Buf(1) = pltp
   CALL wrttrl(Buf)
   DO i = 1 , nss
      Z(5*i-1) = 0
      Z(5*i) = 1
   ENDDO
   Npset = 1
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
      Nm(1) = Z(5*i-4)
      Nm(2) = Z(5*i-3)
      CALL sfetch(Nm,plts,srd,rc)
      CALL sjump(n)
      rc = 3
      IF ( n<0 ) GOTO 1200
      CALL suread(Z(5*i-1),1,nwds,rc)
      IF ( rc/=1 ) GOTO 1200
      ngpset = ngpset + Z(5*i-1)
   ENDDO
!
!     WRITE RECORDS 0 AND 1 OF GPS AND FIRST WORD OF RECORD 2.
!
   file = gps
   CALL open(*1300,gps,Z(buf1),Wrtrew)
   CALL fname(gps,Buf)
   CALL write(gps,Buf,2,1)
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
      CALL sfetch(Z(5*i-4),plts,srd,rc)
      CALL sjump(n)
      CALL suread(Z(icore),lcore,nwds,rc)
      IF ( rc/=2 ) GOTO 1600
      nwds = nwds - 1
      DO j = 1 , nwds
         IF ( Z(icore+j)<0 ) THEN
            Z(icore+j) = Z(icore+j) - ngpset
         ELSEIF ( Z(icore+j)/=0 ) THEN
            Z(icore+j) = Z(icore+j) + ngpset
         ENDIF
      ENDDO
      CALL write(gps,Z(icore+1),nwds,0)
      ngpset = ngpset + Z(5*i-1)
   ENDDO
   CALL clstab(gps,Rew)
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
   CALL open(*1300,els,Z(buf1),Wrtrew)
   CALL fname(els,Buf)
   CALL write(els,Buf,2,1)
   ngp = 0
!
!     LOOP OVER BASIC SUBSTRUCTURES
!
   DO i = 1 , nss
      Nm(1) = Z(5*i-4)
      Nm(2) = Z(5*i-3)
      CALL sfetch(Nm,plts,srd,rc)
      n = 4
      CALL sjump(n)
      rc = 3
      IF ( n<0 ) GOTO 1200
      DO
!
!     LOOP OVER ELEMENT TYPES
!
         CALL suread(Buf,2,n,rc)
         IF ( rc==2 ) THEN
            ngp = ngp + Z(5*i-2)
            EXIT
         ELSE
            IF ( rc/=1 ) GOTO 1200
            CALL write(els,Buf,2,0)
            ngpel = Buf(2)
            offset = 0
            IF ( Buf(1)==bar ) offset = 6
            IF ( Buf(1)==quad4 .OR. Buf(1)==tria3 ) offset = 1
            DO
!
!     LOOP OVER ELEMENTS
!
               CALL suread(elid,1,n,rc)
               IF ( rc/=1 ) GOTO 1200
               CALL write(els,elid,1,0)
               IF ( elid<=0 ) EXIT
               CALL suread(indx,1,n,rc)
               CALL write(els,indx,1,0)
               CALL suread(Z(icore),ngpel+offset,n,rc)
               IF ( rc/=1 ) GOTO 1200
!
!     LOOP OVER CONNECTIONS
!
               k = icore
               DO j = 1 , ngpel
                  IF ( Z(k)/=0 ) Z(k) = Z(k) + ngp
                  k = k + 1
               ENDDO
               CALL write(els,Z(icore),ngpel+offset,0)
            ENDDO
         ENDIF
      ENDDO
   ENDDO
!
   CALL write(els,0,0,1)
   CALL clstab(els,Rew)
!
!     NORMAL MODULE COMPLETION
!
   CALL sofcls
   RETURN
!
!     ABNORMAL MODULE COMPLETION
!
 1100 IF ( rc==2 ) rc = 3
   CALL smsg(rc-2,item,Nm)
   GOTO 1800
 1200 CALL smsg(rc+4,item,Nm)
   GOTO 1800
 1300 n = 1
   GOTO 1700
 1400 n = 2
   GOTO 1700
 1500 n = 3
   GOTO 1700
 1600 n = 8
 1700 CALL mesage(n,file,subr)
   CALL close(file,Rew)
 1800 CALL sofcls
   Npset = -1
END SUBROUTINE pltmrg
