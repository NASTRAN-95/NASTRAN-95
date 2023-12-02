!*==eqmckm.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE eqmckm
   IMPLICIT NONE
   USE c_bitpos
   USE c_blank
   USE c_eqmk1
   USE c_mpyadx
   USE c_names
   USE c_patx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: anyout , firstc , firsto , lascas
   INTEGER :: i , iapp , idg , ieg , ifil , ild , iprec , isb , isbz , isdone , itim , ivec , j , k , l , lcc , lkscc , lsetd ,     &
            & lvec , m , ms , msze , nent , ngset , nout , nvec , nzz , nzz1 , nzz2 , nzz3 , nzz4
   INTEGER , DIMENSION(3) :: idat
   INTEGER , SAVE :: kg
   INTEGER , DIMENSION(10) , SAVE :: kon
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(8) :: ocb
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE CALCULATES THE MPC CONSTRAINT FORCES AND CREATES
!     THE OUTPUT FILE FOR OFP.
!     TASKS INCLUDE CREATING THE SCRATCH FILES FOR THE CURRENT SUBCASES
!     (PGG, QG - ALSO USED IN EQUILIBRIUM CHECKS).
!     NOT CODED TO HANDLE CONICAL ELEMENTS OR SORT2.
!
!ZZ   COMMON /ZZSSA2/ ZZ(1)
   !>>>>EQUIVALENCE (mcb(1),ocb(1)) , (Ksystm(1),Isbz) , (Ksystm(2),Nout) , (Ksystm(15),Itim) , (Ksystm(16),Idat(1))
!WKBI 3/94 SPR93007
   !>>>>EQUIVALENCE (Ksystm(55),Iprec)
   DATA name/4HEQMC , 2HKM/
   DATA kon/1 , 20 , 0 , -1 , 0 , 0 , 0 , 0 , 1 , 8/
   DATA kg/1HG/
!
!                      KNG
!     PARTITION  KGG = ---- , ONLY KMG SAVED
!                      KMG
   anyout = .FALSE.
   nzz = korsz(zz(1))
   lcor = nzz
   luset = kuset
   IF ( kmpc/=0 ) THEN
      kmpc = -1
      CALL calcv(kscr(1),ug,un,um,zz)
      CALL ssg2a(kgg,0,kscr(2),kscr(1))
   ENDIF
!
!     UNAPPEND FILES
!
   nzz3 = nzz - 3*isbz + 1
   nzz2 = nzz3 + isbz
   nzz1 = nzz2 + isbz
   nzz4 = nzz3
   IF ( nskip<=0 ) nzz4 = nzz3 - isbz
!
   IF ( nskip<=1 ) GOTO 300
!
   IF ( kload>0 ) THEN
      trl(1) = kpgg
      mcb(1) = kscr(4)
      CALL curcas(*100,nskip,trl,mcb,zz,nzz2)
      kpgg = mcb(1)
   ENDIF
   GOTO 200
 100  kload = 0
!
 200  IF ( kugv>=0 ) THEN
      trl(1) = kugv
      mcb(1) = kscr(3)
      CALL curcas(*1100,nskip,trl,mcb,zz,nzz2)
      kugv = mcb(1)
   ENDIF
 300  IF ( kmpc==0 ) GOTO 1300
!
!                      PN
!     PARTITION  PGG = --- , ONLY PM SAVED
!                      PM
   IF ( kload>0 ) CALL ssg2a(kpgg,0,kscr(7),kscr(1))
!
!                 M
!     MULTIPLY  QM = -PM + KMG*UGV
!
   md(1) = kscr(5)
   mc(1) = kscr(7)
   CALL rdtrl(mc)
   IF ( kload<=0 ) mc(1) = 0
   ma(1) = kscr(2)
   mb(1) = kugv
   CALL rdtrl(ma)
   CALL rdtrl(mb)
   md(3) = ma(3)
   md(4) = mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
   md(5) = iprec
   mz = nzz
   mt = 0
   msab = 1
   msc = -1
!WKBR 11/93 SPR93007      MPR   = 1
   mpr = iprec
   mscr = kscr(1)
   CALL mpyad(zz,zz,zz)
   IF ( md(3)==md(2) ) md(4) = 1
   CALL wrttrl(md)
!
!                 N       T   M
!     MULTIPLY  QM = - GM * QM
!
   md(1) = kscr(6)
   mc(1) = 0
   ma(1) = kgm
   mb(1) = kscr(5)
   CALL rdtrl(ma)
   CALL rdtrl(mb)
   md(3) = ma(2)
   md(4) = mb(4)
!WKBR SPR93007      MD(5) = 1
   md(5) = iprec
   mt = 1
   msab = -1
   CALL mpyad(zz,zz,zz)
   IF ( md(3)==md(2) ) md(4) = 1
   CALL wrttrl(md)
!
!             N      M
!     MERGE QM AND QM ON SCRATCH 3
!
   trl(1) = kscr(5)
   CALL rdtrl(trl)
   IF ( trl(1)<0 ) GOTO 1100
   kmpc = 1
!
   CALL sdr1b(kscr(1),kscr(6),kscr(5),kscr(3),ug,un,um,kuset,0,0)
   trl(1) = kscr(3)
   CALL rdtrl(trl)
   msze = 2*trl(3)
!
!     CREATE MPC-CONSTRAINT OUTPUT FILE
!
   IF ( koqm<=0 ) GOTO 1300
   iapp = 10
   IF ( nskip<0 ) iapp = 20
   CALL mxcid(*1100,zz,kg,msze/2,2,kuset,kgpl,ksil,nzz2)
   DO i = 2 , msze , 2
      zz(i) = i/2
   ENDDO
!
!     SORT ON EXTERNAL ID
!
   nent = 2
   IF ( msze/=2 ) THEN
!
      ifil = ksil
      DO i = 3 , msze , 2
         IF ( zz(i)>zz(i-2) ) THEN
            nent = nent + 2
         ELSE
            trl(1) = zz(i)
            trl(2) = zz(i+1)
            CALL bishel(*1600,trl,nent,2,zz(1))
         ENDIF
      ENDDO
   ENDIF
!
   lkscc = msze + 1
   IF ( lkscc+146>=nzz4 ) GOTO 1700
   ivec = 0
   trl(1) = kscr(3)
   CALL rdtrl(trl)
   nvec = trl(2)
   itypu = 1
   incu = 1
   isdone = 0
   lascas = .FALSE.
   CALL gopen(kscc,zz(nzz1),rdrw)
   CALL gopen(kscr(3),zz(nzz3),rdrw)
!
   IF ( nskip<=0 ) THEN
!
!     POSITION LAMA
!
      ifil = klam
      CALL open(*1200,klam,zz(nzz4),rdrw)
      CALL read(*1900,*2000,klam,0,0,1,i)
      CALL read(*1900,*2000,klam,0,0,1,i)
   ENDIF
   ifil = koqm
   CALL open(*1800,koqm,zz(nzz2),wrtrw)
   CALL fname(koqm,trl(1))
   trl(3) = itim
   trl(4) = idat(1)
   trl(5) = idat(2)
   trl(6) = idat(3)
   trl(7) = 1
   CALL write(koqm,trl(1),7,1)
!
!     POSITION CASECC.  ASSUME USER WILL MISSET NSKIP
!
   IF ( nskip>1 ) THEN
      j = nskip - 1
      ifil = kscc
      DO i = 1 , j
         CALL fwdrec(*1900,kscc)
      ENDDO
   ENDIF
!
!     LOOP ON EACH VECTOR
!
 400  ivec = ivec + 1
!
!     SUBCASE ID
!
   CALL read(*500,*500,kscc,zz(lkscc),38,0,i)
   isb = zz(lkscc)
   ild = zz(lkscc+3)
   ieg = 0
!
!     CLEAN UP UNUSED WORDS
!
   i = lkscc + 10
   j = lkscc + 49
   DO k = i , j
      zz(k) = 0
   ENDDO
!
!     TITLES
!
   CALL fread(kscc,zz(lkscc+50),96,0)
   CALL fread(kscc,0,-31,0)
   CALL fread(kscc,lcc,1,0)
   CALL fread(kscc,0,-6,0)
!
!     MPCFORCE REQUEST
!
   ngset = 0
   lsetd = lkscc + 146
   CALL fread(kscc,ins(1),3,0)
   IF ( ins(1)<0 ) THEN
!
!     ALL REQUESTED
!
      CALL fread(kscc,0,0,1)
      GOTO 600
   ELSEIF ( ins(1)==0 ) THEN
!
!     NONE REQUESTED
!
      ifil = klam
      CALL fread(kscc,0,0,1)
      IF ( nskip<=0 ) CALL read(*1200,*1200,klam,trl(1),7,0,i)
      GOTO 900
   ELSE
!
!     SET REQUESTED
!
      CALL fread(kscc,0,-lcc+176,0)
!
!     SKIP SYMMETRY SEQUENCE
!
      CALL fread(kscc,i,1,0)
      IF ( i>0 ) CALL fread(kscc,0,-i,0)
      DO
         ifil = kscc
         CALL read(*1900,*2000,kscc,trl(1),2,0,i)
         IF ( trl(1)==ins(1) ) THEN
            IF ( lsetd+trl(2)>nzz4 ) GOTO 1700
            ngset = trl(2)
            CALL fread(kscc,zz(lsetd),ngset,1)
            GOTO 600
         ELSE
            CALL fread(kscc,0,-trl(2),0)
         ENDIF
      ENDDO
   ENDIF
!
!     EOF ON CASE CONTROL.  CHECK IF REALLY DONE
!
 500  IF ( nskip<0 ) THEN
!
      IF ( ivec>nvec ) GOTO 1200
      lascas = .TRUE.
      ivec = ivec - 1
   ELSE
      IF ( ivec>nvec ) GOTO 1200
      ifil = kscc
      GOTO 1900
   ENDIF
!
!     INITIALIZE
!
 600  IF ( lascas ) ivec = ivec + 1
   firstc = .TRUE.
   firsto = .TRUE.
   IF ( nskip<=0 ) THEN
      CALL read(*800,*800,klam,trl(1),7,0,i)
      ild = trl(1)
      ieg = trl(3)
   ENDIF
   mt = lkscc - 1
   DO j = 1 , 10
      i = j + mt
      zz(i) = kon(j)
   ENDDO
!
   IF ( ins(3)/=1 ) THEN
      CALL page2(2)
      WRITE (6,99001) uwm , name
99001 FORMAT (A25,' 2373, ONLY SORT1-REAL SUPPORTED IN ',2A4)
   ENDIF
   zz(mt+1) = ins(2) + iapp
   zz(mt+4) = isb
   zz(mt+5) = ild
   zz(mt+6) = ieg
   lvec = lsetd + ngset - 1
!
!     LOOP ON POINT DATA
!   MT = POINTER TO MATCID GRID ID, MS = POINTER TO CASECC GRID REQUEST.
!
   idg = -1
   mt = -1
   ms = lsetd
 700  DO
      mt = mt + 2
      IF ( mt>msze ) GOTO 900
      IF ( zz(mt)/10/=idg ) THEN
         idg = zz(mt)/10
         IF ( ins(1)<0 ) GOTO 1000
!
!     LOCATE POINT IN SET
!
         i = zz(ms)
         DO
            IF ( ms<lvec ) THEN
               IF ( idg<i ) EXIT
               IF ( idg==i ) GOTO 1000
               i = zz(ms+1)
               IF ( i<0 ) THEN
                  IF ( idg+i<=0 ) GOTO 1000
                  ms = ms + 2
                  i = zz(ms)
               ELSE
                  ms = ms + 1
               ENDIF
            ELSEIF ( ms==lvec ) THEN
!
!     LAST POINT IN SET
!
               IF ( i>=0 .OR. idg+i>0 ) THEN
                  IF ( idg/=i ) EXIT
               ENDIF
               GOTO 1000
            ELSE
               GOTO 900
            ENDIF
         ENDDO
!
!     NOT IN SET
!
         IF ( mt+2>=lkscc ) GOTO 900
      ENDIF
   ENDDO
!
!     END-OF-FILE
!
 800  isdone = 1
!
!     NO MORE GRIDS IN THIS SET
!
 900  IF ( .NOT.firsto ) CALL write(koqm,0,0,1)
   IF ( ivec+1>nvec ) GOTO 1200
   IF ( isdone/=0 ) GOTO 1200
!
!     CHECK IF COLUMN NEEDS TO BE SKIPPED
!
   ifil = kscr(3)
   IF ( firstc ) CALL fwdrec(*1900,kscr(3))
!
   IF ( .NOT.(lascas) ) GOTO 400
   GOTO 600
!
!     PROCESS THE GRID FOR OUTPUT
!
 1000 mcb(1) = 10*idg + ins(2)
   IF ( firstc ) THEN
      IF ( lvec+msze/2>nzz4 ) GOTO 1700
      inru = 1
      ilru = msze/2
      firstc = .FALSE.
      CALL unpack(*900,kscr(3),zz(lvec+1))
   ENDIF
!
   l = 1
   nent = 0
   ocb(3) = 0
   ocb(4) = 0
   ocb(5) = 0
   ocb(6) = 0
   ocb(7) = 0
   ocb(8) = 0
   m = min0(mt+10,msze)
   DO i = mt , m , 2
      j = zz(i)/10
      IF ( j/=idg ) EXIT
      k = zz(i+1) + lvec
      j = zz(i) - j*10 + 2
      IF ( j<=2 ) THEN
!
!     SCALAR
!
         l = 2
         j = 3
      ENDIF
!
      ocb(j) = zz(k)
      IF ( zz(k)/=0 ) nent = nent + 1
   ENDDO
!
   ocb(2) = l
   IF ( nent/=0 ) THEN
      IF ( firsto ) THEN
!
!     WRITE OUT CONTROL RECORD (ODD NUMBER)
!
         anyout = .TRUE.
         CALL write(koqm,zz(lkscc),146,1)
         firsto = .FALSE.
      ENDIF
!
!     WRITE AN ENTRY OUT
!
      CALL write(koqm,ocb(1),8,0)
   ENDIF
   GOTO 700
!
!     CLOSE FILES
!
 1100 CALL close(koqm,krw)
   koqm = -1
   anyout = .FALSE.
 1200 CALL close(kscr(3),krw)
   CALL close(kscc,krw)
   CALL close(klam,krw)
   IF ( anyout ) CALL eof(koqm)
   CALL close(koqm,krw)
   IF ( anyout ) THEN
      trl(1) = koqm
      trl(2) = nvec
      trl(3) = msze/2
      trl(4) = 0
      trl(5) = 0
      trl(6) = 1
      trl(7) = 0
      CALL wrttrl(trl)
   ENDIF
!
!     CALCULATE UPDATED QG FILE - SCRATCH 5
!
 1300 IF ( kspc/=0 ) THEN
      IF ( nskip>1 ) THEN
         trl(1) = kqg
         mcb(1) = kscr(5)
         CALL curcas(*1500,nskip,trl,mcb,zz,nzz2)
         kqg = mcb(1)
      ENDIF
   ENDIF
!
 1400 RETURN
!
!     ERROR CONDITIONS
!
!     KQG BAD
!
 1500 kspc = -1
   GOTO 1400
!
 1600 i = 7
   GOTO 2100
 1700 i = 8
   ifil = nzz4
   GOTO 2100
 1800 i = 1
   GOTO 2100
 1900 i = 2
   GOTO 2100
 2000 i = 3
 2100 CALL mesage(i,ifil,name)
!
!     MPC OUTPUT FILE NOT CREATED, BUT DATA IS ON SCR3 FOR EQMCKS
!
   CALL page2(2)
   WRITE (nout,99002) uwm , name
99002 FORMAT (A25,' 2380, MULTI-POINT CONSTRAINT FORCES NOT OUTPUT IN ',A4,A2,', SEE QUEUED MESSAGES.')
   GOTO 1100
END SUBROUTINE eqmckm
