
SUBROUTINE eqmckm
   IMPLICIT NONE
   INTEGER Idat(3) , Ilru , Incu , Inru , Ins(3) , Iprec , Isbz , Itim , Itypu , Kbgdt , Kcstm , Kexin , Kgg , Kgm , Kgpl , Klam ,  &
         & Kload , Kmpc , Knerw , Knrw , Koqm , Kpgg , Kqg , Krw , Kscc , Kscr(7) , Ksil , Kspc , Ksystm(80) , Kugv , Kuset , Lcor ,&
         & Luset , Ma(7) , Mb(7) , Mc(7) , Md(7) , Mpr , Msab , Msc , Mscr , Mt , Mz , Nout , Nskip , Parm(4) , Rdnrw , Rdrw ,      &
         & Trl(7) , Ug , Um , Un , Wrtnrw , Wrtrw , Zz(20000)
   REAL Skpb(2) , Skps(8)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /bitpos/ Um , Skps , Un , Ug
   COMMON /blank / Skpb , Nskip
   COMMON /eqmk1 / Kscc , Kexin , Kgpl , Kbgdt , Ksil , Kuset , Kgg , Kgm , Kugv , Kpgg , Kqg , Kcstm , Klam , Koqm , Kscr , Kmpc , &
                 & Kload , Kspc , Parm , Trl
   COMMON /mpyadx/ Ma , Mb , Mc , Md , Mz , Mt , Msab , Msc , Mpr , Mscr
   COMMON /names / Rdnrw , Rdrw , Wrtnrw , Wrtrw , Krw , Knrw , Knerw
   COMMON /patx  / Lcor , Ins , Luset
   COMMON /system/ Ksystm
   COMMON /unpakx/ Itypu , Inru , Ilru , Incu
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Zz
   LOGICAL anyout , firstc , firsto , lascas
   INTEGER i , iapp , idg , ieg , ifil , ild , isb , isdone , ivec , j , k , kg , kon(10) , l , lcc , lkscc , lsetd , lvec , m ,    &
         & mcb(7) , ms , msze , name(2) , nent , ngset , nvec , nzz , nzz1 , nzz2 , nzz3 , nzz4 , ocb(8)
   INTEGER korsz
!
!     THIS SUBROUTINE CALCULATES THE MPC CONSTRAINT FORCES AND CREATES
!     THE OUTPUT FILE FOR OFP.
!     TASKS INCLUDE CREATING THE SCRATCH FILES FOR THE CURRENT SUBCASES
!     (PGG, QG - ALSO USED IN EQUILIBRIUM CHECKS).
!     NOT CODED TO HANDLE CONICAL ELEMENTS OR SORT2.
!
!ZZ   COMMON /ZZSSA2/ ZZ(1)
   EQUIVALENCE (mcb(1),ocb(1)) , (Ksystm(1),Isbz) , (Ksystm(2),Nout) , (Ksystm(15),Itim) , (Ksystm(16),Idat(1))
!WKBI 3/94 SPR93007
   EQUIVALENCE (Ksystm(55),Iprec)
   DATA name/4HEQMC , 2HKM/
   DATA kon/1 , 20 , 0 , -1 , 0 , 0 , 0 , 0 , 1 , 8/
   DATA kg/1HG/
!
!                      KNG
!     PARTITION  KGG = ---- , ONLY KMG SAVED
!                      KMG
   anyout = .FALSE.
   nzz = korsz(Zz(1))
   Lcor = nzz
   Luset = Kuset
   IF ( Kmpc/=0 ) THEN
      Kmpc = -1
      CALL calcv(Kscr(1),Ug,Un,Um,Zz)
      CALL ssg2a(Kgg,0,Kscr(2),Kscr(1))
   ENDIF
!
!     UNAPPEND FILES
!
   nzz3 = nzz - 3*Isbz + 1
   nzz2 = nzz3 + Isbz
   nzz1 = nzz2 + Isbz
   nzz4 = nzz3
   IF ( Nskip<=0 ) nzz4 = nzz3 - Isbz
!
   IF ( Nskip<=1 ) GOTO 300
!
   IF ( Kload>0 ) THEN
      Trl(1) = Kpgg
      mcb(1) = Kscr(4)
      CALL curcas(*100,Nskip,Trl,mcb,Zz,nzz2)
      Kpgg = mcb(1)
   ENDIF
   GOTO 200
 100  Kload = 0
!
 200  IF ( Kugv>=0 ) THEN
      Trl(1) = Kugv
      mcb(1) = Kscr(3)
      CALL curcas(*1100,Nskip,Trl,mcb,Zz,nzz2)
      Kugv = mcb(1)
   ENDIF
 300  IF ( Kmpc==0 ) GOTO 1300
!
!                      PN
!     PARTITION  PGG = --- , ONLY PM SAVED
!                      PM
   IF ( Kload>0 ) CALL ssg2a(Kpgg,0,Kscr(7),Kscr(1))
!
!                 M
!     MULTIPLY  QM = -PM + KMG*UGV
!
   Md(1) = Kscr(5)
   Mc(1) = Kscr(7)
   CALL rdtrl(Mc)
   IF ( Kload<=0 ) Mc(1) = 0
   Ma(1) = Kscr(2)
   Mb(1) = Kugv
   CALL rdtrl(Ma)
   CALL rdtrl(Mb)
   Md(3) = Ma(3)
   Md(4) = Mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
   Md(5) = Iprec
   Mz = nzz
   Mt = 0
   Msab = 1
   Msc = -1
!WKBR 11/93 SPR93007      MPR   = 1
   Mpr = Iprec
   Mscr = Kscr(1)
   CALL mpyad(Zz,Zz,Zz)
   IF ( Md(3)==Md(2) ) Md(4) = 1
   CALL wrttrl(Md)
!
!                 N       T   M
!     MULTIPLY  QM = - GM * QM
!
   Md(1) = Kscr(6)
   Mc(1) = 0
   Ma(1) = Kgm
   Mb(1) = Kscr(5)
   CALL rdtrl(Ma)
   CALL rdtrl(Mb)
   Md(3) = Ma(2)
   Md(4) = Mb(4)
!WKBR SPR93007      MD(5) = 1
   Md(5) = Iprec
   Mt = 1
   Msab = -1
   CALL mpyad(Zz,Zz,Zz)
   IF ( Md(3)==Md(2) ) Md(4) = 1
   CALL wrttrl(Md)
!
!             N      M
!     MERGE QM AND QM ON SCRATCH 3
!
   Trl(1) = Kscr(5)
   CALL rdtrl(Trl)
   IF ( Trl(1)<0 ) GOTO 1100
   Kmpc = 1
!
   CALL sdr1b(Kscr(1),Kscr(6),Kscr(5),Kscr(3),Ug,Un,Um,Kuset,0,0)
   Trl(1) = Kscr(3)
   CALL rdtrl(Trl)
   msze = 2*Trl(3)
!
!     CREATE MPC-CONSTRAINT OUTPUT FILE
!
   IF ( Koqm<=0 ) GOTO 1300
   iapp = 10
   IF ( Nskip<0 ) iapp = 20
   CALL mxcid(*1100,Zz,kg,msze/2,2,Kuset,Kgpl,Ksil,nzz2)
   DO i = 2 , msze , 2
      Zz(i) = i/2
   ENDDO
!
!     SORT ON EXTERNAL ID
!
   nent = 2
   IF ( msze/=2 ) THEN
!
      ifil = Ksil
      DO i = 3 , msze , 2
         IF ( Zz(i)>Zz(i-2) ) THEN
            nent = nent + 2
         ELSE
            Trl(1) = Zz(i)
            Trl(2) = Zz(i+1)
            CALL bishel(*1600,Trl,nent,2,Zz(1))
         ENDIF
      ENDDO
   ENDIF
!
   lkscc = msze + 1
   IF ( lkscc+146>=nzz4 ) GOTO 1700
   ivec = 0
   Trl(1) = Kscr(3)
   CALL rdtrl(Trl)
   nvec = Trl(2)
   Itypu = 1
   Incu = 1
   isdone = 0
   lascas = .FALSE.
   CALL gopen(Kscc,Zz(nzz1),Rdrw)
   CALL gopen(Kscr(3),Zz(nzz3),Rdrw)
!
   IF ( Nskip<=0 ) THEN
!
!     POSITION LAMA
!
      ifil = Klam
      CALL open(*1200,Klam,Zz(nzz4),Rdrw)
      CALL read(*1900,*2000,Klam,0,0,1,i)
      CALL read(*1900,*2000,Klam,0,0,1,i)
   ENDIF
   ifil = Koqm
   CALL open(*1800,Koqm,Zz(nzz2),Wrtrw)
   CALL fname(Koqm,Trl(1))
   Trl(3) = Itim
   Trl(4) = Idat(1)
   Trl(5) = Idat(2)
   Trl(6) = Idat(3)
   Trl(7) = 1
   CALL write(Koqm,Trl(1),7,1)
!
!     POSITION CASECC.  ASSUME USER WILL MISSET NSKIP
!
   IF ( Nskip>1 ) THEN
      j = Nskip - 1
      ifil = Kscc
      DO i = 1 , j
         CALL fwdrec(*1900,Kscc)
      ENDDO
   ENDIF
!
!     LOOP ON EACH VECTOR
!
 400  ivec = ivec + 1
!
!     SUBCASE ID
!
   CALL read(*500,*500,Kscc,Zz(lkscc),38,0,i)
   isb = Zz(lkscc)
   ild = Zz(lkscc+3)
   ieg = 0
!
!     CLEAN UP UNUSED WORDS
!
   i = lkscc + 10
   j = lkscc + 49
   DO k = i , j
      Zz(k) = 0
   ENDDO
!
!     TITLES
!
   CALL fread(Kscc,Zz(lkscc+50),96,0)
   CALL fread(Kscc,0,-31,0)
   CALL fread(Kscc,lcc,1,0)
   CALL fread(Kscc,0,-6,0)
!
!     MPCFORCE REQUEST
!
   ngset = 0
   lsetd = lkscc + 146
   CALL fread(Kscc,Ins(1),3,0)
   IF ( Ins(1)<0 ) THEN
!
!     ALL REQUESTED
!
      CALL fread(Kscc,0,0,1)
      GOTO 600
   ELSEIF ( Ins(1)==0 ) THEN
!
!     NONE REQUESTED
!
      ifil = Klam
      CALL fread(Kscc,0,0,1)
      IF ( Nskip<=0 ) CALL read(*1200,*1200,Klam,Trl(1),7,0,i)
      GOTO 900
   ELSE
!
!     SET REQUESTED
!
      CALL fread(Kscc,0,-lcc+176,0)
!
!     SKIP SYMMETRY SEQUENCE
!
      CALL fread(Kscc,i,1,0)
      IF ( i>0 ) CALL fread(Kscc,0,-i,0)
      DO
         ifil = Kscc
         CALL read(*1900,*2000,Kscc,Trl(1),2,0,i)
         IF ( Trl(1)==Ins(1) ) THEN
            IF ( lsetd+Trl(2)>nzz4 ) GOTO 1700
            ngset = Trl(2)
            CALL fread(Kscc,Zz(lsetd),ngset,1)
            GOTO 600
         ELSE
            CALL fread(Kscc,0,-Trl(2),0)
         ENDIF
      ENDDO
   ENDIF
!
!     EOF ON CASE CONTROL.  CHECK IF REALLY DONE
!
 500  IF ( Nskip<0 ) THEN
!
      IF ( ivec>nvec ) GOTO 1200
      lascas = .TRUE.
      ivec = ivec - 1
   ELSE
      IF ( ivec>nvec ) GOTO 1200
      ifil = Kscc
      GOTO 1900
   ENDIF
!
!     INITIALIZE
!
 600  IF ( lascas ) ivec = ivec + 1
   firstc = .TRUE.
   firsto = .TRUE.
   IF ( Nskip<=0 ) THEN
      CALL read(*800,*800,Klam,Trl(1),7,0,i)
      ild = Trl(1)
      ieg = Trl(3)
   ENDIF
   Mt = lkscc - 1
   DO j = 1 , 10
      i = j + Mt
      Zz(i) = kon(j)
   ENDDO
!
   IF ( Ins(3)/=1 ) THEN
      CALL page2(2)
      WRITE (6,99001) Uwm , name
99001 FORMAT (A25,' 2373, ONLY SORT1-REAL SUPPORTED IN ',2A4)
   ENDIF
   Zz(Mt+1) = Ins(2) + iapp
   Zz(Mt+4) = isb
   Zz(Mt+5) = ild
   Zz(Mt+6) = ieg
   lvec = lsetd + ngset - 1
!
!     LOOP ON POINT DATA
!   MT = POINTER TO MATCID GRID ID, MS = POINTER TO CASECC GRID REQUEST.
!
   idg = -1
   Mt = -1
   ms = lsetd
 700  DO
      Mt = Mt + 2
      IF ( Mt>msze ) GOTO 900
      IF ( Zz(Mt)/10/=idg ) THEN
         idg = Zz(Mt)/10
         IF ( Ins(1)<0 ) GOTO 1000
!
!     LOCATE POINT IN SET
!
         i = Zz(ms)
         DO
            IF ( ms<lvec ) THEN
               IF ( idg<i ) EXIT
               IF ( idg==i ) GOTO 1000
               i = Zz(ms+1)
               IF ( i<0 ) THEN
                  IF ( idg+i<=0 ) GOTO 1000
                  ms = ms + 2
                  i = Zz(ms)
               ELSE
                  ms = ms + 1
               ENDIF
            ELSEIF ( ms==lvec ) THEN
!
!     LAST POINT IN SET
!
               IF ( i<0 .AND. idg+i<=0 ) GOTO 1000
               IF ( idg==i ) GOTO 1000
               EXIT
            ELSE
               GOTO 900
            ENDIF
         ENDDO
!
!     NOT IN SET
!
         IF ( Mt+2>=lkscc ) GOTO 900
      ENDIF
   ENDDO
!
!     END-OF-FILE
!
 800  isdone = 1
!
!     NO MORE GRIDS IN THIS SET
!
 900  IF ( .NOT.firsto ) CALL write(Koqm,0,0,1)
   IF ( ivec+1>nvec ) GOTO 1200
   IF ( isdone/=0 ) GOTO 1200
!
!     CHECK IF COLUMN NEEDS TO BE SKIPPED
!
   ifil = Kscr(3)
   IF ( firstc ) CALL fwdrec(*1900,Kscr(3))
!
   IF ( .NOT.(lascas) ) GOTO 400
   GOTO 600
!
!     PROCESS THE GRID FOR OUTPUT
!
 1000 mcb(1) = 10*idg + Ins(2)
   IF ( firstc ) THEN
      IF ( lvec+msze/2>nzz4 ) GOTO 1700
      Inru = 1
      Ilru = msze/2
      firstc = .FALSE.
      CALL unpack(*900,Kscr(3),Zz(lvec+1))
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
   m = min0(Mt+10,msze)
   DO i = Mt , m , 2
      j = Zz(i)/10
      IF ( j/=idg ) EXIT
      k = Zz(i+1) + lvec
      j = Zz(i) - j*10 + 2
      IF ( j<=2 ) THEN
!
!     SCALAR
!
         l = 2
         j = 3
      ENDIF
!
      ocb(j) = Zz(k)
      IF ( Zz(k)/=0 ) nent = nent + 1
   ENDDO
!
   ocb(2) = l
   IF ( nent/=0 ) THEN
      IF ( firsto ) THEN
!
!     WRITE OUT CONTROL RECORD (ODD NUMBER)
!
         anyout = .TRUE.
         CALL write(Koqm,Zz(lkscc),146,1)
         firsto = .FALSE.
      ENDIF
!
!     WRITE AN ENTRY OUT
!
      CALL write(Koqm,ocb(1),8,0)
   ENDIF
   GOTO 700
!
!     CLOSE FILES
!
 1100 CALL close(Koqm,Krw)
   Koqm = -1
   anyout = .FALSE.
 1200 CALL close(Kscr(3),Krw)
   CALL close(Kscc,Krw)
   CALL close(Klam,Krw)
   IF ( anyout ) CALL eof(Koqm)
   CALL close(Koqm,Krw)
   IF ( anyout ) THEN
      Trl(1) = Koqm
      Trl(2) = nvec
      Trl(3) = msze/2
      Trl(4) = 0
      Trl(5) = 0
      Trl(6) = 1
      Trl(7) = 0
      CALL wrttrl(Trl)
   ENDIF
!
!     CALCULATE UPDATED QG FILE - SCRATCH 5
!
 1300 IF ( Kspc/=0 ) THEN
      IF ( Nskip>1 ) THEN
         Trl(1) = Kqg
         mcb(1) = Kscr(5)
         CALL curcas(*1500,Nskip,Trl,mcb,Zz,nzz2)
         Kqg = mcb(1)
      ENDIF
   ENDIF
!
 1400 RETURN
!
!     ERROR CONDITIONS
!
!     KQG BAD
!
 1500 Kspc = -1
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
   WRITE (Nout,99002) Uwm , name
99002 FORMAT (A25,' 2380, MULTI-POINT CONSTRAINT FORCES NOT OUTPUT IN ',A4,A2,', SEE QUEUED MESSAGES.')
   GOTO 1100
END SUBROUTINE eqmckm
