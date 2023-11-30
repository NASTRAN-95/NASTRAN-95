
SUBROUTINE sdcompx(Zi,Zr,Zd) !HIDESTARS (*,Zi,Zr,Zd)
   IMPLICIT NONE
   INTEGER Ablk(15) , Bblk(20) , Blk(15) , C , Chlsky , Col , Config , Dba(7) , Dbc(7) , Dbl(7) , Forma , Frstpc , I1 , I2 , Iac ,  &
         & Incr1 , Incr2 , Itype1 , Itype2 , Itype3 , J1 , J2 , Jstr , Keep , Key(1) , Ksystm(63) , Lasti , Lastpl , Lcore ,        &
         & Lowtri , Nbpw , Nitems , Norew , Nout , Nr , Nrow , Nterms , Nzzadr , Nzzz , Power , Prc(2) , Prevc , Rlcmpx(4) , Row ,  &
         & S , Sc , Scr1 , Scr2 , Scr3 , Spflg , Sprow , Sqr , Start , Statfl , Sturm , Sym , Sysbuf , Typea , Wa , Wb , Words(4)
   REAL Cdp , Csp , Diag , Dsc , Dsr , Eofnrw , Minds , Ptshft , Rdnrw , Rdp , Rdrew , Rect , Rew , Rsp , Shftpt , Tmbpak , Tmgstr ,&
      & Tmio , Tmipak , Tml(4) , Tmpak , Tmpstr , Tmt(4) , Tmupak , Uprtri , Wrt , Wrtrew , Xns(1)
   DOUBLE PRECISION Ddc , Ddr , Mindd , Xdns(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /names / Rdnrw , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri ,       &
                 & Uprtri , Sym
   COMMON /ntime / Nitems , Tmio , Tmbpak , Tmipak , Tmpak , Tmupak , Tmgstr , Tmpstr , Tmt , Tml
   COMMON /packx / Itype1 , Itype2 , I1 , J1 , Incr1
   COMMON /sdcomx/ Row , C , Spflg , Start , Frstpc , Lastpl , Lasti , Sc , Iac , Nzzadr , Wa , Wb , Prevc , Nzzz , Sprow , S ,     &
                 & Blk , Ablk , Bblk
   COMMON /sfact / Dba , Dbl , Dbc , Scr1 , Scr2 , Lcore , Ddr , Ddc , Power , Scr3 , Mindd , Chlsky
   COMMON /sturmx/ Sturm , Shftpt , Keep , Ptshft , Nr
   COMMON /system/ Ksystm
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /unpakx/ Itype3 , I2 , J2 , Incr2
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Xns
   DOUBLE PRECISION Zd(2)
   INTEGER Zi(1)
   REAL Zr(2)
   CHARACTER*10 addi , unadd , unuse
   INTEGER andf , lshift , orf
   INTEGER begn , buf1 , buf2 , buf3 , buf4 , buf5 , c5max , cavg , ci , clos , cmax , dbname(2) , end , groups , i , icmplx(2) ,   &
         & icore , icrq , ier , ifl , ii , ijkl , ireal(2) , ispill , iswtch , j , jj , jklm , jlist , kerr , khr , kk , kr , krow ,&
         & kspill , l , left , loop , lstrow , lstspl , m , morcor , mtype(2) , n , nac , name , nbrstr , nkey , nlist , nn ,       &
         & nspill , nstr , null(20) , nwds , nwords , pcavg , pcgrou , pcmax , pcrow , pcsqr , prec , rc , savg , scra , scrb ,     &
         & scrc , scrd , subnam(5) , two24 , two25
   REAL cons , cspill , csqr , dens , drr , eps , epsmax , fc , fcmax , fnwds , rkhr , rs , save(6) , time , x
   DOUBLE PRECISION dsave3 , rd
   LOGICAL go , rowone , spill , splin , splout
   INTEGER maxc , nbrwds , sx
   EXTERNAL andf , lshift , orf
!
!     SDCOMP PERFORMS THE TRIANGULAR DECOMPOSITION OF A SYMMETRIC
!     MATRIX. THE MATRIX MAY BE REAL OR COMPLEX AND ITS PRECISION MAY
!     BE SNGL OR DBL
!
   !>>>>EQUIVALENCE (Nrow,Dba(3)) , (Forma,Dba(4)) , (Typea,Dba(5)) , (Jstr,Blk(5)) , (Col,Blk(4)) , (Nterms,Blk(6)) , (Xdns(1),Xns(1)) ,&
!>>>>    & (Row,Key(1)) , (Dsr,Ddr) , (rs,rd) , (Dsc,Ddc) , (Minds,Mindd)
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(28),Config) , (Ksystm(40),Nbpw) , (Ksystm(57),Statfl) ,              &
!>>>>    & (dbname(1),subnam(4))
   DATA subnam/4HSDCO , 2HMP , 3*1H / , nkey/6/ , begn/4HBEGN/ , end/4HEND / , two24/16777216/ , two25/33554432/
   DATA ireal , icmplx/4HREAL , 4H     , 4HCOMP , 4HLEX /
   DATA unuse , addi/'    UNUSED' , 'ADDITIONAL'/
!
!     STATEMENT FUNCTIONS
!
   nbrwds(i) = i + nwds*(i*(i+1))/2
   sx(x) = x - sqrt(amax1(x*(x+2.)+cmax*4.-cons,1.)) - 1.0
   maxc(j) = sqrt(float(2*j)/fnwds-float(4*cmax)) - 1.0
!
!     BUFFER ALLOCATION
!
   subnam(3) = begn
   CALL conmsg(subnam,5,0)
   buf1 = Lcore - Sysbuf
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   buf5 = buf4 - Sysbuf
   x = 1.0
   rkhr = 1.0E-10
!
!     INITIALIZATION AS A FUNCTION OF TYPE OF A MATRIX
!     RC   = 1 IF A IS REAL, 2 IF A IS COMPLEX
!     PREC = 1 IF A IS SINGLE, 2 IF A IS DOUBLE
!     NOTE - PRC(1) = 1, PRC(2) = 2, AND
!            PRC(3) = WORDS(1) = 1, PRC(4) = WORDS(2) = 2
!
   rc = Rlcmpx(Typea)
   mtype(1) = ireal(1)
   mtype(2) = ireal(2)
   IF ( rc/=1 ) THEN
      mtype(1) = icmplx(1)
      mtype(2) = icmplx(2)
   ENDIF
   prec = Prc(Typea)
   nwds = Words(Typea)
   fnwds = nwds
   Sturm = 0
!
!     CHECK INPUT PARAMETERS
!
   IF ( Dba(2)/=Dba(3) ) THEN
!
!     ERROR EXITS
!
      ier = -7
      ifl = 0
      GOTO 5000
   ELSE
      icrq = 100 - buf5
      IF ( buf5<100 ) GOTO 4800
      IF ( Nrow==1 ) THEN
!
!     DECOMPOSE A 1X1 MATRIX
!
         Itype1 = Typea
         Itype2 = Typea
         Itype3 = Typea
         Power = 0
         I1 = 1
         J1 = 1
         I2 = 1
         J2 = 1
         Incr1 = 1
         Incr2 = 1
         kk = 1
         null(1) = 1
         go = .FALSE.
         CALL gopen(Dba,Zi(buf1),Rdrew)
         CALL unpack(*1600,Dba,Zr)
         CALL close(Dba,Rew)
         CALL gopen(Dbl,Zi(buf1),Wrtrew)
         Dbl(2) = 0
         Dbl(6) = 0
         IF ( Typea==2 ) THEN
            Mindd = Zd(1)
            Ddr = Zd(1)
            IF ( Zd(1)==0 ) GOTO 1600
            GOTO 3300
         ELSEIF ( Typea==3 ) THEN
            Minds = sqrt(Zr(1)**2+Zr(2)**2)
            Dsr = Zr(1)
            Dsc = Zr(2)
            IF ( Minds==0 ) GOTO 1600
            GOTO 3300
         ELSEIF ( Typea==4 ) THEN
            Mindd = dsqrt(Zd(1)**2+Zd(2)**2)
            Ddr = Zd(1)
            Ddc = Zd(2)
            IF ( Mindd==0 ) GOTO 1600
            GOTO 3300
         ELSE
            Minds = Zr(1)
            Dsr = Zr(1)
            IF ( Zr(1)==0 ) GOTO 1600
            GOTO 3300
         ENDIF
      ELSE
!
!     GENERAL INITIALIZATION
!
         loop = 1
         ispill = buf5 - max0(100,Nrow/100)
         fcmax = 0.
      ENDIF
   ENDIF
 100  ispill = ispill - (loop-1)*Nrow/100
   nspill = ispill
   krow = Nrow + 1
   icrq = -ispill
   IF ( ispill<=0 ) GOTO 4800
   Zi(ispill) = 0
   pcgrou = 0
   pcavg = 0
   pcsqr = 0
   pcmax = 0
   csqr = 0.0
   savg = 0
   clos = alog(float(Nrow)) + 5.0
   clos = 999999
   pcrow = -clos
   Zi(1) = -Nrow
   icrq = Nrow - buf5
   IF ( Nrow>=buf5 ) GOTO 4800
   DO i = 2 , Nrow
      Zi(i) = 0
   ENDDO
   CALL fname(Dba,dbname)
   Power = 0
   scra = Scr3
   scrb = iabs(Dbc(1))
   go = .TRUE.
   spill = .FALSE.
   time = 0.
   groups = 0
   cmax = 0
   cons = 2*ispill/nwds
   c5max = maxc(ispill)
   Dsr = 1.0
   Dsc = 0.
   Minds = 1.E+25
   IF ( prec/=1 ) THEN
      Ddr = 1.0
      Ddc = 0.D0
      Mindd = 1.D+25
   ENDIF
   cavg = 0
   cspill = 0.
!
!     THE FOLLOWING CODE GENERATES THE ACTIVE COLUMN VECTOR FOR EACH
!     ROW, SPILL GROUPS AND TIMING AND USER INFORMATION ABOUT THE
!     DECOMPOSITION
!
   Blk(1) = Dba(1)
   Ablk(1) = scra
   Ablk(2) = Typea
   Ablk(3) = 0
   CALL gopen(Dba,Zi(buf1),Rdrew)
   CALL gopen(scra,Zi(buf2),Wrtrew)
   jlist = 1
   Row = 1
   jj = 0
   kk = 0
   nlist = 0
!
!     BEGIN A ROW BY LOCATING THE DIAGONAL ELEMENT
!
 200  Blk(8) = -1
   kr = krow
   DO
      CALL getstr(*300,Blk)
      IF ( prec==2 ) Jstr = 2*(Jstr-1) + 1
      IF ( Col>Row ) EXIT
      IF ( Col+Nterms-1>=Row ) THEN
!
!     DIAGONAL TERM IS LOCATED - COMPLETE ENTRIES IN THE FULL COLUMN
!     VECTOR AND SAVE THE TERMS FROM EACH STRING IN CORE
!
         IF ( .NOT.go ) GOTO 400
         Jstr = Jstr + (Row-Col)*nwds
         Nterms = Nterms - (Row-Col)
         Col = Row
         DO
            Zi(kr) = Col
            Zi(kr+1) = Nterms
            kr = kr + 2
            nstr = Jstr + Nterms*nwds - 1
            DO jj = Jstr , nstr
               Zr(kr) = Xns(jj)
               kr = kr + 1
            ENDDO
            n = Col + Nterms - 1
            DO j = Col , n
               IF ( Zi(j)<0 ) THEN
                  m = iabs(Zi(j))
                  Zi(j) = Row
                  IF ( m/=1 ) Zi(j+1) = -(m-1)
               ELSEIF ( Zi(j)==0 ) THEN
                  i = j
                  DO
                     i = i - 1
                     IF ( i<=0 ) GOTO 3400
                     IF ( Zi(i)<0 ) THEN
                        m = iabs(Zi(i))
                        Zi(i) = -(j-i)
                        Zi(j) = Row
                        left = m - (j-i+1)
                        IF ( left>0 ) Zi(j+1) = -left
                        EXIT
                     ELSEIF ( Zi(i)/=0 ) THEN
                        GOTO 3500
                     ENDIF
                  ENDDO
               ELSE
                  IF ( Zi(j)>Row .AND. Zi(j)<two24 ) Zi(j) = Zi(j) + two24 + two25
               ENDIF
            ENDDO
            icrq = kr - ispill
            IF ( kr>=ispill ) GOTO 4800
            CALL endget(Blk)
            CALL getstr(*500,Blk)
            IF ( prec==2 ) Jstr = 2*Jstr - 1
         ENDDO
      ELSE
         CALL endget(Blk)
      ENDIF
   ENDDO
 300  kk = kk + 1
   Zi(kk) = Row
   go = .FALSE.
 400  IF ( Blk(8)/=1 ) CALL skprec(Blk,1)
   Row = Row + 1
   IF ( Row>Nrow ) GOTO 1600
   GOTO 200
!
!     EXTRACT ACTIVE COLUMN VECTOR FROM THE FULL COLUMN VECTOR
!
 500  Iac = kr
   i = Iac
   j = Row
   Lastpl = -1
 600  IF ( Zi(j)<0 ) THEN
      j = j - Zi(j)
   ELSEIF ( Zi(j)==0 ) THEN
      kerr = 1051
      GOTO 4700
   ELSE
      IF ( Zi(j)<Row ) THEN
         Zi(i) = j
         i = i + 1
         j = j + 1
         GOTO 700
      ELSEIF ( Zi(j)/=Row ) THEN
         IF ( Zi(j)<two24 ) THEN
            j = j + 1
            GOTO 700
         ELSEIF ( Zi(j)<two25 ) THEN
            Zi(i) = j
            i = i + 1
            j = j + 1
            GOTO 700
         ELSE
            Zi(j) = Zi(j) - two25
         ENDIF
      ENDIF
      Zi(i) = -j
      IF ( Lastpl<0 ) Lastpl = i - Iac
      i = i + 1
      j = j + 1
   ENDIF
 700  IF ( j<=Nrow ) GOTO 600
   icrq = i - ispill
   IF ( i>ispill ) GOTO 4800
   C = i - Iac
   cmax = max0(cmax,C)
   c5max = maxc(ispill)
   nac = Iac + C - 1
   IF ( Lastpl<0 ) Lastpl = C
!
!     MAKE SPILL CALCULATIONS
!
   Spflg = 0
   fc = C
   Start = 2
   IF ( C==1 ) Start = 0
   Frstpc = 0
   IF ( .NOT.spill ) THEN
!
!     *1* CURRENT ROW IS NOT PART OF A SPILL GROUP. TEST FOR CREATION OF
!         A NEW SPILL GROUP
!
      IF ( C>c5max ) GOTO 1100
      GOTO 1200
   ELSEIF ( Row<lstrow ) THEN
!
!     *2* CURRENT ROW IS NEITHER FIRST NOR LAST IN CURRENT SPILL GROUP.
!         TEST FOR PASSIVE COL CONDITION. IF SO, TERMINATE SPILL GROUP.
!         TEST FOR POSSIBLE REDEFINITION OF SPILL GROUP. IF SO, TEST FOR
!         OVERFLOW OF REDEFINITION TABLE,  IF SO, TRY A DIFFERENT
!         STRATEGY FOR DEFINING S AND REDO PREFACE UP TO A LIMIT OF 3
!         TIMES.
!
      IF ( iabs(Zi(Iac+1))-Row<clos ) THEN
         ASSIGN 1400 TO iswtch
         IF ( C<=Zi(Sprow) ) GOTO 1400
         jj = nac
         DO WHILE ( iabs(Zi(jj))>lstrow )
            jj = jj - 1
         ENDDO
         Sc = jj - Iac
         m = sx(fc)
         IF ( Sc<=m ) GOTO 1400
         IF ( nspill+2>=buf5 ) GOTO 900
         S = m
         ijkl = max0(Iac,jj-(Sc-m))
         lstrow = iabs(Zi(ijkl))
         GOTO 1000
      ELSE
         ASSIGN 1300 TO iswtch
         lstrow = Row
         spill = .FALSE.
         Start = 0
         IF ( nspill+2>=buf5 ) GOTO 900
         GOTO 1000
      ENDIF
   ENDIF
!
!     *3* CURRENT ROW IS LAST ROW OF A SPILL GROUP. DETERMINE IF ANOTHER
!         SPILL GROUP FOLLOWS AND, IF SO, ITS RANGE
!
 800  Start = 0
   IF ( C>c5max ) GOTO 1100
   spill = .FALSE.
   GOTO 1200
 900  fcmax = amax1(fcmax,float(cmax))
   CALL close(scra,Rew)
   CALL close(Dba,Rew)
   loop = loop + 1
   IF ( loop<=3 ) GOTO 100
   icrq = buf5 - nspill - 2
   GOTO 4800
 1000 IF ( Zi(nspill)/=0 .AND. Zi(nspill)/=Sprow ) nspill = nspill + 3
   Zi(nspill) = Sprow
   Zi(nspill+1) = S
   Zi(nspill+2) = lstrow
   IF ( Row<lstrow ) THEN
      GOTO iswtch
   ELSEIF ( Row==lstrow ) THEN
      GOTO 800
   ELSE
      kerr = 1065
      GOTO 4700
   ENDIF
 1100 spill = .TRUE.
   Sprow = Row
   groups = groups + 1
   S = min0(sx(fc),Nrow-Sprow)
   IF ( loop/=1 ) THEN
      jj = Iac + S - 1
      DO WHILE ( iabs(Zi(jj))>Sprow+S )
         jj = jj - 1
      ENDDO
      S = jj - Iac + 1
      IF ( loop==3 ) S = min0(S,sx(fcmax))
   ENDIF
   S = min0(S,Nrow-Sprow)
   lstrow = iabs(Zi(Iac+S-1))
   Spflg = S
   Frstpc = lstrow
   savg = savg + S
   GOTO 1400
!
!     TEST FOR CONDITION IN WHICH PASSIVE COLUMNS ARE CREATED
!
 1200 Col = iabs(Zi(Iac+1))
   IF ( Row-pcrow<clos .OR. C<clos/2 .OR. Col-Row<clos ) GOTO 1400
!
!     CREATE PASSIVE COLUMNS BY CHANGING THEIR FIRST
!     APPEARANCE IN THE FULL COLUMN VECTOR
!
 1300 Frstpc = 2
   pcrow = Row
   pcavg = pcavg + C - 1
   pcsqr = pcsqr + (C-1)**2
   pcmax = max0(pcmax,C-1)
   pcgrou = pcgrou + 1
   nac = Iac + C - 1
   ijkl = Iac + 1
   DO i = ijkl , nac
      jj = iabs(Zi(i))
      IF ( Zi(jj)<=Row ) THEN
         Zi(jj) = Col
      ELSE
         Zi(jj) = min0(andf(Zi(jj),two24-1),Col)
      ENDIF
   ENDDO
!
!     WRITE ACTIVE COLUMN VECTOR
!
 1400 CALL write(scra,Key,nkey,0)
   CALL write(scra,Zi(Iac),C,1)
!
!     WRITE ROW OF INPUT MATRIX
!
   Ablk(8) = -1
   Ablk(12) = Row
   kr = krow
 1500 Ablk(4) = Zi(kr)
   nbrstr = Zi(kr+1)
   kr = kr + 2
   DO
      CALL putstr(Ablk)
      Ablk(7) = min0(Ablk(6),nbrstr)
      Jstr = Ablk(5)
      IF ( prec==2 ) Jstr = 2*Jstr - 1
      nstr = Jstr + Ablk(7)*nwds - 1
      DO jj = Jstr , nstr
         Xns(jj) = Zr(kr)
         kr = kr + 1
      ENDDO
      IF ( kr>=Iac ) THEN
         Ablk(8) = 1
         CALL endput(Ablk)
!
!     ACCUMULATE TIMING AND STATISTICS INFORMATION
!
         cavg = cavg + C
         csqr = csqr + C**2
         IF ( spill ) cspill = cspill + C**2
         Zi(Row) = C
         IF ( Row==Nrow ) EXIT
         Row = Row + 1
         GOTO 200
      ELSE
         CALL endput(Ablk)
         IF ( Ablk(7)==nbrstr ) GOTO 1500
         Ablk(4) = Ablk(4) + Ablk(7)
         nbrstr = nbrstr - Ablk(7)
      ENDIF
   ENDDO
!
!     HERE WHEN ALL ROWS PROCESSED - CLOSE FILES AND, IF SINGULAR
!     MATRIX, PRINT SINGULAR COLUMNS AND GIVE ALTERNATE RETURN
!
 1600 CALL close(scra,Rew)
   CALL close(Dba,Rew)
   IF ( go ) THEN
!
!     CALCULATE TIME ESTIMATE, PRINT USER INFORMATION AND
!     CHECK FOR SUFFICIENT TIME TO COMPLETE DECOMPOSITION
!
      dens = float(Dba(7))/10000.
      IF ( dens<0.01 ) dens = 0.01
      IF ( dens>99.99 ) dens = 99.99
      IF ( groups/=0 ) savg = savg/groups
      savg = max0(savg,1)
      time = 0.5*Tmt(Typea)*csqr + 0.5*(Tmpstr+Tmgstr)*float(pcsqr) + Tmpstr*float(cavg) + Tmio*(fnwds+1.0)*cspill/float(savg)
      morcor = nbrwds(cmax) - ispill + 1
!
      cavg = cavg/Nrow
      IF ( pcgrou/=0 ) pcavg = pcavg/pcgrou
      CALL tmtogo(ijkl)
      jklm = 1.E-6*time + 1.0
      icore = iabs(morcor)
      IF ( Dbc(1)>0 ) THEN
         unadd = unuse
         IF ( morcor>0 ) unadd = addi
         CALL page2(4)
         WRITE (Nout,99001,ERR=1700) Uim , mtype , dbname , Nrow , dens , jklm , cavg , pcavg , groups , savg , unadd , icore ,     &
                                   & cmax , pcmax , pcgrou , loop
99001    FORMAT (A29,' 3023 - PARAMETERS FOR ',2A4,' SYMMETRIC DECOMPOSITION OF DATA BLOCK ',2A4,5H (N =,I6,5H, D =,F6.2,2H%),/14X, &
                &17H  TIME ESTIMATE =,I7,17H          C AVG =,I6,17H         PC AVG =,I6,18H    SPILL GROUPS =,I6,                  &
                &17H          S AVG =,I6,/14X,A10,7H CORE =,I9,15H WORDS  C MAX =,I6,17H          PCMAX =,I6,18H       PC GROUPS =, &
               & I6,17H  PREFACE LOOPS =,I6)
         IF ( morcor>0 ) WRITE (Nout,99002)
99002    FORMAT (14X,'(FOR OPTIMAL OPERATION)')
      ENDIF
   ELSE
      CALL close(Dbl,Rew)
      CALL page2(3)
      WRITE (Nout,99003) Ufm , dbname , (Zi(i),i=1,kk)
99003 FORMAT (A23,' 3097. SYMMETRIC DECOMPOSITION OF DATA BLOCK ',2A4,' ABORTED BECAUSE THE FOLLOWING COLUMNS ARE SINGULAR -',/,    &
            & (5X,20I6,/))
      RETURN 1
   ENDIF
 1700 IF ( jklm>=ijkl ) THEN
      ier = -50
      ifl = jklm
      GOTO 5000
   ELSE
!
!     WRITE A END-OF-MATRIX STRING ON THE PASSIVE COLUMN FILE
!
      CALL gopen(scrb,Zi(buf2),Wrtrew)
      Bblk(1) = scrb
      Bblk(2) = Typea
      Bblk(3) = 0
      Bblk(8) = -1
      Bblk(12) = 1
      CALL putstr(Bblk)
      Bblk(4) = Nrow + 1
      Bblk(7) = 1
      Bblk(8) = 1
      CALL endput(Bblk)
      CALL close(scrb,Rew)
!
!     THE STAGE IS SET AT LAST TO PERFORM THE DECOMPOSITION -
!     SO LETS GET THE SHOW UNDERWAY
!
      CALL gopen(scra,Zi(buf1),Rdrew)
      CALL gopen(scrb,Zi(buf2),Rdrew)
      CALL gopen(Dbl,Zi(buf3),Wrtrew)
      scrc = Scr1
      scrd = Scr2
      IF ( Zi(nspill)/=0 ) nspill = nspill + 3
      Zi(nspill) = Nrow + 1
      splin = .FALSE.
      splout = .FALSE.
      spill = .FALSE.
      IF ( groups/=0 ) spill = .TRUE.
      Nzzz = orf(ispill-1,1)
      rowone = .FALSE.
      Dbl(2) = 0
      Dbl(6) = 0
      Dbl(7) = lshift(1,Nbpw-2-(Nbpw-32))
!
!     THIS 'NEXT TO SIGN' BIT WILL BE PICKED UP BY WRTTRL. ADD (NBPW-32)
!     SO THAT CRAY, WITH 48-BIT INTEGER, WILL NOT GET INTO TROUBLE
!
      Blk(1) = Dbl(1)
      Blk(2) = Typea
      Blk(3) = 1
      Wa = Nzzz
      Wb = Wa
      Prevc = 0
      Bblk(8) = -1
      CALL getstr(*3600,Bblk)
      kspill = ispill
   ENDIF
!
!     READ KEY WORDS AND ACTIVE COLUMN VECTOR FOR CURRENT ROW
!
 1800 name = scra
   IF ( splin ) name = scrd
   CALL fread(name,Key,nkey,0)
   Iac = C*nwds + 1
   CALL fread(name,Zi(Iac),C,1)
   nac = Iac + C - 1
   IF ( Zi(Iac)<0 ) Prevc = 0
   IF ( splin ) THEN
!
!     READ CURRENT PIVOT ROW FROM SPILL FILE. IF LAST ROW, CLOSE FILE
!
      Prevc = 0
      CALL fread(scrd,Zr,C*nwds,1)
      IF ( Row>=lstspl ) CALL close(scrd,Rew)
   ELSE
!
!     READ TERMS FROM THE INPUT MATRIX
!
      Ablk(8) = -1
      CALL getstr(*3700,Ablk)
      n = Iac - 1
      DO i = 1 , n
         Zr(i) = 0.
      ENDDO
      CALL sdcin(Ablk,Zi(Iac),C,Zr,Zr)
      DO
!
!     IF DEFINED, MERGE ROW FROM PASSIVE COLUMN FILE
!
         IF ( Row<Bblk(4) ) EXIT
         IF ( Row==Bblk(4) ) THEN
            CALL sdcin(Bblk,Zi(Iac),C,Zr,Zr)
            Bblk(8) = -1
            CALL getstr(*3800,Bblk)
         ELSE
            kerr = 1215
            GOTO 4700
         ENDIF
      ENDDO
   ENDIF
!
!     IF 1ST ROW OF A NEW SPILL GROUP, OPEN SCRATCH FILE TO WRITE
!
   IF ( .NOT.(rowone) ) THEN
      IF ( splout ) GOTO 2000
      IF ( Spflg==0 ) GOTO 2000
      splout = .TRUE.
      CALL gopen(scrc,Zi(buf4),Wrtrew)
      Sprow = Row
      S = Spflg
      lstrow = Frstpc
      Frstpc = 0
!
!     IF S WAS REDEFINED, GET NEW DEFINITION
!
      DO i = kspill , nspill , 3
         IF ( Row<Zi(i) ) EXIT
         IF ( Row==Zi(i) ) GOTO 1850
      ENDDO
      GOTO 1900
 1850 S = Zi(i+1)
      lstrow = Zi(i+2)
      kspill = i + 3
   ENDIF
!
!     WRITE ANY TERMS ALREADY CALCULATED WHICH ARE
!     BEYOND THE RANGE OF THE CURRENT SPILL GROUP
!
 1900 IF ( splout ) THEN
      n = 0
      ijkl = nac
      DO WHILE ( iabs(Zi(ijkl))>lstrow )
         ijkl = ijkl - 1
      ENDDO
      ijkl = ijkl + 1
      IF ( ijkl<=nac ) THEN
         DO i = ijkl , nac
            IF ( Zi(i)>0. ) n = n + 1
         ENDDO
         n = nwds*n*(n+1)/2
      ENDIF
      CALL write(scrc,n,1,0)
      CALL write(scrc,Zr(Nzzz-n),n,1)
!
!     MOVE WA TO ACCOUNT FOR ANY TERMS JUST WRITTEN
!
      IF ( n/=0 ) THEN
         j = Nzzz
         i = Nzzz - n
         IF ( Nzzz-Wa==n ) THEN
            Wa = j
         ELSE
            DO
               j = j - 1
               i = i - 1
               Zr(j) = Zr(i)
               IF ( i<=Wa ) THEN
                  Wa = j
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDIF
   ENDIF
!
!     IF THE PIVOTAL ROW DID NOT COME FROM THE SPILL FILE, IT IS CREATED
!
 2000 IF ( splin ) GOTO 2300
   i = Iac
   l = Wa
   IF ( prec==2 ) l = (Wa-1)/2 + 1
   IF ( Typea==2 ) THEN
!
!     CREATE PIVOT ROW IN RDP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
      IF ( Zi(Iac)>=0 ) THEN
         DO j = 1 , C
            IF ( Zi(i)>=0 ) THEN
               Zd(j) = Zd(j) + Zd(l)
               l = l + 1
            ENDIF
            i = i + 1
         ENDDO
      ENDIF
      ASSIGN 2200 TO khr
      IF ( Zd(1)==0 ) GOTO 3100
      GOTO 2200
   ELSEIF ( Typea==3 ) THEN
!
!     CREATE PIVOT ROW IN CSP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
      IF ( Zi(Iac)>=0 ) THEN
         ci = 2*C - 1
         DO j = 1 , ci , 2
            IF ( Zi(i)>=0 ) THEN
               Zr(j) = Zr(j) + Zr(l)
               Zr(j+1) = Zr(j+1) + Zr(l+1)
               l = l + 2
            ENDIF
            i = i + 1
         ENDDO
      ENDIF
      save(3) = sqrt(Zr(1)**2+Zr(2)**2)
      IF ( save(3)==0 ) GOTO 3200
      DO WHILE ( sqrt(Dsr**2+Dsc**2)>=10. )
         Dsr = Dsr/10.
         Dsc = Dsc/10.
         Power = Power + 1
      ENDDO
      DO WHILE ( sqrt(Dsr**2+Dsc**2)<=0.1 )
         Dsr = Dsr*10.
         Dsc = Dsc*10.
         Power = Power - 1
      ENDDO
      rs = Dsr*Zr(1) - Dsc*Zr(2)
      Dsc = Dsr*Zr(2) + Dsc*Zr(1)
      drr = rs
      Minds = amin1(save(3),Minds)
      GOTO 2300
   ELSEIF ( Typea==4 ) THEN
!
!     CREATE PIVOT ROW IN CDP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
      IF ( Zi(Iac)>=0 ) THEN
         ci = 2*C - 1
         DO j = 1 , ci , 2
            IF ( Zi(i)>=0 ) THEN
               Zd(j) = Zd(j) + Zd(l)
               Zd(j+1) = Zd(j+1) + Zd(l+1)
               l = l + 2
            ENDIF
            i = i + 1
         ENDDO
      ENDIF
!
!     IN COMPARING THE SOURCE CODES HERE FOR CSP AND CDP COMPUTATION,
!     IT IS DECIDED TO CHANGE THE ORIGINAL LINES (COMMENTED OUT) TO THE
!     NEW LINES USING DSAVE3 INSTEAD OF RD       BY G.CHAN/UNISYS, 8/84
!
      dsave3 = dsqrt(Zd(1)**2+Zd(2)**2)
      IF ( dsave3==0 ) GOTO 3200
      DO WHILE ( dsqrt(Ddr**2+Ddc**2)>=10.D0 )
         Ddr = Ddr/10.D0
         Ddc = Ddc/10.D0
         Power = Power + 1
      ENDDO
      DO WHILE ( dsqrt(Ddr**2+Ddc**2)<=0.1D0 )
         Ddr = Ddr*10.D0
         Ddc = Ddc*10.D0
         Power = Power - 1
      ENDDO
      rd = Ddr*Zd(1) - Ddc*Zd(2)
      Ddc = Ddr*Zd(2) + Ddc*Zd(1)
      Ddr = rd
      Mindd = dmin1(dsave3,Mindd)
      GOTO 2300
   ELSE
!
!     CREATE PIVOT ROW IN RSP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
      IF ( Zi(Iac)>=0 ) THEN
         DO j = 1 , C
            IF ( Zi(i)>=0 ) THEN
               Zr(j) = Zr(j) + Zr(l)
               l = l + 1
            ENDIF
            i = i + 1
         ENDDO
      ENDIF
      ASSIGN 2100 TO khr
      IF ( Zr(1)==0 ) GOTO 3100
   ENDIF
 2100 DO WHILE ( abs(Dsr)>=10. )
      Dsr = Dsr/10.
      Power = Power + 1
   ENDDO
   DO WHILE ( abs(Dsr)<=0.1 )
      Dsr = Dsr*10.
      Power = Power - 1
   ENDDO
   Dsr = Dsr*Zr(1)
   Minds = amin1(abs(Zr(1)),Minds)
!
!     COUNTING SIGN CHANGES OF THE LEADING PRINCIPLE MINORS IN STURM
!     SEQ.
!
   IF ( Zr(1)<0. ) Sturm = Sturm + 1
   GOTO 2300
 2200 DO WHILE ( dabs(Ddr)>=10.0D0 )
      Ddr = Ddr/10.D0
      Power = Power + 1
   ENDDO
   DO WHILE ( dabs(Ddr)<=0.1D0 )
      Ddr = Ddr*10.D0
      Power = Power - 1
   ENDDO
   Ddr = Ddr*Zd(1)
   Mindd = dmin1(dabs(Zd(1)),Mindd)
!
!     COUNTING SIGN CHANGES (STURM SEQUENCE PROPERTY)
!
   IF ( Zd(1)<0.D0 ) Sturm = Sturm + 1
!
!     CALCULATE WB
!
 2300 Lasti = 1
   IF ( Start==0 ) GOTO 2600
   IF ( splin ) THEN
      ci = C - (Start-2)
      Sc = ci
      jj = nac
      IF ( .NOT.(splout) ) THEN
         IF ( (ci*(ci+1)+2*C)*nwds/2+C<=Nzzz ) GOTO 2400
         kerr = 1288
         GOTO 4700
      ENDIF
   ELSEIF ( splout ) THEN
      ci = C
      Sc = lstrow - Sprow
      jj = min0(nac,Iac+Start+Sc-2)
   ELSE
      ci = C
      Sc = C
      GOTO 2400
   ENDIF
   DO WHILE ( iabs(Zi(jj))>lstrow )
      jj = jj - 1
   ENDDO
   Sc = jj - Iac - Start + 2
   IF ( Sc<=0 ) THEN
      Sc = 0
      Wb = Wa
      GOTO 2500
   ENDIF
 2400 Nterms = Sc*(ci-1) - (Sc*(Sc-1))/2
   nwords = Nterms*nwds
   Wb = Nzzz - nwords
   IF ( prec==2 ) Wb = orf(Wb-1,1)
   IF ( Wb<Iac+C ) THEN
      kerr = 1288
      GOTO 4700
   ELSEIF ( Wb>Wa+nwds*Prevc ) THEN
      kerr = 1170
      GOTO 4700
   ENDIF
 2500 IF ( splin .AND. Row==lstspl ) splin = .FALSE.
   Lasti = min0(Start+Sc-1,C)
   IF ( Sc/=0 ) THEN
!
!     NOW CALCULATE CONTRIBUTIONS FROM CURRENT PIVOT ROW TO SECOND TERM
!     IN EQUATION (4) IN MEMO CWM-19. NOTE-TERMS ARE CALCULATED ONLY
!     FOR ROW/COL COMBINATIONS IN THE CURRENT SPILL GROUP
!
      IF ( Typea==2 ) THEN
         CALL sdcom2(Zi,Zi(Iac),Zr(Wa+2*Prevc),Zr(Wb))
      ELSEIF ( Typea==3 ) THEN
         CALL sdcom3(Zi,Zi(Iac),Zr(Wa+2*Prevc),Zr(Wb))
      ELSEIF ( Typea==4 ) THEN
         CALL sdcom4(Zi,Zi(Iac),Zr(Wa+4*Prevc),Zr(Wb))
      ELSE
         CALL sdcom1(Zi,Zi(Iac),Zr(Wa+Prevc),Zr(Wb))
      ENDIF
   ENDIF
!
!     SHIP PIVOT ROW OUT TO EITHER MATRIX OR SPILL FILE
!
 2600 IF ( Lasti==C ) THEN
!
!     PIVOT ROW GOES TO OUTPUT FILE - IF REQUIRED, CONVERT TO CHOLESKY
!
      IF ( Row/=Dbl(2)+1 ) THEN
         kerr = 1320
         GOTO 4700
      ELSE
         IF ( Chlsky/=0 ) THEN
            IF ( rc==2 ) THEN
               kerr = 1300
               GOTO 4700
            ELSEIF ( prec==2 ) THEN
               IF ( Zd(1)<0.0D+0 ) GOTO 3000
               Zd(1) = dsqrt(Zd(1))
               IF ( C/=1 ) THEN
                  DO i = 2 , C
                     Zd(i) = Zd(i)*Zd(1)
                  ENDDO
               ENDIF
            ELSE
               IF ( Zr(1)<0. ) GOTO 3000
               Zr(1) = sqrt(Zr(1))
               IF ( C/=1 ) THEN
                  DO i = 2 , C
                     Zr(i) = Zr(i)*Zr(1)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
!
!     WRITE THE ROW WITH PUTSTR/ENDPUT
!
         CALL sdcout(Blk,0,Zi(Iac),C,Zr,Zr)
!
!     IF ACTIVE COLUMNS ARE NOW GOING PASSIVE, MERGE ROWS IN CORE
!     WITH THOSE NOW ON THE PC FILE THUS CREATING A NEW PC FILE
!
         IF ( Frstpc/=0 ) THEN
            IF ( splin .OR. splout ) THEN
               kerr = 1350
               GOTO 4700
            ELSE
               CALL gopen(scrc,Zi(buf4),Wrtrew)
               Blk(1) = scrc
               Blk(3) = 0
               ijkl = Iac + 1
               DO i = ijkl , nac
                  DO WHILE ( iabs(Zi(i))>Bblk(4) )
                     CALL cpystr(Bblk,Blk,1,0)
                     Bblk(8) = -1
                     CALL getstr(*3900,Bblk)
                  ENDDO
                  ci = nac - i + 1
                  CALL sdcout(Blk,0,Zi(i),ci,Zr(Wb),Zr(Wb))
                  Wb = Wb + ci*nwds
               ENDDO
               icrq = Wb - ispill
               IF ( Wb>ispill ) GOTO 4800
               DO
                  CALL cpystr(Bblk,Blk,1,0)
                  IF ( Bblk(4)==Nrow+1 ) THEN
                     CALL close(scrb,Rew)
                     CALL close(scrc,Rew)
                     i = scrb
                     scrb = scrc
                     scrc = i
                     CALL gopen(scrb,Zi(buf2),Rdrew)
                     Bblk(1) = scrb
                     Bblk(8) = -1
                     CALL getstr(*4100,Bblk)
                     Blk(1) = Dbl(1)
                     Blk(3) = 1
                     EXIT
                  ELSE
                     Bblk(8) = -1
                     CALL getstr(*4000,Bblk)
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
!
!     ACCUMULATE MCB INFORMATION FOR PIVOT ROW
!
         nwords = C*nwds
         Dbl(2) = Dbl(2) + 1
         Dbl(6) = max0(Dbl(6),nwords)
         Dbl(7) = Dbl(7) + nwords
      ENDIF
   ELSEIF ( .NOT.splout ) THEN
      kerr = 1310
      GOTO 4700
   ELSE
!
!     PIVOT ROW GOES TO SPILL FILE - SET INDEX WHERE TO BEGIN NEXT AND
!     WRITE ROW AND ACTIVE COLUMNN VECTOR
!
      ijkl = Spflg
      ii = Frstpc
      Spflg = 0
      Frstpc = 0
      Start = Lasti + 1
      CALL write(scrc,Key,nkey,0)
      CALL write(scrc,Zi(Iac),C,1)
      CALL write(scrc,Zr,C*nwds,1)
      IF ( Row<lstrow ) GOTO 2800
!
!     LAST ROW OF CURRENT SPILL GROUP - REWIND FILE AND OPEN IT TO READ.
!     IF ANOTHER SPILL GROUP, SET IT UP
!
      CALL close(scrc,Rew)
      jklm = scrc
      scrc = scrd
      scrd = jklm
      CALL gopen(scrd,Zi(buf5),Rdrew)
      lstspl = Row
      splin = .TRUE.
      splout = .FALSE.
      IF ( ijkl/=0 ) THEN
         splout = .TRUE.
         Sprow = Row
         S = ijkl
         lstrow = ii
         CALL gopen(scrc,Zi(buf4),Wrtrew)
!
!     IF S WAS REDEFINED, GET NEW DEFINITION
!
         DO i = kspill , nspill , 3
            IF ( Row<Zi(i) ) EXIT
            IF ( Row==Zi(i) ) GOTO 2650
         ENDDO
      ENDIF
      GOTO 2700
 2650 S = Zi(i+1)
      lstrow = Zi(i+2)
      kspill = i + 3
!
!     READ ANY TERMS SAVED FROM PREVIOUS SPILL GROUP
!
 2700 IF ( Row==Nrow ) GOTO 2900
      CALL fread(scrd,n,1,0)
      Wa = Nzzz - n
      CALL fread(scrd,Zr(Wa),n,1)
      rowone = .TRUE.
      GOTO 1800
   ENDIF
!
!     PREPARE TO PROCESS NEXT ROW.
!
 2800 IF ( Row/=Nrow ) THEN
      Prevc = C - 1
      rowone = .FALSE.
      Wa = Wb
      GOTO 1800
   ENDIF
!
!     CLOSE FILES AND PUT END MESSAGE IN RUN LOG.
!
 2900 subnam(3) = end
   CALL conmsg(subnam,5,0)
   CALL close(scra,Rew)
   CALL close(scrb,Rew)
   CALL close(Dbl,Rew)
!
!     PRINT ROOTS INFORMATION IF THIS IS EIGENVALUE PROBLEM, AND KEEP
!     TWO LARGEST SHIFT POINT DATA IF SEVERAL SHIFT POINT MOVINGS ARE
!     INVOLVED.
!
   IF ( Shftpt>0. ) WRITE (Nout,99004) Sturm , Shftpt
99004 FORMAT (20X,I5,13H ROOTS BELOW ,1P,E14.6)
   IF ( Sturm/=0 ) THEN
      IF ( Keep<=Sturm ) THEN
         jj = Keep
         rs = Ptshft
         Keep = Sturm
         Ptshft = Shftpt
         Sturm = jj
         Shftpt = rs
      ENDIF
   ELSEIF ( Keep>0 ) THEN
      Sturm = Keep
      Shftpt = Ptshft
   ENDIF
   IF ( Statfl/=1 ) RETURN
!
!     PREPARE AND PRINT STATISTICS REGARDING DECOMPOSITION
!
   IF ( 2*Nrow<buf2 ) THEN
!
      CALL gopen(scra,Zi(buf1),Rdrew)
      CALL gopen(Dbl,Zi(buf2),Rdrew)
      Ablk(1) = scra
      Bblk(1) = Dbl(1)
      Row = 1
      DO i = 1 , 6
         null(i) = 0
      ENDDO
      nn = 2*Nrow - 1
      epsmax = 0.
      n = 0
      DO j = 1 , nn , 2
         Ablk(8) = -1
         Bblk(8) = -1
         CALL fwdrec(*4600,Ablk)
         CALL getstr(*4200,Ablk)
         CALL getstr(*4300,Bblk)
         IF ( Ablk(4)/=Row ) GOTO 4400
         IF ( Bblk(4)/=Row ) GOTO 4500
         ii = Ablk(5)
         jj = Bblk(5)
         IF ( Typea==2 ) THEN
            save(2) = Xdns(ii)
            save(3) = Xdns(jj)
         ELSEIF ( Typea==3 ) THEN
            save(2) = sqrt(Xns(ii)**2+Xns(ii+1)**2)
            save(3) = sqrt(Xns(jj)**2+Xns(jj+1)**2)
         ELSEIF ( Typea==4 ) THEN
            save(2) = dsqrt(Xdns(ii)**2+Xdns(ii+1)**2)
            save(3) = dsqrt(Xdns(jj)**2+Xdns(jj+1)**2)
         ELSE
            save(2) = Xns(ii)
            save(3) = Xns(jj)
         ENDIF
         CALL fwdrec(*4600,Ablk)
         CALL fwdrec(*4600,Bblk)
         eps = abs(save(2)/save(3))
         Zi(j) = Row
         Zi(j+1) = eps
         IF ( save(3)<0. ) n = n + 1
         epsmax = amax1(epsmax,eps)
         Row = Row + 1
      ENDDO
      CALL sort(0,0,2,2,Zi,2*Nrow)
      CALL close(Ablk,Rew)
      CALL close(Bblk,Rew)
      save(1) = 0.1*epsmax
      DO i = 2 , 6
         save(i) = 0.1*save(i-1)
      ENDDO
      DO j = 1 , nn , 2
         IF ( Zr(j+1)>save(1) ) THEN
            null(1) = null(1) + 1
         ELSEIF ( Zr(j+1)>save(2) ) THEN
            null(2) = null(2) + 1
         ELSEIF ( Zr(j+1)>save(3) ) THEN
            null(3) = null(3) + 1
         ELSEIF ( Zr(j+1)>save(4) ) THEN
            null(4) = null(4) + 1
         ELSEIF ( Zr(j+1)>save(5) ) THEN
            null(5) = null(5) + 1
         ELSE
            null(6) = null(6) + 1
         ENDIF
      ENDDO
      i = max0(1,nn-8)
      CALL page2(6)
      WRITE (Nout,99005) Uim , dbname , n , epsmax , (null(j),j=1,6) , (Zi(j),j=i,nn,2)
99005 FORMAT (A29,' 2314. STATISTICS FOR SYMMETRIC DECOMPOSITION OF ','DATA BLOCK ',2A4,7H FOLLOW,/10X,23HNUMBER OF UII .LT. 0 = ,  &
            & I5,/10X,36HMAXIMUM ABSOLUTE VALUE OF AII/UII = ,1P,E12.5,/10X,13HN1 THRU N6 = ,6I6,/10X,                              &
             &36HROW NUMBERS OF 5 LARGEST  AII/UII = ,6I6)
      RETURN
   ELSE
      CALL page2(2)
      WRITE (Nout,99006) Uim
99006 FORMAT (A29,' 2316. INSUFFICIENT CORE TO PREPARE DECOMPOSITION ','STATISTICS.')
      RETURN
   ENDIF
!
!     DIAGONAL ELEMENT .LT. 0.0 IN CHOLESKY DECOMPOSITION
!
 3000 WRITE (Nout,99007) Ufm
99007 FORMAT (A23,' 3181, ATTEMPT TO PERFORM CHOLESKY DECOMPOSITION ON',' A NEGATIVE DEFINITE MATRIX IN SUBROUTINE SDCOMP.')
   GOTO 4900
!
!     DIAGONAL ELEMENT .EQ. 0.0
!
 3100 Zr(1) = rkhr
   IF ( Typea==2 ) Zd(1) = rkhr
   CALL page2(3)
   WRITE (Nout,99008) Uwm , Row , rkhr
99008 FORMAT (A25,' 2396, SDCOMP COMPUTED A ZERO ON THE DIAGONAL DURING',' DECOMPOSITION AT ROW NUMBER',I6,1H.,/5X,                 &
             &'USE OF DIAG 22 OUTPUT SHOULD PERMIT YOU TO CORRELATE THE',' ROW WITH A MODEL D.O.F.',/5X,'A VALUE OF ',E13.6,        &
             &' WILL BE USED IN PLACE OF THE ZERO, HOWEVER',/5X,' THE ACCURACY OF THE DECOMPOSITION MAY BE IN DOUBT.')
   GOTO khr
 3200 CALL close(scra,Rew)
   CALL close(scrb,Rew)
   CALL close(Dbl,Rew)
   CALL close(scrc,Rew)
   CALL close(scrd,Rew)
   RETURN 1
 3300 CALL pack(Zr,Dbl,Dbl)
   CALL close(Dbl,Rew)
   RETURN
!
!     VARIOUS ERRORS LAND HERE
!
 3400 kerr = 1045
   GOTO 4700
 3500 kerr = 1046
   GOTO 4700
 3600 kerr = 1204
   GOTO 4700
 3700 kerr = 660
   GOTO 4700
 3800 kerr = 1216
   GOTO 4700
 3900 kerr = 1370
   GOTO 4700
 4000 kerr = 1340
   GOTO 4700
 4100 kerr = 1420
   GOTO 4700
 4200 kerr = 1620
   GOTO 4700
 4300 kerr = 1630
   GOTO 4700
 4400 kerr = 1640
   GOTO 4700
 4500 kerr = 1650
   GOTO 4700
 4600 kerr = 1407
 4700 WRITE (Nout,99009) Sfm , kerr
99009 FORMAT (A25,' 3130, LOGIC ERROR',I6,' OCCURRED IN SDCOMP.')
   j = 66
   WRITE (Nout,99010) (Key(i),i=1,j)
99010 FORMAT (36H0   CONTENTS OF / SDCOMX / FOLLOW --,/(1X,10I12))
   GOTO 4900
 4800 ier = -8
   ifl = icrq
   GOTO 5000
 4900 ier = -37
   ifl = 0
 5000 CALL mesage(ier,ifl,subnam)
END SUBROUTINE sdcompx