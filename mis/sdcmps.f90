
SUBROUTINE sdcmps(Zi,Zr,Zd)
   IMPLICIT NONE
   INTEGER Ablk(15) , Bblk(20) , Blk(15) , Buf6 , C , Chlsky , Col , Dba(7) , Dbc(7) , Dbl(7) , Diagck , Diaget , Frstpc , Hicore , &
         & I1 , I2 , Iac , Incr1 , Incr2 , Itype1 , Itype2 , Itype3 , J1 , J2 , Jstr , Key(1) , Ksystm(100) , Lasti , Lastpl ,      &
         & Lcore , Lhpw(6) , Machx , Mtisa , Nbpw , Nerr(2) , Nitems , Noglev , Nout , Nrow , Nterms , Nzzadr , Nzzz , Parm(4) ,    &
         & Pdefck , Power , Prc(2) , Prec , Prevc , Rlcmpx(4) , Row , S , Sc , Scr1 , Scr2 , Scr3 , Scrdia , Scrmsg , Spflg ,       &
         & Sprow , Start , Stscr , Sys60 , Sysbuf , Typea , Wa , Wb , Words(4)
   DOUBLE PRECISION Ddc , Ddr , Mindd
   REAL Dsc , Dsr , Minds , Rdnrw , Rdrew , Rew , Tmbpak , Tmgstr , Tmio , Tmipak , Tml(4) , Tmpak , Tmpstr , Tmt(4) , Tmupak ,     &
      & Wrt , Wrtrew , Xns(1)
   LOGICAL First , Opnscr
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /lhpwx / Lhpw , Mtisa
   COMMON /machin/ Machx
   COMMON /names / Rdnrw , Rdrew , Wrt , Wrtrew , Rew
   COMMON /ntime / Nitems , Tmio , Tmbpak , Tmipak , Tmpak , Tmupak , Tmgstr , Tmpstr , Tmt , Tml
   COMMON /packx / Itype1 , Itype2 , I1 , J1 , Incr1
   COMMON /sdcomx/ Row , C , Spflg , Start , Frstpc , Lastpl , Lasti , Sc , Iac , Nzzadr , Wa , Wb , Prevc , Nzzz , Sprow , S ,     &
                 & Blk , Ablk , Bblk
   COMMON /sdcq  / Nerr , Noglev , Buf6 , Scrmsg , Scrdia , Stscr , Pdefck , Diagck , Diaget , Prec , Parm , Opnscr , First
   COMMON /sfact / Dba , Dbl , Dbc , Scr1 , Scr2 , Lcore , Ddr , Ddc , Power , Scr3 , Mindd , Chlsky
   COMMON /system/ Ksystm
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /unpakx/ Itype3 , I2 , J2 , Incr2
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Xns
   DOUBLE PRECISION Zd(1)
   INTEGER Zi(1)
   REAL Zr(1)
   CHARACTER*10 addi , unadd , unuse
   INTEGER andf , lshift , orf
   INTEGER any , begn , buf1 , buf2 , buf3 , buf4 , buf5 , c5max , cavg , ci , clos , cmax , dbname(2) , end , eor , groups , i ,   &
         & icrq , ii , ijkl , ispill , iswtch , j , jj , jklm , kerr , kk , korchg , kr , krow , kspill , l , left , loop , lstdia ,&
         & lstrow , lstspl , m , morcor , n , nac , name , nbrstr , nkey , nspill , nstr , nwds , nwords , pcavg , pcgrou , pcmax , &
         & pcrow , pcsqr , rc , savg , scra , scrb , scrc , scrd , statfl , subnam(3) , two24 , two25
   REAL cons , cspill , csqr , ddrr(2) , fc , fcmax , fnwds , pdefr , rdia , rmant , rv , save(4) , x
   DOUBLE PRECISION ddia , dmant , dv , pdefd
   INTEGER maxc , nbrwds , sx
   LOGICAL rowone , spill , splin , splout
   EXTERNAL andf , lshift , orf
!
!     SDCMPS PERFORMS THE TRIANGULAR DECOMPOSITION OF A SYMMETRIC
!     MATRIX. THE REAL MATRIX INPUT MAY BE SINGLE OR DOUBLE PRECISION.
!     THE OUTPUT MATRICES HAVE POSITIVE DEFINATE CHECKS AND DIAGONAL
!     SINGULARITY CHECKS
!
!     IF SYSTEM(57) IS .GT.1 - USED FOR -CLOS-,
!                      .LT.0 - STOP AFTER PREPASS
!
   EQUIVALENCE (Nrow,Dba(3)) , (Typea,Dba(5)) , (Jstr,Blk(5)) , (Col,Blk(4)) , (Nterms,Blk(6)) , (Row,Key(1)) , (Dsr,Ddr) ,         &
    & (Dsc,Ddc) , (Minds,Mindd) , (ddrr(1),rdia) , (dv,rv) , (dmant,rmant) , (ddia,rdia) , (pdefd,pdefr)
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(31),Hicore) , (Ksystm(40),Nbpw) , (Ksystm(60),Sys60)
   DATA unuse , addi/'    UNUSED' , 'ADDITIONAL'/
   DATA subnam/4HSDCM , 2HPS , 1H / , nkey/6/ , begn/4HBEGN/ , end/4HEND / , two24/16777216/ , two25/33554432/
!
!     STATEMENT FUNCTIONS
!
   nbrwds(i) = i + nwds*(i*(i+1))/2
   sx(x) = x - sqrt(amax1(x*(x+4.0)-cons,0.0)) - 1.0
   maxc(j) = (sqrt(2.*fnwds*float(j))-3.0)/fnwds
!
!     VAX, UNIVAC, AND ALL WORKSTATIONS - OPEN CORE CAN BE INCREASED
!     LOCALLY FOR SDCOMP BY SYS60
!
   x = 1.0
   korchg = 0
   IF ( Sys60/=0 .AND. Machx/=2 .AND. Nbpw<=36 ) THEN
      korchg = Sys60 - Hicore
      IF ( korchg>0 ) THEN
         Lcore = Lcore + korchg
         WRITE (Nout,99001) Uim , Sys60
99001    FORMAT (A29,' - OPEN CORE FOR SDCOMP IS INCREASED TO',I8,' WORDS BY SYSTEM(60)',/)
      ENDIF
   ENDIF
   IF ( Lcore<=0 ) CALL mesage(-8,0,subnam)
!
!     BUFFER ALLOCATION
!
   buf1 = Lcore - Sysbuf
   buf2 = buf1 - Sysbuf
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   buf5 = buf4 - Sysbuf
   Buf6 = buf5 - Sysbuf
!
!     INITIALIZATION AS A FUNCTION OF TYPE OF A MATRIX
!     RC   = 1 IF A IS REAL (2 IF A IS COMPLEX - ILLEGAL)
!     PREC = 1 IF A IS SINGLE, 2 IF A IS DOUBLE
!
   rc = Rlcmpx(Typea)
   IF ( rc/=1 ) THEN
!
!     VARIOUS ERRORS LAND HERE
!
      CALL mesage(-7,Dba(2),subnam)
      GOTO 3300
   ELSE
      statfl = iabs(Ksystm(57))
      Prec = Prc(Typea)
      nwds = Words(Typea)
      fnwds = nwds
!
!     CHECK INPUT PARAMETERS
!
      IF ( Dba(2)/=Dba(3) ) THEN
         CALL mesage(-7,Dba(2),subnam)
         GOTO 3300
      ELSE
         icrq = Nrow + 200 - Buf6
         IF ( icrq>0 ) GOTO 4300
!
!     INITIALIZE POSITIVE DEFINATE CHECKS.  FILES SET IN DRIVER
!
         Parm(1) = 0
         Parm(3) = subnam(1)
         Parm(4) = subnam(2)
         Nerr(1) = 0
         Nerr(2) = 0
         IF ( Pdefck>=0 ) THEN
            i = -Diaget
            j = 1 - Mtisa
            IF ( Prec==2 ) THEN
               pdefd = 2.0D0**i
               dmant = 2.0D0**j
            ELSE
               pdefr = 2.0E0**i
               rmant = 2.0E0**j
            ENDIF
         ENDIF
!
!     STSCR IS STATUS OF -SCRDIA- FILE AT BUF6
!       0 = NOT OPEN
!       1 = READ
!       2 = WRITE
!
         Stscr = 2
         CALL gopen(Scrdia,Zi(Buf6),Wrtrew)
         scra = Scr3
         scrb = iabs(Dbc(1))
         Noglev = 0
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
            CALL gopen(Dba,Zi(buf1),Rdrew)
            Parm(2) = Dba(1)
            CALL unpack(*3200,Dba,Zr)
            CALL close(Dba,Rew)
            CALL gopen(Dbl,Zi(buf1),Wrtrew)
            Dbl(2) = 0
            Dbl(6) = 0
            IF ( Typea==2 ) THEN
               Mindd = Zd(1)
               Ddr = Zd(1)
               IF ( Zd(1)<0 ) THEN
!
                  i = 3
                  CALL sdcmq(*4500,i,Zr,Zr,Zd,Zd,1,Zi)
               ELSEIF ( Zd(1)==0 ) THEN
                  i = 2
                  CALL sdcmq(*4500,i,Zr,Zr,Zd,Zd,1,Zi)
               ENDIF
            ELSE
               Minds = Zr(1)
               Dsr = Zr(1)
               IF ( Zr(1)<0 ) THEN
                  i = 3
                  CALL sdcmq(*4500,i,Zr,Zr,Zd,Zd,1,Zi)
               ELSEIF ( Zr(1)==0 ) THEN
                  i = 2
                  CALL sdcmq(*4500,i,Zr,Zr,Zd,Zd,1,Zi)
               ENDIF
            ENDIF
            CALL pack(Zr,Dbl,Dbl)
            CALL close(Dbl,Rew)
            GOTO 4600
         ELSE
!
!     GENERAL INITIALIZATION
!
            loop = 1
            ispill = Buf6 - max0(100,Nrow/100)
            fcmax = 0.
         ENDIF
      ENDIF
   ENDIF
 100  ispill = ispill - (loop-1)*Nrow/100
   nspill = ispill
   krow = Nrow + 1
   icrq = (3-loop)*Nrow/100 - ispill
   IF ( ispill<=0 ) GOTO 4300
   Zi(ispill) = 0
   pcgrou = 0
   pcavg = 0
   pcsqr = 0
   pcmax = 0
   csqr = 0.0
   savg = 0
   clos = alog(float(Nrow)) + 5.0
   IF ( statfl>1 ) clos = statfl
   pcrow = -clos
   Zi(1) = -Nrow
   DO i = 2 , Nrow
      Zi(i) = 0
   ENDDO
   CALL fname(Dba,dbname)
   Power = 0
   spill = .FALSE.
   groups = 0
   cons = 2*ispill/nwds
   c5max = maxc(ispill)
   Dsr = 1.0
   Dsc = 0.
   Minds = 1.E+25
   IF ( Prec/=1 ) THEN
      Ddr = 1.0D0
      Ddc = 0.D0
      Mindd = 1.D+25
   ENDIF
   cavg = 0
   cmax = 0
   cspill = 0.0
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
   Row = 1
   jj = 0
   eor = 1
!
!     LSTDIA DETERMINES THE LAST DIAGONAL WRITTEN TO SCRATCH FILE
!
   lstdia = 0
!
!     BEGIN A ROW BY LOCATING THE DIAGONAL ELEMENT
!
 200  Blk(8) = -1
!
!     ANY DETERMINES IF ANY STRINGS SKIPPED PRIOR TO DIAGONAL
!     AND -KK- ALLOWS STRING BEYOND ZERO DIAGONAL TO BE SAVED
!
   any = 0
   kr = krow
   DO
      CALL getstr(*300,Blk)
      IF ( Prec==2 ) Jstr = 2*(Jstr-1) + 1
      kk = Nterms
      any = Col
      IF ( Col>Row ) GOTO 500
      kk = 0
      IF ( Col+Nterms-1>=Row ) GOTO 600
      CALL endget(Blk)
   ENDDO
!
!     NULL COLUMN FOUND.  SAVE COLUMN ID AND SET NOGLEV
!
 300  kk = -1
   IF ( any/=0 ) GOTO 500
   IF ( lstdia<Row ) CALL sdcmq(*2200,1,0.,0.,0.D0,0.D0,Row,Zi)
 400  IF ( Blk(8)/=1 ) CALL fwdrec(*3500,Blk)
   Row = Row + 1
   IF ( Row>Nrow ) GOTO 2200
   GOTO 200
!
!     ZERO DIAGONAL FOUND.  FILL CORE AND POINTERS
!
 500  Col = Row
   Zi(kr) = Col
   Zi(kr+1) = 1
   Zi(kr+2) = 0
   IF ( nwds==2 ) Zi(kr+3) = 0
   kr = kr + 2 + nwds
   Nterms = nwds
   IF ( lstdia<Row ) THEN
      ddia = 0.0D0
      CALL sdcmq(*2200,7,0.,0.,0.D0,0.D0,Row,Zi)
      IF ( Noglev>1 ) GOTO 400
      CALL write(Scrdia,rdia,nwds,eor)
      lstdia = Row
      GOTO 800
   ENDIF
!
!     DIAGONAL TERM IS LOCATED -- COMPLETE ENTRIES IN THE FULL COLUMN
!     VECTOR AND SAVE THE TERMS FROM EACH STRING IN CORE
!
 600  Jstr = Jstr + (Row-Col)*nwds
   IF ( lstdia<Row ) THEN
      rdia = Xns(Jstr)
      IF ( Prec==2 ) ddrr(2) = Xns(Jstr+1)
      IF ( Noglev<=1 ) CALL write(Scrdia,rdia,nwds,eor)
      lstdia = Row
   ENDIF
   Nterms = Nterms - (Row-Col)
   Col = Row
 700  Zi(kr) = Col
   Zi(kr+1) = Nterms
   kr = kr + 2
   nstr = Jstr + Nterms*nwds - 1
   DO jj = Jstr , nstr
      Zr(kr) = Xns(jj)
      kr = kr + 1
   ENDDO
 800  n = Col + Nterms - 1
   DO j = Col , n
      IF ( Zi(j)<0 ) THEN
         m = iabs(Zi(j))
         Zi(j) = Row
         IF ( m/=1 ) Zi(j+1) = -(m-1)
      ELSEIF ( Zi(j)==0 ) THEN
         i = j
         DO
            i = i - 1
            IF ( i<=0 ) GOTO 3300
            IF ( Zi(i)<0 ) THEN
               m = iabs(Zi(i))
               Zi(i) = -(j-i)
               Zi(j) = Row
               left = m - (j-i+1)
               IF ( left>0 ) Zi(j+1) = -left
               EXIT
            ELSEIF ( Zi(i)/=0 ) THEN
               GOTO 3400
            ENDIF
         ENDDO
      ELSE
         IF ( Zi(j)>Row .AND. Zi(j)<two24 ) Zi(j) = Zi(j) + two24 + two25
      ENDIF
   ENDDO
   icrq = kr - ispill
   IF ( kr>=ispill ) GOTO 2100
!
!     CHECK IF ZERO DIAGONAL WAS JUST PROCESSED
!
   IF ( kk<0 ) THEN
!
!     EXTRACT ACTIVE COLUMN VECTOR FROM THE FULL COLUMN VECTOR
!
      IF ( Blk(8)/=1 ) CALL fwdrec(*3500,Blk)
   ELSEIF ( kk==0 ) THEN
      CALL endget(Blk)
      CALL getstr(*900,Blk)
      IF ( Prec==2 ) Jstr = 2*Jstr - 1
      GOTO 700
   ELSE
      Col = any
      Nterms = kk
      kk = 0
      GOTO 600
   ENDIF
 900  Iac = kr
   i = Iac
   j = Row
   Lastpl = -1
 1000 IF ( Zi(j)<0 ) THEN
      j = j - Zi(j)
   ELSEIF ( Zi(j)==0 ) THEN
      kerr = 1051
      GOTO 4100
   ELSE
      IF ( Zi(j)<Row ) THEN
         Zi(i) = j
         i = i + 1
         j = j + 1
         GOTO 1100
      ELSEIF ( Zi(j)/=Row ) THEN
         IF ( Zi(j)<two24 ) THEN
            j = j + 1
            GOTO 1100
         ELSEIF ( Zi(j)<two25 ) THEN
            Zi(i) = j
            i = i + 1
            j = j + 1
            GOTO 1100
         ELSE
            Zi(j) = Zi(j) - two25
         ENDIF
      ENDIF
      Zi(i) = -j
      IF ( Lastpl<0 ) Lastpl = i - Iac
      i = i + 1
      j = j + 1
   ENDIF
 1100 IF ( j<=Nrow ) GOTO 1000
   icrq = i - ispill
   IF ( i>ispill ) GOTO 2100
   C = i - Iac
   cmax = max0(cmax,C)
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
! *1* CURRENT ROW IS NOT PART OF A SPILL GROUP. TEST FOR
!     CREATION OF A NEW SPILL GROUP
!
      IF ( C>c5max ) GOTO 1500
      GOTO 1600
   ELSEIF ( Row<lstrow ) THEN
!
! *2* CURRENT ROW IS NEITHER FIRST NOR LAST IN CURRENT SPILL GROUP.
!     TEST FOR PASSIVE COL CONDITION. IF SO, TERMINATE SPILL GROUP.
!     TEST FOR POSSIBLE REDEFINITION OF SPILL GROUP. IF SO, TEST FOR
!     OVERFLOW OF REDEFINITION TABLE,  IF SO, TRY A DIFFERENT STRATEGY
!     FOR DEFINING S AND REDO PREFACE UP TO A LIMIT OF 3 TIMES.
!
      IF ( iabs(Zi(Iac+1))-Row<clos ) THEN
         ASSIGN 1800 TO iswtch
         IF ( C<=Zi(Sprow) ) GOTO 1800
         jj = nac
         DO WHILE ( iabs(Zi(jj))>lstrow )
            jj = jj - 1
         ENDDO
         Sc = jj - Iac
         m = sx(fc)
         IF ( Sc<=m ) GOTO 1800
         IF ( nspill+2>=Buf6 ) GOTO 1300
         S = m
         ijkl = max0(Iac,jj-(Sc-m))
         lstrow = iabs(Zi(ijkl))
         GOTO 1400
      ELSE
         ASSIGN 1700 TO iswtch
         lstrow = Row
         spill = .FALSE.
         Start = 0
         IF ( nspill+2>=Buf6 ) GOTO 1300
         GOTO 1400
      ENDIF
   ENDIF
!
! *3* CURRENT ROW IS LAST ROW OF A SPILL GROUP. DETERMINE IF ANOTHER
!     SPILL GROUP FOLLOWS AND, IF SO, ITS RANGE
!
 1200 Start = 0
   IF ( C>c5max ) GOTO 1500
   spill = .FALSE.
   GOTO 1600
 1300 fcmax = amax1(fcmax,float(cmax))
   CALL close(scra,Rew)
   CALL close(Dba,Rew)
   loop = loop + 1
   IF ( loop<=3 ) GOTO 100
   icrq = Buf6 - nspill - 3
   GOTO 4300
 1400 IF ( Zi(nspill)/=0 .AND. Zi(nspill)/=Sprow ) nspill = nspill + 3
   Zi(nspill) = Sprow
   Zi(nspill+1) = S
   Zi(nspill+2) = lstrow
   IF ( Row<lstrow ) THEN
      GOTO iswtch
   ELSEIF ( Row==lstrow ) THEN
      GOTO 1200
   ELSE
      kerr = 1065
      GOTO 4100
   ENDIF
 1500 spill = .TRUE.
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
   GOTO 1800
!
!     TEST FOR CONDITION IN WHICH PASSIVE COLUMNS ARE CREATED
!
 1600 Col = iabs(Zi(Iac+1))
   IF ( Row-pcrow<clos .OR. C<clos/2 .OR. Col-Row<clos ) GOTO 1800
!
!     CREATE PASSIVE COLUMNS BY CHANGING THEIR FIRST
!     APPEARANCE IN THE FULL COLUMN VECTOR
!
 1700 Frstpc = 2
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
 1800 IF ( Noglev>1 ) GOTO 2000
   CALL write(scra,Key,nkey,0)
   CALL write(scra,Zi(Iac),C,1)
!
!     WRITE ROW OF INPUT MATRIX. -IAC- POINTS TO END OF OUTPUT
!
   Ablk(8) = -1
   Ablk(12) = Row
   kr = krow
 1900 Ablk(4) = Zi(kr)
   nbrstr = Zi(kr+1)
   kr = kr + 2
   DO
      CALL putstr(Ablk)
      Ablk(7) = min0(Ablk(6),nbrstr)
      Jstr = Ablk(5)
      IF ( Prec==2 ) Jstr = 2*Jstr - 1
      nstr = Jstr + Ablk(7)*nwds - 1
      DO jj = Jstr , nstr
         Xns(jj) = Zr(kr)
         kr = kr + 1
      ENDDO
      IF ( kr>=Iac ) THEN
         Ablk(8) = 1
         CALL endput(Ablk)
         EXIT
      ELSE
         CALL endput(Ablk)
         IF ( Ablk(7)==nbrstr ) GOTO 1900
         Ablk(4) = Ablk(4) + Ablk(7)
         nbrstr = nbrstr - Ablk(7)
      ENDIF
   ENDDO
!
!     ACCUMULATE TIMING AND STATISTICS INFORMATION
!
 2000 cavg = cavg + C
   csqr = csqr + C**2
   IF ( spill ) cspill = cspill + C**2
   Zi(Row) = C
   IF ( Row==Nrow ) GOTO 2200
   Row = Row + 1
   GOTO 200
!
!     HERE WHEN ALL ROWS PROCESSED -- CLOSE FILES AND, IF SINGULAR
!     MATRIX, PRINT SINGULAR COLUMNS AND GIVE ALTERNATE RETURN
!
 2100 Parm(1) = -8
   Parm(2) = icrq
   Noglev = 2
 2200 CALL close(scra,Rew)
   CALL close(Dba,Rew)
   CALL close(Scrdia,Rew)
!
!     CALCULATE TIME ESTIMATE, PRINT USER INFORMATION AND
!     CHECK FOR SUFFICIENT TIME TO COMPLETE DECOMPOSITION
!
   IF ( groups/=0 ) savg = savg/groups
   savg = max0(savg,1)
   save(1) = 0.5*Tmt(Typea)*csqr*1.0E-6
   save(2) = 0.5*(Tmpstr+Tmgstr)*float(pcsqr)*1.E-6
   save(3) = Tmpstr*float(cavg)*1.E-6
   save(4) = Tmio*(fnwds+1.0)*cspill/float(savg)*1.0E-6
   morcor = nbrwds(cmax) - ispill + 1
!
   cavg = cavg/Nrow
   IF ( pcgrou/=0 ) pcavg = pcavg/pcgrou
   CALL tmtogo(ijkl)
   jklm = save(1) + save(2) + save(3) + save(4) + 1.0
!
   IF ( Dbc(1)>0 ) CALL page2(9)
   unadd = unuse
   IF ( morcor>0 ) unadd = addi
   IF ( Dbc(1)>0 ) WRITE (Nout,99002) Uim , dbname , Nrow , jklm , cavg , pcavg , groups , savg , unadd , morcor , cmax , pcmax ,   &
                                    & pcgrou , loop
99002 FORMAT (A29,' 3023 - PARAMETERS FOR SYMMETRIC DECOMPOSITION OF ','DATA BLOCK ',2A4,6H ( N =,I5,2H ),/14X,17H  TIME ESTIMATE =,&
            & I7,17H          C AVG =,I6,17H         PC AVG =,I6,18H    SPILL GROUPS =,I6,17H          S AVG =,I6,/14X,A10,         &
            & 7H CORE =,I7,17H WORDS    C MAX =,I6,17H          PCMAX =,I6,18H       PC GROUPS =,I6,17H  PREFACE LOOPS =,I6)
   IF ( morcor>0 ) WRITE (Nout,99003)
99003 FORMAT (15X,'(FOR OPTIMIZED OPERATION)')
   IF ( Dbc(1)>0 ) WRITE (Nout,99004) Uim , subnam(1) , subnam(2) , save
99004 FORMAT (A29,' 2378,',A4,A3,' ESTIMATE OF CPU TIME FOR MT =',1P,E10.3,/18X,'PASSIVE COL. = ',E10.3,14X,'ACTIVE COL. =',E10.3,  &
            & /25X,'SPILL = ',E10.3)
!
!     ESTIMATE FBS TIME AT ONE PASS, 1 LOAD
!
   save(1) = 2.0*float(Nrow)*cavg*(Tmt(Typea)+Tmpstr)*1.E-6
   IF ( Dbc(1)>0 ) WRITE (Nout,99005) save(1)
99005 FORMAT (10X,41HESTIMATE FOR FBS, ONE PASS AND ONE LOAD =,1P,E10.3)
!
   IF ( jklm>=ijkl ) THEN
!
!     INSUFFICIENT TIME
!
      Parm(1) = -50
      Parm(2) = ijkl
      GOTO 4500
   ELSE
      IF ( Noglev>1 ) GOTO 4600
      IF ( Ksystm(57)<0 ) GOTO 4600
!
!     WRITE A END-OF-MATRIX STRING ON THE PASSIVE COLUMN FILE
!
      CALL gopen(scrb,Zi(buf2),Wrtrew)
      Bblk(1) = scrb
      Bblk(2) = Typea
      Bblk(3) = 0
      Bblk(8) = -1
      CALL putstr(Bblk)
      Bblk(4) = Nrow + 1
      Bblk(7) = 1
      Bblk(8) = 1
      CALL endput(Bblk)
      CALL close(scrb,Rew)
      subnam(3) = begn
      CALL conmsg(subnam,3,0)
!
!     THE STAGE IS SET AT LAST TO PERFORM THE DECOMPOSITION -
!     SO LETS GET THE SHOW UNDERWAY
!
      CALL gopen(scra,Zi(buf1),Rdrew)
      CALL gopen(scrb,Zi(buf2),Rdrew)
      CALL gopen(Dbl,Zi(buf3),Wrtrew)
      CALL gopen(Scrdia,Zi(Buf6),Rdrew)
      Stscr = 1
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
 2300 name = scra
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
      CALL fread(Scrdia,rdia,nwds,eor)
      Ablk(8) = -1
      CALL getstr(*4400,Ablk)
      n = Iac - 1
      DO i = 1 , n
         Zr(i) = 0.
      ENDDO
      CALL sdcins(*4200,Ablk,Zi(Iac),C,Zr,Zd)
      DO
!
!     IF DEFINED, MERGE ROW FROM PASSIVE COLUMN FILE
!
         IF ( Row<Bblk(4) ) EXIT
         IF ( Row==Bblk(4) ) THEN
            CALL sdcins(*4200,Bblk,Zi(Iac),C,Zr,Zd)
            Bblk(8) = -1
            CALL getstr(*3700,Bblk)
         ELSE
            kerr = 1215
            GOTO 4100
         ENDIF
      ENDDO
   ENDIF
!
!     IF 1ST ROW OF A NEW SPILL GROUP, OPEN SCRATCH FILE TO WRITE
!
   IF ( .NOT.(rowone) ) THEN
      IF ( splout ) GOTO 2500
      IF ( Spflg==0 ) GOTO 2500
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
         IF ( Row<Zi(i) ) THEN
         ELSEIF ( Row==Zi(i) ) THEN
            GOTO 2350
         ELSE
            EXIT
         ENDIF
      ENDDO
      GOTO 2400
 2350 S = Zi(i+1)
      lstrow = Zi(i+2)
      kspill = i + 3
   ENDIF
!
!     WRITE ANY TERMS ALREADY CALCULATED WHICH ARE
!     BEYOND THE RANGE OF THE CURRENT SPILL GROUP
!
 2400 IF ( splout ) THEN
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
         IF ( (Nzzz-Wa)==n ) THEN
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
 2500 IF ( .NOT.(splin) ) THEN
      i = Iac
      l = Wa
      IF ( Prec==2 ) l = (Wa-1)/2 + 1
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
!
!     CHECK DIAGONAL AND CORRECT
!
         IF ( Zd(1)==0.0D0 ) CALL sdcmq(*4500,2,0,0,ddia,Zd(1),Row,Zi)
         DO WHILE ( dabs(Ddr)>=10.0D0 )
            Ddr = Ddr/10.D0
            Power = Power + 1
         ENDDO
         DO WHILE ( dabs(Ddr)<=0.1D0 )
            Ddr = Ddr*10.D0
            Power = Power - 1
         ENDDO
         Ddr = Ddr*Zd(1)
         Mindd = dmin1(Zd(1),Mindd)
!
!     PERFORM MATRIX COND. CHECKS - D.P. REAL
!
         IF ( Zd(1)<0 ) THEN
            i = 3
         ELSEIF ( Zd(1)==0 ) THEN
            i = 2
         ELSE
            GOTO 2520
         ENDIF
         CALL sdcmq(*4500,i,0,0,ddia,Zd(1),Row,Zi)
!
 2520    IF ( Diagck>=0 ) THEN
            IF ( ddia==0.0D0 ) ddia = Zd(1)
            IF ( ddia/=0.0D0 ) THEN
               dv = dabs(Zd(1)/ddia)
               IF ( dv>1.001D0 ) CALL sdcmq(*4500,6,0,0,ddia,Zd(1),Row,Zi)
               dv = dmant/dv
               IF ( dv>pdefd ) CALL sdcmq(*4500,4,0,0,ddia,Zd(1),Row,Zi)
            ENDIF
         ENDIF
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
!
!     CHECK DIAGONAL AND CORRECT
!
         IF ( Zr(1)==0.0 ) CALL sdcmq(*4500,2,rdia,Zr(1),0,0,Row,Zi)
         DO WHILE ( abs(Dsr)>=10. )
            Dsr = Dsr/10.
            Power = Power + 1
         ENDDO
         DO WHILE ( abs(Dsr)<=0.1 )
            Dsr = Dsr*10.
            Power = Power - 1
         ENDDO
         Dsr = Dsr*Zr(1)
         Minds = amin1(Zr(1),Minds)
!
!     PERFORM MATRIX COND. CHECKS - S.P. REAL
!
         IF ( Zr(1)<0 ) THEN
            i = 3
         ELSEIF ( Zr(1)==0 ) THEN
            i = 2
         ELSE
            GOTO 2540
         ENDIF
         CALL sdcmq(*4500,i,rdia,Zr(1),0,0,Row,Zi)
!
 2540    IF ( Diagck>=0 ) THEN
            IF ( rdia==0.0 ) rdia = Zr(1)
            IF ( rdia/=Zr(1) ) THEN
               rv = abs(Zr(1)/rdia)
               IF ( rv>1.001E0 ) CALL sdcmq(*4500,6,rdia,Zr(1),0,0,Row,Zi)
               rv = rmant/rv
               IF ( rv>pdefr ) CALL sdcmq(*4500,4,rdia,Zr(1),0,0,Row,Zi)
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     CALCULATE WB
!
   Lasti = 1
   IF ( Start==0 ) GOTO 2800
   IF ( splin ) THEN
      ci = C - (Start-2)
      Sc = ci
      jj = nac
      IF ( .NOT.(splout) ) THEN
         IF ( ci<=c5max ) GOTO 2600
         kerr = 1288
         GOTO 4100
      ENDIF
   ELSEIF ( splout ) THEN
      ci = C
      Sc = lstrow - Sprow
      jj = min0(nac,Iac+Start+Sc-2)
   ELSE
      ci = C
      Sc = C
      GOTO 2600
   ENDIF
   DO WHILE ( iabs(Zi(jj))>lstrow )
      jj = jj - 1
   ENDDO
   Sc = jj - Iac - Start + 2
   IF ( Sc<=0 ) THEN
      Sc = 0
      Wb = Wa
      GOTO 2700
   ENDIF
 2600 Nterms = Sc*(ci-1) - (Sc*(Sc-1))/2
   nwords = Nterms*nwds
   Wb = Nzzz - nwords
   IF ( Prec==2 ) Wb = orf(Wb-1,1)
   IF ( Wb<Iac+C ) THEN
      kerr = 1288
      GOTO 4100
   ELSEIF ( Wb>Wa+nwds*Prevc ) THEN
      kerr = 1289
      GOTO 4100
   ENDIF
 2700 IF ( splin .AND. Row==lstspl ) splin = .FALSE.
   Lasti = min0(Start+Sc-1,C)
   IF ( Sc/=0 ) THEN
!
!     NOW CALCULATE CONTIBUTIONS FROM CURRENT PIVOT ROW TO
!     SECOND TERM IN EQUATION (4) IN MEMO CWM-19. NOTE-TERMS ARE
!     CALCULATED ONLY FOR ROW/COL COMBINATIONS IN THE CURRENT SPILL
!     GROUP
!
      IF ( Typea==2 ) THEN
         CALL sdcom2(Zi,Zi(Iac),Zr(Wa+2*Prevc),Zr(Wb))
      ELSE
         CALL sdcom1(Zi,Zi(Iac),Zr(Wa+Prevc),Zr(Wb))
      ENDIF
   ENDIF
!
!     SHIP PIVOT ROW OUT TO EITHER MATRIX OR SPILL FILE
!
 2800 IF ( Lasti==C ) THEN
!
!     PIVOT ROW GOES TO OUTPUT FILE - IF REQUIRED, CONVERT TO CHOLESKY
!
      IF ( Row/=Dbl(2)+1 ) THEN
         kerr = 1320
         GOTO 4100
      ELSE
         IF ( Chlsky/=0 ) THEN
            IF ( Prec==2 ) THEN
               IF ( Zd(1)<0.0D0 ) CALL sdcmq(*4500,3,0,0,ddia,Zd(1),Row,Zi)
               Zd(1) = dsqrt(Zd(1))
               IF ( C/=1 ) THEN
                  DO i = 2 , C
                     Zd(i) = Zd(i)*Zd(1)
                  ENDDO
               ENDIF
            ELSE
               IF ( Zr(1)<0. ) CALL sdcmq(*4500,3,rdia,Zr(1),0,0,Row,Zi)
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
               kerr = 1330
               GOTO 4100
            ELSE
               CALL gopen(scrc,Zi(buf4),Wrtrew)
               Blk(1) = scrc
               Blk(3) = 0
               ijkl = Iac + 1
               DO i = ijkl , nac
                  DO WHILE ( iabs(Zi(i))>Bblk(4) )
                     CALL cpystr(Bblk,Blk,1,0)
                     Bblk(8) = -1
                     CALL getstr(*3800,Bblk)
                  ENDDO
                  ci = nac - i + 1
                  CALL sdcout(Blk,0,Zi(i),ci,Zr(Wb),Zr(Wb))
                  Wb = Wb + ci*nwds
               ENDDO
               icrq = Wb - ispill
               IF ( Wb>ispill ) GOTO 4300
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
                     CALL getstr(*4000,Bblk)
                     Blk(1) = Dbl(1)
                     Blk(3) = 1
                     EXIT
                  ELSE
                     Bblk(8) = -1
                     CALL getstr(*3900,Bblk)
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
      kerr = 1311
      GOTO 4100
   ELSE
!
!     PIVOT ROW GOES TO SPILL FILE - SET INDEX WHERE TO BEGIN NEXT AND
!                                    WRITE ROW AND ACTIVE COLUMN VECTOR
!
      ijkl = Spflg
      ii = Frstpc
      Spflg = 0
      Frstpc = 0
      Start = Lasti + 1
      CALL write(scrc,Key,nkey,0)
      CALL write(scrc,Zi(Iac),C,1)
      CALL write(scrc,Zr,C*nwds,1)
      IF ( Row<lstrow ) GOTO 3000
!
!     LAST ROW OF CURRENT SPILL GROUP - REWIND FILE AND OPEN IT TO READ.
!                                      IF ANOTHER SPILL GROUP, SET IT UP
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
            IF ( Row<Zi(i) ) THEN
            ELSEIF ( Row==Zi(i) ) THEN
               GOTO 2850
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF
      GOTO 2900
 2850 S = Zi(i+1)
      lstrow = Zi(i+2)
      kspill = i + 3
!
!     READ ANY TERMS SAVED FROM PREVIOUS SPILL GROUP
!
 2900 IF ( Row==Nrow ) GOTO 3100
      CALL fread(scrd,n,1,0)
      Wa = Nzzz - n
      CALL fread(scrd,Zr(Wa),n,1)
      rowone = .TRUE.
      GOTO 2300
   ENDIF
!
!     PREPARE TO PROCESS NEXT ROW.
!
 3000 IF ( Row/=Nrow ) THEN
      Prevc = C - 1
      rowone = .FALSE.
      Wa = Wb
      GOTO 2300
   ENDIF
!
!     CLOSE FILES AND PUT END MESSAGE IN RUN LOG.
!
 3100 subnam(3) = end
   CALL conmsg(subnam,3,0)
   GOTO 4500
!
!     1X1 NULL COLUMN
!
 3200 CALL sdcmq(*4500,1,0.,0.,0.D0,0.D0,1,Zi)
   GOTO 4500
 3300 kerr = 1045
   GOTO 4100
 3400 kerr = 1046
   GOTO 4100
 3500 kerr = 1034
   GOTO 4100
 3600 kerr = 1204
   GOTO 4100
 3700 kerr = 1216
   GOTO 4100
 3800 kerr = 1333
   GOTO 4100
 3900 kerr = 1340
   GOTO 4100
 4000 kerr = 1344
 4100 WRITE (Nout,99006) Sfm , kerr
99006 FORMAT (A25,' 2379, LOGIC ERROR',I6,' IN SDCMPS.')
   j = 66
   WRITE (Nout,99007) (Key(i),i=1,j)
99007 FORMAT (36H0   CONTENTS OF / SDCOMX / FOLLOW --,/,(1X,10I12))
 4200 Parm(1) = -37
   Parm(2) = 0
   Parm(3) = subnam(1)
   Parm(4) = subnam(2)
   GOTO 4500
!
!     INSUFFICIENT CORE
!
 4300 Parm(1) = -8
   Parm(2) = icrq
   GOTO 4500
!
!     UNEXPECTED NULL COLUMN
!
 4400 dv = 0.0
   CALL sdcmq(*4500,5,rv,rv,dv,dv,Row,Zi)
!
 4500 CALL close(Dba,Rew)
   CALL close(scra,Rew)
   CALL close(scrb,Rew)
   CALL close(Dbl,Rew)
 4600 CALL close(Scrdia,Rew)
   IF ( Nerr(1)+Nerr(2)>0 ) THEN
      CALL gopen(Scrmsg,Zi(Buf6),Wrt)
      Bblk(2) = 0
      Bblk(3) = 0
      Bblk(4) = 0
      CALL write(Scrmsg,Bblk(2),3,1)
      CALL close(Scrmsg,Rew)
   ENDIF
   IF ( korchg>0 ) Lcore = Lcore - korchg
END SUBROUTINE sdcmps
