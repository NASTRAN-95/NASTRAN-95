!*==sdcompx.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcompx(*,Zi,Zr,Zd)
   USE c_names
   USE c_ntime
   USE c_packx
   USE c_sdcomx
   USE c_sfact
   USE c_sturmx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Zi
   REAL , DIMENSION(2) :: Zr
   REAL(REAL64) , DIMENSION(2) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(10) , SAVE :: addi , unuse
   INTEGER , SAVE :: begn , end , nkey , two24 , two25
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , c5max , cavg , ci , clos , cmax , col , config , forma , groups , i , icore ,      &
            & icrq , ier , ifl , ii , ijkl , ispill , iswtch , j , jj , jklm , jlist , jstr , kerr , khr , kk , kr , krow , kspill ,&
            & l , left , loop , lstrow , lstspl , m , morcor , n , nac , name , nbpw , nbrstr , nlist , nn , nout , nrow , nspill , &
            & nstr , nterms , nwds , nwords , pcavg , pcgrou , pcmax , pcrow , pcsqr , prec , rc , savg , scra , scrb , scrc ,      &
            & scrd , statfl , sysbuf , typea
   REAL :: cons , cspill , csqr , dens , drr , dsc , dsr , eps , epsmax , fc , fcmax , fnwds , minds , rkhr , rs , time , x
   INTEGER , DIMENSION(2) :: dbname , mtype
   REAL(REAL64) :: dsave3 , rd
   LOGICAL :: go , rowone , spill , splin , splout
   INTEGER , DIMENSION(2) , SAVE :: icmplx , ireal
   INTEGER , DIMENSION(1) :: key
   INTEGER :: maxc , nbrwds , sx
   INTEGER , DIMENSION(20) :: null
   REAL , DIMENSION(6) :: save
   INTEGER , DIMENSION(5) , SAVE :: subnam
   CHARACTER(10) :: unadd
   REAL(REAL64) , DIMENSION(1) :: xdns
   EXTERNAL andf , close , conmsg , cpystr , endget , endput , fname , fread , fwdrec , getstr , gopen , lshift , mesage , orf ,    &
          & pack , page2 , putstr , sdcin , sdcom1 , sdcom2 , sdcom3 , sdcom4 , sdcout , skprec , sort , tmtogo , unpack , write
!
! End of declarations rewritten by SPAG
!
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
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     BUFFER ALLOCATION
!
         subnam(3) = begn
         CALL conmsg(subnam,5,0)
         buf1 = lcore - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
         x = 1.0
         rkhr = 1.0E-10
!
!     INITIALIZATION AS A FUNCTION OF TYPE OF A MATRIX
!     RC   = 1 IF A IS REAL, 2 IF A IS COMPLEX
!     PREC = 1 IF A IS SINGLE, 2 IF A IS DOUBLE
!     NOTE - PRC(1) = 1, PRC(2) = 2, AND
!            PRC(3) = WORDS(1) = 1, PRC(4) = WORDS(2) = 2
!
         rc = rlcmpx(typea)
         mtype(1) = ireal(1)
         mtype(2) = ireal(2)
         IF ( rc/=1 ) THEN
            mtype(1) = icmplx(1)
            mtype(2) = icmplx(2)
         ENDIF
         prec = prc(typea)
         nwds = words(typea)
         fnwds = nwds
         sturm = 0
!
!     CHECK INPUT PARAMETERS
!
         IF ( dba(2)/=dba(3) ) THEN
!
!     ERROR EXITS
!
            ier = -7
            ifl = 0
            spag_nextblock_1 = 32
            CYCLE SPAG_DispatchLoop_1
         ELSE
            icrq = 100 - buf5
            IF ( buf5<100 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nrow==1 ) THEN
!
!     DECOMPOSE A 1X1 MATRIX
!
               itype1 = typea
               itype2 = typea
               itype3 = typea
               power = 0
               i1 = 1
               j1 = 1
               i2 = 1
               j2 = 1
               incr1 = 1
               incr2 = 1
               kk = 1
               null(1) = 1
               go = .FALSE.
               CALL gopen(dba,Zi(buf1),rdrew)
               CALL unpack(*100,dba,Zr)
               CALL close(dba,rew)
               CALL gopen(dbl,Zi(buf1),wrtrew)
               dbl(2) = 0
               dbl(6) = 0
               IF ( typea==2 ) THEN
                  mindd = Zd(1)
                  ddr = Zd(1)
                  IF ( Zd(1)/=0 ) THEN
                     spag_nextblock_1 = 24
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( typea==3 ) THEN
                  minds = sqrt(Zr(1)**2+Zr(2)**2)
                  dsr = Zr(1)
                  dsc = Zr(2)
                  IF ( minds/=0 ) THEN
                     spag_nextblock_1 = 24
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( typea==4 ) THEN
                  mindd = dsqrt(Zd(1)**2+Zd(2)**2)
                  ddr = Zd(1)
                  ddc = Zd(2)
                  IF ( mindd/=0 ) THEN
                     spag_nextblock_1 = 24
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSE
                  minds = Zr(1)
                  dsr = Zr(1)
                  IF ( Zr(1)/=0 ) THEN
                     spag_nextblock_1 = 24
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               GOTO 100
            ELSE
!
!     GENERAL INITIALIZATION
!
               loop = 1
               ispill = buf5 - max0(100,nrow/100)
               fcmax = 0.
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         ispill = ispill - (loop-1)*nrow/100
         nspill = ispill
         krow = nrow + 1
         icrq = -ispill
         IF ( ispill<=0 ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Zi(ispill) = 0
         pcgrou = 0
         pcavg = 0
         pcsqr = 0
         pcmax = 0
         csqr = 0.0
         savg = 0
         clos = alog(float(nrow)) + 5.0
         clos = 999999
         pcrow = -clos
         Zi(1) = -nrow
         icrq = nrow - buf5
         IF ( nrow>=buf5 ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 2 , nrow
            Zi(i) = 0
         ENDDO
         CALL fname(dba,dbname)
         power = 0
         scra = scr3
         scrb = iabs(dbc(1))
         go = .TRUE.
         spill = .FALSE.
         time = 0.
         groups = 0
         cmax = 0
         cons = 2*ispill/nwds
         c5max = maxc(ispill)
         dsr = 1.0
         dsc = 0.
         minds = 1.E+25
         IF ( prec/=1 ) THEN
            ddr = 1.0
            ddc = 0.D0
            mindd = 1.D+25
         ENDIF
         cavg = 0
         cspill = 0.
!
!     THE FOLLOWING CODE GENERATES THE ACTIVE COLUMN VECTOR FOR EACH
!     ROW, SPILL GROUPS AND TIMING AND USER INFORMATION ABOUT THE
!     DECOMPOSITION
!
         blk(1) = dba(1)
         ablk(1) = scra
         ablk(2) = typea
         ablk(3) = 0
         CALL gopen(dba,Zi(buf1),rdrew)
         CALL gopen(scra,Zi(buf2),wrtrew)
         jlist = 1
         row = 1
         jj = 0
         kk = 0
         nlist = 0
         spag_nextblock_1 = 3
      CASE (3)
!
!     BEGIN A ROW BY LOCATING THE DIAGONAL ELEMENT
!
         blk(8) = -1
         kr = krow
         SPAG_Loop_1_1: DO
            CALL getstr(*20,blk)
            IF ( prec==2 ) jstr = 2*(jstr-1) + 1
            IF ( col>row ) EXIT SPAG_Loop_1_1
            IF ( col+nterms-1>=row ) THEN
!
!     DIAGONAL TERM IS LOCATED - COMPLETE ENTRIES IN THE FULL COLUMN
!     VECTOR AND SAVE THE TERMS FROM EACH STRING IN CORE
!
               IF ( .NOT.go ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               jstr = jstr + (row-col)*nwds
               nterms = nterms - (row-col)
               col = row
               DO
                  Zi(kr) = col
                  Zi(kr+1) = nterms
                  kr = kr + 2
                  nstr = jstr + nterms*nwds - 1
                  DO jj = jstr , nstr
                     Zr(kr) = xns(jj)
                     kr = kr + 1
                  ENDDO
                  n = col + nterms - 1
                  DO j = col , n
                     IF ( Zi(j)<0 ) THEN
                        m = iabs(Zi(j))
                        Zi(j) = row
                        IF ( m/=1 ) Zi(j+1) = -(m-1)
                     ELSEIF ( Zi(j)==0 ) THEN
                        i = j
                        SPAG_Loop_4_2: DO
                           i = i - 1
                           IF ( i<=0 ) THEN
                              spag_nextblock_1 = 25
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( Zi(i)<0 ) THEN
                              m = iabs(Zi(i))
                              Zi(i) = -(j-i)
                              Zi(j) = row
                              left = m - (j-i+1)
                              IF ( left>0 ) Zi(j+1) = -left
                              EXIT SPAG_Loop_4_2
                           ELSEIF ( Zi(i)/=0 ) THEN
                              spag_nextblock_1 = 26
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDDO SPAG_Loop_4_2
                     ELSE
                        IF ( Zi(j)>row .AND. Zi(j)<two24 ) Zi(j) = Zi(j) + two24 + two25
                     ENDIF
                  ENDDO
                  icrq = kr - ispill
                  IF ( kr>=ispill ) THEN
                     spag_nextblock_1 = 30
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL endget(blk)
                  CALL getstr(*40,blk)
                  IF ( prec==2 ) jstr = 2*jstr - 1
               ENDDO
            ELSE
               CALL endget(blk)
            ENDIF
         ENDDO SPAG_Loop_1_1
 20      kk = kk + 1
         Zi(kk) = row
         go = .FALSE.
         spag_nextblock_1 = 4
      CASE (4)
         IF ( blk(8)/=1 ) CALL skprec(blk,1)
         row = row + 1
         IF ( row<=nrow ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 100
!
!     EXTRACT ACTIVE COLUMN VECTOR FROM THE FULL COLUMN VECTOR
!
 40      iac = kr
         i = iac
         j = row
         lastpl = -1
         spag_nextblock_1 = 5
      CASE (5)
         IF ( Zi(j)<0 ) THEN
            j = j - Zi(j)
         ELSEIF ( Zi(j)==0 ) THEN
            kerr = 1051
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( Zi(j)<row ) THEN
               Zi(i) = j
               i = i + 1
               j = j + 1
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Zi(j)/=row ) THEN
               IF ( Zi(j)<two24 ) THEN
                  j = j + 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Zi(j)<two25 ) THEN
                  Zi(i) = j
                  i = i + 1
                  j = j + 1
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  Zi(j) = Zi(j) - two25
               ENDIF
            ENDIF
            Zi(i) = -j
            IF ( lastpl<0 ) lastpl = i - iac
            i = i + 1
            j = j + 1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( j<=nrow ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icrq = i - ispill
         IF ( i>ispill ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         c = i - iac
         cmax = max0(cmax,c)
         c5max = maxc(ispill)
         nac = iac + c - 1
         IF ( lastpl<0 ) lastpl = c
!
!     MAKE SPILL CALCULATIONS
!
         spflg = 0
         fc = c
         start = 2
         IF ( c==1 ) start = 0
         frstpc = 0
         IF ( .NOT.spill ) THEN
!
!     *1* CURRENT ROW IS NOT PART OF A SPILL GROUP. TEST FOR CREATION OF
!         A NEW SPILL GROUP
!
            IF ( c<=c5max ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( row<lstrow ) THEN
!
!     *2* CURRENT ROW IS NEITHER FIRST NOR LAST IN CURRENT SPILL GROUP.
!         TEST FOR PASSIVE COL CONDITION. IF SO, TERMINATE SPILL GROUP.
!         TEST FOR POSSIBLE REDEFINITION OF SPILL GROUP. IF SO, TEST FOR
!         OVERFLOW OF REDEFINITION TABLE,  IF SO, TRY A DIFFERENT
!         STRATEGY FOR DEFINING S AND REDO PREFACE UP TO A LIMIT OF 3
!         TIMES.
!
            IF ( iabs(Zi(iac+1))-row<clos ) THEN
               ASSIGN 80 TO iswtch
               IF ( c<=Zi(sprow) ) GOTO 80
               jj = nac
               DO WHILE ( iabs(Zi(jj))>lstrow )
                  jj = jj - 1
               ENDDO
               sc = jj - iac
               m = sx(fc)
               IF ( sc<=m ) GOTO 80
               IF ( nspill+2>=buf5 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               s = m
               ijkl = max0(iac,jj-(sc-m))
               lstrow = iabs(Zi(ijkl))
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ASSIGN 60 TO iswtch
               lstrow = row
               spill = .FALSE.
               start = 0
               IF ( nspill+2<buf5 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     *3* CURRENT ROW IS LAST ROW OF A SPILL GROUP. DETERMINE IF ANOTHER
!         SPILL GROUP FOLLOWS AND, IF SO, ITS RANGE
!
         start = 0
         IF ( c>c5max ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spill = .FALSE.
         spag_nextblock_1 = 11
      CASE (8)
         fcmax = amax1(fcmax,float(cmax))
         CALL close(scra,rew)
         CALL close(dba,rew)
         loop = loop + 1
         IF ( loop<=3 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icrq = buf5 - nspill - 2
         spag_nextblock_1 = 30
      CASE (9)
         IF ( Zi(nspill)/=0 .AND. Zi(nspill)/=sprow ) nspill = nspill + 3
         Zi(nspill) = sprow
         Zi(nspill+1) = s
         Zi(nspill+2) = lstrow
         IF ( row<lstrow ) THEN
            GOTO iswtch
         ELSEIF ( row==lstrow ) THEN
            spag_nextblock_1 = 7
         ELSE
            kerr = 1065
            spag_nextblock_1 = 29
         ENDIF
      CASE (10)
         spill = .TRUE.
         sprow = row
         groups = groups + 1
         s = min0(sx(fc),nrow-sprow)
         IF ( loop/=1 ) THEN
            jj = iac + s - 1
            DO WHILE ( iabs(Zi(jj))>sprow+s )
               jj = jj - 1
            ENDDO
            s = jj - iac + 1
            IF ( loop==3 ) s = min0(s,sx(fcmax))
         ENDIF
         s = min0(s,nrow-sprow)
         lstrow = iabs(Zi(iac+s-1))
         spflg = s
         frstpc = lstrow
         savg = savg + s
         GOTO 80
      CASE (11)
!
!     TEST FOR CONDITION IN WHICH PASSIVE COLUMNS ARE CREATED
!
         col = iabs(Zi(iac+1))
         IF ( row-pcrow<clos .OR. c<clos/2 .OR. col-row<clos ) GOTO 80
!
!     CREATE PASSIVE COLUMNS BY CHANGING THEIR FIRST
!     APPEARANCE IN THE FULL COLUMN VECTOR
!
 60      frstpc = 2
         pcrow = row
         pcavg = pcavg + c - 1
         pcsqr = pcsqr + (c-1)**2
         pcmax = max0(pcmax,c-1)
         pcgrou = pcgrou + 1
         nac = iac + c - 1
         ijkl = iac + 1
         DO i = ijkl , nac
            jj = iabs(Zi(i))
            IF ( Zi(jj)<=row ) THEN
               Zi(jj) = col
            ELSE
               Zi(jj) = min0(andf(Zi(jj),two24-1),col)
            ENDIF
         ENDDO
!
!     WRITE ACTIVE COLUMN VECTOR
!
 80      CALL write(scra,key,nkey,0)
         CALL write(scra,Zi(iac),c,1)
!
!     WRITE ROW OF INPUT MATRIX
!
         ablk(8) = -1
         ablk(12) = row
         kr = krow
         SPAG_Loop_1_4: DO
            ablk(4) = Zi(kr)
            nbrstr = Zi(kr+1)
            kr = kr + 2
            SPAG_Loop_2_3: DO
               CALL putstr(ablk)
               ablk(7) = min0(ablk(6),nbrstr)
               jstr = ablk(5)
               IF ( prec==2 ) jstr = 2*jstr - 1
               nstr = jstr + ablk(7)*nwds - 1
               DO jj = jstr , nstr
                  xns(jj) = Zr(kr)
                  kr = kr + 1
               ENDDO
               IF ( kr>=iac ) THEN
                  ablk(8) = 1
                  CALL endput(ablk)
!
!     ACCUMULATE TIMING AND STATISTICS INFORMATION
!
                  cavg = cavg + c
                  csqr = csqr + c**2
                  IF ( spill ) cspill = cspill + c**2
                  Zi(row) = c
                  IF ( row==nrow ) EXIT SPAG_Loop_2_3
                  row = row + 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  CALL endput(ablk)
                  IF ( ablk(7)==nbrstr ) CYCLE SPAG_Loop_1_4
                  ablk(4) = ablk(4) + ablk(7)
                  nbrstr = nbrstr - ablk(7)
               ENDIF
            ENDDO SPAG_Loop_2_3
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
!
!     HERE WHEN ALL ROWS PROCESSED - CLOSE FILES AND, IF SINGULAR
!     MATRIX, PRINT SINGULAR COLUMNS AND GIVE ALTERNATE RETURN
!
 100     CALL close(scra,rew)
         CALL close(dba,rew)
         IF ( go ) THEN
!
!     CALCULATE TIME ESTIMATE, PRINT USER INFORMATION AND
!     CHECK FOR SUFFICIENT TIME TO COMPLETE DECOMPOSITION
!
            dens = float(dba(7))/10000.
            IF ( dens<0.01 ) dens = 0.01
            IF ( dens>99.99 ) dens = 99.99
            IF ( groups/=0 ) savg = savg/groups
            savg = max0(savg,1)
            time = 0.5*tmt(typea)*csqr + 0.5*(tmpstr+tmgstr)*float(pcsqr) + tmpstr*float(cavg) + tmio*(fnwds+1.0)*cspill/float(savg)
            morcor = nbrwds(cmax) - ispill + 1
!
            cavg = cavg/nrow
            IF ( pcgrou/=0 ) pcavg = pcavg/pcgrou
            CALL tmtogo(ijkl)
            jklm = 1.E-6*time + 1.0
            icore = iabs(morcor)
            IF ( dbc(1)>0 ) THEN
               unadd = unuse
               IF ( morcor>0 ) unadd = addi
               CALL page2(4)
               WRITE (nout,99001,ERR=120) uim , mtype , dbname , nrow , dens , jklm , cavg , pcavg , groups , savg , unadd , icore ,&
                    & cmax , pcmax , pcgrou , loop
99001          FORMAT (A29,' 3023 - PARAMETERS FOR ',2A4,' SYMMETRIC DECOMPOSITION OF DATA BLOCK ',2A4,5H (N =,I6,5H, D =,F6.2,2H%),&
                     & /14X,17H  TIME ESTIMATE =,I7,17H          C AVG =,I6,17H         PC AVG =,I6,18H    SPILL GROUPS =,I6,       &
                      &17H          S AVG =,I6,/14X,A10,7H CORE =,I9,15H WORDS  C MAX =,I6,17H          PCMAX =,I6,                 &
                      &18H       PC GROUPS =,I6,17H  PREFACE LOOPS =,I6)
               IF ( morcor>0 ) WRITE (nout,99002)
99002          FORMAT (14X,'(FOR OPTIMAL OPERATION)')
            ENDIF
         ELSE
            CALL close(dbl,rew)
            CALL page2(3)
            WRITE (nout,99003) ufm , dbname , (Zi(i),i=1,kk)
99003       FORMAT (A23,' 3097. SYMMETRIC DECOMPOSITION OF DATA BLOCK ',2A4,' ABORTED BECAUSE THE FOLLOWING COLUMNS ARE SINGULAR -',&
                  & /,(5X,20I6,/))
            RETURN 1
         ENDIF
 120     IF ( jklm>=ijkl ) THEN
            ier = -50
            ifl = jklm
            spag_nextblock_1 = 32
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     WRITE A END-OF-MATRIX STRING ON THE PASSIVE COLUMN FILE
!
            CALL gopen(scrb,Zi(buf2),wrtrew)
            bblk(1) = scrb
            bblk(2) = typea
            bblk(3) = 0
            bblk(8) = -1
            bblk(12) = 1
            CALL putstr(bblk)
            bblk(4) = nrow + 1
            bblk(7) = 1
            bblk(8) = 1
            CALL endput(bblk)
            CALL close(scrb,rew)
!
!     THE STAGE IS SET AT LAST TO PERFORM THE DECOMPOSITION -
!     SO LETS GET THE SHOW UNDERWAY
!
            CALL gopen(scra,Zi(buf1),rdrew)
            CALL gopen(scrb,Zi(buf2),rdrew)
            CALL gopen(dbl,Zi(buf3),wrtrew)
            scrc = scr1
            scrd = scr2
            IF ( Zi(nspill)/=0 ) nspill = nspill + 3
            Zi(nspill) = nrow + 1
            splin = .FALSE.
            splout = .FALSE.
            spill = .FALSE.
            IF ( groups/=0 ) spill = .TRUE.
            nzzz = orf(ispill-1,1)
            rowone = .FALSE.
            dbl(2) = 0
            dbl(6) = 0
            dbl(7) = lshift(1,nbpw-2-(nbpw-32))
!
!     THIS 'NEXT TO SIGN' BIT WILL BE PICKED UP BY WRTTRL. ADD (NBPW-32)
!     SO THAT CRAY, WITH 48-BIT INTEGER, WILL NOT GET INTO TROUBLE
!
            blk(1) = dbl(1)
            blk(2) = typea
            blk(3) = 1
            wa = nzzz
            wb = wa
            prevc = 0
            bblk(8) = -1
            CALL getstr(*200,bblk)
            kspill = ispill
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     READ KEY WORDS AND ACTIVE COLUMN VECTOR FOR CURRENT ROW
!
         name = scra
         IF ( splin ) name = scrd
         CALL fread(name,key,nkey,0)
         iac = c*nwds + 1
         CALL fread(name,Zi(iac),c,1)
         nac = iac + c - 1
         IF ( Zi(iac)<0 ) prevc = 0
         IF ( splin ) THEN
!
!     READ CURRENT PIVOT ROW FROM SPILL FILE. IF LAST ROW, CLOSE FILE
!
            prevc = 0
            CALL fread(scrd,Zr,c*nwds,1)
            IF ( row>=lstspl ) CALL close(scrd,rew)
         ELSE
!
!     READ TERMS FROM THE INPUT MATRIX
!
            ablk(8) = -1
            CALL getstr(*220,ablk)
            n = iac - 1
            DO i = 1 , n
               Zr(i) = 0.
            ENDDO
            CALL sdcin(ablk,Zi(iac),c,Zr,Zr)
            SPAG_Loop_1_5: DO
!
!     IF DEFINED, MERGE ROW FROM PASSIVE COLUMN FILE
!
               IF ( row<bblk(4) ) EXIT SPAG_Loop_1_5
               IF ( row==bblk(4) ) THEN
                  CALL sdcin(bblk,Zi(iac),c,Zr,Zr)
                  bblk(8) = -1
                  CALL getstr(*240,bblk)
               ELSE
                  kerr = 1215
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO SPAG_Loop_1_5
         ENDIF
!
!     IF 1ST ROW OF A NEW SPILL GROUP, OPEN SCRATCH FILE TO WRITE
!
         IF ( .NOT.(rowone) ) THEN
            IF ( splout ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( spflg==0 ) THEN
               spag_nextblock_1 = 14
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            splout = .TRUE.
            CALL gopen(scrc,Zi(buf4),wrtrew)
            sprow = row
            s = spflg
            lstrow = frstpc
            frstpc = 0
!
!     IF S WAS REDEFINED, GET NEW DEFINITION
!
            SPAG_Loop_1_6: DO i = kspill , nspill , 3
               IF ( row<Zi(i) ) EXIT SPAG_Loop_1_6
               IF ( row==Zi(i) ) GOTO 130
            ENDDO SPAG_Loop_1_6
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
 130        s = Zi(i+1)
            lstrow = Zi(i+2)
            kspill = i + 3
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
!
!     WRITE ANY TERMS ALREADY CALCULATED WHICH ARE
!     BEYOND THE RANGE OF THE CURRENT SPILL GROUP
!
         IF ( splout ) THEN
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
            CALL write(scrc,Zr(nzzz-n),n,1)
!
!     MOVE WA TO ACCOUNT FOR ANY TERMS JUST WRITTEN
!
            IF ( n/=0 ) THEN
               j = nzzz
               i = nzzz - n
               IF ( nzzz-wa==n ) THEN
                  wa = j
               ELSE
                  SPAG_Loop_1_7: DO
                     j = j - 1
                     i = i - 1
                     Zr(j) = Zr(i)
                     IF ( i<=wa ) THEN
                        wa = j
                        EXIT SPAG_Loop_1_7
                     ENDIF
                  ENDDO SPAG_Loop_1_7
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 14
      CASE (14)
!
!     IF THE PIVOTAL ROW DID NOT COME FROM THE SPILL FILE, IT IS CREATED
!
         IF ( splin ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = iac
         l = wa
         IF ( prec==2 ) l = (wa-1)/2 + 1
         IF ( typea==2 ) THEN
!
!     CREATE PIVOT ROW IN RDP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
            IF ( Zi(iac)>=0 ) THEN
               DO j = 1 , c
                  IF ( Zi(i)>=0 ) THEN
                     Zd(j) = Zd(j) + Zd(l)
                     l = l + 1
                  ENDIF
                  i = i + 1
               ENDDO
            ENDIF
            ASSIGN 160 TO khr
            IF ( Zd(1)/=0 ) GOTO 160
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( typea==3 ) THEN
!
!     CREATE PIVOT ROW IN CSP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
            IF ( Zi(iac)>=0 ) THEN
               ci = 2*c - 1
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
            IF ( save(3)==0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO WHILE ( sqrt(dsr**2+dsc**2)>=10. )
               dsr = dsr/10.
               dsc = dsc/10.
               power = power + 1
            ENDDO
            DO WHILE ( sqrt(dsr**2+dsc**2)<=0.1 )
               dsr = dsr*10.
               dsc = dsc*10.
               power = power - 1
            ENDDO
            rs = dsr*Zr(1) - dsc*Zr(2)
            dsc = dsr*Zr(2) + dsc*Zr(1)
            drr = rs
            minds = amin1(save(3),minds)
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( typea==4 ) THEN
!
!     CREATE PIVOT ROW IN CDP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
            IF ( Zi(iac)>=0 ) THEN
               ci = 2*c - 1
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
            IF ( dsave3==0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO WHILE ( dsqrt(ddr**2+ddc**2)>=10.D0 )
               ddr = ddr/10.D0
               ddc = ddc/10.D0
               power = power + 1
            ENDDO
            DO WHILE ( dsqrt(ddr**2+ddc**2)<=0.1D0 )
               ddr = ddr*10.D0
               ddc = ddc*10.D0
               power = power - 1
            ENDDO
            rd = ddr*Zd(1) - ddc*Zd(2)
            ddc = ddr*Zd(2) + ddc*Zd(1)
            ddr = rd
            mindd = dmin1(dsave3,mindd)
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     CREATE PIVOT ROW IN RSP, ACCUMULATE DETERMINANT AND MIN DIAGONAL
!
            IF ( Zi(iac)>=0 ) THEN
               DO j = 1 , c
                  IF ( Zi(i)>=0 ) THEN
                     Zr(j) = Zr(j) + Zr(l)
                     l = l + 1
                  ENDIF
                  i = i + 1
               ENDDO
            ENDIF
            ASSIGN 140 TO khr
            IF ( Zr(1)==0 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
 140     DO WHILE ( abs(dsr)>=10. )
            dsr = dsr/10.
            power = power + 1
         ENDDO
         DO WHILE ( abs(dsr)<=0.1 )
            dsr = dsr*10.
            power = power - 1
         ENDDO
         dsr = dsr*Zr(1)
         minds = amin1(abs(Zr(1)),minds)
!
!     COUNTING SIGN CHANGES OF THE LEADING PRINCIPLE MINORS IN STURM
!     SEQ.
!
         IF ( Zr(1)<0. ) sturm = sturm + 1
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
 160     DO WHILE ( dabs(ddr)>=10.0D0 )
            ddr = ddr/10.D0
            power = power + 1
         ENDDO
         DO WHILE ( dabs(ddr)<=0.1D0 )
            ddr = ddr*10.D0
            power = power - 1
         ENDDO
         ddr = ddr*Zd(1)
         mindd = dmin1(dabs(Zd(1)),mindd)
!
!     COUNTING SIGN CHANGES (STURM SEQUENCE PROPERTY)
!
         IF ( Zd(1)<0.D0 ) sturm = sturm + 1
         spag_nextblock_1 = 15
      CASE (15)
!
!     CALCULATE WB
!
         lasti = 1
         IF ( start==0 ) THEN
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( splin ) THEN
            ci = c - (start-2)
            sc = ci
            jj = nac
            IF ( .NOT.(splout) ) THEN
               IF ( (ci*(ci+1)+2*c)*nwds/2+c<=nzzz ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               kerr = 1288
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( splout ) THEN
            ci = c
            sc = lstrow - sprow
            jj = min0(nac,iac+start+sc-2)
         ELSE
            ci = c
            sc = c
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO WHILE ( iabs(Zi(jj))>lstrow )
            jj = jj - 1
         ENDDO
         sc = jj - iac - start + 2
         IF ( sc<=0 ) THEN
            sc = 0
            wb = wa
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 16
      CASE (16)
         nterms = sc*(ci-1) - (sc*(sc-1))/2
         nwords = nterms*nwds
         wb = nzzz - nwords
         IF ( prec==2 ) wb = orf(wb-1,1)
         IF ( wb<iac+c ) THEN
            kerr = 1288
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( wb>wa+nwds*prevc ) THEN
            kerr = 1170
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
         IF ( splin .AND. row==lstspl ) splin = .FALSE.
         lasti = min0(start+sc-1,c)
         IF ( sc/=0 ) THEN
!
!     NOW CALCULATE CONTRIBUTIONS FROM CURRENT PIVOT ROW TO SECOND TERM
!     IN EQUATION (4) IN MEMO CWM-19. NOTE-TERMS ARE CALCULATED ONLY
!     FOR ROW/COL COMBINATIONS IN THE CURRENT SPILL GROUP
!
            IF ( typea==2 ) THEN
               CALL sdcom2(Zi,Zi(iac),Zr(wa+2*prevc),Zr(wb))
            ELSEIF ( typea==3 ) THEN
               CALL sdcom3(Zi,Zi(iac),Zr(wa+2*prevc),Zr(wb))
            ELSEIF ( typea==4 ) THEN
               CALL sdcom4(Zi,Zi(iac),Zr(wa+4*prevc),Zr(wb))
            ELSE
               CALL sdcom1(Zi,Zi(iac),Zr(wa+prevc),Zr(wb))
            ENDIF
         ENDIF
         spag_nextblock_1 = 18
      CASE (18)
!
!     SHIP PIVOT ROW OUT TO EITHER MATRIX OR SPILL FILE
!
         IF ( lasti==c ) THEN
!
!     PIVOT ROW GOES TO OUTPUT FILE - IF REQUIRED, CONVERT TO CHOLESKY
!
            IF ( row/=dbl(2)+1 ) THEN
               kerr = 1320
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( chlsky/=0 ) THEN
                  IF ( rc==2 ) THEN
                     kerr = 1300
                     spag_nextblock_1 = 29
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( prec==2 ) THEN
                     IF ( Zd(1)<0.0D+0 ) THEN
                        spag_nextblock_1 = 21
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Zd(1) = dsqrt(Zd(1))
                     IF ( c/=1 ) THEN
                        DO i = 2 , c
                           Zd(i) = Zd(i)*Zd(1)
                        ENDDO
                     ENDIF
                  ELSE
                     IF ( Zr(1)<0. ) THEN
                        spag_nextblock_1 = 21
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Zr(1) = sqrt(Zr(1))
                     IF ( c/=1 ) THEN
                        DO i = 2 , c
                           Zr(i) = Zr(i)*Zr(1)
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
!
!     WRITE THE ROW WITH PUTSTR/ENDPUT
!
               CALL sdcout(blk,0,Zi(iac),c,Zr,Zr)
!
!     IF ACTIVE COLUMNS ARE NOW GOING PASSIVE, MERGE ROWS IN CORE
!     WITH THOSE NOW ON THE PC FILE THUS CREATING A NEW PC FILE
!
               IF ( frstpc/=0 ) THEN
                  IF ( splin .OR. splout ) THEN
                     kerr = 1350
                     spag_nextblock_1 = 29
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     CALL gopen(scrc,Zi(buf4),wrtrew)
                     blk(1) = scrc
                     blk(3) = 0
                     ijkl = iac + 1
                     DO i = ijkl , nac
                        DO WHILE ( iabs(Zi(i))>bblk(4) )
                           CALL cpystr(bblk,blk,1,0)
                           bblk(8) = -1
                           CALL getstr(*260,bblk)
                        ENDDO
                        ci = nac - i + 1
                        CALL sdcout(blk,0,Zi(i),ci,Zr(wb),Zr(wb))
                        wb = wb + ci*nwds
                     ENDDO
                     icrq = wb - ispill
                     IF ( wb>ispill ) THEN
                        spag_nextblock_1 = 30
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     SPAG_Loop_1_8: DO
                        CALL cpystr(bblk,blk,1,0)
                        IF ( bblk(4)==nrow+1 ) THEN
                           CALL close(scrb,rew)
                           CALL close(scrc,rew)
                           i = scrb
                           scrb = scrc
                           scrc = i
                           CALL gopen(scrb,Zi(buf2),rdrew)
                           bblk(1) = scrb
                           bblk(8) = -1
                           CALL getstr(*300,bblk)
                           blk(1) = dbl(1)
                           blk(3) = 1
                           EXIT SPAG_Loop_1_8
                        ELSE
                           bblk(8) = -1
                           CALL getstr(*280,bblk)
                        ENDIF
                     ENDDO SPAG_Loop_1_8
                  ENDIF
               ENDIF
!
!     ACCUMULATE MCB INFORMATION FOR PIVOT ROW
!
               nwords = c*nwds
               dbl(2) = dbl(2) + 1
               dbl(6) = max0(dbl(6),nwords)
               dbl(7) = dbl(7) + nwords
            ENDIF
         ELSEIF ( .NOT.splout ) THEN
            kerr = 1310
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     PIVOT ROW GOES TO SPILL FILE - SET INDEX WHERE TO BEGIN NEXT AND
!     WRITE ROW AND ACTIVE COLUMNN VECTOR
!
            ijkl = spflg
            ii = frstpc
            spflg = 0
            frstpc = 0
            start = lasti + 1
            CALL write(scrc,key,nkey,0)
            CALL write(scrc,Zi(iac),c,1)
            CALL write(scrc,Zr,c*nwds,1)
            IF ( row<lstrow ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     LAST ROW OF CURRENT SPILL GROUP - REWIND FILE AND OPEN IT TO READ.
!     IF ANOTHER SPILL GROUP, SET IT UP
!
            CALL close(scrc,rew)
            jklm = scrc
            scrc = scrd
            scrd = jklm
            CALL gopen(scrd,Zi(buf5),rdrew)
            lstspl = row
            splin = .TRUE.
            splout = .FALSE.
            IF ( ijkl/=0 ) THEN
               splout = .TRUE.
               sprow = row
               s = ijkl
               lstrow = ii
               CALL gopen(scrc,Zi(buf4),wrtrew)
!
!     IF S WAS REDEFINED, GET NEW DEFINITION
!
               SPAG_Loop_1_9: DO i = kspill , nspill , 3
                  IF ( row<Zi(i) ) EXIT SPAG_Loop_1_9
                  IF ( row==Zi(i) ) GOTO 170
               ENDDO SPAG_Loop_1_9
            ENDIF
            GOTO 180
 170        s = Zi(i+1)
            lstrow = Zi(i+2)
            kspill = i + 3
!
!     READ ANY TERMS SAVED FROM PREVIOUS SPILL GROUP
!
 180        IF ( row==nrow ) THEN
               spag_nextblock_1 = 20
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fread(scrd,n,1,0)
            wa = nzzz - n
            CALL fread(scrd,Zr(wa),n,1)
            rowone = .TRUE.
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 19
      CASE (19)
!
!     PREPARE TO PROCESS NEXT ROW.
!
         IF ( row/=nrow ) THEN
            prevc = c - 1
            rowone = .FALSE.
            wa = wb
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
!
!     CLOSE FILES AND PUT END MESSAGE IN RUN LOG.
!
         subnam(3) = end
         CALL conmsg(subnam,5,0)
         CALL close(scra,rew)
         CALL close(scrb,rew)
         CALL close(dbl,rew)
!
!     PRINT ROOTS INFORMATION IF THIS IS EIGENVALUE PROBLEM, AND KEEP
!     TWO LARGEST SHIFT POINT DATA IF SEVERAL SHIFT POINT MOVINGS ARE
!     INVOLVED.
!
         IF ( shftpt>0. ) WRITE (nout,99004) sturm , shftpt
99004    FORMAT (20X,I5,13H ROOTS BELOW ,1P,E14.6)
         IF ( sturm/=0 ) THEN
            IF ( keep<=sturm ) THEN
               jj = keep
               rs = ptshft
               keep = sturm
               ptshft = shftpt
               sturm = jj
               shftpt = rs
            ENDIF
         ELSEIF ( keep>0 ) THEN
            sturm = keep
            shftpt = ptshft
         ENDIF
         IF ( statfl/=1 ) RETURN
!
!     PREPARE AND PRINT STATISTICS REGARDING DECOMPOSITION
!
         IF ( 2*nrow<buf2 ) THEN
!
            CALL gopen(scra,Zi(buf1),rdrew)
            CALL gopen(dbl,Zi(buf2),rdrew)
            ablk(1) = scra
            bblk(1) = dbl(1)
            row = 1
            DO i = 1 , 6
               null(i) = 0
            ENDDO
            nn = 2*nrow - 1
            epsmax = 0.
            n = 0
            DO j = 1 , nn , 2
               ablk(8) = -1
               bblk(8) = -1
               CALL fwdrec(*360,ablk)
               CALL getstr(*320,ablk)
               CALL getstr(*340,bblk)
               IF ( ablk(4)/=row ) THEN
                  spag_nextblock_1 = 27
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( bblk(4)/=row ) THEN
                  spag_nextblock_1 = 28
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ii = ablk(5)
               jj = bblk(5)
               IF ( typea==2 ) THEN
                  save(2) = xdns(ii)
                  save(3) = xdns(jj)
               ELSEIF ( typea==3 ) THEN
                  save(2) = sqrt(xns(ii)**2+xns(ii+1)**2)
                  save(3) = sqrt(xns(jj)**2+xns(jj+1)**2)
               ELSEIF ( typea==4 ) THEN
                  save(2) = dsqrt(xdns(ii)**2+xdns(ii+1)**2)
                  save(3) = dsqrt(xdns(jj)**2+xdns(jj+1)**2)
               ELSE
                  save(2) = xns(ii)
                  save(3) = xns(jj)
               ENDIF
               CALL fwdrec(*360,ablk)
               CALL fwdrec(*360,bblk)
               eps = abs(save(2)/save(3))
               Zi(j) = row
               Zi(j+1) = eps
               IF ( save(3)<0. ) n = n + 1
               epsmax = amax1(epsmax,eps)
               row = row + 1
            ENDDO
            CALL sort(0,0,2,2,Zi,2*nrow)
            CALL close(ablk,rew)
            CALL close(bblk,rew)
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
            WRITE (nout,99005) uim , dbname , n , epsmax , (null(j),j=1,6) , (Zi(j),j=i,nn,2)
99005       FORMAT (A29,' 2314. STATISTICS FOR SYMMETRIC DECOMPOSITION OF ','DATA BLOCK ',2A4,7H FOLLOW,/10X,                       &
                   &23HNUMBER OF UII .LT. 0 = ,I5,/10X,36HMAXIMUM ABSOLUTE VALUE OF AII/UII = ,1P,E12.5,/10X,13HN1 THRU N6 = ,6I6,  &
                  & /10X,36HROW NUMBERS OF 5 LARGEST  AII/UII = ,6I6)
            RETURN
         ELSE
            CALL page2(2)
            WRITE (nout,99006) uim
99006       FORMAT (A29,' 2316. INSUFFICIENT CORE TO PREPARE DECOMPOSITION ','STATISTICS.')
            RETURN
         ENDIF
      CASE (21)
!
!     DIAGONAL ELEMENT .LT. 0.0 IN CHOLESKY DECOMPOSITION
!
         WRITE (nout,99007) ufm
99007    FORMAT (A23,' 3181, ATTEMPT TO PERFORM CHOLESKY DECOMPOSITION ON',' A NEGATIVE DEFINITE MATRIX IN SUBROUTINE SDCOMP.')
         spag_nextblock_1 = 31
      CASE (22)
!
!     DIAGONAL ELEMENT .EQ. 0.0
!
         Zr(1) = rkhr
         IF ( typea==2 ) Zd(1) = rkhr
         CALL page2(3)
         WRITE (nout,99008) uwm , row , rkhr
99008    FORMAT (A25,' 2396, SDCOMP COMPUTED A ZERO ON THE DIAGONAL DURING',' DECOMPOSITION AT ROW NUMBER',I6,1H.,/5X,              &
                &'USE OF DIAG 22 OUTPUT SHOULD PERMIT YOU TO CORRELATE THE',' ROW WITH A MODEL D.O.F.',/5X,'A VALUE OF ',E13.6,     &
                &' WILL BE USED IN PLACE OF THE ZERO, HOWEVER',/5X,' THE ACCURACY OF THE DECOMPOSITION MAY BE IN DOUBT.')
         GOTO khr
      CASE (23)
         CALL close(scra,rew)
         CALL close(scrb,rew)
         CALL close(dbl,rew)
         CALL close(scrc,rew)
         CALL close(scrd,rew)
         RETURN 1
      CASE (24)
         CALL pack(Zr,dbl,dbl)
         CALL close(dbl,rew)
         RETURN
      CASE (25)
!
!     VARIOUS ERRORS LAND HERE
!
         kerr = 1045
         spag_nextblock_1 = 29
      CASE (26)
         kerr = 1046
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 200     kerr = 1204
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 220     kerr = 660
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 240     kerr = 1216
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 260     kerr = 1370
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 280     kerr = 1340
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 300     kerr = 1420
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 320     kerr = 1620
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 340     kerr = 1630
         spag_nextblock_1 = 29
      CASE (27)
         kerr = 1640
         spag_nextblock_1 = 29
      CASE (28)
         kerr = 1650
         spag_nextblock_1 = 29
         CYCLE SPAG_DispatchLoop_1
 360     kerr = 1407
         spag_nextblock_1 = 29
      CASE (29)
         WRITE (nout,99009) sfm , kerr
99009    FORMAT (A25,' 3130, LOGIC ERROR',I6,' OCCURRED IN SDCOMP.')
         j = 66
         WRITE (nout,99010) (key(i),i=1,j)
99010    FORMAT (36H0   CONTENTS OF / SDCOMX / FOLLOW --,/(1X,10I12))
         spag_nextblock_1 = 31
      CASE (30)
         ier = -8
         ifl = icrq
         spag_nextblock_1 = 32
      CASE (31)
         ier = -37
         ifl = 0
         spag_nextblock_1 = 32
      CASE (32)
         CALL mesage(ier,ifl,subnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdcompx
