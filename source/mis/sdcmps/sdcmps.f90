!*==sdcmps.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcmps(Zi,Zr,Zd)
   USE c_lhpwx
   USE c_machin
   USE c_names
   USE c_ntime
   USE c_packx
   USE c_sdcomx
   USE c_sdcq
   USE c_sfact
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
   REAL , DIMENSION(1) :: Zr
   REAL(REAL64) , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(10) , SAVE :: addi , unuse
   INTEGER :: any , buf1 , buf2 , buf3 , buf4 , buf5 , c5max , cavg , ci , clos , cmax , col , eor , groups , hicore , i , icrq ,   &
            & ii , ijkl , ispill , iswtch , j , jj , jklm , jstr , kerr , kk , korchg , kr , krow , kspill , l , left , loop ,      &
            & lstdia , lstrow , lstspl , m , morcor , n , nac , name , nbpw , nbrstr , nout , nrow , nspill , nstr , nterms , nwds ,&
            & nwords , pcavg , pcgrou , pcmax , pcrow , pcsqr , rc , savg , scra , scrb , scrc , scrd , statfl , sys60 , sysbuf ,   &
            & typea
   INTEGER , SAVE :: begn , end , nkey , two24 , two25
   REAL :: cons , cspill , csqr , dsc , dsr , fc , fcmax , fnwds , minds , pdefr , rdia , rmant , rv , x
   INTEGER , DIMENSION(2) :: dbname
   REAL(REAL64) :: ddia , dmant , dv , pdefd
   REAL , DIMENSION(2) :: ddrr
   INTEGER , DIMENSION(1) :: key
   INTEGER :: maxc , nbrwds , sx
   LOGICAL :: rowone , spill , splin , splout
   REAL , DIMENSION(4) :: save
   INTEGER , DIMENSION(3) , SAVE :: subnam
   CHARACTER(10) :: unadd
   EXTERNAL andf , close , conmsg , cpystr , endget , endput , fname , fread , fwdrec , getstr , gopen , lshift , mesage , orf ,    &
          & pack , page2 , putstr , sdcins , sdcmq , sdcom1 , sdcom2 , sdcout , tmtogo , unpack , write
!
! End of declarations rewritten by SPAG
!
!
!     SDCMPS PERFORMS THE TRIANGULAR DECOMPOSITION OF A SYMMETRIC
!     MATRIX. THE REAL MATRIX INPUT MAY BE SINGLE OR DOUBLE PRECISION.
!     THE OUTPUT MATRICES HAVE POSITIVE DEFINATE CHECKS AND DIAGONAL
!     SINGULARITY CHECKS
!
!     IF SYSTEM(57) IS .GT.1 - USED FOR -CLOS-,
!                      .LT.0 - STOP AFTER PREPASS
!
   !>>>>EQUIVALENCE (Nrow,Dba(3)) , (Typea,Dba(5)) , (Jstr,Blk(5)) , (Col,Blk(4)) , (Nterms,Blk(6)) , (Row,Key(1)) , (Dsr,Ddr) ,         &
!>>>>    & (Dsc,Ddc) , (Minds,Mindd) , (ddrr(1),rdia) , (dv,rv) , (dmant,rmant) , (ddia,rdia) , (pdefd,pdefr)
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(31),Hicore) , (Ksystm(40),Nbpw) , (Ksystm(60),Sys60)
   DATA unuse , addi/'    UNUSED' , 'ADDITIONAL'/
   DATA subnam/4HSDCM , 2HPS , 1H / , nkey/6/ , begn/4HBEGN/ , end/4HEND / , two24/16777216/ , two25/33554432/
!
!     STATEMENT FUNCTIONS
!
   nbrwds(i) = i + nwds*(i*(i+1))/2
   sx(x) = x - sqrt(amax1(x*(x+4.0)-cons,0.0)) - 1.0
   maxc(j) = (sqrt(2.*fnwds*float(j))-3.0)/fnwds
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     VAX, UNIVAC, AND ALL WORKSTATIONS - OPEN CORE CAN BE INCREASED
!     LOCALLY FOR SDCOMP BY SYS60
!
         x = 1.0
         korchg = 0
         IF ( sys60/=0 .AND. machx/=2 .AND. nbpw<=36 ) THEN
            korchg = sys60 - hicore
            IF ( korchg>0 ) THEN
               lcore = lcore + korchg
               WRITE (nout,99001) uim , sys60
99001          FORMAT (A29,' - OPEN CORE FOR SDCOMP IS INCREASED TO',I8,' WORDS BY SYSTEM(60)',/)
            ENDIF
         ENDIF
         IF ( lcore<=0 ) CALL mesage(-8,0,subnam)
!
!     BUFFER ALLOCATION
!
         buf1 = lcore - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
         buf6 = buf5 - sysbuf
!
!     INITIALIZATION AS A FUNCTION OF TYPE OF A MATRIX
!     RC   = 1 IF A IS REAL (2 IF A IS COMPLEX - ILLEGAL)
!     PREC = 1 IF A IS SINGLE, 2 IF A IS DOUBLE
!
         rc = rlcmpx(typea)
         IF ( rc/=1 ) THEN
!
!     VARIOUS ERRORS LAND HERE
!
            CALL mesage(-7,dba(2),subnam)
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ELSE
            statfl = iabs(ksystm(57))
            prec = prc(typea)
            nwds = words(typea)
            fnwds = nwds
!
!     CHECK INPUT PARAMETERS
!
            IF ( dba(2)/=dba(3) ) THEN
               CALL mesage(-7,dba(2),subnam)
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ELSE
               icrq = nrow + 200 - buf6
               IF ( icrq>0 ) THEN
                  spag_nextblock_1 = 28
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     INITIALIZE POSITIVE DEFINATE CHECKS.  FILES SET IN DRIVER
!
               parm(1) = 0
               parm(3) = subnam(1)
               parm(4) = subnam(2)
               nerr(1) = 0
               nerr(2) = 0
               IF ( pdefck>=0 ) THEN
                  i = -diaget
                  j = 1 - mtisa
                  IF ( prec==2 ) THEN
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
               stscr = 2
               CALL gopen(scrdia,Zi(buf6),wrtrew)
               scra = scr3
               scrb = iabs(dbc(1))
               noglev = 0
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
                  CALL gopen(dba,Zi(buf1),rdrew)
                  parm(2) = dba(1)
                  CALL unpack(*160,dba,Zr)
                  CALL close(dba,rew)
                  CALL gopen(dbl,Zi(buf1),wrtrew)
                  dbl(2) = 0
                  dbl(6) = 0
                  IF ( typea==2 ) THEN
                     mindd = Zd(1)
                     ddr = Zd(1)
                     IF ( Zd(1)<0 ) THEN
!
                        i = 3
                        CALL sdcmq(*340,i,Zr,Zr,Zd,Zd,1,Zi)
                     ELSEIF ( Zd(1)==0 ) THEN
                        i = 2
                        CALL sdcmq(*340,i,Zr,Zr,Zd,Zd,1,Zi)
                     ENDIF
                  ELSE
                     minds = Zr(1)
                     dsr = Zr(1)
                     IF ( Zr(1)<0 ) THEN
                        i = 3
                        CALL sdcmq(*340,i,Zr,Zr,Zd,Zd,1,Zi)
                     ELSEIF ( Zr(1)==0 ) THEN
                        i = 2
                        CALL sdcmq(*340,i,Zr,Zr,Zd,Zd,1,Zi)
                     ENDIF
                  ENDIF
                  CALL pack(Zr,dbl,dbl)
                  CALL close(dbl,rew)
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     GENERAL INITIALIZATION
!
                  loop = 1
                  ispill = buf6 - max0(100,nrow/100)
                  fcmax = 0.
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         ispill = ispill - (loop-1)*nrow/100
         nspill = ispill
         krow = nrow + 1
         icrq = (3-loop)*nrow/100 - ispill
         IF ( ispill<=0 ) THEN
            spag_nextblock_1 = 28
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
         IF ( statfl>1 ) clos = statfl
         pcrow = -clos
         Zi(1) = -nrow
         DO i = 2 , nrow
            Zi(i) = 0
         ENDDO
         CALL fname(dba,dbname)
         power = 0
         spill = .FALSE.
         groups = 0
         cons = 2*ispill/nwds
         c5max = maxc(ispill)
         dsr = 1.0
         dsc = 0.
         minds = 1.E+25
         IF ( prec/=1 ) THEN
            ddr = 1.0D0
            ddc = 0.D0
            mindd = 1.D+25
         ENDIF
         cavg = 0
         cmax = 0
         cspill = 0.0
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
         row = 1
         jj = 0
         eor = 1
!
!     LSTDIA DETERMINES THE LAST DIAGONAL WRITTEN TO SCRATCH FILE
!
         lstdia = 0
         spag_nextblock_1 = 3
      CASE (3)
!
!     BEGIN A ROW BY LOCATING THE DIAGONAL ELEMENT
!
         blk(8) = -1
!
!     ANY DETERMINES IF ANY STRINGS SKIPPED PRIOR TO DIAGONAL
!     AND -KK- ALLOWS STRING BEYOND ZERO DIAGONAL TO BE SAVED
!
         any = 0
         kr = krow
         DO
            CALL getstr(*20,blk)
            IF ( prec==2 ) jstr = 2*(jstr-1) + 1
            kk = nterms
            any = col
            IF ( col>row ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            kk = 0
            IF ( col+nterms-1>=row ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL endget(blk)
         ENDDO
!
!     NULL COLUMN FOUND.  SAVE COLUMN ID AND SET NOGLEV
!
 20      kk = -1
         IF ( any/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( lstdia<row ) CALL sdcmq(*100,1,0.,0.,0.D0,0.D0,row,Zi)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( blk(8)/=1 ) CALL fwdrec(*180,blk)
         row = row + 1
         IF ( row<=nrow ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 100
      CASE (5)
!
!     ZERO DIAGONAL FOUND.  FILL CORE AND POINTERS
!
         col = row
         Zi(kr) = col
         Zi(kr+1) = 1
         Zi(kr+2) = 0
         IF ( nwds==2 ) Zi(kr+3) = 0
         kr = kr + 2 + nwds
         nterms = nwds
         IF ( lstdia<row ) THEN
            ddia = 0.0D0
            CALL sdcmq(*100,7,0.,0.,0.D0,0.D0,row,Zi)
            IF ( noglev>1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(scrdia,rdia,nwds,eor)
            lstdia = row
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     DIAGONAL TERM IS LOCATED -- COMPLETE ENTRIES IN THE FULL COLUMN
!     VECTOR AND SAVE THE TERMS FROM EACH STRING IN CORE
!
         jstr = jstr + (row-col)*nwds
         IF ( lstdia<row ) THEN
            rdia = xns(jstr)
            IF ( prec==2 ) ddrr(2) = xns(jstr+1)
            IF ( noglev<=1 ) CALL write(scrdia,rdia,nwds,eor)
            lstdia = row
         ENDIF
         nterms = nterms - (row-col)
         col = row
         spag_nextblock_1 = 7
      CASE (7)
         Zi(kr) = col
         Zi(kr+1) = nterms
         kr = kr + 2
         nstr = jstr + nterms*nwds - 1
         DO jj = jstr , nstr
            Zr(kr) = xns(jj)
            kr = kr + 1
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
         n = col + nterms - 1
         DO j = col , n
            IF ( Zi(j)<0 ) THEN
               m = iabs(Zi(j))
               Zi(j) = row
               IF ( m/=1 ) Zi(j+1) = -(m-1)
            ELSEIF ( Zi(j)==0 ) THEN
               i = j
               SPAG_Loop_2_1: DO
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
                     EXIT SPAG_Loop_2_1
                  ELSEIF ( Zi(i)/=0 ) THEN
                     spag_nextblock_1 = 26
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ELSE
               IF ( Zi(j)>row .AND. Zi(j)<two24 ) Zi(j) = Zi(j) + two24 + two25
            ENDIF
         ENDDO
         icrq = kr - ispill
         IF ( kr>=ispill ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     CHECK IF ZERO DIAGONAL WAS JUST PROCESSED
!
         IF ( kk<0 ) THEN
!
!     EXTRACT ACTIVE COLUMN VECTOR FROM THE FULL COLUMN VECTOR
!
            IF ( blk(8)/=1 ) CALL fwdrec(*180,blk)
         ELSEIF ( kk==0 ) THEN
            CALL endget(blk)
            CALL getstr(*40,blk)
            IF ( prec==2 ) jstr = 2*jstr - 1
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            col = any
            nterms = kk
            kk = 0
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      iac = kr
         i = iac
         j = row
         lastpl = -1
         spag_nextblock_1 = 9
      CASE (9)
         IF ( Zi(j)<0 ) THEN
            j = j - Zi(j)
         ELSEIF ( Zi(j)==0 ) THEN
            kerr = 1051
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( Zi(j)<row ) THEN
               Zi(i) = j
               i = i + 1
               j = j + 1
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Zi(j)/=row ) THEN
               IF ( Zi(j)<two24 ) THEN
                  j = j + 1
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Zi(j)<two25 ) THEN
                  Zi(i) = j
                  i = i + 1
                  j = j + 1
                  spag_nextblock_1 = 10
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
         spag_nextblock_1 = 10
      CASE (10)
         IF ( j<=nrow ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icrq = i - ispill
         IF ( i>ispill ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         c = i - iac
         cmax = max0(cmax,c)
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
! *1* CURRENT ROW IS NOT PART OF A SPILL GROUP. TEST FOR
!     CREATION OF A NEW SPILL GROUP
!
            IF ( c<=c5max ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( row<lstrow ) THEN
!
! *2* CURRENT ROW IS NEITHER FIRST NOR LAST IN CURRENT SPILL GROUP.
!     TEST FOR PASSIVE COL CONDITION. IF SO, TERMINATE SPILL GROUP.
!     TEST FOR POSSIBLE REDEFINITION OF SPILL GROUP. IF SO, TEST FOR
!     OVERFLOW OF REDEFINITION TABLE,  IF SO, TRY A DIFFERENT STRATEGY
!     FOR DEFINING S AND REDO PREFACE UP TO A LIMIT OF 3 TIMES.
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
               IF ( nspill+2>=buf6 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               s = m
               ijkl = max0(iac,jj-(sc-m))
               lstrow = iabs(Zi(ijkl))
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ASSIGN 60 TO iswtch
               lstrow = row
               spill = .FALSE.
               start = 0
               IF ( nspill+2<buf6 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
!
! *3* CURRENT ROW IS LAST ROW OF A SPILL GROUP. DETERMINE IF ANOTHER
!     SPILL GROUP FOLLOWS AND, IF SO, ITS RANGE
!
         start = 0
         IF ( c>c5max ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spill = .FALSE.
         spag_nextblock_1 = 15
      CASE (12)
         fcmax = amax1(fcmax,float(cmax))
         CALL close(scra,rew)
         CALL close(dba,rew)
         loop = loop + 1
         IF ( loop<=3 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icrq = buf6 - nspill - 3
         spag_nextblock_1 = 28
      CASE (13)
         IF ( Zi(nspill)/=0 .AND. Zi(nspill)/=sprow ) nspill = nspill + 3
         Zi(nspill) = sprow
         Zi(nspill+1) = s
         Zi(nspill+2) = lstrow
         IF ( row<lstrow ) THEN
            GOTO iswtch
         ELSEIF ( row==lstrow ) THEN
            spag_nextblock_1 = 11
         ELSE
            kerr = 1065
            spag_nextblock_1 = 27
         ENDIF
      CASE (14)
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
      CASE (15)
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
 80      IF ( noglev<=1 ) THEN
            CALL write(scra,key,nkey,0)
            CALL write(scra,Zi(iac),c,1)
!
!     WRITE ROW OF INPUT MATRIX. -IAC- POINTS TO END OF OUTPUT
!
            ablk(8) = -1
            ablk(12) = row
            kr = krow
            SPAG_Loop_1_3: DO
               ablk(4) = Zi(kr)
               nbrstr = Zi(kr+1)
               kr = kr + 2
               SPAG_Loop_2_2: DO
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
                     EXIT SPAG_Loop_2_2
                  ELSE
                     CALL endput(ablk)
                     IF ( ablk(7)==nbrstr ) CYCLE SPAG_Loop_1_3
                     ablk(4) = ablk(4) + ablk(7)
                     nbrstr = nbrstr - ablk(7)
                  ENDIF
               ENDDO SPAG_Loop_2_2
               EXIT SPAG_Loop_1_3
            ENDDO SPAG_Loop_1_3
         ENDIF
!
!     ACCUMULATE TIMING AND STATISTICS INFORMATION
!
         cavg = cavg + c
         csqr = csqr + c**2
         IF ( spill ) cspill = cspill + c**2
         Zi(row) = c
         IF ( row==nrow ) GOTO 100
         row = row + 1
         spag_nextblock_1 = 3
      CASE (16)
!
!     HERE WHEN ALL ROWS PROCESSED -- CLOSE FILES AND, IF SINGULAR
!     MATRIX, PRINT SINGULAR COLUMNS AND GIVE ALTERNATE RETURN
!
         parm(1) = -8
         parm(2) = icrq
         noglev = 2
 100     CALL close(scra,rew)
         CALL close(dba,rew)
         CALL close(scrdia,rew)
!
!     CALCULATE TIME ESTIMATE, PRINT USER INFORMATION AND
!     CHECK FOR SUFFICIENT TIME TO COMPLETE DECOMPOSITION
!
         IF ( groups/=0 ) savg = savg/groups
         savg = max0(savg,1)
         save(1) = 0.5*tmt(typea)*csqr*1.0E-6
         save(2) = 0.5*(tmpstr+tmgstr)*float(pcsqr)*1.E-6
         save(3) = tmpstr*float(cavg)*1.E-6
         save(4) = tmio*(fnwds+1.0)*cspill/float(savg)*1.0E-6
         morcor = nbrwds(cmax) - ispill + 1
!
         cavg = cavg/nrow
         IF ( pcgrou/=0 ) pcavg = pcavg/pcgrou
         CALL tmtogo(ijkl)
         jklm = save(1) + save(2) + save(3) + save(4) + 1.0
!
         IF ( dbc(1)>0 ) CALL page2(9)
         unadd = unuse
         IF ( morcor>0 ) unadd = addi
         IF ( dbc(1)>0 ) WRITE (nout,99002) uim , dbname , nrow , jklm , cavg , pcavg , groups , savg , unadd , morcor , cmax ,     &
                              & pcmax , pcgrou , loop
99002    FORMAT (A29,' 3023 - PARAMETERS FOR SYMMETRIC DECOMPOSITION OF ','DATA BLOCK ',2A4,6H ( N =,I5,2H ),/14X,                  &
                &17H  TIME ESTIMATE =,I7,17H          C AVG =,I6,17H         PC AVG =,I6,18H    SPILL GROUPS =,I6,                  &
                &17H          S AVG =,I6,/14X,A10,7H CORE =,I7,17H WORDS    C MAX =,I6,17H          PCMAX =,I6,                     &
               & 18H       PC GROUPS =,I6,17H  PREFACE LOOPS =,I6)
         IF ( morcor>0 ) WRITE (nout,99003)
99003    FORMAT (15X,'(FOR OPTIMIZED OPERATION)')
         IF ( dbc(1)>0 ) WRITE (nout,99004) uim , subnam(1) , subnam(2) , save
99004    FORMAT (A29,' 2378,',A4,A3,' ESTIMATE OF CPU TIME FOR MT =',1P,E10.3,/18X,'PASSIVE COL. = ',E10.3,14X,'ACTIVE COL. =',     &
               & E10.3,/25X,'SPILL = ',E10.3)
!
!     ESTIMATE FBS TIME AT ONE PASS, 1 LOAD
!
         save(1) = 2.0*float(nrow)*cavg*(tmt(typea)+tmpstr)*1.E-6
         IF ( dbc(1)>0 ) WRITE (nout,99005) save(1)
99005    FORMAT (10X,41HESTIMATE FOR FBS, ONE PASS AND ONE LOAD =,1P,E10.3)
!
         IF ( jklm>=ijkl ) THEN
!
!     INSUFFICIENT TIME
!
            parm(1) = -50
            parm(2) = ijkl
            GOTO 340
         ELSE
            IF ( noglev>1 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ksystm(57)<0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     WRITE A END-OF-MATRIX STRING ON THE PASSIVE COLUMN FILE
!
            CALL gopen(scrb,Zi(buf2),wrtrew)
            bblk(1) = scrb
            bblk(2) = typea
            bblk(3) = 0
            bblk(8) = -1
            CALL putstr(bblk)
            bblk(4) = nrow + 1
            bblk(7) = 1
            bblk(8) = 1
            CALL endput(bblk)
            CALL close(scrb,rew)
            subnam(3) = begn
            CALL conmsg(subnam,3,0)
!
!     THE STAGE IS SET AT LAST TO PERFORM THE DECOMPOSITION -
!     SO LETS GET THE SHOW UNDERWAY
!
            CALL gopen(scra,Zi(buf1),rdrew)
            CALL gopen(scrb,Zi(buf2),rdrew)
            CALL gopen(dbl,Zi(buf3),wrtrew)
            CALL gopen(scrdia,Zi(buf6),rdrew)
            stscr = 1
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
         spag_nextblock_1 = 17
      CASE (17)
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
            CALL fread(scrdia,rdia,nwds,eor)
            ablk(8) = -1
            CALL getstr(*320,ablk)
            n = iac - 1
            DO i = 1 , n
               Zr(i) = 0.
            ENDDO
            CALL sdcins(*300,ablk,Zi(iac),c,Zr,Zd)
            SPAG_Loop_1_4: DO
!
!     IF DEFINED, MERGE ROW FROM PASSIVE COLUMN FILE
!
               IF ( row<bblk(4) ) EXIT SPAG_Loop_1_4
               IF ( row==bblk(4) ) THEN
                  CALL sdcins(*300,bblk,Zi(iac),c,Zr,Zd)
                  bblk(8) = -1
                  CALL getstr(*220,bblk)
               ELSE
                  kerr = 1215
                  spag_nextblock_1 = 27
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO SPAG_Loop_1_4
         ENDIF
!
!     IF 1ST ROW OF A NEW SPILL GROUP, OPEN SCRATCH FILE TO WRITE
!
         IF ( .NOT.(rowone) ) THEN
            IF ( splout ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( spflg==0 ) THEN
               spag_nextblock_1 = 19
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
            SPAG_Loop_1_5: DO i = kspill , nspill , 3
               IF ( row<Zi(i) ) THEN
               ELSEIF ( row==Zi(i) ) THEN
                  GOTO 110
               ELSE
                  EXIT SPAG_Loop_1_5
               ENDIF
            ENDDO SPAG_Loop_1_5
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
 110        s = Zi(i+1)
            lstrow = Zi(i+2)
            kspill = i + 3
         ENDIF
         spag_nextblock_1 = 18
      CASE (18)
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
               IF ( (nzzz-wa)==n ) THEN
                  wa = j
               ELSE
                  SPAG_Loop_1_6: DO
                     j = j - 1
                     i = i - 1
                     Zr(j) = Zr(i)
                     IF ( i<=wa ) THEN
                        wa = j
                        EXIT SPAG_Loop_1_6
                     ENDIF
                  ENDDO SPAG_Loop_1_6
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 19
      CASE (19)
!
!     IF THE PIVOTAL ROW DID NOT COME FROM THE SPILL FILE, IT IS CREATED
!
         IF ( .NOT.(splin) ) THEN
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
!
!     CHECK DIAGONAL AND CORRECT
!
               IF ( Zd(1)==0.0D0 ) CALL sdcmq(*340,2,0,0,ddia,Zd(1),row,Zi)
               DO WHILE ( dabs(ddr)>=10.0D0 )
                  ddr = ddr/10.D0
                  power = power + 1
               ENDDO
               DO WHILE ( dabs(ddr)<=0.1D0 )
                  ddr = ddr*10.D0
                  power = power - 1
               ENDDO
               ddr = ddr*Zd(1)
               mindd = dmin1(Zd(1),mindd)
!
!     PERFORM MATRIX COND. CHECKS - D.P. REAL
!
               IF ( Zd(1)<0 ) THEN
                  i = 3
               ELSEIF ( Zd(1)==0 ) THEN
                  i = 2
               ELSE
                  GOTO 115
               ENDIF
               CALL sdcmq(*340,i,0,0,ddia,Zd(1),row,Zi)
!
 115           IF ( diagck>=0 ) THEN
                  IF ( ddia==0.0D0 ) ddia = Zd(1)
                  IF ( ddia/=0.0D0 ) THEN
                     dv = dabs(Zd(1)/ddia)
                     IF ( dv>1.001D0 ) CALL sdcmq(*340,6,0,0,ddia,Zd(1),row,Zi)
                     dv = dmant/dv
                     IF ( dv>pdefd ) CALL sdcmq(*340,4,0,0,ddia,Zd(1),row,Zi)
                  ENDIF
               ENDIF
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
!
!     CHECK DIAGONAL AND CORRECT
!
               IF ( Zr(1)==0.0 ) CALL sdcmq(*340,2,rdia,Zr(1),0,0,row,Zi)
               DO WHILE ( abs(dsr)>=10. )
                  dsr = dsr/10.
                  power = power + 1
               ENDDO
               DO WHILE ( abs(dsr)<=0.1 )
                  dsr = dsr*10.
                  power = power - 1
               ENDDO
               dsr = dsr*Zr(1)
               minds = amin1(Zr(1),minds)
!
!     PERFORM MATRIX COND. CHECKS - S.P. REAL
!
               IF ( Zr(1)<0 ) THEN
                  i = 3
               ELSEIF ( Zr(1)==0 ) THEN
                  i = 2
               ELSE
                  GOTO 120
               ENDIF
               CALL sdcmq(*340,i,rdia,Zr(1),0,0,row,Zi)
!
 120           IF ( diagck>=0 ) THEN
                  IF ( rdia==0.0 ) rdia = Zr(1)
                  IF ( rdia/=Zr(1) ) THEN
                     rv = abs(Zr(1)/rdia)
                     IF ( rv>1.001E0 ) CALL sdcmq(*340,6,rdia,Zr(1),0,0,row,Zi)
                     rv = rmant/rv
                     IF ( rv>pdefr ) CALL sdcmq(*340,4,rdia,Zr(1),0,0,row,Zi)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
!     CALCULATE WB
!
         lasti = 1
         IF ( start==0 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( splin ) THEN
            ci = c - (start-2)
            sc = ci
            jj = nac
            IF ( .NOT.(splout) ) THEN
               IF ( ci<=c5max ) THEN
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               kerr = 1288
               spag_nextblock_1 = 27
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( splout ) THEN
            ci = c
            sc = lstrow - sprow
            jj = min0(nac,iac+start+sc-2)
         ELSE
            ci = c
            sc = c
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO WHILE ( iabs(Zi(jj))>lstrow )
            jj = jj - 1
         ENDDO
         sc = jj - iac - start + 2
         IF ( sc<=0 ) THEN
            sc = 0
            wb = wa
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
         nterms = sc*(ci-1) - (sc*(sc-1))/2
         nwords = nterms*nwds
         wb = nzzz - nwords
         IF ( prec==2 ) wb = orf(wb-1,1)
         IF ( wb<iac+c ) THEN
            kerr = 1288
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( wb>wa+nwds*prevc ) THEN
            kerr = 1289
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
         IF ( splin .AND. row==lstspl ) splin = .FALSE.
         lasti = min0(start+sc-1,c)
         IF ( sc/=0 ) THEN
!
!     NOW CALCULATE CONTIBUTIONS FROM CURRENT PIVOT ROW TO
!     SECOND TERM IN EQUATION (4) IN MEMO CWM-19. NOTE-TERMS ARE
!     CALCULATED ONLY FOR ROW/COL COMBINATIONS IN THE CURRENT SPILL
!     GROUP
!
            IF ( typea==2 ) THEN
               CALL sdcom2(Zi,Zi(iac),Zr(wa+2*prevc),Zr(wb))
            ELSE
               CALL sdcom1(Zi,Zi(iac),Zr(wa+prevc),Zr(wb))
            ENDIF
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
!
!     SHIP PIVOT ROW OUT TO EITHER MATRIX OR SPILL FILE
!
         IF ( lasti==c ) THEN
!
!     PIVOT ROW GOES TO OUTPUT FILE - IF REQUIRED, CONVERT TO CHOLESKY
!
            IF ( row/=dbl(2)+1 ) THEN
               kerr = 1320
               spag_nextblock_1 = 27
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( chlsky/=0 ) THEN
                  IF ( prec==2 ) THEN
                     IF ( Zd(1)<0.0D0 ) CALL sdcmq(*340,3,0,0,ddia,Zd(1),row,Zi)
                     Zd(1) = dsqrt(Zd(1))
                     IF ( c/=1 ) THEN
                        DO i = 2 , c
                           Zd(i) = Zd(i)*Zd(1)
                        ENDDO
                     ENDIF
                  ELSE
                     IF ( Zr(1)<0. ) CALL sdcmq(*340,3,rdia,Zr(1),0,0,row,Zi)
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
                     kerr = 1330
                     spag_nextblock_1 = 27
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
                           CALL getstr(*240,bblk)
                        ENDDO
                        ci = nac - i + 1
                        CALL sdcout(blk,0,Zi(i),ci,Zr(wb),Zr(wb))
                        wb = wb + ci*nwds
                     ENDDO
                     icrq = wb - ispill
                     IF ( wb>ispill ) THEN
                        spag_nextblock_1 = 28
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     SPAG_Loop_1_7: DO
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
                           CALL getstr(*280,bblk)
                           blk(1) = dbl(1)
                           blk(3) = 1
                           EXIT SPAG_Loop_1_7
                        ELSE
                           bblk(8) = -1
                           CALL getstr(*260,bblk)
                        ENDIF
                     ENDDO SPAG_Loop_1_7
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
            kerr = 1311
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     PIVOT ROW GOES TO SPILL FILE - SET INDEX WHERE TO BEGIN NEXT AND
!                                    WRITE ROW AND ACTIVE COLUMN VECTOR
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
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     LAST ROW OF CURRENT SPILL GROUP - REWIND FILE AND OPEN IT TO READ.
!                                      IF ANOTHER SPILL GROUP, SET IT UP
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
               SPAG_Loop_1_8: DO i = kspill , nspill , 3
                  IF ( row<Zi(i) ) THEN
                  ELSEIF ( row==Zi(i) ) THEN
                     GOTO 130
                  ELSE
                     EXIT SPAG_Loop_1_8
                  ENDIF
               ENDDO SPAG_Loop_1_8
            ENDIF
            GOTO 140
 130        s = Zi(i+1)
            lstrow = Zi(i+2)
            kspill = i + 3
!
!     READ ANY TERMS SAVED FROM PREVIOUS SPILL GROUP
!
 140        IF ( row==nrow ) THEN
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fread(scrd,n,1,0)
            wa = nzzz - n
            CALL fread(scrd,Zr(wa),n,1)
            rowone = .TRUE.
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 23
      CASE (23)
!
!     PREPARE TO PROCESS NEXT ROW.
!
         IF ( row/=nrow ) THEN
            prevc = c - 1
            rowone = .FALSE.
            wa = wb
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 24
      CASE (24)
!
!     CLOSE FILES AND PUT END MESSAGE IN RUN LOG.
!
         subnam(3) = end
         CALL conmsg(subnam,3,0)
         GOTO 340
!
!     1X1 NULL COLUMN
!
 160     CALL sdcmq(*340,1,0.,0.,0.D0,0.D0,1,Zi)
         GOTO 340
      CASE (25)
         kerr = 1045
         spag_nextblock_1 = 27
      CASE (26)
         kerr = 1046
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 180     kerr = 1034
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 200     kerr = 1204
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 220     kerr = 1216
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 240     kerr = 1333
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 260     kerr = 1340
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
 280     kerr = 1344
         spag_nextblock_1 = 27
      CASE (27)
         WRITE (nout,99006) sfm , kerr
99006    FORMAT (A25,' 2379, LOGIC ERROR',I6,' IN SDCMPS.')
         j = 66
         WRITE (nout,99007) (key(i),i=1,j)
99007    FORMAT (36H0   CONTENTS OF / SDCOMX / FOLLOW --,/,(1X,10I12))
 300     parm(1) = -37
         parm(2) = 0
         parm(3) = subnam(1)
         parm(4) = subnam(2)
         GOTO 340
      CASE (28)
!
!     INSUFFICIENT CORE
!
         parm(1) = -8
         parm(2) = icrq
         GOTO 340
!
!     UNEXPECTED NULL COLUMN
!
 320     dv = 0.0
         CALL sdcmq(*340,5,rv,rv,dv,dv,row,Zi)
!
 340     CALL close(dba,rew)
         CALL close(scra,rew)
         CALL close(scrb,rew)
         CALL close(dbl,rew)
         spag_nextblock_1 = 29
      CASE (29)
         CALL close(scrdia,rew)
         IF ( nerr(1)+nerr(2)>0 ) THEN
            CALL gopen(scrmsg,Zi(buf6),wrt)
            bblk(2) = 0
            bblk(3) = 0
            bblk(4) = 0
            CALL write(scrmsg,bblk(2),3,1)
            CALL close(scrmsg,rew)
         ENDIF
         IF ( korchg>0 ) lcore = lcore - korchg
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdcmps
