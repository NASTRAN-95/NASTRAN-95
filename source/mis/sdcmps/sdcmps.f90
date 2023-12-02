!*==sdcmps.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcmps(Zi,Zr,Zd)
USE C_LHPWX
USE C_MACHIN
USE C_NAMES
USE C_NTIME
USE C_PACKX
USE C_SDCOMX
USE C_SDCQ
USE C_SFACT
USE C_SYSTEM
USE C_TYPE
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
         IF ( sys60/=0 .AND. Machx/=2 .AND. nbpw<=36 ) THEN
            korchg = sys60 - hicore
            IF ( korchg>0 ) THEN
               Lcore = Lcore + korchg
               WRITE (nout,99001) Uim , sys60
99001          FORMAT (A29,' - OPEN CORE FOR SDCOMP IS INCREASED TO',I8,' WORDS BY SYSTEM(60)',/)
            ENDIF
         ENDIF
         IF ( Lcore<=0 ) CALL mesage(-8,0,subnam)
!
!     BUFFER ALLOCATION
!
         buf1 = Lcore - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
         Buf6 = buf5 - sysbuf
!
!     INITIALIZATION AS A FUNCTION OF TYPE OF A MATRIX
!     RC   = 1 IF A IS REAL (2 IF A IS COMPLEX - ILLEGAL)
!     PREC = 1 IF A IS SINGLE, 2 IF A IS DOUBLE
!
         rc = Rlcmpx(typea)
         IF ( rc/=1 ) THEN
!
!     VARIOUS ERRORS LAND HERE
!
            CALL mesage(-7,Dba(2),subnam)
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ELSE
            statfl = iabs(Ksystm(57))
            Prec = Prc(typea)
            nwds = Words(typea)
            fnwds = nwds
!
!     CHECK INPUT PARAMETERS
!
            IF ( Dba(2)/=Dba(3) ) THEN
               CALL mesage(-7,Dba(2),subnam)
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ELSE
               icrq = nrow + 200 - Buf6
               IF ( icrq>0 ) THEN
                  spag_nextblock_1 = 28
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
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
               IF ( nrow==1 ) THEN
!
!     DECOMPOSE A 1X1 MATRIX
!
                  Itype1 = typea
                  Itype2 = typea
                  Itype3 = typea
                  Power = 0
                  I1 = 1
                  J1 = 1
                  I2 = 1
                  J2 = 1
                  Incr1 = 1
                  Incr2 = 1
                  CALL gopen(Dba,Zi(buf1),Rdrew)
                  Parm(2) = Dba(1)
                  CALL unpack(*160,Dba,Zr)
                  CALL close(Dba,Rew)
                  CALL gopen(Dbl,Zi(buf1),Wrtrew)
                  Dbl(2) = 0
                  Dbl(6) = 0
                  IF ( typea==2 ) THEN
                     Mindd = Zd(1)
                     Ddr = Zd(1)
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
                  CALL pack(Zr,Dbl,Dbl)
                  CALL close(Dbl,Rew)
                  spag_nextblock_1 = 29
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     GENERAL INITIALIZATION
!
                  loop = 1
                  ispill = Buf6 - max0(100,nrow/100)
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
         CALL fname(Dba,dbname)
         Power = 0
         spill = .FALSE.
         groups = 0
         cons = 2*ispill/nwds
         c5max = maxc(ispill)
         dsr = 1.0
         dsc = 0.
         minds = 1.E+25
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
         Ablk(2) = typea
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
         spag_nextblock_1 = 3
      CASE (3)
!
!     BEGIN A ROW BY LOCATING THE DIAGONAL ELEMENT
!
         Blk(8) = -1
!
!     ANY DETERMINES IF ANY STRINGS SKIPPED PRIOR TO DIAGONAL
!     AND -KK- ALLOWS STRING BEYOND ZERO DIAGONAL TO BE SAVED
!
         any = 0
         kr = krow
         DO
            CALL getstr(*20,Blk)
            IF ( Prec==2 ) jstr = 2*(jstr-1) + 1
            kk = nterms
            any = col
            IF ( col>Row ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            kk = 0
            IF ( col+nterms-1>=Row ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL endget(Blk)
         ENDDO
!
!     NULL COLUMN FOUND.  SAVE COLUMN ID AND SET NOGLEV
!
 20      kk = -1
         IF ( any/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( lstdia<Row ) CALL sdcmq(*100,1,0.,0.,0.D0,0.D0,Row,Zi)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( Blk(8)/=1 ) CALL fwdrec(*180,Blk)
         Row = Row + 1
         IF ( Row<=nrow ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         GOTO 100
      CASE (5)
!
!     ZERO DIAGONAL FOUND.  FILL CORE AND POINTERS
!
         col = Row
         Zi(kr) = col
         Zi(kr+1) = 1
         Zi(kr+2) = 0
         IF ( nwds==2 ) Zi(kr+3) = 0
         kr = kr + 2 + nwds
         nterms = nwds
         IF ( lstdia<Row ) THEN
            ddia = 0.0D0
            CALL sdcmq(*100,7,0.,0.,0.D0,0.D0,Row,Zi)
            IF ( Noglev>1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(Scrdia,rdia,nwds,eor)
            lstdia = Row
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
!     DIAGONAL TERM IS LOCATED -- COMPLETE ENTRIES IN THE FULL COLUMN
!     VECTOR AND SAVE THE TERMS FROM EACH STRING IN CORE
!
         jstr = jstr + (Row-col)*nwds
         IF ( lstdia<Row ) THEN
            rdia = Xns(jstr)
            IF ( Prec==2 ) ddrr(2) = Xns(jstr+1)
            IF ( Noglev<=1 ) CALL write(Scrdia,rdia,nwds,eor)
            lstdia = Row
         ENDIF
         nterms = nterms - (Row-col)
         col = Row
         spag_nextblock_1 = 7
      CASE (7)
         Zi(kr) = col
         Zi(kr+1) = nterms
         kr = kr + 2
         nstr = jstr + nterms*nwds - 1
         DO jj = jstr , nstr
            Zr(kr) = Xns(jj)
            kr = kr + 1
         ENDDO
         spag_nextblock_1 = 8
      CASE (8)
         n = col + nterms - 1
         DO j = col , n
            IF ( Zi(j)<0 ) THEN
               m = iabs(Zi(j))
               Zi(j) = Row
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
                     Zi(j) = Row
                     left = m - (j-i+1)
                     IF ( left>0 ) Zi(j+1) = -left
                     EXIT SPAG_Loop_2_1
                  ELSEIF ( Zi(i)/=0 ) THEN
                     spag_nextblock_1 = 26
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ELSE
               IF ( Zi(j)>Row .AND. Zi(j)<two24 ) Zi(j) = Zi(j) + two24 + two25
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
            IF ( Blk(8)/=1 ) CALL fwdrec(*180,Blk)
         ELSEIF ( kk==0 ) THEN
            CALL endget(Blk)
            CALL getstr(*40,Blk)
            IF ( Prec==2 ) jstr = 2*jstr - 1
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
            col = any
            nterms = kk
            kk = 0
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      Iac = kr
         i = Iac
         j = Row
         Lastpl = -1
         spag_nextblock_1 = 9
      CASE (9)
         IF ( Zi(j)<0 ) THEN
            j = j - Zi(j)
         ELSEIF ( Zi(j)==0 ) THEN
            kerr = 1051
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( Zi(j)<Row ) THEN
               Zi(i) = j
               i = i + 1
               j = j + 1
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Zi(j)/=Row ) THEN
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
            IF ( Lastpl<0 ) Lastpl = i - Iac
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
            IF ( C<=c5max ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Row<lstrow ) THEN
!
! *2* CURRENT ROW IS NEITHER FIRST NOR LAST IN CURRENT SPILL GROUP.
!     TEST FOR PASSIVE COL CONDITION. IF SO, TERMINATE SPILL GROUP.
!     TEST FOR POSSIBLE REDEFINITION OF SPILL GROUP. IF SO, TEST FOR
!     OVERFLOW OF REDEFINITION TABLE,  IF SO, TRY A DIFFERENT STRATEGY
!     FOR DEFINING S AND REDO PREFACE UP TO A LIMIT OF 3 TIMES.
!
            IF ( iabs(Zi(Iac+1))-Row<clos ) THEN
               ASSIGN 80 TO iswtch
               IF ( C<=Zi(Sprow) ) GOTO 80
               jj = nac
               DO WHILE ( iabs(Zi(jj))>lstrow )
                  jj = jj - 1
               ENDDO
               Sc = jj - Iac
               m = sx(fc)
               IF ( Sc<=m ) GOTO 80
               IF ( nspill+2>=Buf6 ) THEN
                  spag_nextblock_1 = 12
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               S = m
               ijkl = max0(Iac,jj-(Sc-m))
               lstrow = iabs(Zi(ijkl))
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSE
               ASSIGN 60 TO iswtch
               lstrow = Row
               spill = .FALSE.
               Start = 0
               IF ( nspill+2<Buf6 ) THEN
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
         Start = 0
         IF ( C>c5max ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spill = .FALSE.
         spag_nextblock_1 = 15
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         fcmax = amax1(fcmax,float(cmax))
         CALL close(scra,Rew)
         CALL close(Dba,Rew)
         loop = loop + 1
         IF ( loop<=3 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         icrq = Buf6 - nspill - 3
         spag_nextblock_1 = 28
         CYCLE SPAG_DispatchLoop_1
      CASE (13)
         IF ( Zi(nspill)/=0 .AND. Zi(nspill)/=Sprow ) nspill = nspill + 3
         Zi(nspill) = Sprow
         Zi(nspill+1) = S
         Zi(nspill+2) = lstrow
         IF ( Row<lstrow ) THEN
            GOTO iswtch
         ELSEIF ( Row==lstrow ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
            kerr = 1065
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (14)
         spill = .TRUE.
         Sprow = Row
         groups = groups + 1
         S = min0(sx(fc),nrow-Sprow)
         IF ( loop/=1 ) THEN
            jj = Iac + S - 1
            DO WHILE ( iabs(Zi(jj))>Sprow+S )
               jj = jj - 1
            ENDDO
            S = jj - Iac + 1
            IF ( loop==3 ) S = min0(S,sx(fcmax))
         ENDIF
         S = min0(S,nrow-Sprow)
         lstrow = iabs(Zi(Iac+S-1))
         Spflg = S
         Frstpc = lstrow
         savg = savg + S
         GOTO 80
      CASE (15)
!
!     TEST FOR CONDITION IN WHICH PASSIVE COLUMNS ARE CREATED
!
         col = iabs(Zi(Iac+1))
         IF ( Row-pcrow<clos .OR. C<clos/2 .OR. col-Row<clos ) GOTO 80
!
!     CREATE PASSIVE COLUMNS BY CHANGING THEIR FIRST
!     APPEARANCE IN THE FULL COLUMN VECTOR
!
 60      Frstpc = 2
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
               Zi(jj) = col
            ELSE
               Zi(jj) = min0(andf(Zi(jj),two24-1),col)
            ENDIF
         ENDDO
!
!     WRITE ACTIVE COLUMN VECTOR
!
 80      IF ( Noglev<=1 ) THEN
            CALL write(scra,key,nkey,0)
            CALL write(scra,Zi(Iac),C,1)
!
!     WRITE ROW OF INPUT MATRIX. -IAC- POINTS TO END OF OUTPUT
!
            Ablk(8) = -1
            Ablk(12) = Row
            kr = krow
            SPAG_Loop_1_3: DO
               Ablk(4) = Zi(kr)
               nbrstr = Zi(kr+1)
               kr = kr + 2
               SPAG_Loop_2_2: DO
                  CALL putstr(Ablk)
                  Ablk(7) = min0(Ablk(6),nbrstr)
                  jstr = Ablk(5)
                  IF ( Prec==2 ) jstr = 2*jstr - 1
                  nstr = jstr + Ablk(7)*nwds - 1
                  DO jj = jstr , nstr
                     Xns(jj) = Zr(kr)
                     kr = kr + 1
                  ENDDO
                  IF ( kr>=Iac ) THEN
                     Ablk(8) = 1
                     CALL endput(Ablk)
                     EXIT SPAG_Loop_2_2
                  ELSE
                     CALL endput(Ablk)
                     IF ( Ablk(7)==nbrstr ) CYCLE SPAG_Loop_1_3
                     Ablk(4) = Ablk(4) + Ablk(7)
                     nbrstr = nbrstr - Ablk(7)
                  ENDIF
               ENDDO SPAG_Loop_2_2
               EXIT SPAG_Loop_1_3
            ENDDO SPAG_Loop_1_3
         ENDIF
!
!     ACCUMULATE TIMING AND STATISTICS INFORMATION
!
         cavg = cavg + C
         csqr = csqr + C**2
         IF ( spill ) cspill = cspill + C**2
         Zi(Row) = C
         IF ( Row==nrow ) GOTO 100
         Row = Row + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
!
!     HERE WHEN ALL ROWS PROCESSED -- CLOSE FILES AND, IF SINGULAR
!     MATRIX, PRINT SINGULAR COLUMNS AND GIVE ALTERNATE RETURN
!
         Parm(1) = -8
         Parm(2) = icrq
         Noglev = 2
 100     CALL close(scra,Rew)
         CALL close(Dba,Rew)
         CALL close(Scrdia,Rew)
!
!     CALCULATE TIME ESTIMATE, PRINT USER INFORMATION AND
!     CHECK FOR SUFFICIENT TIME TO COMPLETE DECOMPOSITION
!
         IF ( groups/=0 ) savg = savg/groups
         savg = max0(savg,1)
         save(1) = 0.5*Tmt(typea)*csqr*1.0E-6
         save(2) = 0.5*(Tmpstr+Tmgstr)*float(pcsqr)*1.E-6
         save(3) = Tmpstr*float(cavg)*1.E-6
         save(4) = Tmio*(fnwds+1.0)*cspill/float(savg)*1.0E-6
         morcor = nbrwds(cmax) - ispill + 1
!
         cavg = cavg/nrow
         IF ( pcgrou/=0 ) pcavg = pcavg/pcgrou
         CALL tmtogo(ijkl)
         jklm = save(1) + save(2) + save(3) + save(4) + 1.0
!
         IF ( Dbc(1)>0 ) CALL page2(9)
         unadd = unuse
         IF ( morcor>0 ) unadd = addi
         IF ( Dbc(1)>0 ) WRITE (nout,99002) Uim , dbname , nrow , jklm , cavg , pcavg , groups , savg , unadd , morcor , cmax ,     &
                              & pcmax , pcgrou , loop
99002    FORMAT (A29,' 3023 - PARAMETERS FOR SYMMETRIC DECOMPOSITION OF ','DATA BLOCK ',2A4,6H ( N =,I5,2H ),/14X,                  &
                &17H  TIME ESTIMATE =,I7,17H          C AVG =,I6,17H         PC AVG =,I6,18H    SPILL GROUPS =,I6,                  &
                &17H          S AVG =,I6,/14X,A10,7H CORE =,I7,17H WORDS    C MAX =,I6,17H          PCMAX =,I6,                     &
               & 18H       PC GROUPS =,I6,17H  PREFACE LOOPS =,I6)
         IF ( morcor>0 ) WRITE (nout,99003)
99003    FORMAT (15X,'(FOR OPTIMIZED OPERATION)')
         IF ( Dbc(1)>0 ) WRITE (nout,99004) Uim , subnam(1) , subnam(2) , save
99004    FORMAT (A29,' 2378,',A4,A3,' ESTIMATE OF CPU TIME FOR MT =',1P,E10.3,/18X,'PASSIVE COL. = ',E10.3,14X,'ACTIVE COL. =',     &
               & E10.3,/25X,'SPILL = ',E10.3)
!
!     ESTIMATE FBS TIME AT ONE PASS, 1 LOAD
!
         save(1) = 2.0*float(nrow)*cavg*(Tmt(typea)+Tmpstr)*1.E-6
         IF ( Dbc(1)>0 ) WRITE (nout,99005) save(1)
99005    FORMAT (10X,41HESTIMATE FOR FBS, ONE PASS AND ONE LOAD =,1P,E10.3)
!
         IF ( jklm>=ijkl ) THEN
!
!     INSUFFICIENT TIME
!
            Parm(1) = -50
            Parm(2) = ijkl
            GOTO 340
         ELSE
            IF ( Noglev>1 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Ksystm(57)<0 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     WRITE A END-OF-MATRIX STRING ON THE PASSIVE COLUMN FILE
!
            CALL gopen(scrb,Zi(buf2),Wrtrew)
            Bblk(1) = scrb
            Bblk(2) = typea
            Bblk(3) = 0
            Bblk(8) = -1
            CALL putstr(Bblk)
            Bblk(4) = nrow + 1
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
            Zi(nspill) = nrow + 1
            splin = .FALSE.
            splout = .FALSE.
            spill = .FALSE.
            IF ( groups/=0 ) spill = .TRUE.
            Nzzz = orf(ispill-1,1)
            rowone = .FALSE.
            Dbl(2) = 0
            Dbl(6) = 0
            Dbl(7) = lshift(1,nbpw-2-(nbpw-32))
!
!     THIS 'NEXT TO SIGN' BIT WILL BE PICKED UP BY WRTTRL. ADD (NBPW-32)
!     SO THAT CRAY, WITH 48-BIT INTEGER, WILL NOT GET INTO TROUBLE
!
            Blk(1) = Dbl(1)
            Blk(2) = typea
            Blk(3) = 1
            Wa = Nzzz
            Wb = Wa
            Prevc = 0
            Bblk(8) = -1
            CALL getstr(*200,Bblk)
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
            CALL getstr(*320,Ablk)
            n = Iac - 1
            DO i = 1 , n
               Zr(i) = 0.
            ENDDO
            CALL sdcins(*300,Ablk,Zi(Iac),C,Zr,Zd)
            SPAG_Loop_1_4: DO
!
!     IF DEFINED, MERGE ROW FROM PASSIVE COLUMN FILE
!
               IF ( Row<Bblk(4) ) EXIT SPAG_Loop_1_4
               IF ( Row==Bblk(4) ) THEN
                  CALL sdcins(*300,Bblk,Zi(Iac),C,Zr,Zd)
                  Bblk(8) = -1
                  CALL getstr(*220,Bblk)
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
            IF ( Spflg==0 ) THEN
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            splout = .TRUE.
            CALL gopen(scrc,Zi(buf4),Wrtrew)
            Sprow = Row
            S = Spflg
            lstrow = Frstpc
            Frstpc = 0
!
!     IF S WAS REDEFINED, GET NEW DEFINITION
!
            SPAG_Loop_1_5: DO i = kspill , nspill , 3
               IF ( Row<Zi(i) ) THEN
               ELSEIF ( Row==Zi(i) ) THEN
                  GOTO 110
               ELSE
                  EXIT SPAG_Loop_1_5
               ENDIF
            ENDDO SPAG_Loop_1_5
            spag_nextblock_1 = 18
            CYCLE SPAG_DispatchLoop_1
 110        S = Zi(i+1)
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
                  SPAG_Loop_1_6: DO
                     j = j - 1
                     i = i - 1
                     Zr(j) = Zr(i)
                     IF ( i<=Wa ) THEN
                        Wa = j
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
            i = Iac
            l = Wa
            IF ( Prec==2 ) l = (Wa-1)/2 + 1
            IF ( typea==2 ) THEN
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
               IF ( Zd(1)==0.0D0 ) CALL sdcmq(*340,2,0,0,ddia,Zd(1),Row,Zi)
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
                  GOTO 115
               ENDIF
               CALL sdcmq(*340,i,0,0,ddia,Zd(1),Row,Zi)
!
 115           IF ( Diagck>=0 ) THEN
                  IF ( ddia==0.0D0 ) ddia = Zd(1)
                  IF ( ddia/=0.0D0 ) THEN
                     dv = dabs(Zd(1)/ddia)
                     IF ( dv>1.001D0 ) CALL sdcmq(*340,6,0,0,ddia,Zd(1),Row,Zi)
                     dv = dmant/dv
                     IF ( dv>pdefd ) CALL sdcmq(*340,4,0,0,ddia,Zd(1),Row,Zi)
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
               IF ( Zr(1)==0.0 ) CALL sdcmq(*340,2,rdia,Zr(1),0,0,Row,Zi)
               DO WHILE ( abs(dsr)>=10. )
                  dsr = dsr/10.
                  Power = Power + 1
               ENDDO
               DO WHILE ( abs(dsr)<=0.1 )
                  dsr = dsr*10.
                  Power = Power - 1
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
               CALL sdcmq(*340,i,rdia,Zr(1),0,0,Row,Zi)
!
 120           IF ( Diagck>=0 ) THEN
                  IF ( rdia==0.0 ) rdia = Zr(1)
                  IF ( rdia/=Zr(1) ) THEN
                     rv = abs(Zr(1)/rdia)
                     IF ( rv>1.001E0 ) CALL sdcmq(*340,6,rdia,Zr(1),0,0,Row,Zi)
                     rv = rmant/rv
                     IF ( rv>pdefr ) CALL sdcmq(*340,4,rdia,Zr(1),0,0,Row,Zi)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
!     CALCULATE WB
!
         Lasti = 1
         IF ( Start==0 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( splin ) THEN
            ci = C - (Start-2)
            Sc = ci
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
            ci = C
            Sc = lstrow - Sprow
            jj = min0(nac,Iac+Start+Sc-2)
         ELSE
            ci = C
            Sc = C
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO WHILE ( iabs(Zi(jj))>lstrow )
            jj = jj - 1
         ENDDO
         Sc = jj - Iac - Start + 2
         IF ( Sc<=0 ) THEN
            Sc = 0
            Wb = Wa
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
         nterms = Sc*(ci-1) - (Sc*(Sc-1))/2
         nwords = nterms*nwds
         Wb = Nzzz - nwords
         IF ( Prec==2 ) Wb = orf(Wb-1,1)
         IF ( Wb<Iac+C ) THEN
            kerr = 1288
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Wb>Wa+nwds*Prevc ) THEN
            kerr = 1289
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
         IF ( splin .AND. Row==lstspl ) splin = .FALSE.
         Lasti = min0(Start+Sc-1,C)
         IF ( Sc/=0 ) THEN
!
!     NOW CALCULATE CONTIBUTIONS FROM CURRENT PIVOT ROW TO
!     SECOND TERM IN EQUATION (4) IN MEMO CWM-19. NOTE-TERMS ARE
!     CALCULATED ONLY FOR ROW/COL COMBINATIONS IN THE CURRENT SPILL
!     GROUP
!
            IF ( typea==2 ) THEN
               CALL sdcom2(Zi,Zi(Iac),Zr(Wa+2*Prevc),Zr(Wb))
            ELSE
               CALL sdcom1(Zi,Zi(Iac),Zr(Wa+Prevc),Zr(Wb))
            ENDIF
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
!
!     SHIP PIVOT ROW OUT TO EITHER MATRIX OR SPILL FILE
!
         IF ( Lasti==C ) THEN
!
!     PIVOT ROW GOES TO OUTPUT FILE - IF REQUIRED, CONVERT TO CHOLESKY
!
            IF ( Row/=Dbl(2)+1 ) THEN
               kerr = 1320
               spag_nextblock_1 = 27
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( Chlsky/=0 ) THEN
                  IF ( Prec==2 ) THEN
                     IF ( Zd(1)<0.0D0 ) CALL sdcmq(*340,3,0,0,ddia,Zd(1),Row,Zi)
                     Zd(1) = dsqrt(Zd(1))
                     IF ( C/=1 ) THEN
                        DO i = 2 , C
                           Zd(i) = Zd(i)*Zd(1)
                        ENDDO
                     ENDIF
                  ELSE
                     IF ( Zr(1)<0. ) CALL sdcmq(*340,3,rdia,Zr(1),0,0,Row,Zi)
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
                     spag_nextblock_1 = 27
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     CALL gopen(scrc,Zi(buf4),Wrtrew)
                     Blk(1) = scrc
                     Blk(3) = 0
                     ijkl = Iac + 1
                     DO i = ijkl , nac
                        DO WHILE ( iabs(Zi(i))>Bblk(4) )
                           CALL cpystr(Bblk,Blk,1,0)
                           Bblk(8) = -1
                           CALL getstr(*240,Bblk)
                        ENDDO
                        ci = nac - i + 1
                        CALL sdcout(Blk,0,Zi(i),ci,Zr(Wb),Zr(Wb))
                        Wb = Wb + ci*nwds
                     ENDDO
                     icrq = Wb - ispill
                     IF ( Wb>ispill ) THEN
                        spag_nextblock_1 = 28
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     SPAG_Loop_1_7: DO
                        CALL cpystr(Bblk,Blk,1,0)
                        IF ( Bblk(4)==nrow+1 ) THEN
                           CALL close(scrb,Rew)
                           CALL close(scrc,Rew)
                           i = scrb
                           scrb = scrc
                           scrc = i
                           CALL gopen(scrb,Zi(buf2),Rdrew)
                           Bblk(1) = scrb
                           Bblk(8) = -1
                           CALL getstr(*280,Bblk)
                           Blk(1) = Dbl(1)
                           Blk(3) = 1
                           EXIT SPAG_Loop_1_7
                        ELSE
                           Bblk(8) = -1
                           CALL getstr(*260,Bblk)
                        ENDIF
                     ENDDO SPAG_Loop_1_7
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
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
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
            CALL write(scrc,key,nkey,0)
            CALL write(scrc,Zi(Iac),C,1)
            CALL write(scrc,Zr,C*nwds,1)
            IF ( Row<lstrow ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
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
               SPAG_Loop_1_8: DO i = kspill , nspill , 3
                  IF ( Row<Zi(i) ) THEN
                  ELSEIF ( Row==Zi(i) ) THEN
                     GOTO 130
                  ELSE
                     EXIT SPAG_Loop_1_8
                  ENDIF
               ENDDO SPAG_Loop_1_8
            ENDIF
            GOTO 140
 130        S = Zi(i+1)
            lstrow = Zi(i+2)
            kspill = i + 3
!
!     READ ANY TERMS SAVED FROM PREVIOUS SPILL GROUP
!
 140        IF ( Row==nrow ) THEN
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fread(scrd,n,1,0)
            Wa = Nzzz - n
            CALL fread(scrd,Zr(Wa),n,1)
            rowone = .TRUE.
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 23
      CASE (23)
!
!     PREPARE TO PROCESS NEXT ROW.
!
         IF ( Row/=nrow ) THEN
            Prevc = C - 1
            rowone = .FALSE.
            Wa = Wb
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
         CYCLE SPAG_DispatchLoop_1
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
         WRITE (nout,99006) Sfm , kerr
99006    FORMAT (A25,' 2379, LOGIC ERROR',I6,' IN SDCMPS.')
         j = 66
         WRITE (nout,99007) (key(i),i=1,j)
99007    FORMAT (36H0   CONTENTS OF / SDCOMX / FOLLOW --,/,(1X,10I12))
 300     Parm(1) = -37
         Parm(2) = 0
         Parm(3) = subnam(1)
         Parm(4) = subnam(2)
         GOTO 340
      CASE (28)
!
!     INSUFFICIENT CORE
!
         Parm(1) = -8
         Parm(2) = icrq
         GOTO 340
!
!     UNEXPECTED NULL COLUMN
!
 320     dv = 0.0
         CALL sdcmq(*340,5,rv,rv,dv,dv,Row,Zi)
!
 340     CALL close(Dba,Rew)
         CALL close(scra,Rew)
         CALL close(scrb,Rew)
         CALL close(Dbl,Rew)
         spag_nextblock_1 = 29
      CASE (29)
         CALL close(Scrdia,Rew)
         IF ( Nerr(1)+Nerr(2)>0 ) THEN
            CALL gopen(Scrmsg,Zi(Buf6),Wrt)
            Bblk(2) = 0
            Bblk(3) = 0
            Bblk(4) = 0
            CALL write(Scrmsg,Bblk(2),3,1)
            CALL close(Scrmsg,Rew)
         ENDIF
         IF ( korchg>0 ) Lcore = Lcore - korchg
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdcmps
