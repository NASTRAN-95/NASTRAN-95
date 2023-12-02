!*==trlga.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trlga(Casecc,Usetd,Dlt,Slt,Bgpdt,Sil,Cstm,Ap,Tmldtb,Itrl,Iscr1,Iscr2,Iscr3,Est,Newslt,Mgg,Iscr4,Mpt1)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_LOADX
   USE C_QVECT
   USE C_SYSTEM
   USE C_TWO
   USE C_ZBLPKX
   USE C_ZNTPKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Casecc
   INTEGER :: Usetd
   INTEGER :: Dlt
   INTEGER :: Slt
   INTEGER :: Bgpdt
   INTEGER :: Sil
   INTEGER :: Cstm
   INTEGER :: Ap
   INTEGER :: Tmldtb
   INTEGER :: Itrl
   INTEGER :: Iscr1
   INTEGER :: Iscr2
   INTEGER :: Iscr3
   INTEGER :: Est
   INTEGER :: Newslt
   INTEGER :: Mgg
   INTEGER :: Iscr4
   INTEGER :: Mpt1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , iclst , idload , idltr , iflag , iharm , illst , iloop , ip1 , iqr ,       &
            & iqvid , iqvrn , iretn , isel , isild , isllst , itauo , iterm , j , k , l , ll , lusetd , m , mskue , n1 , nave ,     &
            & ndload , nex , ngrav , noslt , nsimpl , nsubl , nx , nz , nz1 , sysbuf
   INTEGER , DIMENSION(30) :: gvect
   INTEGER , SAVE :: itran1
   INTEGER , DIMENSION(38) :: iz
   INTEGER , DIMENSION(4) :: izb
   INTEGER , DIMENSION(7) :: mcb , pg
   INTEGER , DIMENSION(2) , SAVE :: minus , name , namt
   REAL :: scale
   EXTERNAL andf , bldpk , bldpkn , close , extern , fread , fwdrec , gopen , gravl1 , gravl2 , intpk , korsz , makmcb , mesage ,   &
          & open , rdtrl , read , skprec , ssg2b , ssgslt , write , wrttrl , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THE PURPOSE OF THIS ROUTINE IS TO CONSTRUCT THE AP MATRIX
!     WHICH HAS 1 COLUMN FOR EACH FUNCTION OF TIME
!     AND TO BUILD THE TIME FUNCTION TABLE (FORMAT SHOWN IN TRLGC)
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Z(1),Iz(1)) , (Zb(1),Izb(1))
   DATA name/4HTRLG , 4HA   / , namt/4HDLT  , 4HTRLG/
   DATA itran1 , minus/4HTRAN , -1 , -1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CORE IS ALLOCATED AS FOLLOWS -
!     . EXTERN PHASE (BUILD STATIC LOADS)
!                                                           POINTER
!     DLOAD STUFF--TLOAD ID,RECORD NO.IN DLT,SCALE FACTOR   ILLST
!     EXTERN LOAD LIST IN SLT ((NEX LENGTH)                 ISLLST
!     2  BUFFERS
!     1  G VECTOR (NG)  COMING FROM TOP
!        N.B.  EXTER WILL OPEN NEWSLT,BGPDT,CSTM,SIL
!
!     . DYNAMIC PHASE
!     DLOAD STUFF                                           ILLST
!     EXTERN LOAD LIST                                      ISLLST
!     SIL TO SILD CONVERTER (NG LENGTH)                     ISILD
!     4  BUFFERS
!     2  P SIZE VECTORS
!     COMPRESSED LIST  SILD,A,TAU                           ICLST
!
!     BRING IN DATA FROM CASECC(DLOAD ID -- TSTEP ID)
!
         nsubl = 0
         nz = korsz(iz)
         ibuf1 = nz - sysbuf + 1
         nx = ibuf1 - 1
         CALL gopen(Casecc,iz(ibuf1),0)
         CALL fread(Casecc,iz(1),166,1)
         idload = iz(13)
         Itrl = iz(38)
         CALL close(Casecc,1)
         IF ( idload==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BUILD NEW SLT
!
         CALL ssgslt(Slt,Newslt,Est)
!
!     FIND DLOAD, TLOAD
!
         file = Dlt
         CALL open(*100,Dlt,iz(ibuf1),0)
         CALL read(*120,*20,Dlt,iz(1),nx,0,iflag)
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
!
!     IS IT A DLOAD SET
!
 20      ndload = iz(3)
         nsimpl = iflag - 3 - ndload
         IF ( ndload/=0 ) THEN
            k = 3
            DO i = 1 , ndload
               k = k + 1
               IF ( iz(k)==idload ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     ITS  A SIMPLE LOAD
!
            ENDDO
         ENDIF
!
!     PROCESS SIMPLE LOAD REQUEST
!
         m = iflag + 1
         iflag = m
         iz(m) = idload
         Z(m+1) = 1.0
         l = ndload + 3
         DO i = 1 , nsimpl
            l = l + 1
            IF ( iz(l)==idload ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL mesage(-31,idload,namt)
         RETURN
      CASE (2)
!
!     PROCESS DLOAD SET
!     FORMAT OF DLOAD = SET ID,SCALE,SCALE,ID,SCALE,ID .... -1,-1
!
         nz1 = nx - iflag
!
!     BRING  IN  ALL  DLOADS
!
         l = iflag + 1
         CALL read(*120,*40,Dlt,iz(l),nz1,0,i)
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
!
!     FIND SELECTED ID
!
 40      isel = l
         DO WHILE ( iz(isel)/=idload )
            SPAG_Loop_2_1: DO
               isel = isel + 2
               IF ( iz(isel+1)==-1 ) THEN
                  isel = isel + 2
                  IF ( isel-l<=i ) EXIT SPAG_Loop_2_1
                  CALL mesage(-31,idload,namt)
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_2_1
         ENDDO
!
!     FOUND DLOAD SELECTED
!
         scale = Z(isel+1)
!
!     CONVERT SCALE FACTORS TO OVERALL SCALE FACTORS
!     BUILD LIST OF TRIPLES-- TLOAD ID,RECORD NO.IN DLT, SCALE FACTOR
!
         l = isel + 2
         m = isel + i
         iflag = m
         nsubl = 0
         spag_nextblock_1 = 3
      CASE (3)
         idload = iz(l+1)
         Z(l) = Z(l)*scale
         k = ndload + 3
         DO i = 1 , nsimpl
            k = k + 1
            IF ( iz(l+1)==iz(k) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
         ENDDO
         CALL mesage(-31,idload,namt)
         RETURN
      CASE (4)
!
!     FOUND SIMPLE ID
!
         iz(m) = iz(l+1)
         Z(m+1) = Z(l)
         iz(m+2) = i
         l = l + 2
         m = m + 3
         nsubl = nsubl + 1
         IF ( iz(l+1)>=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     FOUND SIMPLE LOAD
!
         IF ( ndload/=0 ) i = i + 1
         iz(m+2) = i - 1
         nsubl = 1
         spag_nextblock_1 = 6
      CASE (6)
!
!     MOVE STUFF TO BOTTOM OF CORE
!
         CALL close(Dlt,1)
         illst = nz - nsubl*3 + 1
         nz = nz - nsubl*3
         ibuf1 = nz - sysbuf + 1
         l = iflag
         k = illst
         DO i = 1 , nsubl
            CALL gopen(Dlt,iz(ibuf1),0)
            CALL skprec(Dlt,iz(l+2))
            CALL fread(Dlt,izb,2,0)
            iz(k) = izb(2)
            CALL close(Dlt,1)
            iz(k+1) = iz(l+2)
            iz(k+2) = iz(l+1)
            l = l + 3
            k = k + 3
         ENDDO
!
!     SET UP FOR EXTERN
!
         file = Newslt
         nx = ibuf1 - 1
         isllst = illst
         noslt = 0
         mcb(1) = Slt
         CALL rdtrl(mcb)
         IF ( mcb(1)<=0 ) noslt = -1
         mcb(1) = Sil
         mcb(3) = 0
         CALL rdtrl(mcb)
         Ng = mcb(3)
         IF ( noslt/=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*100,Newslt,iz(ibuf1),0)
         CALL read(*120,*60,Newslt,iz(1),nx,0,iflag)
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(Newslt,1)
         m = illst
         DO i = 1 , nsubl
            SPAG_Loop_2_2: DO j = 3 , iflag
               IF ( iz(m)==iz(j) ) THEN
!
!     FOUND LOAD TO BUILD
!
                  iz(j) = -iabs(iz(j))
                  EXIT SPAG_Loop_2_2
               ENDIF
            ENDDO SPAG_Loop_2_2
            m = m + 3
         ENDDO
!
!     ZERO LOADS NOT TO BUILD
!
         m = illst - iflag + 2
         isllst = m
         DO j = 3 , iflag
            IF ( iz(j)<0 ) THEN
               iz(m) = iabs(iz(j))
            ELSE
               iz(m) = 0
            ENDIF
            m = m + 1
         ENDDO
         nex = iflag - 2
         nz = nz - nex
         ngrav = 0
         iharm = 0
         n1 = nex
         ibuf1 = nz - sysbuf + 1
         ibuf2 = ibuf1 - sysbuf
!
!     SET UP SCRATCH FILE FOR QLOADL
!
         Itran = itran1
         Iqvect = Iscr1
         CALL gopen(Iscr1,iz(ibuf1),1)
         CALL makmcb(pg,Iscr2,Ng,2,1)
         Slt1 = Newslt
         Bgpdt1 = Bgpdt
         Cstm1 = Cstm
         Sil1 = Sil
         Est1 = Est
         Mass = Mgg
         Mpt = Mpt1
         CALL gopen(pg,iz(ibuf2),1)
         Lc = ibuf2 - 1
         CALL extern(nex,ngrav,gvect,iz(isllst),pg,n1,iharm)
         CALL close(pg,1)
         CALL wrttrl(pg)
         CALL write(Iscr1,minus,2,1)
         CALL close(Iscr1,1)
         IF ( ngrav/=0 ) THEN
!
!     DO GRAVITY LOADS
!
            mcb(1) = Mgg
            CALL rdtrl(mcb)
            IF ( mcb(1)<=0 ) CALL mesage(-56,0,nave)
!
!     SAVE LOAD LIST IN CORE
!
            CALL gopen(Iscr4,iz(ibuf2),1)
            CALL write(Iscr4,iz(isllst),3*nsubl+nex,1)
            CALL close(Iscr4,1)
            CALL gravl1(ngrav,gvect,Iscr3,iharm)
            CALL ssg2b(Mgg,Iscr3,0,Tmldtb,0,1,1,Ap)
            CALL gravl2(ngrav,Tmldtb,pg)
            n1 = n1 + ngrav
!
!     RESTORE LOAD LIST TO CORE
!
            CALL gopen(Iscr4,iz(ibuf2),0)
            CALL fread(Iscr4,iz(isllst),3*nsubl+nex,1)
            CALL close(Iscr4,1)
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     BUILD SIL  TO SILD CONVERTER
!
         file = Usetd
         CALL gopen(Usetd,iz(ibuf1),0)
         mcb(1) = Usetd
         CALL rdtrl(mcb)
         lusetd = mcb(2)
         CALL fread(Usetd,iz(1),lusetd,1)
         CALL close(Usetd,1)
         isild = isllst - Ng
         mskue = Two1(Iue)
         l = isild
         DO i = 1 , lusetd
            IF ( andf(iz(i),mskue)==0 ) THEN
               iz(l) = i
               l = l + 1
            ENDIF
         ENDDO
         nz = nz - Ng
!
!     BEGIN LOOP ON EACH TLOAD CARD
!
         ibuf1 = nz - sysbuf + 1
         iclst = 2*lusetd + 1
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         CALL makmcb(mcb,Ap,lusetd,2,1)
         CALL gopen(Ap,iz(ibuf2),1)
         iterm = 0
         CALL gopen(Tmldtb,iz(ibuf3),1)
         iqvrn = 0
         ibuf4 = ibuf3 - sysbuf
         CALL gopen(Iscr3,iz(ibuf4),1)
         nz = ibuf4 - 1
         IF ( nz>=5*lusetd ) THEN
            DO iloop = 1 , nsubl
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
!
!     ZERO AP AND TAU AREA
!
                     k = 2*lusetd
                     DO i = 1 , k
                        Z(i) = 0.0
                     ENDDO
!
!     FIND APPROPRIATE STATIC LOAD
!
                     k = illst + (iloop-1)*3
                     scale = Z(k+2)
                     idload = iz(k)
                     idltr = iz(k+1)
                     IF ( noslt==0 ) THEN
                        k = isllst - 1
                        m = 0
                        DO i = 1 , nex
                           l = k + i
                           IF ( iz(l)==idload ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           IF ( iz(l)/=0 ) m = m + 1
                        ENDDO
                     ENDIF
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  CASE (2)
!
!     POSITION TO PROPER AP RECORD
!
                     file = pg(1)
                     CALL gopen(pg,iz(ibuf1),0)
                     CALL skprec(pg,m)
                     CALL intpk(*62,pg,0,1,0)
                     DO WHILE ( Ieol==0 )
                        CALL zntpki
                        Zb(1) = Zb(1)*scale
                        k = isild + Iii - 1
                        k = iz(k)
                        Z(k) = Zb(1)
                     ENDDO
 62                  CALL close(pg,1)
                     spag_nextblock_2 = 3
                  CASE (3)
!
!     PROCESS DLT STUFF
!
                     CALL gopen(Dlt,iz(ibuf1),0)
                     file = Dlt
                     CALL skprec(Dlt,idltr)
                     CALL fread(Dlt,gvect,8,0)
                     DO
!
!     READS AND BUILDS COMPRESSED LIST SILD,AI,TAU,FOR ALL AI.S
!
                        CALL read(*120,*64,Dlt,izb,4,0,iflag)
                        l = izb(1)
                        Z(l) = Zb(2) + Z(l)
                        Z(l+lusetd) = Zb(3)
                     ENDDO
 64                  CALL close(Dlt,1)
                     iqr = 0
                     ASSIGN 66 TO iretn
                     m = 0
                     k = iclst
                     DO i = 1 , lusetd
                        IF ( Z(i)/=0.0 ) THEN
                           Z(i) = Z(i)*scale
                           m = m + 1
                           iz(k) = i
                           Z(k+1) = Z(i)
                           Z(k+2) = Z(i+lusetd)
                           k = k + 3
                        ENDIF
                     ENDDO
                     spag_nextblock_2 = 4
                  CASE (4)
!
!     SORT ON TAU
!
                     k = 3*m + iclst - 4
                     IF ( iclst<=k ) THEN
                        DO l = iclst , k , 3
                           IF ( .NOT.(Z(l+5)>Z(l+2) .OR. (Z(l+5)==Z(l+2) .AND. iz(l+3)>=iz(l))) ) THEN
                              ll = l
                              iz(k+4) = iz(l+3)
                              Z(k+5) = Z(l+4)
                              Z(k+6) = Z(l+5)
                              SPAG_Loop_3_3: DO
                                 iz(ll+3) = iz(ll)
                                 Z(ll+4) = Z(ll+1)
                                 Z(ll+5) = Z(ll+2)
                                 ll = ll - 3
                                 IF ( .NOT.(ll>=iclst .AND. (Z(k+6)<Z(ll+2) .OR. (Z(k+6)==Z(ll+2).AND.iz(k+4)<iz(ll)))) ) THEN
                                    iz(ll+3) = iz(k+4)
                                    Z(ll+4) = Z(k+5)
                                    Z(ll+5) = Z(k+6)
                                    EXIT SPAG_Loop_3_3
                                 ENDIF
                              ENDDO SPAG_Loop_3_3
                           ENDIF
                        ENDDO
                     ENDIF
!
!     OUTPUT PVECTOR FOR EACH UNIQUE TAU
!
                     l = iclst
                     SPAG_Loop_2_4: DO
!WKBR 8/94 ALPHA  341 TAUO = Z(L+2)
                        itauo = iz(l+2)
                        CALL bldpk(1,1,Ap,0,0)
                        DO
                           Za(1) = Z(l+1)
                           Iib = iz(l)
                           CALL zblpki
                           l = l + 3
!WKBR 8/94 ALPHA IF (L.LT.3*M+ICLST .AND. Z(L+2).EQ.TAUO) GO TO 345
                           IF ( l>=3*m+iclst .OR. iz(l+2)/=itauo ) THEN
                              CALL bldpkn(Ap,0,mcb)
!
!     PUT OUT LINE OF TIME TABLE
!
                              iterm = iterm + 1
                              CALL write(Tmldtb,iterm,1,0)
                              CALL write(Tmldtb,idload,1,0)
                              CALL write(Tmldtb,gvect,1,0)
!WKBR 8/94 ALPHA CALL WRITE (TMLDTB,TAUO,1,0)
                              CALL write(Tmldtb,itauo,1,0)
                              CALL write(Tmldtb,gvect(3),6,0)
                              CALL write(Tmldtb,iqr,1,0)
                              IF ( l>=iclst+3*m ) GOTO iretn
                              CYCLE SPAG_Loop_2_4
                           ENDIF
                        ENDDO
                        EXIT SPAG_Loop_2_4
                     ENDDO SPAG_Loop_2_4
!
!     FIND PROPER QVEC RECORD
!
 66                  IF ( noslt/=0 ) CYCLE
                     CALL gopen(Iscr1,iz(ibuf1),0)
                     file = Iscr1
                     SPAG_Loop_2_5: DO
                        CALL read(*70,*140,Iscr1,iqvid,1,0,iflag)
                        IF ( iqvid==-1 ) GOTO 70
                        IF ( iqvid==idload ) EXIT SPAG_Loop_2_5
                        CALL fwdrec(*120,Iscr1)
                     ENDDO SPAG_Loop_2_5
!
!     BUILD LIST OF SILD,AI,TAU FROM QVEC STUFF
!
 68                  CALL fread(Iscr1,m,1,0)
                     k = iclst
                     IF ( m/=-1 ) THEN
                        DO i = 1 , m
                           CALL fread(Iscr1,Zb,2,0)
                           Zb(2) = Zb(2)*scale
                           j = isild + izb(1) - 1
                           j = iz(j)
                           iz(k) = j
                           Z(k+1) = Zb(2)
                           Z(k+2) = Z(j+lusetd)
                           k = k + 3
                        ENDDO
                        iqvrn = iqvrn + 1
                        iqr = iqvrn
                        CALL fread(Iscr1,iz(k),9,0)
                        CALL write(Iscr3,iz(k),9,0)
                        ASSIGN 68 TO iretn
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
!
!     END OF QVECT PROCESSING
!
 70                  CALL close(Iscr1,1)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
!     END OF TLOAD CARD LOOP
!
            ENDDO
            CALL close(Ap,1)
            CALL wrttrl(mcb)
            CALL close(Iscr3,1)
!
!     APPEND QVECT STUFF TO TMLDTB
!
            CALL gopen(Iscr3,iz(ibuf1),0)
            file = Iscr3
            CALL write(Tmldtb,0,0,1)
            CALL read(*80,*80,Iscr3,iz(1),nz,0,iflag)
         ENDIF
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 80      CALL write(Tmldtb,iz(1),iflag,1)
         CALL close(Tmldtb,1)
         mcb(1) = Tmldtb
         mcb(2) = iterm
         mcb(3) = iflag
         CALL wrttrl(mcb)
         CALL close(Iscr3,1)
         spag_nextblock_1 = 8
      CASE (8)
         RETURN
!
!     FATAL ERRORS
!
 100     ip1 = -1
         spag_nextblock_1 = 9
      CASE (9)
         CALL mesage(ip1,file,name)
         RETURN
 120     ip1 = -2
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 140     ip1 = -3
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         CALL mesage(-8,0,name)
         CALL mesage(-61,0,name)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trlga
