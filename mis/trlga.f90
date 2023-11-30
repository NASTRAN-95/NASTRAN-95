
SUBROUTINE trlga(Casecc,Usetd,Dlt,Slt,Bgpdt,Sil,Cstm,Ap,Tmldtb,Itrl,Iscr1,Iscr2,Iscr3,Est,Newslt,Mgg,Iscr4,Mpt1)
   IMPLICIT NONE
   INTEGER Bgpdt1 , Cstm1 , Est1 , Idit , Ieol , Ieor , Iib , Iii , Iqvect , Isil , Isk(11) , Itran , Iue , Iz(38) , Izb(4) ,       &
         & Ksystm(65) , Lc , Lodc , Mass , Mpt , N(3) , Ng , Nobld , Sil1 , Slt1 , Sysbuf , Two1(32)
   REAL Edt , Gptt , Old , Z(1) , Za(4) , Zb(4)
   COMMON /bitpos/ Isk , Iue
   COMMON /blank / Ng
   COMMON /loadx / Lc , Slt1 , Bgpdt1 , Old , Cstm1 , Sil1 , Isil , Est1 , Mpt , Gptt , Edt , N , Lodc , Mass , Nobld , Idit
   COMMON /qvect / Itran , Iqvect
   COMMON /system/ Ksystm
   COMMON /two   / Two1
   COMMON /zblpkx/ Za , Iib
   COMMON /zntpkx/ Zb , Iii , Ieol , Ieor
   COMMON /zzzzzz/ Z
   INTEGER Ap , Bgpdt , Casecc , Cstm , Dlt , Est , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Itrl , Mgg , Mpt1 , Newslt , Sil , Slt ,        &
         & Tmldtb , Usetd
   INTEGER andf , korsz
   INTEGER file , gvect(30) , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , iclst , idload , idltr , iflag , iharm , illst , iloop , ip1 ,    &
         & iqr , iqvid , iqvrn , iretn , isel , isild , isllst , itauo , iterm , itran1 , j , k , l , ll , lusetd , m , mcb(7) ,    &
         & minus(2) , mskue , n1 , name(2) , namt(2) , nave , ndload , nex , ngrav , noslt , nsimpl , nsubl , nx , nz , nz1 , pg(7)
   REAL scale
   EXTERNAL andf
!
!     THE PURPOSE OF THIS ROUTINE IS TO CONSTRUCT THE AP MATRIX
!     WHICH HAS 1 COLUMN FOR EACH FUNCTION OF TIME
!     AND TO BUILD THE TIME FUNCTION TABLE (FORMAT SHOWN IN TRLGC)
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Z(1),Iz(1)) , (Zb(1),Izb(1))
   DATA name/4HTRLG , 4HA   / , namt/4HDLT  , 4HTRLG/
   DATA itran1 , minus/4HTRAN , -1 , -1/
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
   nz = korsz(Iz)
   ibuf1 = nz - Sysbuf + 1
   nx = ibuf1 - 1
   CALL gopen(Casecc,Iz(ibuf1),0)
   CALL fread(Casecc,Iz(1),166,1)
   idload = Iz(13)
   Itrl = Iz(38)
   CALL close(Casecc,1)
   IF ( idload==0 ) GOTO 1200
!
!     BUILD NEW SLT
!
   CALL ssgslt(Slt,Newslt,Est)
!
!     FIND DLOAD, TLOAD
!
   file = Dlt
   CALL open(*1300,Dlt,Iz(ibuf1),0)
   CALL read(*1500,*100,Dlt,Iz(1),nx,0,iflag)
   GOTO 1700
!
!     IS IT A DLOAD SET
!
 100  ndload = Iz(3)
   nsimpl = iflag - 3 - ndload
   IF ( ndload/=0 ) THEN
      k = 3
      DO i = 1 , ndload
         k = k + 1
         IF ( Iz(k)==idload ) GOTO 200
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
   Iz(m) = idload
   Z(m+1) = 1.0
   l = ndload + 3
   DO i = 1 , nsimpl
      l = l + 1
      IF ( Iz(l)==idload ) GOTO 600
   ENDDO
   CALL mesage(-31,idload,namt)
   GOTO 99999
!
!     PROCESS DLOAD SET
!     FORMAT OF DLOAD = SET ID,SCALE,SCALE,ID,SCALE,ID .... -1,-1
!
 200  nz1 = nx - iflag
!
!     BRING  IN  ALL  DLOADS
!
   l = iflag + 1
   CALL read(*1500,*300,Dlt,Iz(l),nz1,0,i)
   GOTO 1700
!
!     FIND SELECTED ID
!
 300  isel = l
   DO WHILE ( Iz(isel)/=idload )
      DO
         isel = isel + 2
         IF ( Iz(isel+1)==-1 ) THEN
            isel = isel + 2
            IF ( isel-l<=i ) EXIT
            CALL mesage(-31,idload,namt)
            GOTO 99999
         ENDIF
      ENDDO
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
 400  idload = Iz(l+1)
   Z(l) = Z(l)*scale
   k = ndload + 3
   DO i = 1 , nsimpl
      k = k + 1
      IF ( Iz(l+1)==Iz(k) ) GOTO 500
!
   ENDDO
   CALL mesage(-31,idload,namt)
   GOTO 99999
!
!     FOUND SIMPLE ID
!
 500  Iz(m) = Iz(l+1)
   Z(m+1) = Z(l)
   Iz(m+2) = i
   l = l + 2
   m = m + 3
   nsubl = nsubl + 1
   IF ( Iz(l+1)<0 ) GOTO 700
   GOTO 400
!
!     FOUND SIMPLE LOAD
!
 600  IF ( ndload/=0 ) i = i + 1
   Iz(m+2) = i - 1
   nsubl = 1
!
!     MOVE STUFF TO BOTTOM OF CORE
!
 700  CALL close(Dlt,1)
   illst = nz - nsubl*3 + 1
   nz = nz - nsubl*3
   ibuf1 = nz - Sysbuf + 1
   l = iflag
   k = illst
   DO i = 1 , nsubl
      CALL gopen(Dlt,Iz(ibuf1),0)
      CALL skprec(Dlt,Iz(l+2))
      CALL fread(Dlt,Izb,2,0)
      Iz(k) = Izb(2)
      CALL close(Dlt,1)
      Iz(k+1) = Iz(l+2)
      Iz(k+2) = Iz(l+1)
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
   IF ( noslt/=0 ) GOTO 900
   CALL open(*1300,Newslt,Iz(ibuf1),0)
   CALL read(*1500,*800,Newslt,Iz(1),nx,0,iflag)
   GOTO 1700
 800  CALL close(Newslt,1)
   m = illst
   DO i = 1 , nsubl
      DO j = 3 , iflag
         IF ( Iz(m)==Iz(j) ) THEN
!
!     FOUND LOAD TO BUILD
!
            Iz(j) = -iabs(Iz(j))
            EXIT
         ENDIF
      ENDDO
      m = m + 3
   ENDDO
!
!     ZERO LOADS NOT TO BUILD
!
   m = illst - iflag + 2
   isllst = m
   DO j = 3 , iflag
      IF ( Iz(j)<0 ) THEN
         Iz(m) = iabs(Iz(j))
      ELSE
         Iz(m) = 0
      ENDIF
      m = m + 1
   ENDDO
   nex = iflag - 2
   nz = nz - nex
   ngrav = 0
   iharm = 0
   n1 = nex
   ibuf1 = nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
!
!     SET UP SCRATCH FILE FOR QLOADL
!
   Itran = itran1
   Iqvect = Iscr1
   CALL gopen(Iscr1,Iz(ibuf1),1)
   CALL makmcb(pg,Iscr2,Ng,2,1)
   Slt1 = Newslt
   Bgpdt1 = Bgpdt
   Cstm1 = Cstm
   Sil1 = Sil
   Est1 = Est
   Mass = Mgg
   Mpt = Mpt1
   CALL gopen(pg,Iz(ibuf2),1)
   Lc = ibuf2 - 1
   CALL extern(nex,ngrav,gvect,Iz(isllst),pg,n1,iharm)
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
      CALL gopen(Iscr4,Iz(ibuf2),1)
      CALL write(Iscr4,Iz(isllst),3*nsubl+nex,1)
      CALL close(Iscr4,1)
      CALL gravl1(ngrav,gvect,Iscr3,iharm)
      CALL ssg2b(Mgg,Iscr3,0,Tmldtb,0,1,1,Ap)
      CALL gravl2(ngrav,Tmldtb,pg)
      n1 = n1 + ngrav
!
!     RESTORE LOAD LIST TO CORE
!
      CALL gopen(Iscr4,Iz(ibuf2),0)
      CALL fread(Iscr4,Iz(isllst),3*nsubl+nex,1)
      CALL close(Iscr4,1)
   ENDIF
!
!     BUILD SIL  TO SILD CONVERTER
!
 900  file = Usetd
   CALL gopen(Usetd,Iz(ibuf1),0)
   mcb(1) = Usetd
   CALL rdtrl(mcb)
   lusetd = mcb(2)
   CALL fread(Usetd,Iz(1),lusetd,1)
   CALL close(Usetd,1)
   isild = isllst - Ng
   mskue = Two1(Iue)
   l = isild
   DO i = 1 , lusetd
      IF ( andf(Iz(i),mskue)==0 ) THEN
         Iz(l) = i
         l = l + 1
      ENDIF
   ENDDO
   nz = nz - Ng
!
!     BEGIN LOOP ON EACH TLOAD CARD
!
   ibuf1 = nz - Sysbuf + 1
   iclst = 2*lusetd + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   CALL makmcb(mcb,Ap,lusetd,2,1)
   CALL gopen(Ap,Iz(ibuf2),1)
   iterm = 0
   CALL gopen(Tmldtb,Iz(ibuf3),1)
   iqvrn = 0
   ibuf4 = ibuf3 - Sysbuf
   CALL gopen(Iscr3,Iz(ibuf4),1)
   nz = ibuf4 - 1
   IF ( nz>=5*lusetd ) THEN
      DO iloop = 1 , nsubl
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
         idload = Iz(k)
         idltr = Iz(k+1)
         IF ( noslt==0 ) THEN
            k = isllst - 1
            m = 0
            DO i = 1 , nex
               l = k + i
               IF ( Iz(l)==idload ) GOTO 920
               IF ( Iz(l)/=0 ) m = m + 1
            ENDDO
         ENDIF
         GOTO 960
!
!     POSITION TO PROPER AP RECORD
!
 920     file = pg(1)
         CALL gopen(pg,Iz(ibuf1),0)
         CALL skprec(pg,m)
         CALL intpk(*940,pg,0,1,0)
         DO WHILE ( Ieol==0 )
            CALL zntpki
            Zb(1) = Zb(1)*scale
            k = isild + Iii - 1
            k = Iz(k)
            Z(k) = Zb(1)
         ENDDO
 940     CALL close(pg,1)
!
!     PROCESS DLT STUFF
!
 960     CALL gopen(Dlt,Iz(ibuf1),0)
         file = Dlt
         CALL skprec(Dlt,idltr)
         CALL fread(Dlt,gvect,8,0)
         DO
!
!     READS AND BUILDS COMPRESSED LIST SILD,AI,TAU,FOR ALL AI.S
!
            CALL read(*1500,*980,Dlt,Izb,4,0,iflag)
            l = Izb(1)
            Z(l) = Zb(2) + Z(l)
            Z(l+lusetd) = Zb(3)
         ENDDO
 980     CALL close(Dlt,1)
         iqr = 0
         ASSIGN 1040 TO iretn
         m = 0
         k = iclst
         DO i = 1 , lusetd
            IF ( Z(i)/=0.0 ) THEN
               Z(i) = Z(i)*scale
               m = m + 1
               Iz(k) = i
               Z(k+1) = Z(i)
               Z(k+2) = Z(i+lusetd)
               k = k + 3
            ENDIF
         ENDDO
!
!     SORT ON TAU
!
 1000    k = 3*m + iclst - 4
         IF ( iclst<=k ) THEN
            DO l = iclst , k , 3
               IF ( .NOT.(Z(l+5)>Z(l+2) .OR. (Z(l+5)==Z(l+2) .AND. Iz(l+3)>=Iz(l))) ) THEN
                  ll = l
                  Iz(k+4) = Iz(l+3)
                  Z(k+5) = Z(l+4)
                  Z(k+6) = Z(l+5)
                  DO
                     Iz(ll+3) = Iz(ll)
                     Z(ll+4) = Z(ll+1)
                     Z(ll+5) = Z(ll+2)
                     ll = ll - 3
                     IF ( .NOT.(ll>=iclst .AND. (Z(k+6)<Z(ll+2) .OR. (Z(k+6)==Z(ll+2).AND.Iz(k+4)<Iz(ll)))) ) THEN
                        Iz(ll+3) = Iz(k+4)
                        Z(ll+4) = Z(k+5)
                        Z(ll+5) = Z(k+6)
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
!
!     OUTPUT PVECTOR FOR EACH UNIQUE TAU
!
         l = iclst
!WKBR 8/94 ALPHA  341 TAUO = Z(L+2)
 1020    itauo = Iz(l+2)
         CALL bldpk(1,1,Ap,0,0)
         DO
            Za(1) = Z(l+1)
            Iib = Iz(l)
            CALL zblpki
            l = l + 3
!WKBR 8/94 ALPHA IF (L.LT.3*M+ICLST .AND. Z(L+2).EQ.TAUO) GO TO 345
            IF ( l>=3*m+iclst .OR. Iz(l+2)/=itauo ) THEN
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
               GOTO 1020
            ENDIF
         ENDDO
!
!     FIND PROPER QVEC RECORD
!
 1040    IF ( noslt/=0 ) CYCLE
         CALL gopen(Iscr1,Iz(ibuf1),0)
         file = Iscr1
         DO
            CALL read(*1080,*1600,Iscr1,iqvid,1,0,iflag)
            IF ( iqvid==-1 ) GOTO 1080
            IF ( iqvid==idload ) EXIT
            CALL fwdrec(*1500,Iscr1)
         ENDDO
!
!     BUILD LIST OF SILD,AI,TAU FROM QVEC STUFF
!
 1060    CALL fread(Iscr1,m,1,0)
         k = iclst
         IF ( m/=-1 ) THEN
            DO i = 1 , m
               CALL fread(Iscr1,Zb,2,0)
               Zb(2) = Zb(2)*scale
               j = isild + Izb(1) - 1
               j = Iz(j)
               Iz(k) = j
               Z(k+1) = Zb(2)
               Z(k+2) = Z(j+lusetd)
               k = k + 3
            ENDDO
            iqvrn = iqvrn + 1
            iqr = iqvrn
            CALL fread(Iscr1,Iz(k),9,0)
            CALL write(Iscr3,Iz(k),9,0)
            ASSIGN 1060 TO iretn
            GOTO 1000
         ENDIF
!
!     END OF QVECT PROCESSING
!
 1080    CALL close(Iscr1,1)
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
      CALL gopen(Iscr3,Iz(ibuf1),0)
      file = Iscr3
      CALL write(Tmldtb,0,0,1)
      CALL read(*1100,*1100,Iscr3,Iz(1),nz,0,iflag)
   ENDIF
   GOTO 1700
 1100 CALL write(Tmldtb,Iz(1),iflag,1)
   CALL close(Tmldtb,1)
   mcb(1) = Tmldtb
   mcb(2) = iterm
   mcb(3) = iflag
   CALL wrttrl(mcb)
   CALL close(Iscr3,1)
 1200 RETURN
!
!     FATAL ERRORS
!
 1300 ip1 = -1
 1400 CALL mesage(ip1,file,name)
   RETURN
 1500 ip1 = -2
   GOTO 1400
 1600 ip1 = -3
   GOTO 1400
 1700 CALL mesage(-8,0,name)
   CALL mesage(-61,0,name)
   RETURN
99999 RETURN
END SUBROUTINE trlga
