!*==read2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE read2(Maa,Phia,Scr1,Norm,Ia,Uset,Mi,Lama,Ipout,Scr2,Epsi,Scr3)
   USE c_condas
   USE c_givn
   USE c_output
   USE c_packx
   USE c_sturmx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Maa
   INTEGER :: Phia
   INTEGER :: Scr1
   INTEGER :: Norm
   INTEGER :: Ia
   REAL :: Uset
   INTEGER :: Mi
   INTEGER :: Lama
   INTEGER :: Ipout
   INTEGER :: Scr2
   REAL :: Epsi
   INTEGER :: Scr3
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(13) :: core
   REAL*8 , DIMENSION(1) :: dcore
   REAL*8 :: dxmax
   REAL :: gm , refreq , t2 , t3 , t4 , t5 , t6 , t7 , tphi , xmax , xmax1
   INTEGER :: i , i0 , icopy , iden , iflag , imi , imsg , ip1 , ipont , istor , j , jjj , jstor , k , kk , l , l1 , l2 , lcore ,   &
            & m , mcol , mm , ncol , ncol2 , nlama , nrow , nrow2 , nwords , nwrds
   INTEGER , DIMENSION(50) , SAVE :: ihead
   INTEGER , DIMENSION(10) , SAVE :: ihead1
   INTEGER , DIMENSION(7) :: im , iphia , ix
   INTEGER , SAVE :: mass , max , point
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
! End of declarations rewritten by SPAG
!
!
!     COMPUTE MODAL MASS AND NORMALIZES VECTORS ACCORDING TO POINT,
!     MASS, OR MAX.  ALSO LOOKS FOR LARGE OFF DIAGONAL TERM
!
   !>>>>EQUIVALENCE (Consts(2),Tphi) , (ix(2),ncol) , (ix(3),nrow) , (Core(1),Icore(1),Dcore(1)) , (dxmax,xmax)
   DATA ihead1/21 , 9 , 8*0/
   DATA ihead/21 , 6 , 7*0 , 7 , 40*0/
   DATA mass , point/4HMASS , 4HPOIN/
   DATA max/4HMAX /
   DATA nam/4HREAD , 1H2/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     READ2  SHOULD NORMALIZE  PHIA  ACCORDING TO NORM +METHOD
!
         lcore = korsz(core)
!
!     DECIDE IF MI WANTED
!
         imi = 0
         ix(1) = Mi
         CALL rdtrl(ix)
         IF ( ix(1)<=0 ) THEN
            Epsi = 0.0
            imi = -1
            IF ( Norm==mass ) Norm = max
         ENDIF
         ix(1) = Phia
         CALL rdtrl(ix)
         CALL makmcb(iphia,Phia,ix(3),ix(4),ix(5))
!
!     SET UP TO HANDLE IDENTITY MATRIX
!
         iden = 0
         im(1) = Maa
         CALL rdtrl(im)
         IF ( im(4)==8 ) iden = 1
!
!     FIND TYPE OF NORMALIZATION
!
         IF ( Norm==mass ) THEN
!
!     COMPUTE UNNORMALIZED MODAL MASS
!
            ASSIGN 120 TO icopy
         ELSE
            ipont = 1
            IF ( Norm/=point ) THEN
               IF ( Ia<1 .OR. Ia>nrow ) THEN
               ENDIF
!
!     TYPE IS  MAX
!
               ipont = 0
            ENDIF
!
!     POINT
!
            ASSIGN 20 TO icopy
         ENDIF
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!
!     PROCESS PHIA - NORMALIZE - COPY TO PHIA
!
 20      lcore = lcore - sysbuf
         CALL gopen(Scr1,core(lcore+1),0)
         lcore = lcore - sysbuf
         CALL gopen(Phia,core(lcore+1),1)
         itb = ix(5)
         jj = nrow
         ii = 1
         incur = 1
         ita1 = itb
         itb1 = itb
         incur1 = 1
         DO i = 1 , ncol
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL unpack(*22,Scr1,core(3))
                  ii1 = ii
                  jj1 = jj
                  jjj = 1
                  IF ( itb==2 ) THEN
                     DO j = 1 , nrow
                        IF ( dabs(dcore(j+1))>dabs(dcore(jjj+1)) ) jjj = j
                     ENDDO
                     jjj = jjj + 1
                     IF ( ipont==1 ) THEN
                        jjj = Ia + 1
                        IF ( dabs(dcore(jjj))<=1.0D-15 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                     dxmax = dcore(jjj)
                     DO j = 1 , nrow
                        dcore(j+1) = dcore(j+1)/dxmax
                     ENDDO
                  ELSE
                     DO j = 1 , nrow
                        IF ( abs(core(j+2))>abs(core(jjj+2)) ) jjj = j
                     ENDDO
                     jjj = jjj + 2
                     IF ( ipont==1 ) THEN
                        jjj = Ia + 2
                        IF ( abs(core(jjj))<=1.0E-15 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                     xmax = core(jjj)
                     DO j = 1 , nrow
                        core(j+2) = core(j+2)/xmax
                     ENDDO
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL pack(core(3),Phia,iphia)
                  CYCLE
 22               ii1 = 1
                  jj1 = 1
                  CALL pack(core,Phia,iphia)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(Phia,1)
         CALL close(Scr1,1)
         spag_nextblock_1 = 2
      CASE (2)
!
!     COMPUTE MODAL MASS
!
         IF ( imi<0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iden==0 ) THEN
!
            CALL ssg2b(Maa,Phia,0,Scr2,0,itb,1,Scr3)
            CALL ssg2b(Phia,Scr2,0,Mi,1,itb,1,Scr3)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ASSIGN 40 TO icopy
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 40      CALL ssg2b(Phia,Scr1,0,Mi,1,itb,1,Scr3)
         spag_nextblock_1 = 3
      CASE (3)
!
!     COMPUTE GENERALIZED STIFFNESS
!
!
!     COMPUTE FREQUENCY ETC
!
         itb = 1
         ii = 1
         jj = ncol
         incur = 1
         imsg = 0
         CALL gopen(Lama,core(lcore+1),0)
         CALL read(*160,*60,Lama,core(1),lcore,1,nlama)
         CALL mesage(-8,0,nam)
         GOTO 200
!
!     NLAMA IS THE NUMBER OF EIGENVALUES FOUND   NCOL IS TH NUMBER OF
!     VECTORS
!
!
!     BRING IN THE ORDER FOUND
!
 60      kk = nlama + 2*ncol + 8
!
!     KK IS THE POINTER TO THE ORDER FOUND
!     L1 AND  L2 ARE COUNTERS FOR MISSING LOW FREQ. BELOW SHIFT POINTS
!     STURM AND KEEP WERE SAVED IN SDCOMP, SHFTPT AND PTSHFT IN FEER
!     AND INVPWR (REAL SYMMETRIC EIGENVALUE PROBLEM ONLY)
!
         CALL read(*160,*80,Lama,icore(kk+1),lcore,1,iflag)
         CALL mesage(-8,0,nam)
         GOTO 200
 80      CALL close(Lama,1)
         CALL gopen(Lama,core(lcore+1),1)
         CALL write(Lama,ihead(1),50,0)
         CALL write(Lama,head(1),96,1)
         lcore = lcore + sysbuf
         core(nlama+6) = 0.0
         core(nlama+7) = 0.0
         IF ( imi>=0 ) THEN
            CALL gopen(Mi,core(lcore+1),0)
            l1 = sturm
            l2 = keep
            shftpt = shftpt + 1.E-10
            ptshft = ptshft + 1.E-10
         ENDIF
         DO i = 1 , nlama
            spag_nextblock_3 = 1
            SPAG_DispatchLoop_3: DO
               SELECT CASE (spag_nextblock_3)
               CASE (1)
                  icore(nlama+1) = i
                  l = kk + i
                  icore(nlama+2) = icore(l)
                  core(nlama+3) = core(i)
                  core(nlama+4) = sqrt(abs(core(i)))
                  core(nlama+5) = core(nlama+4)/tphi
                  IF ( core(i)>1.E-10 .AND. core(i)<=shftpt ) l1 = l1 - 1
                  IF ( core(i)>1.E-10 .AND. core(i)<=ptshft ) l2 = l2 - 1
                  IF ( imi<0 ) THEN
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
                  IF ( i<=ncol ) THEN
                     l = nlama + i + 7
                     k = l - 1 + i
                     CALL unpack(*82,Mi,core(l))
                     core(nlama+6) = core(k)
                     core(nlama+7) = core(k)*core(nlama+3)
!
!     ZERO OUT GENERALIZED MASS AND GENERALIZED STIFFNESS FOR THE RIGID
!     BODY MODE OF ZERO FREQUENCY
!
!     (G.C.  3/92
!     NEXT 4 NEW LINES CAUSED DEMO T03121A TO DIE. MORE STUDY IS NEEDED)
!
!     IF (CORE(I) .GE. 0.0) GO TO 200
!     CORE(NLAMA+3) = 0.0
!     CORE(NLAMA+4) = 0.0
!     CORE(NLAMA+5) = 0.0
                     core(l) = core(k)
                     spag_nextblock_3 = 2
                     CYCLE SPAG_DispatchLoop_3
                  ENDIF
!
!     NO MORE VECTORS
!     REPLACE STURM BY SMALLER OF L1 OR L2, IF NOT ALL LOWER MODES FOUND
!     SET STRUM TO   -1 IF THERE IS NOT ENOUGH INFORMATION,
!     SET STRUM TO -999 IF DIAG 37 IS REQUESTED (NOT TO PRINT MESSAGE).
!
 82               core(nlama+6) = 0.0
                  core(nlama+7) = 0.0
                  spag_nextblock_3 = 2
               CASE (2)
                  CALL write(Lama,core(nlama+1),7,0)
                  EXIT SPAG_DispatchLoop_3
               END SELECT
            ENDDO SPAG_DispatchLoop_3
         ENDDO
         IF ( l1<0 ) l1 = 0
         IF ( l2<0 ) l2 = 0
         IF ( l1>l2 ) l1 = l2
         IF ( sturm/=-1 .AND. l1>=0 ) sturm = l1
         IF ( sturm>nr .AND. nr>0 ) sturm = sturm - nr
         IF ( keep<=0 .AND. ptshft>0. ) sturm = -1
         CALL sswtch(37,j)
         IF ( j==1 ) sturm = -999
         CALL close(Lama,1)
         IF ( imi>=0 ) CALL close(Mi,1)
         imsg = 0
         xmax = 0.
         xmax1 = 0.
         istor = 0
         jstor = 0
!
!     EPSI = 0 IMPLIES TO NOT CHECK MODAL MASS TERMS
!
         IF ( Epsi/=0.0 ) THEN
            CALL gopen(Mi,core(lcore+1),0)
            DO i = 1 , ncol
               m = nlama + i + 7
               mcol = m + ncol
               CALL unpack(*220,Mi,core(mcol))
               IF ( core(m)/=0 ) THEN
                  SPAG_Loop_2_1: DO j = 1 , ncol
                     IF ( i==j ) EXIT SPAG_Loop_2_1
                     k = mcol + j - 1
                     mm = nlama + j + 7
                     IF ( core(mm)/=0.0 ) THEN
                        gm = abs(core(k))/sqrt(abs(core(m)*core(mm)))
                        IF ( gm>xmax1 ) THEN
                           xmax1 = gm
                           istor = i
                           jstor = j
                        ENDIF
                        IF ( gm>Epsi ) THEN
                           imsg = imsg + 1
                           xmax = amax1(xmax,gm)
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ENDDO
!
            CALL close(Mi,1)
            IF ( imsg/=0 ) CALL mesage(34,xmax,Epsi)
         ENDIF
         IF ( givens/=.0 ) THEN
            IF ( nnv==0 ) THEN
               IF ( lfreq>.0 ) THEN
!
!     REARRANGE THE EIGENVALUE TABLE, IF NECESSARY, FOR GIVENS METHOD
!
                  CALL gopen(Lama,core(lcore+1),0)
                  CALL skprec(Lama,1)
                  nwords = 7*nlama
                  CALL read(*160,*200,Lama,core(1),nwords,1,nwrds)
                  refreq = core(3)
                  DO i = 2 , nlama
                     j = 7*(i-1) + 3
                     IF ( core(j)<refreq ) THEN
                        refreq = core(j)
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         CALL gopen(Ipout,core(lcore+1),0)
         CALL read(*180,*100,Ipout,core(1),lcore,1,iflag)
         CALL mesage(-8,0,nam)
         GOTO 200
 100     CALL close(Ipout,1)
         CALL gopen(Ipout,core(lcore+1),1)
         ihead1(3) = icore(1)
         CALL write(Ipout,ihead1,10,0)
         i0 = 0
         core(i0+9) = xmax1
         icore(i0+10) = istor
         icore(i0+11) = jstor
         icore(i0+12) = imsg
         icore(i0+13) = sturm
         CALL write(Ipout,core(2),40,0)
         CALL write(Ipout,head,96,1)
         IF ( icore(1)==1 ) THEN
            iflag = iflag - 12
            ihead1(3) = 3
            ihead1(10) = 6
            CALL write(Ipout,ihead1,50,0)
            CALL write(Ipout,head,96,1)
            IF ( iflag/=0 ) CALL write(Ipout,core(13),iflag,0)
         ENDIF
         CALL close(Ipout,1)
         ix(1) = Ipout
         CALL wrttrl(ix)
         RETURN
 120     IF ( iden==0 ) THEN
!
            CALL ssg2b(Maa,Phia,0,Scr2,0,iphia(5),1,Scr3)
            CALL ssg2b(Phia,Scr2,0,Mi,1,iphia(5),1,Scr3)
         ELSE
!
!     MASS MATRIX IS IDENTITY
!
            CALL ssg2b(Phia,Scr1,0,Mi,1,iphia(5),1,Scr3)
         ENDIF
!
!     BRING IN DIAGONALS
!
         lcore = lcore - sysbuf
         CALL gopen(Mi,core(lcore+1),0)
         itb = iphia(5)
         ii = 1
         jj = ncol
         IF ( itb/=2 ) THEN
            DO j = 1 , ncol
               CALL unpack(*125,Mi,core(ncol+1))
               k = ncol + j
               core(j) = 1.0/sqrt(abs(core(k)))
               CYCLE
 125           core(j) = 0.0
            ENDDO
         ELSE
            DO j = 1 , ncol
               CALL unpack(*130,Mi,dcore(ncol+1))
               k = ncol + j
               dcore(j) = 1.0D0/dsqrt(dabs(dcore(k)))
               CYCLE
 130           dcore(j) = 0.0D0
            ENDDO
         ENDIF
         CALL close(Mi,1)
!
!     DIVIDE EACH TERM BY SQRT (MI)
!
         CALL gopen(Scr1,core(lcore+1),0)
         lcore = lcore - sysbuf
         CALL gopen(Phia,core(lcore+1),1)
         ii = 1
         jj = nrow
         incur = 1
         ita1 = itb
         itb1 = itb
         ncol2 = itb*ncol
         nrow2 = itb*nrow
         ii1 = 1
         jj1 = nrow
         incur1 = 1
         DO i = 1 , ncol
            CALL unpack(*140,Scr1,core(ncol2+1))
            IF ( itb/=2 ) THEN
               DO j = 1 , nrow
                  k = ncol + j
                  core(k) = core(k)*core(i)
               ENDDO
            ELSE
               DO j = 1 , nrow
                  k = ncol + j
                  dcore(k) = dcore(k)*dcore(i)
               ENDDO
            ENDIF
            CALL pack(core(ncol2+1),Phia,iphia)
            CYCLE
 140        DO j = 1 , nrow2
               k = ncol2 + j
               core(k) = 0.0
            ENDDO
            CALL pack(core(ncol2+1),Phia,iphia)
         ENDDO
         CALL close(Phia,1)
         CALL close(Scr1,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     COPY ROUTINE - PHIA TO SCR1
!
         lcore = lcore - sysbuf
         CALL gopen(Phia,core(lcore+1),0)
         lcore = lcore - sysbuf
         CALL gopen(Scr1,core(lcore+1),1)
         dcore(1) = 0.0D+0
         itb = ix(5)
         ita1 = itb
         itb1 = itb
         incur = 1
         incur1 = 1
         DO jjj = 1 , ncol
            ii = 0
            CALL unpack(*150,Phia,core(3))
            ii1 = ii
            jj1 = jj
            CALL pack(core(3),Scr1,iphia)
            CYCLE
 150        ii1 = 1
            jj1 = 1
            CALL pack(core,Scr1,iphia)
         ENDDO
         CALL close(Phia,1)
         CALL close(Scr1,1)
         lcore = lcore + 2*sysbuf
         GOTO icopy
 160     DO
            ip1 = Lama
            CALL mesage(-2,ip1,nam)
         ENDDO
 180     ip1 = Ipout
         CALL mesage(-2,ip1,nam)
         GOTO 160
 200     CALL mesage(-3,Lama,nam)
 220     CALL mesage(-5,Mi,nam)
!
!
         ENTRY read5(Ipout)
!     ===================
!
!     PUT OUT EIGENVALUE SUMMARY IN CASE NO EIGENVALUES FOUND
!
         lcore = korsz(core) - sysbuf
         istor = 0
         jstor = 0
         imsg = 0
         xmax1 = 0.
         ix(2) = 1
         DO i = 3 , 7
            ix(i) = 0
         ENDDO
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         CALL bckrec(Lama)
         CALL close(Lama,2)
         CALL gopen(Lama,core(lcore+1),3)
         SPAG_Loop_1_2: DO i = 1 , nlama
            IF ( core(3)==refreq ) EXIT SPAG_Loop_1_2
            t2 = core(2)
            t3 = core(3)
            t4 = core(4)
            t5 = core(5)
            t6 = core(6)
            t7 = core(7)
            DO j = 2 , nlama
               k = 7*(j-2)
               core(k+2) = core(k+9)
               core(k+3) = core(k+10)
               core(k+4) = core(k+11)
               core(k+5) = core(k+12)
               core(k+6) = core(k+13)
               core(k+7) = core(k+14)
            ENDDO
            k = 7*(nlama-1)
            core(k+2) = t2
            core(k+3) = t3
            core(k+4) = t4
            core(k+5) = t5
            core(k+6) = t6
            core(k+7) = t7
         ENDDO SPAG_Loop_1_2
         CALL write(Lama,core(1),nwords,1)
         spag_nextblock_1 = 7
      CASE (7)
         CALL close(Lama,1)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE read2
