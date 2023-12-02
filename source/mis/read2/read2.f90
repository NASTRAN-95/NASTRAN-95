!*==read2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE read2(Maa,Phia,Scr1,Norm,Ia,Uset,Mi,Lama,Ipout,Scr2,Epsi,Scr3)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_GIVN
   USE C_OUTPUT
   USE C_PACKX
   USE C_STURMX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
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
      ASSIGN 1100 TO icopy
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
      ASSIGN 100 TO icopy
   ENDIF
   GOTO 1200
!
!
!     PROCESS PHIA - NORMALIZE - COPY TO PHIA
!
 100  lcore = lcore - Sysbuf
   CALL gopen(Scr1,core(lcore+1),0)
   lcore = lcore - Sysbuf
   CALL gopen(Phia,core(lcore+1),1)
   Itb = ix(5)
   Jj = nrow
   Ii = 1
   Incur = 1
   Ita1 = Itb
   Itb1 = Itb
   Incur1 = 1
   DO i = 1 , ncol
      CALL unpack(*200,Scr1,core(3))
      Ii1 = Ii
      Jj1 = Jj
      jjj = 1
      IF ( Itb==2 ) THEN
         DO j = 1 , nrow
            IF ( dabs(dcore(j+1))>dabs(dcore(jjj+1)) ) jjj = j
         ENDDO
         jjj = jjj + 1
         IF ( ipont==1 ) THEN
            jjj = Ia + 1
            IF ( dabs(dcore(jjj))<=1.0D-15 ) GOTO 150
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
            IF ( abs(core(jjj))<=1.0E-15 ) GOTO 150
         ENDIF
         xmax = core(jjj)
         DO j = 1 , nrow
            core(j+2) = core(j+2)/xmax
         ENDDO
      ENDIF
 150  CALL pack(core(3),Phia,iphia)
      CYCLE
 200  Ii1 = 1
      Jj1 = 1
      CALL pack(core,Phia,iphia)
   ENDDO
   CALL close(Phia,1)
   CALL close(Scr1,1)
!
!     COMPUTE MODAL MASS
!
 300  IF ( imi<0 ) GOTO 500
   IF ( iden==0 ) THEN
!
      CALL ssg2b(Maa,Phia,0,Scr2,0,Itb,1,Scr3)
      CALL ssg2b(Phia,Scr2,0,Mi,1,Itb,1,Scr3)
      GOTO 500
   ELSE
      ASSIGN 400 TO icopy
      GOTO 1200
   ENDIF
 400  CALL ssg2b(Phia,Scr1,0,Mi,1,Itb,1,Scr3)
!
!     COMPUTE GENERALIZED STIFFNESS
!
!
!     COMPUTE FREQUENCY ETC
!
 500  Itb = 1
   Ii = 1
   Jj = ncol
   Incur = 1
   imsg = 0
   CALL gopen(Lama,core(lcore+1),0)
   CALL read(*1300,*600,Lama,core(1),lcore,1,nlama)
   CALL mesage(-8,0,nam)
   GOTO 1500
!
!     NLAMA IS THE NUMBER OF EIGENVALUES FOUND   NCOL IS TH NUMBER OF
!     VECTORS
!
!
!     BRING IN THE ORDER FOUND
!
 600  kk = nlama + 2*ncol + 8
!
!     KK IS THE POINTER TO THE ORDER FOUND
!     L1 AND  L2 ARE COUNTERS FOR MISSING LOW FREQ. BELOW SHIFT POINTS
!     STURM AND KEEP WERE SAVED IN SDCOMP, SHFTPT AND PTSHFT IN FEER
!     AND INVPWR (REAL SYMMETRIC EIGENVALUE PROBLEM ONLY)
!
   CALL read(*1300,*700,Lama,Icore(kk+1),lcore,1,iflag)
   CALL mesage(-8,0,nam)
   GOTO 1500
 700  CALL close(Lama,1)
   CALL gopen(Lama,core(lcore+1),1)
   CALL write(Lama,ihead(1),50,0)
   CALL write(Lama,Head(1),96,1)
   lcore = lcore + Sysbuf
   core(nlama+6) = 0.0
   core(nlama+7) = 0.0
   IF ( imi>=0 ) THEN
      CALL gopen(Mi,core(lcore+1),0)
      l1 = Sturm
      l2 = Keep
      Shftpt = Shftpt + 1.E-10
      Ptshft = Ptshft + 1.E-10
   ENDIF
   DO i = 1 , nlama
      Icore(nlama+1) = i
      l = kk + i
      Icore(nlama+2) = Icore(l)
      core(nlama+3) = core(i)
      core(nlama+4) = sqrt(abs(core(i)))
      core(nlama+5) = core(nlama+4)/tphi
      IF ( core(i)>1.E-10 .AND. core(i)<=Shftpt ) l1 = l1 - 1
      IF ( core(i)>1.E-10 .AND. core(i)<=Ptshft ) l2 = l2 - 1
      IF ( imi<0 ) GOTO 800
      IF ( i<=ncol ) THEN
         l = nlama + i + 7
         k = l - 1 + i
         CALL unpack(*750,Mi,core(l))
         core(nlama+6) = core(k)
         core(nlama+7) = core(k)*core(nlama+3)
         core(l) = core(k)
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
         GOTO 800
      ENDIF
!
!     NO MORE VECTORS
!     REPLACE STURM BY SMALLER OF L1 OR L2, IF NOT ALL LOWER MODES FOUND
!     SET STRUM TO   -1 IF THERE IS NOT ENOUGH INFORMATION,
!     SET STRUM TO -999 IF DIAG 37 IS REQUESTED (NOT TO PRINT MESSAGE).
!
 750  core(nlama+6) = 0.0
      core(nlama+7) = 0.0
 800  CALL write(Lama,core(nlama+1),7,0)
   ENDDO
   IF ( l1<0 ) l1 = 0
   IF ( l2<0 ) l2 = 0
   IF ( l1>l2 ) l1 = l2
   IF ( Sturm/=-1 .AND. l1>=0 ) Sturm = l1
   IF ( Sturm>Nr .AND. Nr>0 ) Sturm = Sturm - Nr
   IF ( Keep<=0 .AND. Ptshft>0. ) Sturm = -1
   CALL sswtch(37,j)
   IF ( j==1 ) Sturm = -999
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
         CALL unpack(*1600,Mi,core(mcol))
         IF ( core(m)/=0 ) THEN
            DO j = 1 , ncol
               IF ( i==j ) EXIT
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
            ENDDO
         ENDIF
      ENDDO
!
      CALL close(Mi,1)
      IF ( imsg/=0 ) CALL mesage(34,xmax,Epsi)
   ENDIF
   IF ( Givens/=.0 ) THEN
      IF ( Nnv==0 ) THEN
         IF ( Lfreq>.0 ) THEN
!
!     REARRANGE THE EIGENVALUE TABLE, IF NECESSARY, FOR GIVENS METHOD
!
            CALL gopen(Lama,core(lcore+1),0)
            CALL skprec(Lama,1)
            nwords = 7*nlama
            CALL read(*1300,*1500,Lama,core(1),nwords,1,nwrds)
            refreq = core(3)
            DO i = 2 , nlama
               j = 7*(i-1) + 3
               IF ( core(j)<refreq ) THEN
                  refreq = core(j)
                  GOTO 1700
               ENDIF
            ENDDO
            GOTO 1800
         ENDIF
      ENDIF
   ENDIF
 900  CALL gopen(Ipout,core(lcore+1),0)
   CALL read(*1400,*1000,Ipout,core(1),lcore,1,iflag)
   CALL mesage(-8,0,nam)
   GOTO 1500
 1000 CALL close(Ipout,1)
   CALL gopen(Ipout,core(lcore+1),1)
   ihead1(3) = Icore(1)
   CALL write(Ipout,ihead1,10,0)
   i0 = 0
   core(i0+9) = xmax1
   Icore(i0+10) = istor
   Icore(i0+11) = jstor
   Icore(i0+12) = imsg
   Icore(i0+13) = Sturm
   CALL write(Ipout,core(2),40,0)
   CALL write(Ipout,Head,96,1)
   IF ( Icore(1)==1 ) THEN
      iflag = iflag - 12
      ihead1(3) = 3
      ihead1(10) = 6
      CALL write(Ipout,ihead1,50,0)
      CALL write(Ipout,Head,96,1)
      IF ( iflag/=0 ) CALL write(Ipout,core(13),iflag,0)
   ENDIF
   CALL close(Ipout,1)
   ix(1) = Ipout
   CALL wrttrl(ix)
   RETURN
 1100 IF ( iden==0 ) THEN
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
   lcore = lcore - Sysbuf
   CALL gopen(Mi,core(lcore+1),0)
   Itb = iphia(5)
   Ii = 1
   Jj = ncol
   IF ( Itb/=2 ) THEN
      DO j = 1 , ncol
         CALL unpack(*1120,Mi,core(ncol+1))
         k = ncol + j
         core(j) = 1.0/sqrt(abs(core(k)))
         CYCLE
 1120    core(j) = 0.0
      ENDDO
   ELSE
      DO j = 1 , ncol
         CALL unpack(*1140,Mi,dcore(ncol+1))
         k = ncol + j
         dcore(j) = 1.0D0/dsqrt(dabs(dcore(k)))
         CYCLE
 1140    dcore(j) = 0.0D0
      ENDDO
   ENDIF
   CALL close(Mi,1)
!
!     DIVIDE EACH TERM BY SQRT (MI)
!
   CALL gopen(Scr1,core(lcore+1),0)
   lcore = lcore - Sysbuf
   CALL gopen(Phia,core(lcore+1),1)
   Ii = 1
   Jj = nrow
   Incur = 1
   Ita1 = Itb
   Itb1 = Itb
   ncol2 = Itb*ncol
   nrow2 = Itb*nrow
   Ii1 = 1
   Jj1 = nrow
   Incur1 = 1
   DO i = 1 , ncol
      CALL unpack(*1150,Scr1,core(ncol2+1))
      IF ( Itb/=2 ) THEN
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
 1150 DO j = 1 , nrow2
         k = ncol2 + j
         core(k) = 0.0
      ENDDO
      CALL pack(core(ncol2+1),Phia,iphia)
   ENDDO
   CALL close(Phia,1)
   CALL close(Scr1,1)
   GOTO 300
!
!     COPY ROUTINE - PHIA TO SCR1
!
 1200 lcore = lcore - Sysbuf
   CALL gopen(Phia,core(lcore+1),0)
   lcore = lcore - Sysbuf
   CALL gopen(Scr1,core(lcore+1),1)
   dcore(1) = 0.0D+0
   Itb = ix(5)
   Ita1 = Itb
   Itb1 = Itb
   Incur = 1
   Incur1 = 1
   DO jjj = 1 , ncol
      Ii = 0
      CALL unpack(*1250,Phia,core(3))
      Ii1 = Ii
      Jj1 = Jj
      CALL pack(core(3),Scr1,iphia)
      CYCLE
 1250 Ii1 = 1
      Jj1 = 1
      CALL pack(core,Scr1,iphia)
   ENDDO
   CALL close(Phia,1)
   CALL close(Scr1,1)
   lcore = lcore + 2*Sysbuf
   GOTO icopy
 1300 DO
      ip1 = Lama
      CALL mesage(-2,ip1,nam)
   ENDDO
 1400 ip1 = Ipout
   CALL mesage(-2,ip1,nam)
   GOTO 1300
 1500 CALL mesage(-3,Lama,nam)
 1600 CALL mesage(-5,Mi,nam)
!
!
   ENTRY read5(Ipout)
!     ===================
!
!     PUT OUT EIGENVALUE SUMMARY IN CASE NO EIGENVALUES FOUND
!
   lcore = korsz(core) - Sysbuf
   istor = 0
   jstor = 0
   imsg = 0
   xmax1 = 0.
   ix(2) = 1
   DO i = 3 , 7
      ix(i) = 0
   ENDDO
   GOTO 900
 1700 CALL bckrec(Lama)
   CALL close(Lama,2)
   CALL gopen(Lama,core(lcore+1),3)
   DO i = 1 , nlama
      IF ( core(3)==refreq ) EXIT
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
   ENDDO
   CALL write(Lama,core(1),nwords,1)
 1800 CALL close(Lama,1)
   GOTO 900
END SUBROUTINE read2
