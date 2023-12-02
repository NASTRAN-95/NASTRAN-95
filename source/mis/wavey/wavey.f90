!*==wavey.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE wavey(Ig,Ild,New,Nc,Ic,Kact,Maxb,Maxw,Averw,Sumw,Rms,Brms,Jg)
   USE c_bands
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Ild
   INTEGER , DIMENSION(1) :: New
   INTEGER :: Nc
   INTEGER , DIMENSION(1) :: Ic
   INTEGER , DIMENSION(1) :: Kact
   INTEGER :: Maxb
   INTEGER :: Maxw
   REAL :: Averw
   INTEGER :: Sumw
   REAL :: Rms
   REAL :: Brms
   INTEGER , DIMENSION(1) :: Jg
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ann , wave
   REAL(REAL64) :: bsumsq , sumsq
   INTEGER :: i , ib , ib1 , iwave , j , k , kt , l , m
   EXTERNAL bunpak
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     COMPUTE WAVEFRONT AND ACTIVE COLUMN DATA -
!     MAXIMUM WAVEFRONT, AVERAGE WAVEFRONT, SUM OF ROW WAVEFRONTS,
!     SUM OF SQUARES OF ROW WAVEFRONTS, RMS WAVEFRONT, AND BANDWIDTH,
!     RMS BANDWIDTH, AND MINIMUM NODAL DEGREE.
!     DIAGONAL TERMS ARE INCLUDED.
!
!     IG     = CONNECTION TABLE
!     ILD(I) = NEW LABEL FOR NODE WITH ORIGINAL INTERNAL LABEL I
!     NEW(I) = INTERNAL LABEL CORRESPONDING TO NEW LABEL I
!              NEW AND ILD ARE INVERSES OF EACH OTHER
!     NC     = COMPONENT ID
!              IF NC.LE.0, USE ALL COMPONENTS.
!     IC(I)  = COMPONENT INDEX FOR ORIGINAL NODE I.
!     KACT(I)= LIST OF ACTIVE COLUMN FLAGS (UPDATED FOR EACH ROW)
!            = 1 IF COL I IS ACTIVE AT GIVEN ROW
!     MAXB   = BANDWIDTH
!     MAXW   = MAXIMUM WAVEFRONT
!     AVERW  = AVERAGE WAVEFRONT
!     SUMW   = SUM OF ROW WAVEFRONTS
!     SUMSQ  = SUM OF SQUARES OF ROW WAVEFRONTS
!     BSUMSQ = SUM OF SQUARES OF ROW BANDWIDTHS
!     RMS    = RMS WAVEFRONT
!     BRMS   = RMS BANDWIDTH
!     JG     = SCRATCH SPACE FOR BUNPAK
!     NN     = NUMBER OF NODES
!     MM     = MAX NODAL DEGREE
!     MINDEG = MINIMUM NODAL DEGREE
!
!     INPUT  - IG,ILD,NN,MM,NC,IC.
!     OUTPUT - NEW,KACT,MAXW,AVERW,SUMW,RMS,MAXB,BRMS,MINDEG
!
!
!     INITIALIZE WAVEFRONT DATA.
!
   Maxb = 0
   Maxw = 0
   Sumw = 0
   sumsq = 0.D0
   bsumsq = 0.D0
   Averw = 0.
   Rms = 0.
   mindeg = min0(mindeg,mm)
   IF ( nn*mm<=0 ) RETURN
!
!     INITIALIZE NEW, THE INVERSE OF ILD
!
   IF ( Nc<=0 ) THEN
      DO i = 1 , nn
         k = Ild(i)
         IF ( k>0 ) New(k) = i
      ENDDO
   ENDIF
!
!     INITIALIZE ACTIVE COLUMN FLAGS (1 FOR ACTIVE)
!
   DO i = 1 , nn
      Kact(i) = 0
   ENDDO
!
!     COMPUTE WAVEFRONT DATA.
!
   iwave = 1
   kt = 0
   DO i = 1 , nn
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
!
!     COMPUTE NUMBER OF ACTIVE COLUMNS FOR ROW I
!
            k = New(i)
            IF ( Nc>0 ) THEN
               IF ( k<=0 ) CYCLE
               IF ( Nc/=Ic(k) ) CYCLE
            ENDIF
            kt = kt + 1
            CALL bunpak(Ig,k,mm,Jg)
            ib = 0
            DO j = 1 , mm
               l = Jg(j)
               IF ( l==0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               m = Ild(l)
               ib = max0(ib,i-m)
               IF ( m>i ) THEN
                  IF ( Kact(m)/=1 ) THEN
                     iwave = iwave + 1
                     Kact(m) = 1
                  ENDIF
               ENDIF
            ENDDO
            spag_nextblock_1 = 3
         CASE (2)
            mindeg = min0(mindeg,j-1)
            spag_nextblock_1 = 3
         CASE (3)
!
!     IB1 = ROW BANDWIDTH FOR ROW I (DIAGONAL INCLUDED)
!
            ib1 = ib + 1
            Maxb = max0(Maxb,ib1)
            IF ( Kact(i)==1 ) iwave = iwave - 1
!
!     IWAVE = CURRENT NUMBER OF ACTIVE COLUMNS FOR ROW I
!             (DIAGONAL INCLUDED)
!
            Maxw = max0(Maxw,iwave)
            Sumw = Sumw + iwave
            wave = float(iwave)
            sumsq = sumsq + wave*wave
            wave = float(ib1)
            bsumsq = bsumsq + wave*wave
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
!
   ENDDO
!
   ann = float(kt)
   Averw = float(Sumw)/ann
   Rms = sqrt(sngl(sumsq)/ann)
   Brms = sqrt(sngl(bsumsq)/ann)
END SUBROUTINE wavey
