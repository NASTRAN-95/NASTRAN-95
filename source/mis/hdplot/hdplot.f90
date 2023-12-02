!*==hdplot.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hdplot(Gplst,Nmax,Maxsf,Iopcor,Ib)
!
   USE c_blank
   USE c_hdptrs
   USE c_hdrec
   USE c_hdsc
   USE c_plothd
   USE c_pltscr
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   INTEGER :: Nmax
   INTEGER :: Maxsf
   INTEGER :: Iopcor
   INTEGER :: Ib
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL , SAVE :: debug
   INTEGER :: i , j , lintc , m , n , nc , nholes , nps
   INTEGER , DIMENSION(100) :: isys
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(29) :: ptrs
   REAL , DIMENSION(20) :: x , y , z
   EXTERNAL close , gopen , hdsket , line , mesage , read
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Isys(1),Skps) , (Ptrs(1),Xdum)
   DATA name/4HHDPL , 4HOT  /
   DATA debug/.FALSE./
!
!     CALL SSWTCH (47,J)
!     IF (J .EQ. 1) DEBUG = .TRUE.
!
!     SET MNE EQUAL TO THE MAXIMUM NUMBER OF EDGES IN ANY ONE POLYGON.
!
   mne = Nmax
!
!     MNP=MNE+2+2*NHOLES   WHERE NHOLES IS THE NUMBER OF HOLES,IF ANY
!
   nholes = 0
   mnp = mne + 2 + 2*nholes
!
!     SET DISTANCE FROM VIEWER, AND SET SCALING FACTOR = 1 UNITS/INCH
!
   dv = 99999.
   scf = 1.00
!
!     SET MAX. LINES OF INTERSECTION ALLOWED IN HDSOLV (DIMEN. OF XCC)
!
   lintc = 800
   IF ( isys(85)/=0 ) lintc = isys(85)
!
!     DEFINE EULERIAN ANGLES IN DEGREES.
!
   psi = 0.
   phi = 0.
   theta = 0.
!
!     INITIALIZE ARRAY POINTERS IN OPEN CORE SPACE (USED, SET BY PLOT,
!     IS NO. OF WORDS ALREADY IN USE)
!
   xdum = 1
   xcc = xdum + used
   xasolv = xcc + lintc
   yasolv = xasolv + 50
   zasolv = yasolv + 50
   x1skt = zasolv + 50
   y1skt = x1skt + 160
   z1skt = y1skt + 160
   zcoef1 = z1skt + 160
   zcoef = zcoef1 + 150
   icount = zcoef + 150
   irct = icount + 150
   x21 = irct + 100
   y21 = x21 + 200
   z21 = y21 + 200
   iia = z21 + 200
   xe = iia + 200
   ye = xe + 150
   xu = ye + 150
   yu = xu + 150
   ibeg = yu + 150
   iend = ibeg + 100
   ict = iend + 100
   icct = ict + 100
   xi = icct + 100
   icore = (25+5*mne+4*mnp)*(Maxsf+1)
   j = (Iopcor-icore-xi)/5
   yi = xi + j
   zi = yi + j
   di = zi + j
   work = di + j
   IF ( debug .OR. j<300 ) WRITE (iout,99001) Nmax , Maxsf , icore , used , lintc , Iopcor , Ib , nsets , j , ptrs
!
99001 FORMAT (1X,10HIN HDPLOT ,9I8,/,(5X,15I8))
   IF ( j<300 ) THEN
      j = 300*5 + xi + icore - Iopcor
      CALL mesage(-8,j,name)
   ENDIF
!
   CALL gopen(nscr2,Gplst(Ib),0)
   CALL line(0.,0.,0.,0.,1,-1)
   DO
      CALL read(*100,*100,nscr2,nofsur,44,0,m)
      nps = npers
      DO i = 1 , nps
         x(i) = p(1,i)
         y(i) = p(2,i)
         z(i) = p(3,i)
      ENDDO
      IF ( debug ) WRITE (iout,99002) nofsur , ns , elid , lid , nps , (x(n),y(n),z(n),n=1,nps)
99002 FORMAT (1X,5I10/(1X,3G20.4))
      nc = 0
      CALL hdsket(x,y,z,nps,nc)
   ENDDO
 100  CALL close(nscr2,1)
   nc = 1
   CALL hdsket(x,y,z,nps,nc)
   IF ( nc/=0 ) THEN
      WRITE (iout,99003) nc , icore , dv
99003 FORMAT (22H CODE FOR HIDDEN ERROR,I3,6H ICORE,I9,3H DV,F15.5)
   ENDIF
   CALL line(0.,0.,0.,0.,1,+1)
   IF ( debug ) WRITE (iout,99004)
99004 FORMAT (1X,10HOUT HDPLOT)
END SUBROUTINE hdplot
