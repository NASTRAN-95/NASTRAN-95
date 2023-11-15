
SUBROUTINE hdplot(Gplst,Nmax,Maxsf,Iopcor,Ib)
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Di , Elid , Elset , G(3) , Ibeg , Icct , Icore , Icount , Ict , Idum(3) , Iend , Iia , Iout , Irct , Isys(100) , Lid ,   &
         & Merr , Mne , Mnp , Ngp , Nnn , Nofsur , Npers , Ns , Nscr1 , Nscr2 , Nscr3 , Nsets , Nsil , Ptrs(29) , Rz(1) , Skp1(7) , &
         & Skp2(2) , Skp22(7) , Skps , Used , Work , X1skt , X21 , Xasolv , Xcc , Xdum , Xe , Xi , Xu , Y1skt , Y21 , Yasolv , Ye , &
         & Yi , Yu , Z1skt , Z21 , Zasolv , Zcoef , Zcoef1 , Zi
   REAL Dv , P(3,13) , Phi , Psi , Scf , Theta
   COMMON /blank / Ngp , Nsil , Nsets , Skp1 , Skp2 , Elset , Skp22 , Merr , Idum , Nscr1 , Nscr2 , Nscr3
   COMMON /hdptrs/ Xdum , Xcc , Xasolv , Yasolv , Zasolv , X1skt , Y1skt , Z1skt , Zcoef1 , Zcoef , Icount , Irct , X21 , Y21 ,     &
                 & Z21 , Iia , Xe , Ye , Xu , Yu , Xi , Yi , Zi , Di , Ibeg , Iend , Ict , Icct , Work
   COMMON /hdrec / Nofsur , Ns , Elid , Lid , Npers , P
   COMMON /hdsc  / Scf , Psi , Phi , Theta , Mne , Dv , Mnp , Icore
   COMMON /plothd/ Used
   COMMON /pltscr/ Nnn , G
   COMMON /system/ Skps , Iout
   COMMON /zzzzzz/ Rz
!
! Dummy argument declarations
!
   INTEGER Ib , Iopcor , Maxsf , Nmax
   INTEGER Gplst(1)
!
! Local variable declarations
!
   LOGICAL debug
   INTEGER i , j , lintc , m , n , name(2) , nc , nholes , nps
   REAL x(20) , y(20) , z(20)
!
! End of declarations
!
   EQUIVALENCE (Isys(1),Skps) , (Ptrs(1),Xdum)
   DATA name/4HHDPL , 4HOT  /
   DATA debug/.FALSE./
!
!     CALL SSWTCH (47,J)
!     IF (J .EQ. 1) DEBUG = .TRUE.
!
!     SET MNE EQUAL TO THE MAXIMUM NUMBER OF EDGES IN ANY ONE POLYGON.
!
   Mne = Nmax
!
!     MNP=MNE+2+2*NHOLES   WHERE NHOLES IS THE NUMBER OF HOLES,IF ANY
!
   nholes = 0
   Mnp = Mne + 2 + 2*nholes
!
!     SET DISTANCE FROM VIEWER, AND SET SCALING FACTOR = 1 UNITS/INCH
!
   Dv = 99999.
   Scf = 1.00
!
!     SET MAX. LINES OF INTERSECTION ALLOWED IN HDSOLV (DIMEN. OF XCC)
!
   lintc = 800
   IF ( Isys(85)/=0 ) lintc = Isys(85)
!
!     DEFINE EULERIAN ANGLES IN DEGREES.
!
   Psi = 0.
   Phi = 0.
   Theta = 0.
!
!     INITIALIZE ARRAY POINTERS IN OPEN CORE SPACE (USED, SET BY PLOT,
!     IS NO. OF WORDS ALREADY IN USE)
!
   Xdum = 1
   Xcc = Xdum + Used
   Xasolv = Xcc + lintc
   Yasolv = Xasolv + 50
   Zasolv = Yasolv + 50
   X1skt = Zasolv + 50
   Y1skt = X1skt + 160
   Z1skt = Y1skt + 160
   Zcoef1 = Z1skt + 160
   Zcoef = Zcoef1 + 150
   Icount = Zcoef + 150
   Irct = Icount + 150
   X21 = Irct + 100
   Y21 = X21 + 200
   Z21 = Y21 + 200
   Iia = Z21 + 200
   Xe = Iia + 200
   Ye = Xe + 150
   Xu = Ye + 150
   Yu = Xu + 150
   Ibeg = Yu + 150
   Iend = Ibeg + 100
   Ict = Iend + 100
   Icct = Ict + 100
   Xi = Icct + 100
   Icore = (25+5*Mne+4*Mnp)*(Maxsf+1)
   j = (Iopcor-Icore-Xi)/5
   Yi = Xi + j
   Zi = Yi + j
   Di = Zi + j
   Work = Di + j
   IF ( debug .OR. j<300 ) WRITE (Iout,99001) Nmax , Maxsf , Icore , Used , lintc , Iopcor , Ib , Nsets , j , Ptrs
!
99001 FORMAT (1X,10HIN HDPLOT ,9I8,/,(5X,15I8))
   IF ( j<300 ) THEN
      j = 300*5 + Xi + Icore - Iopcor
      CALL mesage(-8,j,name)
   ENDIF
!
   CALL gopen(Nscr2,Gplst(Ib),0)
   CALL line(0.,0.,0.,0.,1,-1)
   DO
      CALL read(*100,*100,Nscr2,Nofsur,44,0,m)
      nps = Npers
      DO i = 1 , nps
         x(i) = P(1,i)
         y(i) = P(2,i)
         z(i) = P(3,i)
      ENDDO
      IF ( debug ) WRITE (Iout,99002) Nofsur , Ns , Elid , Lid , nps , (x(n),y(n),z(n),n=1,nps)
99002 FORMAT (1X,5I10/(1X,3G20.4))
      nc = 0
      CALL hdsket(x,y,z,nps,nc)
   ENDDO
 100  CALL close(Nscr2,1)
   nc = 1
   CALL hdsket(x,y,z,nps,nc)
   IF ( nc/=0 ) THEN
      WRITE (Iout,99003) nc , Icore , Dv
99003 FORMAT (22H CODE FOR HIDDEN ERROR,I3,6H ICORE,I9,3H DV,F15.5)
   ENDIF
   CALL line(0.,0.,0.,0.,1,+1)
   IF ( debug ) WRITE (Iout,99004)
99004 FORMAT (1X,10HOUT HDPLOT)
   RETURN
END SUBROUTINE hdplot
