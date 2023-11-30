
SUBROUTINE frrd1c(Frl,Frqset,Mdd,Bdd,Kdd,Ifr,Ull,Lll,Scr1,Scr2,Scr3,Scr4,Igood)
   IMPLICIT NONE
   DOUBLE PRECISION Amcb(2) , Bmcb(2) , Cmcb(2) , Ddc , Ddr , Det(2) , Dett , Minda , Mindd
   INTEGER Chlsky , Fa(7) , Fl(7) , Fu(7) , Ia(7) , Ib , Ibbar , Icbr(3) , Icore(1) , Iib , Iibb , Il(7) , Ipow , Iprec , Iscr1 ,   &
         & Iscr2 , Iscr3 , Iu(7) , Ksystm(63) , Lcore , M1fil , M2fil , M3fil , Mcba(12) , Mcbb(12) , Mcbc(12) , Mcbd(12) , Mcbe(12)&
         & , Mcore(1) , Mfa(7) , Mfc(7) , Mfl(7) , Mindia , Mx(7) , Mxx , Nomat , Nout , Nx , Ny , Sr1 , Sr2 , Sr3 , Sysbuf
   REAL Core(1) , Power , Powr
   CHARACTER*23 Ufm
   COMMON /cdcmpx/ Fa , Fl , Fu , Sr1 , Sr2 , Sr3 , Det , Powr , Nx , Minda , Ib , Ibbar
   COMMON /dcompx/ Ia , Il , Iu , Iscr1 , Iscr2 , Iscr3 , Dett , Ipow , Ny , Mindia , Iib , Iibb , Icbr
   COMMON /saddx / Nomat , Lcore , Mcba , Mcbb , Mcbc , Mcbd , Mcbe , Mx
   COMMON /sfact / Mfa , Mfl , Mfc , M1fil , M2fil , Mxx , Ddr , Ddc , Power , M3fil , Mindd , Chlsky
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Core
   INTEGER Bdd , Frl , Frqset , Ifr , Igood , Kdd , Lll , Mdd , Scr1 , Scr2 , Scr3 , Scr4 , Ull
   INTEGER i , mx4a , mx4b , mx4c , name(2) , nz
   INTEGER korsz
   REAL w
!                                         (A)      (B)        (C)
!     THIS ROUTINE FORMS AND DECOMPOSES   KDD + I*W*BDD - W**2*MDD
!     WHERE  W = OMEGA, CYCLIC FREQ. AND I = SQUARE ROOT MINUS ONE
!
!     THE DECOMPOSITION ROUTINES ARE CALLED ACCORDING TO THE FOLLOWING
!     TABLE AS DETERMINED BY THE MATRIX RESULTING FROM THE ADDITION
!
!     IF MATRIX IS     COMPLEX SYMMETRIC    CALL SDCOMP
!                              UNSYMMETRIC  CALL CDCOMP
!                      REAL    SYMMETRIC    CALL SDCOMP
!                              UNSYMMETRIC  CALL DECOMP
!
   !>>>>EQUIVALENCE (Mcore(1),Core(1))
   !>>>>EQUIVALENCE (Icore(1),Core(1))
   !>>>>EQUIVALENCE (Amcb(1),Mcba(9)) , (Bmcb(1),Mcbb(9)) , (Cmcb(1),Mcbc(9)) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec)
   DATA name/4HFRRD , 4H1C  /
!
!
   Nx = korsz(Core)
   nz = Nx - Sysbuf
!
!     PICK UP CURRENT FREQUENCY
!
   CALL gopen(Frl,Core(nz+1),0)
   CALL skprec(Frl,Frqset-1)
   CALL fread(Frl,Core,Ifr,1)
   w = Core(Ifr)
   CALL close(Frl,1)
!
!     ADD MATRICES TOGETHER
!
   Mcba(1) = Kdd
   Mcbb(1) = Bdd
   Mcbc(1) = Mdd
   CALL rdtrl(Mcba)
   CALL rdtrl(Mcbb)
   CALL rdtrl(Mcbc)
   IF ( Mcba(1)<=0 .OR. Mcbc(1)<=0 ) THEN
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,', EITHER STIFFNESS MATRIX OR MASS MATRIX IS MISSING')
      CALL mesage(-37,0,name)
   ENDIF
!
   Mcba(8) = 2
   Mcbb(8) = 4
   Mcbc(8) = 2
   Amcb(1) = 1.0D0
   Amcb(2) = 0.0D0
   Bmcb(1) = 0.0D0
   Bmcb(2) = w
   Cmcb(1) = -w*w
   Cmcb(2) = 0.0D0
   IF ( Mcbb(1)<=0 ) THEN
!
!     NO BDD TO BE ADDED
!
      Mcbb(1) = 0
      Mcbb(8) = 0
      Bmcb(2) = 0.0D0
   ENDIF
!
   Mx(1) = Scr3
   Mx(2) = Mcba(2)
   Mx(3) = Mcba(3)
   mx4a = 6
   mx4b = 6
   mx4c = 6
   IF ( Mcba(1)>0 ) mx4a = Mcba(4)
   IF ( Mcbb(1)>0 ) mx4b = Mcbb(4)
   IF ( Mcbc(1)>0 ) mx4c = Mcbc(4)
   Mx(4) = min0(mx4a,mx4b,mx4c)
   Mx(5) = 2 + Iprec
   IF ( Mcba(1)<=0 .OR. Mcba(5)<=2 ) THEN
      IF ( Mcbb(1)<=0 ) THEN
         IF ( Mcbc(1)<=0 .OR. Mcbc(5)<=2 ) Mx(5) = Iprec
      ENDIF
   ENDIF
   Lcore = Nx
   Nomat = 3
   CALL sadd(Core,Core)
   CALL wrttrl(Mx)
!
!     SET UP TO DECOMPOSE MATRICES
!
   Fa(1) = Scr3
   CALL rdtrl(Fa)
   Igood = 1
   IF ( Fa(4)==6 ) THEN
!
!     USE SDCOMP TO PERFORM DECOMPOSITION
!
      Mfa(1) = Scr3
      Mfl(1) = Lll
      Mfc(1) = Ull
      DO i = 2 , 7
         Mfa(i) = Fa(i)
         Mfl(i) = Fa(i)
         Mfc(i) = Fa(i)
      ENDDO
      Mfl(4) = 4
      M1fil = Scr1
      M2fil = Scr2
      M3fil = Scr4
      Mxx = korsz(Mcore)
      Chlsky = 0
      CALL sdcomp(*200,Mcore,Mcore,Mcore)
      Igood = 0
!
!     DIRECTION FOR FRRD1D TO USE  FBS RATHER THAN GFBS
!
      Ull = -iabs(Ull)
!
      CALL wrttrl(Mfl)
   ELSEIF ( Fa(5)<=2 ) THEN
!
!     USE DECOMP TO PERFORM DECOMPOSITION
!
      Ia(1) = Scr3
      Il(1) = Lll
      Iu(1) = Ull
      DO i = 2 , 7
         Ia(i) = Fa(i)
         Il(i) = Fa(i)
         Iu(i) = Fa(i)
      ENDDO
      Il(4) = 4
      Iu(4) = 5
      Iscr1 = Scr1
      Iscr2 = Scr2
      Iscr3 = Scr4
      Ny = korsz(Icore)
      CALL decomp(*200,Icore,Icore,Icore)
      CALL wrttrl(Il)
      CALL wrttrl(Iu)
      Igood = 0
      GOTO 99999
   ELSE
      Fl(1) = Lll
      Fu(1) = Ull
      DO i = 2 , 7
         Fl(i) = Fa(i)
         Fu(i) = Fa(i)
      ENDDO
      Fl(4) = 4
      Fu(4) = 5
      Sr1 = Scr1
      Sr2 = Scr2
      Sr3 = Scr4
      CALL cdcomp(*200,Core(1),Core(1),Core(1))
      Igood = 0
      CALL wrttrl(Fl)
      CALL wrttrl(Fu)
   ENDIF
!
!     FORCE RE-EVALUATION OF DECOMP PARAM IF W = 0.0
!
 100  IF ( w==0.0 ) THEN
      Ib = 0
      Ibbar = 0
   ENDIF
   RETURN
!
!     MATRIX SINGULR
!
 200  i = 5
   IF ( w/=0.0 ) i = -5
   CALL mesage(i,Scr3,name)
   GOTO 100
99999 RETURN
END SUBROUTINE frrd1c