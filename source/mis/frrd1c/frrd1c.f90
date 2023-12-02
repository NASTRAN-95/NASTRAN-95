!*==frrd1c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frrd1c(Frl,Frqset,Mdd,Bdd,Kdd,Ifr,Ull,Lll,Scr1,Scr2,Scr3,Scr4,Igood)
USE C_CDCMPX
USE C_DCOMPX
USE C_SADDX
USE C_SFACT
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Frl
   INTEGER :: Frqset
   INTEGER :: Mdd
   INTEGER :: Bdd
   INTEGER :: Kdd
   INTEGER :: Ifr
   INTEGER :: Ull
   INTEGER :: Lll
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Igood
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: amcb , bmcb , cmcb
   INTEGER :: i , iprec , mx4a , mx4b , mx4c , nout , nz , sysbuf
   INTEGER , DIMENSION(1) :: icore , mcore
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: w
   EXTERNAL cdcomp , close , decomp , fread , gopen , korsz , mesage , rdtrl , sadd , sdcomp , skprec , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         Nx = korsz(Core)
         nz = Nx - sysbuf
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
            WRITE (nout,99001) Ufm
99001       FORMAT (A23,', EITHER STIFFNESS MATRIX OR MASS MATRIX IS MISSING')
            CALL mesage(-37,0,name)
         ENDIF
!
         Mcba(8) = 2
         Mcbb(8) = 4
         Mcbc(8) = 2
         amcb(1) = 1.0D0
         amcb(2) = 0.0D0
         bmcb(1) = 0.0D0
         bmcb(2) = w
         cmcb(1) = -w*w
         cmcb(2) = 0.0D0
         IF ( Mcbb(1)<=0 ) THEN
!
!     NO BDD TO BE ADDED
!
            Mcbb(1) = 0
            Mcbb(8) = 0
            bmcb(2) = 0.0D0
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
         Mx(5) = 2 + iprec
         IF ( Mcba(1)<=0 .OR. Mcba(5)<=2 ) THEN
            IF ( Mcbb(1)<=0 ) THEN
               IF ( Mcbc(1)<=0 .OR. Mcbc(5)<=2 ) Mx(5) = iprec
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
            Mxx = korsz(mcore)
            Chlsky = 0
            CALL sdcomp(*20,mcore,mcore,mcore)
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
            Ny = korsz(icore)
            CALL decomp(*20,icore,icore,icore)
            CALL wrttrl(Il)
            CALL wrttrl(Iu)
            Igood = 0
            RETURN
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
            CALL cdcomp(*20,Core(1),Core(1),Core(1))
            Igood = 0
            CALL wrttrl(Fl)
            CALL wrttrl(Fu)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     FORCE RE-EVALUATION OF DECOMP PARAM IF W = 0.0
!
         IF ( w==0.0 ) THEN
            Ib = 0
            Ibbar = 0
         ENDIF
         RETURN
!
!     MATRIX SINGULR
!
 20      i = 5
         IF ( w/=0.0 ) i = -5
         CALL mesage(i,Scr3,name)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frrd1c
