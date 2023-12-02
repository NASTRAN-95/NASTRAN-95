!*==frrd1c.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frrd1c(Frl,Frqset,Mdd,Bdd,Kdd,Ifr,Ull,Lll,Scr1,Scr2,Scr3,Scr4,Igood)
   USE c_cdcmpx
   USE c_dcompx
   USE c_saddx
   USE c_sfact
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
         nx = korsz(core)
         nz = nx - sysbuf
!
!     PICK UP CURRENT FREQUENCY
!
         CALL gopen(Frl,core(nz+1),0)
         CALL skprec(Frl,Frqset-1)
         CALL fread(Frl,core,Ifr,1)
         w = core(Ifr)
         CALL close(Frl,1)
!
!     ADD MATRICES TOGETHER
!
         mcba(1) = Kdd
         mcbb(1) = Bdd
         mcbc(1) = Mdd
         CALL rdtrl(mcba)
         CALL rdtrl(mcbb)
         CALL rdtrl(mcbc)
         IF ( mcba(1)<=0 .OR. mcbc(1)<=0 ) THEN
            WRITE (nout,99001) ufm
99001       FORMAT (A23,', EITHER STIFFNESS MATRIX OR MASS MATRIX IS MISSING')
            CALL mesage(-37,0,name)
         ENDIF
!
         mcba(8) = 2
         mcbb(8) = 4
         mcbc(8) = 2
         amcb(1) = 1.0D0
         amcb(2) = 0.0D0
         bmcb(1) = 0.0D0
         bmcb(2) = w
         cmcb(1) = -w*w
         cmcb(2) = 0.0D0
         IF ( mcbb(1)<=0 ) THEN
!
!     NO BDD TO BE ADDED
!
            mcbb(1) = 0
            mcbb(8) = 0
            bmcb(2) = 0.0D0
         ENDIF
!
         mx(1) = Scr3
         mx(2) = mcba(2)
         mx(3) = mcba(3)
         mx4a = 6
         mx4b = 6
         mx4c = 6
         IF ( mcba(1)>0 ) mx4a = mcba(4)
         IF ( mcbb(1)>0 ) mx4b = mcbb(4)
         IF ( mcbc(1)>0 ) mx4c = mcbc(4)
         mx(4) = min0(mx4a,mx4b,mx4c)
         mx(5) = 2 + iprec
         IF ( mcba(1)<=0 .OR. mcba(5)<=2 ) THEN
            IF ( mcbb(1)<=0 ) THEN
               IF ( mcbc(1)<=0 .OR. mcbc(5)<=2 ) mx(5) = iprec
            ENDIF
         ENDIF
         lcore = nx
         nomat = 3
         CALL sadd(core,core)
         CALL wrttrl(mx)
!
!     SET UP TO DECOMPOSE MATRICES
!
         fa(1) = Scr3
         CALL rdtrl(fa)
         Igood = 1
         IF ( fa(4)==6 ) THEN
!
!     USE SDCOMP TO PERFORM DECOMPOSITION
!
            mfa(1) = Scr3
            mfl(1) = Lll
            mfc(1) = Ull
            DO i = 2 , 7
               mfa(i) = fa(i)
               mfl(i) = fa(i)
               mfc(i) = fa(i)
            ENDDO
            mfl(4) = 4
            m1fil = Scr1
            m2fil = Scr2
            m3fil = Scr4
            mxx = korsz(mcore)
            chlsky = 0
            CALL sdcomp(*20,mcore,mcore,mcore)
            Igood = 0
!
!     DIRECTION FOR FRRD1D TO USE  FBS RATHER THAN GFBS
!
            Ull = -iabs(Ull)
!
            CALL wrttrl(mfl)
         ELSEIF ( fa(5)<=2 ) THEN
!
!     USE DECOMP TO PERFORM DECOMPOSITION
!
            ia(1) = Scr3
            il(1) = Lll
            iu(1) = Ull
            DO i = 2 , 7
               ia(i) = fa(i)
               il(i) = fa(i)
               iu(i) = fa(i)
            ENDDO
            il(4) = 4
            iu(4) = 5
            iscr1 = Scr1
            iscr2 = Scr2
            iscr3 = Scr4
            ny = korsz(icore)
            CALL decomp(*20,icore,icore,icore)
            CALL wrttrl(il)
            CALL wrttrl(iu)
            Igood = 0
            RETURN
         ELSE
            fl(1) = Lll
            fu(1) = Ull
            DO i = 2 , 7
               fl(i) = fa(i)
               fu(i) = fa(i)
            ENDDO
            fl(4) = 4
            fu(4) = 5
            sr1 = Scr1
            sr2 = Scr2
            sr3 = Scr4
            CALL cdcomp(*20,core(1),core(1),core(1))
            Igood = 0
            CALL wrttrl(fl)
            CALL wrttrl(fu)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     FORCE RE-EVALUATION OF DECOMP PARAM IF W = 0.0
!
         IF ( w==0.0 ) THEN
            ib = 0
            ibbar = 0
         ENDIF
         RETURN
!
!     MATRIX SINGULR
!
 20      i = 5
         IF ( w/=0.0 ) i = -5
         CALL mesage(i,Scr3,name)
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frrd1c
