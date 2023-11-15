
SUBROUTINE frrd1d(Pd,Ull,Lll,Scr1,Scr2,Udv,Ifr,Nload,Igood,Nfreq)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1)
   INTEGER Fb(7) , Fl(7) , Fu(7) , Fx(7) , Icore(1) , Ii , Ii1 , Incr , Incr1 , Iprec , Iscrx , Isign , It1 , It2 , It3 , Jj , Jj1 ,&
         & Ksystm(65) , Mach , Mcore(1) , Mfb(7) , Mfl(7) , Mflt(7) , Mfx(7) , Mprec , Msign , Mx , Nx , Prec , Sysbuf
   COMMON /fbsx  / Mfl , Mflt , Mfb , Mfx , Mx , Mprec , Msign , Iscrx
   COMMON /gfbsx / Fl , Fu , Fb , Fx , Nx , Prec , Isign
   COMMON /machin/ Mach
   COMMON /packx / It2 , It3 , Ii1 , Jj1 , Incr1
   COMMON /system/ Ksystm
   COMMON /unpakx/ It1 , Ii , Jj , Incr
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Ifr , Igood , Lll , Nfreq , Nload , Pd , Scr1 , Scr2 , Udv , Udv1 , Ull
!
! Local variable declarations
!
   INTEGER file , i , ic , iflag , m , mcb(7) , name(2) , nz
   INTEGER korsz
!
! End of declarations
!
!
!     ROUTINE SOLVES FOR UDV GIVEN ULL,LLL, AND PD
!
!     IF IGOOD = 1 DCOMP FAILED -- PUT ZERO SOLUTION VECTORS OUT
!
!     1. PULL LOADS FROM PD ONTO SCR1
!     2. SOLVE FOR UDV-S ON SCR2
!     3. STACK SOLVED LOADS ON UDV
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Core(1),Icore(1),Mcore(1))
   DATA name/4HFRRD , 4H1D  / , ic/0/
!
   Nx = korsz(Core)
   Fb(1) = Pd
   CALL rdtrl(Fb)
   Fx(1) = Scr2
   IF ( Ifr==1 ) Fx(1) = Udv
   Fx(2) = Nload
   Fx(3) = Fb(3)
   Fx(4) = 2
   Fx(5) = 2 + Iprec
   It1 = Fb(5)
   Incr = 1
   Incr1 = 1
   It2 = It1
   It3 = 2 + Iprec
   IF ( Igood/=1 ) THEN
!
!     PULL LOADS FROM PD ONTO SCR1
!
      Fu(1) = Ull
      CALL rdtrl(Fu)
      Fl(1) = Lll
      CALL rdtrl(Fl)
      IF ( Nfreq/=1 ) THEN
         nz = Nx - Sysbuf
         CALL gopen(Pd,Core(nz+1),0)
         CALL skprec(Pd,Ifr-1)
         nz = nz - Sysbuf
         CALL gopen(Scr1,Core(nz+1),1)
         CALL makmcb(mcb,Scr1,Fb(3),2,It3)
         DO i = 1 , Nload
            IF ( i>1 ) CALL skprec(Pd,Nfreq-1)
            Ii = 0
            CALL unpack(*20,Pd,Core)
            Ii1 = Ii
            Jj1 = Jj
 10         CALL pack(Core,Scr1,mcb)
            CYCLE
 20         Core(1) = 0
            Core(ic+2) = 0
            Core(ic+3) = 0
            Core(ic+4) = 0
            Ii1 = 1
            Jj1 = 1
            GOTO 10
         ENDDO
         CALL wrttrl(mcb)
         CALL close(Pd,1)
         CALL close(Scr1,1)
!
!     SET UP FOR GFBS
!
         Fb(1) = Scr1
      ENDIF
      Fb(2) = Nload
      CALL wrttrl(Fb)
      Prec = 1
      IF ( Fb(5)==2 .OR. Fb(5)==4 ) Prec = 2
      Isign = 1
      IF ( Fu(1)<0 ) THEN
!
!     SET UP FOR FBS
!
         DO i = 1 , 7
            Mfl(i) = Fl(i)
!
!     FBS DOES NOT USE THE MATRIX CONTROL BLOCK MFLT.
!     IF MFLT(1) EXISTS, SET ISCRX = MFLT(1) FILE FOR NEW FBS METHOD.
!     OTHERWISE SET ISCRX = 0, AND WE DO NOT HAVE A SCRATCH FILE FOR
!     NEW FBS. OLD FBS WILL BE USED.
!
            Mfb(i) = Fb(i)
            Mfx(i) = Fx(i)
         ENDDO
         Mprec = Prec
         Msign = Isign
         Mx = korsz(Mcore)
         Iscrx = Mflt(1)
         Mcore(1) = Mflt(1)
         CALL rdtrl(Mcore(1))
         IF ( Mcore(1)<=0 ) Iscrx = 0
         CALL fbs(Mcore,Mcore)
         CALL wrttrl(Mfx)
      ELSE
         CALL gfbs(Core,Core)
         CALL wrttrl(Fx)
      ENDIF
   ENDIF
   Icore(1) = 16777215
!                16777215 = '00FFFFFF'X
   iflag = 1
!
!     STACK LOADS ONTO UDV
!
   file = Udv
   nz = Nx - Sysbuf
   IF ( Ifr==1 ) THEN
!
      IF ( Igood/=1 ) GOTO 400
      CALL gopen(Udv,Core(nz+1),1)
      Fx(2) = 0
      Fx(6) = 0
      Fx(7) = 0
      CALL wrttrl(Fx)
      GOTO 300
   ELSE
      CALL open(*500,Udv,Core(nz+1),0)
      Fx(1) = Udv
      CALL rdtrl(Fx)
      IF ( Mach/=1 ) THEN
         CALL skpfil(Udv,1)
         CALL skpfil(Udv,-1)
         GOTO 200
      ELSE
         DO
            CALL fwdrec(*100,Udv)
         ENDDO
      ENDIF
   ENDIF
 100  CALL bckrec(Udv)
   CALL skprec(Udv,1)
 200  CALL close(Udv,2)
   CALL open(*500,Udv,Core(nz+1),3)
!
!     RESET TYPE FLAGS
!
   It1 = Fx(5)
   It2 = It1
   It3 = It1
   IF ( Igood/=1 ) THEN
      nz = nz - Sysbuf
      CALL gopen(Scr2,Core(nz+1),0)
   ENDIF
 300  DO i = 1 , Nload
      IF ( Igood/=1 ) THEN
         Ii = 0
         CALL unpack(*350,Scr2,Core)
         Ii1 = Ii
         Jj1 = Jj
         CALL pack(Core,Udv,Fx)
         CYCLE
      ENDIF
 350  Core(1) = 0
      Core(ic+2) = 0
      Core(ic+3) = 0
      Core(ic+4) = 0
      Ii1 = 1
      Jj1 = 1
      CALL pack(Core,Udv,Fx)
   ENDDO
   CALL close(Udv,1)
   IF ( Igood/=1 ) CALL close(Scr2,1)
   CALL wrttrl(Fx)
 400  RETURN
!
!     ERROR MESAGES
!
 500  CALL mesage(-1,file,name)
!
!
   ENTRY frrd1e(Udv1,Udv,Nload,Nfreq)
!     ===================================
!
   nz = korsz(Core) - Sysbuf
!
!     ROUTINE REORDERS SOLUTIONS TO GET SORT BY LOADS
!
   file = Udv1
   CALL open(*500,Udv1,Core(nz+1),0)
   nz = nz - Sysbuf
   CALL gopen(Udv,Core(nz+1),1)
   file = Udv1
   DO i = 1 , Nload
      CALL skprec(Udv1,i)
      DO m = 1 , Nfreq
         Ii = 0
         CALL unpack(*540,Udv1,Core)
         Ii1 = Ii
         Jj1 = Jj
 520     CALL pack(Core,Udv,mcb)
         IF ( m<Nfreq ) CALL skprec(Udv1,Nload-1)
         CYCLE
 540     Core(1) = 0
         Core(ic+2) = 0
         Core(ic+3) = 0
         Core(ic+4) = 0
         Ii1 = 1
         Jj1 = 1
         GOTO 520
      ENDDO
      CALL rewind(Udv1)
   ENDDO
   CALL close(Udv1,1)
   CALL close(Udv,1)
   Fx(1) = Udv1
   CALL rdtrl(Fx)
   Fx(1) = Udv
   CALL wrttrl(Fx)
END SUBROUTINE frrd1d
