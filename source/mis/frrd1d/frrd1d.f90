!*==frrd1d.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frrd1d(Pd,Ull,Lll,Scr1,Scr2,Udv,Ifr,Nload,Igood,Nfreq)
   USE c_fbsx
   USE c_gfbsx
   USE c_machin
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   USE C_FBSX
   USE C_GFBSX
   USE C_MACHIN
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
   IMPLICIT NONE
   REAL Core(1)
   INTEGER Fb(7) , Fl(7) , Fu(7) , Fx(7) , icore(1) , Ii , Ii1 , Incr , Incr1 , iprec , Iscrx , Isign , It1 , It2 , It3 , Jj , Jj1 ,&
         & Ksystm(65) , Mach , mcore(1) , Mfb(7) , Mfl(7) , Mflt(7) , Mfx(7) , Mprec , Msign , Mx , Nx , Prec , sysbuf
   COMMON /fbsx  / Mfl , Mflt , Mfb , Mfx , Mx , Mprec , Msign , Iscrx
   COMMON /gfbsx / Fl , Fu , Fb , Fx , Nx , Prec , Isign
   COMMON /machin/ Mach
   COMMON /packx / It2 , It3 , Ii1 , Jj1 , Incr1
   COMMON /system/ Ksystm
   COMMON /unpakx/ It1 , Ii , Jj , Incr
   COMMON /zzzzzz/ Core
   INTEGER Ifr , Igood , Lll , Nfreq , Nload , Pd , Scr1 , Scr2 , Udv , Udv1 , Ull
   INTEGER file , i , ic , iflag , m , mcb(7) , name(2) , nz
   INTEGER korsz
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     ROUTINE SOLVES FOR UDV GIVEN ULL,LLL, AND PD
!
!     IF IGOOD = 1 DCOMP FAILED -- PUT ZERO SOLUTION VECTORS OUT
!
!     1. PULL LOADS FROM PD ONTO SCR1
!     2. SOLVE FOR UDV-S ON SCR2
!     3. STACK SOLVED LOADS ON UDV
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec) , (Core(1),Icore(1),Mcore(1))
   DATA name/4HFRRD , 4H1D  / , ic/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Nx = korsz(Core)
         Fb(1) = Pd
         CALL rdtrl(Fb)
         Fx(1) = Scr2
         IF ( Ifr==1 ) Fx(1) = Udv
         Fx(2) = Nload
         Fx(3) = Fb(3)
         Fx(4) = 2
         Fx(5) = 2 + iprec
         It1 = Fb(5)
         Incr = 1
         Incr1 = 1
         It2 = It1
         It3 = 2 + iprec
         IF ( Igood/=1 ) THEN
!
!     PULL LOADS FROM PD ONTO SCR1
!
            Fu(1) = Ull
            CALL rdtrl(Fu)
            Fl(1) = Lll
            CALL rdtrl(Fl)
            IF ( Nfreq/=1 ) THEN
               nz = Nx - sysbuf
               CALL gopen(Pd,Core(nz+1),0)
               CALL skprec(Pd,Ifr-1)
               nz = nz - sysbuf
               CALL gopen(Scr1,Core(nz+1),1)
               CALL makmcb(mcb,Scr1,Fb(3),2,It3)
               DO i = 1 , Nload
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        IF ( i>1 ) CALL skprec(Pd,Nfreq-1)
                        Ii = 0
                        CALL unpack(*2,Pd,Core)
                        Ii1 = Ii
                        Jj1 = Jj
                        spag_nextblock_2 = 2
                     CASE (2)
                        CALL pack(Core,Scr1,mcb)
                        CYCLE
 2                      Core(1) = 0
                        Core(ic+2) = 0
                        Core(ic+3) = 0
                        Core(ic+4) = 0
                        Ii1 = 1
                        Jj1 = 1
                        spag_nextblock_2 = 2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
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
               Mx = korsz(mcore)
               Iscrx = Mflt(1)
               mcore(1) = Mflt(1)
               CALL rdtrl(mcore(1))
               IF ( mcore(1)<=0 ) Iscrx = 0
               CALL fbs(mcore,mcore)
               CALL wrttrl(Mfx)
            ELSE
               CALL gfbs(Core,Core)
               CALL wrttrl(Fx)
            ENDIF
         ENDIF
         icore(1) = 16777215
!                16777215 = '00FFFFFF'X
         iflag = 1
!
!     STACK LOADS ONTO UDV
!
         file = Udv
         nz = Nx - sysbuf
         IF ( Ifr==1 ) THEN
!
            IF ( Igood/=1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL gopen(Udv,Core(nz+1),1)
            Fx(2) = 0
            Fx(6) = 0
            Fx(7) = 0
            CALL wrttrl(Fx)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL open(*40,Udv,Core(nz+1),0)
            Fx(1) = Udv
            CALL rdtrl(Fx)
            IF ( Mach/=1 ) THEN
               CALL skpfil(Udv,1)
               CALL skpfil(Udv,-1)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               DO
                  CALL fwdrec(*20,Udv)
               ENDDO
            ENDIF
         ENDIF
 20      CALL bckrec(Udv)
         CALL skprec(Udv,1)
         spag_nextblock_1 = 2
      CASE (2)
         CALL close(Udv,2)
         CALL open(*40,Udv,Core(nz+1),3)
!
!     RESET TYPE FLAGS
!
         It1 = Fx(5)
         It2 = It1
         It3 = It1
         IF ( Igood/=1 ) THEN
            nz = nz - sysbuf
            CALL gopen(Scr2,Core(nz+1),0)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         DO i = 1 , Nload
            IF ( Igood/=1 ) THEN
               Ii = 0
               CALL unpack(*30,Scr2,Core)
               Ii1 = Ii
               Jj1 = Jj
               CALL pack(Core,Udv,Fx)
               CYCLE
            ENDIF
 30         Core(1) = 0
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
         spag_nextblock_1 = 4
      CASE (4)
         RETURN
!
!     ERROR MESAGES
!
 40      CALL mesage(-1,file,name)
!
!
         ENTRY frrd1e(Udv1,Udv,Nload,Nfreq)
!     ===================================
!
         nz = korsz(Core) - sysbuf
!
!     ROUTINE REORDERS SOLUTIONS TO GET SORT BY LOADS
!
         file = Udv1
         CALL open(*40,Udv1,Core(nz+1),0)
         nz = nz - sysbuf
         CALL gopen(Udv,Core(nz+1),1)
         file = Udv1
         DO i = 1 , Nload
            CALL skprec(Udv1,i)
            DO m = 1 , Nfreq
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     Ii = 0
                     CALL unpack(*42,Udv1,Core)
                     Ii1 = Ii
                     Jj1 = Jj
                     spag_nextblock_3 = 2
                  CASE (2)
                     CALL pack(Core,Udv,mcb)
                     IF ( m<Nfreq ) CALL skprec(Udv1,Nload-1)
                     CYCLE
 42                  Core(1) = 0
                     Core(ic+2) = 0
                     Core(ic+3) = 0
                     Core(ic+4) = 0
                     Ii1 = 1
                     Jj1 = 1
                     spag_nextblock_3 = 2
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
            CALL rewind(Udv1)
         ENDDO
         CALL close(Udv1,1)
         CALL close(Udv,1)
         Fx(1) = Udv1
         CALL rdtrl(Fx)
         Fx(1) = Udv
         CALL wrttrl(Fx)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frrd1d
