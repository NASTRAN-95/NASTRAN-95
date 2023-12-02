!*==gkam1a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gkam1a(Mi,Phidh,Sdt,Scr1,Scr2,Iopt,Iout,Nopp,W,Nw,Nosdt,Lhset,I2dd,Iws,Scr3)
USE C_BLANK
USE C_CONDAS
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mi
   INTEGER :: Phidh
   INTEGER :: Sdt
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Iopt
   INTEGER :: Iout
   INTEGER :: Nopp
   REAL , DIMENSION(1) :: W
   INTEGER :: Nw
   INTEGER :: Nosdt
   INTEGER :: Lhset
   INTEGER :: I2dd
   INTEGER :: Iws
   INTEGER :: Scr3
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibuf , icrq , imi , ip1 , iprec , iret , iz , k , lc , mii , ne , nout , nz , sysbuf
   REAL , SAVE :: g
   INTEGER , DIMENSION(3) , SAVE :: ihh
   INTEGER , DIMENSION(2) , SAVE :: itab , name
   INTEGER , DIMENSION(13) , SAVE :: itabt
   REAL(REAL64) , DIMENSION(2) :: ma
   REAL(REAL64) :: mc , md
   INTEGER , DIMENSION(7) :: mcb
   REAL :: xmass
   REAL(REAL64) , DIMENSION(2) , SAVE :: zero
   EXTERNAL close , fread , gopen , korsz , makmcb , mesage , open , pack , pretab , rdtrl , skprec , ssg2b , tab , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(55),Iprec) , (md,ma(1)) , (mc,ma(2))
   DATA zero/0.0D0 , 0.0D0/
   DATA name/4HGKAM , 4H1A  /
   DATA ihh/4HMHH  , 4HBHH  , 4HKHH /
   DATA g/0.0/
   DATA itabt , itab(1)/4 , 15 , 21 , 1 , 25 , 22 , 2 , 35 , 23 , 3 , 45 , 24 , 4 , 0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         mc = 0.0
         IF ( Nopp>=0 ) THEN
!
!     COMPUTE PHIDH(T)*I2DD*PHIDH ONTO SCR2
!
            CALL ssg2b(I2dd,Phidh,0,Scr1,0,2,1,Iout)
            CALL ssg2b(Phidh,Scr1,0,Scr2,1,2,1,Iout)
            mcb(1) = I2dd
            CALL rdtrl(mcb)
            IF ( mcb(4)==6 ) THEN
               mcb(1) = Scr2
               CALL rdtrl(mcb)
               mcb(4) = 6
               CALL wrttrl(mcb)
            ENDIF
            mii = Scr1
         ENDIF
         IF ( Nopp<0 ) mii = Iout
!
!     BUILD  MII  DATA  BLOCK  = MIXF(W)
!
         lc = korsz(W(Nw+1))
         nz = lc - sysbuf
!
!     RESTORE MODES
!     FILE = SCR3
!
         CALL open(*20,Scr3,W(nz+1),0)
         CALL fread(Scr3,W,Nw,1)
         CALL close(Scr3,1)
         file = Mi
         CALL open(*60,Mi,W(nz+1),0)
         imi = 0
         CALL skprec(Mi,Iws)
         spag_nextblock_1 = 2
      CASE (2)
         nz = nz - sysbuf
         ibuf = nz - sysbuf
         icrq = -ibuf
         IF ( icrq>0 ) THEN
            ip1 = -8
            file = icrq
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL gopen(mii,W(nz+1),1)
            CALL makmcb(mcb,mii,Lhset,6,iprec)
            IF ( Kdamp==1 ) mcb(5) = mcb(5) + 2
!
!     SET UP FOR  PACK  AND  UNPACK
!
            It1 = 2
            IF ( Kdamp==1 ) It1 = 4
            It2 = mcb(5)
            Incr = 1
            It11 = 2
            Incr1 = 1
            DO i = 1 , Nw
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     mc = 0.0
                     k = Iws + i - 1
                     Ii = i
                     Jj = i
                     Iii = k
                     Jjj = k
                     IF ( imi/=0 ) THEN
!
!     PICK UP MODAL MASS FROM LAMA
!
                        CALL fread(Mi+1,0,-5,0)
                        CALL fread(Mi+1,xmass,1,0)
                        CALL fread(Mi+1,0,-1,0)
                        md = xmass
                     ELSE
                        CALL unpack(*40,Mi,md)
                     ENDIF
                     IF ( Iopt==2 ) THEN
!
!     BUILDING  BHH
!
                        IF ( Kdamp==1 ) THEN
                           md = 0.0
!
!     BUILDING  MHH
!
                           CALL pack(md,mii,mcb)
                           CYCLE
                        ELSE
                           ASSIGN 4 TO iret
                           IF ( Nosdt>0 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                           GOTO 4
                        ENDIF
                     ELSEIF ( Iopt==3 ) THEN
!
!     BUILDING  KHH
!
                        md = md*W(i)*W(i)
                        IF ( Kdamp/=1 ) THEN
                           CALL pack(md,mii,mcb)
                           CYCLE
                        ELSE
                           ASSIGN 2 TO iret
                           IF ( Nosdt>0 ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDIF
                     ELSE
                        CALL pack(md,mii,mcb)
                        CYCLE
                     ENDIF
 2                   mc = g*md
                     CALL pack(md,mii,mcb)
                     CYCLE
 4                   md = md*W(i)*g
                     CALL pack(md,mii,mcb)
                     CYCLE
                  CASE (2)
!
!     LOOK UP G(W)  IN  SDT
!
                     IF ( itab(1)<=0 ) THEN
                        itab(1) = 1
                        itab(2) = Nosdt
                        CALL pretab(Sdt,W(Nw+1),W(Nw+1),W(ibuf),ibuf-1,iz,itab(1),itabt)
                     ENDIF
                     CALL tab(itab(2),W(i)/Twophi,g)
                     GOTO iret
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
!     ADD  INTERPOLATION HERE
!
            ENDDO
            CALL close(Mi,1)
            CALL close(Mi+1,1)
            ne = Lhset - Nw
            IF ( ne>0 ) THEN
               DO i = 1 , ne
                  CALL pack(zero,mii,mcb)
               ENDDO
            ENDIF
            CALL wrttrl(mcb)
            CALL close(mii,1)
            RETURN
         ENDIF
!
!     ERROR MESAGES
!
 20      ip1 = -1
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(ip1,file,name)
         RETURN
 40      WRITE (nout,99001) Sfm , ihh(Iopt)
99001    FORMAT (A25,' 2203, NULL COLUMN FOUND IN MI FILE DURING ASSEMBLY',' OF ',A4,' MATRIX BY GKAM MODULE.')
         ip1 = -37
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     USE LAMA RATHER THAN MI
!
 60      CALL gopen(Mi+1,W(nz+1),0)
         CALL skprec(Mi+1,1)
         CALL fread(Mi+1,mcb,-7*(Iws-1),0)
         imi = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gkam1a
