
SUBROUTINE gkam1a(Mi,Phidh,Sdt,Scr1,Scr2,Iopt,Iout,Nopp,W,Nw,Nosdt,Lhset,I2dd,Iws,Scr3)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Degra , Pi , Radeg , S4pisq , Twophi , Xx(9)
   INTEGER Ii , Iii , Incr , Incr1 , Iprec , It1 , It11 , It2 , Jj , Jjj , Kdamp , Ksystm(65) , Nout , Sysbuf
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Xx , Kdamp
   COMMON /condas/ Pi , Twophi , Radeg , Degra , S4pisq
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Ksystm
   COMMON /unpakx/ It11 , Iii , Jjj , Incr1
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
!
! Dummy argument declarations
!
   INTEGER I2dd , Iopt , Iout , Iws , Lhset , Mi , Nopp , Nosdt , Nw , Phidh , Scr1 , Scr2 , Scr3 , Sdt
   REAL W(1)
!
! Local variable declarations
!
   INTEGER file , i , ibuf , icrq , ihh(3) , imi , ip1 , iret , itab(2) , itabt(13) , iz , k , lc , mcb(7) , mii , name(2) , ne , nz
   REAL g , xmass
   INTEGER korsz
   DOUBLE PRECISION ma(2) , mc , md , zero(2)
!
! End of declarations
!
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(55),Iprec) , (md,ma(1)) , (mc,ma(2))
   DATA zero/0.0D0 , 0.0D0/
   DATA name/4HGKAM , 4H1A  /
   DATA ihh/4HMHH  , 4HBHH  , 4HKHH /
   DATA g/0.0/
   DATA itabt , itab(1)/4 , 15 , 21 , 1 , 25 , 22 , 2 , 35 , 23 , 3 , 45 , 24 , 4 , 0/
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
   nz = lc - Sysbuf
!
!     RESTORE MODES
!     FILE = SCR3
!
   CALL open(*200,Scr3,W(nz+1),0)
   CALL fread(Scr3,W,Nw,1)
   CALL close(Scr3,1)
   file = Mi
   CALL open(*500,Mi,W(nz+1),0)
   imi = 0
   CALL skprec(Mi,Iws)
 100  nz = nz - Sysbuf
   ibuf = nz - Sysbuf
   icrq = -ibuf
   IF ( icrq>0 ) THEN
      ip1 = -8
      file = icrq
      GOTO 300
   ELSE
      CALL gopen(mii,W(nz+1),1)
      CALL makmcb(mcb,mii,Lhset,6,Iprec)
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
            CALL unpack(*400,Mi,md)
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
               ASSIGN 140 TO iret
               IF ( Nosdt<=0 ) GOTO 140
               GOTO 160
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
               ASSIGN 120 TO iret
               IF ( Nosdt>0 ) GOTO 160
            ENDIF
         ELSE
            CALL pack(md,mii,mcb)
            CYCLE
         ENDIF
 120     mc = g*md
         CALL pack(md,mii,mcb)
         CYCLE
 140     md = md*W(i)*g
         CALL pack(md,mii,mcb)
         CYCLE
!
!     LOOK UP G(W)  IN  SDT
!
 160     IF ( itab(1)<=0 ) THEN
            itab(1) = 1
            itab(2) = Nosdt
            CALL pretab(Sdt,W(Nw+1),W(Nw+1),W(ibuf),ibuf-1,iz,itab(1),itabt)
         ENDIF
         CALL tab(itab(2),W(i)/Twophi,g)
         GOTO iret
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
 200  ip1 = -1
 300  CALL mesage(ip1,file,name)
   RETURN
 400  WRITE (Nout,99001) Sfm , ihh(Iopt)
99001 FORMAT (A25,' 2203, NULL COLUMN FOUND IN MI FILE DURING ASSEMBLY',' OF ',A4,' MATRIX BY GKAM MODULE.')
   ip1 = -37
   GOTO 300
!
!     USE LAMA RATHER THAN MI
!
 500  CALL gopen(Mi+1,W(nz+1),0)
   CALL skprec(Mi+1,1)
   CALL fread(Mi+1,mcb,-7*(Iws-1),0)
   imi = 1
   GOTO 100
END SUBROUTINE gkam1a
