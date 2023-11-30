
SUBROUTINE gfsmo2
   IMPLICIT NONE
   INTEGER Ac , Afry , Ahj , Ahy , Ajh , Axy , Ayh , Azy , Badd(11) , Comptp , Dkaa , Dkfrfr , Gia , Gjh , Gyh , H , Ibuf , Ident , &
         & Ii , Incr , Kc , Kcomp , Khhbar , Kjj , Kjjl , Kmat , Kyy , Kzz , Kzzbar , Lama , Lcore , Llmode , Lmodes , Mhhbar ,     &
         & Mmat , Mzz , Name(2) , Nmodes , Nn , Nofree , Nograv , Nout , Nsub0 , Nsub1 , Nsub2 , Phia , Phix , Pout , Pvec , Scr1 , &
         & Scr10 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , Scr8 , Scr9 , Sfbit , Sysbuf , Two(32) , Typin , Typout , Uf , Ufr ,   &
         & Uh , Ui , Um , Unz , Us , Uset , Usetd , Usetf , Uy , Uz , Z(1)
   REAL Bit1(3) , Bit2(15) , Bit3(2) , Form , Rz(1)
   DOUBLE PRECISION Dbadd(5)
   COMMON /bitpos/ Unz , Uz , Um , Uh , Bit1 , Uf , Us , Bit2 , Uy , Ufr , Bit3 , Ui
   COMMON /blank / Nograv , Nofree , Kcomp , Comptp , Form , Llmode
   COMMON /gfsmox/ Axy , Afry , Kyy , Dkaa , Dkfrfr , Usetf , Phia , Phix , Lama , Kmat , Mmat , Gia , Pout , Scr1 , Scr2 , Scr3 ,  &
                 & Scr4 , Scr5 , Scr6 , Scr7 , Scr8 , Lmodes , Nmodes , Ibuf , Sfbit , Badd , Name
   COMMON /packx / Typin , Typout , Ii , Nn , Incr
   COMMON /patx  / Lcore , Nsub0 , Nsub1 , Nsub2 , Uset
   COMMON /system/ Sysbuf , Nout
   COMMON /two   / Two
   COMMON /zzzzzz/ Z
   REAL eigval(7)
   INTEGER file , i , i1 , i2 , igk , igm , mcb(7) , n , nsub0s , nsub1s , phiar
!
!     THIS ROUTINE IS THE CONTINUATION OF GFSMOD
!
!
!
!
!     MODULE PARAMETERS
!
!
!     SYSTEM COMMON
!
!
!     CALCV COMMON BLOCK
!
!
!     OPEN CORE
!
!
!     PACK COMMON BLOCK
!
!
!     POWERS OF TWO
!
!
!     USET BIT POSITIONS - SOME OF THESE ARE USED JUST HERE
!
!
!     LOCAL VARIABLES FOR GFSMO1 AND GFSMO2
!
!
!     SCRATCH FILE ASSIGNMENTS
!
   !>>>>EQUIVALENCE (Badd(2),Dbadd(1)) , (Rz(1),Z(1)) , (Scr1,Usetd) , (Scr2,Pvec,Ident,Kjjl) , (Scr3,Azy,Ahj,Kjj,Gjh) ,                 &
!>>>>    & (Scr4,Ajh,Khhbar,Gyh) , (Scr5,Ac,Ayh,Mzz,Kzzbar) , (Scr6,Kzz) , (Scr7,Kc,Ahy) , (Scr8,H) , (Scr9,Mmat) , (Scr10,Gia,Mhhbar)
!
!
!***********************************************************************
!
!
!     GET THE GENERALIZED STIFFNESS AND MASS FOR THE DESIRED MODES
!     FROM THE LAMA DATA BLOCK
!
   IF ( 2*Lmodes>=Ibuf ) THEN
      n = -8
!
      CALL mesage(n,file,Name)
      GOTO 99999
   ELSE
      CALL gopen(Lama,Z(Ibuf),0)
      file = Lama
      CALL fwdrec(*200,Lama)
      igk = 1
      igm = Lmodes + 1
      DO i = 1 , Lmodes
         DO
            CALL read(*100,*200,Lama,eigval,7,0,n)
            IF ( eigval(6)/=0.0 ) THEN
               Rz(igk) = eigval(7)
               igk = igk + 1
               Rz(igm) = eigval(6)
               igm = igm + 1
               EXIT
            ENDIF
         ENDDO
      ENDDO
      CALL close(Lama,1)
!
!     GENERATE THE DIAGONAL MODAL STIFFNESS MATRIX
!
      i1 = 1
      i2 = Lmodes
      CALL makmcb(mcb,Kzz,Lmodes,6,2)
      CALL gopen(Kzz,Z(Ibuf),1)
      Typin = 1
      Typout = 2
      Incr = 1
      DO i = i1 , i2
         Ii = i
         Nn = i
         CALL pack(Rz(i),Kzz,mcb)
      ENDDO
      CALL close(Kzz,1)
      CALL wrttrl(mcb)
!
!     GENERATE THE DIAGANOL MODAL MASS MATRIX
!
      i1 = Lmodes + 1
      i2 = 2*Lmodes
      CALL makmcb(mcb,Mzz,Lmodes,6,2)
      CALL gopen(Mzz,Z(Ibuf),1)
      DO i = i1 , i2
         Ii = i - Lmodes
         Nn = Ii
         CALL pack(Rz(i),Mzz,mcb)
      ENDDO
      CALL close(Mzz,1)
      CALL wrttrl(mcb)
!
!     IF A FREE SURFACE EXISTS - EXPAND THE MASS MATRIX
!     THE PARTITIONING VECTOR WILL BE SAVED FOR DMAP USE
!
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(Mhhbar,Mzz)
      ELSE
         Uset = Usetd
         CALL calcv(Pout,Uh,Uz,Ufr,Z(1))
         nsub0s = Nsub0
         nsub1s = Nsub1
         CALL gfsmrg(Mhhbar,Mzz,0,0,0,Pout,Pout)
      ENDIF
!
!     COMPUTE THE FINAL MASS MATRIX
!
      CALL ssg2b(Ajh,Gjh,Mhhbar,Mmat,1,2,1,Scr2)
!
!     IF GRAVITY EXISTS - TRANSFORM THE ADDITIONAL STIFFNESS AND
!     ADD IT IN.  BE SURE TO USE ONLY THOSE MODES REQUESTED IN
!     THE TRANSFORMATION FROM PHIA
!
      IF ( Nograv<0 ) THEN
!
         CALL gfswch(Kzz,Kzzbar)
      ELSE
         Uset = Usetd
         IF ( Lmodes>=Nmodes ) THEN
!
            phiar = Phia
         ELSE
            CALL calcv(Pvec,Um,Uz,Unz,Z(1))
            CALL gfsptn(Phia,phiar,0,0,0,Pvec,0)
         ENDIF
!
         CALL ssg2b(phiar,Dkaa,0,Scr2,1,2,1,Scr5)
         CALL ssg2b(Scr2,phiar,Kzz,Kzzbar,0,2,1,Scr10)
      ENDIF
!
!     IF A FREE SURFACE EXISTS - MERGE THE FREE SURFACE STIFFNESS IN
!
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(Khhbar,Kzzbar)
      ELSE
         Nsub0 = nsub0s
         Nsub1 = nsub1s
         CALL gfsmrg(Khhbar,Kzzbar,0,0,Dkfrfr,Pout,Pout)
      ENDIF
!
!     COMPUTE THE FINAL STIFFNESS MATRIX BY ADDING IN COMPRESSIBILITY
!     IF IT EXISTS
!
      IF ( Sfbit/=0 ) THEN
         CALL gfswch(Khhbar,Kmat)
      ELSE
         Badd(1) = 2
         Dbadd(1) = 1.0D0
         Badd(7) = 2
         Dbadd(4) = 1.0D0
!
         CALL ssg2c(Khhbar,Kc,Kmat,0,Badd)
      ENDIF
!
!     TRANSFORM THE FINAL PRESSURE TRANSFORMATION MATRIX OR IF SPC
!     POINTS EXIST ON THE FLUID MERGE IN ZEROS
!
      Uset = Usetf
      IF ( Sfbit/=0 ) THEN
!
         CALL calcv(Pvec,Uy,Uf,Us,Z(1))
         CALL gfsmrg(Gyh,Gjh,0,0,0,0,Pvec)
      ELSE
         CALL ssg2b(H,Gjh,0,Gyh,1,2,1,Scr5)
      ENDIF
!
!     PARTITION OUT THE FREE SURFACE POINTS
!
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(Gyh,Gia)
         RETURN
      ELSE
         CALL calcv(Pvec,Uy,Ufr,Ui,Z(1))
         CALL gfsptn(Gyh,0,Gia,0,0,0,Pvec)
         RETURN
      ENDIF
   ENDIF
!
!     ERROR EXITS
!
 100  n = -1
   CALL mesage(n,file,Name)
   GOTO 99999
 200  n = -2
   CALL mesage(n,file,Name)
99999 RETURN
END SUBROUTINE gfsmo2