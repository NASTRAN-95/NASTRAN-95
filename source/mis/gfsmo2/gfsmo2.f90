!*==gfsmo2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsmo2
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_GFSMOX
   USE C_PACKX
   USE C_PATX
   USE C_SYSTEM
   USE C_TWO
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ac , ahj , ahy , ajh , ayh , azy , file , gjh , gyh , h , i , i1 , i2 , ident , igk , igm , kc , khhbar , kjj , kjjl ,&
            & kzz , kzzbar , mhhbar , mzz , n , nsub0s , nsub1s , phiar , pvec , scr10 , scr9 , usetd
   REAL*8 , DIMENSION(5) :: dbadd
   REAL , DIMENSION(7) :: eigval
   INTEGER , DIMENSION(7) :: mcb
   REAL , DIMENSION(1) :: rz
!
! End of declarations rewritten by SPAG
!
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
               rz(igk) = eigval(7)
               igk = igk + 1
               rz(igm) = eigval(6)
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
      CALL makmcb(mcb,kzz,Lmodes,6,2)
      CALL gopen(kzz,Z(Ibuf),1)
      Typin = 1
      Typout = 2
      Incr = 1
      DO i = i1 , i2
         Ii = i
         Nn = i
         CALL pack(rz(i),kzz,mcb)
      ENDDO
      CALL close(kzz,1)
      CALL wrttrl(mcb)
!
!     GENERATE THE DIAGANOL MODAL MASS MATRIX
!
      i1 = Lmodes + 1
      i2 = 2*Lmodes
      CALL makmcb(mcb,mzz,Lmodes,6,2)
      CALL gopen(mzz,Z(Ibuf),1)
      DO i = i1 , i2
         Ii = i - Lmodes
         Nn = Ii
         CALL pack(rz(i),mzz,mcb)
      ENDDO
      CALL close(mzz,1)
      CALL wrttrl(mcb)
!
!     IF A FREE SURFACE EXISTS - EXPAND THE MASS MATRIX
!     THE PARTITIONING VECTOR WILL BE SAVED FOR DMAP USE
!
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(mhhbar,mzz)
      ELSE
         Uset = usetd
         CALL calcv(Pout,Uh,Uz,Ufr,Z(1))
         nsub0s = Nsub0
         nsub1s = Nsub1
         CALL gfsmrg(mhhbar,mzz,0,0,0,Pout,Pout)
      ENDIF
!
!     COMPUTE THE FINAL MASS MATRIX
!
      CALL ssg2b(ajh,gjh,mhhbar,Mmat,1,2,1,Scr2)
!
!     IF GRAVITY EXISTS - TRANSFORM THE ADDITIONAL STIFFNESS AND
!     ADD IT IN.  BE SURE TO USE ONLY THOSE MODES REQUESTED IN
!     THE TRANSFORMATION FROM PHIA
!
      IF ( Nograv<0 ) THEN
!
         CALL gfswch(kzz,kzzbar)
      ELSE
         Uset = usetd
         IF ( Lmodes>=Nmodes ) THEN
!
            phiar = Phia
         ELSE
            CALL calcv(pvec,Um,Uz,Unz,Z(1))
            CALL gfsptn(Phia,phiar,0,0,0,pvec,0)
         ENDIF
!
         CALL ssg2b(phiar,Dkaa,0,Scr2,1,2,1,Scr5)
         CALL ssg2b(Scr2,phiar,kzz,kzzbar,0,2,1,scr10)
      ENDIF
!
!     IF A FREE SURFACE EXISTS - MERGE THE FREE SURFACE STIFFNESS IN
!
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(khhbar,kzzbar)
      ELSE
         Nsub0 = nsub0s
         Nsub1 = nsub1s
         CALL gfsmrg(khhbar,kzzbar,0,0,Dkfrfr,Pout,Pout)
      ENDIF
!
!     COMPUTE THE FINAL STIFFNESS MATRIX BY ADDING IN COMPRESSIBILITY
!     IF IT EXISTS
!
      IF ( Sfbit/=0 ) THEN
         CALL gfswch(khhbar,Kmat)
      ELSE
         Badd(1) = 2
         dbadd(1) = 1.0D0
         Badd(7) = 2
         dbadd(4) = 1.0D0
!
         CALL ssg2c(khhbar,kc,Kmat,0,Badd)
      ENDIF
!
!     TRANSFORM THE FINAL PRESSURE TRANSFORMATION MATRIX OR IF SPC
!     POINTS EXIST ON THE FLUID MERGE IN ZEROS
!
      Uset = Usetf
      IF ( Sfbit/=0 ) THEN
!
         CALL calcv(pvec,Uy,Uf,Us,Z(1))
         CALL gfsmrg(gyh,gjh,0,0,0,0,pvec)
      ELSE
         CALL ssg2b(h,gjh,0,gyh,1,2,1,Scr5)
      ENDIF
!
!     PARTITION OUT THE FREE SURFACE POINTS
!
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(gyh,Gia)
         RETURN
      ELSE
         CALL calcv(pvec,Uy,Ufr,Ui,Z(1))
         CALL gfsptn(gyh,0,Gia,0,0,0,pvec)
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
99999 END SUBROUTINE gfsmo2
