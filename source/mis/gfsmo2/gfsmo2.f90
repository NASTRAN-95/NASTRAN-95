!*==gfsmo2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsmo2
   IMPLICIT NONE
   USE c_bitpos
   USE c_blank
   USE c_gfsmox
   USE c_packx
   USE c_patx
   USE c_system
   USE c_two
   USE c_zzzzzz
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
! Local variable declarations rewritten by SPAG
!
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
   IF ( 2*lmodes>=ibuf ) THEN
      n = -8
!
      CALL mesage(n,file,name)
      GOTO 99999
   ELSE
      CALL gopen(lama,z(ibuf),0)
      file = lama
      CALL fwdrec(*200,lama)
      igk = 1
      igm = lmodes + 1
      DO i = 1 , lmodes
         DO
            CALL read(*100,*200,lama,eigval,7,0,n)
            IF ( eigval(6)/=0.0 ) THEN
               rz(igk) = eigval(7)
               igk = igk + 1
               rz(igm) = eigval(6)
               igm = igm + 1
               EXIT
            ENDIF
         ENDDO
      ENDDO
      CALL close(lama,1)
!
!     GENERATE THE DIAGONAL MODAL STIFFNESS MATRIX
!
      i1 = 1
      i2 = lmodes
      CALL makmcb(mcb,kzz,lmodes,6,2)
      CALL gopen(kzz,z(ibuf),1)
      typin = 1
      typout = 2
      incr = 1
      DO i = i1 , i2
         ii = i
         nn = i
         CALL pack(rz(i),kzz,mcb)
      ENDDO
      CALL close(kzz,1)
      CALL wrttrl(mcb)
!
!     GENERATE THE DIAGANOL MODAL MASS MATRIX
!
      i1 = lmodes + 1
      i2 = 2*lmodes
      CALL makmcb(mcb,mzz,lmodes,6,2)
      CALL gopen(mzz,z(ibuf),1)
      DO i = i1 , i2
         ii = i - lmodes
         nn = ii
         CALL pack(rz(i),mzz,mcb)
      ENDDO
      CALL close(mzz,1)
      CALL wrttrl(mcb)
!
!     IF A FREE SURFACE EXISTS - EXPAND THE MASS MATRIX
!     THE PARTITIONING VECTOR WILL BE SAVED FOR DMAP USE
!
      IF ( nofree<0 ) THEN
!
         CALL gfswch(mhhbar,mzz)
      ELSE
         uset = usetd
         CALL calcv(pout,uh,uz,ufr,z(1))
         nsub0s = nsub0
         nsub1s = nsub1
         CALL gfsmrg(mhhbar,mzz,0,0,0,pout,pout)
      ENDIF
!
!     COMPUTE THE FINAL MASS MATRIX
!
      CALL ssg2b(ajh,gjh,mhhbar,mmat,1,2,1,scr2)
!
!     IF GRAVITY EXISTS - TRANSFORM THE ADDITIONAL STIFFNESS AND
!     ADD IT IN.  BE SURE TO USE ONLY THOSE MODES REQUESTED IN
!     THE TRANSFORMATION FROM PHIA
!
      IF ( nograv<0 ) THEN
!
         CALL gfswch(kzz,kzzbar)
      ELSE
         uset = usetd
         IF ( lmodes>=nmodes ) THEN
!
            phiar = phia
         ELSE
            CALL calcv(pvec,um,uz,unz,z(1))
            CALL gfsptn(phia,phiar,0,0,0,pvec,0)
         ENDIF
!
         CALL ssg2b(phiar,dkaa,0,scr2,1,2,1,scr5)
         CALL ssg2b(scr2,phiar,kzz,kzzbar,0,2,1,scr10)
      ENDIF
!
!     IF A FREE SURFACE EXISTS - MERGE THE FREE SURFACE STIFFNESS IN
!
      IF ( nofree<0 ) THEN
!
         CALL gfswch(khhbar,kzzbar)
      ELSE
         nsub0 = nsub0s
         nsub1 = nsub1s
         CALL gfsmrg(khhbar,kzzbar,0,0,dkfrfr,pout,pout)
      ENDIF
!
!     COMPUTE THE FINAL STIFFNESS MATRIX BY ADDING IN COMPRESSIBILITY
!     IF IT EXISTS
!
      IF ( sfbit/=0 ) THEN
         CALL gfswch(khhbar,kmat)
      ELSE
         badd(1) = 2
         dbadd(1) = 1.0D0
         badd(7) = 2
         dbadd(4) = 1.0D0
!
         CALL ssg2c(khhbar,kc,kmat,0,badd)
      ENDIF
!
!     TRANSFORM THE FINAL PRESSURE TRANSFORMATION MATRIX OR IF SPC
!     POINTS EXIST ON THE FLUID MERGE IN ZEROS
!
      uset = usetf
      IF ( sfbit/=0 ) THEN
!
         CALL calcv(pvec,uy,uf,us,z(1))
         CALL gfsmrg(gyh,gjh,0,0,0,0,pvec)
      ELSE
         CALL ssg2b(h,gjh,0,gyh,1,2,1,scr5)
      ENDIF
!
!     PARTITION OUT THE FREE SURFACE POINTS
!
      IF ( nofree<0 ) THEN
!
         CALL gfswch(gyh,gia)
         RETURN
      ELSE
         CALL calcv(pvec,uy,ufr,ui,z(1))
         CALL gfsptn(gyh,0,gia,0,0,0,pvec)
         RETURN
      ENDIF
   ENDIF
!
!     ERROR EXITS
!
 100  n = -1
   CALL mesage(n,file,name)
   GOTO 99999
 200  n = -2
   CALL mesage(n,file,name)
99999 END SUBROUTINE gfsmo2
