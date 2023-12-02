!*==gfscom.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfscom(Awy,Nuy,Kc,Ident,Ac,Scr)
   IMPLICIT NONE
   USE c_blank
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zblpkx
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Awy
   INTEGER :: Nuy
   INTEGER :: Kc
   INTEGER :: Ident
   INTEGER :: Ac
   INTEGER :: Scr
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: dkcomp , val
   REAL*8 , DIMENSION(1) :: dz
   INTEGER :: i , ibuf , j , nrow , nz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: rz
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     ROUTINE TO COMPUTE THE FLUID COMPRESSIBILTY MATRIX
!
!     THIS MATRIX CONTAINS THE SPRING FACTOR WHICH COUPLES THE
!     STRUCTURE AND FREE SURFACE TO PREVENT VOLUME CHANGES
!
!
!
!
!
!     MODULE PARAMETERS
!
!
!     OPEN CORE
!
!
!     SYSTEM COMMON
!
!
!     PACK - UNPACK COMMON BLOCKS
!
!
   !>>>>EQUIVALENCE (Z(1),Rz(1),Dz(1)) , (Val,A(1))
!
   DATA name/4HGFSC , 4HOM  /
!
!     ALLOCATE CORE
!
   nz = korsz(z(1))
   ibuf = nz - sysbuf
   nz = ibuf - 1
   IF ( nz<Nuy ) THEN
!
!     ERRORS
!
      CALL mesage(-8,0,name)
      GOTO 99999
   ELSE
!
!     FORM A COLUMN VECTOR OF ONES
!
      ti1 = 1
      to1 = 2
      i1 = 1
      n1 = Nuy
      incr1 = 1
      DO i = 1 , Nuy
         rz(i) = 1.0
      ENDDO
      CALL makmcb(mcb,Ident,Nuy,2,2)
      CALL gopen(Ident,z(ibuf),1)
      CALL pack(rz(1),Ident,mcb)
      CALL close(Ident,1)
      CALL wrttrl(mcb)
!
      CALL ssg2b(Awy,Ident,0,Ac,0,2,1,Scr)
!
!     PERFORM MULTIPLY TO GET COMPRESSIBLITY MATRIX
!
!
!     UNPACK ROW OF AC INTO CORE
!
      mcb(1) = Ac
      CALL rdtrl(mcb)
      nrow = mcb(3)
      IF ( nz<2*nrow ) THEN
         CALL mesage(-8,0,name)
         GOTO 99999
      ELSE
         to2 = 2
         i2 = 1
         n2 = nrow
         incr2 = 1
!
         CALL gopen(Ac,z(ibuf),0)
         CALL unpack(*100,Ac,dz(1))
         GOTO 200
      ENDIF
   ENDIF
!
!     AC IS NULL
!
 100  DO i = 1 , nrow
      dz(i) = 0.0D0
   ENDDO
!
!     SET UP TO CREATE KC MATRIX
!
 200  CALL close(Ac,1)
!
   dkcomp = dble(kcomp)
   CALL gopen(Kc,z(ibuf),1)
   CALL makmcb(mcb,Kc,nrow,1,2)
!
!     LOOP OVER NON-ZERO TERMS OF AC TO CREATE KC
!
   DO i = 1 , nrow
      CALL bldpk(2,2,Kc,0,0)
      IF ( dz(i)/=0.0D0 ) THEN
         DO j = 1 , nrow
            irow = j
            val = dkcomp*dz(j)*dz(i)
            CALL zblpki
         ENDDO
      ENDIF
      CALL bldpkn(Kc,0,mcb)
   ENDDO
   CALL close(Kc,1)
!
!     WRITE TRAILER
!
   CALL wrttrl(mcb)
99999 END SUBROUTINE gfscom
