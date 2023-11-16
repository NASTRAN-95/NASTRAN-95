
SUBROUTINE gfscom(Awy,Nuy,Kc,Ident,Ac,Scr)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Comptp , Form , Kcomp , Rz(1)
   DOUBLE PRECISION Dz(1) , Val
   INTEGER I1 , I2 , Incr1 , Incr2 , Irow , Lmodes , N1 , N2 , Nofree , Nograv , Sysbuf , Ti1 , To1 , To2 , Z(1)
   COMMON /blank / Nograv , Nofree , Kcomp , Comptp , Form , Lmodes
   COMMON /packx / Ti1 , To1 , I1 , N1 , Incr1
   COMMON /system/ Sysbuf
   COMMON /unpakx/ To2 , I2 , N2 , Incr2
   COMMON /zblpkx/ A , Irow
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ac , Awy , Ident , Kc , Nuy , Scr
!
! Local variable declarations
!
   DOUBLE PRECISION dkcomp
   INTEGER i , ibuf , j , mcb(7) , name(2) , nrow , nz
   INTEGER korsz
!
! End of declarations
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
   EQUIVALENCE (Z(1),Rz(1),Dz(1)) , (Val,A(1))
!
   DATA name/4HGFSC , 4HOM  /
!
!     ALLOCATE CORE
!
   nz = korsz(Z(1))
   ibuf = nz - Sysbuf
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
      Ti1 = 1
      To1 = 2
      I1 = 1
      N1 = Nuy
      Incr1 = 1
      DO i = 1 , Nuy
         Rz(i) = 1.0
      ENDDO
      CALL makmcb(mcb,Ident,Nuy,2,2)
      CALL gopen(Ident,Z(ibuf),1)
      CALL pack(Rz(1),Ident,mcb)
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
         To2 = 2
         I2 = 1
         N2 = nrow
         Incr2 = 1
!
         CALL gopen(Ac,Z(ibuf),0)
         CALL unpack(*100,Ac,Dz(1))
         GOTO 200
      ENDIF
   ENDIF
!
!     AC IS NULL
!
 100  DO i = 1 , nrow
      Dz(i) = 0.0D0
   ENDDO
!
!     SET UP TO CREATE KC MATRIX
!
 200  CALL close(Ac,1)
!
   dkcomp = dble(Kcomp)
   CALL gopen(Kc,Z(ibuf),1)
   CALL makmcb(mcb,Kc,nrow,1,2)
!
!     LOOP OVER NON-ZERO TERMS OF AC TO CREATE KC
!
   DO i = 1 , nrow
      CALL bldpk(2,2,Kc,0,0)
      IF ( Dz(i)/=0.0D0 ) THEN
         DO j = 1 , nrow
            Irow = j
            Val = dkcomp*Dz(j)*Dz(i)
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
   RETURN
99999 RETURN
END SUBROUTINE gfscom
