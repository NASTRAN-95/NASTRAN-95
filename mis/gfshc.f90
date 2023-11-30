
SUBROUTINE gfshc(Awy,Nuy,Hc,Ident,Ac,Mrow)
   IMPLICIT NONE
   REAL A(4) , Rz(1)
   DOUBLE PRECISION Dz(1) , Val
   INTEGER I1 , I2 , Incr1 , Incr2 , Irow , N1 , N2 , Sysbuf , Ti1 , To1 , To2 , Z(1)
   COMMON /packx / Ti1 , To1 , I1 , N1 , Incr1
   COMMON /system/ Sysbuf
   COMMON /unpakx/ To2 , I2 , N2 , Incr2
   COMMON /zblpkx/ A , Irow
   COMMON /zzzzzz/ Z
   INTEGER Ac , Awy , Hc , Ident , Mrow , Nuy
   DOUBLE PRECISION dterm
   INTEGER i , ibuf , ir , mcb(7) , mr , name(2) , nrow , nz , scr
   INTEGER korsz
!
!     ROUTINE TO GENERATE CONSTRAINT MATRIX FOR PURELY INCOMPRESSIBLE
!     FORMULATION WHEN NO SPC'S ARE ON FLUID
!
!
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
   DATA name/4HGFSH , 4HC   /
!
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
      CALL ssg2b(Awy,Ident,0,Ac,0,2,1,scr)
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
 200  CALL close(Ac,1)
!
!     LOCATE LARGEST TERM IN AC
!
   dterm = -1.0D10
   DO i = 1 , nrow
      IF ( Dz(i)>dterm ) THEN
         Mrow = i
         dterm = Dz(i)
      ENDIF
   ENDDO
!
!     GENERATE THE HC MATRIX
!
   CALL makmcb(mcb,Hc,nrow,1,2)
   CALL gopen(Hc,Z(ibuf),1)
!
!     GENERATE COLUMNS UP TO MROW
!
   IF ( Mrow/=1 ) THEN
      mr = Mrow - 1
      DO ir = 1 , mr
         CALL bldpk(2,2,Hc,0,0)
         Irow = ir
         Val = 1.0D0
         CALL zblpki
         Irow = Mrow
         Val = -Dz(ir)/dterm
         CALL zblpki
         CALL bldpkn(Hc,0,mcb)
      ENDDO
   ENDIF
!
!     PACK OUT NULL COLUMN FOR MROW
!
   CALL bldpk(2,2,Hc,0,0)
   CALL bldpkn(Hc,0,mcb)
!
!     GENERATE REMAINING ROWS
!
   IF ( Mrow<nrow ) THEN
      mr = Mrow + 1
      DO ir = mr , nrow
         CALL bldpk(2,2,Hc,0,0)
         Irow = Mrow
         Val = -Dz(ir)/dterm
         CALL zblpki
         Irow = ir
         Val = 1.0D0
         CALL zblpki
         CALL bldpkn(Hc,0,mcb)
      ENDDO
   ENDIF
!
   CALL close(Hc,1)
   CALL wrttrl(mcb)
!
   RETURN
99999 RETURN
END SUBROUTINE gfshc