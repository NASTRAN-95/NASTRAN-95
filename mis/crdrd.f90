
SUBROUTINE crdrd(*,*,Mu,Indcom,N23)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bgpdt , Buf(20) , Cstm , Geomp , Gpoint , Ihalf , Jhalf , Kn , Knkl1 , Mac , Mask16 , Nogo , Rgt , Scr1 , Z(1)
   REAL Buf1 , Buf2 , Buf3 , Buf4 , Dz(1) , Rz(1)
   COMMON /gp4fil/ Geomp , Bgpdt , Cstm , Rgt , Scr1
   COMMON /gp4prm/ Buf , Buf1 , Buf2 , Buf3 , Buf4 , Knkl1 , Mask16 , Nogo , Gpoint , Kn
   COMMON /machin/ Mac , Ihalf , Jhalf
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Indcom , Mu , N23
!
! Local variable declarations
!
   REAL cdep , coeff , ddrcos(3) , deptfm(9) , idrcos(3) , indtfm(9) , rlngth , rodcos(3) , xd , yd , zd
   INTEGER i , idep , mask15 , mcode(2)
!
! End of declarations
!
!
!     WRITE THE RIGID ROD ELEMENT ON THE RG FILE
!
!     EXTERNAL        ORF    ,LSHIFT
!     INTEGER         ORF    ,LSHIFT
   EQUIVALENCE (Z(1),Dz(1),Rz(1))
!
!     INDTFM = INDEPENDENT GRID POINT TRANSFORMATION MATRIX
!     DEPTFM = DEPENDENT GRID POINT TRANSFORMATION MATRIX
!     RODCOS = BASIC COSINES OF ROD ELEMENT
!     IDRCOS = DIRECTION COSINES OF INDEPENDENT GRID POINT
!     DDRCOS = DIRECTION COSINES OF DEPENDENT GRID POINT
!
   mask15 = Jhalf/2
!
!     OBTAIN TRANSFORMATION MATRIX
!
   IF ( Z(Knkl1+3)/=0 ) THEN
      DO i = 1 , 4
         Buf(i) = Z(Knkl1+2+i)
      ENDDO
      CALL transs(Buf,indtfm)
   ENDIF
   IF ( Z(Knkl1+10)/=0 ) THEN
      DO i = 1 , 4
         Buf(i) = Z(Knkl1+9+i)
      ENDDO
      CALL transs(Buf,deptfm)
   ENDIF
!
!     COMPUTE THE LENGTH OF THE RIGID ROD ELEMENT
!
   xd = Rz(Knkl1+11) - Rz(Knkl1+4)
   yd = Rz(Knkl1+12) - Rz(Knkl1+5)
   zd = Rz(Knkl1+13) - Rz(Knkl1+6)
!
!     CHECK TO SEE IF LENGTH OF ROD IS ZERO
!
   IF ( xd==0.0 .AND. yd==0.0 .AND. zd==0.0 ) RETURN 1
   rlngth = sqrt(xd*xd+yd*yd+zd*zd)
!
!     COMPUTE THE BASIC DIRECTION COSINES OF THE RIGID ROD ELEMENT
!
   rodcos(1) = xd/rlngth
   rodcos(2) = yd/rlngth
   rodcos(3) = zd/rlngth
!
!     OBTAIN THE DIRECTION COSINES ASSOCIATED WITH
!     THE INDEPENDENT GRID POINT
!
   IF ( Z(Knkl1+3)/=0 ) THEN
      CALL gmmats(rodcos,1,3,0,indtfm,3,3,0,idrcos)
   ELSE
      DO i = 1 , 3
         idrcos(i) = rodcos(i)
      ENDDO
   ENDIF
!
!     OBTAIN THE DIRECTION COSINES ASSOCIATED WITH
!     THE DEPENDENT GRID POINT
!
   IF ( Z(Knkl1+10)/=0 ) THEN
      CALL gmmats(rodcos,1,3,0,deptfm,3,3,0,ddrcos)
   ELSE
      DO i = 1 , 3
         ddrcos(i) = rodcos(i)
      ENDDO
   ENDIF
!
!     DETERMINE THE DEPENDENT SIL AND THE CORRESPONDING COEFFICIENT
!
   DO i = 1 , 3
      IF ( Indcom==i ) THEN
         idep = Z(Knkl1+6+i)
         cdep = rodcos(i)
         EXIT
      ENDIF
   ENDDO
!
!     CHECK TO SEE IF RIGID ROD IS PROPERLY DEFINED
!
   IF ( abs(cdep)<0.0 ) RETURN 2
   mcode(2) = idep
   IF ( idep>mask15 ) N23 = 3
   DO i = 1 , 3
      mcode(1) = Z(Knkl1+i-1)
      IF ( mcode(1)>mask15 ) N23 = 3
      coeff = -idrcos(i)/cdep
      CALL write(Rgt,mcode,2,0)
      CALL write(Rgt,coeff,1,0)
      mcode(1) = Z(Knkl1+6+i)
      IF ( mcode(1)>mask15 ) N23 = 3
      coeff = ddrcos(i)/cdep
      CALL write(Rgt,mcode,2,0)
      CALL write(Rgt,coeff,1,0)
   ENDDO
   Z(Mu) = idep
   Mu = Mu - 1
END SUBROUTINE crdrd
