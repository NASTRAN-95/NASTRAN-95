
SUBROUTINE bmgtns(Cstm,Ncstm,Ecpt,Ta)
   IMPLICIT NONE
   INTEGER Ncstm
   REAL Cstm(1) , Ecpt(4)
   DOUBLE PRECISION Ta(9)
   REAL fl1 , fl2
   INTEGER i , int1 , int2 , j , k , kk
   DOUBLE PRECISION ke(9) , r , tl(9) , x , xl , xn(3) , y , z
!/// THIS ROUTINE WAS LIFTED FROM PRETRD AND TRANSD AND CONVERTED
!///// TO HAVE ONE ENTRY POINT
!
! PRETRD SETS UP EVENTUAL CALLS TO TRANSD.  FOR A MODULE TO USE TRANSD
! A CALL TO PRETRD MUST BE INITIATED BY THE MODULE DRIVER ONCE AND ONLY
! ONCE.  CSTM IS ARRAY OF COORDINATE SYSTEM TRANSFORMATION MATRICES
! AND NCSTM IS THE LENGTH OF THIS ARRAY.
!
!*****
! GIVEN THE ECPT ARRAY OF LENGTH 4, THE FIRST WORD BEING AN INTEGER
! COORDINATE SYSTEM IDENTIFICATION NUMBER AND THE NEXT WORDS BEING THE
! REAL COORDINATES OF A POINT IN BASIC COORDINATES, THIS ROUTINE
! COMPUTES THE TRANSFORMATION (DIRECTION COSINE) MATRIX TA WHICH WILL
! MAP A VECTOR FROM THE LOCAL SYSTEM LABELED ECPT(1) TO BASIC COORDI-
! NATES.  TA IS A DOUBLE PRECISION MATRIX.
!*****
!
!
   EQUIVALENCE (fl1,int1) , (fl2,int2)
!
   fl1 = Ecpt(1)
   IF ( int1==0 ) THEN
!
! THE LOCAL SYSTEM IS BASIC.
!
      DO i = 1 , 9
         Ta(i) = 0.0D0
      ENDDO
      Ta(1) = 1.0D0
      Ta(5) = 1.0D0
      Ta(9) = 1.0D0
      GOTO 99999
   ELSE
      DO i = 1 , Ncstm , 14
         fl2 = Cstm(i)
         IF ( int1==int2 ) THEN
            kk = i
            fl2 = Cstm(i+1)
            IF ( int2==1 ) GOTO 100
            IF ( int2==2 .OR. int2==3 ) GOTO 200
         ENDIF
      ENDDO
!
! THE COORDINATE SYSTEM ID. COULD NOT BE FOUND IN THE CSTM.
!
      CALL mesage(-30,25,int1)
   ENDIF
!
! THE COORDINATE SYSTEM IS RECTANGULAR.
!
 100  DO j = 1 , 9
      k = kk + 4 + j
      Ta(j) = Cstm(k)
   ENDDO
   RETURN
 200  xn(1) = Ecpt(2) - Cstm(kk+2)
   xn(2) = Ecpt(3) - Cstm(kk+3)
   xn(3) = Ecpt(4) - Cstm(kk+4)
   x = Cstm(kk+5)*xn(1) + Cstm(kk+8)*xn(2) + Cstm(kk+11)*xn(3)
   y = Cstm(kk+6)*xn(1) + Cstm(kk+9)*xn(2) + Cstm(kk+12)*xn(3)
   z = Cstm(kk+7)*xn(1) + Cstm(kk+10)*xn(2) + Cstm(kk+13)*xn(3)
   r = dsqrt(x**2+y**2)
   IF ( r==0.0D0 ) GOTO 100
   DO j = 1 , 9
      k = kk + 4 + j
      ke(j) = Cstm(k)
   ENDDO
   IF ( int2==3 ) THEN
!
! THE COORDINATE SYSTEM IS SPHERICAL.
!
      xl = dsqrt(x*x+y*y+z*z)
      tl(1) = x/xl
      tl(2) = (x*z)/(r*xl)
      tl(3) = -y/r
      tl(4) = y/xl
      tl(5) = (y*z)/(r*xl)
      tl(6) = x/r
      tl(7) = z/xl
      tl(8) = -r/xl
      tl(9) = 0.0D0
   ELSE
!
! THE COORDINATE SYSTEM IS CYLINDRICAL.
!
      tl(1) = x/r
      tl(2) = -y/r
      tl(3) = 0.0D0
      tl(4) = -tl(2)
      tl(5) = tl(1)
      tl(6) = 0.0D0
      tl(7) = 0.0D0
      tl(8) = 0.0D0
      tl(9) = 1.0D0
   ENDIF
   CALL gmmatd(ke(1),3,3,0,tl(1),3,3,0,Ta(1))
   RETURN
99999 RETURN
END SUBROUTINE bmgtns
