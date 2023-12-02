!*==pretrs.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pretrs(Cstmx,Ncstmx)
   USE c_zzzzzz
   USE C_ZZZZZZ
   IMPLICIT NONE
   REAL Cstm(1)
   COMMON /zzzzzz/ Cstm
   INTEGER Ncstmx
   REAL Cstmx(1) , Ecpt(4) , Ta(9)
   REAL fl1 , fl2 , ke(9) , r , tl(9) , x , xl , xn(3) , y , z
   INTEGER i , icheck , int1 , int2 , j , k , kk , ncstm , offset
   INTEGER locfx
!
!     PRETRS SETS UP EVENTUAL CALLS TO TRANSS.  FOR A MODULE TO USE
!     TRANSS A CALL TO PRETRS MUST BE INITIATED BY THE MODULE DRIVER
!     ONCE AND ONLY ONCE.  CSTMX IS ARRAY OF COORDINATE SYSTEM
!     TRANSFORMATION MATRICES AND MCSTMX IS THE LENGTH OF THIS ARRAY.
!
!     THE CSTMX ARRAY MUST BE WITHIN OPEN CORE BOUND, AND THERE IS NO
!     CHECK ON THIS CONDITION
!
!     GIVEN THE ARRAY ECPT OF LENGTH 4, THE FIRST WORD BEING AN INTEGER
!     COORDINATE SYSTEM IDENTIFICATION NUMBER AND THE NEXT WORDS BEING
!     THE REAL COORDINATES OF A POINT IN BASIC COORDINATES, THIS ROUTINE
!     COMPUTES THE TRANSFORMATION (DIRECTION COSINE) MATRIX TA WHICH
!     WILL MAP A VECTOR FROM THE LOCAL SYSTEM LABELED ECPT(1) TO BASIC
!     COORDINATES.
!
!     REVISED  7/92 BY G.CHAN/UNISYS. NEW REFERENCE TO CSTM ARRAY SUCH
!     THAT THE SOURCE CODE IS UP TO ANSI FORTRAN 77 STANDARD.
!
   !>>>>EQUIVALENCE (fl1,int1) , (fl2,int2)
!
   ncstm = Ncstmx
   offset = locfx(Cstmx(1)) - locfx(Cstm(1))
   IF ( offset<0 ) CALL errtrc('PRETRS  ',1)
   icheck = 123456789
   RETURN
!
!
   ENTRY transs(Ecpt,Ta)
!     ======================
!
   fl1 = Ecpt(1)
   IF ( int1==0 ) THEN
!
!     THE LOCAL SYSTEM IS BASIC.
!
      DO i = 1 , 9
         Ta(i) = 0.0
      ENDDO
      Ta(1) = 1.0
      Ta(5) = 1.0
      Ta(9) = 1.0
      RETURN
   ELSE
      IF ( icheck/=123456789 ) CALL errtrc('PRETRS  ',10)
      DO j = 1 , ncstm , 14
         i = j + offset
         fl2 = Cstm(i)
         IF ( int1==int2 ) THEN
            kk = i
            fl2 = Cstm(i+1)
            IF ( int2==1 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            IF ( int2==2 .OR. int2==3 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDIF
      ENDDO
!
!     THE COORDINATE SYSTEM ID. COULD NOT BE FOUND IN THE CSTM.
!
      CALL mesage(-30,25,int1)
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE C_ZZZZZZ
!
!     THE COORDINATE SYSTEM IS RECTANGULAR.
!
      DO J = 1 , 9
         K = Kk + 4 + J
         Ta(J) = Cstm(K)
      ENDDO
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      USE C_ZZZZZZ
      EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
      Xn(1) = Ecpt(2) - Cstm(Kk+2)
      Xn(2) = Ecpt(3) - Cstm(Kk+3)
      Xn(3) = Ecpt(4) - Cstm(Kk+4)
      X = Cstm(Kk+5)*Xn(1) + Cstm(Kk+8)*Xn(2) + Cstm(Kk+11)*Xn(3)
      Y = Cstm(Kk+6)*Xn(1) + Cstm(Kk+9)*Xn(2) + Cstm(Kk+12)*Xn(3)
      Z = Cstm(Kk+7)*Xn(1) + Cstm(Kk+10)*Xn(2) + Cstm(Kk+13)*Xn(3)
      R = sqrt(X**2+Y**2)
      IF ( R==0.0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      DO J = 1 , 9
         K = Kk + 4 + J
         Ke(J) = Cstm(K)
      ENDDO
      IF ( Int2==3 ) THEN
!
!     THE COORDINATE SYSTEM IS SPHERICAL.
!
         Xl = sqrt(X**2+Y**2+Z**2)
         Tl(1) = X/Xl
         Tl(2) = (X*Z)/(R*Xl)
         Tl(3) = -Y/R
         Tl(4) = Y/Xl
         Tl(5) = (Y*Z)/(R*Xl)
         Tl(6) = X/R
         Tl(7) = Z/Xl
         Tl(8) = -R/Xl
         Tl(9) = 0.0
      ELSE
!
!     THE COORDINATE SYSTEM IS CYLINDRICAL.
!
         Tl(1) = X/R
         Tl(2) = -Y/R
         Tl(3) = 0.0
         Tl(4) = -Tl(2)
         Tl(5) = Tl(1)
         Tl(6) = 0.0
         Tl(7) = 0.0
         Tl(8) = 0.0
         Tl(9) = 1.0
      ENDIF
      CALL gmmats(Ke(1),3,3,0,Tl(1),3,3,0,Ta(1))
   END SUBROUTINE spag_block_2
END SUBROUTINE pretrs
