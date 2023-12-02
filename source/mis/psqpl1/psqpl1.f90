!*==psqpl1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE psqpl1
   USE c_condas
   USE c_matin
   USE c_pla32s
   USE c_pla3es
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: degra , ftemp
   INTEGER :: i , j , k , km
   INTEGER , DIMENSION(12) , SAVE :: m
   INTEGER , DIMENSION(100) :: necpt
   REAL , DIMENSION(10) :: requiv
   REAL , DIMENSION(3) :: vq1 , vq2 , vq3 , vq4
   EXTERNAL gmmats , mesage , pstrb1 , transs
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CALCULATES PHASE I OUTPUT FOR PLA3
!     FOR  THE QUAD-PLATE PART OF COMBINATION ELEMENTS
!
!     PHASE I OF STRESS DATA RECOVERY FOR TRI OR QUAD PLATE.
!
!     OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING.
!
!     1) ELEMENT ID
!     2) 4 SILS
!     3) I
!     4) Z1 AND Z2
!     5) 4  5X6 S-SUB-I ARRAYS
!     THUS, 128 WORDS FOR QUAD-PLATE
!
!     ECPT LISTS AS OF AUGUST 4, 1967
!
!                 DEFINITION                   DEFINITION
!       ECPT      BSC.BEND.TRI.-----TYPE       QUAD.PLT.---------TYPE
!     ========   ==============     =======    ==============    =======
!     ECPT( 1) = ELEMENT ID         INTEGER ** ELEMENT           INTEGER
!     ECPT( 2) = GRID PT. A         INTEGER ** GRID PT.A         INTEGER
!     ECPT( 3) = GRID PT. B         INTEGER ** GRID PT.B         INTEGER
!     ECPT( 4) = GRID PT. C         INTEGER ** GRID PT.C         INTEGER
!     ECPT( 5) = THETA              REAL    ** GRID PT.D         INTEGER
!     ECPT( 6) = MAT ID 1           INTEGER ** THETA             REAL
!     ECPT( 7) = I  MOM. OF INERT.  REAL    ** MAT ID 1          INTEGER
!     ECPT( 8) = MAT ID 2           INTEGER ** I  MOM. OF INERT. REAL
!     ECPT( 9) = T2                 REAL    ** MAT ID 2          INTEGER
!     ECPT(10) = NON-STRUCT. MASS   REAL    ** T2                REAL
!     ECPT(11) = Z1                 REAL    ** NON-STRUCT. MASS  REAL
!     ECPT(12) = Z2                 REAL    ** Z1                REAL
!     ECPT(13) = COORD. SYS. ID 1   INTEGER ** Z2                REAL
!     ECPT(14) = X1                 REAL    ** COORD. SYS. ID 1  INTEGER
!     ECPT(15) = Y1                 REAL    ** X1                REAL
!     ECPT(16) = Z1                 REAL    ** Y1                REAL
!     ECPT(17) = COORD. SYS. ID 2   INTEGER ** Z1                REAL
!     ECPT(18) = X2                 REAL    ** COORD. SYS. ID 2  INTEGER
!     ECPT(19) = Y2                 REAL    ** X2                REAL
!     ECPT(20) = Z2                 REAL    ** Y2                REAL
!     ECPT(21) = COORD. SYS. ID 3   INTEGER ** Z2                REAL
!     ECPT(22) = X3                 REAL    ** COORD. SYS. ID 3  INTEGER
!     ECPT(23) = Y3                 REAL    ** X3                REAL
!     ECPT(24) = Z3                 REAL    ** Y3                REAL
!     ECPT(25) = ELEMENT TEMP       REAL    ** Z3                REAL
!     ECPT(26) =                            ** COORD. SYS. ID 4  INTEGER
!     ECPT(27) =                            ** X4                REAL
!     ECPT(28) =                            ** Y4                REAL
!     ECPT(29) =                            ** Z4                REAL
!     ECPT(30) =                            ** ELEMENT TEMP      REAL
!
   !>>>>EQUIVALENCE (Consts(4),Degra) , (Ecpt(1),Necpt(1)) , (Vq1(1),Ecpt(15)) , (Vq2(1),Ecpt(19)) , (Vq3(1),Ecpt(23)) ,                 &
!>>>>    & (Vq4(1),Ecpt(27)) , (Requiv(1),R(1,1))
   DATA m/2 , 4 , 1 , 3 , 1 , 2 , 4 , 2 , 3 , 1 , 3 , 4/
!
   theta = ecpt(6)*degra
   sinang = sin(theta)
   cosang = cos(theta)
!
!     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
!     SUB TRIANGLES. (2X5) FOR QUADRILATERAL PLATE.
!     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
!
!     ZERO OUT R-MATRIX
!
   DO i = 1 , 10
      requiv(i) = 0.0
   ENDDO
!
!     SHIFT ECPT UP TO MATCH PSTRB1 FOR CERTAIN VARIABLES.
!
   DO i = 6 , 12
      ecpt(i) = ecpt(i+1)
   ENDDO
!
   DO i = 1 , 3
      d1(i) = vq3(i) - vq1(i)
      d2(i) = vq4(i) - vq2(i)
      a1(i) = vq2(i) - vq1(i)
   ENDDO
!
!     NON-NORMALIZED K-VECTOR = D1 CROSS D2
!
   kvect(1) = d1(2)*d2(3) - d2(2)*d1(3)
   kvect(2) = d1(3)*d2(1) - d2(3)*d1(1)
   kvect(3) = d1(1)*d2(2) - d2(1)*d1(2)
!
!     NORMALIZE K-VECTOR
!
   temp = sqrt(kvect(1)**2+kvect(2)**2+kvect(3)**2)
   DO i = 1 , 3
      kvect(i) = kvect(i)/temp
   ENDDO
!
!     COMPUTE H = (A1 DOT KVECT)/2
!
   temp = (a1(1)*kvect(1)+a1(2)*kvect(2)+a1(3)*kvect(3))/2.0
!
!     I-VECTOR =(A1) - H*(KVECT)    NON-NORMALIZED
!
   DO i = 1 , 3
      ivect(i) = a1(i) - temp*kvect(i)
   ENDDO
!
!     NORMALIZE I-VECTOR
!
   temp = sqrt(ivect(1)**2+ivect(2)**2+ivect(3)**2)
   DO i = 1 , 3
      ivect(i) = ivect(i)/temp
   ENDDO
!
!     J-VECTOR = K X I  VECTORS
!
   jvect(1) = kvect(2)*ivect(3) - ivect(2)*kvect(3)
   jvect(2) = kvect(3)*ivect(1) - ivect(3)*kvect(1)
   jvect(3) = kvect(1)*ivect(2) - ivect(1)*kvect(2)
!
!     NORMALIZE J VECTOR TO MAKE SURE
!
   temp = sqrt(jvect(1)**2+jvect(2)**2+jvect(3)**2)
   DO i = 1 , 3
      jvect(i) = jvect(i)/temp
   ENDDO
!
!     X3 GOES INTO R(1,3) = D1 DOT IVECT
!
   r(1,3) = d1(1)*ivect(1) + d1(2)*ivect(2) + d1(3)*ivect(3)
!
!     X2 GOES INTO R(1,2) AND Y3 GOES INTO R(2,3)
!
   r(1,2) = a1(1)*ivect(1) + a1(2)*ivect(2) + a1(3)*ivect(3)
   r(2,3) = d1(1)*jvect(1) + d1(2)*jvect(2) + d1(3)*jvect(3)
!
!     X4 GOES INTO R(1,4) AND Y4 GOES INTO R(2,4)
!
   r(1,4) = d2(1)*ivect(1) + d2(2)*ivect(2) + d2(3)*ivect(3) + r(1,2)
   r(2,4) = d2(1)*jvect(1) + d2(2)*jvect(2) + d2(3)*jvect(3)
!
!     STRESS CALCULATION POINT WHICH IS THE DIAGONALS INTERSECTION.
!
   ftemp = r(1,3)*r(2,4) + r(2,3)*(r(1,2)-r(1,4))
   IF ( ftemp==0.0 ) CALL mesage(-30,26,ecpt(1))
   r(1,5) = r(1,2)*r(1,3)*r(2,4)/ftemp
   r(2,5) = r(1,2)*r(2,3)*r(2,4)/ftemp
!
!     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
!
   IF ( r(2,3)<=0.0 .OR. r(2,4)<=0.0 ) THEN
      CALL mesage(-30,35,ecpt(1))
   ELSE
      temp = r(1,2) - (r(1,2)-r(1,3))*r(2,4)/r(2,3)
      IF ( r(1,4)>=temp ) THEN
         CALL mesage(-30,35,ecpt(1))
      ELSE
         temp = r(2,3)*r(1,4)/r(2,4)
         IF ( r(1,3)<=temp ) CALL mesage(-30,35,ecpt(1))
      ENDIF
   ENDIF
!
!     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT...
!
!     COMPUTE SUB-TRIANGLE COORDINATES
!     CALL BASIC BENDING ROUTINE FOR ALL SUB-TRIANGLES.
!
   DO i = 1 , 60
      ssum(i) = 0.0
   ENDDO
!
   DO j = 1 , 4
      km = 3*j - 3
      subsca = m(km+1)
      subscb = m(km+2)
      subscc = m(km+3)
!
      DO i = 1 , 2
         vv1(i) = r(i,subscb) - r(i,subsca)
         vv2(i) = r(i,subscc) - r(i,subsca)
      ENDDO
      xsubb = sqrt(vv1(1)**2+vv1(2)**2)
      u1 = vv1(1)/xsubb
      u2 = vv1(2)/xsubb
      xsubc = u1*vv2(1) + vv2(2)*u2
      ysubc = u1*vv2(2) - vv2(1)*u2
!
      xc = sqrt((r(1,subsca)-r(1,5))**2+(r(2,subsca)-r(2,5))**2)
      yc = 0.0
!
      sinth = sinang*u1 - cosang*u2
      costh = cosang*u1 + sinang*u2
      IF ( abs(sinth)<1.0E-06 ) sinth = 0.0
!
!     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR TRIANGLE -J-
!
      CALL pstrb1(1)
!
!     RETURNING FROM PSTRB1 THE FOLLOWING QUANTITIES ARE AT HAND.
!
!       S , S , S , EACH 5X3.   45 WORDS STORED IN A(1) THRU A(45)
!        A   B   C
!
!
!     SET UP OF T-MATRIX
!
      t(1) = 1.0
      t(2) = 0.0
      t(3) = 0.0
      t(4) = 0.0
      t(5) = u1
      t(6) = u2
      t(7) = 0.0
      t(8) = -u2
      t(9) = u1
!
!     SET UP V-MATRIX PER FMMS 51-A
!
      v(1) = u1*u1*0.25
      v(2) = u2*u2*0.25
      v(11) = u1*u2*0.25
      v(3) = -v(11)*2.00
      v(4) = 0.0
      v(5) = 0.0
      v(6) = v(2)
      v(7) = v(1)
      v(8) = -v(3)
      v(9) = 0.0
      v(10) = 0.0
      v(12) = -v(11)
      v(13) = v(1) - v(2)
      v(14) = 0.0
      v(15) = 0.0
      v(16) = 0.0
      v(17) = 0.0
      v(18) = 0.0
      v(19) = u1*0.25
      v(20) = -u2*0.25
      v(21) = 0.0
      v(22) = 0.0
      v(23) = 0.0
      v(24) = -v(20)
      v(25) = v(19)
!
!     ADD IN S , S , S   TO THE 4 5X3 SSUM MATRICES
!             A   B   C
!
      DO i = 1 , 3
         CALL gmmats(v,5,5,0,a(15*i-14),5,3,0,temp15)
         CALL gmmats(temp15,5,3,0,t,3,3,0,prod15)
!
!     POINTER TO SSUM MATRIX
!
         npoint = km + i
         npoint = 15*m(npoint) - 15
         DO k = 1 , 15
            nsubc = npoint + k
            ssum(nsubc) = ssum(nsubc) + prod15(k)
         ENDDO
      ENDDO
!
   ENDDO
!
!     FILL E-MATRIX
!
   DO i = 1 , 18
      e(i) = 0.0
   ENDDO
   e(1) = kvect(1)
   e(4) = kvect(2)
   e(7) = kvect(3)
   e(11) = ivect(1)
   e(14) = ivect(2)
   e(17) = ivect(3)
   e(12) = jvect(1)
   e(15) = jvect(2)
   e(18) = jvect(3)
!
   DO i = 1 , 4
!
!     DO WE NEED TRANSFORMATION T
!                                I
      nsubc = 4*i + 10
      IF ( necpt(nsubc)==0 ) THEN
!
         DO k = 1 , 18
            tite(k) = e(k)
         ENDDO
      ELSE
         CALL transs(necpt(nsubc),t)
         CALL gmmats(t,3,3,1,e(1),3,3,0,tite(1))
         CALL gmmats(t,3,3,1,e(10),3,3,0,tite(10))
      ENDIF
!
      CALL gmmats(ssum(15*i-14),5,3,0,tite,6,3,1,ph1out(30*i-21))
!
   ENDDO
!
!     I, Z1, Z2, ELEM ID, 4 SILS FOR PHASE 2
!
   ph1out(1) = ecpt(1)
   ph1out(2) = ecpt(2)
   ph1out(3) = ecpt(3)
   ph1out(4) = ecpt(4)
   ph1out(5) = ecpt(5)
   ph1out(6) = ecpt(7)
   ph1out(7) = ecpt(11)
   ph1out(8) = ecpt(12)
!
!     ALL PHASE ONE COMPLETE
!
END SUBROUTINE psqpl1
