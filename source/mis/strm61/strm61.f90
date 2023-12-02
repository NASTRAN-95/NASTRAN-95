!*==strm61.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strm61
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_SDR2X5
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(9) :: balotr , emod , trans
   REAL , SAVE :: blank , degra
   REAL :: determ , dista , distb , distc , nsm , theta1 , thetam , tmem1 , tmem3 , tmem5 , to , x , y
   REAL , DIMENSION(6) :: e , ee1 , eph1 , xc , yc , zc
   INTEGER :: i , i1 , idele , ii , ij , ij1 , ising , j , j1 , j2 , jj , matid1 , mz
   INTEGER , DIMENSION(6) :: ics , nl
   INTEGER , DIMENSION(45) :: iest
   INTEGER , DIMENSION(6,3) :: ind
   REAL , DIMENSION(3) :: ivect , jvect , kvect
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(6,6) :: q
   REAL , DIMENSION(36) :: qq , tmm
   REAL , DIMENSION(3,12) :: tm
   EXTERNAL gmmats , invers , mat , transs , trif
!
! End of declarations rewritten by SPAG
!
!
!
!   PHASE I OF  STRESS DATA RECOVERY FOR TRIANGULAR MEMBRANE ELEMENT TRI
!
!   OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING
!
!   1) ELEMENT ID             WORDS    1    STORAGE IN PH1OUT   1
!   2) SIX SILS               WORDS    6                        2-7
!   3) THICKNESS T1           WORDS    1                        8
!   4) THICKNESS T2           WORDS    1                        9
!   5) THICKNESS T3           WORDS    1                       10
!   6) REFERENCE TEMP T0      WORDS    1                       11
!   7) S SUB I MATRICES       WORDS    216                     12-227
!   8) THERMAL VECTOR G ALF   WORDS    3                      228-230
!
!    EST ENTRIES SAME AS IN SUBROUTINE KTRM6S
!
!
!
!
!
   !>>>>EQUIVALENCE (Nph1ou(1),Ph1out(1)) , (Iest(1),Est(1))
   !>>>>EQUIVALENCE (tm(1,1),tmm(1))
!
   DATA name/4HSTRM , 4H61  / , blank/4H    /
   DATA degra/0.0174532925/
!
   idele = iest(1)
   DO i = 1 , 6
      nl(i) = iest(i+1)
   ENDDO
   thetam = Est(8)
   matid1 = iest(9)
   tmem1 = Est(10)
   tmem3 = Est(11)
   tmem5 = Est(12)
!
!   IF TMEM3 OR TMEM5 IS 0.0 OR BLANK , IT WILL BE SET EQUAL TO TMEM1
!
   IF ( tmem3==0.0 .OR. tmem3==blank ) tmem3 = tmem1
!
   IF ( tmem5==0.0 .OR. tmem5==blank ) tmem5 = tmem1
!
   nsm = Est(13)
!
   j = 0
   DO i = 14 , 34 , 4
      j = j + 1
      ics(j) = iest(i)
      xc(j) = Est(i+1)
      yc(j) = Est(i+2)
      zc(j) = Est(i+3)
   ENDDO
   Eltemp = (Est(38)+Est(39)+Est(40)+Est(41)+Est(42)+Est(43))/6.0
   theta1 = thetam*degra
   Sinth = sin(theta1)
   Costh = cos(theta1)
   IF ( abs(Sinth)<=1.0E-06 ) Sinth = 0.0
!
!
!   EVALUATE MATERIAL PROPERTIES
!
   Matflg = 2
   Matid = matid1
   CALL mat(idele)
   to = Tref
!
!   CALCULATIONS FOR THE TRIANGLE
!
   CALL trif(xc,yc,zc,ivect,jvect,kvect,dista,distb,distc,iest(1),name)
!
!   TRANSFORMATION MATRIX BETWEEN ELEMENT AND BASIC CO-ORDINATES
!
   e(1) = ivect(1)
   e(2) = jvect(1)
   e(3) = ivect(2)
   e(4) = jvect(2)
   e(5) = ivect(3)
   e(6) = jvect(3)
!
!   CALCULATIONS FOR  Q MATRIX AND ITS INVERSE
!
   DO i = 1 , 6
      DO j = 1 , 6
         q(i,j) = 0.0
      ENDDO
   ENDDO
   DO i = 1 , 6
      q(i,1) = 1.0
      q(i,2) = xc(i)
      q(i,3) = yc(i)
      q(i,4) = xc(i)*xc(i)
      q(i,5) = xc(i)*yc(i)
      q(i,6) = yc(i)*yc(i)
   ENDDO
!
!     FIND INVERSE OF Q  MATRIX
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   ising = -1
   CALL invers(6,q,6,qq(1),0,determ,ising,ind)
!
!   ISING EQUAL TO 2 IMPLIES THAT Q MATRIX IS SINGULAR
!
   DO i = 1 , 6
      DO j = 1 , 6
         ij = (i-1)*6 + j
         qq(ij) = q(i,j)
      ENDDO
   ENDDO
   DO i = 1 , 9
      balotr(i) = 0.0
   ENDDO
!
   DO i = 1 , 7
      Ph1out(i) = Est(i)
   ENDDO
   Ph1out(8) = Est(10)
   Ph1out(9) = Est(11)
   Ph1out(10) = Est(12)
   Ph1out(11) = to
   emod(1) = Em(1)
   emod(2) = Em(2)
   emod(3) = Em(3)
   emod(4) = Em(2)
   emod(5) = Em(4)
   emod(6) = Em(5)
   emod(7) = Em(3)
   emod(8) = Em(5)
   emod(9) = Em(6)
!
!   STRESSES AND STRAINS ARE EVALUATED AT FOUR POINTS ,VIZ., THE THREE
!   CORNER GRID POINTS AND THE CENTROID
!
   DO jj = 1 , 4
      j = 2*(jj-1) + 1
      IF ( j==7 ) THEN
         x = (xc(1)+xc(3)+xc(5))/3.0
         y = (yc(1)+yc(3)+yc(5))/3.0
      ELSE
         x = xc(j)
         y = yc(j)
      ENDIF
      DO i = 1 , 36
         tmm(i) = 0.0E0
      ENDDO
!
!   TM MATRIX IS THE PRODUCT OF B AND QINVERSE MATRICES
!
      DO j = 1 , 6
         j1 = (j-1)*2 + 1
         j2 = j1 + 1
         tm(1,j1) = q(2,j) + 2.0*x*q(4,j) + y*q(5,j)
         tm(2,j2) = q(3,j) + x*q(5,j) + 2.0*y*q(6,j)
         tm(3,j1) = tm(2,j2)
         tm(3,j2) = tm(1,j1)
      ENDDO
      DO ii = 1 , 6
         IF ( ics(ii)==0 ) THEN
            DO i = 1 , 3
               DO j = 1 , 2
                  i1 = (i-1)*2 + j
                  j1 = (j-1)*3 + i
                  ee1(j1) = e(i1)
               ENDDO
            ENDDO
         ELSE
            CALL transs(iest(4*ii+10),trans)
            CALL gmmats(e,3,2,+1,trans,3,3,0,ee1)
         ENDIF
         ij1 = (jj-1)*54 + (ii-1)*9 + 12
         mz = (ii-1)*6 + 1
         CALL gmmats(emod,3,3,0,tmm(mz),2,3,+1,eph1)
         CALL gmmats(eph1,3,2,0,ee1,2,3,0,Ph1out(ij1))
      ENDDO
   ENDDO
   CALL gmmats(emod,3,3,0,Alf,3,1,0,Ph1out(228))
END SUBROUTINE strm61
