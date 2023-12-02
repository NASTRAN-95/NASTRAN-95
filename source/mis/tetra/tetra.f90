!*==tetra.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tetra(Temps,Pg,Iopt)
   USE c_matin
   USE c_matout
   USE c_system
   USE c_trimex
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: Temps
   REAL , DIMENSION(6) :: Pg
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: alfa , p
   REAL , DIMENSION(72) :: c
   REAL , DIMENSION(18) :: ctg
   REAL :: dum , hdeter , temp1
   REAL , DIMENSION(36) :: g
   REAL , DIMENSION(16) :: h
   INTEGER :: i , ising , j , k , l
   INTEGER , DIMENSION(2) :: necpt
   REAL , DIMENSION(12) :: temp
   EXTERNAL basglb , gmmats , invers , mat , mesage
!
! End of declarations rewritten by SPAG
!
!
!     ELEMENT THERMAL LOAD GENERATOR FOR THE TETRAHEDRON SOLID ELEMENT
!
!     LOOKING DOWN ON THIS ELEMENT, GRIDS 1,2,3 ARE THE BASE AND MUST BE
!     LABELED COUNTERCLOCKWISE. GRID 4 MUST BE ABOVE THE PLANE FORMED BY
!     GRIDS 1,2,3 AND CLOSEST TO THIS OBSERVER.
!
!     ECPT FOR THE TETRAHEDRON SOLID ELEMENT
!
!     ECPT( 1) = ELEMENT ID
!     ECPT( 4) = SIL GRID POINT 3
!     ECPT( 5) = SIL GRID POINT 4
!     ECPT( 2) = MATERIAL ID (MAT1 MATERIAL TYPE)
!     ECPT( 3) = SIL GRID POINT 1
!     ECPT( 4) = SIL GRID POINT 2
!     ECPT( 5) = SIL GRID POINT 3
!     ECPT( 6) = SIL GRID POINT 4
!     ECPT( 7) = COORD SYS ID GRID PT 1
!     ECPT( 8) = X1
!     ECPT( 9) = Y1
!     ECPT(10) = Z1
!     ECPT(11) = COORD SYS ID GRID PT 2
!     ECPT(12) = X2
!     ECPT(13) = Y2
!     ECPT(14) = Z2
!     ECPT(15) = COORD SYS ID GRID PT 3
!     ECPT(16) = X3
!     ECPT(17) = Y3
!     ECPT(18) = Z3
!     ECPT(19) = COORD SYS ID GRID PT 4
!     ECPT(20) = X4
!     ECPT(21) = Y4
!     ECPT(22) = Z4
!     ECPT(23) = ELEMENT TEMPERATURE
!
   !>>>>EQUIVALENCE (Necpt(1),Ecpt(1))
!
!     FILL THE 4 X 4 H MATRIX.
!
   h(1) = 1.0
   h(2) = ecpt(8)
   h(3) = ecpt(9)
   h(4) = ecpt(10)
   h(5) = 1.0
   h(6) = ecpt(12)
   h(7) = ecpt(13)
   h(8) = ecpt(14)
   h(9) = 1.0
   h(10) = ecpt(16)
   h(11) = ecpt(17)
   h(12) = ecpt(18)
   h(13) = 1.0
   h(14) = ecpt(20)
   h(15) = ecpt(21)
   h(16) = ecpt(22)
!
!     INVERT H AND GET THE DETERMINANT
!
   ising = 0
!
   CALL invers(4,h(1),4,dum,0,hdeter,ising,temp(1))
!
!     IF THE MATRIX IS SINGULAR TETRAHEDRON IS BAD
!
   hdeter = abs(hdeter)
   IF ( ising/=2 ) THEN
!
!     GET THE MATERIAL DATA AND FILL THE 6X6 G MATERIAL STRESS-STRAIN
!     MATRIX.
!
      inflag = 1
      matid = necpt(2)
      eltemp = ecpt(23)
      CALL mat(necpt(1))
      DO i = 1 , 36
         g(i) = 0.0
      ENDDO
      temp1 = (1.0+nu)*(1.0-2.0*nu)
      IF ( temp1/=0.0 ) THEN
         g(1) = e*(1.0-nu)/temp1
         g(8) = g(1)
         g(15) = g(1)
         g(2) = e*nu/temp1
         g(3) = g(2)
         g(7) = g(2)
         g(9) = g(2)
         g(13) = g(2)
         g(14) = g(2)
         g(22) = gg
         g(29) = gg
         g(36) = gg
!
!     FILL 4 C-MATRICES. (6X3) EACH.
!
         DO i = 1 , 72
            c(i) = 0.0
         ENDDO
         DO i = 1 , 4
            j = 18*i - 18
            c(j+1) = h(i+4)
            c(j+5) = h(i+8)
            c(j+9) = h(i+12)
            c(j+11) = h(i+12)
            c(j+12) = h(i+8)
            c(j+13) = h(i+12)
            c(j+15) = h(i+4)
            c(j+16) = h(i+8)
            c(j+17) = h(i+4)
         ENDDO
!
!     DIVIDE DETERMINANT BY 6.0, AND BY AN ADDITIONAL 2.0 IF A SUB-TETRA
!     FOR THE HEXA-10 ELEMENT.
!
         IF ( Iopt/=0 ) THEN
            hdeter = hdeter/12.0
         ELSE
            hdeter = hdeter/6.0
         ENDIF
!
!     INTRODUCE TBAR AND ALPHA
!
         hdeter = hdeter*(0.25*(Temps(1)+Temps(2)+Temps(3)+Temps(4))-tsub0)*alpha
!
!     FILL ALPHA VECTOR
!
         alfa(1) = hdeter
         alfa(2) = hdeter
         alfa(3) = hdeter
         alfa(4) = 0.0
         alfa(5) = 0.0
         alfa(6) = 0.0
!
!     LOOP FOR THE FOUR GRID POINTS
!
         DO i = 1 , 4
            CALL gmmats(c(18*i-17),6,3,1,g(1),6,6,0,ctg(1))
            CALL gmmats(ctg(1),3,6,0,alfa(1),6,1,0,p(1))
!
!     TRANSFORM TO GLOBAL
!
            p(4) = 0.0
            p(5) = 0.0
            p(6) = 0.0
            k = 4*i + 3
            IF ( necpt(k)/=0 ) CALL basglb(p(1),p(1),necpt(k+1),necpt(k))
!
!     INSERT LOAD VECTOR FOR GRID POINT
!
            l = necpt(i+2) - 1
            DO j = 1 , 3
               l = l + 1
               Pg(l) = Pg(l) + p(j)
            ENDDO
         ENDDO
         RETURN
      ELSE
         WRITE (out,99001) ufm , matid , ecpt(1)
99001    FORMAT (A23,' 4003, AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ','UNDER MATERIAL ID =',I9,' FOR ELEMENT ID =',I9)
!
         CALL mesage(-61,0,0)
      ENDIF
   ELSE
      WRITE (out,99002) ufm , necpt(1)
99002 FORMAT (A23,' 4002, MODULE SSG1 DETECTS BAD OR REVERSE GEOMETRY ','FOR ELEMENT ID =',I9)
      CALL mesage(-61,0,0)
   ENDIF
END SUBROUTINE tetra
