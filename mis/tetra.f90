
SUBROUTINE tetra(Temps,Pg,Iopt)
   IMPLICIT NONE
   REAL Alpha , E , Ecpt(23) , Eltemp , Gg , Gsube , Nu , Rho , Sigc , Sigs , Sigt , Sysbuf , Tsub0
   INTEGER Inflag , Matid , Necpt(2) , Out
   CHARACTER*23 Ufm
   COMMON /matin / Matid , Inflag , Eltemp
   COMMON /matout/ E , Gg , Nu , Rho , Alpha , Tsub0 , Gsube , Sigt , Sigc , Sigs
   COMMON /system/ Sysbuf , Out
   COMMON /trimex/ Ecpt
   COMMON /xmssg / Ufm
   INTEGER Iopt
   REAL Pg(6) , Temps(4)
   REAL alfa(6) , c(72) , ctg(18) , dum , g(36) , h(16) , hdeter , p(6) , temp(12) , temp1
   INTEGER i , ising , j , k , l
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
   EQUIVALENCE (Necpt(1),Ecpt(1))
!
!     FILL THE 4 X 4 H MATRIX.
!
   h(1) = 1.0
   h(2) = Ecpt(8)
   h(3) = Ecpt(9)
   h(4) = Ecpt(10)
   h(5) = 1.0
   h(6) = Ecpt(12)
   h(7) = Ecpt(13)
   h(8) = Ecpt(14)
   h(9) = 1.0
   h(10) = Ecpt(16)
   h(11) = Ecpt(17)
   h(12) = Ecpt(18)
   h(13) = 1.0
   h(14) = Ecpt(20)
   h(15) = Ecpt(21)
   h(16) = Ecpt(22)
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
      Inflag = 1
      Matid = Necpt(2)
      Eltemp = Ecpt(23)
      CALL mat(Necpt(1))
      DO i = 1 , 36
         g(i) = 0.0
      ENDDO
      temp1 = (1.0+Nu)*(1.0-2.0*Nu)
      IF ( temp1/=0.0 ) THEN
         g(1) = E*(1.0-Nu)/temp1
         g(8) = g(1)
         g(15) = g(1)
         g(2) = E*Nu/temp1
         g(3) = g(2)
         g(7) = g(2)
         g(9) = g(2)
         g(13) = g(2)
         g(14) = g(2)
         g(22) = Gg
         g(29) = Gg
         g(36) = Gg
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
         hdeter = hdeter*(0.25*(Temps(1)+Temps(2)+Temps(3)+Temps(4))-Tsub0)*Alpha
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
            IF ( Necpt(k)/=0 ) CALL basglb(p(1),p(1),Necpt(k+1),Necpt(k))
!
!     INSERT LOAD VECTOR FOR GRID POINT
!
            l = Necpt(i+2) - 1
            DO j = 1 , 3
               l = l + 1
               Pg(l) = Pg(l) + p(j)
            ENDDO
         ENDDO
         RETURN
      ELSE
         WRITE (Out,99001) Ufm , Matid , Ecpt(1)
99001    FORMAT (A23,' 4003, AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ','UNDER MATERIAL ID =',I9,' FOR ELEMENT ID =',I9)
!
         CALL mesage(-61,0,0)
      ENDIF
   ELSE
      WRITE (Out,99002) Ufm , Necpt(1)
99002 FORMAT (A23,' 4002, MODULE SSG1 DETECTS BAD OR REVERSE GEOMETRY ','FOR ELEMENT ID =',I9)
      CALL mesage(-61,0,0)
   ENDIF
END SUBROUTINE tetra
