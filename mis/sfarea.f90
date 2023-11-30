
SUBROUTINE sfarea(Ngpt,V,G)
   IMPLICIT NONE
   REAL Dummy(16) , Surfac , Volume
   COMMON /blank / Dummy , Volume , Surfac
   INTEGER Ngpt
   REAL G(1) , V(1)
   REAL a(6) , s2d8 , tetra , trim6 , trpl1 , trshl
   REAL area
   INTEGER i , j , jj , jx(6) , k , kk , kx(12) , l , m , n , narea , sub(2) , type(6)
!
!     THIS SUBROUTINE IS CALLED ONLY BY EMGFIN TO COMPUTE THE SURFACE
!     AREAS OF THE SOLID AND PLATE ELEMENTS
!     NOTE - THE INPUT VALUE OF NGPT (NO. OF GRID POINTS) WILL BE
!     CHANGED TO NO. OF SURFACE AREAS (OUTPUT) BY THIS ROUTINE
!
!     DEFINITION OF SURFACES ( 1 THRU 6)
!
!     LET CORNER POINTS 1, 2, 3, 4 ON THE BOTTOM SURFACE OF A CUBE,
!     AND 5, 6, 7, AND 8 ARE ON TOP SURFACE. CORNER POINT 5 IS ON TOP
!     OF POINT 1, THEN FOR SOLID (BRICK) ELEMENTS -
!
!       FACE       CORNER POINTS
!     -------     ---------------
!        1          1  2  3  4
!        2          1  2  6  5
!        3          2  3  7  6
!        4          3  4  8  7
!        5          4  1  5  8
!        6          5  6  7  8
!
!     IN WEDGE AND TETRA, FACE 1 CONTAINS CORNER POINTS 1, 2, AND 3,
!     FACE 2 IS MADE UP OF 1, 2, AND M, WHERE M IS A CORNER POINT NOT
!     ON FACE 1, AND SIMILARLY, FACE 3 HOLDS CONNER POINTS 2, 3, AND N,
!     AND SO ON.
!
!     PLATE (TRIANG AND QUAD) ELEMENTS HAVE ONE SURFACE. MASS AND VOLUME
!     ARE COMPUTED HERE FOR THESE ELEMENTS.
!
   DATA tetra , s2d8 , trim6 , trpl1 , trshl/4HCTET , 4HCIS2 , 4HCTRI , 4HCTRP , 4HCTRS/
   DATA jx/129 , 133 , 137 , 141 , 145 , 149/
   DATA type/4 , 3 , 8 , 6 , 20 , 32/
   DATA kx/9 , 33 , 5 , 89 , 17 , 101 , 29 , 113 , 41 , 125 , 89 , 113/
   DATA sub/4HSFAR , 4HEA  /
!
!     AREA(I,J,K)=.5*SQRT(
!    1 ((G(J+2)-G(I+2))*(G(K+3)-G(I+3))-(G(J+3)-G(I+3))*(G(K+2)-G(I+2)))
!    2 **2
!    3+((G(J+3)-G(I+3))*(G(K+1)-G(I+1))-(G(J+1)-G(I+1))*(G(K+3)-G(I+3)))
!    4 **2
!    5+((G(J+1)-G(I+1))*(G(K+2)-G(I+2))-(G(J+2)-G(I+2))*(G(K+1)-G(I+1)))
!    6 **2)
!
!     (THE ABOVE FUNCTION MAY BE TOO LONG FOR SOME MACHINE THAT
!      WOULD CREATE PROBLEM IN COMPILING. SO MOVE IT OUT AND MAKE
!      IT AN EXTERNAL FUNCTION. AND ADD A 'G,' INSIDE ARG. LIST)
!
!
!     1 2 3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 - GRID PT
!     1 5 9 13 17 21 25 29 33 37 41 45 49 53 57 61 65 69 73 77 - POINTER
!
!     21 22 23 24 25  26  27  28  29  30  31  32  C1  C2  C3  C4  C5  C6
!     81 85 89 93 97 101 105 109 113 117 121 125 129 133 137 141 145 149
!     (WHERE C1, C2, ..., C6 ARE CENTER POINTS ON FACES 1, 2, ..., 6)
!
   DO l = 1 , 6
      IF ( Ngpt==type(l) ) THEN
         IF ( l==1 ) GOTO 700
         IF ( l==2 ) GOTO 800
         IF ( l==3 ) GOTO 300
         IF ( l==4 ) GOTO 200
         IF ( l==5 ) GOTO 400
         IF ( l==6 ) GOTO 500
      ENDIF
!                   NO. OF GRID PT =  4,  3, 8, 6,20,32
   ENDDO
   CALL mesage(-37,0,sub)
!
!     4-GRID ELEMENT (TETRA)
!
 100  a(1) = area(G,1,5,9)
   a(2) = area(G,1,5,13)
   a(3) = area(G,5,9,13)
   a(4) = area(G,1,9,13)
   narea = 4
   GOTO 1000
!
!     6-GRID ELEMENT (WEDGE)
!
 200  IF ( V(1)==trim6 .OR. V(1)==trpl1 .OR. V(1)==trshl ) THEN
!
!     6-GRID TRIANGULAR ELEMENTS (TRIM6, TRPLT1, TRSHL)
!
      i = 129
      j = 21
      k = 9
      DO l = 1 , 3
         G(l+i) = G(l+j) + (G(l+k)-G(l+j))*.33333
      ENDDO
      a(1) = area(G,1,5,129) + area(G,5,9,129) + area(G,9,13,129) + area(G,13,17,129) + area(G,17,21,129) + area(G,21,1,129)
      GOTO 900
   ELSE
      a(1) = area(G,1,5,9)
      a(2) = area(G,1,5,13) + area(G,13,17,5)
      a(3) = area(G,5,9,17) + area(G,17,21,9)
      a(4) = area(G,1,9,13) + area(G,9,13,21)
      a(5) = area(G,13,17,21)
      narea = 5
      GOTO 1000
   ENDIF
!
!     8-GIRD ELEMENT
!
 300  IF ( V(1)==s2d8 ) THEN
!
!     8-GRID ELEMENT (IS2D8)
!
      j = 33
      a(1) = G(j)
      GOTO 900
   ELSE
      a(1) = area(G,1,5,9) + area(G,1,9,13)
      a(2) = area(G,1,5,17) + area(G,5,17,21)
      a(3) = area(G,5,21,25) + area(G,5,25,9)
      a(4) = area(G,9,25,29) + area(G,9,29,13)
      a(5) = area(G,13,1,29) + area(G,1,29,17)
      a(6) = area(G,17,21,25) + area(G,17,25,29)
      GOTO 600
   ENDIF
!
!     20-GRID ELEMENT
!
 400  a(1) = area(G,1,5,29) + area(G,29,5,13) + area(G,13,5,9) + area(G,13,17,21) + area(G,13,21,29) + area(G,29,25,21)
   a(2) = area(G,1,5,33) + area(G,33,5,53) + area(G,53,33,49) + area(G,37,57,53) + area(G,53,37,5) + area(G,5,9,37)
   a(3) = area(G,9,37,13) + area(G,13,37,61) + area(G,61,37,57) + area(G,61,65,41) + area(G,41,61,13) + area(G,13,41,17)
   a(4) = area(G,17,41,21) + area(G,21,69,41) + area(G,41,65,69) + area(G,69,73,45) + area(G,45,69,21) + area(G,21,45,25)
   a(5) = area(G,1,33,29) + area(G,29,77,33) + area(G,33,49,77) + area(G,77,73,45) + area(G,45,77,29) + area(G,29,45,25)
   a(6) = area(G,49,53,77) + area(G,77,53,61) + area(G,61,53,57) + area(G,61,65,69) + area(G,69,61,77) + area(G,77,69,73)
   GOTO 600
!
!     32-GRID ELEMENT
!
 500  DO l = 1 , 6
      a(l) = 0.0
   ENDDO
   kk = 1
   DO l = 129 , 152 , 4
      m = kx(kk)
      n = kx(kk+1)
      DO jj = 1 , 3
         G(l+jj) = 0.5*(G(m+jj)+G(n+jj))
      ENDDO
      kk = kk + 2
   ENDDO
   jj = 2
   DO l = 1 , 12
      m = l*4 - 3
      n = m + 4
      IF ( n>48 ) n = 1
      a(1) = a(1) + area(G,m,n,jx(1))
      a(jj) = a(jj) + area(G,m,n,jx(jj))
      m = (l+20)*4 - 3
      n = m + 4
      IF ( n>128 ) n = 81
      a(6) = a(6) + area(G,m,n,jx(6))
      a(jj) = a(jj) + area(G,m,n,jx(jj))
      IF ( mod(l,3)==0 ) jj = jj + 1
   ENDDO
   a(2) = a(2) + area(G,1,49,133) + area(G,49,65,133) + area(G,65,81,133) + area(G,13,53,133) + area(G,53,69,133)                   &
        & + area(G,69,93,133)
   a(3) = a(3) + area(G,13,53,137) + area(G,53,69,137) + area(G,69,93,137) + area(G,25,57,137) + area(G,57,73,137)                  &
        & + area(G,73,105,137)
   a(4) = a(4) + area(G,25,57,141) + area(G,57,73,141) + area(G,73,105,141) + area(G,37,61,141) + area(G,61,77,141)                 &
        & + area(G,77,117,141)
   a(5) = a(5) + area(G,37,61,145) + area(G,61,77,145) + area(G,77,117,145) + area(G,1,49,145) + area(G,49,65,145)                  &
        & + area(G,65,81,145)
 600  narea = 6
   GOTO 1000
!
!     4-GRID ELEMENT (QUAD)
!
 700  IF ( V(1)==tetra ) GOTO 100
   a(1) = area(G,1,5,9) + area(G,1,5,13)
   GOTO 900
!
!     3-GRID ELEMENT
!
 800  a(1) = area(G,1,5,9)
 900  narea = 1
!
!     AT THIS POINT, V(4) AND V(5) ARE THICKNESS AND DENSITY OF THE
!     PLATE. COMPUTE VOLUME AND MASS AND PUT THEM BACK IN V(4) AND V(5)
!
   IF ( Volume>0.0 ) THEN
      j = 4
      V(j+1) = a(1)*V(j)*V(j+1)
      V(j) = a(1)*V(j)*Volume
   ENDIF
!
 1000 Ngpt = narea
   DO l = 1 , narea
      V(l+5) = a(l)*Surfac
   ENDDO
END SUBROUTINE sfarea
