!*==sfarea.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sfarea(Ngpt,V,G)
   USE c_blank
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ngpt
   REAL , DIMENSION(1) :: V
   REAL , DIMENSION(1) :: G
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: a
   INTEGER :: i , j , jj , k , kk , l , m , n , narea
   INTEGER , DIMENSION(6) , SAVE :: jx , type
   INTEGER , DIMENSION(12) , SAVE :: kx
   REAL , SAVE :: s2d8 , tetra , trim6 , trpl1 , trshl
   INTEGER , DIMENSION(2) , SAVE :: sub
   EXTERNAL area , mesage
!
! End of declarations rewritten by SPAG
!
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
         IF ( l==1 ) THEN
            CALL spag_block_7
            RETURN
         ENDIF
         IF ( l==2 ) THEN
            CALL spag_block_8
            RETURN
         ENDIF
         IF ( l==3 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( l==4 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( l==5 ) THEN
            CALL spag_block_4
            RETURN
         ENDIF
         IF ( l==6 ) THEN
            CALL spag_block_5
            RETURN
         ENDIF
      ENDIF
!                   NO. OF GRID PT =  4,  3, 8, 6,20,32
   ENDDO
   CALL mesage(-37,0,sub)
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     4-GRID ELEMENT (TETRA)
!
      A(1) = area(G,1,5,9)
      A(2) = area(G,1,5,13)
      A(3) = area(G,5,9,13)
      A(4) = area(G,1,9,13)
      Narea = 4
      CALL spag_block_10
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     6-GRID ELEMENT (WEDGE)
!
      IF ( V(1)==Trim6 .OR. V(1)==Trpl1 .OR. V(1)==Trshl ) THEN
!
!     6-GRID TRIANGULAR ELEMENTS (TRIM6, TRPLT1, TRSHL)
!
         I = 129
         J = 21
         K = 9
         DO L = 1 , 3
            G(L+I) = G(L+J) + (G(L+K)-G(L+J))*.33333
         ENDDO
         A(1) = area(G,1,5,129) + area(G,5,9,129) + area(G,9,13,129) + area(G,13,17,129) + area(G,17,21,129) + area(G,21,1,129)
         CALL spag_block_9
         RETURN
      ELSE
         A(1) = area(G,1,5,9)
         A(2) = area(G,1,5,13) + area(G,13,17,5)
         A(3) = area(G,5,9,17) + area(G,17,21,9)
         A(4) = area(G,1,9,13) + area(G,9,13,21)
         A(5) = area(G,13,17,21)
         Narea = 5
         CALL spag_block_10
         RETURN
      ENDIF
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!
!     8-GIRD ELEMENT
!
      IF ( V(1)==S2d8 ) THEN
!
!     8-GRID ELEMENT (IS2D8)
!
         J = 33
         A(1) = G(J)
         CALL spag_block_9
         RETURN
      ELSE
         A(1) = area(G,1,5,9) + area(G,1,9,13)
         A(2) = area(G,1,5,17) + area(G,5,17,21)
         A(3) = area(G,5,21,25) + area(G,5,25,9)
         A(4) = area(G,9,25,29) + area(G,9,29,13)
         A(5) = area(G,13,1,29) + area(G,1,29,17)
         A(6) = area(G,17,21,25) + area(G,17,25,29)
         CALL spag_block_6
         RETURN
      ENDIF
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
!
!     20-GRID ELEMENT
!
      A(1) = area(G,1,5,29) + area(G,29,5,13) + area(G,13,5,9) + area(G,13,17,21) + area(G,13,21,29) + area(G,29,25,21)
      A(2) = area(G,1,5,33) + area(G,33,5,53) + area(G,53,33,49) + area(G,37,57,53) + area(G,53,37,5) + area(G,5,9,37)
      A(3) = area(G,9,37,13) + area(G,13,37,61) + area(G,61,37,57) + area(G,61,65,41) + area(G,41,61,13) + area(G,13,41,17)
      A(4) = area(G,17,41,21) + area(G,21,69,41) + area(G,41,65,69) + area(G,69,73,45) + area(G,45,69,21) + area(G,21,45,25)
      A(5) = area(G,1,33,29) + area(G,29,77,33) + area(G,33,49,77) + area(G,77,73,45) + area(G,45,77,29) + area(G,29,45,25)
      A(6) = area(G,49,53,77) + area(G,77,53,61) + area(G,61,53,57) + area(G,61,65,69) + area(G,69,61,77) + area(G,77,69,73)
      CALL spag_block_6
   END SUBROUTINE spag_block_4
   SUBROUTINE spag_block_5
!
!     32-GRID ELEMENT
!
      DO L = 1 , 6
         A(L) = 0.0
      ENDDO
      Kk = 1
      DO L = 129 , 152 , 4
         M = Kx(Kk)
         N = Kx(Kk+1)
         DO Jj = 1 , 3
            G(L+Jj) = 0.5*(G(M+Jj)+G(N+Jj))
         ENDDO
         Kk = Kk + 2
      ENDDO
      Jj = 2
      DO L = 1 , 12
         M = L*4 - 3
         N = M + 4
         IF ( N>48 ) N = 1
         A(1) = A(1) + area(G,M,N,Jx(1))
         A(Jj) = A(Jj) + area(G,M,N,Jx(Jj))
         M = (L+20)*4 - 3
         N = M + 4
         IF ( N>128 ) N = 81
         A(6) = A(6) + area(G,M,N,Jx(6))
         A(Jj) = A(Jj) + area(G,M,N,Jx(Jj))
         IF ( mod(L,3)==0 ) Jj = Jj + 1
      ENDDO
      A(2) = A(2) + area(G,1,49,133) + area(G,49,65,133) + area(G,65,81,133) + area(G,13,53,133) + area(G,53,69,133)                &
           & + area(G,69,93,133)
      A(3) = A(3) + area(G,13,53,137) + area(G,53,69,137) + area(G,69,93,137) + area(G,25,57,137) + area(G,57,73,137)               &
           & + area(G,73,105,137)
      A(4) = A(4) + area(G,25,57,141) + area(G,57,73,141) + area(G,73,105,141) + area(G,37,61,141) + area(G,61,77,141)              &
           & + area(G,77,117,141)
      A(5) = A(5) + area(G,37,61,145) + area(G,61,77,145) + area(G,77,117,145) + area(G,1,49,145) + area(G,49,65,145)               &
           & + area(G,65,81,145)
      CALL spag_block_6
   END SUBROUTINE spag_block_5
   SUBROUTINE spag_block_6
      Narea = 6
      CALL spag_block_10
   END SUBROUTINE spag_block_6
   SUBROUTINE spag_block_7
!
!     4-GRID ELEMENT (QUAD)
!
      IF ( V(1)==Tetra ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      A(1) = area(G,1,5,9) + area(G,1,5,13)
      CALL spag_block_9
   END SUBROUTINE spag_block_7
   SUBROUTINE spag_block_8
!
!     3-GRID ELEMENT
!
      A(1) = area(G,1,5,9)
      CALL spag_block_9
   END SUBROUTINE spag_block_8
   SUBROUTINE spag_block_9
      Narea = 1
!
!     AT THIS POINT, V(4) AND V(5) ARE THICKNESS AND DENSITY OF THE
!     PLATE. COMPUTE VOLUME AND MASS AND PUT THEM BACK IN V(4) AND V(5)
!
      IF ( volume>0.0 ) THEN
         J = 4
         V(J+1) = A(1)*V(J)*V(J+1)
         V(J) = A(1)*V(J)*volume
      ENDIF
      CALL spag_block_10
   END SUBROUTINE spag_block_9
   SUBROUTINE spag_block_10
!
      Ngpt = Narea
      DO L = 1 , Narea
         V(L+5) = A(L)*surfac
      ENDDO
   END SUBROUTINE spag_block_10
END SUBROUTINE sfarea
