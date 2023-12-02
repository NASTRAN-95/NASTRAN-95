!*==hdvs1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hdvs1(A,La,Ir)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: A
   INTEGER :: La
   INTEGER , DIMENSION(1) :: Ir
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ij , it , itt , j , k , l , m , t , tt
   INTEGER , DIMENSION(21) :: il , iu
   REAL :: r
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!                                  FIRST EXECUTABLE STATEMENT
         IF ( La<=0 ) RETURN
         m = 1
         i = 1
         j = La
         r = .375
         spag_nextblock_1 = 2
      CASE (2)
         IF ( i==j ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( r>.5898437 ) THEN
            r = r - .21875
         ELSE
            r = r + 3.90625E-2
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         k = i
!                                  SELECT A CENTRAL ELEMENT OF THE
!                                  ARRAY AND SAVE IT IN LOCATION T
         ij = i + (j-i)*r
         t = A(ij)
         it = Ir(ij)
!                                  IF FIRST ELEMENT OF ARRAY IS GREATER
!                                  THAN T, INTERCHANGE WITH T
         IF ( A(i)>t ) THEN
            A(ij) = A(i)
            A(i) = t
            t = A(ij)
            Ir(ij) = Ir(i)
            Ir(i) = it
            it = Ir(ij)
         ENDIF
         l = j
!                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
!                                  T, INTERCHANGE WITH T
         IF ( A(j)<t ) THEN
            A(ij) = A(j)
            A(j) = t
            t = A(ij)
            Ir(ij) = Ir(j)
            Ir(j) = it
            it = Ir(ij)
!                                  IF FIRST ELEMENT OF ARRAY IS GREATER
!                                  THAN T, INTERCHANGE WITH T
            IF ( A(i)>t ) THEN
               A(ij) = A(i)
               A(i) = t
               t = A(ij)
               Ir(ij) = Ir(i)
               Ir(i) = it
               it = Ir(ij)
            ENDIF
         ENDIF
         DO
!                                  FIND AN ELEMENT IN THE SECOND HALF OF
!                                  THE ARRAY WHICH IS SMALLER THAN T
            l = l - 1
            IF ( A(l)<=t ) THEN
               SPAG_Loop_2_1: DO
!                                  FIND AN ELEMENT IN THE FIRST HALF OF
!                                  THE ARRAY WHICH IS GREATER THAN T
                  k = k + 1
                  IF ( A(k)>=t ) THEN
!                                  INTERCHANGE THESE ELEMENTS
                     IF ( k<=l ) THEN
                        IF ( A(l)/=A(k) ) THEN
                           tt = A(l)
                           A(l) = A(k)
                           A(k) = tt
                           itt = Ir(l)
                           Ir(l) = Ir(k)
                           Ir(k) = itt
                        ENDIF
                        EXIT SPAG_Loop_2_1
                     ELSE
!                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
!                                  THE ARRAY YET TO BE SORTED
                        IF ( l-i<=j-k ) THEN
                           il(m) = k
                           iu(m) = j
                           j = l
                           m = m + 1
                        ELSE
                           il(m) = i
                           iu(m) = l
                           i = k
                           m = m + 1
                        ENDIF
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 4
      CASE (4)
!                                  BEGIN AGAIN ON ANOTHER PORTION OF
!                                  THE UNSORTED ARRAY
         m = m - 1
         IF ( m==0 ) RETURN
         i = il(m)
         j = iu(m)
         spag_nextblock_1 = 5
      CASE (5)
         IF ( j-i>=11 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( i==1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = i - 1
         DO
            i = i + 1
            IF ( i==j ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            t = A(i+1)
            it = Ir(i+1)
            IF ( A(i)>t ) THEN
               k = i
               SPAG_Loop_2_2: DO
                  A(k+1) = A(k)
                  Ir(k+1) = Ir(k)
                  k = k - 1
                  IF ( t>=A(k) ) THEN
                     A(k+1) = t
                     Ir(k+1) = it
                     EXIT SPAG_Loop_2_2
                  ENDIF
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE hdvs1
