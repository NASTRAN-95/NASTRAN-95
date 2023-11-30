
SUBROUTINE hdvs1(A,La,Ir)
   IMPLICIT NONE
   INTEGER La
   INTEGER A(1) , Ir(1)
   INTEGER i , ij , il(21) , it , itt , iu(21) , j , k , l , m , t , tt
   REAL r
!                                  FIRST EXECUTABLE STATEMENT
   IF ( La<=0 ) RETURN
   m = 1
   i = 1
   j = La
   r = .375
 100  IF ( i==j ) GOTO 300
   IF ( r>.5898437 ) THEN
      r = r - .21875
   ELSE
      r = r + 3.90625E-2
   ENDIF
 200  k = i
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
         DO
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
                  EXIT
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
                  GOTO 400
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!                                  BEGIN AGAIN ON ANOTHER PORTION OF
!                                  THE UNSORTED ARRAY
 300  m = m - 1
   IF ( m==0 ) RETURN
   i = il(m)
   j = iu(m)
 400  IF ( j-i>=11 ) GOTO 200
   IF ( i==1 ) GOTO 100
   i = i - 1
   DO
      i = i + 1
      IF ( i==j ) GOTO 300
      t = A(i+1)
      it = Ir(i+1)
      IF ( A(i)>t ) THEN
         k = i
         DO
            A(k+1) = A(k)
            Ir(k+1) = Ir(k)
            k = k - 1
            IF ( t>=A(k) ) THEN
               A(k+1) = t
               Ir(k+1) = it
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDDO
END SUBROUTINE hdvs1