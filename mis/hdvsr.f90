
SUBROUTINE hdvsr(A,La,Ir)
   IMPLICIT NONE
   INTEGER La
   REAL A(1)
   INTEGER Ir(1)
   INTEGER i , ij , il(21) , it , itt , iu(21) , j , k , l , m
   REAL r , t , tt
!   IMSL ROUTINE NAME   - HDVSR
!
!-----------------------------------------------------------------------
!
!   COMPUTER            - CDC/SINGLE
!
!   LATEST REVISION     - JANUARY 1, 1978
!
!   PURPOSE             - SORTING OF ARRAYS BY ALGEBRAIC VALUE -
!                           PERMUTATIONS RETURNED
!
!   USAGE               - CALL HDVSR (A,LA,IR)
!
!   ARGUMENTS    A      - ON INPUT, A CONTAINS THE ARRAY TO BE SORTED.
!                         ON OUTPUT, A CONTAINS THE SORTED ARRAY.
!                LA     - INPUT VARIABLE CONTAINING THE NUMBER OF
!                           ELEMENTS IN THE ARRAY TO BE SORTED.
!                IR     - VECTOR OF LENGTH LA.
!                         ON INPUT, IR CONTAINS THE INTEGER VALUES
!                           1,2,...,LA. SEE REMARKS.
!                         ON OUTPUT, IR CONTAINS A RECORD OF THE
!                           PERMUTATIONS MADE ON THE VECTOR A.
!
!   PRECISION/HARDWARE  - SINGLE/ALL
!
!   REQD. IMSL ROUTINES - NONE REQUIRED
!
!                           CONVENTIONS IS AVAILABLE IN THE MANUAL
!                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
!
!   REMARKS      THE VECTOR IR MUST BE INITIALIZED BEFORE ENTERING
!                HDVSR.  ORDINARILY, IR(1)=1, IR(2)=2, ...,
!                IR(LA)=LA.  FOR WIDER APPLICABILITY, ANY INTEGER
!                THAT IS TO BE ASSOCIATED WITH A(I) FOR I=1,2,...,LA
!                MAY BE ENTERED INTO IR(I).
!
!   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
!
!   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
!                           APPLIED TO THIS CODE.  NO OTHER WARRANTY,
!                           EXPRESSED OR IMPLIED, IS APPLICABLE.
!
!-----------------------------------------------------------------------
!
!                                  SPECIFICATIONS FOR ARGUMENTS
!                                  SPECIFICATIONS FOR LOCAL VARIABLES
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
END SUBROUTINE hdvsr