
SUBROUTINE alg30(A,B)
   IMPLICIT NONE
   REAL A(9,9) , B(9)
   REAL amax , pv
   INTEGER ic , index(9) , ir , j , l , l1 , n
!
!
   n = 9
   DO j = 1 , n
      index(j) = 0
   ENDDO
   DO
      amax = -1.0
      DO j = 1 , n
         IF ( index(j)==0 ) THEN
            DO l = 1 , n
               IF ( index(l)==0 ) THEN
                  pv = abs(A(j,l))
                  IF ( pv>amax ) THEN
                     ir = j
                     ic = l
                     amax = pv
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      IF ( amax<=0.0 ) RETURN
      index(ic) = ir
      IF ( ic/=ir ) THEN
         DO l = 1 , n
            pv = A(ir,l)
            A(ir,l) = A(ic,l)
            A(ic,l) = pv
            IF ( l<=1 ) THEN
               pv = B(ir)
               B(ir) = B(ic)
               B(ic) = pv
            ENDIF
         ENDDO
      ENDIF
      pv = 1.0/A(ic,ic)
      A(ic,ic) = 1.0
      DO l = 1 , n
         A(ic,l) = A(ic,l)*pv
         IF ( l<=1 ) B(ic) = B(ic)*pv
      ENDDO
      DO l1 = 1 , n
         IF ( l1/=ic ) THEN
            pv = A(l1,ic)
            A(l1,ic) = 0.0
            DO l = 1 , n
               A(l1,l) = A(l1,l) - A(ic,l)*pv
            ENDDO
            B(l1) = B(l1) - B(ic)*pv
         ENDIF
      ENDDO
   ENDDO
END SUBROUTINE alg30
