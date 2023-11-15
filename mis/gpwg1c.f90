
SUBROUTINE gpwg1c(B,E,Eig,Iflag)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Iflag
   DOUBLE PRECISION B(3,3) , E(3,3) , Eig(3)
!
! Local variable declarations
!
   DOUBLE PRECISION bmax , bp(3,3) , c , detb , ep(3,3) , epsil , r , s , t
   INTEGER i , ii , j , k
!
! End of declarations
!
!
!     DOUBLE PRECISION VERSION, BY G.CHAN/SPERRY    8/86
!
!     IFLAG=0 MEANS RUN OK
!     IFLAG=1 MEANS NO SOLUTION IN 20 ITERATIONS
!
!
   detb = 0.0D0
   DO i = 1 , 3
      DO j = 1 , 3
         detb = detb + B(i,j)*B(i,j)
      ENDDO
   ENDDO
   epsil = dsqrt(detb)*1.0D-5
   Iflag = 0
   ii = 1
   DO i = 1 , 3
      DO j = 1 , 3
         E(i,j) = 0.0D0
         IF ( i==j ) E(i,j) = 1.0D0
      ENDDO
   ENDDO
   IF ( detb/=0.0D0 ) THEN
      DO
         bmax = dmax1(dabs(B(1,2)),dabs(B(1,3)),dabs(B(2,3)))
         IF ( dabs(bmax)<epsil ) EXIT
         IF ( bmax==dabs(B(1,2)) ) THEN
            i = 1
            j = 2
            k = 3
         ELSEIF ( bmax/=dabs(B(1,3)) ) THEN
            i = 2
            j = 3
            k = 1
         ELSE
            i = 1
            j = 3
            k = 2
         ENDIF
         r = (B(j,j)-B(i,i))/B(i,j)
         IF ( dabs(r)<1.0D-6 ) THEN
            s = dsqrt(.5D0)
            c = s
         ELSEIF ( dabs(r)>1.0D+6 ) THEN
            s = 0.0D0
            c = 1.0D0
         ELSE
            t = dsqrt((r*r)/4.0D0+1.0D0) - 0.5D0*r
            c = dsqrt(1.0D0+t*t)
            s = t/c
            c = 1.0D0/c
         ENDIF
         bp(i,i) = B(i,i)*c*c + B(j,j)*s*s - 2.0D0*B(i,j)*s*c
         bp(j,j) = B(i,i)*s*s + B(j,j)*c*c + 2.0D0*B(i,j)*s*c
         bp(k,k) = B(k,k)
         bp(j,i) = 0.0D0
         bp(i,j) = 0.0D0
         bp(k,i) = B(i,k)*c - B(j,k)*s
         bp(i,k) = bp(k,i)
         bp(k,j) = B(j,k)*c + B(i,k)*s
         bp(j,k) = bp(k,j)
         ep(i,1) = E(i,1)*c - E(j,1)*s
         ep(j,1) = E(i,1)*s + E(j,1)*c
         ep(k,1) = E(k,1)
         ep(i,2) = E(i,2)*c - E(j,2)*s
         ep(j,2) = E(i,2)*s + E(j,2)*c
         ep(k,2) = E(k,2)
         ep(i,3) = E(i,3)*c - E(j,3)*s
         ep(j,3) = E(i,3)*s + E(j,3)*c
         ep(k,3) = E(k,3)
         DO i = 1 , 3
            DO j = 1 , 3
               B(i,j) = bp(i,j)
               E(i,j) = ep(i,j)
            ENDDO
         ENDDO
         IF ( ii>=21 ) THEN
            Iflag = 1
            GOTO 99999
         ELSE
            ii = ii + 1
         ENDIF
      ENDDO
   ENDIF
   DO i = 1 , 3
      Eig(i) = B(i,i)
   ENDDO
99999 END SUBROUTINE gpwg1c
