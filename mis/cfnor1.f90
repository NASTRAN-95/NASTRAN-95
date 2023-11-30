
SUBROUTINE cfnor1(Right,Left,Size2,Option,Ri)
   IMPLICIT NONE
   REAL Dumxc(21)
   INTEGER Ksystm(65) , Nout
   LOGICAL Qpr
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /feerxc/ Dumxc , Qpr
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   INTEGER Option , Size2
   REAL Left(1) , Ri(2) , Right(1)
   INTEGER i , j
   REAL rj(2) , rsqrt , theta2
   LOGICAL skip
!
!     CFNOR1 IS A SINGLE-PRECISION ROUTINE (CREATED FOR USE BY
!     THE COMPLEX FEER METHOD) WHICH NORMALIZES A COMPLEX PAIR
!     OF VECTORS TO MAGNITUDE UNITY
!
!     DEFINITION OF INPUT PARAMETERS
!
!     RIGHT    = ORIGINAL RIGHT-HANDED COMPLEX SINGLE PRECISION VECTOR
!     LEFT     = ORIGINAL LEFT -HANDED COMPLEX SINGLE PRECISION VECTOR
!     SIZE2    = LENGTH OF EITHER VECTOR IN SINGLE PRECISION WORDS
!                (I.E., TWICE THE LENGTH OF THE COMPLEX VECTORS)
!     OPTION   = 0  NORMALIZE THE INPUT VECTORS, AND OUTPUT THE
!                   SQUARE ROOT OF THE INNER PRODUCT IN RI(2)
!              = 1  ONLY OUTPUT INNER-PRODUCT, IN RI(2)
!              = 2  ONLY OUTPUT SQUARE ROOT OF INNER-PRODUCT, IN RI(2)
!
!     DEFINITION OF OUTPUT PARAMETERS
!
!     RIGHT    = NORMALIZED RIGHT-HANDED VECTOR
!     LEFT     = NORMALIZED LEFT -HANDED VECTOR
!     RI       = INNER-PRODUCT, OR SQUARE ROOT OF INNER-PRODUCT (SEE
!                OPTION)
!
   EQUIVALENCE (Ksystm(2),Nout)
!
   skip = .FALSE.
   DO
!
!     COMPUTE INNER PRODUCT (LEFT*RIGHT)
!
      Ri(1) = 0.
      Ri(2) = 0.
      DO i = 1 , Size2 , 2
         j = i + 1
         Ri(1) = Ri(1) + Left(i)*Right(i) - Left(j)*Right(j)
         Ri(2) = Ri(2) + Left(j)*Right(i) + Left(i)*Right(j)
      ENDDO
      IF ( Option/=1 ) THEN
         IF ( skip ) THEN
            theta2 = sqrt(Ri(1)**2+Ri(2)**2)
            WRITE (Nout,99001) theta2 , Ri
99001       FORMAT (3H --,32(4H----),/,7H CFNOR1,6X,16HOUTPUT MAGNITUDE,E16.8,8X,2E16.8,/,3H --,32(4H----))
            WRITE (Nout,99003) (Right(i),i=1,Size2)
            WRITE (Nout,99004)
            WRITE (Nout,99003) (Left(i),i=1,Size2)
            WRITE (Nout,99004)
         ELSE
!
!     COMPUTE MAGNITUDE OF SQUARE ROOT
!
            rsqrt = sqrt(sqrt(Ri(1)**2+Ri(2)**2))
            IF ( rsqrt>0. ) THEN
!
!     COMPUTE MODULUS OF SQUARE ROOT
!
               theta2 = .5*atan2(Ri(2),Ri(1))
!
!     COMPUTE REAL AND IMAGINARY PARTS OF SQUARE ROOT OF INNER PRODUCT
!
               Ri(1) = rsqrt*cos(theta2)
               Ri(2) = rsqrt*sin(theta2)
               IF ( Option==2 ) EXIT
               rj(1) = Ri(1)
               rj(2) = Ri(2)
!
!     INVERT THE ABOVE COMPLEX NUMBER (THETA2 IS DUMMY)
!
               theta2 = 1./(Ri(1)**2+Ri(2)**2)
               Ri(1) = Ri(1)*theta2
               Ri(2) = -Ri(2)*theta2
!
!     NORMALIZE THE INPUT VECTORS
!
               DO i = 1 , Size2 , 2
                  j = i + 1
                  theta2 = Right(i)
                  Right(i) = Ri(1)*Right(i) - Ri(2)*Right(j)
                  Right(j) = Ri(2)*theta2 + Ri(1)*Right(j)
                  theta2 = Left(i)
                  Left(i) = Ri(1)*Left(i) - Ri(2)*Left(j)
                  Left(j) = Ri(2)*theta2 + Ri(1)*Left(j)
               ENDDO
!
!     ----------- SPECIAL PRINT ----------------------------------------
               IF ( Qpr ) THEN
                  skip = .TRUE.
                  CYCLE
               ENDIF
            ELSE
               WRITE (Nout,99002) Uwm
99002          FORMAT (A25,' 3162',//5X,'ATTEMPT TO NORMALIZE NULL VECTOR. ','NO ACTION TAKEN.'//)
               EXIT
            ENDIF
         ENDIF
!     ------------------------------------------------------------------
!
         Ri(1) = rj(1)
         Ri(2) = rj(2)
      ENDIF
      EXIT
   ENDDO
99003 FORMAT ((1H ,4E25.16))
99004 FORMAT (3H --,32(4H----))
END SUBROUTINE cfnor1
