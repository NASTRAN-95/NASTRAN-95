!*==incore.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE incore(A,N,B,Cx,Ix)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
   INTEGER :: Ix
   COMPLEX , DIMENSION(N,N) :: A
   COMPLEX , DIMENSION(Ix,N) :: B
   COMPLEX , DIMENSION(Ix,N) :: Cx
!
! Local variable declarations rewritten by SPAG
!
   REAL :: amax
   COMPLEX :: cmax , csum , scrch , t1 , t2 , t3
   INTEGER :: i , irow , j , jj , jmax , jp1 , k , l , nm1
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     IN-CORE DECOMPOSITION OF SQUARE, COMPLEX, NXN MATRIX,A.
!     AX = B.
!     CX = X
!     IX = NUMBER OF B VECTORS SPECIFIED.
!
!
!
   IF ( N==2 ) THEN
      DO i = 1 , Ix
         Cx(i,2) = (B(i,2)-(B(i,1)*A(1,2)/A(1,1)))/(A(2,2)-(A(2,1)*A(1,2)/A(1,1)))
         Cx(i,1) = B(i,1)/A(1,1) - A(2,1)*Cx(i,2)/A(1,1)
      ENDDO
      RETURN
   ELSEIF ( N==1 ) THEN
      DO i = 1 , Ix
         Cx(i,1) = B(i,1)/A(1,1)
      ENDDO
      RETURN
   ENDIF
   nm1 = N - 1
!
!     PIVOT MAYBE.
!
   DO j = 1 , nm1
      cmax = A(j,j)
      jp1 = j + 1
      jmax = j
      DO jj = jp1 , N
         IF ( cabs(A(j,jj))>cabs(cmax) ) THEN
            cmax = A(j,jj)
            irow = jj
            jmax = jj
         ENDIF
      ENDDO
!
!     IROW = ROW WITH LARGEST ELEMENT IN COLUMN J.
!     MOVE PIVOT ROW TO TOP OF ELIMINATION
!
      amax = cabs(cmax)
      IF ( amax/=0. ) THEN
         IF ( jmax/=j ) THEN
            DO jj = j , N
               scrch = A(jj,j)
               A(jj,j) = A(jj,irow)
               A(jj,irow) = scrch
            ENDDO
!
!     INTERCHANGE B VECTOR
!
            DO jj = 1 , Ix
               scrch = B(jj,j)
               B(jj,j) = B(jj,irow)
               B(jj,irow) = scrch
            ENDDO
         ENDIF
!
!     ELIMINATE COLUMN
!
         A(j,j) = (1.0,0.0)/A(j,j)
         t1 = A(j,j)
         DO i = jp1 , N
            t2 = A(i,j)
            IF ( cabs(t2)>=(1.0E-19) ) THEN
               t2 = -t2*t1
               A(i,j) = t2
               DO l = jp1 , N
                  t3 = A(j,l)
                  IF ( cabs(t3)>=(1.0E-19) ) A(i,l) = A(i,l) + t3*t2
               ENDDO
            ENDIF
         ENDDO
!
!     HANDLE B ELIMINATION.
!
         DO jj = 1 , Ix
            B(jj,j) = B(jj,j)*t1
         ENDDO
         DO jj = 1 , Ix
            DO k = jp1 , N
               B(jj,k) = B(jj,k) - B(jj,j)*A(j,k)
            ENDDO
         ENDDO
      ENDIF
   ENDDO
!
!     BACKWARD PASS.
!
   DO jj = 1 , Ix
      Cx(jj,N) = B(jj,N)/A(N,N)
   ENDDO
   DO jj = 1 , Ix
      i = N
      SPAG_Loop_2_1: DO
         csum = (0.,0.)
         k = i - 1
         DO j = i , N
            csum = csum + Cx(jj,j)*A(j,k)
         ENDDO
         Cx(jj,k) = B(jj,k) + csum
         IF ( i<=2 ) EXIT SPAG_Loop_2_1
         i = i - 1
      ENDDO SPAG_Loop_2_1
   ENDDO
END SUBROUTINE incore
