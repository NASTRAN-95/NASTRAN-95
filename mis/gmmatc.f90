
SUBROUTINE gmmatc(A,Rowa,Cola,Mta,B,Rowb,Colb,Ntb,C)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Cola , Colb , Mta , Ntb , Rowa , Rowb
   COMPLEX A(1) , B(1) , C(1)
!
! Local variable declarations
!
   INTEGER i , ij , ij1 , ijn , ik , ik1 , ikn , incik1 , inckj1 , incrik , incrkj , iparm(2) , kj , kj1 , ncola , ncolb , nrowa ,  &
         & nrowb , nta , nterms
!
! End of declarations
!
!*****
!     GMMATC - G E N E R A L  M A T R I X  M U L T I P L Y
!                                 A N D
!                           T R A N S P O S E
!            S I N G L E  P R E C I S I O N  V E R S I O N
!     COMPLEX VERSION
!
!     PERFORMS                                     WHEN
!               A            *  B            =  C     MTA=0  NTB= 0
!               A            *  B TRANSPOSE  =  C          0       1
!               A TRANSPOSE  *  B            =  C          1       0
!               A TRANSPOSE  *  B TRANSPOSE  =  C          1       1
!*****
!     A -  IS A MATRIX (ROWA) ROWS BY (COLA) COLUMNS
!     B -  IS A MATRIX (ROWB) ROWS BY (COLB) COLUMNS
!     A,B AND C ARE STORED BY ROWS (EXAMPLE)
!              MATRIX                   STORED
!         A=   1    2              A=   1
!              3    4                   2
!              5    6                   3
!                                       4
!                                       5
!                                       6
!*****
!
!
!     IF MTA .LT. 0, C IS NOT ZEROED OUT.  HENCE THE ROUTINE, IN THIS
!     CASE, COMPUTES  A * B  +  D  =  C  WHERE THE MATRIX  D  HAS BEEN
!     STORED ROW-WISE AT  C  BY THE CALLING PROGRAM.  IF MTA = -1,  A
!     IS TRANSPOSED.  IF MTA = -2,  A  IS NOT TRANSPOSED.  NTB IS
!     DEFINED AS ABOVE AND IS INDEPENDENT OF MTA.
!
!
!
!
!
!
!
!
   nta = iabs(Mta)
   IF ( Mta==(-2) ) nta = 0
   IF ( nta/=0 ) THEN
!
! A IS TRANSPOSED
!
      nrowa = Cola
      ncola = Rowa
      incrik = Cola
      ikn = (Rowa-1)*Cola + 1
      incik1 = 1
   ELSE
!
! A IS NOT TRANSPOSED
!
      nrowa = Rowa
      ncola = Cola
      incrik = 1
      ikn = Cola
      incik1 = Cola
   ENDIF
   IF ( Ntb/=0 ) THEN
!
! B IS TRANSPOSED
!
      nrowb = Colb
      ncolb = Rowb
      incrkj = 1
      inckj1 = Colb
   ELSE
!
! B IS NOT TRANSPOSED
!
      nrowb = Rowb
      ncolb = Colb
      incrkj = Colb
      inckj1 = 1
   ENDIF
!
! CHECK CONSISTANT DIMENSIONS AND ZERO C IF NO D MATRIX
!
   IF ( ncola/=nrowb ) THEN
      iparm(1) = nta
      iparm(2) = Ntb
      CALL mesage(-30,21,iparm(1))
      GOTO 99999
   ELSEIF ( Mta>=0 ) THEN
      nterms = nrowa*ncolb
      DO i = 1 , nterms
         C(i) = 0
      ENDDO
   ENDIF
!
! PERFORM MATRIX MULTIPLICATION
!
   ij1 = 1
   ijn = ncolb
   ik1 = 1
   DO i = 1 , nrowa
      kj1 = 1
      DO ij = ij1 , ijn
         kj = kj1
         DO ik = ik1 , ikn , incrik
            C(ij) = C(ij) + A(ik)*B(kj)
            kj = kj + incrkj
         ENDDO
         kj1 = kj1 + inckj1
      ENDDO
      ij1 = ijn + 1
      ijn = ijn + ncolb
      ik1 = ik1 + incik1
      ikn = ikn + incik1
   ENDDO
   RETURN
99999 RETURN
END SUBROUTINE gmmatc
