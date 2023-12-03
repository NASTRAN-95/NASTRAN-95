!*==smmats.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE smmats(A,Irowa,Icola,Mta,B,Irowb,Icolb,Ntb,C,E)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: A
   INTEGER :: Irowa
   INTEGER :: Icola
   INTEGER :: Mta
   REAL , DIMENSION(1) :: B
   INTEGER :: Irowb
   INTEGER :: Icolb
   INTEGER :: Ntb
   REAL , DIMENSION(1) :: C
   REAL , DIMENSION(1) :: E
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: aia , bjb , cij , eij
   INTEGER :: cola , colb , i , ia , ifix , ij , ilim , inci , incj , incka , inckb , j , jb , jlim , k , klim , lim , nta , rowa , &
            & rowb , spag_nextblock_1
   INTEGER , DIMENSION(2) :: iparm
   EXTERNAL mesage
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*****
!     SMMATS - S P E C I A L   M A T R I X   M U L T I P L Y
!                                 A N D
!                           T R A N S P O S E
!            S I N G L E  P R E C I S I O N  V E R S I O N
!
!     PERFORMS                                     WHEN
!               A            *  B            =  C     MTA=0  NTB= 0
!               A            *  B TRANSPOSE  =  C          0       1
!               A TRANSPOSE  *  B            =  C          1       0
!               A TRANSPOSE  *  B TRANSPOSE  =  C          1       1
!  THE CORRESPONDING OPERATIONS ARE DONE ON -E- USING ABSOLUTE VALUES.
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
!*****
! IF MTA .LT. 0, C AND E ARE NOT ZEROED OUT.  HENCE THE ROUTINE, IN THIS
!     CASE, COMPUTES  A * B  +  D  =  C  WHERE THE MATRIX  D  HAS BEEN
!     STORED ROW-WISE AT  C  BY THE CALLING PROGRAM.  IF MTA = -1,  A
!     IS TRANSPOSED.  IF MTA = -2,  A  IS NOT TRANSPOSED.  NTB IS
!     DEFINED AS ABOVE AND IS INDEPENDENT OF MTA.
!
!
         rowa = Irowa
         cola = Icola
         rowb = Irowb
         colb = Icolb
         nta = iabs(Mta)
         IF ( Mta==(-2) ) nta = 0
         IF ( nta==0 .AND. Ntb==0 ) THEN
            IF ( cola/=rowb ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( nta==1 .AND. Ntb==0 ) THEN
            IF ( rowa/=rowb ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( nta==0 .AND. Ntb==1 ) THEN
            IF ( cola/=colb ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( nta==1 .AND. Ntb==1 ) THEN
            IF ( rowa/=colb ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( nta==1 ) THEN
            ilim = cola
            klim = rowa
            inci = 1
            incka = cola
         ELSE
            ilim = rowa
            klim = cola
            inci = cola
            incka = 1
         ENDIF
         IF ( Ntb==1 ) THEN
            jlim = rowb
            incj = colb
            inckb = 1
         ELSE
            jlim = colb
            incj = 1
            inckb = colb
         ENDIF
         IF ( Mta>=0 ) THEN
            lim = ilim*jlim
            DO i = 1 , lim
               E(i) = 0.0
               C(i) = 0.0
            ENDDO
         ENDIF
         ij = 0
         i = 0
         spag_nextblock_1 = 2
      CASE (2)
         i = i + 1
         ifix = i*inci - cola
         j = 0
         SPAG_Loop_1_1: DO
            j = j + 1
            ij = ij + 1
            ia = ifix
            jb = j*incj - colb
            cij = dble(C(ij))
            eij = dble(E(ij))
            k = 0
            DO
               k = k + 1
               ia = ia + incka
               jb = jb + inckb
               aia = dble(A(ia))
               bjb = dble(B(jb))
               cij = cij + aia*bjb
               eij = eij + dabs(aia)*dabs(bjb)
               IF ( k>=klim ) THEN
                  C(ij) = sngl(cij)
                  E(ij) = sngl(eij)
                  IF ( j<jlim ) CYCLE SPAG_Loop_1_1
                  IF ( i<ilim ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  RETURN
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         iparm(1) = nta
         iparm(2) = Ntb
         CALL mesage(-30,21,iparm(1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE smmats