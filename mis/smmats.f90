
SUBROUTINE smmats(A,Irowa,Icola,Mta,B,Irowb,Icolb,Ntb,C,E)
   IMPLICIT NONE
   INTEGER Icola , Icolb , Irowa , Irowb , Mta , Ntb
   REAL A(1) , B(1) , C(1) , E(1)
   DOUBLE PRECISION aia , bjb , cij , eij
   INTEGER cola , colb , i , ia , ifix , ij , ilim , inci , incj , incka , inckb , iparm(2) , j , jb , jlim , k , klim , lim , nta ,&
         & rowa , rowb
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
      IF ( cola/=rowb ) GOTO 300
   ELSEIF ( nta==1 .AND. Ntb==0 ) THEN
      IF ( rowa/=rowb ) GOTO 300
   ELSEIF ( nta==0 .AND. Ntb==1 ) THEN
      IF ( cola/=colb ) GOTO 300
   ELSEIF ( nta==1 .AND. Ntb==1 ) THEN
      IF ( rowa/=colb ) GOTO 300
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
 100  i = i + 1
   ifix = i*inci - cola
   j = 0
 200  j = j + 1
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
         IF ( j<jlim ) GOTO 200
         IF ( i<ilim ) GOTO 100
         RETURN
      ENDIF
   ENDDO
 300  iparm(1) = nta
   iparm(2) = Ntb
   CALL mesage(-30,21,iparm(1))
END SUBROUTINE smmats
