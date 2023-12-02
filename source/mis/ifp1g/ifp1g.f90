!*==ifp1g.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1g(Itype,Case,Isub1)
   USE c_ifp1a
   USE c_output
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
   INTEGER , DIMENSION(200,2) :: Case
   INTEGER :: Isub1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: char , i , i2 , ii , ipos , isave , its , j , k , k1
   INTEGER , DIMENSION(1) :: core
   EXTERNAL khrfn1
!
! End of declarations rewritten by SPAG
!
!
!     MAKE SURE THIS VERSION ALSO WORKS IN UNIVAC, IBM, CDC AND 64-BIT
!     MACHINES
!     ================================================================
!     IZZZBB = 0 (ALL BITS ZERO)
!     IBEN   = FIRST BYTE BLANK, REST IS ZERO FILL
!     EQUAL  = FIRST BYTE EQUAL, REST IS ZERO FILL
!
   !>>>>EQUIVALENCE (Corex(1),Corey(1)) , (Core(1),Corey(401))
!
!     FIND EQUAL SIGN AND COPY REMAINING DATA ON CARD
!
!     OR FIND THE FIRST BLANK CHARACTER AFTER THE FIRST NON-BLANK WORD
!     (USED ONLY FOR ITYPE = 8, PTITLE, AXIS TITLE ETC. WHERE EQUAL SIGN
!     IS OPTIONAL AND NOT MANDATORY)
!
   k = -1
   i2 = nwpc - 2
   DO i = 1 , i2
      DO j = 1 , ncpw4
         char = khrfn1(izzzbb,1,core(i),j)
         IF ( char==equal ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         IF ( char/=iben .AND. k==-1 ) k = 0
         IF ( char==iben .AND. k==0 ) k = i*100 + j
      ENDDO
   ENDDO
   IF ( Itype==8 ) THEN
      i = k/100
      j = mod(k,100)
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      K = (Itype-1)*32
      K1 = K + 38
      IF ( Itype==8 ) K1 = 0
      IF ( J==Ncpw4 ) THEN
         I = I + 1
         J = 0
      ENDIF
      J = J + 1
      Ipos = 1
      Its = K + 1
      Isave = Izzzbb
      DO Ii = I , I2
         SPAG_Loop_2_1: DO
            Isave = khrfn1(Isave,Ipos,Core(Ii),J)
            Ipos = Ipos + 1
            IF ( Ipos>4 ) THEN
               Ipos = 1
               IF ( Itype==7 ) THEN
                  title(Its) = Isave
               ELSEIF ( istr/=1 ) THEN
                  title(Its) = Isave
               ELSE
                  Case(K1+1,Isub1) = Isave
                  K1 = K1 + 1
               ENDIF
               Isave = Izzzbb
               Its = Its + 1
            ENDIF
            J = J + 1
            IF ( J>Ncpw4 ) THEN
               J = 1
               EXIT SPAG_Loop_2_1
            ENDIF
         ENDDO SPAG_Loop_2_1
      ENDDO
      DO I = Ipos , 4
         Isave = khrfn1(Isave,I,Iben,1)
      ENDDO
      IF ( Itype==7 ) THEN
         title(Its) = Isave
      ELSEIF ( istr/=1 ) THEN
         title(Its) = Isave
      ELSE
         Case(K1+1,Isub1) = Isave
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE ifp1g
