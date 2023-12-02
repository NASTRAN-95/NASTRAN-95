!*==ifp1g.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1g(Itype,Case,Isub1)
   IMPLICIT NONE
   USE C_IFP1A
   USE C_OUTPUT
   USE C_ZZZZZZ
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
   i2 = Nwpc - 2
   DO i = 1 , i2
      DO j = 1 , Ncpw4
         char = khrfn1(Izzzbb,1,core(i),j)
         IF ( char==Equal ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
         IF ( char/=Iben .AND. k==-1 ) k = 0
         IF ( char==Iben .AND. k==0 ) k = i*100 + j
      ENDDO
   ENDDO
   IF ( Itype==8 ) THEN
      i = k/100
      j = mod(k,100)
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      k = (Itype-1)*32
      k1 = k + 38
      IF ( Itype==8 ) k1 = 0
      IF ( j==Ncpw4 ) THEN
         i = i + 1
         j = 0
      ENDIF
      j = j + 1
      ipos = 1
      its = k + 1
      isave = Izzzbb
      DO ii = i , i2
         SPAG_Loop_2_1: DO
            isave = khrfn1(isave,ipos,core(ii),j)
            ipos = ipos + 1
            IF ( ipos>4 ) THEN
               ipos = 1
               IF ( Itype==7 ) THEN
                  Title(its) = isave
               ELSEIF ( Istr/=1 ) THEN
                  Title(its) = isave
               ELSE
                  Case(k1+1,Isub1) = isave
                  k1 = k1 + 1
               ENDIF
               isave = Izzzbb
               its = its + 1
            ENDIF
            j = j + 1
            IF ( j>Ncpw4 ) THEN
               j = 1
               EXIT SPAG_Loop_2_1
            ENDIF
         ENDDO SPAG_Loop_2_1
      ENDDO
      DO i = ipos , 4
         isave = khrfn1(isave,i,Iben,1)
      ENDDO
      IF ( Itype==7 ) THEN
         Title(its) = isave
      ELSEIF ( Istr/=1 ) THEN
         Title(its) = isave
      ELSE
         Case(k1+1,Isub1) = isave
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE ifp1g
