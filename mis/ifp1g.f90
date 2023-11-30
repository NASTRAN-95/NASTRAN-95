
SUBROUTINE ifp1g(Itype,Case,Isub1)
   IMPLICIT NONE
   INTEGER Core(1) , Corey(401) , Equal , Iben , Istr , Izzzbb , Ncpw4 , Nwpc , Title(32)
   REAL Corex(1) , Skip1(3) , Skip2(4) , Skip3(2)
   COMMON /ifp1a / Skip1 , Nwpc , Ncpw4 , Skip2 , Izzzbb , Istr , Skip3 , Iben , Equal
   COMMON /output/ Title
   COMMON /zzzzzz/ Corex
   INTEGER Isub1 , Itype
   INTEGER Case(200,2)
   INTEGER char , i , i2 , ii , ipos , isave , its , j , k , k1
   INTEGER khrfn1
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
         char = khrfn1(Izzzbb,1,Core(i),j)
         IF ( char==Equal ) GOTO 100
         IF ( char/=Iben .AND. k==-1 ) k = 0
         IF ( char==Iben .AND. k==0 ) k = i*100 + j
      ENDDO
   ENDDO
   IF ( Itype==8 ) THEN
      i = k/100
      j = mod(k,100)
   ENDIF
 100  k = (Itype-1)*32
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
      DO
         isave = khrfn1(isave,ipos,Core(ii),j)
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
            EXIT
         ENDIF
      ENDDO
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
END SUBROUTINE ifp1g