
SUBROUTINE tldrs(Offset,Ii,Trans,Trans1)
   IMPLICIT NONE
   DOUBLE PRECISION Dffset
   INTEGER Ii
   REAL Offset
   DOUBLE PRECISION Trand(1) , Trand1(36)
   REAL Trans(1) , Trans1(36)
   INTEGER i , ipoint , j , jpoint , kpoint , lpoint
!
!     &    ENTRY TLDRD (DFFSET,II,TRAND,TRAND1)
!
!     MODIFIED TO INCLUDE THE EFFECTS OF OFFSET
!
!
!     SINGLE PRECISION -
!
   DO i = 1 , 36
      Trans1(i) = 0.0
   ENDDO
!
   ipoint = 9*(Ii-1)
!
   DO i = 1 , 3
      jpoint = 6*(i-1)
      kpoint = jpoint + 21
      lpoint = 3*(i-1) + ipoint
!
      DO j = 1 , 3
         Trans1(jpoint+j) = Trans(lpoint+j)
         Trans1(kpoint+j) = Trans(lpoint+j)
      ENDDO
   ENDDO
!
   IF ( Offset/=0.0 ) THEN
      Trans1(4) = Offset*Trans(ipoint+4)
      Trans1(5) = Offset*Trans(ipoint+5)
      Trans1(6) = Offset*Trans(ipoint+6)
      Trans1(10) = -Offset*Trans(ipoint+1)
      Trans1(11) = -Offset*Trans(ipoint+2)
      Trans1(12) = -Offset*Trans(ipoint+3)
   ENDIF
   GOTO 99999
!
   ENTRY tldrd(Dffset,Ii,Trand,Trand1)
!     ====================================
!
!     DOUBLE PRECISION -
!
   DO i = 1 , 36
      Trand1(i) = 0.0D0
   ENDDO
!
   ipoint = 9*(Ii-1)
!
   DO i = 1 , 3
      jpoint = 6*(i-1)
      kpoint = jpoint + 21
      lpoint = 3*(i-1) + ipoint
!
      DO j = 1 , 3
         Trand1(jpoint+j) = Trand(lpoint+j)
         Trand1(kpoint+j) = Trand(lpoint+j)
      ENDDO
   ENDDO
!
   IF ( Dffset/=0.0D0 ) THEN
      Trand1(4) = Dffset*Trand(ipoint+4)
      Trand1(5) = Dffset*Trand(ipoint+5)
      Trand1(6) = Dffset*Trand(ipoint+6)
      Trand1(10) = -Dffset*Trand(ipoint+1)
      Trand1(11) = -Dffset*Trand(ipoint+2)
      Trand1(12) = -Dffset*Trand(ipoint+3)
   ENDIF
99999 RETURN
END SUBROUTINE tldrs
