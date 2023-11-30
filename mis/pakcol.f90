
SUBROUTINE pakcol(Terms,Nterms)
   IMPLICIT NONE
   INTEGER A(4) , Irow
   DOUBLE PRECISION Tval , Val
   COMMON /zblpkx/ A , Irow
   INTEGER Nterms
   INTEGER Terms(1)
   INTEGER i , iloc , isil , j , jloc , jsil , kloc , nt , temp(7)
!
!     PACKS OUT A COLUMN OF AF OR DKGG MATRIX - DATA IS IN THE
!     FOLLOWING SAMPLE FORMATS.
!
!                  ---------------------
!                  I  NEGATIVE ROWSIL  I
!                  I-------------------I
!                  I  1 MATRIX TERM    I
!                  I-------------------I
!                  I  POSITIVE ROWSIL  I
!                  I-------------------I
!                  I                   I
!                  I  3 MATRIX TERMS   I
!                  I                   I
!                  ---------------------
!
!     MATRIX TERMS ARE IN DOUBLE PRECISION
!
!
!
!
!     PACK COMMON BLOCK
!
!
   EQUIVALENCE (Val,A(1))
   EQUIVALENCE (Tval,A(3))
!
!***********************************************************************
!
!     SORT THE MATRIX ENTRIES BY ABSOULUTE SIL VALUES
!
   iloc = 1
 100  isil = Terms(iloc)
   jloc = iloc
   jsil = isil
   DO
      jloc = jloc + 3
      IF ( jsil>0 ) jloc = jloc + 4
      IF ( jloc>=Nterms ) THEN
!
         iloc = iloc + 3
         IF ( isil>0 ) iloc = iloc + 4
         IF ( iloc<Nterms ) GOTO 100
!
!     PACK OUT TERMS - ADDING ANY IDENTICAL SIL
!
         iloc = 1
         DO
            Irow = iabs(Terms(iloc))
            nt = 2
            IF ( Terms(iloc)>0 ) nt = 6
!
            DO i = 1 , nt , 2
               A(1) = Terms(iloc+i)
               A(2) = Terms(iloc+i+1)
               jloc = iloc
               DO
                  j = jloc
                  jloc = j + 3
                  IF ( Terms(j)>0 ) jloc = j + 7
                  IF ( jloc>=Nterms ) EXIT
                  IF ( Terms(jloc)/=Terms(iloc) ) EXIT
!
!     DUPLICATE SILS - ADD THEM
!
                  A(3) = Terms(jloc+i)
                  A(4) = Terms(jloc+i+1)
                  Val = Val + Tval
                  j = jloc
               ENDDO
!
!     PACK OUT TERM
!
               CALL zblpki
               Irow = Irow + 1
            ENDDO
!
            iloc = jloc
            IF ( iloc>=Nterms ) GOTO 99999
         ENDDO
      ELSE
         jsil = Terms(jloc)
         IF ( iabs(jsil)<iabs(isil) ) THEN
!
            nt = 3
            IF ( jsil>0 ) nt = 7
            DO i = 1 , nt
               temp(i) = Terms(jloc+i-1)
            ENDDO
!
            kloc = jloc - 1
            DO i = iloc , kloc
               j = kloc - i + iloc
               Terms(j+nt) = Terms(j)
            ENDDO
!
            DO i = 1 , nt
               Terms(iloc+i-1) = temp(i)
            ENDDO
            isil = jsil
         ENDIF
      ENDIF
   ENDDO
!
99999 RETURN
END SUBROUTINE pakcol
