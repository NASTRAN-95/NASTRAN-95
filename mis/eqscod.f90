
SUBROUTINE eqscod(Loc,N,Z)
   IMPLICIT NONE
   INTEGER Loc , N
   INTEGER Z(1)
   INTEGER i , icode , iloc , inew , ist , j , mend , ng
   INTEGER lshift , orf
   EXTERNAL lshift , orf
!
!
   i = Loc
   mend = Loc + N - 1
 100  ist = i
   ng = 1
   DO WHILE ( i<mend-2 )
      IF ( Z(i+3)/=Z(ist) ) EXIT
      ng = ng + 1
      i = i + 3
   ENDDO
   IF ( ng/=1 ) THEN
      DO j = 1 , ng
         iloc = ist + 3*(j-1)
         icode = 8*j + ng
         inew = lshift(icode,26)
         Z(iloc+2) = orf(Z(iloc+2),inew)
      ENDDO
      i = i + 3
      IF ( i<mend-2 ) GOTO 100
   ELSE
      i = i + 3
      IF ( i<mend-2 ) GOTO 100
   ENDIF
END SUBROUTINE eqscod
