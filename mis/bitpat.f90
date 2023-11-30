
SUBROUTINE bitpat(Icode,Ibits)
   IMPLICIT NONE
   INTEGER Junk(38) , Nbpc , Nbpw
   COMMON /system/ Junk , Nbpc , Nbpw
   INTEGER Icode
   INTEGER Ibits(2)
   INTEGER i , ia , iblank , int(9) , j , k , list(32) , n , nbits
   INTEGER klshft , krshft , orf
   EXTERNAL orf
!
!     THE PURPOSE OF THIS ROUTINE IS TO TRANSFORM THE DOF WORD INTO ITS
!     NASTRAN DIGITAL REPRESENTATION.
!
   DATA iblank/4H    /
   DATA int/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9/
!
   Ibits(1) = iblank
   Ibits(2) = iblank
!
   CALL decode(Icode,list,n)
   IF ( n==0 ) RETURN
!
   j = 1
   nbits = -Nbpc
   DO i = 1 , n
      nbits = nbits + Nbpc
      ia = list(i) + 1
      k = Nbpw - nbits
      Ibits(j) = klshft(krshft(Ibits(j),k/Nbpc),k/Nbpc)
      Ibits(j) = orf(Ibits(j),krshft(int(ia),nbits/Nbpc))
      IF ( i==4 ) THEN
         j = 2
         nbits = -Nbpc
      ENDIF
   ENDDO
END SUBROUTINE bitpat
