!*==bitpat.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bitpat(Icode,Ibits)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Icode
   INTEGER , DIMENSION(2) :: Ibits
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ia , j , k , n , nbits
   INTEGER , SAVE :: iblank
   INTEGER , DIMENSION(9) , SAVE :: int
   INTEGER , DIMENSION(32) :: list
   EXTERNAL decode , klshft , krshft , orf
!
! End of declarations rewritten by SPAG
!
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
   nbits = -nbpc
   DO i = 1 , n
      nbits = nbits + nbpc
      ia = list(i) + 1
      k = nbpw - nbits
      Ibits(j) = klshft(krshft(Ibits(j),k/nbpc),k/nbpc)
      Ibits(j) = orf(Ibits(j),krshft(int(ia),nbits/nbpc))
      IF ( i==4 ) THEN
         j = 2
         nbits = -nbpc
      ENDIF
   ENDDO
END SUBROUTINE bitpat
