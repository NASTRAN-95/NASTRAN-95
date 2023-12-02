!*==filswi.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE filswi(Name1,Name2)
   USE c_system
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Name1
   INTEGER :: Name2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , k1 , k2 , mask2 , n , unit , unit1 , unit2
   INTEGER , SAVE :: mask1
   EXTERNAL andf , complf , mesage , orf
!
! End of declarations rewritten by SPAG
!
!
!     FILSWI SWITCHES THE UNITS ASSIGNED TO THE SPECIFIED DATA BLOCKS.
!
   DATA mask1/32767/
!                     '7FFF'X
!
!     SEARCH FIST FOR POINTERS TO FIAT.
!
   IF ( Name1==Name2 ) RETURN
   k1 = 0
   k2 = 0
   n = 2*lfist - 1
   DO i = 1 , n , 2
      IF ( fist(i)==Name1 ) k1 = fist(i+1)
      IF ( fist(i)==Name2 ) k2 = fist(i+1)
   ENDDO
   IF ( k1<=0 .OR. k2<=0 ) THEN
      WRITE (nout,99001) sfm
99001 FORMAT (A23,' 2178, GINO REFERENCE NAMES, IMPROPER FOR ','SUBROUTINE FILSWI.')
      CALL mesage(-61,0,0)
   ENDIF
!
!     SWITCH UNIT REFERENCE NUMBERS IN FIAT.
!
   mask2 = complf(mask1)
   unit1 = andf(fiat(k1+1),mask1)
   unit2 = andf(fiat(k2+1),mask1)
   n = icfiat*fiat(3) - 2
   DO i = 4 , n , icfiat
      unit = andf(fiat(i),mask1)
      IF ( unit==unit1 ) fiat(i) = orf(andf(fiat(i),mask2),unit2)
      IF ( unit==unit2 ) fiat(i) = orf(andf(fiat(i),mask2),unit1)
   ENDDO
END SUBROUTINE filswi
