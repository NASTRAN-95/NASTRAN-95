
SUBROUTINE filswi(Name1,Name2)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Fiat(4) , Fist(1) , Icfiat , Lfist , Nfist , Nout , Sys
   CHARACTER*25 Sfm , Uwm
   REAL Skip(21)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /system/ Sys , Nout , Skip , Icfiat
   COMMON /xfiat / Fiat
   COMMON /xfist / Nfist , Lfist , Fist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
!
! Dummy argument declarations
!
   INTEGER Name1 , Name2
!
! Local variable declarations
!
   INTEGER andf , complf , orf
   INTEGER i , k1 , k2 , mask1 , mask2 , n , unit , unit1 , unit2
   EXTERNAL andf , complf , orf
!
! End of declarations
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
   n = 2*Lfist - 1
   DO i = 1 , n , 2
      IF ( Fist(i)==Name1 ) k1 = Fist(i+1)
      IF ( Fist(i)==Name2 ) k2 = Fist(i+1)
   ENDDO
   IF ( k1<=0 .OR. k2<=0 ) THEN
      WRITE (Nout,99001) Sfm
99001 FORMAT (A23,' 2178, GINO REFERENCE NAMES, IMPROPER FOR ','SUBROUTINE FILSWI.')
      CALL mesage(-61,0,0)
   ENDIF
!
!     SWITCH UNIT REFERENCE NUMBERS IN FIAT.
!
   mask2 = complf(mask1)
   unit1 = andf(Fiat(k1+1),mask1)
   unit2 = andf(Fiat(k2+1),mask1)
   n = Icfiat*Fiat(3) - 2
   DO i = 4 , n , Icfiat
      unit = andf(Fiat(i),mask1)
      IF ( unit==unit1 ) Fiat(i) = orf(andf(Fiat(i),mask2),unit2)
      IF ( unit==unit2 ) Fiat(i) = orf(andf(Fiat(i),mask2),unit1)
   ENDDO
END SUBROUTINE filswi
