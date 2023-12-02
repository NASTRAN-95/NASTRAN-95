!*==edit.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE edit(Name,Iopt,Itest)
   IMPLICIT NONE
   USE C_ITEMDT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: Iopt
   INTEGER :: Itest
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , index , it , mask
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , delete , fdsub
!
! End of declarations rewritten by SPAG
!
!
!     REMOVES SELECTED ITEMS OF THE SUBSTRUCTURE NAME FROM THE SOF.
!     THE VALUE OF IOPT IS THE SUM OF THE FOLLOWING INTEGERS REFLECTING
!     WHICH ITEMS ARE TO BE REMOVED.
!
!              1 = STIFFNESS MATRIX
!              2 = MASS MATRIX
!              4 = LOAD DATA
!              8 = SOLUTION DATA
!             16 = TRANSFORMATION DATA
!             32 = ALL ITEMS OF SUBSTRUCTURE
!             64 = APPENDED LOADS DATA
!            128 = DAMPING MATRICES
!            256 = MODES DATA
!
!     THE OUTPUT VARIABLE ITEST TAKES ON ONE OF THE FOLLOWING VALUES
!              1   NORMATL RETURN
!              4   IF NAME DOES NOT EXIST
!
   DATA nmsbr/4HEDIT , 4H    /
!
   CALL chkopn(nmsbr(1))
   Itest = 1
   IF ( Iopt>0 ) THEN
      CALL fdsub(Name(1),index)
      IF ( index==-1 ) THEN
!
!     NAME DOES NOT EXIST.
!
         Itest = 4
         RETURN
      ELSE
!
!     REMOVE SELECTED ITEMS ACCORDING TO IOPT S VALUE.
!
         DO i = 1 , Nitem
            mask = Item(7,i)
            IF ( andf(Iopt,mask)/=0 ) CALL delete(Name,Item(1,i),it)
         ENDDO
      ENDIF
   ENDIF
   RETURN
END SUBROUTINE edit
