
SUBROUTINE edit(Name,Iopt,Itest)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Item(7,1) , Nitem
   COMMON /itemdt/ Nitem , Item
!
! Dummy argument declarations
!
   INTEGER Iopt , Itest
   INTEGER Name(2)
!
! Local variable declarations
!
   INTEGER andf
   INTEGER i , index , it , mask , nmsbr(2)
   EXTERNAL andf
!
! End of declarations
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
         GOTO 99999
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
99999 END SUBROUTINE edit
