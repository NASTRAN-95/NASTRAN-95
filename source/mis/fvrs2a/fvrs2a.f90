!*==fvrs2a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fvrs2a(File,Kk1,Kk2,Noro,Buffer)
   USE c_packx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Kk1
   INTEGER :: Kk2
   INTEGER :: Noro
   INTEGER , DIMENSION(1) :: Buffer
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: k1 , k2 , row
   INTEGER , DIMENSION(7) :: trl
   REAL :: value
   EXTERNAL close , gopen , pack , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     GENERATE COLUMN REORDERING MATRIX. THIS MATRIX WILL REORDER
!     COLUMNS OF A MATRIX BY POST-MULTIPLYING THE MATRIX WHOSE
!     COLUMNS ARE TO BE REORDERED BY THE REORDERING MATRIX.
!
!     THE MATRIX WILL BE A REAL SINGLE-PRECISION SQUARE MATRIX.
!
!
!
   Noro = -1
   IF ( Kk1==1 .OR. Kk2==1 ) RETURN
!
   Noro = 1
!
   typin = 1
   typout = 1
   incr = 1
!
   trl(1) = File
   trl(2) = 0
   trl(3) = Kk1*Kk2
   trl(4) = 1
   trl(5) = typout
   trl(6) = 0
   trl(7) = 0
!
   CALL gopen(File,Buffer,1)
!
   value = 1.0
!
   DO k1 = 1 , Kk1
      row = k1
      DO k2 = 1 , Kk2
!
         ii = row
         nn = row
         CALL pack(value,File,trl)
!
         row = row + Kk1
!
      ENDDO
   ENDDO
!
   CALL close(File,1)
   CALL wrttrl(trl)
!
END SUBROUTINE fvrs2a
