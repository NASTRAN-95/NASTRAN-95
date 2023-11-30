
SUBROUTINE fvrs2a(File,Kk1,Kk2,Noro,Buffer)
   IMPLICIT NONE
   INTEGER Ii , Incr , Nn , Typin , Typout
   COMMON /packx / Typin , Typout , Ii , Nn , Incr
   INTEGER File , Kk1 , Kk2 , Noro
   INTEGER Buffer(1)
   INTEGER k1 , k2 , row , trl(7)
   REAL value
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
   Typin = 1
   Typout = 1
   Incr = 1
!
   trl(1) = File
   trl(2) = 0
   trl(3) = Kk1*Kk2
   trl(4) = 1
   trl(5) = Typout
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
         Ii = row
         Nn = row
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