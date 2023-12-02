!*==emg1b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emg1b(Buf,Sil,Ii,File,Dampc)
USE C_EMG1BX
USE C_EMGDIC
USE C_EMGPRM
USE C_IEMG1B
USE C_SMA1IO
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: Buf
   INTEGER :: Sil
   INTEGER :: Ii
   INTEGER :: File
   REAL(REAL64) :: Dampc
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c
   LOGICAL :: double
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , ic , idbloc , if4gg , j , j1 , j2 , kmat , ndbloc , outpt , rowsiz , zbase
   REAL , DIMENSION(1) :: rz
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL emgout , mesage
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE REPLACES SMA1B AND GROUPS TOGETHER THE
!     SUB-PARTITIONS OF A PIVOT-PARTITION.
!
!     THE SUB-PARTIONS ARE ARRANGED IN CORE BY ASCENDING SILS OF THE
!     ELEMENT INVOLVED.
!
!
   !>>>>EQUIVALENCE (Ksystm(2),Outpt)
   !>>>>EQUIVALENCE (Z(1),Dz(1),Rz(1)) , (c,ic)
   !>>>>EQUIVALENCE (Smaio(13),If4gg)
!
   DATA subr/4HEMG1 , 4HB   /
!
   IF ( Error ) RETURN
   IF ( Sil==-1111111 ) THEN
!
!     OUTPUT PIVOT-ROWS-PARTITION
!
      IF ( Icall>0 ) THEN
         IF ( Last ) Ilast = 1
         CALL emgout(Z(Ibloc),Z(Ibloc),(Nbloc-Ibloc+1)/Precis,Ilast,Dict,Filtyp,Precis)
      ENDIF
      Ilast = 0
      Icall = 0
      RETURN
   ELSE
      double = .FALSE.
      IF ( Precis==2 ) double = .TRUE.
      Icall = Icall + 1
!
!     IF -FILE- EQUALS IF4GG FOR THE OLD ELEMENTS, THEN THE ELEMENT
!     DAMPING CONSTANT SENT IS PLACED IN THE DICTIONARY AND A SIMPLE
!     RETURN IS MADE.
!
      IF ( .NOT.(Heat) ) THEN
         IF ( File==if4gg ) THEN
            c = Dampc
            Dict(5) = ic
            Icall = Icall - 1
            RETURN
         ELSEIF ( Ii<=0 ) THEN
            Irows = 6
            Dict(4) = 63
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
!
      Irows = 1
      Dict(4) = 1
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      IF ( Icall<=1 ) THEN
         rowsiz = Nsils*Irows
         Dict(3) = rowsiz
         Ibloc = Jcore + mod(Jcore+1,2)
         IF ( Dict(2)==2 ) THEN
            Nbloc = Ibloc + Irows*Precis - 1
         ELSE
            Nbloc = Ibloc + rowsiz*Irows*Precis - 1
         ENDIF
         IF ( Nbloc>Ncore ) CALL mesage(-8,Nbloc-Ncore,subr)
         IF ( double ) THEN
!
            idbloc = Ibloc/2 + 1
            ndbloc = Nbloc/2
            DO i = idbloc , ndbloc
               dz(i) = 0.0D0
            ENDDO
         ELSE
            DO i = Ibloc , Nbloc
               rz(i) = 0.0E0
            ENDDO
         ENDIF
      ENDIF
!
!     INSERT SUB-PARTITION OF PARTITION IN POSITION OF SIL ORDER.
!
!     BUF IS ASSUMED DOUBLE PRECISION.
!
      DO i = 1 , Nsils
         IF ( Sil==Sils(i) ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ENDDO
      WRITE (outpt,99001) Sfm , Elid
99001 FORMAT (A25,' 3116, ELEMENT ID',I10,' SENDS BAD SIL TO ROUTINE ','EMG1B.')
      CALL mesage(-37,0,subr)
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
      IF ( Dict(2)==2 ) THEN
!
!     SIMPLE DIAGONAL MATRIX INSERTION
!
         kmat = 1
         IF ( double ) THEN
!
            j1 = idbloc
            DO i = 1 , Irows
               dz(j1) = dz(j1) + Buf(kmat)
               j1 = j1 + 1
               kmat = kmat + 14
            ENDDO
         ELSE
            j1 = Ibloc
            DO i = 1 , Irows
               rz(j1) = rz(j1) + sngl(Buf(kmat))
               j1 = j1 + 1
               kmat = kmat + 14
            ENDDO
         ENDIF
      ELSE
         zbase = Irows*(i-1)
         kmat = 1
         IF ( double ) THEN
!
!     DOUBLE PRECISION ADDITION OF MATRIX DATA.
!
            j1 = idbloc + zbase
            j2 = j1 + Irows - 1
            DO i = 1 , Irows
               DO j = j1 , j2
                  dz(j) = dz(j) + Buf(kmat)
                  kmat = kmat + 1
               ENDDO
               j1 = j1 + rowsiz
               j2 = j2 + rowsiz
            ENDDO
         ELSE
!
!     SINGLE PRECISION ADDITION OF DATA
!
            j1 = Ibloc + zbase
            j2 = j1 + Irows - 1
            DO i = 1 , Irows
               DO j = j1 , j2
                  rz(j) = rz(j) + sngl(Buf(kmat))
                  kmat = kmat + 1
               ENDDO
               j1 = j1 + rowsiz
               j2 = j2 + rowsiz
            ENDDO
         ENDIF
      ENDIF
!
      RETURN
   END SUBROUTINE spag_block_2
END SUBROUTINE emg1b
