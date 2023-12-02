!*==emg1b.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emg1b(Buf,Sil,Ii,File,Dampc)
   USE c_emg1bx
   USE c_emgdic
   USE c_emgprm
   USE c_iemg1b
   USE c_sma1io
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
   IF ( error ) RETURN
   IF ( Sil==-1111111 ) THEN
!
!     OUTPUT PIVOT-ROWS-PARTITION
!
      IF ( icall>0 ) THEN
         IF ( last ) ilast = 1
         CALL emgout(z(ibloc),z(ibloc),(nbloc-ibloc+1)/precis,ilast,dict,filtyp,precis)
      ENDIF
      ilast = 0
      icall = 0
      RETURN
   ELSE
      double = .FALSE.
      IF ( precis==2 ) double = .TRUE.
      icall = icall + 1
!
!     IF -FILE- EQUALS IF4GG FOR THE OLD ELEMENTS, THEN THE ELEMENT
!     DAMPING CONSTANT SENT IS PLACED IN THE DICTIONARY AND A SIMPLE
!     RETURN IS MADE.
!
      IF ( .NOT.(heat) ) THEN
         IF ( File==if4gg ) THEN
            c = Dampc
            dict(5) = ic
            icall = icall - 1
            RETURN
         ELSEIF ( Ii<=0 ) THEN
            irows = 6
            dict(4) = 63
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
!
      irows = 1
      dict(4) = 1
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      IF ( Icall<=1 ) THEN
         Rowsiz = nsils*Irows
         Dict(3) = Rowsiz
         Ibloc = jcore + mod(jcore+1,2)
         IF ( Dict(2)==2 ) THEN
            Nbloc = Ibloc + Irows*Precis - 1
         ELSE
            Nbloc = Ibloc + Rowsiz*Irows*Precis - 1
         ENDIF
         IF ( Nbloc>ncore ) CALL mesage(-8,Nbloc-ncore,Subr)
         IF ( Double ) THEN
!
            Idbloc = Ibloc/2 + 1
            Ndbloc = Nbloc/2
            DO I = Idbloc , Ndbloc
               Dz(I) = 0.0D0
            ENDDO
         ELSE
            DO I = Ibloc , Nbloc
               Rz(I) = 0.0E0
            ENDDO
         ENDIF
      ENDIF
!
!     INSERT SUB-PARTITION OF PARTITION IN POSITION OF SIL ORDER.
!
!     BUF IS ASSUMED DOUBLE PRECISION.
!
      DO I = 1 , nsils
         IF ( Sil==sils(I) ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ENDDO
      WRITE (Outpt,99001) sfm , elid
99001 FORMAT (A25,' 3116, ELEMENT ID',I10,' SENDS BAD SIL TO ROUTINE ','EMG1B.')
      CALL mesage(-37,0,Subr)
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
      IF ( Dict(2)==2 ) THEN
!
!     SIMPLE DIAGONAL MATRIX INSERTION
!
         Kmat = 1
         IF ( Double ) THEN
!
            J1 = Idbloc
            DO I = 1 , Irows
               Dz(J1) = Dz(J1) + Buf(Kmat)
               J1 = J1 + 1
               Kmat = Kmat + 14
            ENDDO
         ELSE
            J1 = Ibloc
            DO I = 1 , Irows
               Rz(J1) = Rz(J1) + sngl(Buf(Kmat))
               J1 = J1 + 1
               Kmat = Kmat + 14
            ENDDO
         ENDIF
      ELSE
         Zbase = Irows*(I-1)
         Kmat = 1
         IF ( Double ) THEN
!
!     DOUBLE PRECISION ADDITION OF MATRIX DATA.
!
            J1 = Idbloc + Zbase
            J2 = J1 + Irows - 1
            DO I = 1 , Irows
               DO J = J1 , J2
                  Dz(J) = Dz(J) + Buf(Kmat)
                  Kmat = Kmat + 1
               ENDDO
               J1 = J1 + Rowsiz
               J2 = J2 + Rowsiz
            ENDDO
         ELSE
!
!     SINGLE PRECISION ADDITION OF DATA
!
            J1 = Ibloc + Zbase
            J2 = J1 + Irows - 1
            DO I = 1 , Irows
               DO J = J1 , J2
                  Rz(J) = Rz(J) + sngl(Buf(Kmat))
                  Kmat = Kmat + 1
               ENDDO
               J1 = J1 + Rowsiz
               J2 = J2 + Rowsiz
            ENDDO
         ENDIF
      ENDIF
!
   END SUBROUTINE spag_block_2
END SUBROUTINE emg1b
