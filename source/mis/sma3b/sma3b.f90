!*==sma3b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sma3b(Iflag,Izk)
   USE c_genely
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iflag
   INTEGER :: Izk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j , nz
   INTEGER , DIMENSION(7) :: se , ze
   EXTERNAL bckrec , bldpk , bldpkn , close , fread , gopen , korsz , makmcb , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE PROCESSES A GENERAL ELEMENT FROM GEI
!
!     IT  PRODUCES A ZE MATRIX OR A ZINVS MATRIX AND A SE MATRIX
!
!     ASSUMES GEI SITS AT BEGINNING OF UI SET AND IS OPEN TO READ
!
!
!
!     COMPUTE LENGTH OF VARIABLE CORE
!
   nz = korsz(z) - sysbuf
   Iflag = -1
   nz = nz - sysbuf
!
! SKIP M+N WORDS ON GEI FILE
!
   CALL fread(gei,z,m+n,0)
!
! READ FLAG VARIABLE FOR Z OR K MATRIX
!
   CALL fread(gei,Izk,1,0)
!
! IF Z MATRIX INPUT,WRITE ON ZE1 FILE
! IF K MATRIX INPUT,WRITE ON ZINVS FILE
!
   CALL makmcb(ze,ze1,m,6,2)
   IF ( Izk==2 ) ze(1) = zinvs
   CALL makmcb(se,se1,n,2,2)
!
! READY FOR PACKING MATRICES
!
!
! OPEN ZE MATRIX
!
   CALL gopen(ze,z(nz+1),1)
!
!     LOOP ON M COLUMNS OF ZE
!
   DO i = 1 , m
      CALL bldpk(2,2,ze(1),0,0)
      DO j = 1 , m
         CALL fread(gei,z,1,0)
         d11(1) = z(1)
         idx = j
         CALL zblpki
      ENDDO
      CALL bldpkn(ze(1),0,ze)
   ENDDO
   CALL close(ze(1),1)
   CALL wrttrl(ze)
   IF ( n/=0 ) THEN
      Iflag = 1
!
!     NOW BUILD SE TRANSPOSE
!
!
!     OPEN AND WRITE HEADER
!
      CALL gopen(se,z(nz+1),1)
!
!     LOOP ON N COLUMNS OF SE
!     LOOP ON M COLUMNS OF SE  TRANSPOSE
!
      DO i = 1 , m
         CALL bldpk(2,2,se(1),0,0)
         DO j = 1 , n
            CALL fread(gei,z,1,0)
            d11(1) = -z(1)
            idx = j
            CALL zblpki
         ENDDO
         CALL bldpkn(se(1),0,se)
      ENDDO
      CALL close(se(1),1)
      CALL wrttrl(se)
   ENDIF
!
!     BACKSPACE GEI SO UD AND UI AVAILABLE LATER
!
   CALL bckrec(gei)
   CALL close(gei,2)
   id(1) = id1
   id(2) = m
   id(3) = m
   id(4) = 8
   id(5) = 2
   id(6) = 1
   id(7) = 0
END SUBROUTINE sma3b
