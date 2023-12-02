!*==ferltd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ferltd(Ifile,Dz,Dy,Zm)
   USE c_feerim
   USE c_unpakx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Ifile
   REAL(REAL64) , DIMENSION(1) :: Dz
   REAL(REAL64) , DIMENSION(1) :: Dy
   REAL(REAL64) , DIMENSION(1) :: Zm
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dcore
   REAL(REAL64) :: dsum
   INTEGER :: i , iccol , icol , ii , ilcol , indx , j , mem , n , ntms
   EXTERNAL dsspos , rewind , skprec , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!  FERLTD was originally subroutine FRMLTD.  FERLTD allows for
!  reading the input matrix from core and after the core data is
!  exhausted, then reading the remaining data from the file.
!  See subroutine FERRDM for how data is stored within memory for the
!  matrix and for the contents of SMAPOS.
!
!   FEER MATRIX TRANSPOSE MULTIPLY  (DOUBLE PREC)
!
   !>>>>EQUIVALENCE (Dcore(1),Icore(1))
         n = Ifile(2)
         iccol = 1
         IF ( nidsma==0 ) THEN
            CALL rewind(Ifile)
            CALL skprec(Ifile,1)
            spag_nextblock_1 = 3
         ELSE
            mem = nidsma
            ilcol = smapos(1)
            DO i = 1 , n
               iccol = i
! CHECK TO SEE IF REMAINING DATA IS ON THE FILE AND NOT IN MEMORY
               IF ( iccol>ilcol ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Dy(i) = 0.D0
               dsum = 0.D0
               SPAG_Loop_2_1: DO
                  icol = icore(mem)
                  IF ( icol/=i ) EXIT SPAG_Loop_2_1
                  ntms = icore(mem+1)
                  ip = icore(mem+2+2*ntms)
                  np = ip + ntms - 1
                  indx = mem/2 + 1
                  ii = 0
                  DO j = ip , np
                     ii = ii + 1
                     dsum = dsum + dcore(indx+ii)*Dz(j)
                  ENDDO
                  Dy(i) = dsum
                  mem = mem + 4 + 2*ntms
               ENDDO SPAG_Loop_2_1
            ENDDO
            RETURN
         ENDIF
      CASE (2)
         CALL dsspos(Ifile,smapos(2),smapos(3),smapos(4))
         spag_nextblock_1 = 3
      CASE (3)
         incr = 1
         ityp = Ifile(5)
         DO i = iccol , n
            Dy(i) = 0.D0
            ip = 0
            CALL unpack(*20,Ifile,Zm(1))
            ii = 0
            dsum = 0.D0
            DO j = ip , np
               ii = ii + 1
               dsum = dsum + Zm(ii)*Dz(j)
            ENDDO
            Dy(i) = dsum
 20      ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ferltd
