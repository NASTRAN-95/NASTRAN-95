!*==ferlts.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ferlts(Ifile,Dz,Dy,Zm)
   IMPLICIT NONE
   USE C_FEERIM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Ifile
   REAL , DIMENSION(1) :: Dz
   REAL , DIMENSION(1) :: Dy
   REAL , DIMENSION(1) :: Zm
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: dcore
   REAL :: dsum
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
!   FEER MATRIX TRANSPOSE MULTIPLY  (SINGLE PRECISION)
!   SEE SUBROUTINE FERRDM FOR CONTENTS OF SMAPOS AND HOW THE MATRIX
!   DATA IS STORED IN MEMORY.
!
   !>>>>EQUIVALENCE (Dcore(1),Icore(1))
         n = Ifile(2)
         iccol = 1
         IF ( Nidsma==0 ) THEN
            CALL rewind(Ifile)
            CALL skprec(Ifile,1)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ELSE
            mem = Nidsma
            ilcol = Smapos(1)
            DO i = 1 , n
               iccol = i
! CHECK TO SEE IF REMAINING DATA IS ON THE FILE AND NOT IN MEMORY
               IF ( iccol>ilcol ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Dy(i) = 0.
               dsum = 0.
               SPAG_Loop_2_1: DO
                  icol = Icore(mem)
                  IF ( icol/=i ) EXIT SPAG_Loop_2_1
                  ntms = Icore(mem+1)
                  Ip = Icore(mem+2+ntms)
                  Np = Ip + ntms - 1
                  indx = mem + 1
                  ii = 0
                  DO j = Ip , Np
                     ii = ii + 1
                     dsum = dsum + dcore(indx+ii)*Dz(j)
                  ENDDO
                  Dy(i) = dsum
                  mem = mem + 4 + ntms
               ENDDO SPAG_Loop_2_1
            ENDDO
            RETURN
         ENDIF
      CASE (2)
         CALL dsspos(Ifile,Smapos(2),Smapos(3),Smapos(4))
         spag_nextblock_1 = 3
      CASE (3)
         Incr = 1
         Iprc = Ifile(5)
         DO i = iccol , n
            Dy(i) = 0.
            Ip = 0
            CALL unpack(*20,Ifile,Zm(1))
            ii = 0
            dsum = 0.0
            DO j = Ip , Np
               ii = ii + 1
               dsum = dsum + Zm(ii)*Dz(j)
            ENDDO
            Dy(i) = dsum
 20      ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ferlts
