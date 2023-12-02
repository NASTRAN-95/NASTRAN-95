!*==read7.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE read7(Nr1,Olama,Ophia,Nlama,Nphia)
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nr1
   INTEGER :: Olama
   INTEGER :: Ophia
   INTEGER :: Nlama
   INTEGER :: Nphia
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: dcore
   REAL(REAL64) :: dx
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ifile , ifl , j , lc , nn , nr , nrow , sgldbl
   INTEGER , DIMENSION(7) :: ix
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(7) :: x
   EXTERNAL close , fwdrec , gopen , korsz , makmcb , mesage , pack , rdtrl , read , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     READ7  COPIES NR VECTORS FROM OPHIA TO NPHIA -
!     IT ALSO PLACES THE EIGENVALUES ON NLAMA
!     THIS ROUTINE HANDLES BOTH SINGLE AND DOUBLE PRECISION
!
   !>>>>EQUIVALENCE (Dcore(1),Core(1)) , (x(1),dx)
   DATA name/4HREAD , 4H7   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     GET ORGANIZED
!
         nr = Nr1
         lc = korsz(Core)
         ibuf1 = lc - Sysbuf + 1
         ibuf2 = ibuf1 - Sysbuf
         ibuf3 = ibuf2 - Sysbuf
         ibuf4 = ibuf3 - Sysbuf
         ix(1) = Ophia
         CALL rdtrl(ix)
         nrow = ix(3)
         Ii = 1
         Jj = nrow
         It1 = ix(5)
         It2 = It1
         Itb = It1
         dcore(1) = 0.0D0
         Incrp = 1
         ASSIGN 2 TO sgldbl
         IF ( Itb==2 ) ASSIGN 4 TO sgldbl
         Incur = 1
!
!     OPEN OLD FILES
!
         CALL gopen(Olama,Core(ibuf1),0)
         CALL fwdrec(*20,Olama)
         CALL gopen(Ophia,Core(ibuf2),0)
!
!     OPEN NEW FILES TO WRITE
!
         CALL gopen(Nlama,Core(ibuf3),1)
         CALL gopen(Nphia,Core(ibuf4),1)
!
!     START COPY LOOP
!
         CALL makmcb(ix,Nphia,nrow,ix(4),It2)
         DO i = 1 , nr
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL read(*20,*40,Olama,x,7,0,ifl)
                  Ii = 0
                  CALL unpack(*6,Ophia,dcore(2))
                  GOTO sgldbl
 2                x(1) = sqrt(x(6))
                  DO j = 1 , nrow
                     Core(j+2) = Core(j+2)/x(1)
                  ENDDO
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 4                dx = sqrt(x(6))
                  DO j = 1 , nrow
                     dcore(j+1) = dcore(j+1)/dx
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
                  Iip = Ii
                  Jjp = Jj
                  CALL pack(dcore(2),Nphia,ix)
                  spag_nextblock_2 = 3
               CASE (3)
                  dx = x(3)
                  CALL write(Nlama,dx,2,1)
                  CYCLE
!
!     NULL COLUMN
!
 6                Iip = 1
                  Jjp = 1
                  CALL pack(dcore,Nphia,ix)
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(Olama,1)
         CALL close(Ophia,1)
         CALL close(Nlama,2)
         CALL close(Nphia,1)
         RETURN
!
!     ERRORS
!
 20      nn = -2
         spag_nextblock_1 = 2
      CASE (2)
         ifile = Olama
         CALL mesage(nn,ifile,name)
         RETURN
 40      nn = -3
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE read7
