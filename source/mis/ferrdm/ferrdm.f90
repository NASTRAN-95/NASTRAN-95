!*==ferrdm.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ferrdm(Mcb,Nidx,Memtot,Ibuffi,Lasind,Ipos)
USE C_NAMES
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Mcb
   INTEGER :: Nidx
   INTEGER :: Memtot
   INTEGER :: Ibuffi
   INTEGER :: Lasind
   INTEGER , DIMENSION(7) :: Ipos
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dcore , dxl
   INTEGER :: i , iblock , icbp , iclr , ii , index , iprec , jcol , jrow , mem , mindex , ncol , nout , ntms , ntwds
   INTEGER , DIMENSION(20) :: iblk
   INTEGER , DIMENSION(1) :: ixl
   REAL , DIMENSION(1) :: rcore , rxl
   EXTERNAL close , dscpos , endget , getstr , gopen , rewind , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!  FERRDM - This routine will store an entire matrix in memory
!           if sufficient memory exists.  The matrix
!           is stored in memory according to the following scheme:
!
!  1st word = current column number
!  2nd word = number of terms in string (ntms)
!  3rd word           }
!     |               }
!     |               } = actual
!     |               }   matrix
!     |               }   string
!     |               }   data
!     |               }
!     |               }
!  3+(ntms*prec)      } (where prec=1 for s.p.; =2 for d.p. )
!  3+(ntms*prec)+1 = row position of first element in above string
!  3+(ntms*prec)+2 = number of terms in ABOVE string (ntms)
!
!  The above data repeats for all strings within a column and then
!  for all columns in the matrix.
!
!  Argument list :
!     MCB    - Matrix control block for input matrix
!     NIDX   - Memory index for storing matrix data
!     MEMTOT - Total amount of memory available for this data
!     IBUFFI - Buffer allocation for input matrix
!     LASIND - Memory index of last string stored in memory
!     IPOS   - 6 word array with the following information
!              (1) = last column read into memory
!              (2) = block number of following column not read into memory
!              (3) = current logical record pointer for following column
!                    not read into memory
!              (4) = current buffer pointer for following record not read
!                    into memory
!              (5) = last block number in file
!              (6) = current logical record pointer for last record in file
!              (7) = current buffer pointer for last record in file
!
   !>>>>EQUIVALENCE (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(55),Iprec)
   !>>>>EQUIVALENCE (Icore,Dcore,Rcore,Dxl,Rxl,Ixl)
         mem = Nidx
         ncol = Mcb(2)
         ntwds = 0
         Ipos(1) = ncol
         DO i = 1 , 20
            iblk(i) = 0
         ENDDO
         iblk(1) = Mcb(1)
         iblk(9) = 1
         iblk(10) = 1
         CALL gopen(Mcb,Icore(Ibuffi),Rdrew)
         CALL rewind(Mcb)
         CALL skprec(Mcb,1)
         DO jcol = 1 , ncol
            iblk(8) = -1
            Lasind = mem - 1
            CALL dscpos(Mcb,iblock,iclr,icbp)
            DO
               CALL getstr(*20,iblk(1))
               index = iblk(5)
               ntms = iblk(6)
               jrow = iblk(4)
               ntwds = ntwds + 4 + ntms*iprec
               IF ( ntwds>Memtot ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Icore(mem) = jcol
               Icore(mem+1) = ntms
               IF ( iprec==1 ) THEN
                  mindex = mem + 1
                  DO ii = 1 , ntms
                     rcore(mindex+ii) = rxl(index+ii-1)
                  ENDDO
               ELSE
                  mindex = mem/2 + 1
                  DO ii = 1 , ntms
                     dcore(mindex+ii) = dxl(index+ii-1)
                  ENDDO
               ENDIF
               mem = mem + 2 + ntms*iprec
               Icore(mem) = jrow
               Icore(mem+1) = ntms
               mem = mem + 2
               CALL endget(iblk(1))
            ENDDO
 20      ENDDO
         Lasind = mem - 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         Ipos(1) = jcol - 1
         Ipos(2) = iblock
         Ipos(3) = iclr
         Ipos(4) = icbp
         CALL skprec(Mcb,ncol-jcol+1)
         CALL dscpos(Mcb,iblock,iclr,icbp)
         Ipos(5) = iblock
         Ipos(6) = iclr
         Ipos(7) = icbp
         spag_nextblock_1 = 3
      CASE (3)
         CALL close(Mcb,Rew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ferrdm
