!*==modac2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE modac2(Nv,Inp1,Iout)
   USE c_modac3
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nv
   INTEGER :: Inp1
   INTEGER :: Iout
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibuf1 , ibuf2 , icol , ifn , ikr , inv , ip1 , j , k , m , nload
   INTEGER , DIMENSION(2) :: ihd
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: z
   EXTERNAL close , cyct2b , fname , fwdrec , gopen , mesage , open , rdtrl , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     MODAC2  REDUCES THE SIZE OF INP1 (BY REMOVING SELECTED COLUMNS)
!
!     CORE IS LAIDED OUT AS FOLLOWS
!
!         CONTENTS            LENGTH  TYPE   POINTER
!         --------            ------  ----   -------
!
!         NEW TIMES           NFN      R     IFN
!         KEEP/REMOVE         NFO      I     IKR
!         COPIED COLUMN       MCB(3)   R     ICOL
!
!         2  BUFFERS          SYSBUF   I     IBUF1
!                             SYSBUF   I     IBUF2
!
!     VARIABLES
!
!     NV       NUMBER OF COLUMS TO PROCESS TOGETHER (MINUS SAYS ADD HEAD
!     INP1     COPY FROM THIS FILE
!     IOUT     COPY TO  THIS  FILE
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HMODA , 4HC2  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ALLOCATE CORE
!
         mcb(1) = Iout
         CALL rdtrl(mcb)
         IF ( mcb(1)<=0 ) RETURN
         mcb(1) = Inp1
         CALL rdtrl(mcb)
         IF ( mcb(1)<=0 ) RETURN
         nload = mcb(2)/(nfo*iabs(Nv))
         ifn = 1
         ikr = ifn + nfn
         icol = ikr + nfo
         ibuf1 = nz - sysbuf + 1
         ibuf2 = ibuf1 - sysbuf
         IF ( icol+mcb(3)+2*sysbuf>nz ) CALL mesage(-8,0,name)
!
!     OPEN  FILES
!
         file = Inp1
         CALL gopen(Inp1,iz(ibuf1),0)
         file = Iout
         CALL open(*20,Iout,iz(ibuf2),1)
         CALL fname(Iout,ihd)
         CALL write(Iout,ihd,2,0)
         IF ( Nv<=0 ) CALL write(Iout,z,nfn,0)
         CALL write(Iout,0,0,1)
!
!     SET UP MATRIX TRAILER
!
         file = Inp1
         mcb(2) = 0
         mcb(6) = 0
         mcb(7) = 0
         mcb(1) = Iout
         itc = mcb(5)
         incr = 1
         inv = iabs(Nv)
         DO m = 1 , nload
            k = ikr - 1
            DO i = 1 , nfo
               k = k + 1
               IF ( iz(k)==0 ) THEN
!
!     SKIP COLUMN
!
                  DO j = 1 , inv
                     CALL fwdrec(*40,Inp1)
                  ENDDO
               ELSE
!
!     KEEP COLUMN
!
                  CALL cyct2b(Inp1,Iout,inv,iz(icol),mcb)
               ENDIF
            ENDDO
         ENDDO
!
!     CLOSE  UP
!
         CALL close(Inp1,1)
         CALL close(Iout,1)
         CALL wrttrl(mcb)
         RETURN
!
!     ERROR MESSAGES
!
 20      ip1 = -1
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(ip1,file,name)
 40      ip1 = -2
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE modac2
