!*==mpy3nu.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3nu(Iz)
   USE c_mpy3cp
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Iz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iacols , ipoint , l , l1 , l2 , lac , ll , lp
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     CALCULATES NEXT TIME USED FOR INDIVIDUAL COLUMNS OF B OR FOR ROWS
!     CORRESPONDING TO NON-ZERO TERMS IN COLUMN OF A.
!
   !>>>>EQUIVALENCE (Ipoint,Zpntrs(3)) , (Iacols,Zpntrs(5))
   DATA name/4HMPY3 , 4HNU  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CALCULATION BY SEARCH THROUGH ROW OF A IN QUESTION.
!
         lp = ipoint + id - 1
         l1 = Iz(lp)
         IF ( l1==0 ) THEN
!
!    ERROR MESSAGE.
!
            WRITE (nout,99001) ufm
99001       FORMAT (A23,' 6557, UNEXPECTED NULL COLUMN OF A(T) ENCOUNTERED.')
            CALL mesage(-37,0,name)
         ELSE
            IF ( id/=ncb ) THEN
               ll = id + 1
               DO l = ll , ncb
                  lp = lp + 1
                  IF ( Iz(lp)/=0 ) THEN
                     l2 = Iz(lp) - 1
                     GOTO 10
                  ENDIF
               ENDDO
            ENDIF
            l2 = laend
 10         lac = iacols + l1 - 2
            DO l = l1 , l2
               lac = lac + 1
               IF ( j<Iz(lac) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            ntbu = 99999999
         ENDIF
         RETURN
      CASE (2)
         ntbu = Iz(lac)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mpy3nu