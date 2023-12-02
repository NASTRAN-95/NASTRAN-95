!*==frmltx.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frmltx(Ifile,Dz,Dy,Zm)
   USE c_feerxx
   USE c_system
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
   REAL(REAL64) :: dp , dsum
   INTEGER :: i , ifl , ii , j , jj , ll , ll2 , n , nrec , ntms , nwds
   INTEGER , DIMENSION(2) :: idp
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL mesage , read , rewind , skprec , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     LOWER TRIANGULAR TRANSPOSE WITH OFF-DIAGONAL SWITCH
!     DOUBLE PRECISION VERSION
!
!     LAST REVISED  11/91, BY G.CHAN/UNISYS
!     ADDITIONAL OF A NEW METHOD WHICH IS MORE EFFICIENT, AND IS
!     ALREADY GOOD FOR VECTORIZATION
!
   !>>>>EQUIVALENCE (dp,idp(1))
   DATA nam/4HFRML , 4HTX  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         n = Ifile(2)
         ifl = Ifile(1)
         IF ( Ifile(7)<0 ) ifl = -Ifile(7)
         CALL rewind(ifl)
         IF ( Ifile(7)>=0 ) THEN
            CALL skprec(ifl,1)
            ityp = Ifile(5)
!
!     NASTRAN ORIGINAL METHOD
!
            incr = 1
            DO i = 1 , n
               Dy(i) = 0.0D+0
               ip = 0
               CALL unpack(*20,ifl,Zm(1))
               IF ( ip==i ) Zm(1) = -Zm(1)
               dsum = 0.D0
               ii = 0
               DO j = ip , np
                  ii = ii + 1
                  dsum = dsum - Zm(ii)*Dz(j)
               ENDDO
               Dy(i) = dsum
            ENDDO
            RETURN
         ENDIF
!
!     NEW METHOD
!
!     UNLIKE FRMLTD, IFL WAS UNPACKED BACKWARD FIRST, THEN FORWARD BY
!     UNPSCR/FEER3. SO WE SKIP BACKWARD PASS BEFORE READING DATA
!
 20      nrec = Ifile(4)/10
         CALL skprec(ifl,nrec+1)
         nwds = Ifile(5)
         nrec = 0
         ll2 = 0
         ntms = 1
         DO i = 1 , n
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( ntms<ll2 ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  nrec = nrec + 1
                  CALL read(*40,*22,ifl,Zm,nzm,1,ll)
                  CALL mesage(-8,0,nam)
 22               ll2 = ll/nwds
                  ntms = 1
                  spag_nextblock_2 = 2
               CASE (2)
                  dp = Zm(ntms)
                  ii = idp(1)
                  jj = idp(2)
                  IF ( ii/=i ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  Zm(ntms+1) = -Zm(ntms+1)
                  dsum = 0.0D+0
                  ll = ntms
                  DO j = ii , jj
                     ll = ll + 1
                     dsum = dsum - Zm(ll)*Dz(j)
                  ENDDO
                  Dy(i) = dsum
                  ntms = ntms + jj - ii + 2
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         RETURN
!
 40      j = Ifile(4)/10
         WRITE (nout,99001) nrec , i , n , j
99001    FORMAT ('0*** TRY TO READ RECORD',I5,'.  I,N,IFILE(4) =',2I7,I5)
         CALL mesage(-2,ifl,nam)
         spag_nextblock_1 = 2
      CASE (2)
         WRITE (nout,99002) ii , i
99002    FORMAT ('0*** II AND I MISMATCH =',2I8)
         CALL mesage(-37,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE frmltx
