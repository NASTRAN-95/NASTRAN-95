!*==frmlta.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frmlta(Ifile,Z,Y,Zm)
   USE c_feerxx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Ifile
   REAL , DIMENSION(1) :: Z
   REAL , DIMENSION(1) :: Y
   REAL , DIMENSION(1) :: Zm
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dp , sum
   INTEGER :: i , idp , ifl , ii , ilf , j , jj , ll , ll2 , n , nrec , ntms
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL mesage , read , rewind , skprec , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     LOWER TRIANGULAR TRANSPOSE WITH OFF-DIAGONAL SWITCH
!     SINGLE PRECISION VERSION
!
!     LAST REVISED  11/91, BY G.CHAN/UNISYS
!     ADDITIONAL OF A NEW METHODS WHICH IS MORE EFFICIENT, AND IS
!     ALREADY GOOD FOR VECTORIZATION
!
   !>>>>EQUIVALENCE (dp,idp)
   DATA nam/4HFRML , 4HTA  /
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
               Y(i) = 0.0
               ip = 0
               CALL unpack(*20,ifl,Zm(1))
               IF ( ip==i ) Zm(1) = -Zm(1)
               sum = 0.0
               ii = 0
               DO j = ip , np
                  ii = ii + 1
                  sum = sum - Zm(ii)*Z(j)
               ENDDO
               Y(i) = sum
            ENDDO
            RETURN
         ENDIF
!
!     NEW METHOD
!
!     UNLIKE FRMLT, IFL WAS UNPACKED BACKWARD FIRST, THEN FORWARD BY
!     UNPSCR/FEER3. SO WE SKIP BACKWARD PASS BEFORE READING DATA
!
 20      nrec = Ifile(4)/10
         CALL skprec(ifl,nrec+1)
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
 22               ll2 = ll
                  ntms = 1
                  spag_nextblock_2 = 2
               CASE (2)
                  dp = Zm(ntms)
                  ii = idp
                  IF ( ii/=i ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  dp = Zm(ntms+1)
                  jj = idp
                  Zm(ntms+2) = -Zm(ntms+2)
                  sum = 0.0
                  ll = ntms + 1
                  DO j = ii , jj
                     ll = ll + 1
                     sum = sum - Zm(ll)*Z(j)
                  ENDDO
                  Y(i) = sum
                  ntms = ntms + jj - ii + 3
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         RETURN
!
 40      j = Ifile(4)/10
         WRITE (nout,99001) nrec , i , n , j
99001    FORMAT ('0*** TRY TO READ RECORD',I5,'.  I,N,IFILE(4) =',2I7,I5)
         CALL mesage(-2,ilf,nam)
         spag_nextblock_1 = 2
      CASE (2)
         WRITE (nout,99002) ii , i
99002    FORMAT ('0*** II AND I MISMATCH =',2I8)
         CALL mesage(-37,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE frmlta
