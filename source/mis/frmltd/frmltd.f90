!*==frmltd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frmltd(Ifile,Dz,Dy,Zm)
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
   INTEGER :: i , ifl , ii , j , jj , ll , ll2 , n , next , nrec , nwds
   INTEGER , DIMENSION(2) :: idp
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL mesage , read , rewind , skprec , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     FEER MATRIX TRANSPOSE MULTIPLY  (DOUBLE PREC)
!               T
!     DY = IFILE * DZ        WHERE DZ IS A VECTOR ALREADY IN CORE
!                            IFILE IS A GINO MATIRX FILE
!
!     LAST REVISED  11/91, BY C.CHAN/UNISYS
!     ADDITION OF A NEW TRANSPOSE MULTIPLY METHOD WHICH IS MORE
!     EFFECIENT, AND IS ALREADY GOOD FOR VECTORIZATION
!
!DB   LOGICAL          DEBUG
   !>>>>EQUIVALENCE (dp,idp(1))
   DATA nam/4HFRML , 4HTD  /
!DB   DATA     DEBUG , ITER     ,MAX     / .FALSE.  ,0      ,4       /
!
!DB   IF (.NOT.DEBUG) GO TO 20
!     ITER = ITER + 1
!     IF (ITER .GT. MAX) DEBUG = .FALSE.
!     IF (DEBUG) WRITE (NOUT,10) NZM,IFILE(5)
!  10 FORMAT ('  .... IN FRMLTD DEBUG.   NZM,IFILE(5) =',2I8)
!  20 CONTINUE
   n = Ifile(2)
   ifl = Ifile(1)
   IF ( Ifile(7)<0 ) ifl = -Ifile(7)
   CALL rewind(ifl)
   CALL skprec(ifl,1)
   IF ( Ifile(7)<0 ) THEN
!
!     NEW METHOD, READ ONLY AND NO UNPACK
!
!     UNLIKE FRMLTX, IFL WAS UNPACKED FORWARD BY UNPSCR
!
      nrec = 0
      nwds = Ifile(5)
!DB   N20  = N - 20
!     IF (DEBUG) WRITE (NOUT,60) NWDS,NZM
!  60 FORMAT ('  /@60   NWDS,NZM =',2I8)
      ll2 = 0
      next = 1
      DO i = 1 , n
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               IF ( next<ll2 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               nrec = nrec + 1
!DB   IF (DEBUG) WRITE (NOUT,70) NREC,I
!  70 FORMAT ('  ...READING RECORD',I5,'.  I =',I7)
               CALL read(*100,*5,ifl,Zm,nzm,1,ll)
               CALL mesage(-8,0,nam)
 5             ll2 = ll/nwds
!DB   IF (DEBUG) WRITE (NOUT,90) LL,NREC,LL2
!  90 FORMAT (1X,I10,' WORDS READ FROM RECORD NO.',I5,'   LL2 =',I10)
               next = 1
               spag_nextblock_1 = 2
            CASE (2)
               dp = Zm(next)
               ii = idp(1)
               jj = idp(2)
!DB   IF (DEBUG .AND. (I.LT.20 .OR. I.GT.N20)) WRITE (NOUT,110) I,II,JJ,
!     1                                                         NEXT
! 110 FORMAT ('   @110  I,II,JJ,NEXT =',4I8)
               IF ( ii==jj ) THEN
                  Dy(i) = Zm(next+1)*Dz(ii)
               ELSE
                  dsum = 0.0D+0
                  ll = next
                  DO j = ii , jj
                     ll = ll + 1
                     dsum = dsum + Zm(ll)*Dz(j)
                  ENDDO
                  Dy(i) = dsum
               ENDIF
               next = next + jj - ii + 2
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
   ELSE
      ityp = Ifile(5)
!
!     NASTRAN ORIGIANL METHOD
!
      incr = 1
      DO i = 1 , n
         Dy(i) = 0.0D+0
         ip = 0
         CALL unpack(*50,ifl,Zm(1))
         dsum = 0.0D+0
         ii = 0
         DO j = ip , np
            ii = ii + 1
            dsum = dsum + Zm(ii)*Dz(j)
         ENDDO
         Dy(i) = dsum
 50   ENDDO
   ENDIF
   RETURN
!
 100  j = Ifile(4)/10
   WRITE (nout,99001) nrec , i , n , j
99001 FORMAT ('0*** TRY TO READ RECORD',I5,'.   I,N,IFILE(4) =',2I7,I5)
   CALL mesage(-2,ifl,nam)
!
END SUBROUTINE frmltd
