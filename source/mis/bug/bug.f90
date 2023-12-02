!*==bug.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bug(Name,Loc,Buf,Nwds)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: Name
   INTEGER :: Loc
   REAL , DIMENSION(1) :: Buf
   INTEGER :: Nwds
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(4) , DIMENSION(28) :: a
   CHARACTER(8) , DIMENSION(14) :: b
   CHARACTER(4) , SAVE :: blank , xloc
   CHARACTER(8) , SAVE :: err , zero
   INTEGER :: i , j , l
   INTEGER , SAVE :: limit , line , nwpl
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE PRINTS NAME,LOC, AND CONTENT OF BUF ARRAY
!     E.G.   CALL BUG ('SUBR ABC',105,CORE(1),120)
!     LIMITED TO 5000 LINES EACH CALL,  14 VALUES PER LINE
!
!     (THIS ROUTINE REPLACES THE OLD ONE IN NASTRAN)
!     WRITTEN BY G.CHAN/SPERRY     MARCH 1986
!
   !>>>>EQUIVALENCE (a(1),b(1))
   DATA line , nwpl , limit/0 , 14 , 5000/
   DATA zero , blank , xloc , err/' 00 ' , '    ' , 'LOC' , '(ERR)'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL sswtch(20,l)
         IF ( l==0 ) RETURN
!
         ENTRY bug1(Name,Loc,Buf,Nwds)
!     ==============================
!
         IF ( Nwds<0 ) RETURN
         l = 2
         i = 0
         CALL a42k8(Name(1),Name(2),b(1))
         CALL int2k8(*20,Loc,a(3))
         a(4) = a(3)
         a(3) = xloc
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( i>=Nwds ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         i = i + 1
         l = l + 1
         j = numtyp(Buf(i)) + 1
         IF ( j==1 ) THEN
            b(l) = zero
         ELSEIF ( j==2 ) THEN
            CALL int2k8(*20,Buf(i),b(l))
         ELSEIF ( j==3 ) THEN
            CALL fp2k8(*20,Buf(i),b(l))
         ELSEIF ( j==4 ) THEN
            CALL a42k8(Buf(i),Buf(i+1),b(l))
            IF ( numtyp(Buf(i+1))/=3 ) THEN
               a(l*2) = blank
            ELSE
               i = i + 1
            ENDIF
            IF ( i>=Nwds ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            GOTO 20
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!            ZERO,INT,REAL,BCD
 20      b(l) = err
         spag_nextblock_1 = 4
      CASE (4)
         IF ( l<nwpl ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( l>0 ) WRITE (nout,99001) (b(j),j=1,l)
99001    FORMAT (2X,14(A8,1X))
         line = line + 1
         IF ( line>limit ) THEN
!
            WRITE (nout,99002) limit
99002       FORMAT (/2X,'PRINT LINES IN BUG EXCEEDS LIMIT OF',I6)
         ELSE
            l = 0
            IF ( i<Nwds ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bug
