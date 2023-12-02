!*==frsw.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frsw(V1,V2,V3,Vb)
   IMPLICIT NONE
   USE C_FEERXX
   USE C_OPINV
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: V1
   REAL , DIMENSION(1) :: V2
   REAL , DIMENSION(1) :: V3
   REAL , DIMENSION(1) :: Vb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: base , i , ib , ie , ifb , ii , ik , j , ji , jj , jx , l16 , ljj , ll , ll2 , mcbltx , nrec , nrow , ntms
   INTEGER , DIMENSION(15) :: iblk
   INTEGER , DIMENSION(6) , SAVE :: nam
   REAL :: sum , v3j , xljj
   REAL , DIMENSION(1) :: xl
   REAL , SAVE :: zero
   EXTERNAL conmsg , endget , endgtb , frmlt , getstb , getstr , mesage , read , rewind , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     LAST REVISED  11/91, BY G.CHAN/UNISYS
!     ADDITION OF A NEW FORWARD-BACKWARD SUBSTITUTION METHOD, WHICH IS
!     MORE EFFICIENT, AND IS ALREADY GOOD FOR VECTORIZATION.
!
!DB   LOGICAL          DEBUG
   !>>>>EQUIVALENCE (Xl(1),Iz(1))
   !>>>>EQUIVALENCE (ljj,xljj) , (L16,Dumm(6))
   DATA nam/4HFRSW , 4H     , 2*4HBEGN , 4HEND  , 4HBGIN/
   DATA zero/0.0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!DB   DATA     DEBUG , ITER    ,MAX  /  .FALSE.  ,0     ,3      /
!
!DB   IF (.NOT.DEBUG) GO TO 20
!     ITER = ITER + 1
!     IF (ITER .GT. MAX) DEBUG = .FALSE.
!     WRITE  (IO,10) NZVB,ITER
!  10 FORMAT ('  .... IN FRSW2.  NZVB =',I8,',   ITER =',I3)
!  20 CONTINUE
         nrow = Mcblt(2)
         CALL frmlt(Mcbsma(1),V1(1),V3(1),Vb(1))
         IF ( Mcblt(7)<0 ) THEN
!
!     NEW METHOD
!
!     THE MCBLT MATRIX HAS BEEN RE-WRITTEN FORWARD FIRST THAN BACKWARD
!     BY UNPSCR IN FEER3. NO STRING OPERATION HERE
!
            IF ( nam(3)==nam(5) ) nam(3) = nam(6)
            IF ( l16/=0 ) CALL conmsg(nam,3,0)
            mcbltx = -Mcblt(7)
            IF ( mod(Mcblt(4),10)/=2 ) THEN
               j = mod(Mcblt(4),10)
               WRITE (Io,99001) j
99001          FORMAT ('0*** MCBLT MATRIX IN WRONG FORM.  UNPSCR FLAG =',I3)
               CALL mesage(-37,0,nam)
            ELSE
               CALL rewind(mcbltx)
               CALL skprec(mcbltx,1)
!     NWDS = MCBLT(5)
!
!     IZ(1)                                                      GINO
!      / V1   V2    V3          VB (OPEN CORE LENGTH = NZVB)    BUFFERS
!     +-----+-----+-----+-----+-------------------------------+---------
!                         OPEN  CORE
!
!     FORWARD SWEEP DIRECTLY ON V3
!
               nrec = 0
               ll2 = 0
               base = 1
               ifb = +450
               DO j = 1 , nrow
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        IF ( base<ll2 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        nrec = nrec + 1
!DB   IF (DEBUG) WRITE (IO,210) NREC,IFB
! 210 FORMAT ('  ...READING RECORD',I5,'.   IFB =',I5)
                        CALL read(*40,*2,mcbltx,Vb,Nzvb,1,ll)
                        CALL mesage(-8,0,nam)
! 220 LL2  = LL/NWDS
 2                      ll2 = ll
!DB   LL3  = LL2/30
!     LL4  = LL2 - LL3
                        base = 1
                        spag_nextblock_2 = 2
                     CASE (2)
                        xljj = Vb(base)
                        ii = ljj
                        xljj = Vb(base+1)
                        jj = ljj
!DB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
!    1    WRITE (IO,240) IFB,J,BASE,II,JJ
! 240 FORMAT (11X,'IFB,J,BASE,II,JJ =',4I8)
                        IF ( ii/=j ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        ntms = jj - ii + 1
                        ib = base + 3
                        ie = base + 1 + ntms
                        base = ie + 1
                        IF ( ntms>1 ) THEN
                           v3j = V3(j)
                           IF ( v3j/=zero ) THEN
                              DO i = ib , ie
                                 ii = ii + 1
                                 V3(ii) = V3(ii) + Vb(i)*v3j
                              ENDDO
                           ENDIF
                        ENDIF
                        V3(j) = V3(j)/Vb(ib-1)
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
!
!     BACKWARD SUBSTITUTION OMIT DIAGONAL
!
               IF ( nrow/=1 ) THEN
                  nrec = 0
                  ll2 = 0
                  base = 1
                  j = nrow
                  ifb = -490
                  DO jx = 1 , nrow
                     spag_nextblock_3 = 1
                     SPAG_DispatchLoop_3: DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
                           IF ( base<ll2 ) THEN
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                           nrec = nrec + 1
!DB   IF (DEBUG) WRITE (IO,210) NREC,IFB
                           CALL read(*40,*4,mcbltx,Vb,Nzvb,1,ll)
                           CALL mesage(-8,0,nam)
! 270 LL2  = LL/NWDS
 4                         ll2 = ll
!DB   LL3  = LL2/30
!     LL4  = LL2 - LL3
                           base = 1
                           spag_nextblock_3 = 2
                        CASE (2)
                           xljj = Vb(base)
                           ii = ljj
                           xljj = Vb(base+1)
                           jj = ljj
!DB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
!    1    WRITE (IO,240) IFB,J,BASE,II,JJ
                           IF ( ii/=j ) THEN
                              spag_nextblock_1 = 3
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           ntms = jj - ii + 1
                           ib = base + 3
                           ie = base + 1 + ntms
                           base = ie + 1
                           IF ( ntms>1 ) THEN
                              sum = zero
                              DO i = ib , ie
                                 ii = ii + 1
                                 sum = sum + Vb(i)*V3(ii)
                              ENDDO
                              V3(j) = V3(j) + sum
                           ENDIF
                           j = j - 1
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
                  ENDDO
               ENDIF
            ENDIF
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     NASTRAN ORIGINAL METHOD
!
            iblk(1) = Mcblt(1)
            iblk(9) = 1
            iblk(10) = 1
            CALL rewind(Mcblt)
            CALL skprec(Mcblt,1)
!
!     FORWARD SWEEP DIRECTLY ON V3
!
            DO j = 1 , nrow
               iblk(8) = -1
               DO
                  CALL getstr(*5,iblk(1))
                  ji = iblk(5)
                  ntms = iblk(6)
                  ik = iblk(4)
                  IF ( ik==j ) THEN
                     ntms = ntms - 1
                     xljj = xl(ji)
                     ji = ji + 1
                     ik = ik + 1
                  ENDIF
                  IF ( ntms/=0 ) THEN
                     v3j = V3(j)
                     IF ( v3j/=zero ) THEN
                        DO ii = 1 , ntms
                           V3(ik) = V3(ik) + xl(ji)*v3j
                           ik = ik + 1
                           ji = ji + 1
                        ENDDO
                     ENDIF
                  ENDIF
                  CALL endget(iblk(1))
               ENDDO
 5             V3(j) = V3(j)/xljj
            ENDDO
!
!     BACKWARD SUBSTITUTION OMIT DIAGONAL
!
            IF ( nrow==1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            j = nrow
            iblk(8) = -1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO
            CALL getstb(*20,iblk(1))
            ntms = iblk(6)
            ji = iblk(5)
            ik = iblk(4)
            IF ( ik-ntms+1==j ) ntms = ntms - 1
            IF ( ntms/=0 ) THEN
               sum = zero
               DO ii = 1 , ntms
                  sum = sum + xl(ji)*V3(ik)
                  ji = ji - 1
                  ik = ik - 1
               ENDDO
               V3(j) = V3(j) + sum
            ENDIF
            CALL endgtb(iblk(1))
         ENDDO
 20      IF ( j==1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = j - 1
         iblk(8) = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ERROR
!
 40      i = Mcblt(4)/10
         WRITE (Io,99002) nrec , j , i , ifb
99002    FORMAT ('0*** TRY TO READ RECORD',I5,'.  J,MCBLT(4),IFB =',I7,2I5)
         CALL mesage(-2,mcbltx,nam)
         spag_nextblock_1 = 3
      CASE (3)
         WRITE (Io,99003) ifb , ii , j
99003    FORMAT ('0*** ERROR.   IFB),II,J =',I5,1H),2I8)
         CALL mesage(-37,0,nam)
         spag_nextblock_1 = 4
      CASE (4)
!
         nam(3) = nam(5)
         IF ( l16/=0 ) CALL conmsg(nam,3,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frsw
