!*==frbk.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frbk(V1,V2,V3,Vb)
   USE c_feerxx
   USE c_opinv
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   INTEGER :: base , i , ib , ie , ifb , ii , ik , j , ji , jj , l16 , ljj , ll , ll2 , mcbltx , nam , nrec , nrow , ntms
   INTEGER , DIMENSION(6) , SAVE :: buf
   INTEGER , DIMENSION(15) :: iblk
   REAL :: sum , v3j , xljj
   REAL , DIMENSION(1) :: xl
   REAL , SAVE :: zero
   EXTERNAL conmsg , endget , endgtb , frmlt , fwdrec , getstb , getstr , mesage , read , rewind , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     LAST REVISED BY G.CHAN/UNISYS  11/1991
!     . ELIMINATE UN-NECCESSARY REWIND AND SKIP AFTER FIRST CALL TO THIS
!       ROUTINE (NASTRAN ORIGINAL METHOD)
!     . ADDITION OF A NEW BACKWARD-FORWARD SUBSTITUTION METHOD WHICH IS
!       MORE EFFICIENT, AND IS ALREADY GOOD FOR VECTORIZATION
!
!DB   LOGICAL          DEBUG
   !>>>>EQUIVALENCE (Xl(1),Iz(1))
   !>>>>EQUIVALENCE (ljj,xljj) , (L16,Dumm(6))
   DATA buf/4HFRBK , 4H     , 2*4HBEGN , 4HEND  , 4HBGIN/
   DATA zero/0.0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!DB   DATA     DEBUG , ITER    ,MAX /  .FALSE.   ,0      ,3      /
!
!DB   IF (.NOT.DEBUG) GO TO 20
!     ITER = ITER + 1
!     IF (ITER .GT. MAX) DEBUG = .FALSE.
!     WRITE  (IO,10) NZVB,ITER
!  10 FORMAT ('  .... IN FRBK.  NZVB =',I8,',  ITER =',I3)
!  20 CONTINUE
         nrow = mcblt(2)
         DO i = 1 , nrow
            V2(i) = V1(i)
         ENDDO
!
!     SELECTION OF ORIGINAL OR NEW FBS METHOD
!
         j = nrow
         IF ( mcblt(7)<0 ) THEN
!
!     NEW METHOD
!
!     MATRIX MCBLT HAS BEEN RE-WRITTEN TO MCBLTX BY UNPSCR/FEER3. NO
!     STRING OPERATIONS HERE.
!
            IF ( buf(3)==buf(5) ) buf(3) = buf(6)
            IF ( l16/=0 ) CALL conmsg(buf,3,0)
            mcbltx = -mcblt(7)
            IF ( mod(mcblt(4),10)/=3 ) THEN
               j = mod(mcblt(4),10)
               WRITE (io,99001) j
99001          FORMAT ('0*** MCBLT MATRIX IN WRONG FORM.  UNPSCR FLAG =',I3)
               CALL mesage(-37,0,buf(1))
            ELSE
               nrec = 0
               CALL rewind(mcbltx)
               CALL fwdrec(*40,mcbltx)
!     NWDS = MCBLT(5)
!
!     IZ(1)                                                      GINO
!      / V1   V2    V3           VB (OPEN CORE LENGTH = NZVB)   BUFFERS
!     +-----+-----+-----+-----+-------------------------------+---------
!                          OPEN  CORE
!
!     BACKWARD SUBSTITUTION
!
               ll2 = 0
               base = 1
               ifb = -350
               DO ik = 1 , nrow
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
                        CALL read(*40,*2,mcbltx,Vb,nzvb,1,ll)
                        CALL mesage(-8,0,nam)
! 220 LL2  = LL/NWDS
 2                      ll2 = ll
!DB   LL3  = LL2/30
!     LL4  = LL2 - LL3
!     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
! 230 FORMAT (1X,I10,' WORDS READ FROM RECORD NO.',I5,'.   LL2 =',I10)
                        base = 1
                        spag_nextblock_2 = 2
                     CASE (2)
                        xljj = Vb(base)
                        ii = ljj
                        xljj = Vb(base+1)
                        jj = ljj
!DB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
!    1    WRITE (IO,250) J,BASE,II,JJ,IFB
! 250 FORMAT (11X,'J,BASE,II,JJ,IFB =',5I8)
                        IF ( ii/=j ) THEN
                           spag_nextblock_1 = 4
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
                              sum = sum + Vb(i)*V2(ii)
                           ENDDO
                           V2(j) = V2(j) + sum
                        ENDIF
                        V2(j) = V2(j)/Vb(ib-1)
                        j = j - 1
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               CALL frmlt(mcbsma(1),V2(1),V3(1),Vb(1))
!
!     FORWARD SWEEP DIRECTLY ON V3
!
               IF ( nrow/=1 ) THEN
                  nrec = 0
                  ll2 = 0
                  base = 1
                  ifb = +390
                  DO j = 1 , nrow
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
                           CALL read(*40,*4,mcbltx,Vb,nzvb,1,ll)
                           CALL mesage(-8,0,nam)
! 290 LL2  = LL/NWDS
 4                         ll2 = ll
!DB   LL3  = LL2/30
!     LL4  = LL2 - LL3
!     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
                           base = 1
                           spag_nextblock_3 = 2
                        CASE (2)
                           xljj = Vb(base)
                           ii = ljj
                           xljj = Vb(base+1)
                           jj = ljj
!DB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
!    1    WRITE (IO,250) J,BASE,II,JJ,IFB
                           IF ( ii/=j ) THEN
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           ntms = jj - ii + 1
                           V3(j) = V3(j)/Vb(base+2)
                           IF ( ntms>1 ) THEN
                              v3j = V3(j)
                              IF ( v3j/=zero ) THEN
                                 ib = base + 3
                                 ie = base + 1 + ntms
                                 DO i = ib , ie
                                    ii = ii + 1
                                    V3(ii) = V3(ii) + Vb(i)*v3j
                                 ENDDO
                              ENDIF
                           ENDIF
                           base = base + ntms + 2
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
                  ENDDO
               ENDIF
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     NASTRAN ORIGIANL METHOD
!
            iblk(1) = mcblt(1)
            iblk(9) = 1
            iblk(10) = 1
!
!     BACKWARD SUBSTITUTION
!
            IF ( buf(3)==buf(5) ) THEN
!
!     ALREADY AT END, NO SKIP NEEDED
!
               buf(3) = buf(6)
               IF ( l16/=0 ) CALL conmsg(buf,3,0)
            ELSE
               buf(3) = buf(4)
               IF ( l16/=0 ) CALL conmsg(buf,3,0)
!
!     REWIND AND SKIP TO COLUMN N
!
               CALL rewind(mcblt)
               CALL skprec(mcblt,nrow+1)
            ENDIF
!
            iblk(8) = -1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL getstb(*20,iblk(1))
         ntms = iblk(6)
         ji = iblk(5)
         ik = iblk(4)
         IF ( ik-ntms+1==j ) THEN
            ntms = ntms - 1
            xljj = xl(ji-ntms)
            IF ( ntms==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         sum = zero
         DO ii = 1 , ntms
            sum = sum + xl(ji)*V2(ik)
            ji = ji - 1
            ik = ik - 1
         ENDDO
         V2(j) = V2(j) + sum
         spag_nextblock_1 = 3
      CASE (3)
         CALL endgtb(iblk(1))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      V2(j) = V2(j)/xljj
         IF ( j==1 ) THEN
            CALL frmlt(mcbsma(1),V2(1),V3(1),Vb(1))
!
!     FORWARD SWEEP DIRECTLY ON V3
!
            DO j = 1 , nrow
               iblk(8) = -1
               DO
                  CALL getstr(*30,iblk(1))
                  ji = iblk(5)
                  ntms = iblk(6)
                  ik = iblk(4)
                  IF ( ik==j ) THEN
                     ntms = ntms - 1
                     V3(j) = V3(j)/xl(ji)
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
 30         ENDDO
            spag_nextblock_1 = 5
         ELSE
            j = j - 1
            iblk(8) = -1
            spag_nextblock_1 = 2
         ENDIF
         CYCLE
!
 40      i = mcblt(4)/10
         WRITE (io,99002) nrec , j , i , ifb
99002    FORMAT ('0*** TRY TO READ RECORD',I5,'.  J,MCBLT(4),IFB =',I7,2I5)
         CALL mesage(-3,mcbltx,buf(1))
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (io,99003) j , ii , ifb
99003    FORMAT ('0*** ROW MISMATCH.  J,II,(IFB =',I7,I12,3H  (,I4)
         CALL mesage(-37,0,buf(1))
         spag_nextblock_1 = 5
      CASE (5)
!
         buf(3) = buf(5)
         IF ( l16/=0 ) CALL conmsg(buf,3,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frbk
