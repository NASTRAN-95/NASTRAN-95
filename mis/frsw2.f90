
SUBROUTINE frsw2(V1,V2,V3,Vb)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dumm(18)
   INTEGER Io , Iz(1) , Ksystm , L16 , Mcblt(7) , Mcbsma(7) , Nzvb
   DOUBLE PRECISION Xl(1)
   COMMON /feerxx/ Dumm , Nzvb
   COMMON /opinv / Mcblt , Mcbsma
   COMMON /system/ Ksystm , Io
   COMMON /zzzzzz/ Iz
!
! Dummy argument declarations
!
   DOUBLE PRECISION V1(1) , V2(1) , V3(1) , Vb(1)
!
! Local variable declarations
!
   INTEGER base , i , ib , iblk(15) , ie , ifb , ii , ik , j , ji , jj , jx , ljj(2) , ll , ll2 , mcbltx , nam(6) , nrec , nrow ,   &
         & ntms , nwds
   DOUBLE PRECISION sum , v3j , xljj , zero
!
! End of declarations
!
!
!     LAST REVISED  11/91, BY G.CHAN/UNISYS
!     ADDITION OF A NEW FORWARD-BACKWARD SUBSTITUTION METHOD, WHICH IS
!     MORE EFFICIENT, AND IS ALREADY GOOD FOR VECTORIZATION.
!
!DB   LOGICAL          DEBUG
   EQUIVALENCE (Xl(1),Iz(1))
   EQUIVALENCE (ljj(1),xljj) , (L16,Dumm(6))
   DATA nam/4HFRSW , 4H2    , 2*4HBEGN , 4HEND  , 4HBGIN/
   DATA zero/0.0D+0/
!DB   DATA     DEBUG , ITER    ,MAX  / .FALSE.  ,0     ,3      /
!
!DB   IF (.NOT.DEBUG) GO TO 20
!     ITER = ITER + 1
!     IF (ITER .GT. MAX) DEBUG = .FALSE.
!     WRITE  (IO,10) NZVB,ITER
!  10 FORMAT ('  .... IN FRSW2.  NZVB =',I8,',   ITER =',I3)
!  20 CONTINUE
   nrow = Mcblt(2)
   CALL frmltd(Mcbsma(1),V1(1),V3(1),Vb(1))
   IF ( Mcblt(7)<0 ) THEN
!
!     NEW METHOD
!
!     THE MCBLT MATRIX HAS BEEN RE-WRITTEN FORWARD FIRST THAN BACKWARD
!     BY UNPSCR IN FEER3. NO STRING OPERATION HERE
!
      IF ( nam(3)==nam(5) ) nam(3) = nam(6)
      IF ( L16/=0 ) CALL conmsg(nam,3,0)
      mcbltx = -Mcblt(7)
      IF ( mod(Mcblt(4),10)/=2 ) THEN
         j = mod(Mcblt(4),10)
         WRITE (Io,99001) j
99001    FORMAT ('0*** MCBLT MATRIX IN WRONG FORM.  UNPSCR FLAG =',I3)
         CALL mesage(-37,0,nam)
      ELSE
         nrec = 0
         CALL rewind(mcbltx)
         CALL fwdrec(*300,mcbltx)
         nwds = Mcblt(5)
!
!     IZ(1)                                                     GINO
!      / V1   V2    V3          VB (OPEN CORE LENGTH = NZVB)   BUFFERS
!     +-----+-----+-----+-----+-------------------------------+--------
!                         OPEN  CORE
!
!     FORWARD SWEEP DIRECTLY ON V3
!
         ll2 = 0
         base = 1
         ifb = +450
         DO j = 1 , nrow
            IF ( base<ll2 ) GOTO 20
            nrec = nrec + 1
!DB   IF (DEBUG) WRITE (IO,210) NREC,IFB
! 210 FORMAT ('  ...READING RECORD',I5,'.   IFB =',I5)
            CALL read(*300,*10,mcbltx,Vb,Nzvb,1,ll)
            CALL mesage(-8,0,nam)
 10         ll2 = ll/nwds
!DB   LL3  = LL2/30
!     LL4  = LL2 - LL3
!     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
! 230 FORMAT (5X,I10,' WORDS READ FROM RECORD',I5,'.   LL2 =',I8)
            base = 1
 20         xljj = Vb(base)
            ii = ljj(1)
            jj = ljj(2)
!DB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
!    1    WRITE (IO,250) J,BASE,II,JJ,IFB
! 250 FORMAT (11X,'J,BASE,II,JJ,IFB =',5I8)
            IF ( ii/=j ) GOTO 400
            ntms = jj - ii + 1
            ib = base + 2
            ie = base + ntms
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
               IF ( base<ll2 ) GOTO 30
               nrec = nrec + 1
!DB   IF (DEBUG) WRITE (IO,210) NREC,IFB
               CALL read(*300,*25,mcbltx,Vb,Nzvb,1,ll)
               CALL mesage(-8,0,nam)
 25            ll2 = ll/nwds
!DB   LL3  = LL2/30
!     LL4  = LL2 - LL3
!     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
               base = 1
 30            xljj = Vb(base)
               ii = ljj(1)
               jj = ljj(2)
!DB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
!    1    WRITE (IO,250) J,BASE,II,JJ,IFB
               IF ( ii/=j ) GOTO 400
               ntms = jj - ii + 1
               ib = base + 2
               ie = base + ntms
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
            ENDDO
         ENDIF
      ENDIF
      GOTO 500
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
            CALL getstr(*40,iblk(1))
            ji = iblk(5)
            ntms = iblk(6)
            ik = iblk(4)
            IF ( ik==j ) THEN
               ntms = ntms - 1
               xljj = Xl(ji)
               ji = ji + 1
               ik = ik + 1
            ENDIF
            IF ( ntms/=0 ) THEN
               v3j = V3(j)
               IF ( v3j/=zero ) THEN
                  DO ii = 1 , ntms
                     V3(ik) = V3(ik) + Xl(ji)*v3j
                     ik = ik + 1
                     ji = ji + 1
                  ENDDO
               ENDIF
            ENDIF
            CALL endget(iblk(1))
         ENDDO
 40      V3(j) = V3(j)/xljj
      ENDDO
!
!     BACKWARD SUBSTITUTION OMIT DIAGONAL
!
      IF ( nrow==1 ) GOTO 500
      j = nrow
      iblk(8) = -1
   ENDIF
 100  DO
      CALL getstb(*200,iblk(1))
      ntms = iblk(6)
      ji = iblk(5)
      ik = iblk(4)
      IF ( ik-ntms+1==j ) ntms = ntms - 1
      IF ( ntms/=0 ) THEN
         sum = zero
         DO ii = 1 , ntms
            sum = sum + Xl(ji)*V3(ik)
            ji = ji - 1
            ik = ik - 1
         ENDDO
         V3(j) = V3(j) + sum
      ENDIF
      CALL endgtb(iblk(1))
   ENDDO
 200  IF ( j==1 ) GOTO 500
   j = j - 1
   iblk(8) = -1
   GOTO 100
!
!     ERROR
!
 300  i = Mcblt(4)/10
   WRITE (Io,99002) nrec , j , i , ifb
99002 FORMAT ('0*** TRY TO READ RECORD',I5,'.  J,MCBLT(4),IFB =',I7,2I5)
   CALL mesage(-2,mcbltx,nam)
 400  WRITE (Io,99003) ifb , ii , j
99003 FORMAT ('0*** ERROR.   IFB),II,J =',I5,1H),2I8)
   CALL mesage(-37,0,nam)
!
 500  nam(3) = nam(5)
   IF ( L16/=0 ) CALL conmsg(nam,3,0)
END SUBROUTINE frsw2
