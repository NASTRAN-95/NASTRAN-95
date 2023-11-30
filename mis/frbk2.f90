
SUBROUTINE frbk2(V1,V2,V3,Vb)
   IMPLICIT NONE
   REAL Dumm(18)
   INTEGER Io , Iz(1) , Ksystm , L16 , Mcblt(7) , Mcbsma(7) , Nzvb
   DOUBLE PRECISION Xl(1)
   COMMON /feerxx/ Dumm , Nzvb
   COMMON /opinv / Mcblt , Mcbsma
   COMMON /system/ Ksystm , Io
   COMMON /zzzzzz/ Iz
   DOUBLE PRECISION V1(1) , V2(1) , V3(1) , Vb(1)
   INTEGER base , buf(6) , i , ib , iblk(15) , ie , ifb , ii , ik , j , ji , jj , ljj(2) , ll , ll2 , mcbltx , nam , nrec , nrow ,  &
         & ntms , nwds
   DOUBLE PRECISION sum , v3j , xljj , zero
!
!     LAST REVISED BY G.CHAN/UNISYS  11/1991
!     . ELIMINATE UN-NECCESSARY REWIND AND SKIP AFTER FIRST CALL TO THIS
!       ROUTINE (NASTRAN ORIGINAL METHOD)
!     . ADDITION OF A NEW BACKWARD-FORWARD SUBSTITUTION METHOD WHICH IS
!       MORE EFFICIENT, AND IS ALREADY GOOD FOR VECTORIZATION
!
!DB   LOGICAL          DEBUG
   EQUIVALENCE (Xl(1),Iz(1))
   EQUIVALENCE (ljj(1),xljj) , (L16,Dumm(6))
   DATA buf/4HFRBK , 4H2    , 2*4HBEGN , 4HEND  , 4HBGIN/
   DATA zero/0.0D+0/
!DB   DATA     DEBUG , ITER    ,MAX /  .FALSE.   ,0      ,3      /
!
!DB   IF (.NOT.DEBUG) GO TO 20
!     ITER = ITER + 1
!     IF (ITER .GT. MAX) DEBUG = .FALSE.
!     WRITE  (IO,10) NZVB,ITER
!  10 FORMAT ('  .... IN FRBK2.  NZVB =',I8,',  ITER =',I3)
!  20 CONTINUE
   nrow = Mcblt(2)
   DO i = 1 , nrow
      V2(i) = V1(i)
   ENDDO
!
!     SELECTION OF ORIGINAL OR NEW FBS METHOD
!
   j = nrow
   IF ( Mcblt(7)<0 ) THEN
!
!     NEW METHOD
!
!     MATRIX MCBLT HAS BEEN RE-WRITTEN TO MCBLTX BY UNPSCR/FEER3. NO
!     STRING OPERATIONS HERE.
!
      IF ( buf(3)==buf(5) ) buf(3) = buf(6)
      IF ( L16/=0 ) CALL conmsg(buf,3,0)
      mcbltx = -Mcblt(7)
      IF ( mod(Mcblt(4),10)/=3 ) THEN
         j = mod(Mcblt(4),10)
         WRITE (Io,99001) j
99001    FORMAT ('0*** MCBLT MATRIX IN WRONG FORM.   UNPSCR FLAG =',2I3)
         CALL mesage(-37,0,buf(1))
      ELSE
         nrec = 0
         CALL rewind(mcbltx)
         CALL fwdrec(*400,mcbltx)
         nwds = Mcblt(5)
!
!     IZ(1)                                                      GINO
!      / V1   V2    V3           VB (OPEN CORE LENGTH = NZVB)   BUFFERS
!     +-----+-----+-----+-----+-------------------------------+---------
!                          OPEN  CORE
!
!
!     BACKWARD SUBSTITUTION
!
!
         ll2 = 0
         base = 1
         ifb = -350
         DO ik = 1 , nrow
            IF ( base<ll2 ) GOTO 20
            nrec = nrec + 1
!DB   IF (DEBUG) WRITE (IO,210) NREC,IFB
! 210 FORMAT ('  ...READING RECORD',I5,'.   IFB =',I5)
            CALL read(*400,*10,mcbltx,Vb,Nzvb,1,ll)
            CALL mesage(-8,0,nam)
 10         ll2 = ll/nwds
!DB   LL3  = LL2/30
!     LL4  = LL2 - LL3
!     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
! 230 FORMAT (1X,I10,' WORDS READ FROM RECORD NO.',I5,'.   LL2 =',I10)
            base = 1
 20         xljj = Vb(base)
            ii = ljj(1)
            jj = ljj(2)
!DB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
!    1    WRITE (IO,250) J,BASE,II,JJ,IFB
! 250 FORMAT (11X,'J,BASE,II,JJ,IFB =',5I8)
            IF ( ii/=j ) GOTO 500
            ntms = jj - ii + 1
            ib = base + 2
            ie = base + ntms
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
         ENDDO
         CALL frmltd(Mcbsma(1),V2(1),V3(1),Vb(1))
!
!     FORWARD SWEEP DIRECTLY ON V3
!
         IF ( nrow/=1 ) THEN
            nrec = 0
            ll2 = 0
            base = 1
            ifb = +390
            DO j = 1 , nrow
               IF ( base<ll2 ) GOTO 30
               nrec = nrec + 1
!DB   IF (DEBUG) WRITE (IO,210) NREC,IFB
               CALL read(*400,*25,mcbltx,Vb,Nzvb,1,ll)
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
               IF ( ii/=j ) GOTO 500
               ntms = jj - ii + 1
               V3(j) = V3(j)/Vb(base+1)
               IF ( ntms>1 ) THEN
                  v3j = V3(j)
                  IF ( v3j/=zero ) THEN
                     ib = base + 2
                     ie = base + ntms
                     DO i = ib , ie
                        ii = ii + 1
                        V3(ii) = V3(ii) + Vb(i)*v3j
                     ENDDO
                  ENDIF
               ENDIF
               base = base + ntms + 1
            ENDDO
         ENDIF
      ENDIF
      GOTO 600
   ELSE
!
!     NASTRAN ORIGIANL METHOD
!
      iblk(1) = Mcblt(1)
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
         IF ( L16/=0 ) CALL conmsg(buf,3,0)
      ELSE
!     BUF(3) = BUF(4)
         IF ( L16/=0 ) CALL conmsg(buf,3,0)
!
!     REWIND AND SKIP TO COLUMN N
!
         CALL rewind(Mcblt)
         CALL skprec(Mcblt,nrow+1)
      ENDIF
!
      iblk(8) = -1
   ENDIF
 100  CALL getstb(*300,iblk(1))
   ntms = iblk(6)
   ji = iblk(5)
   ik = iblk(4)
   IF ( ik-ntms+1==j ) THEN
      ntms = ntms - 1
      xljj = Xl(ji-ntms)
      IF ( ntms==0 ) GOTO 200
   ENDIF
   sum = zero
   DO ii = 1 , ntms
      sum = sum + Xl(ji)*V2(ik)
      ji = ji - 1
      ik = ik - 1
   ENDDO
   V2(j) = V2(j) + sum
 200  CALL endgtb(iblk(1))
   GOTO 100
 300  V2(j) = V2(j)/xljj
   IF ( j==1 ) THEN
      CALL frmltd(Mcbsma(1),V2(1),V3(1),Vb(1))
!
!     FORWARD SWEEP DIRECTLY ON V3
!
      DO j = 1 , nrow
         iblk(8) = -1
         DO
            CALL getstr(*350,iblk(1))
            ji = iblk(5)
            ntms = iblk(6)
            ik = iblk(4)
            IF ( ik==j ) THEN
               ntms = ntms - 1
               V3(j) = V3(j)/Xl(ji)
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
 350  ENDDO
      GOTO 600
   ELSE
      j = j - 1
      iblk(8) = -1
      GOTO 100
   ENDIF
!
 400  i = Mcblt(4)/10
   WRITE (Io,99002) nrec , j , i , ifb
99002 FORMAT ('0*** TRY TO READ RECORD',I5,'.  J,MCBLT(4),IFB =',I7,2I5)
   CALL mesage(-3,mcbltx,nam)
 500  WRITE (Io,99003) j , ii , ifb
99003 FORMAT ('0*** ROW MISMATCH.  J,II,(IFB =',I7,I12,3H  (,I4)
   CALL mesage(-37,0,buf(1))
!
 600  buf(3) = buf(5)
   IF ( L16/=0 ) CALL conmsg(buf,3,0)
END SUBROUTINE frbk2
