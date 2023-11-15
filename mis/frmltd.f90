
SUBROUTINE frmltd(Ifile,Dz,Dy,Zm)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum18(18)
   INTEGER Ibuf , Incr , Ip , Ityp , Iz(1) , Nout , Np , Nzm
   COMMON /feerxx/ Dum18 , Nzm
   COMMON /system/ Ibuf , Nout
   COMMON /unpakx/ Ityp , Ip , Np , Incr
   COMMON /zzzzzz/ Iz
!
! Dummy argument declarations
!
   DOUBLE PRECISION Dy(1) , Dz(1) , Zm(1)
   INTEGER Ifile(7)
!
! Local variable declarations
!
   DOUBLE PRECISION dp , dsum
   INTEGER i , idp(2) , ifl , ii , j , jj , ll , ll2 , n , nam(2) , next , nrec , nwds
!
! End of declarations
!
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
   EQUIVALENCE (dp,idp(1))
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
         IF ( next<ll2 ) GOTO 40
         nrec = nrec + 1
!DB   IF (DEBUG) WRITE (NOUT,70) NREC,I
!  70 FORMAT ('  ...READING RECORD',I5,'.  I =',I7)
         CALL read(*100,*20,ifl,Zm,Nzm,1,ll)
         CALL mesage(-8,0,nam)
 20      ll2 = ll/nwds
!DB   IF (DEBUG) WRITE (NOUT,90) LL,NREC,LL2
!  90 FORMAT (1X,I10,' WORDS READ FROM RECORD NO.',I5,'   LL2 =',I10)
         next = 1
 40      dp = Zm(next)
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
      ENDDO
   ELSE
      Ityp = Ifile(5)
!
!     NASTRAN ORIGIANL METHOD
!
      Incr = 1
      DO i = 1 , n
         Dy(i) = 0.0D+0
         Ip = 0
         CALL unpack(*50,ifl,Zm(1))
         dsum = 0.0D+0
         ii = 0
         DO j = Ip , Np
            ii = ii + 1
            dsum = dsum + Zm(ii)*Dz(j)
         ENDDO
         Dy(i) = dsum
 50   ENDDO
   ENDIF
   GOTO 99999
!
 100  j = Ifile(4)/10
   WRITE (Nout,99001) nrec , i , n , j
99001 FORMAT ('0*** TRY TO READ RECORD',I5,'.   I,N,IFILE(4) =',2I7,I5)
   CALL mesage(-2,ifl,nam)
!
99999 END SUBROUTINE frmltd
