
SUBROUTINE frmlta(Ifile,Z,Y,Zm)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dm18(18)
   INTEGER Ibuf , Incr , Ip , Ityp , Iz(1) , Nout , Np , Nzm
   COMMON /feerxx/ Dm18 , Nzm
   COMMON /system/ Ibuf , Nout
   COMMON /unpakx/ Ityp , Ip , Np , Incr
   COMMON /zzzzzz/ Iz
!
! Dummy argument declarations
!
   INTEGER Ifile(7)
   REAL Y(1) , Z(1) , Zm(1)
!
! Local variable declarations
!
   REAL dp , sum
   INTEGER i , idp , ifl , ii , ilf , j , jj , ll , ll2 , n , nam(2) , nrec , ntms
!
! End of declarations
!
!
!     LOWER TRIANGULAR TRANSPOSE WITH OFF-DIAGONAL SWITCH
!     SINGLE PRECISION VERSION
!
!     LAST REVISED  11/91, BY G.CHAN/UNISYS
!     ADDITIONAL OF A NEW METHODS WHICH IS MORE EFFICIENT, AND IS
!     ALREADY GOOD FOR VECTORIZATION
!
   EQUIVALENCE (dp,idp)
   DATA nam/4HFRML , 4HTA  /
!
   n = Ifile(2)
   ifl = Ifile(1)
   IF ( Ifile(7)<0 ) ifl = -Ifile(7)
   CALL rewind(ifl)
   IF ( Ifile(7)>=0 ) THEN
      CALL skprec(ifl,1)
      Ityp = Ifile(5)
!
!     NASTRAN ORIGINAL METHOD
!
      Incr = 1
      DO i = 1 , n
         Y(i) = 0.0
         Ip = 0
         CALL unpack(*100,ifl,Zm(1))
         IF ( Ip==i ) Zm(1) = -Zm(1)
         sum = 0.0
         ii = 0
         DO j = Ip , Np
            ii = ii + 1
            sum = sum - Zm(ii)*Z(j)
         ENDDO
         Y(i) = sum
      ENDDO
      GOTO 99999
   ENDIF
!
!     NEW METHOD
!
!     UNLIKE FRMLT, IFL WAS UNPACKED BACKWARD FIRST, THEN FORWARD BY
!     UNPSCR/FEER3. SO WE SKIP BACKWARD PASS BEFORE READING DATA
!
 100  nrec = Ifile(4)/10
   CALL skprec(ifl,nrec+1)
   nrec = 0
   ll2 = 0
   ntms = 1
   DO i = 1 , n
      IF ( ntms<ll2 ) GOTO 200
      nrec = nrec + 1
      CALL read(*300,*150,ifl,Zm,Nzm,1,ll)
      CALL mesage(-8,0,nam)
 150  ll2 = ll
      ntms = 1
 200  dp = Zm(ntms)
      ii = idp
      IF ( ii/=i ) GOTO 400
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
   ENDDO
   GOTO 99999
!
 300  j = Ifile(4)/10
   WRITE (Nout,99001) nrec , i , n , j
99001 FORMAT ('0*** TRY TO READ RECORD',I5,'.  I,N,IFILE(4) =',2I7,I5)
   CALL mesage(-2,ilf,nam)
 400  WRITE (Nout,99002) ii , i
99002 FORMAT ('0*** II AND I MISMATCH =',2I8)
   CALL mesage(-37,0,nam)
!
99999 RETURN
END SUBROUTINE frmlta
