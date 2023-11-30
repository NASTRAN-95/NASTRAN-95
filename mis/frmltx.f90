
SUBROUTINE frmltx(Ifile,Dz,Dy,Zm)
   IMPLICIT NONE
   REAL Dm18(18)
   INTEGER Ibuf , Incr , Ip , Ityp , Iz(1) , Nout , Np , Nzm
   COMMON /feerxx/ Dm18 , Nzm
   COMMON /system/ Ibuf , Nout
   COMMON /unpakx/ Ityp , Ip , Np , Incr
   COMMON /zzzzzz/ Iz
   DOUBLE PRECISION Dy(1) , Dz(1) , Zm(1)
   INTEGER Ifile(7)
   DOUBLE PRECISION dp , dsum
   INTEGER i , idp(2) , ifl , ii , j , jj , ll , ll2 , n , nam(2) , nrec , ntms , nwds
!
!     LOWER TRIANGULAR TRANSPOSE WITH OFF-DIAGONAL SWITCH
!     DOUBLE PRECISION VERSION
!
!     LAST REVISED  11/91, BY G.CHAN/UNISYS
!     ADDITIONAL OF A NEW METHOD WHICH IS MORE EFFICIENT, AND IS
!     ALREADY GOOD FOR VECTORIZATION
!
   EQUIVALENCE (dp,idp(1))
   DATA nam/4HFRML , 4HTX  /
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
         Dy(i) = 0.0D+0
         Ip = 0
         CALL unpack(*100,ifl,Zm(1))
         IF ( Ip==i ) Zm(1) = -Zm(1)
         dsum = 0.D0
         ii = 0
         DO j = Ip , Np
            ii = ii + 1
            dsum = dsum - Zm(ii)*Dz(j)
         ENDDO
         Dy(i) = dsum
      ENDDO
      GOTO 99999
   ENDIF
!
!     NEW METHOD
!
!     UNLIKE FRMLTD, IFL WAS UNPACKED BACKWARD FIRST, THEN FORWARD BY
!     UNPSCR/FEER3. SO WE SKIP BACKWARD PASS BEFORE READING DATA
!
 100  nrec = Ifile(4)/10
   CALL skprec(ifl,nrec+1)
   nwds = Ifile(5)
   nrec = 0
   ll2 = 0
   ntms = 1
   DO i = 1 , n
      IF ( ntms<ll2 ) GOTO 200
      nrec = nrec + 1
      CALL read(*300,*150,ifl,Zm,Nzm,1,ll)
      CALL mesage(-8,0,nam)
 150  ll2 = ll/nwds
      ntms = 1
 200  dp = Zm(ntms)
      ii = idp(1)
      jj = idp(2)
      IF ( ii/=i ) GOTO 400
      Zm(ntms+1) = -Zm(ntms+1)
      dsum = 0.0D+0
      ll = ntms
      DO j = ii , jj
         ll = ll + 1
         dsum = dsum - Zm(ll)*Dz(j)
      ENDDO
      Dy(i) = dsum
      ntms = ntms + jj - ii + 2
   ENDDO
   GOTO 99999
!
 300  j = Ifile(4)/10
   WRITE (Nout,99001) nrec , i , n , j
99001 FORMAT ('0*** TRY TO READ RECORD',I5,'.  I,N,IFILE(4) =',2I7,I5)
   CALL mesage(-2,ifl,nam)
 400  WRITE (Nout,99002) ii , i
99002 FORMAT ('0*** II AND I MISMATCH =',2I8)
   CALL mesage(-37,0,nam)
!
99999 RETURN
END SUBROUTINE frmltx
