
SUBROUTINE fbsi4(Block,Y,Mem,Dmem,Ibuff)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dbb(7) , Dbc(7) , Dbl(7) , Dbu(7) , Ipos(7) , Lasind , Nout , Nvec , Nvecsz , Nwds , Rd , Rdrew , Rew , Wrt , Wrtrew
   DOUBLE PRECISION L(2)
   CHARACTER*25 Sfm , Uwm
   REAL Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /fbsm  / Nvec , Nvecsz , Nwds , Lasind , Ipos
   COMMON /fbsx  / Dbl , Dbu , Dbb , Dbc
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ L
!
! Dummy argument declarations
!
   INTEGER Block(8) , Ibuff(2) , Mem(2)
   DOUBLE PRECISION Dmem(2)
   DOUBLE COMPLEX Y(1)
!
! Local variable declarations
!
   INTEGER begn , buf(2) , end , ifcol , ii , ij , ik , indxi , indxl , iopen , irow , iyrow , j , j1 , ji , k , kcol , last ,      &
         & lcol , ncol , nidlt , nrows , subnam
   DOUBLE COMPLEX ljj , sum , yjk , zero
!
! End of declarations
!
!
!     FBSI4 EXECUTES THE FORWARD/BACKWARD PASS FOR FBSI IN CDP
!
   DATA zero/(0.0D+0,0.0D+0)/
   DATA subnam , begn , end/4HFBS4 , 4HBEGN , 4HEND /
!
   ncol = Dbl(2)
   buf(1) = subnam
   buf(2) = begn
   iopen = 0
   CALL conmsg(buf,2,0)
   last = Nvec*Nvecsz
   nidlt = 1
   lcol = Ipos(1)
   DO j = 1 , lcol
!      PRINT *,' FORWARD, PROCESSING COLUMN J=',J
      j1 = j - 1
!
! CHECK IF THIS ROW VALUE IS ZERO FOR ALL RIGHT HAND VECTORS
!
      DO k = j , last , Nvecsz
         IF ( Y(k)/=zero ) GOTO 50
      ENDDO
!
! ALL VALUES FOR THIS ROW ARE ZERO, SKIP TO NEXT ROW OF RIGHT HAND VECTORS
!
      IF ( nidlt>=Lasind ) EXIT
      kcol = Mem(nidlt)
      IF ( kcol/=j ) GOTO 1000
      DO
         nrows = Mem(nidlt+1)
         nidlt = nidlt + nrows*Nwds + 4
         IF ( nidlt>=Lasind ) GOTO 300
         kcol = Mem(nidlt)
         IF ( kcol/=j ) GOTO 200
      ENDDO
!
!     GET 1ST STRING FOR COLUMN AND SAVE DIAGONAL ELEMENT
!
 50   kcol = Mem(nidlt)
      IF ( kcol/=j ) GOTO 1000
      nrows = Mem(nidlt+1)
      irow = Mem(nidlt+nrows*Nwds+2)
      indxi = (nidlt+3)/2
      indxl = indxi + nrows*2 - 1
      ljj = 1.0D+0/dcmplx(Dmem(indxi),Dmem(indxi+1))
      IF ( nrows==1 ) GOTO 150
      indxi = indxi + 2
      irow = irow + 1
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)
!
 100  DO k = 1 , last , Nvecsz
         yjk = Y(j1+k)
         IF ( yjk/=zero ) THEN
            iyrow = irow + k - 1
            DO ij = indxi , indxl , 2
               Y(iyrow) = Y(iyrow) + dcmplx(Dmem(ij),Dmem(ij+1))*yjk
               iyrow = iyrow + 1
            ENDDO
         ENDIF
      ENDDO
!
!     GET NEXT STRING IN TRIANGULAR FACTOR
!
 150  nidlt = nidlt + 4 + nrows*Nwds
      IF ( nidlt<Lasind ) THEN
         kcol = Mem(nidlt)
         IF ( kcol==j ) THEN
            nrows = Mem(nidlt+1)
            irow = Mem(nidlt+nrows*Nwds+2)
            indxi = (nidlt+3)/2
            indxl = indxi + nrows*2 - 1
            GOTO 100
         ENDIF
      ENDIF
!
!     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL
!
      DO k = j , last , Nvecsz
         Y(k) = Y(k)*ljj
      ENDDO
!
 200  ENDDO
 300  IF ( lcol/=ncol ) THEN
      ifcol = lcol + 1
      CALL gopen(Dbl,Ibuff,Rdrew)
!
! POSITION FILE TO APPROPRIATE COLUMN TO BE READ
!
      CALL dsspos(Dbl,Ipos(2),Ipos(3),Ipos(4))
      DO j = ifcol , ncol
         j1 = j - 1
!
! CHECK IF THIS ROW VALUE IS ZERO FOR ALL RIGHT HAND VECTORS
!
         DO k = j , last , Nvecsz
            IF ( Y(k)/=zero ) GOTO 320
         ENDDO
!
! ALL VALUES FOR THIS ROW ARE ZERO, SKIP TO NEXT ROW OF RIGHT HAND VECTORS
!
         CALL skprec(Dbl,1)
         CYCLE
!
!     GET 1ST STRING FOR COLUMN AND SAVE DIAGONAL ELEMENT
!
 320     Block(8) = -1
         CALL getstr(*1000,Block)
         IF ( Block(4)/=j ) GOTO 1000
         irow = Block(4)
         indxi = Block(5)
         nrows = Block(6)
         indxl = indxi + nrows*2 - 1
         ljj = 1.0D+0/dcmplx(L(indxi),L(indxi+1))
         IF ( nrows==1 ) GOTO 360
         indxi = indxi + 2
         irow = irow + 1
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)
!
 340     DO k = 1 , last , Nvecsz
            yjk = Y(j1+k)
            IF ( yjk/=zero ) THEN
               iyrow = irow + k - 1
               DO ij = indxi , indxl , 2
                  Y(iyrow) = Y(iyrow) + dcmplx(L(ij),L(ij+1))*yjk
                  iyrow = iyrow + 1
               ENDDO
            ENDIF
         ENDDO
!
!     GET NEXT STRING IN TRIANGULAR FACTOR
!
 360     CALL endget(Block)
         CALL getstr(*380,Block)
         irow = Block(4)
         indxi = Block(5)
         nrows = Block(6)
         indxl = indxi + nrows*2 - 1
         GOTO 340
!
!     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL
!
 380     DO k = j , last , Nvecsz
            Y(k) = Y(k)*ljj
         ENDDO
      ENDDO
   ENDIF
   IF ( ncol==1 ) GOTO 900
   j = ncol - 1
   IF ( lcol==ncol ) THEN
      DO
!
!
!     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN
!
         nidlt = nidlt - 1
         nrows = Mem(nidlt)
         nidlt = nidlt - nrows*Nwds - 3
         kcol = Mem(nidlt)
         IF ( kcol/=ncol ) THEN
            nidlt = nidlt + nrows*Nwds + 4
!
!     GET A STRING IN CURRENT COLUMN. IF THIS STRING INCLUDES DIAGONAL,
!     ADJUST STRING TO SKIP IT.
!
            j1 = j - 1
            GOTO 600
         ENDIF
      ENDDO
   ELSE
!
!     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN
!
      CALL bckrec(Block)
   ENDIF
!
!     GET A STRING IN CURRENT COLUMN. IF THIS STRING INCLUDES DIAGONAL,
!     ADJUST STRING TO SKIP IT.
!
 400  j1 = j - 1
   Block(8) = -1
   DO
      CALL getstb(*500,Block)
      irow = Block(4)
      nrows = Block(6)
      IF ( irow-nrows==j1 ) nrows = nrows - 1
      IF ( nrows/=0 ) THEN
         indxi = Block(5)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)
!
         DO k = 1 , last , Nvecsz
            ji = indxi + 2
            ik = irow + k
            sum = (0.0D+0,0.0D+0)
            DO ii = 1 , nrows
               ji = ji - 2
               ik = ik - 1
               sum = sum + dcmplx(L(ji),L(ji+1))*Y(ik)
            ENDDO
            Y(j1+k) = Y(j1+k) + sum
         ENDDO
      ENDIF
!
!     TERMINATE CURRENT STRING AND GET NEXT STRING
!
      CALL endgtb(Block)
   ENDDO
!
!     END-OF-COLUMN -- TEST FOR COMPLETION
!
 500  IF ( j==1 ) GOTO 900
   j = j - 1
   IF ( j/=lcol ) GOTO 400
   j1 = j - 1
!      print *,' processing column in backward step, j=',j
 600  nidlt = nidlt - 1
   IF ( nidlt<=1 ) GOTO 800
   nrows = Mem(nidlt)
   irow = Mem(nidlt-1)
   nidlt = nidlt - nrows*Nwds - 3
   kcol = Mem(nidlt)
 700  IF ( kcol==j ) THEN
      indxi = nidlt/2 + nrows*2
      irow = irow + nrows - 1
      IF ( (irow-nrows)==j1 ) nrows = nrows - 1
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)
!
      DO k = 1 , last , Nvecsz
         ji = indxi + 2
         ik = irow + k
         sum = 0.0D+0
         DO ii = 1 , nrows
            ji = ji - 2
            ik = ik - 1
            sum = sum + dcmplx(Dmem(ji),Dmem(ji+1))*Y(ik)
         ENDDO
         Y(j1+k) = Y(j1+k) + sum
      ENDDO
!
!     TERMINATE CURRENT STRING AND GET NEXT STRING
!
      GOTO 600
   ENDIF
!
!     END-OF-COLUMN -- TEST FOR COMPLETION
!
 800  IF ( j/=1 ) THEN
      j = j - 1
      j1 = j - 1
      GOTO 700
   ENDIF
!
 900  buf(2) = end
   CALL conmsg(buf,2,0)
   CALL close(Dbl,Rew)
   RETURN
!
!     FATAL ERROR MESSAGE
!
 1000 WRITE (Nout,99001) Sfm , subnam
99001 FORMAT (A25,' 2149, SUBROUTINE ',A4,/5X,'FIRST ELEMENT OF A COLU','MN OF LOWER TRIANGULAR MATRIX IS NOT THE DIAGONAL ELEMENT')
   CALL mesage(-61,0,0)
END SUBROUTINE fbsi4
