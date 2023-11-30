
SUBROUTINE fbs21(Block,Y,Yn,Nwds)
   IMPLICIT NONE
   INTEGER Dbl , N , Nout
   DOUBLE PRECISION L(1)
   CHARACTER*25 Sfm , Uwm
   REAL Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /fbsx  / Dbl , N
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ L
   INTEGER Nwds
   INTEGER Block(20)
   REAL Y(1) , Yn(1)
   INTEGER begn , buf(3) , end , ii , ij , ik , j , ji , jk , jstr , k , last , nbritm , nbrvec , nstr , nterms , subnam(2)
   DOUBLE PRECISION ljj
   INTEGER locfx
   REAL xlij , xljj , yjk
!
!     FBS2 EXECUTES THE FORWARD/BACKWARD PASS FOR FBS IN RSP
!                                                        ===
!
   DATA subnam , begn , end/4HFBS2 , 4H1    , 4HBEGN , 4HEND /
!
   buf(1) = subnam(1)
   buf(2) = subnam(2)
   buf(3) = begn
   CALL conmsg(buf,3,0)
   nbritm = Nwds/2
   nbrvec = (locfx(Yn)-locfx(Y))/Nwds + 1
   last = 1 + (nbrvec-1)*nbritm
   DO j = 1 , N
!
!     MAKE 1ST STRING CALL FOR COLUMN AND SAVE DIAGONAL ELEMENT
!
      Block(8) = -1
      CALL getstr(*500,Block)
      IF ( Block(4)/=j ) GOTO 500
      jstr = Block(5)
      ljj = L(jstr)
!WKBI
      xljj = ljj
      IF ( Block(6)==1 ) GOTO 100
      nstr = jstr + Block(6) - 1
      jstr = jstr + 1
      Block(4) = Block(4) + 1
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)
!
 50   DO k = 1 , last , nbritm
         yjk = Y(j+k-1)
         ik = Block(4) + k - 1
         DO ij = jstr , nstr
!WKBI
            xlij = L(ij)
!WKBR Y(IK) = Y(IK) + L(IJ)*YJK
            Y(ik) = Y(ik) + xlij*yjk
            ik = ik + 1
         ENDDO
      ENDDO
!
!     GET NEXT STRING IN TRIANGULAR FACTOR
!
 100  CALL endget(Block)
      CALL getstr(*150,Block)
      jstr = Block(5)
      nstr = jstr + Block(6) - 1
      GOTO 50
!
!     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL
!
 150  DO k = 1 , last , nbritm
!WKBR Y(J+K-1) = Y(J+K-1)/LJJ
         Y(j+k-1) = Y(j+k-1)/xljj
      ENDDO
   ENDDO
!
!     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN
!
   IF ( N==1 ) GOTO 400
   CALL bckrec(Block)
   j = N - 1
!
!     GET A STRING IN CURRENT COLUMN. IF STRING INCLUDES DIAGONAL,
!     ADJUST STRING TO SKIP IT.
!
   Block(8) = -1
 200  DO
      CALL getstb(*300,Block)
      IF ( Block(4)-Block(6)+1==j ) Block(6) = Block(6) - 1
      IF ( Block(6)/=0 ) THEN
         nterms = Block(6)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)
!
         DO k = 1 , last , nbritm
            ji = Block(5)
            ik = Block(4) + k - 1
            jk = j + k - 1
            DO ii = 1 , nterms
               Y(jk) = Y(jk) + L(ji)*Y(ik)
               ji = ji - 1
               ik = ik - 1
            ENDDO
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
 300  IF ( j/=1 ) THEN
!
      j = j - 1
      Block(8) = -1
      GOTO 200
   ENDIF
 400  buf(3) = end
   CALL conmsg(buf,3,0)
   RETURN
!
!     FATAL ERROR MESSAGE
!
 500  WRITE (Nout,99001) Sfm , subnam
99001 FORMAT (A25,' 2149, SUBROUTINE ',2A4,/5X,'FIRST ELEMENT OF A COL','UMN OF LOWER TRIANGULAR MATRIX IS NOT THE DIAGONAL ELEMENT'&
            & )
   CALL mesage(-61,0,0)
END SUBROUTINE fbs21