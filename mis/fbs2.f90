
SUBROUTINE fbs2(Block,Y,Yn,Nwds)
   IMPLICIT NONE
   INTEGER Dbl , Mach , N , Nout
   DOUBLE PRECISION L(1)
   CHARACTER*25 Sfm , Uwm
   REAL Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /fbsx  / Dbl , N
   COMMON /machin/ Mach
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ L
   INTEGER Nwds
   INTEGER Block(8)
   DOUBLE PRECISION Y(1) , Yn(1)
   INTEGER begn , buf(2) , end , ii , ij , ik , j , j1 , ji , jstr , k , last , nbritm , nstr , nterms , subnam
   DOUBLE PRECISION ljj , sum , yjk , zero
   INTEGER locfx
!
!     FBS2 EXECUTES THE FORWARD/BACKWARD PASS FOR FBSF IN RDP
!
   DATA zero/0.0D+0/
   DATA subnam , begn , end/4HFBS2 , 4HBEGN , 4HEND /
!
   buf(1) = subnam
   buf(2) = begn
   CALL conmsg(buf,2,0)
   nbritm = Nwds/2
   j = (locfx(Yn)-locfx(Y)+1)/Nwds
   last = max0(j,1)*nbritm
   DO j = 1 , N
      j1 = j - 1
      DO k = j , last , nbritm
         IF ( Y(k)/=zero ) GOTO 50
      ENDDO
      CALL skprec(Block(1),1)
      CYCLE
!
!     MAKE 1ST STRING CALL FOR COLUMN AND SAVE DIAGONAL ELEMENT
!
 50   Block(8) = -1
      CALL getstr(*600,Block)
      IF ( Block(4)/=j ) GOTO 600
      jstr = Block(5)
      ljj = 1.0D+0/L(jstr)
      IF ( Block(6)==1 ) GOTO 150
      nstr = jstr + Block(6) - 1
      jstr = jstr + 1
      Block(4) = Block(4) + 1
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)
!
 100  DO k = 1 , last , nbritm
         yjk = Y(j1+k)
         IF ( yjk/=zero ) THEN
            ik = Block(4) + k - 1
            DO ij = jstr , nstr
               Y(ik) = Y(ik) + L(ij)*yjk
               ik = ik + 1
            ENDDO
         ENDIF
      ENDDO
!
!     GET NEXT STRING IN TRIANGULAR FACTOR
!
 150  CALL endget(Block)
      CALL getstr(*200,Block)
      jstr = Block(5)
      nstr = jstr + Block(6) - 1
      GOTO 100
!
!     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL
!
 200  DO k = j , last , nbritm
         Y(k) = Y(k)*ljj
      ENDDO
!
   ENDDO
!
!     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN
!
   IF ( N==1 ) GOTO 500
   CALL bckrec(Block)
   j = N - 1
!
!     GET A STRING IN CURRENT COLUMN. IF THIS STRING INCLUDES DIAGONAL,
!     ADJUST STRING TO SKIP IT.
!
 300  j1 = j - 1
   Block(8) = -1
   DO
      CALL getstb(*400,Block)
      IF ( Block(4)-Block(6)==j1 ) Block(6) = Block(6) - 1
      IF ( Block(6)/=0 ) THEN
         nterms = Block(6)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)
!
         DO k = 1 , last , nbritm
            ji = Block(5) + 1
            ik = Block(4) + k
            sum = 0.0D+0
            DO ii = 1 , nterms
               ji = ji - 1
               ik = ik - 1
               sum = sum + L(ji)*Y(ik)
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
 400  IF ( j/=1 ) THEN
      j = j - 1
      GOTO 300
   ENDIF
!
 500  buf(2) = end
   CALL conmsg(buf,2,0)
   RETURN
!
!
!     FATAL ERROR MESSAGE
!
 600  WRITE (Nout,99001) Sfm , subnam
99001 FORMAT (A25,' 2149, SUBROUTINE ',A4,/5X,'FIRST ELEMENT OF A COLU','MN OF LOWER TRIANGULAR MATRIX IS NOT THE DIAGONAL ELEMENT')
   CALL mesage(-61,0,0)
END SUBROUTINE fbs2