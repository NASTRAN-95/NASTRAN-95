!*==fbs4.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbs4(Block,Y,Yn,Nwds)
   USE c_fbsx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(8) :: Block
   REAL(REAL64) , DIMENSION(1) :: Y
   REAL(REAL64) , DIMENSION(1) :: Yn
   INTEGER :: Nwds
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: begn , end , subnam
   INTEGER , DIMENSION(2) :: buf
   INTEGER :: ii , ij , ik , j , j1 , ji , jk , jstr , k , last , nbritm , nstr , nterms
   REAL(REAL64) :: ljji , ljjr , ssqr , sum1 , sum2 , yjki , yjkr
   REAL(REAL64) , SAVE :: zero
   EXTERNAL bckrec , conmsg , endget , endgtb , getstb , getstr , locfx , mesage , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     FBS4 EXECUTES THE FORWARD/BACKWARD PASS FOR FBSF IN CDP
!
   !>>>>EQUIVALENCE (sum1,yjkr) , (sum2,yjki)
   DATA subnam , begn , end/4HFBS4 , 4HBEGN , 4HEND /
   DATA zero/0.0D+0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         buf(1) = subnam
         buf(2) = begn
         CALL conmsg(buf,2,0)
         nbritm = Nwds/2
         j = (locfx(Yn)-locfx(Y)+1)/Nwds
         last = max0(j,1)*nbritm
         DO j = 1 , n
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  j1 = j - 1
                  DO k = 1 , last , nbritm
                     yjkr = Y(j1*2+k)
                     yjki = Y(j1*2+k+1)
                     IF ( yjkr/=zero .OR. yjki/=zero ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  CALL skprec(Block(1),1)
               CASE (2)
!
!     MAKE 1ST CALL FOR COLUMN AND SAVE DIAGONAL ELEMENT
!
                  Block(8) = -1
                  CALL getstr(*40,Block)
                  IF ( Block(4)/=j ) GOTO 40
                  jstr = Block(5)
                  ljjr = l(jstr)
                  ljji = l(jstr+1)
                  IF ( Block(6)==1 ) THEN
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  nstr = jstr + 2*Block(6) - 2
                  jstr = jstr + 2
                  Block(4) = Block(4) + 1
                  spag_nextblock_2 = 3
               CASE (3)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)
!
                  DO k = 1 , last , nbritm
                     yjkr = Y(j1*2+k)
                     yjki = Y(j1*2+k+1)
                     IF ( yjkr/=zero .OR. yjki/=zero ) THEN
                        ik = 2*Block(4) + k - 2
                        DO ij = jstr , nstr , 2
                           Y(ik) = Y(ik) + l(ij)*yjkr - l(ij+1)*yjki
                           Y(ik+1) = Y(ik+1) + l(ij)*yjki + l(ij+1)*yjkr
                           ik = ik + 2
                        ENDDO
                     ENDIF
                  ENDDO
                  spag_nextblock_2 = 4
               CASE (4)
!
!     GET NEXT STRING IN TRIANGULAR FACTOR
!
                  CALL endget(Block)
                  CALL getstr(*2,Block)
                  jstr = Block(5)
                  nstr = jstr + 2*Block(6) - 2
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
!
!     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL
!
 2                ssqr = 1.0D+0/(ljjr**2+ljji**2)
                  DO k = 1 , last , nbritm
                     yjkr = (Y(2*j+k-2)*ljjr+Y(2*j+k-1)*ljji)*ssqr
                     Y(2*j+k-1) = -(Y(2*j+k-2)*ljji-Y(2*j+k-1)*ljjr)*ssqr
                     Y(2*j+k-2) = yjkr
                  ENDDO
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
!     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN
!
         IF ( n==1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL bckrec(Block)
         j = n - 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     GET A STRING IN CURRENT COLUMN.  IF STRING INCLUDES DIAGONAL,
!     ADJUST STRING TO SKIP IT.
!
         j1 = j - 1
         Block(8) = -1
         DO
            CALL getstb(*20,Block)
            IF ( Block(4)-Block(6)==j1 ) Block(6) = Block(6) - 1
            IF ( Block(6)/=0 ) THEN
               nterms = Block(6)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)
!
               DO k = 1 , last , nbritm
                  ji = Block(5) + 2
                  ik = Block(4)*2 + k
                  jk = j1*2 + k
                  sum1 = 0.0D+0
                  sum2 = 0.0D+0
                  DO ii = 1 , nterms
                     ji = ji - 2
                     ik = ik - 2
                     sum1 = sum1 + l(ji)*Y(ik) - l(ji+1)*Y(ik+1)
                     sum2 = sum2 + l(ji)*Y(ik+1) + l(ji+1)*Y(ik)
                  ENDDO
                  Y(jk) = Y(jk) + sum1
                  Y(jk+1) = Y(jk+1) + sum2
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
 20      IF ( j/=1 ) THEN
!
            j = j - 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         buf(2) = end
         CALL conmsg(buf,2,0)
         RETURN
!
!     FATAL ERROR MESSAGE
!
 40      WRITE (nout,99001) sfm , subnam
99001    FORMAT (A25,' 2149, SUBROUTINE ',A4,/5X,'FIRST ELEMENT OF A COLU',                                                         &
                &'MN OF LOWER TRIANGULAR MATRIX IS NOT THE DIAGONAL ELEMENT')
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fbs4
