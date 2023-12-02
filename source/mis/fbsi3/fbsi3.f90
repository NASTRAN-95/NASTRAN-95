!*==fbsi3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbsi3(Block,Y,Mem,Dmem,Ibuff)
   USE c_fbsm
   USE c_fbsx
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(8) :: Block
   COMPLEX , DIMENSION(1) :: Y
   INTEGER , DIMENSION(2) :: Mem
   REAL , DIMENSION(2) :: Dmem
   INTEGER , DIMENSION(2) :: Ibuff
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: begn , end , subnam
   INTEGER , DIMENSION(2) :: buf
   INTEGER :: ifcol , ii , ij , ik , indxi , indxl , iopen , irow , iyrow , j , j1 , ji , k , kcol , last , lcol , ncol , nidlt ,   &
            & nrows
   COMPLEX :: ljj , sum , yjk
   COMPLEX , SAVE :: zero
   EXTERNAL bckrec , close , conmsg , dsspos , endget , endgtb , getstb , getstr , gopen , mesage , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     FBSI3 EXECUTES THE FORWARD/BACKWARD PASS FOR FBSI IN CSP
!
   DATA zero/(0.0,0.0)/
   DATA subnam , begn , end/4HFBS4 , 4HBEGN , 4HEND /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ncol = dbl(2)
         buf(1) = subnam
         buf(2) = begn
         iopen = 0
         CALL conmsg(buf,2,0)
         last = nvec*nvecsz
         nidlt = 1
         lcol = ipos(1)
         SPAG_Loop_1_1: DO j = 1 , lcol
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!      PRINT *,' FORWARD, PROCESSING COLUMN J=',J
                  j1 = j - 1
!
! CHECK IF THIS ROW VALUE IS ZERO FOR ALL RIGHT HAND VECTORS
!
                  DO k = j , last , nvecsz
                     IF ( Y(k)/=zero ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
!
! ALL VALUES FOR THIS ROW ARE ZERO, SKIP TO NEXT ROW OF RIGHT HAND VECTORS
!
                  IF ( nidlt>=lasind ) EXIT SPAG_Loop_1_1
                  kcol = Mem(nidlt)
                  IF ( kcol/=j ) GOTO 40
                  DO
                     nrows = Mem(nidlt+1)
                     nidlt = nidlt + nrows*nwds + 4
                     IF ( nidlt>=lasind ) EXIT SPAG_Loop_1_1
                     kcol = Mem(nidlt)
                     IF ( kcol/=j ) EXIT SPAG_DispatchLoop_2
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
!
!     GET 1ST STRING FOR COLUMN AND SAVE DIAGONAL ELEMENT
!
                  kcol = Mem(nidlt)
                  IF ( kcol/=j ) GOTO 40
                  nrows = Mem(nidlt+1)
                  irow = Mem(nidlt+nrows*nwds+2)
                  indxi = nidlt + 2
                  indxl = indxi + nrows*2 - 1
                  ljj = 1.0/cmplx(Dmem(indxi),Dmem(indxi+1))
                  IF ( nrows==1 ) THEN
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  indxi = indxi + 2
                  irow = irow + 1
                  spag_nextblock_2 = 3
               CASE (3)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)
!
                  DO k = 1 , last , nvecsz
                     yjk = Y(j1+k)
                     IF ( yjk/=zero ) THEN
                        iyrow = irow + k - 1
                        DO ij = indxi , indxl , 2
                           Y(iyrow) = Y(iyrow) + cmplx(Dmem(ij),Dmem(ij+1))*yjk
                           iyrow = iyrow + 1
                        ENDDO
                     ENDIF
                  ENDDO
                  spag_nextblock_2 = 4
               CASE (4)
!
!     GET NEXT STRING IN TRIANGULAR FACTOR
!
                  nidlt = nidlt + 4 + nrows*nwds
                  IF ( nidlt<lasind ) THEN
                     kcol = Mem(nidlt)
                     IF ( kcol==j ) THEN
                        nrows = Mem(nidlt+1)
                        irow = Mem(nidlt+nrows*nwds+2)
                        indxi = nidlt + 2
                        indxl = indxi + nrows*2 - 1
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
!
!     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL
!
                  DO k = j , last , nvecsz
                     Y(k) = Y(k)*ljj
                  ENDDO
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO SPAG_Loop_1_1
         IF ( lcol/=ncol ) THEN
            ifcol = lcol + 1
            CALL gopen(dbl,Ibuff,rdrew)
!
! POSITION FILE TO APPROPRIATE COLUMN TO BE READ
!
            CALL dsspos(dbl,ipos(2),ipos(3),ipos(4))
            DO j = ifcol , ncol
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     j1 = j - 1
!
! CHECK IF THIS ROW VALUE IS ZERO FOR ALL RIGHT HAND VECTORS
!
                     DO k = j , last , nvecsz
                        IF ( Y(k)/=zero ) THEN
                           spag_nextblock_3 = 2
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                     ENDDO
!
! ALL VALUES FOR THIS ROW ARE ZERO, SKIP TO NEXT ROW OF RIGHT HAND VECTORS
!
                     CALL skprec(dbl,1)
                  CASE (2)
!
!     GET 1ST STRING FOR COLUMN AND SAVE DIAGONAL ELEMENT
!
                     Block(8) = -1
                     CALL getstr(*40,Block)
                     IF ( Block(4)/=j ) GOTO 40
                     irow = Block(4)
                     indxi = Block(5)
                     nrows = Block(6)
                     indxl = indxi + nrows*2 - 1
                     ljj = 1.0/cmplx(l(indxi),l(indxi+1))
                     IF ( nrows==1 ) THEN
                        spag_nextblock_3 = 4
                        CYCLE SPAG_DispatchLoop_3
                     ENDIF
                     indxi = indxi + 2
                     irow = irow + 1
                     spag_nextblock_3 = 3
                  CASE (3)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(I,K) = Y(I,K) + L(I,J)*Y(J,K)
!
                     DO k = 1 , last , nvecsz
                        yjk = Y(j1+k)
                        IF ( yjk/=zero ) THEN
                           iyrow = irow + k - 1
                           DO ij = indxi , indxl , 2
                              Y(iyrow) = Y(iyrow) + cmplx(l(ij),l(ij+1))*yjk
                              iyrow = iyrow + 1
                           ENDDO
                        ENDIF
                     ENDDO
                     spag_nextblock_3 = 4
                  CASE (4)
!
!     GET NEXT STRING IN TRIANGULAR FACTOR
!
                     CALL endget(Block)
                     CALL getstr(*2,Block)
                     irow = Block(4)
                     indxi = Block(5)
                     nrows = Block(6)
                     indxl = indxi + nrows*2 - 1
                     spag_nextblock_3 = 3
                     CYCLE SPAG_DispatchLoop_3
!
!     END-OF-COLUMN ON TRIANGULAR FACTOR -- DIVIDE BY DIAGONAL
!
 2                   DO k = j , last , nvecsz
                        Y(k) = Y(k)*ljj
                     ENDDO
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
            ENDDO
         ENDIF
         IF ( ncol==1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = ncol - 1
         IF ( lcol==ncol ) THEN
            DO
!
!
!     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN
!
               nidlt = nidlt - 1
               nrows = Mem(nidlt)
               nidlt = nidlt - nrows*nwds - 3
               kcol = Mem(nidlt)
               IF ( kcol/=ncol ) THEN
                  nidlt = nidlt + nrows*nwds + 4
!
!     GET A STRING IN CURRENT COLUMN. IF THIS STRING INCLUDES DIAGONAL,
!     ADJUST STRING TO SKIP IT.
!
                  j1 = j - 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ELSE
!
!     INITIALIZE FOR BACKWARD PASS BY SKIPPING THE NTH COLUMN
!
            CALL bckrec(Block)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     GET A STRING IN CURRENT COLUMN. IF THIS STRING INCLUDES DIAGONAL,
!     ADJUST STRING TO SKIP IT.
!
         j1 = j - 1
         Block(8) = -1
         DO
            CALL getstb(*20,Block)
            irow = Block(4)
            nrows = Block(6)
            IF ( irow-nrows==j1 ) nrows = nrows - 1
            IF ( nrows/=0 ) THEN
               indxi = Block(5)
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)
!
               DO k = 1 , last , nvecsz
                  ji = indxi + 2
                  ik = irow + k
                  sum = (0.0,0.0)
                  DO ii = 1 , nrows
                     ji = ji - 2
                     ik = ik - 1
                     sum = sum + cmplx(l(ji),l(ji+1))*Y(ik)
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
 20      IF ( j==1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = j - 1
         IF ( j/=lcol ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j1 = j - 1
         spag_nextblock_1 = 3
      CASE (3)
!      print *,' processing column in backward step, j=',j
         nidlt = nidlt - 1
         IF ( nidlt<=1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nrows = Mem(nidlt)
         irow = Mem(nidlt-1)
         nidlt = nidlt - nrows*nwds - 3
         kcol = Mem(nidlt)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( kcol==j ) THEN
            indxi = nidlt + nrows*2
            irow = irow + nrows - 1
            IF ( (irow-nrows)==j1 ) nrows = nrows - 1
!
!     PROCESS CURRENT STRING IN TRIANGULAR FACTOR AGAINST EACH
!     LOAD VECTOR IN CORE -- Y(J,K) = Y(J,K) + L(J,I)*Y(I,K)
!
            DO k = 1 , last , nvecsz
               ji = indxi + 2
               ik = irow + k
               sum = 0.0
               DO ii = 1 , nrows
                  ji = ji - 2
                  ik = ik - 1
                  sum = sum + cmplx(Dmem(ji),Dmem(ji+1))*Y(ik)
               ENDDO
               Y(j1+k) = Y(j1+k) + sum
!
!     TERMINATE CURRENT STRING AND GET NEXT STRING
!
            ENDDO
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     END-OF-COLUMN -- TEST FOR COMPLETION
!
         IF ( j/=1 ) THEN
            j = j - 1
            j1 = j - 1
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
!
         buf(2) = end
         CALL conmsg(buf,2,0)
         CALL close(dbl,rew)
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
END SUBROUTINE fbsi3
