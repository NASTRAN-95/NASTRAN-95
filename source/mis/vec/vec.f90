!*==vec.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE vec
   USE c_bitpos
   USE c_blank
   USE c_packx
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: b , c , d , modnam
   INTEGER , SAVE :: blank , fi , fo , nbn , nermax
   LOGICAL :: cols , flag1 , flag2 , l0 , l1 , lz
   INTEGER :: f , i , ib , j , k , l , lc , lcex , maskx , maskx0 , maskx1 , nr , nw , nz , offset
   INTEGER , DIMENSION(2,2) , SAVE :: lr
   INTEGER , DIMENSION(2) :: nam , p
   INTEGER , DIMENSION(7) :: t
   EXTERNAL andf , close , fname , gopen , korsz , makmcb , mesage , pack , read , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THE CALL TO THIS MODULE IS
!                   VEC USET  / V / C,N,X / C,N,X0 / C,N,X1 $
!          OR       VEC USETD / V / C,N,X / C,N,X0 / C,N,X1 $
!
!     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
!                   VEC USET  / V / C,N,X / C,N,X0 / C,N,COMP $
!          OR       VEC USETD / V / C,N,X / C,N,X0 / C,N,COMP $
!
!     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
!                   VEC USET  / V / C,N,X / C,N,COMP / C,N,X1 $
!          OR       VEC USETD / V / C,N,X / C,N,COMP / C,N,X1 $
!
!     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
!                   VEC USET  / V / C,N,BITID / C,N,* / C,N,* / C,N,I $
!          OR       VEC USET  / V / C,N,BITID / C,N,X1 $
!          OR       VEC USETD / V / C,N,BITID / C,N,* / C,N,* / C,N,I $
!          OR       VEC USETD / V / C,N,BITID / C,N,X1 $
!
!     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
!                   VEC USET  / V / C,N,COLUMNS / C,N,LEFT  / C,N,* /
!                                                             C,N,I $
!          OR       VEC USETD / V / C,N,COLUMNS / C,N,LEFT  / C,N,* /
!                                                             C,N,I $
!                   ( V WILL HAVE -I- COLUMNS GENERATED FROM BIT
!                     POSITIONS 1,2,3,...,I OF USET (OR USETD) WHERE
!                     THE 32 RIGHT-MOST BITS ARE CONSIDERED, COUNTING
!                     FROM LEFT TO RIGHT. )
!
!     ALTERNATE FORM OF THE CALL TO THIS MODULE IS
!                   VEC USET  / V / C,N,COLUMNS / C,N,RIGHT / C,N,* /
!                                                             C,N,I $
!          OR       VEC USETD / V / C,N,COLUMNS / C,N,RIGHT / C,N,* /
!                                                             C,N,I $
!                   ( V WILL HAVE -I- COLUMNS GENERATED FROM BIT
!                     POSITIONS 32,31,...,33-I OF USET (OR USETD) WHERE
!                     THE 32 RIGHT-MOST BITS ARE CONSIDERED, COUNTING
!                     FROM LEFT TO RIGHT. )
!
!
!     CORE REQUIREMENTS.. ONE BUFFER PLUS USET (OR USETD).
!     FOR COLUMNS OPTION, ONE GINO BUFFER PLUS 2*USET (OR USETD) REQD.
!
!
   !>>>>EQUIVALENCE (nr,t(3))
   DATA nermax , blank/10 , 1H /
   DATA b , c , d/4HBITI , 4HD    , 4HCOMP , 4H     , 4HCOLU , 4HMNS /
   DATA lr/4HRIGH , 4HT    , 4HLEFT , 4H    /
   DATA modnam/4HVEC  , 4H    /
   DATA fi , fo , nbn/101 , 201 , 32/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         flag1 = .FALSE.
         flag2 = .FALSE.
         offset = 0
         nerr = 0
         lz = .FALSE.
         l0 = .FALSE.
         l1 = .FALSE.
         lc = korsz(x) - lb
         IF ( lc<=0 ) CALL mesage(-8,lc,modnam)
         ib = lc + 1
!
!     CHECK PARAMETER VALUES AND COMPUTE MASKS.
!
         IF ( p1(1)/=d(1) .OR. p1(2)/=d(2) ) THEN
            cols = .FALSE.
            IF ( p1(1)/=b(1) .OR. p1(2)/=b(2) ) THEN
               IF ( p1(2)==blank ) THEN
                  DO i = 1 , nbn
                     IF ( p1(1)==bn(i,2) ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
               p(1) = p1(1)
               p(2) = p1(2)
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            cols = .TRUE.
            DO j = 1 , 2
               IF ( p2(1)==lr(1,j) .AND. p2(2)==lr(2,j) ) GOTO 10
            ENDDO
            j = 2
 10         j = 2*j - 3
         ENDIF
         lz = .TRUE.
         l0 = .TRUE.
         IF ( p4<0 .OR. p4>32 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( cols ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( p4>0 ) THEN
            maskx1 = two(p4)
            spag_nextblock_1 = 7
         ELSE
            IF ( p2(2)==blank ) THEN
               DO i = 1 , nbn
                  IF ( p2(1)==bn(i,2) ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
            spag_nextblock_1 = 3
         ENDIF
      CASE (2)
         i = bn(i,1)
         maskx = two(i)
!
         IF ( p2(1)==c(1) .AND. p2(2)==c(2) ) THEN
            l0 = .TRUE.
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( p2(2)==blank ) THEN
            DO i = 1 , nbn
               IF ( p2(1)==bn(i,2) ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         p(1) = p2(1)
         p(2) = p2(2)
         spag_nextblock_1 = 9
      CASE (4)
         i = bn(i,1)
         maskx0 = two(i)
         spag_nextblock_1 = 5
      CASE (5)
!
         IF ( p3(1)==c(1) .AND. p3(2)==c(2) ) THEN
            l1 = .TRUE.
            IF ( .NOT.(l0) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            WRITE (nout,99001) ufm
99001       FORMAT (A23,' 2146, BOTH OF THE SECOND AND THIRD VEC PARAMETERS ','REQUEST COMPLEMENT.')
            CALL mesage(-61,0,0)
            RETURN
         ELSE
            IF ( p3(2)==blank ) THEN
               DO i = 1 , nbn
                  IF ( p3(1)==bn(i,2) ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
            ENDIF
            p(1) = p3(1)
            p(2) = p3(2)
            spag_nextblock_1 = 9
         ENDIF
      CASE (6)
         i = bn(i,1)
         maskx1 = two(i)
         spag_nextblock_1 = 7
      CASE (7)
!
!     BLAST READ USET (OR USETD) INTO CORE.
!
         f = fi
         CALL fname(f,nam)
         CALL gopen(f,x(ib),0)
         CALL read(*60,*40,f,x,lc,0,nw)
!
!     INSUFFICIENT CORE - IF DESIRED, THIS ROUTINE CAN BE WRITTEN TO
!     RUN IN SMALLER CORE.
!
         lcex = 0
         DO
            CALL read(*60,*20,f,x,lc,0,nw)
            lcex = lcex + lc
         ENDDO
 20      lcex = lcex + nw
         IF ( cols ) lcex = 2*lcex
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 40      CALL close(f,1)
         IF ( cols ) THEN
            IF ( p4<=0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            offset = nw
            k = 1
            l = 1
            IF ( j<0 ) k = 32
            maskx1 = two(k)
            IF ( 2*nw>lc ) THEN
               lcex = 2*nw - lc
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     PREPARE OUTPUT FILE.
!
         f = fo
         CALL gopen(f,x(ib),1)
         CALL makmcb(t,f,0,2,1)
         tyin = 1
         tyou = 1
         ii = 1
         incr = 1
         SPAG_Loop_1_1: DO
!
!     CREATE VECTOR IN CORE OCCUPIED BY USET (OR USETD).
!
            nr = 0
            nz = 0
!
            DO i = 1 , nw
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( .NOT.(lz) ) THEN
                        IF ( andf(x(i),maskx)==0 ) THEN
                           IF ( .NOT.(l0) ) THEN
                              IF ( andf(x(i),maskx0)/=0 ) THEN
                                 nerr = nerr + 1
                                 IF ( nerr<=nermax ) THEN
                                    WRITE (nout,99002) ufm , i
99002                               FORMAT (A23,' 2122, MODULE VEC - SET X BIT IS ZERO BUT SUBSET X0',' BIT IS NOT.  I =',I10)
                                 ENDIF
                              ENDIF
                           ENDIF
                           IF ( .NOT.(l1) ) THEN
                              IF ( andf(x(i),maskx1)/=0 ) THEN
                                 nerr = nerr + 1
                                 IF ( nerr<=nermax ) THEN
                                    WRITE (nout,99003) ufm , i
99003                               FORMAT (A23,' 2123, MODULE VEC - SET X BIT IS ZERO BUT SUBSET X1',' BIT IS NOT.  I =',I10)
                                 ENDIF
                              ENDIF
                           ENDIF
                           CYCLE
                        ENDIF
                     ENDIF
                     IF ( .NOT.l0 ) THEN
                        IF ( .NOT.l1 ) THEN
                           IF ( andf(x(i),maskx1)==0 ) THEN
                              IF ( andf(x(i),maskx0)/=0 ) THEN
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                              nerr = nerr + 1
                              IF ( nerr<=nermax ) THEN
                                 WRITE (nout,99004) ufm , i
99004                            FORMAT (A23,' 2121, MODULE VEC - BOTH SUBSET BITS ARE ZERO.',3X,'I =',I10)
                              ENDIF
                              CYCLE
                           ELSEIF ( andf(x(i),maskx0)/=0 ) THEN
                              nerr = nerr + 1
                              IF ( nerr<=nermax ) THEN
                                 WRITE (nout,99005) ufm , i
99005                            FORMAT (A23,' 2120, MODULE VEC - BOTH SUBSET BITS ARE NON-ZERO.',3X,'I =',I10)
                              ENDIF
                              CYCLE
                           ENDIF
                        ELSEIF ( andf(x(i),maskx0)/=0 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ELSEIF ( andf(x(i),maskx1)==0 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     nr = nr + 1
                     nz = nz + 1
                     x(nr+offset) = 1.0
                  CASE (2)
                     nr = nr + 1
                     x(nr+offset) = 0.0
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
!
            IF ( nerr<=0 ) THEN
!
               IF ( .NOT.(flag1) ) THEN
                  flag1 = .TRUE.
                  IF ( nr<=0 ) THEN
                     WRITE (nout,99006) uwm
99006                FORMAT (A25,' 2124, MODULE VEC - NR=0, OUTPUT WILL BE PURGED.')
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ENDIF
               IF ( nz<=0 ) THEN
                  IF ( .NOT.(flag2) ) THEN
                     flag2 = .TRUE.
                     WRITE (nout,99007) uwm
99007                FORMAT (A25,' 2125, MODULE VEC - NZ=0, ONE OR MORE COLUMNS OF ','OUTPUT MATRIX WILL BE NULL.')
                  ENDIF
               ENDIF
!
!     PACK OUT COLUMN OF OUTPUT VECTOR.
!
               nn = nr
               CALL pack(x(offset+1),f,t)
               IF ( .NOT.cols .OR. l>=p4 ) THEN
                  CALL wrttrl(t)
                  EXIT SPAG_Loop_1_1
               ELSE
                  l = l + 1
                  k = k + j
                  maskx1 = two(k)
               ENDIF
            ELSE
               IF ( nerr>nermax ) THEN
                  WRITE (nout,99008) ufm , nerr , nermax
99008             FORMAT (A23,' 2145,',I8,' FATAL MESSAGES HAVE BEEN GENERATED IN',' SUBROUTINE VEC.',/5X,'ONLY THE FIRST',I4,      &
                         &' HAVE BEEN PRINTED.')
               ENDIF
               CALL mesage(-61,0,0)
               RETURN
            ENDIF
         ENDDO SPAG_Loop_1_1
         CALL close(f,1)
!
         RETURN
!
!     ERROR PROCESSING.
!
 60      WRITE (nout,99009) ufm , f , nam
99009    FORMAT (A23,' 2141, MODULE VEC - EOF ENCOUNTERED WHILE READING ','GINO FILE ',I3,', DATA BLOCK ',2A4)
         CALL mesage(-61,0,0)
         RETURN
      CASE (8)
         WRITE (nout,99010) ufm , lc , lcex
99010    FORMAT (A23,' 2142, INSUFFICIENT CORE FOR MODULE VEC.  AVAILABLE',' CORE =',I11,' WORDS.',/5X,'ADDITIONAL CORE NEEDED =',  &
               & I11,' WORDS.')
         CALL mesage(-61,0,0)
         RETURN
      CASE (9)
         WRITE (nout,99011) ufm , p
99011    FORMAT (A23,' 2143, MODULE VEC UNABLE TO IDENTIFY SET OR SUBSET ','DESCRIPTOR ',2A4)
         CALL mesage(-61,0,0)
         RETURN
      CASE (10)
         WRITE (nout,99012) ufm , p4
99012    FORMAT (A23,' 2150, ILLEGAL VALUE FOR FOURTH PARAMETER =',I11)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE vec
