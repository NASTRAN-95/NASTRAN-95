
SUBROUTINE vec
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bn(32,2) , Ii , Incr , Lb , Nerr , Nn , Nout , P1(2) , P2(2) , P3(2) , P4 , Two(32) , Tyin , Tyou
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   REAL X(1)
   COMMON /bitpos/ Bn
   COMMON /blank / P1 , P2 , P3 , P4
   COMMON /packx / Tyin , Tyou , Ii , Nn , Incr
   COMMON /system/ Lb , Nout , Nerr
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ X
!
! Local variable declarations
!
   INTEGER andf , korsz
   INTEGER b(2) , blank , c(2) , d(2) , f , fi , fo , i , ib , j , k , l , lc , lcex , lr(2,2) , maskx , maskx0 , maskx1 , modnam(2)&
         & , nam(2) , nbn , nermax , nr , nw , nz , offset , p(2) , t(7)
   LOGICAL cols , flag1 , flag2 , l0 , l1 , lz
   EXTERNAL andf
!
! End of declarations
!
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
   EQUIVALENCE (nr,t(3))
   DATA nermax , blank/10 , 1H /
   DATA b , c , d/4HBITI , 4HD    , 4HCOMP , 4H     , 4HCOLU , 4HMNS /
   DATA lr/4HRIGH , 4HT    , 4HLEFT , 4H    /
   DATA modnam/4HVEC  , 4H    /
   DATA fi , fo , nbn/101 , 201 , 32/
!
!
   flag1 = .FALSE.
   flag2 = .FALSE.
   offset = 0
   Nerr = 0
   lz = .FALSE.
   l0 = .FALSE.
   l1 = .FALSE.
   lc = korsz(X) - Lb
   IF ( lc<=0 ) CALL mesage(-8,lc,modnam)
   ib = lc + 1
!
!     CHECK PARAMETER VALUES AND COMPUTE MASKS.
!
   IF ( P1(1)/=d(1) .OR. P1(2)/=d(2) ) THEN
      cols = .FALSE.
      IF ( P1(1)/=b(1) .OR. P1(2)/=b(2) ) THEN
         IF ( P1(2)==blank ) THEN
            DO i = 1 , nbn
               IF ( P1(1)==Bn(i,2) ) GOTO 100
            ENDDO
         ENDIF
         p(1) = P1(1)
         p(2) = P1(2)
         GOTO 1100
      ENDIF
   ELSE
      cols = .TRUE.
      DO j = 1 , 2
         IF ( P2(1)==lr(1,j) .AND. P2(2)==lr(2,j) ) GOTO 50
      ENDDO
      j = 2
 50   j = 2*j - 3
   ENDIF
   lz = .TRUE.
   l0 = .TRUE.
   IF ( P4<0 .OR. P4>32 ) GOTO 1200
   IF ( cols ) GOTO 600
   IF ( P4>0 ) THEN
      maskx1 = Two(P4)
      GOTO 600
   ELSE
      IF ( P2(2)==blank ) THEN
         DO i = 1 , nbn
            IF ( P2(1)==Bn(i,2) ) GOTO 500
         ENDDO
      ENDIF
      GOTO 200
   ENDIF
 100  i = Bn(i,1)
   maskx = Two(i)
!
   IF ( P2(1)==c(1) .AND. P2(2)==c(2) ) THEN
      l0 = .TRUE.
      GOTO 400
   ELSEIF ( P2(2)==blank ) THEN
      DO i = 1 , nbn
         IF ( P2(1)==Bn(i,2) ) GOTO 300
      ENDDO
   ENDIF
 200  p(1) = P2(1)
   p(2) = P2(2)
   GOTO 1100
 300  i = Bn(i,1)
   maskx0 = Two(i)
!
 400  IF ( P3(1)==c(1) .AND. P3(2)==c(2) ) THEN
      l1 = .TRUE.
      IF ( .NOT.(l0) ) GOTO 600
      WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 2146, BOTH OF THE SECOND AND THIRD VEC PARAMETERS ','REQUEST COMPLEMENT.')
      CALL mesage(-61,0,0)
      GOTO 99999
   ELSE
      IF ( P3(2)==blank ) THEN
         DO i = 1 , nbn
            IF ( P3(1)==Bn(i,2) ) GOTO 500
         ENDDO
      ENDIF
      p(1) = P3(1)
      p(2) = P3(2)
      GOTO 1100
   ENDIF
 500  i = Bn(i,1)
   maskx1 = Two(i)
!
!     BLAST READ USET (OR USETD) INTO CORE.
!
 600  f = fi
   CALL fname(f,nam)
   CALL gopen(f,X(ib),0)
   CALL read(*900,*800,f,X,lc,0,nw)
!
!     INSUFFICIENT CORE - IF DESIRED, THIS ROUTINE CAN BE WRITTEN TO
!     RUN IN SMALLER CORE.
!
   lcex = 0
   DO
      CALL read(*900,*700,f,X,lc,0,nw)
      lcex = lcex + lc
   ENDDO
 700  lcex = lcex + nw
   IF ( cols ) lcex = 2*lcex
   GOTO 1000
 800  CALL close(f,1)
   IF ( cols ) THEN
      IF ( P4<=0 ) GOTO 1200
      offset = nw
      k = 1
      l = 1
      IF ( j<0 ) k = 32
      maskx1 = Two(k)
      IF ( 2*nw>lc ) THEN
         lcex = 2*nw - lc
         GOTO 1000
      ENDIF
   ENDIF
!
!     PREPARE OUTPUT FILE.
!
   f = fo
   CALL gopen(f,X(ib),1)
   CALL makmcb(t,f,0,2,1)
   Tyin = 1
   Tyou = 1
   Ii = 1
   Incr = 1
   DO
!
!     CREATE VECTOR IN CORE OCCUPIED BY USET (OR USETD).
!
      nr = 0
      nz = 0
!
      DO i = 1 , nw
         IF ( .NOT.(lz) ) THEN
            IF ( andf(X(i),maskx)==0 ) THEN
               IF ( .NOT.(l0) ) THEN
                  IF ( andf(X(i),maskx0)/=0 ) THEN
                     Nerr = Nerr + 1
                     IF ( Nerr<=nermax ) THEN
                        WRITE (Nout,99002) Ufm , i
99002                   FORMAT (A23,' 2122, MODULE VEC - SET X BIT IS ZERO BUT SUBSET X0',' BIT IS NOT.  I =',I10)
                     ENDIF
                  ENDIF
               ENDIF
               IF ( .NOT.(l1) ) THEN
                  IF ( andf(X(i),maskx1)/=0 ) THEN
                     Nerr = Nerr + 1
                     IF ( Nerr<=nermax ) THEN
                        WRITE (Nout,99003) Ufm , i
99003                   FORMAT (A23,' 2123, MODULE VEC - SET X BIT IS ZERO BUT SUBSET X1',' BIT IS NOT.  I =',I10)
                     ENDIF
                  ENDIF
               ENDIF
               CYCLE
            ENDIF
         ENDIF
         IF ( .NOT.l0 ) THEN
            IF ( .NOT.l1 ) THEN
               IF ( andf(X(i),maskx1)==0 ) THEN
                  IF ( andf(X(i),maskx0)/=0 ) GOTO 820
                  Nerr = Nerr + 1
                  IF ( Nerr<=nermax ) THEN
                     WRITE (Nout,99004) Ufm , i
99004                FORMAT (A23,' 2121, MODULE VEC - BOTH SUBSET BITS ARE ZERO.',3X,'I =',I10)
                  ENDIF
                  CYCLE
               ELSEIF ( andf(X(i),maskx0)/=0 ) THEN
                  Nerr = Nerr + 1
                  IF ( Nerr<=nermax ) THEN
                     WRITE (Nout,99005) Ufm , i
99005                FORMAT (A23,' 2120, MODULE VEC - BOTH SUBSET BITS ARE NON-ZERO.',3X,'I =',I10)
                  ENDIF
                  CYCLE
               ENDIF
            ELSEIF ( andf(X(i),maskx0)/=0 ) THEN
               GOTO 820
            ENDIF
         ELSEIF ( andf(X(i),maskx1)==0 ) THEN
            GOTO 820
         ENDIF
         nr = nr + 1
         nz = nz + 1
         X(nr+offset) = 1.0
         CYCLE
 820     nr = nr + 1
         X(nr+offset) = 0.0
      ENDDO
!
      IF ( Nerr<=0 ) THEN
!
         IF ( .NOT.(flag1) ) THEN
            flag1 = .TRUE.
            IF ( nr<=0 ) THEN
               WRITE (Nout,99006) Uwm
99006          FORMAT (A25,' 2124, MODULE VEC - NR=0, OUTPUT WILL BE PURGED.')
               EXIT
            ENDIF
         ENDIF
         IF ( nz<=0 ) THEN
            IF ( .NOT.(flag2) ) THEN
               flag2 = .TRUE.
               WRITE (Nout,99007) Uwm
99007          FORMAT (A25,' 2125, MODULE VEC - NZ=0, ONE OR MORE COLUMNS OF ','OUTPUT MATRIX WILL BE NULL.')
            ENDIF
         ENDIF
!
!     PACK OUT COLUMN OF OUTPUT VECTOR.
!
         Nn = nr
         CALL pack(X(offset+1),f,t)
         IF ( .NOT.cols .OR. l>=P4 ) THEN
            CALL wrttrl(t)
            EXIT
         ELSE
            l = l + 1
            k = k + j
            maskx1 = Two(k)
         ENDIF
      ELSE
         IF ( Nerr>nermax ) THEN
            WRITE (Nout,99008) Ufm , Nerr , nermax
99008       FORMAT (A23,' 2145,',I8,' FATAL MESSAGES HAVE BEEN GENERATED IN',' SUBROUTINE VEC.',/5X,'ONLY THE FIRST',I4,            &
                   &' HAVE BEEN PRINTED.')
         ENDIF
         CALL mesage(-61,0,0)
         GOTO 99999
      ENDIF
   ENDDO
   CALL close(f,1)
!
   RETURN
!
!     ERROR PROCESSING.
!
 900  WRITE (Nout,99009) Ufm , f , nam
99009 FORMAT (A23,' 2141, MODULE VEC - EOF ENCOUNTERED WHILE READING ','GINO FILE ',I3,', DATA BLOCK ',2A4)
   CALL mesage(-61,0,0)
   GOTO 99999
 1000 WRITE (Nout,99010) Ufm , lc , lcex
99010 FORMAT (A23,' 2142, INSUFFICIENT CORE FOR MODULE VEC.  AVAILABLE',' CORE =',I11,' WORDS.',/5X,'ADDITIONAL CORE NEEDED =',I11, &
             &' WORDS.')
   CALL mesage(-61,0,0)
   GOTO 99999
 1100 WRITE (Nout,99011) Ufm , p
99011 FORMAT (A23,' 2143, MODULE VEC UNABLE TO IDENTIFY SET OR SUBSET ','DESCRIPTOR ',2A4)
   CALL mesage(-61,0,0)
   GOTO 99999
 1200 WRITE (Nout,99012) Ufm , P4
99012 FORMAT (A23,' 2150, ILLEGAL VALUE FOR FOURTH PARAMETER =',I11)
   CALL mesage(-61,0,0)
!
99999 RETURN
END SUBROUTINE vec
