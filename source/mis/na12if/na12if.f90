!*==na12if.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE na12if(*,A,N,B,Int)
   USE c_xreadx
   USE C_XREADX
   IMPLICIT NONE
   INTEGER Nout
   COMMON /xreadx/ Nout
   REAL B
   INTEGER Int , N
   INTEGER A(1)
   CHARACTER*1 C(1)
   CHARACTER*1 bk , num(10) , pt , t(24) , tj
   CHARACTER*12 blnk , next , temp
   INTEGER i , j , k , nt
   REAL xi
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     VAX, IBM AND UNIVAC VERSION (CHARACTER FUNCTION PROCESSING)
!     ===========================
!
   !>>>>EQUIVALENCE (temp,t(1)) , (next,t(13)) , (i,xi)
   DATA bk , pt , blnk/' ' , '.' , '            '/
   DATA num/'0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ARRAY A, IN NA1 BCD WORDS (OR C IN CHARACTERS), IS DECODED TO
!     AN INTEGER OR TO A F.P. NUMBER IN B.
!     INT SHOULD BE SET TO +1 IF CALLER IS EXPECTING B TO BE AN INTEGER,
!     OR SET TO -1 IF B IS TO BE A F.P. NUMBER.   SET INT TO ZERO IF
!     CALLER IS NOT SURE.  IN THIS LAST CASE, INT WILL BE SET TO +1 OR
!     -1 BY NA12IF/NK12IF ACCORDING TO THE INPUT DATA TYPE.
!     THESE ROUTINES HANDLE UP TO 12 DIGITS INPUT DATA (N .LE. 12)
!     (NO SYSTEM ENCODE/DECODE FUNCTIONS ARE USED)
!
!     ENTRY POINTS   NA1 2 IF  (BCD-INTEGER/FP VERSION)
!                    NK1 2 IF  (CHARACTER-INTEGER/FP VERSION)
!
!     WRITTEN BY G.CHAN/SPERRY IN AUG. 1985
!     PARTICULARLY FOR XREAD ROUTINE, IN SUPPORT OF ITS NEW FREE-FIELD
!     INPUT FORMAT.  THIS SUBROUTINE IS MACHINE INDEPENDENT
!
         IF ( N>12 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL b2k(A,temp,N)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY nk12if(*,C,N,B,Int)
!     ****************************
!
         IF ( N>12 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 1 , N
            t(i) = C(i)
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( Int>=1 ) THEN
!
!     QUICK WAY TO GET THE INTEGER
!
            i = 0
            j = 0
            SPAG_Loop_1_1: DO
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     j = j + 1
                     IF ( j>N ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     tj = t(j)
                     IF ( tj==bk ) CYCLE
                     DO k = 1 , 10
                        IF ( tj==num(k) ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     EXIT SPAG_Loop_1_1
                  CASE (2)
                     i = i*10 + k - 1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO SPAG_Loop_1_1
         ENDIF
         nt = 1
         k = 24
         j = N
         next = blnk
         DO i = 1 , 12
            IF ( i>N ) THEN
               t(k) = bk
               k = k - 1
            ELSE
               tj = t(j)
               IF ( tj/=bk ) THEN
                  IF ( tj==pt ) nt = nt - 2
                  t(k) = tj
                  k = k - 1
               ENDIF
            ENDIF
            j = j - 1
         ENDDO
!
         IF ( nt<-1 .OR. Int*nt<0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Int==0 ) Int = nt
         IF ( Int<0 ) THEN
            READ (next,99001) B
99001       FORMAT (F12.0)
            RETURN
         ELSEIF ( Int==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            READ (next,99002) i
99002       FORMAT (I12)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         B = xi
         RETURN
      CASE (4)
!
         B = 0.
         WRITE (Nout,99003) N
99003    FORMAT (5X,'*** N.GT.12/NA12IF',I6)
         spag_nextblock_1 = 5
      CASE (5)
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE na12if
