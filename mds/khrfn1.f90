
INTEGER FUNCTION khrfn1(Word1,I,Word2,J)
   IMPLICIT NONE
   INTEGER I , J
   INTEGER Word1(1) , Word2(1)
   INTEGER ii , jj , temp1 , temp2
   CHARACTER*4 tempc1 , tempc2
!
!     CHARACTER-FUNCTIONS 1,2,3,4, AND 5 WERE WRITTEN BY G.CHAN/UNISYS
!     TO STANDARDIZE NASTRAN BCD-WORD BYTE PROCESSING.
!
!     NOTE - THE INPUT WORD(S) ARE INTEGERS OR REALS, HOLDING BCD TYPE
!            DATA. (NOT CHARACTER)
!            BYTE COUNTS FROM LEFT TO RIGHT
!
!     THESE FIVE CHARACTER FUNCTIONS ARE COMPLETELY MACHINE INDEPENDENT
!
!     KHRFN1 REPLACES THE I-TH BYTE OF WORD 1 BY THE J-TH BYTE OF WORD2
!     E.G.   WORD1=ABCD,    WORD2=1234
!            KHRFN1(WORD1,3,WORD2,2) GIVES  AB2D
!
!     ABSOLUTE VALUES OF I AND J ARE USED
!
!     THE CODE BELOW WORKS WITH ALL MACHINES.  HOWEVER, SEE THE
!     SIMPLIFIED VERSION FURTHER DOWN.
!
!     INTEGER      WORD1(1),WORD2(1),TEMP(2)
!     CHARACTER*8  TEMP8
!
!     TEMP(1) = WORD1(1)
!     TEMP(2) = WORD2(1)
!     CALL BCDKH2 (TEMP,TEMP8)
!     II = IABS(I)
!     JJ = IABS(J) + 4
!     TEMP8(II:II) = TEMP8(JJ:JJ)
!     CALL KHRBC2 (TEMP8,TEMP)
!     KHRFN1 = TEMP(1)
!
!     SIMPLIFIED VERSION
!
!     FOR MACHINES (CDC, IBM, VAX, AND GRAY) THAT ALLOW EQUIVALENCE
!     BETWEEN CHARACTERS AND INTEGER VARIABLES, THE FOLLOWING SIMPLIFIED
!     CODE CAN BE USED.
!
!     CHARACTER*n  TEMPC1,TEMPC2
!        (WHERE n is 10 for CDC, 8 for 64-BIT UNIX and
!                     4 for VAX and IBM)
   !>>>>EQUIVALENCE (temp1,tempc1) , (temp2,tempc2)
!
   temp1 = Word1(1)
   temp2 = Word2(1)
   ii = iabs(I)
   jj = iabs(J)
   tempc1(ii:ii) = tempc2(jj:jj)
   khrfn1 = temp1
!
!     DEC/ULTRIX VERSION
!     ==================
!     THE ABOVE VAX VERSION DOES NOT WORK IN DEC/ULTRIX(RISC)
!
!     INTEGER     TEMP1,TEMP2,WORD1,WORD2
!     CHARACTER*1 TMP1(4),TMP2(4)
!     EQUIVALENCE (TEMP1,TMP1(1)),(TEMP2,TMP2(1))
!
!     TEMP1 = WORD1
!     TEMP2 = WORD2
!     II = IABS(I)
!     JJ = IABS(J)
!     TMP1(II) = TMP2(JJ)
!     KHR = TEMP1
!     RETURN
!
!     CDC VERSION
!     ===========
!     THE CHARACTER OPERATIONS IN CDC MACHINE ARE EXTREMELY SLOW.
!     THE FOLLOWING CODE, USING SHIFT/AND/OR IS 2 TO 3 TIMES
!     FASTER
!
!     INTEGER       WORD1,WORD2,MASK1(4),MASK2(4),BLANK
!     DATA  MASK1 / O"77000000000000000000", O"00770000000000000000",
!    1              O"00007700000000000000", O"00000077000000000000"/
!     DATA  MASK2 / O"00777777000000000000", O"77007777000000000000",
!    1              O"77770077000000000000", O"77777700000000000000"/
!     DATA  BLANK / O"00000000555555555555"/
!
!     II = IABS(I)
!     JJ = IABS(J)
!     KK = (JJ-II)*6
!     JJ = SHIFT(WORD2,KK)
!     KK = AND(JJ,MASK1(II))
!     JJ = AND(WORD1,MASK2(II))
!     II = OR(JJ,KK)
!     KHRFN1 = OR(II,BLANK)
!     RETURN
!
!
!     UNIVAC VERSION (1988 ORIGINAL)
!     ==============================
!
!     INTEGER     WORD1(1),WORD2(1)
!     CHARACTER   W1(8)*1 ,W4*4, W8*8
!     EUIVALENCE  (W1(1),W4,W8)
!
!     WRITE  (W8,10) WORD1(1),WORD2(1)
!  10 FORMAT (2A4)
!     II = IABS(I)
!     JJ = IABS(J) + 4
!     W1(II) = W1(JJ)
!     READ (W4,20) KHRFN1
!  20 FORMAT (A4)
!     RETURN
!
END FUNCTION khrfn1