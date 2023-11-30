
FUNCTION maskn(L2,L1)
   IMPLICIT NONE
   INTEGER L1 , L2
   INTEGER maskn
   INTEGER lshift , rshift
   EXTERNAL lshift , rshift
!
!     TO BUILD AN INTEGER MASK FOR BIT MANIPULATION
!                                                                   0 OR
!     64  60        48     36   32         <--- BIT COUNT ---          1
!      +---+---------+------+----+-------------------------------------+
!       ...   ....     ....   ...  ....00000011111111111111111111000...
!      +---+---------+------+----+-------------------------------------+
!                                            /                  /
!                                            L2                L1
!
!      BIT COUNTS FROM RIHGT (L1) TO LEFT (L2).  L1=0 IS SAME AS L1=1
!
!      E.G.      L2    L1      MASK PATTERN, WITH LEADING ZERO BITS
!               ----  ----  ------------------------------------------
!                12     0    A 12 BIT MASK, RIGHT ADJUSTED
!                24     8    A 24 BIT MASK, RIGHT ADJUSTED, WITH 8
!                            TRAILING ZERO BITS.
!
!      THIS ROUTINE IS SUITABLE FOR MACHINE WORD OF ANY BIT SIZE
!      BIT PATTERN CAN ALSO INCLUDE SIGN BIT.
!      SYSTEM MASK ROUINTE, IF IT EXISTS, IS NOT USED.
!
!      WRITTEN BY G.CHAN/UNISYS  10/1992
!
!
   IF ( L2<L1 ) CALL errtrc('MASKN   ',L2-L1)
   maskn = lshift(1,L2) - 1
   IF ( L1>1 ) maskn = lshift(rshift(maskn,L1-1),L1-1)
END FUNCTION maskn