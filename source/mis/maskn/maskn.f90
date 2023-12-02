!*==maskn.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION maskn(L2,L1)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: maskn
   INTEGER :: L2
   INTEGER :: L1
   EXTERNAL errtrc , lshift , rshift
!
! End of declarations rewritten by SPAG
!
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
