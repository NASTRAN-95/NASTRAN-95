
SUBROUTINE loglog(A,B,C,D,E,F)
   IMPLICIT NONE
   REAL A , B , C , D , E , F
   REAL aa , bb , cc , dd , ee
!
!     WRITTEN BY G.CHAN/UNISYS  7/92, THE 1992 SUMMER OLYMPIC WEEK
!
!     LOG-LOG TABLE LOOKUP           10 +------+------+------+--+
!                                     D                 *
!     INPUT : A,B, C,D, AND E         8 +------+-------/-----+--+
!     OUTPUT: F                                       /
!                                                    /
!     ALL A,B,C,D,E,F IN LOG          4 +------+----/-+------+--+
!     SCALE                           F            *
!                                                 /
!     LINEAR EVALUATION ON LOG        2 +------+-/-----+------+--+
!     SCALE (NO POLYNOMIAL            B         *
!     EVALUATION)
!                                     1 +------+------+------+--+
!                                       1      2A  E  4 C    8  10
   aa = alog10(A)
   bb = alog10(B)
   cc = alog10(C) - aa
   dd = alog10(D) - bb
   ee = alog10(E) - aa
   F = 10.**(ee*dd/cc+bb)
   RETURN
!
!
   ENTRY smilog(A,B,C,D,E,F)
!     ==========================
!
!     SEMI-LOG TABLE LOOKUP       10 +--+--+--+--+--+--+--+--+--+
!                                  D                *
!     INPUT : A,B, C,D, AND E      8 +--+--+--+--+-/+--+--+--+--+
!     OUTPUT: F                                   /
!                                                /
!     A,C,E IN LINEAR SCALE        4 +--+--+--+-/+--+--+--+--+--+
!     B,D,F IN LOG SCALE           F           *
!                                             /
!                                  2 +--+--+-/+--+--+--+--+--+--+
!                                  B        *
!
!                                  1 +--+--+--+--+--+--+--+--+--+
!                                    0  1  2A 3E 4  C  6  7  8  9
   bb = alog10(B)
   cc = C - A
   dd = alog10(D) - bb
   ee = E - A
   F = 10.**(ee*dd/cc+bb)
   RETURN
!
!
   ENTRY logsmi(A,B,C,D,E,F)
!     ==========================
!
!     LOG-SEMI TABLE LOOKUP          10 +-----+-----+-----+--+
!                                     D                 *
!     INPUT:  A,B, C,D, AND E         8 +-----+-----+--/--+--+
!     OUTPUT: F                                       /
!                                     6 +-----+-----+/----+--+
!     A,C,E IN LOG SCALE                            /
!     B,D,F IN LINEAR SCALE           4 +-----+----/+-----+--+
!                                     F           *
!                                     2 +-----+--/--+-----+--+
!                                     B         *
!                                     0 +-----+-----+-----+--+
!                                       1     2 A E 4   C 8  10
   aa = alog10(A)
   cc = alog10(C) - aa
   dd = D - B
   ee = alog10(E) - aa
   F = ee*dd/cc + B
END SUBROUTINE loglog
