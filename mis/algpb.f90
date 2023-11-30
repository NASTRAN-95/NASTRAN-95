
SUBROUTINE algpb(Idat,Ntype)
   IMPLICIT NONE
   INTEGER Idat , Ntype
   INTEGER itype , na(4)
   INTEGER numtyp
!
   DATA na/2 , 2 , 3 , 1/
!                ZERO, INTEGER, REAL, ALPHA
!
!     RETURN FROM NUMTYP IS            SET NTYPE TO
!       0 -  ZERO                        1 - ALPHA
!       1 -  INTEGER                     2 - INTEGER
!       2 -  REAL                        3 - REAL
!       3 -  BCD
!
!     BLANK IS ALPHA,  ZERO IS INTEGER UNLESS NUMTYP SET IT TO REAL
!
   itype = numtyp(Idat) + 1
   Ntype = na(itype)
END SUBROUTINE algpb