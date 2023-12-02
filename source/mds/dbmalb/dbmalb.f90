!*==dbmalb.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dbmalb(Lenreq,Index)
   USE i_dsiof
   USE i_zzzzzz
   USE I_DSIOF
   USE I_ZZZZZZ
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'ZZZZZZ.COM'
   INTEGER Index , Lenreq
   INTEGER iprev , lenavl , newind , next
!********************************************************************
!     DBMALB - ALLOCATES A MEMORY BLOCK OF LENGTH "LENREQ"
!              FROM THE FREE CHAIN AND RETURNS THE
!              POINTER IN MEMORY FOR THE BLOCK IN "INDEX" (RELATIVE TO.
!              /DBM/.
!
!     EACH FREE BLOCK IN MEMORY HAS THE FOLLOWING FORMAT:
!         WORD 1  POINTER TO PREVIOUS FREE BLOCK (=0, IF FIRST)
!         WORD 2  POINTER TO NEXT FREE BLOCK (=0, IF END OF CHAIN)
!         WORD 3  NUMBER OF WORDS AVAILABLE IN THIS FREE BLOCK
!
!     NOTE:  IDBFRE POINTS TO THE FIRST FREE BLOCK OF THE CHAIN
!********************************************************************
   next = idbfre
   IF ( idbfre/=0 ) THEN
      SPAG_Loop_1_1: DO
!  OBTAIN THE LENGTH OF THE FREE BLOCK
         lenavl = mem(next+2)
         IF ( lenavl>=Lenreq ) THEN
!  RETURN POINTER FOR THIS BLOCK
            Index = next
            IF ( Lenreq/=lenavl ) THEN
!  COME HERE WHEN FREE BLOCK HAS MORE SPACE THEN REQUESTED
               newind = Index + Lenreq + 4
               iprev = mem(Index)
               next = mem(Index+1)
!  CHECK TO DETERMINE IF ANY SPACE REMAINS
               IF ( (lenavl-Lenreq-4)<=0 ) THEN
!  FREE CHAIN IS EXHAUSTED
                  idbfre = 0
                  EXIT SPAG_Loop_1_1
               ELSE
!  RECOMPUTE FREE SPACE AND SET UP CHAIN WORDS
                  mem(newind+2) = lenavl - Lenreq - 4
                  IF ( iprev==0 ) THEN
                     IF ( next==0 ) THEN
!  NEW BLOCK IS THE ONLY FREE BLOCK
                        idbfre = newind
                        mem(newind) = 0
                        mem(newind+1) = 0
                     ELSE
!  NO PREVIOUS BLOCK, NEWLY CREATED BLOCK BECOMES THE FIRST FREE BLOCK
                        idbfre = newind
                        mem(newind) = 0
                        mem(newind+1) = next
                        mem(next) = newind
                     ENDIF
                  ELSEIF ( next==0 ) THEN
!  PREVIOUS BLOCK EXISTS BUT THE NEWLY CREATED BLOCK IS LAST
                     mem(iprev+1) = newind
                     mem(newind) = iprev
                     mem(newind+1) = 0
                  ELSE
!  CONNECT TO PREVIOUS AND NEXT FREE BLOCK
                     mem(newind) = iprev
                     mem(newind+1) = next
                     mem(iprev+1) = newind
                     mem(next) = newind
                  ENDIF
                  RETURN
               ENDIF
            ELSE
!  COME HERE WHEN REQUESTED BLOCK SAME SIZE AS FREE BLOCK
               next = mem(next+1)
               iprev = mem(Index)
               IF ( iprev==0 ) THEN
                  IF ( next==0 ) THEN
!  NO MORE FREE BLOCKS EXIST, SET IDBFRE TO ZERO
                     idbfre = 0
                  ELSE
!  NO PREVIOUS BLOCK, SET IDBFRE TO POINT TO NEW FIRST FREE BLOCK
                     idbfre = next
                     mem(next) = 0
                  ENDIF
               ELSEIF ( next==0 ) THEN
!  PREVIOUS BLOCK EXITS BUT BLOCK ALLOCATED WAS LAST IN CHAIN
                  mem(iprev+1) = 0
               ELSE
!  CONNECT THE PREVIOUS FREE BLOCK WITH THE NEXT FREE BLOCK
                  mem(iprev+1) = next
                  mem(next) = iprev
               ENDIF
               RETURN
            ENDIF
         ELSE
!  MEMORY NOT AVAILABLE IN THIS BLOCK, CHECK FOR OTHER BLOCKS
            next = mem(next+1)
!  IF NO MORE FREE BLOCKS, RETURN WITH INDEX SET TO -1
            IF ( next==0 ) EXIT SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
   Index = -1
END SUBROUTINE dbmalb
