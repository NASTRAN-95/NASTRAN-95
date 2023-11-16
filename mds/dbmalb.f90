
SUBROUTINE dbmalb(Lenreq,Index)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'ZZZZZZ.COM'
!
! Dummy argument declarations
!
   INTEGER Index , Lenreq
!
! Local variable declarations
!
   INTEGER iprev , lenavl , newind , next
!
! End of declarations
!
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
   next = Idbfre
   IF ( Idbfre/=0 ) THEN
      DO
!  OBTAIN THE LENGTH OF THE FREE BLOCK
         lenavl = Mem(next+2)
         IF ( lenavl>=Lenreq ) THEN
!  RETURN POINTER FOR THIS BLOCK
            Index = next
            IF ( Lenreq/=lenavl ) THEN
!  COME HERE WHEN FREE BLOCK HAS MORE SPACE THEN REQUESTED
               newind = Index + Lenreq + 4
               iprev = Mem(Index)
               next = Mem(Index+1)
!  CHECK TO DETERMINE IF ANY SPACE REMAINS
               IF ( (lenavl-Lenreq-4)<=0 ) THEN
!  FREE CHAIN IS EXHAUSTED
                  Idbfre = 0
                  EXIT
               ELSE
!  RECOMPUTE FREE SPACE AND SET UP CHAIN WORDS
                  Mem(newind+2) = lenavl - Lenreq - 4
                  IF ( iprev==0 ) THEN
                     IF ( next==0 ) THEN
!  NEW BLOCK IS THE ONLY FREE BLOCK
                        Idbfre = newind
                        Mem(newind) = 0
                        Mem(newind+1) = 0
                     ELSE
!  NO PREVIOUS BLOCK, NEWLY CREATED BLOCK BECOMES THE FIRST FREE BLOCK
                        Idbfre = newind
                        Mem(newind) = 0
                        Mem(newind+1) = next
                        Mem(next) = newind
                     ENDIF
                  ELSEIF ( next==0 ) THEN
!  PREVIOUS BLOCK EXISTS BUT THE NEWLY CREATED BLOCK IS LAST
                     Mem(iprev+1) = newind
                     Mem(newind) = iprev
                     Mem(newind+1) = 0
                  ELSE
!  CONNECT TO PREVIOUS AND NEXT FREE BLOCK
                     Mem(newind) = iprev
                     Mem(newind+1) = next
                     Mem(iprev+1) = newind
                     Mem(next) = newind
                  ENDIF
                  GOTO 99999
               ENDIF
            ELSE
!  COME HERE WHEN REQUESTED BLOCK SAME SIZE AS FREE BLOCK
               next = Mem(next+1)
               iprev = Mem(Index)
               IF ( iprev==0 ) THEN
                  IF ( next==0 ) THEN
!  NO MORE FREE BLOCKS EXIST, SET IDBFRE TO ZERO
                     Idbfre = 0
                  ELSE
!  NO PREVIOUS BLOCK, SET IDBFRE TO POINT TO NEW FIRST FREE BLOCK
                     Idbfre = next
                     Mem(next) = 0
                  ENDIF
               ELSEIF ( next==0 ) THEN
!  PREVIOUS BLOCK EXITS BUT BLOCK ALLOCATED WAS LAST IN CHAIN
                  Mem(iprev+1) = 0
               ELSE
!  CONNECT THE PREVIOUS FREE BLOCK WITH THE NEXT FREE BLOCK
                  Mem(iprev+1) = next
                  Mem(next) = iprev
               ENDIF
               GOTO 99999
            ENDIF
         ELSE
!  MEMORY NOT AVAILABLE IN THIS BLOCK, CHECK FOR OTHER BLOCKS
            next = Mem(next+1)
!  IF NO MORE FREE BLOCKS, RETURN WITH INDEX SET TO -1
            IF ( next==0 ) EXIT
         ENDIF
      ENDDO
   ENDIF
   Index = -1
   RETURN
99999 RETURN
END SUBROUTINE dbmalb
