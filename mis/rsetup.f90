
SUBROUTINE rsetup(Lvl,Lvls1,Lvls2,Nacum,Idim)
   IMPLICIT NONE
   REAL Dum3b(3)
   INTEGER Idpth , N , Ngrid
   COMMON /bandb / Dum3b , Ngrid
   COMMON /bandg / N , Idpth
   INTEGER Idim
   INTEGER Lvl(1) , Lvls1(1) , Lvls2(1) , Nacum(1)
   INTEGER i , itemp
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     SETUP COMPUTES THE REVERSE LEVELING INFO FROM LVLS2 AND STORES
!     IT INTO LVLS2.  NACUM(I) IS INITIALIZED TO NODES/ITH LEVEL FOR
!     NODES ON THE PSEUDO-DIAMETER OF THE GRAPH.  LVL IS INITIALIZED TO
!     NON-ZERO FOR NODES ON THE PSEUDO-DIAM AND NODES IN A DIFFERENT
!     COMPONENT OF THE GRAPH.
!
!
!     IDIM=NUMBER OF LEVELS IN A GIVEN COMPONENT.
!     NACUM IS DIMENSIONED TO IDIM IN SIZE
!
!     DIMENSION EXCEEDED  . . .  STOP JOB.
!
   IF ( Idpth<=Idim ) THEN
!
      DO i = 1 , Idpth
         Nacum(i) = 0
      ENDDO
      DO i = 1 , N
         Lvl(i) = 1
         Lvls2(i) = Idpth + 1 - Lvls2(i)
         itemp = Lvls2(i)
         IF ( itemp<=Idpth ) THEN
            IF ( itemp/=Lvls1(i) ) THEN
               Lvl(i) = 0
            ELSE
               Nacum(itemp) = Nacum(itemp) + 1
            ENDIF
         ENDIF
      ENDDO
      GOTO 99999
   ENDIF
   Ngrid = -3
   RETURN
99999 RETURN
END SUBROUTINE rsetup