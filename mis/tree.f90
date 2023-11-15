
 
SUBROUTINE tree(Iroot,Ndstk,Lvl,Iwk,Ndeg,Lvlwth,Lvlbot,Lvln,Maxlw,Ibort,Jwk)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Ibort , Iroot , Lvlbot , Lvln , Lvlwth , Maxlw
   INTEGER Iwk(1) , Jwk(1) , Lvl(1) , Ndeg(1) , Ndstk(1)
!
! Local variable declarations
!
   INTEGER inow , itest , itop , iwknow , j , lvltop , ndrow
!
! End of declarations
!
!
!     TREE DROPS A TREE IN NDSTK FROM IROOT
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     LVL-      ARRAY INDICATING AVAILABLE NODES IN NDSTK WITH ZERO
!               ENTRIES. TREE ENTERS LEVEL NUMBERS ASSIGNED
!               DURING EXECUTION OF OF THIS PROCEDURE
!     IWK-      ON OUTPUT CONTAINS NODE NUMBERS USED IN TREE
!               ARRANGED BY LEVELS (IWK(LVLN) CONTAINS IROOT
!               AND IWK(LVLBOT+LVLWTH-1) CONTAINS LAST NODE ENTERED)
!     JWK-      ON ONTPUT CONTAINS A ROW OF UNPACKED GRID NOS.
!               CURRENTLY, JWK AND RENUM SHARE SAME CORE SPACE
!     LVLWTH-   ON OUTPUT CONTAINS WIDTH OF LAST LEVEL
!     LVLBOT-   ON OUTPUT CONTAINS INDEX INTO IWK OF FIRST
!               NODE IN LAST LEVEL
!     MAXLW-    ON OUTPUT CONTAINS THE MAXIMUM LEVEL WIDTH
!     LVLN-     ON INPUT THE FIRST AVAILABLE LOCATION IN IWK
!               USUALLY ONE BUT IF IWK IS USED TO STORE PREVIOUS
!               CONNECTED COMPONENTS, LVLN IS NEXT AVAILABLE LOCATION.
!               ON OUTPUT THE TOTAL NUMBER OF LEVELS + 1
!     IBORT-    INPUT PARAM WHICH TRIGGERS EARLY RETURN IF
!               MAXLW BECOMES .GE. IBORT
!
!     INTEGER          BUNPK
!
   Maxlw = 0
   itop = Lvln
   inow = Lvln
   Lvlbot = Lvln
   lvltop = Lvln + 1
   Lvln = 1
   Lvl(Iroot) = 1
   Iwk(itop) = Iroot
 100  Lvln = Lvln + 1
   DO
      iwknow = Iwk(inow)
      ndrow = Ndeg(iwknow)
      CALL bunpak(Ndstk,iwknow,ndrow,Jwk)
      DO j = 1 , ndrow
         itest = Jwk(j)
         IF ( Lvl(itest)==0 ) THEN
            Lvl(itest) = Lvln
            itop = itop + 1
            Iwk(itop) = itest
         ENDIF
      ENDDO
      inow = inow + 1
      IF ( inow>=lvltop ) THEN
         Lvlwth = lvltop - Lvlbot
         IF ( Maxlw<Lvlwth ) Maxlw = Lvlwth
         IF ( Maxlw>=Ibort .OR. itop<lvltop ) RETURN
         Lvlbot = inow
         lvltop = itop + 1
         GOTO 100
      ENDIF
   ENDDO
END SUBROUTINE tree
