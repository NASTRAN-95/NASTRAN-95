!*==tree.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE tree(Iroot,Ndstk,Lvl,Iwk,Ndeg,Lvlwth,Lvlbot,Lvln,Maxlw,Ibort,Jwk)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iroot
   INTEGER , DIMENSION(1) :: Ndstk
   INTEGER , DIMENSION(1) :: Lvl
   INTEGER , DIMENSION(1) :: Iwk
   INTEGER , DIMENSION(1) :: Ndeg
   INTEGER :: Lvlwth
   INTEGER :: Lvlbot
   INTEGER :: Lvln
   INTEGER :: Maxlw
   INTEGER :: Ibort
   INTEGER , DIMENSION(1) :: Jwk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: inow , itest , itop , iwknow , j , lvltop , ndrow
   EXTERNAL bunpak
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
   SPAG_Loop_1_1: DO
      Lvln = Lvln + 1
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
            CYCLE SPAG_Loop_1_1
         ENDIF
      ENDDO
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
END SUBROUTINE tree
