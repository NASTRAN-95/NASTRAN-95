!*==fndiam.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fndiam(Snd1,Snd2,Ndstk,Ndeg,Lvl,Lvls1,Lvls2,Iwk,Idflt,Ndlst,Jwk,Idim)
   IMPLICIT NONE
   USE C_BANDB
   USE C_BANDG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Idim
   INTEGER :: Snd1
   INTEGER :: Snd2
   INTEGER , DIMENSION(1) :: Ndstk
   INTEGER , DIMENSION(1) :: Ndeg
   INTEGER , DIMENSION(1) :: Lvl
   INTEGER , DIMENSION(1) :: Lvls1
   INTEGER , DIMENSION(1) :: Lvls2
   INTEGER , DIMENSION(1) :: Iwk
   INTEGER :: Idflt
   INTEGER , DIMENSION(Idim) :: Ndlst
   INTEGER , DIMENSION(1) :: Jwk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: flag , i , lvlbot , lvln , lvlwth , maxlw , mtw1 , mtw2 , ndxl , ndxn , snd
   EXTERNAL sortdg , tree
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     FNDIAM IS THE CONTROL PROCEDURE FOR FINDING THE PSEUDO-DIAMETER
!     OF NDSTK AS WELL AS THE LEVEL STRUCTURE FROM EACH END
!
!     SND1-     ON INPUT THIS IS THE NODE NUMBER OF THE FIRST
!               ATTEMPT AT FINDING A DIAMETER.  ON OUTPUT IT
!               CONTAINS THE ACTUAL NUMBER USED.
!     SND2-     ON OUTPUT CONTAINS OTHER END OF DIAMETER
!     LVLS1-    ARRAY CONTAINING LEVEL STRUCTURE WITH SND1 AS ROOT
!     LVLS2-    ARRAY CONTAINING LEVEL STRUCTURE WITH SND2 AS ROOT
!     IDFLT-    FLAG USED IN PICKING FINAL LEVEL STRUCTURE, SET =1
!               IF WIDTH OF LVLS1 .GE. WIDTH OF LVLS2, OTHERWISE =2
!     LVL,IWK-  WORKING STORAGE
!     JWK-      WORKING STORAGE, CURRENTLY SHARING SAME SPACE WITH RENUM
!     DIMENSION OF NDLST IS THE MAX NUMBER OF NODES IN LAST LEVEL.
!
!
         flag = 0
         mtw2 = N
         snd = Snd1
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
!
!     ZERO LVL TO INDICATE ALL NODES ARE AVAILABLE TO TREE
!
            DO i = 1 , N
               Lvl(i) = 0
            ENDDO
            lvln = 1
!
!     DROP A TREE FROM SND
!
            CALL tree(snd,Ndstk,Lvl,Iwk,Ndeg,lvlwth,lvlbot,lvln,maxlw,mtw2,Jwk)
            IF ( flag<1 ) THEN
               flag = 1
               EXIT SPAG_Loop_1_1
            ELSEIF ( Idpth>=lvln-1 ) THEN
               IF ( maxlw<mtw2 ) THEN
                  mtw2 = maxlw
                  Snd2 = snd
!
!     STORE NARROWEST REVERSE LEVEL STRUCTURE IN LVLS2
!
                  DO i = 1 , N
                     Lvls2(i) = Lvl(i)
                  ENDDO
               ENDIF
               IF ( ndxn==ndxl ) THEN
                  Idflt = 1
                  IF ( mtw2<=mtw1 ) Idflt = 2
                  IF ( Idpth>Idim ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  RETURN
               ELSE
!
!     TRY NEXT NODE IN NDLST
!
                  ndxn = ndxn + 1
                  snd = Ndlst(ndxn)
               ENDIF
            ELSE
!
!     START AGAIN WITH NEW STARTING NODE
!
               Snd1 = snd
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         Idpth = lvln - 1
         mtw1 = maxlw
!
!     COPY LEVEL STRUCTURE INTO LVLS1
!
         DO i = 1 , N
            Lvls1(i) = Lvl(i)
         ENDDO
         ndxn = 1
         ndxl = 0
         mtw2 = N
!
!     SORT LAST LEVEL BY DEGREE  AND STORE IN NDLST
!
         CALL sortdg(Ndlst,Iwk(lvlbot),ndxl,lvlwth,Ndeg)
         IF ( ndxl<=Idim ) THEN
!
            snd = Ndlst(1)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     DIMENSION EXCEEDED  . . .  STOP JOB.
!
         Ngrid = -3
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fndiam
