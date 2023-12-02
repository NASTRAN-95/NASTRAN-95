!*==dbmint.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dbmint
   IMPLICIT NONE
   USE I_DSIOF
   USE C_SYSTEM
   USE C_ZZZZZZ
!********************************************************************
!     DBMINT - INITIALIZES ALL PARAMETERS AND THE FREE BLOCK CHAIN
!              FOR THE IN-MEMORY DATA BASE.
!
!        ARGUMENTS
!            IDBADR - (INPUT)-BEGINNING ADDRESS FOR IN-MEMORY DATA BASE
!            IDBLEN - (INPUT)-NUMBER OF MEMORY WORDS FOR IN-MEMORY
!                             DATA BASE
!        / DBMPAR/
!            IDBBAS - (OUTPUT)-INDEX TO IN-MEMORY DATA BASE RELATIVE
!                              TO /DBM/
!            IDBFRE - (OUTPUT)-INDEX TO FREE CHAIN OF IN-MEMORY DATA
!                              BASE RELATIVE TO /DBM/
!            IDBDIR - (OUTPUT)-INDEX TO FIRST DIRECTORY BLOCK
!        FREE CHAIN FORMAT
!               IDBFRE==> WORD 0 - 0   (POINTS TO PREVIOUS FREE BLOCK
!                                      IN CHAIN, ALWAYS 0 FOR 1ST BLK)
!                         WORD 1 - 0   (POINTS TO NEXT BLOCK IN CHAIN
!                                      -INITIALLY SET TO ZERO)
!                         WORD 2 - L   (NUMBER OF FREE WORDS IN BLOCK)
!        DIRECTORY FORMAT
!               THE FIRST TWO WORDS OF THE DIRECTORY BLOCK CONTAIN:
!                   WORD  0 - MAXIMUM NUMBER OF ENTRIES IN DIRECTORY
!                   WORD  1 - CURRENT ENTRIES IN THE DIRECTORY
!               EACH ENTRY IN THE DIRECTORY HAS THE FOLLOWING FORMAT
!               (NOTE, FIRST ENTRY BEGINS AT WORD 3 OF BLOCK)
!                   WORD  0 - UNIT NUMBER OF DMAP FILE AS FOUND IN FIAT
!                   WORD  1 - INDEX TO FIRST IN-MEMORY DATA BLOCK
!                   WORD  2 - INDEX TO LAST IN-MEMORY DATA BLOCK
!                   WORD  3 - INDEX TO CURRENT IN-MEMORY DATA BLOCK
!                   WORD  4 - CURRENT BLOCK NUMBER BEING PROCESSED
!                   WORD  5 - LAST BLOCK NUMBER
!                   WORD  6 - ORIGINAL BUFFER ADDRESS
!                   WORD  7 - TOTAL BLOCKS (EXT. FILE + IN M. DB)
!                   WORD  8 - OPEN FLAG FOR EXT. FILE (0,NO;1,YES)
!               WORDS  9-10 - DMAP FILE NAME
!               WORDS 11-16 - DMAP FILE TRAILER
!********************************************************************
   idbdir = 0
   IF ( idblen/=0 ) THEN
!  INITIALIZE THE CHAIN OF FREE BLOCKS AS ONE BIG FREE BLOCK
      idbbas = locfx(Mem)
      idbfre = idbadr - idbbas + 1
      Mem(idbfre) = 0
      Mem(idbfre+1) = 0
      Mem(idbfre+2) = idblen - 2
      maxalc = idblen/(Isysbf-3+4)
      idbdir = 1
   ENDIF
END SUBROUTINE dbmint
