!*==/home/marcusmae/nasa/nastran/SPAGged/C_CLSTRS.f90  created by SPAG 8.01RF at 14:46 on  2 Dec 2023
MODULE C_CLSTRS
!
!GPTABD
!     BLOCK DATA PROGRAM FOR ALL MODULES HAVING ANYTHING TO DO WITH THE
!     NASTRAN STRUCTURAL ELEMENTS.
!
!     NOTE. ALL MODULES SHOULD BE WRITTEN TO TAKE ADVANTAGE OF THE
!     FLEXIBLE NATURE OF THIS DATA.
!
!     THE ELEMENTS OF NASTRAN ARE ALL REPRESENTED BELOW.  THEY ARE
!     ARRANGED BY ELEMENT TYPE NUMBER.  EACH ELEMENT ENTRY BELOW
!     CONTAINS -INCR- NUMBER OF VALUES.  -INCR- AT SOME FUTURE DATE MAY
!     GROW LARGER THUS MODULE WRITERS SHOULD ALWAYS INCLUDE -INCR- WHEN
!     COMPUTING INDEXES INTO THIS DATA.
!
!     -NELEM- IS SIMPLY THE CURRENT NUMBER OF ELEMENTS IN NASTRAN.
!
!     -LAST- IS SIMPLY THE NUMBER OF THE FIRST WORD OF THE LAST ELEMENT
!     ENTRY SUCH THAT DO LOOPS MAY HAVE THE FOLLOWING FORM.
!
!              DO 100 I = 1,LAST,INCR
!                     . . .
!                     . . .
!                     . . .
!          100 CONTINUE
!
!     THUS IN THE ABOVE LOOP E(I) POINTS TO THE FIRST WORD OF OF AN
!     ELEMENT ENTRY.
!
!     TERMS OF EACH ELEMENT ENTRY.
!     ============================
!      1. AND 2.  = ELEMENT NAME STORED 2A4
!      3. ELEMENT TYPE NUMBER
!      4. AND 5. ELEMENT-CONNECTION-TABLE RECORD ID AND BIT NUMBER
!      6. NUMBER OF ELEMENT CONNECTION TABLE WORDS FOR THIS ELEMENT
!      7. 8. AND 9. SAME AS 4. 5. AND 6. BUT FOR ELEMENT PROPERTY TABLE
!     10. NUMBER OF GRID POINTS FOR THIS ELEMENT
!     11. SCALAR
!     12. NUMBER OF WORDS IN THE ELEMENT-SUMMARY-TABLE FOR THIS ELEMENT
!     13. POSITION IN ECT OF FIRST GRID POINT
!     14. AND 15. TEMPERATURE TYPE AND COUNT AS USED BY THE SSG MODULE
!     16. TWO LETTER SYMBOL FOR PLOTTING.  ELEMENT WILL BE PLOTTED IF
!         WORD 10 IS 2 TO 42 AND WORD 11 IS ZERO AND WORD 16 .NE. 2HXX
!     17. NUMBER OF ESTA WORDS SDR2 WILL PICK UP FROM PHASE-1 ELEMENT
!         ROUTINES AND PASS TO THE PHASE-2 ELEMENT ROUTINES
!     18. AND 19. THE REAL STRESS WORD AND FORCE WORD COUNTS FOR OUT-
!         PUTS FROM THE SDR2 PHASE-2 ROUTINES TO AN OUTPUT FILE FOR OFP
!     20. AND 21. COMPLEX STRESS AND FORCE POINTERS FOR ORDERING OF
!         COMPLEX STRESS AND FORCE OUTPUTS TO A FILE FOR OFP PROCESSING
!     22. 23. AND 24. SMA1, SMA2, AND DS1 ELEMENT OVERLAY TREE POSITION
!     25. MAXIMUM DEGREES OF FREEDOM DEFINED FOR ELEMENT
!
   INTEGER, DIMENSION(530) :: Complx
!
   INTEGER , DIMENSION(100) :: comp1 , comp2 , comp3 , comp4 , comp5
   INTEGER , DIMENSION(30) :: comp6
!
   EQUIVALENCE (Complx(1),Comp1(1)) , (Complx(101),Comp2(1)) , (Complx(201),Comp3(1)) , (Complx(301),Comp4(1)) ,                    &
    & (Complx(401),Comp5(1)) , (Complx(501),Comp6(1))
!
!     COMPLX DESCRIBES THE MANNER IN WHICH THE TWO PARTS OF COMPLEX
!     STRESSES AND FORCES ARE RELATED TO EACH OTHER.  ANY ELEMENT WHICH
!     HAS COMPLEX STRESS OR FORCE OUTPUT POINTS TO  A STRING WITH WORDS
!     20 AND 21 OF ITS ELEMENT ENTRY.  THE COMPLX TABLE IS USED IN SDR2
!     AND DDRMM, WHICH ARE IN LINKS 13 AND 12 RESPECTIVELY
!
!     EACH STRING IS DEFINDED AS FOLLOWS
!      0  TERMINATES THE FORMAT STRING
!     -N  PUT INTO PRINT BUFFER THE REAL PART OF PAIR
!     +N  (AND  N.LE.I)  PUT BOTH REAL AND IMAGINARY PARTS INTO BUFFER
!     +N  (AND  N.GT.I)  PUT IMAGINARY PART ONLY INTO PRINT BUFFER
!         WHERE    I  =  THE LENGTH OF STRING IN WORD 18 (REAL-STRESS)
!                        OR 19 (REAL-FORCE) PLUS 1
!
   DATA comp1/1 , -2 , -3 , -4 , -5 , -6 , 8 , 9 , 10 , 11 , 12 , 0 , 1 , -2 , 5 , -3 , 6 , 0 , 1 , -2 , 4 , 0 , 1 , -2 , -3 , -4 , &
      & -5 , -6 , -7 , -8 , -9 , 11 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 0 , 1 , -2 , 7 , -4 , 9 , 0 , 1 , -2 , -3 , 6 , 7 , 0 , 1 ,&
      & -2 , -3 , -4 , -5 , -6 , 18 , 19 , 20 , 21 , 22 , -10 , -11 , -12 , -13 , 26 , 27 , 28 , 29 , 0 , 1 , 2 , -3 , 20 , -4 ,    &
      & 21 , -5 , 22 , 10 , -11 , 28 , -12 , 29 , -13 , 30 , 0 , 1 , -2 , -3 , -4 , 10 , 11 , 12 , 0 , 1 , -2 , -3 , -4/
   DATA comp2/ - 5 , -6 , -7 , 11 , 12 , 13 , 14 , 15 , 16 , 0 , 1 , -2 , -3 , -4 , -5 , 7 , 8 , 9 , 10 , 0 , 0 , 1 , -2 , -3 , -4 ,&
      & -5 , -6 , -7 , -8 , -9 , -10 , 12 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 20 , 0 , 1 , -2 , -3 , -4 , -5 , -6 , -7 , 9 , 10 ,  &
      & 11 , 12 , 13 , 14 , 0 , 1 , -2 , -3 , -4 , -5 , -6 , -7 , -8 , -9 , 19 , 20 , 21 , 22 , 23 , 24 , 25 , 26 , -10 , -11 ,     &
      & -12 , -13 , -14 , -15 , -16 , -17 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 34 , 0 , 0 , 1 , 2 , -3 , -11 , -17 , -4 , -12 ,     &
      & -18 , 25 , 33/
   DATA comp3/39 , 26 , 34 , 40 , 0 , 1 , 2 , -3 , -12 , -18 , -4 , -13 , -19 , 11 , 26 , 35 , 41 , 27 , 36 , 42 , 0 , 0 , 0 , 0 ,  &
      & 0 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , -3 , -4 , -5 , -6 , -7 , -8 , -9 , -10 , -11 , 14 , 15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , &
      & 0 , 1 , 2 , -3 , -4 , -5 , -6 , 17 , 18 , 19 , 20 , -7 , -8 , -9 , -10 , 21 , 22 , 23 , 24 , -11 , -12 , -13 , -14 , 25 ,   &
      & 26 , 27 , 28 , 0 , 1 , 2 , -3 , -4 , -5 , -6 , -7 , -8 , -9 , -10 , -11 , 50 , 51 , 52 , 53 , 54 , 55 , 56 , 57 , 58 , -12 ,&
      & -13/
   DATA comp4/ - 14 , -15 , -16 , -17 , -18 , -19 , -20 , 59 , 60 , 61 , 62 , 63 , 64 , 65 , 66 , 67 , -21 , -22 , -23 , -24 , -25 ,&
      & -26 , -27 , -28 , -29 , 68 , 69 , 70 , 71 , 72 , 73 , 74 , 75 , 76 , -30 , -31 , -32 , -33 , -34 , -35 , -36 , -37 , -38 ,  &
      & 77 , 78 , 79 , 80 , 81 , 82 , 83 , 84 , 85 , -39 , -40 , -41 , -42 , -43 , -44 , -45 , -46 , -47 , 86 , 87 , 88 , 89 , 90 , &
      & 91 , 92 , 93 , 94 , 0 , 1 , 2 , -3 , -4 , -5 , -6 , 21 , 22 , 23 , 24 , -7 , -8 , -9 , -10 , 25 , 26 , 27 , 28 , -11 , -12 ,&
      & -13 , -14 , 29 , 30 , 31 , 32 , -15 , -16 , -17/
   DATA comp5/ - 18 , 33 , 34 , 35 , 36 , 0 , 0 , 0 , 0 , 0 , 1 , 2 , 3 , 4 , 5 , -6 , 49 , -7 , 50 , -8 , 51 , 9 , 10 , -11 , 54 , &
      & -12 , 55 , -13 , 56 , 14 , 15 , -16 , 59 , -17 , 60 , -18 , 61 , 19 , 20 , -21 , 64 , -22 , 65 , -23 , 66 , 24 , 25 , -26 , &
      & 69 , -27 , 70 , -28 , 71 , 29 , 30 , -31 , 74 , -32 , 75 , -33 , 76 , 34 , 35 , -36 , 79 , -37 , 80 , -38 , 81 , 39 , 40 ,  &
      & -41 , 84 , -42 , 85 , -43 , 86 , 0 , 0 , 0 , 1 , -2 , -3 , -4 , -5 , -6 , -7 , 14 , 15 , 16 , 17 , 18 , 19 , -8 , -9 , -10 ,&
      & -11 , -12 , 20 , 21/
   DATA comp6/22 , 23 , 24 , 0 , 1 , -2 , -3 , -4 , -5 , -6 , 19 , 20 , 21 , 22 , 23 , -10 , -11 , -12 , -13 , -14 , 27 , 28 , 29 , &
      & 30 , 31 , 0 , 0 , 0 , 0 , 0/

END MODULE C_CLSTRS
