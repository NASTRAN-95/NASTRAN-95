!*==rdmodx.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rdmodx(File,Mode,Word)
   USE c_xrdmod
   USE c_zzzzzz
   USE C_XRDMOD
   USE C_ZZZZZZ
   IMPLICIT NONE
   INTEGER Bitson , Check1 , Check2 , Entry , Filex , Refptr , Z(1)
   COMMON /xrdmod/ Filex , Refptr , Check1 , Check2 , Bitson , Entry
   COMMON /zzzzzz/ Z
   INTEGER File
   INTEGER A(1) , Mode(1) , Word(2)
   INTEGER blank , eor , i , name(2) , next(2)
   INTEGER complf , locfx
!
!     ENTRY POINTS - RDMODX (FILE ,MODE,WORD)
!                    RDMODY (A    ,MODE,WORD)
!                    RDMODE (*,*,*,MODE,WORD)
!                    RDWORD (      MODE,WORD)
!     RDMODX, RDMODE AND RDWORD CALLED BY PLOT, FIND, PARAM AND SETINP
!     RDMODY CALLED ONLY BY PLOT
!
!     REVISED 10/10/92 BY G.CHAN/UNISYS
!     THE ORIGINAL WAY PASSING 'FILE' AND ARRAY 'A' FROM RDMODX AND
!     RDMODY ARE NOT ANSI FORTRAN77 STANDARD. THERE IS NO GUARANTY THAT
!     RDMODE AND RDWORD WILL PICK THEM UP CORRECTLY. MODIFICATIONS HERE
!     ARE (1) SAVE 'FILE' IN /XRDMOD/, AND (2) COMPUTE A REFERENCE
!     POINTER, REFPTR, SUCH THAT ARRAY A IS ACCESSIBLE VIA ARRAY Z
!
   DATA blank , eor , name/1H  , 1000000 , 4HRDMO , 4HDX  /
!
!     -RDMODX- IS CALLED IF -MODE- IS TO BE READ FROM DATA SET -FILE-
!
   Entry = 0
   Filex = File
   Check1 = 13579
   CALL spag_block_1
   RETURN
!
!
   ENTRY rdmody(A,Mode,Word)
!     ==========================
!
!     -RDMODY- IS CALLED IF -MODE- IS TO BE READ FROM THE -A- ARRAY
!
!     COMPUTE THE REFERENCE POINTER FROM Z(1) TO A(1), AND NEXT TIME
!     WHEN A ARRAY IS USED, USE Z ARRAY WITH THE REFERENCE POINTER
!
   Entry = 1
   Refptr = locfx(A(1)) - locfx(Z(1))
   Check2 = 24680
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      USE C_XRDMOD
      USE C_ZZZZZZ
      Bitson = complf(0)
      RETURN
!
!
      ENTRY rdmode(*,*,*,Mode,Word)
!     ==============================
!
!     -RDMODE- IS CALLED TO READ -MODE-
!     IF MODE = -4, THE NEXT TWO WORDS ARE READ INTO -WORD-
!     IF MODE IS NEGATIVE AND NOT = -4, ONLY THE NEXT ONE WORD IS READ
!     INTO -WORD-
!     RETURN 1 - NUMERIC MODE (-MODE- NEGATIVE)
!                -MODE- = -1, -WORD- IS INTEGER
!                -MODE- = -2, -WORD- IS REAL NUMBER
!                -MODE- = -3, -WORD- IS ZERO ?
!                -MODE- = -4, -WORD- IS D.P.REAL
!     RETURN 2 - ALPHABETIC MODE (-MODE- POSITIVE)
!     RETURN 3 - END OF LOGICAL CARD (RECORD TERMINATED),
!                -MODE- = 1000000
!
      IF ( Entry/=0 ) THEN
         DO
!
            IF ( Check2/=24680 ) CALL mesage(-37,0,Name)
            mode(1) = Z(Entry+Refptr)
            Entry = Entry + 1
            IF ( mode(1)<0 ) THEN
!
               word(1) = Z(Entry+Refptr)
               Entry = Entry + 1
               IF ( mode(1)/=-4 ) RETURN 1
               word(2) = Z(Entry+Refptr)
               Entry = Entry + 1
               RETURN 1
            ELSEIF ( mode(1)/=0 ) THEN
               IF ( mode(1)>=Eor ) THEN
                  Entry = Entry + 1
                  RETURN 3
               ELSE
                  SPAG_Loop_2_1: DO
                     Next(1) = Z(Entry+0+Refptr)
                     Next(2) = Z(Entry+1+Refptr)
                     Entry = Entry + 2
                     IF ( Next(1)/=Bitson .AND. Next(1)/=Blank ) RETURN 2
                     mode(1) = mode(1) - 1
                     IF ( mode(1)<=0 ) EXIT SPAG_Loop_2_1
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ENDIF
         ENDDO
      ELSE
         IF ( Check1/=13579 ) CALL mesage(-37,0,Name)
         DO
!
            CALL fread(Filex,mode,1,0)
            IF ( mode(1)<0 ) THEN
!
               I = 1
               IF ( mode(1)==-4 ) I = 2
               CALL fread(Filex,word,I,0)
               RETURN 1
            ELSEIF ( mode(1)==0 ) THEN
               CALL fread(Filex,0,0,1)
            ELSEIF ( mode(1)>=Eor ) THEN
               CALL fread(Filex,0,0,1)
               RETURN 3
            ELSE
               SPAG_Loop_2_2: DO
                  CALL fread(Filex,Next,2,0)
                  IF ( Next(1)/=Bitson .AND. Next(1)/=Blank ) RETURN 2
                  mode(1) = mode(1) - 1
                  IF ( mode(1)<=0 ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO
      ENDIF
!
!
      ENTRY rdword(mode,word)
!     ========================
!
!     -RDWORD- IS CALLED TO READ TWO BCD WORDS INTO -WORD-
!     NOTE - ALL DATA FIELD DELIMITERS ARE SKIPPED
!
      word(1) = Next(1)
      word(2) = Next(2)
      SPAG_Loop_1_3: DO
         mode(1) = mode(1) - 1
         IF ( mode(1)<=0 ) EXIT SPAG_Loop_1_3
         IF ( Entry/=0 ) THEN
!
            IF ( Check2/=24680 ) CALL mesage(-37,0,Name)
            Next(1) = Z(Entry+Refptr)
            Next(2) = Z(Entry+1+Refptr)
            Entry = Entry + 2
         ELSE
            IF ( Check1/=13579 ) CALL mesage(-37,0,Name)
            CALL fread(Filex,Next,2,0)
         ENDIF
         IF ( Next(1)/=Bitson .AND. Next(1)/=Blank ) EXIT SPAG_Loop_1_3
      ENDDO SPAG_Loop_1_3
   END SUBROUTINE spag_block_1
END SUBROUTINE rdmodx
