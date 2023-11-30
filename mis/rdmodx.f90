
SUBROUTINE rdmodx(File,Mode,Word)
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
   GOTO 100
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
 100  Bitson = complf(0)
   RETURN
!
!
   ENTRY rdmode(Mode,Word) !HIDESTARS (*,*,*,Mode,Word)
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
         IF ( Check2/=24680 ) CALL mesage(-37,0,name)
         Mode(1) = Z(Entry+Refptr)
         Entry = Entry + 1
         IF ( Mode(1)<0 ) THEN
!
            Word(1) = Z(Entry+Refptr)
            Entry = Entry + 1
            IF ( Mode(1)/=-4 ) RETURN 1
            Word(2) = Z(Entry+Refptr)
            Entry = Entry + 1
            RETURN 1
         ELSEIF ( Mode(1)/=0 ) THEN
            IF ( Mode(1)>=eor ) THEN
               Entry = Entry + 1
               RETURN 3
            ELSE
               DO
                  next(1) = Z(Entry+0+Refptr)
                  next(2) = Z(Entry+1+Refptr)
                  Entry = Entry + 2
                  IF ( next(1)/=Bitson .AND. next(1)/=blank ) RETURN 2
                  Mode(1) = Mode(1) - 1
                  IF ( Mode(1)<=0 ) EXIT
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ELSE
      IF ( Check1/=13579 ) CALL mesage(-37,0,name)
      DO
!
         CALL fread(Filex,Mode,1,0)
         IF ( Mode(1)<0 ) THEN
!
            i = 1
            IF ( Mode(1)==-4 ) i = 2
            CALL fread(Filex,Word,i,0)
            RETURN 1
         ELSEIF ( Mode(1)==0 ) THEN
            CALL fread(Filex,0,0,1)
         ELSEIF ( Mode(1)>=eor ) THEN
            CALL fread(Filex,0,0,1)
            RETURN 3
         ELSE
            DO
               CALL fread(Filex,next,2,0)
               IF ( next(1)/=Bitson .AND. next(1)/=blank ) RETURN 2
               Mode(1) = Mode(1) - 1
               IF ( Mode(1)<=0 ) EXIT
            ENDDO
         ENDIF
      ENDDO
   ENDIF
!
!
   ENTRY rdword(Mode,Word)
!     ========================
!
!     -RDWORD- IS CALLED TO READ TWO BCD WORDS INTO -WORD-
!     NOTE - ALL DATA FIELD DELIMITERS ARE SKIPPED
!
   Word(1) = next(1)
   Word(2) = next(2)
   DO
      Mode(1) = Mode(1) - 1
      IF ( Mode(1)<=0 ) EXIT
      IF ( Entry/=0 ) THEN
!
         IF ( Check2/=24680 ) CALL mesage(-37,0,name)
         next(1) = Z(Entry+Refptr)
         next(2) = Z(Entry+1+Refptr)
         Entry = Entry + 2
      ELSE
         IF ( Check1/=13579 ) CALL mesage(-37,0,name)
         CALL fread(Filex,next,2,0)
      ENDIF
      IF ( next(1)/=Bitson .AND. next(1)/=blank ) EXIT
   ENDDO
END SUBROUTINE rdmodx