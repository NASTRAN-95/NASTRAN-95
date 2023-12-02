!*==khrfn3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION khrfn3(Word1,Word2,Move,Idir)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: khrfn3
   INTEGER , DIMENSION(1) :: Word1
   INTEGER , DIMENSION(1) :: Word2
   INTEGER :: Move
   INTEGER :: Idir
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iend , imove , ncpw , word3
   EXTERNAL khrfn1
!
! End of declarations rewritten by SPAG
!
!
!     CHARACTER FUNCTION KHRFN3 MERGES TWO WORDS, WORD1 AND WORD2, BY
!     BYTES
!
!     (+)MOVE IS NO. OF BYTES INVOLVED PRELIMINARY SHIFTING
!     (-)MOVE IS NO. OF BYTES IN MERGING, NO PRELIMINARY SHIFTING.
!     IDIR IS LEFT OR RIGHT SHIFT OF WORD2. THE VACANT BYTES ARE THEN
!     FILLED IN BY WORD1.  (LEFT SHIFT IF IDIR=1, RIGHT SHIFT OTHERWISE)
!
!     NOTE - KHRFN3 HANDLES ONLY 4 BYTES OF WORD. IF MACHINE WORD HAS
!     MORE THAN 4 BYTES PER WORD, KHRFN3 DOES NOT ZERO-FILL NOR BLANK-
!     FILL THE REST OF THE WORD. THE CALLER SHOULD MAKE THE PROPER
!     CHOICE BY ZERO-FILL OR BLANK-FILL THE INPUT WORDS, WORD1 ADN WORD2
!
!     THE FOLLOWING TABLE GIVES THE RESULTS OF KHRFN3 FOR VARIOUS INPUT
!     VALUES OF MOVE AND IDIR:
!
!        GIVEN:    WORD1=ABCD  AND  WORD2=1234  (IN BCD)
!                 IDIR=1   IDIR.NE.1            IDIR=1   IDIR.NE.1
!                --------------------          --------------------
!        MOVE= 0:   1234     1234      MOVE=-0:   1234     1234
!        MOVE= 1:   234D     A123      MOVE=-1:   123D     A234
!        MOVE= 2:   34CD     AB12      MOVE=-2:   12CD     AB34
!        MOVE= 3:   4BCD     ABC1      MOVE=-3:   1BCD     ABC4
!        MOVE= 4:   ABCD     ABCD      MOVE=-4:   ABCD     ABCD
!
!     THIS ROUTINE WAS WRITTEN BY G.CHAN TO REPLACE THE ORIGINAL VAX
!     ROUTINE WHICH WAS VERY VERY INEFFICIENT.
!
!
   ncpw = 4
   imove = iabs(Move)
   iend = ncpw - imove
   word3 = Word2(1)
   IF ( Move<0 ) THEN
      IF ( Idir==1 ) THEN
         DO i = 1 , imove
            word3 = khrfn1(word3,i+iend,Word1(1),i+iend)
         ENDDO
      ELSE
         DO i = 1 , imove
            word3 = khrfn1(word3,i,Word1(1),i)
         ENDDO
      ENDIF
   ELSEIF ( Move/=0 ) THEN
      word3 = Word1(1)
      IF ( imove<ncpw ) THEN
         IF ( Idir==1 ) THEN
            DO i = 1 , iend
               word3 = khrfn1(word3,i,Word2(1),i+imove)
            ENDDO
         ELSE
            DO i = 1 , iend
               word3 = khrfn1(word3,i+imove,Word2(1),i)
            ENDDO
         ENDIF
      ENDIF
   ENDIF
   khrfn3 = word3
END FUNCTION khrfn3
