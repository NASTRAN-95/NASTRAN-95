!*==fnxt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fnxt(Ii,J)
   USE c_sof
   USE c_sofcom
   USE c_sys
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ii
   INTEGER :: J
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: filnum , filsup , i , iblock , index , l , max
   INTEGER , SAVE :: indsbr , ird , iwrt
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL chkopn , errmkn , sofio
!
! End of declarations rewritten by SPAG
!
!
!     FETCHES FROM THE RANDOM ACCESS STORAGE DEVICE THE BLOCK OF THE
!     ARRAY NXT CONTAINING THE ENTRY FOR BLOCK I.  IT STORES THE FETCHED
!     BLOCK IN THE ARRAY BUF, STARTING AT LOCATION NXT.  THE OUTPUT J
!     INDICATES THAT BLOCK I HAS THE JTH ENTRY IN THE ARRAY BUF.
!
   DATA ird , iwrt , indsbr/1 , 2 , 9/
   DATA nmsbr/4HFNXT , 4H    /
!
!     FILNUM IS THE NUMBER OF THE DEVICE TO WHICH BLOCK I BELONGS.
!
   CALL chkopn(nmsbr(1))
   index = Ii
   DO l = 1 , nfiles
      IF ( index>filsiz(l) ) THEN
         index = index - filsiz(l)
      ELSE
         filnum = l
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     ERROR MESSAGES.
!
   CALL errmkn(indsbr,1)
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     INDEX IS THE INDEX OF BLOCK I WITHIN FILE FILNUM.
!     FILSUP IS THE NUMBER OF THE SUPERBLOCK WITHIN FILE FILNUM TO WHICH
!     BLOCK I BELONGS, AND SUPSIZ IS THE SIZE OF A SUPERBLOCK.
!
      Filsup = (Index-1)/supsiz
      IF ( Index-1/=Filsup*supsiz ) Filsup = Filsup + 1
!
!     COMPUTE THE LOGICAL BLOCK NUMBER, WITHIN THE ARRAY NXT, IN WHICH
!     THE ITH BLOCK HAS AN ENTRY, ALSO COMPUTE THE INDEX OF THIS ENTRY
!     RELATIVE TO THE ARRAY BUF.  STORE THE BLOCK NUMBER IN IBLOCK, AND
!     THE INDEX IN J.
!
      Iblock = 0
      Max = Filnum - 1
      IF ( Max>=1 ) THEN
         DO I = 1 , Max
            Iblock = Iblock + nxtfsz(I)
         ENDDO
      ENDIF
      Iblock = Iblock + Filsup
      J = (Index-(Filsup-1)*supsiz)/2 + 1 + nxt
      IF ( Iblock==nxtlbn ) RETURN
      IF ( Iblock>nxttsz ) THEN
         CALL errmkn(Indsbr,1)
      ELSE
!
!     THE DESIRED NXT BLOCK IS NOT PRESENTLY IN CORE, MUST THEREFORE
!     FETCH IT.
!
         IF ( ditpbn/=0 ) THEN
!
!     THE IN CORE BLOCK SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY ONE BLOCK OF THE DIT.
!
            IF ( ditup ) THEN
!
!     THE DIT BLOCK NOW IN CORE HAS BEEN UPDATED.  MUST THEREFORE WRITE
!     IT OUT BEFORE READING IN THE DESIRED NXT BLOCK.
!
               CALL sofio(Iwrt,ditpbn,buf(dit-2))
               ditup = .FALSE.
            ENDIF
            ditpbn = 0
            ditlbn = 0
         ELSEIF ( nxtpbn/=0 ) THEN
!
!     THE IN CORE BLOCK SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY ONE BLOCK OF NXT.
!
            IF ( nxtup ) THEN
!
!     THE NEXT BLOCK CURRENTLY IN CORE HAS BEEN UPDATED.  MUST THEREFORE
!     WRITE IT OUT BEFORE READING IN A NEW BLOCK.
!
               CALL sofio(Iwrt,nxtpbn,buf(nxt-2))
               nxtup = .FALSE.
            ENDIF
         ENDIF
!
!     READ THE DESIRED NXT BLOCK INTO CORE.
!
         nxtlbn = Iblock
         nxtpbn = 0
         IF ( Max>=1 ) THEN
            DO I = 1 , Max
               nxtpbn = nxtpbn + filsiz(I)
            ENDDO
         ENDIF
         nxtpbn = nxtpbn + (Filsup-1)*supsiz + 2
         CALL sofio(Ird,nxtpbn,buf(nxt-2))
         RETURN
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE fnxt
