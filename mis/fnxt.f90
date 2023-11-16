
SUBROUTINE fnxt(Ii,J)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Blksiz , Buf(1) , Dirsiz , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Filsiz(10) , Iodum(8) , Mdidum(4) , Mdiup , &
         & Nfiles , Nxt , Nxtcur , Nxtfsz(10) , Nxtlbn , Nxtpbn , Nxtrst , Nxttsz , Supsiz
   LOGICAL Ditup , Nxtup
   REAL Filnam(10)
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdidum , Nxt , Nxtpbn , Nxtlbn , Nxttsz , Nxtfsz ,     &
                 & Nxtcur , Ditup , Mdiup , Nxtup , Nxtrst
   COMMON /sofcom/ Nfiles , Filnam , Filsiz
   COMMON /sys   / Blksiz , Dirsiz , Supsiz
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Ii , J
!
! Local variable declarations
!
   INTEGER filnum , filsup , i , iblock , index , indsbr , ird , iwrt , l , max , nmsbr(2)
!
! End of declarations
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
   DO l = 1 , Nfiles
      IF ( index>Filsiz(l) ) THEN
         index = index - Filsiz(l)
      ELSE
         filnum = l
         GOTO 100
      ENDIF
   ENDDO
!
!     ERROR MESSAGES.
!
   CALL errmkn(indsbr,1)
   GOTO 99999
!
!     INDEX IS THE INDEX OF BLOCK I WITHIN FILE FILNUM.
!     FILSUP IS THE NUMBER OF THE SUPERBLOCK WITHIN FILE FILNUM TO WHICH
!     BLOCK I BELONGS, AND SUPSIZ IS THE SIZE OF A SUPERBLOCK.
!
 100  filsup = (index-1)/Supsiz
   IF ( index-1/=filsup*Supsiz ) filsup = filsup + 1
!
!     COMPUTE THE LOGICAL BLOCK NUMBER, WITHIN THE ARRAY NXT, IN WHICH
!     THE ITH BLOCK HAS AN ENTRY, ALSO COMPUTE THE INDEX OF THIS ENTRY
!     RELATIVE TO THE ARRAY BUF.  STORE THE BLOCK NUMBER IN IBLOCK, AND
!     THE INDEX IN J.
!
   iblock = 0
   max = filnum - 1
   IF ( max>=1 ) THEN
      DO i = 1 , max
         iblock = iblock + Nxtfsz(i)
      ENDDO
   ENDIF
   iblock = iblock + filsup
   J = (index-(filsup-1)*Supsiz)/2 + 1 + Nxt
   IF ( iblock==Nxtlbn ) RETURN
   IF ( iblock>Nxttsz ) THEN
      CALL errmkn(indsbr,1)
   ELSE
!
!     THE DESIRED NXT BLOCK IS NOT PRESENTLY IN CORE, MUST THEREFORE
!     FETCH IT.
!
      IF ( Ditpbn/=0 ) THEN
!
!     THE IN CORE BLOCK SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY ONE BLOCK OF THE DIT.
!
         IF ( Ditup ) THEN
!
!     THE DIT BLOCK NOW IN CORE HAS BEEN UPDATED.  MUST THEREFORE WRITE
!     IT OUT BEFORE READING IN THE DESIRED NXT BLOCK.
!
            CALL sofio(iwrt,Ditpbn,Buf(Dit-2))
            Ditup = .FALSE.
         ENDIF
         Ditpbn = 0
         Ditlbn = 0
      ELSEIF ( Nxtpbn/=0 ) THEN
!
!     THE IN CORE BLOCK SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY ONE BLOCK OF NXT.
!
         IF ( Nxtup ) THEN
!
!     THE NEXT BLOCK CURRENTLY IN CORE HAS BEEN UPDATED.  MUST THEREFORE
!     WRITE IT OUT BEFORE READING IN A NEW BLOCK.
!
            CALL sofio(iwrt,Nxtpbn,Buf(Nxt-2))
            Nxtup = .FALSE.
         ENDIF
      ENDIF
!
!     READ THE DESIRED NXT BLOCK INTO CORE.
!
      Nxtlbn = iblock
      Nxtpbn = 0
      IF ( max>=1 ) THEN
         DO i = 1 , max
            Nxtpbn = Nxtpbn + Filsiz(i)
         ENDDO
      ENDIF
      Nxtpbn = Nxtpbn + (filsup-1)*Supsiz + 2
      CALL sofio(ird,Nxtpbn,Buf(Nxt-2))
      RETURN
   ENDIF
99999 RETURN
END SUBROUTINE fnxt
