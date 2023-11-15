
SUBROUTINE fdsub(Name,I)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Blksiz , Buf(1) , Dirsiz , Dit , Ditbl , Ditlbn , Ditnsb , Ditpbn , Ditsiz , Iodum(8) , Mdidum(4) , Nxtdum(15)
   LOGICAL Ditup
   COMMON /sof   / Dit , Ditpbn , Ditlbn , Ditsiz , Ditnsb , Ditbl , Iodum , Mdidum , Nxtdum , Ditup
   COMMON /sys   / Blksiz , Dirsiz
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER I
   INTEGER Name(2)
!
! Local variable declarations
!
   REAL dummy
   INTEGER j , k , kk , max , nblks , nmsbr(2) , nnms
!
! End of declarations
!
!                                                           ** PRETTIED
!     SEARCHES IF THE SUBSTRUCTURE NAME HAS AN ENTRY IN THE DIT. IF IT
!     DOES, THE OUTPUT VALUE OF I WILL INDICATE THAT NAME IS THE ITH
!     SUBSTRUCTURE IN THE DIT.  I WILL BE SET TO -1 IF NAME DOES NOT
!     HAVE AN ENTRY IN THEDIT.
!
   DATA nmsbr/4HFDUB , 4HB   /
!
!     NNMS IS THE NUMBER OF NAMES ON ONE BLOCK OF THE DIT, AND NBLKS IS
!     THE SIZE OF THE DIT IN NUMBER OF BLOCKS.
!
   CALL chkopn(nmsbr(1))
   IF ( Ditnsb/=0 ) THEN
      nnms = Blksiz/2
      nblks = Ditsiz/Blksiz
      IF ( Ditsiz/=nblks*Blksiz ) nblks = nblks + 1
!
!     START LOOKING FOR THE SUBSTRUCTURE NAME.
!
      max = Blksiz
      DO j = 1 , nblks
         I = 1 + (j-1)*nnms
         CALL fdit(I,dummy)
         IF ( j==nblks ) max = Ditsiz - (nblks-1)*Blksiz
!
!     SEARCH THE BLOCK OF THE DIT WHICH IS PRESENTLY IN CORE.
!
         DO k = 1 , max , 2
            IF ( Buf(Dit+k)==Name(1) .AND. Buf(Dit+k+1)==Name(2) ) THEN
               kk = k
               GOTO 100
            ENDIF
         ENDDO
      ENDDO
   ENDIF
!
!     DID NOT FIND NAME IN THE DIT.
!
   I = -1
   RETURN
!
!     DID FIND NAME IN THE DIT.  RETURN NAME INDEX NUMBER
!
 100  I = (Ditlbn-1)*nnms + (kk+1)/2
END SUBROUTINE fdsub
