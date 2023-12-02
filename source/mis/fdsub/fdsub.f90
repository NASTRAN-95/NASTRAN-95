!*==fdsub.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fdsub(Name,I)
   IMPLICIT NONE
   USE C_SOF
   USE C_SYS
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: I
!
! Local variable declarations rewritten by SPAG
!
   REAL :: dummy
   INTEGER :: j , k , kk , max , nblks , nnms
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL chkopn , fdit
!
! End of declarations rewritten by SPAG
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
               CALL spag_block_1
               RETURN
            ENDIF
         ENDDO
      ENDDO
   ENDIF
!
!     DID NOT FIND NAME IN THE DIT.
!
   I = -1
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     DID FIND NAME IN THE DIT.  RETURN NAME INDEX NUMBER
!
      I = (Ditlbn-1)*nnms + (kk+1)/2
   END SUBROUTINE spag_block_1
END SUBROUTINE fdsub
