!*==cpyfil.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cpyfil(Infile,Oufile,Area,N,Count)
   USE c_blank
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Infile
   INTEGER :: Oufile
   INTEGER , DIMENSION(2) :: Area
   INTEGER :: N
   INTEGER :: Count
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: eor , nwds , type
   INTEGER , DIMENSION(15) :: inblk , oublk
   EXTERNAL cpystr , fname , read , rectyp , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CPYFIL COPIES RECORDS FROM INFILE TO OUFILE UNTIL AN END-OF-FILE
!     ON INFILE IS ENCOUNTERED. AN END-OF-FILE IS NOT WRITTEN ON OUFILE.
!     RECTYPE IS CALLED PRIOR TO COPYING EACH RECORD. STRING RECORDS ARE
!     COPIED USING CPYSTR. NORMAL RECORDS ARE COPIED USING READ/WRITE.
!     UPON EXIT, INFILE IS POSITIONED IMMEDIATELY AFTER THE END-OF-FILE
!     AND OUFILE IS POSITIONED AFTER THE LAST RECORD WRITTEN.
!
!     THIS ROUTINE DOES NOT OPEN NOR CLOSE ANY FILE, NOR WRITE ANY
!     MATRIX TRAILER
!
!
!     INITIALIZE STRING COMMUNICATION BLOCKS AND DETERMINE RECORD TYPE
!
         inblk(1) = Infile
         oublk(1) = Oufile
         Count = 0
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL rectyp(Infile,type)
            IF ( type==0 ) EXIT SPAG_Loop_1_1
!
!     COPY STRING RECORDS
!
            CALL cpystr(inblk,oublk,0,0)
            Count = Count + oublk(13)
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
!
!     COPY NORMAL RECORD
!
         eor = 1
         CALL read(*99999,*20,Infile,Area,N,0,nwds)
         eor = 0
         nwds = N
 20      IF ( Count==0 .AND. iparam==-1111 ) THEN
            CALL fname(Infile,Area(nwds+1))
            IF ( Area(nwds+1)==Area(1) .AND. Area(nwds+2)==Area(2) ) CALL fname(Oufile,Area)
         ENDIF
         CALL write(Oufile,Area,nwds,eor)
         Count = Count + nwds
         IF ( eor/=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!     RETURN WHEN END-OF-FILE IS ENCOUNTERED
!
99999 END SUBROUTINE cpyfil
