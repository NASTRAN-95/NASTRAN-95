
SUBROUTINE cpyfil(Infile,Oufile,Area,N,Count)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iparam
   COMMON /blank / Iparam
!
! Dummy argument declarations
!
   INTEGER Count , Infile , N , Oufile
   INTEGER Area(2)
!
! Local variable declarations
!
   INTEGER eor , inblk(15) , nwds , oublk(15) , type
!
! End of declarations
!
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
 100  DO
      CALL rectyp(Infile,type)
      IF ( type==0 ) EXIT
!
!     COPY STRING RECORDS
!
      CALL cpystr(inblk,oublk,0,0)
      Count = Count + oublk(13)
   ENDDO
!
!     COPY NORMAL RECORD
!
 200  eor = 1
   CALL read(*99999,*300,Infile,Area,N,0,nwds)
   eor = 0
   nwds = N
 300  IF ( Count==0 .AND. Iparam==-1111 ) THEN
      CALL fname(Infile,Area(nwds+1))
      IF ( Area(nwds+1)==Area(1) .AND. Area(nwds+2)==Area(2) ) CALL fname(Oufile,Area)
   ENDIF
   CALL write(Oufile,Area,nwds,eor)
   Count = Count + nwds
   IF ( eor==0 ) GOTO 200
   GOTO 100
!
!     RETURN WHEN END-OF-FILE IS ENCOUNTERED
!
99999 END SUBROUTINE cpyfil
