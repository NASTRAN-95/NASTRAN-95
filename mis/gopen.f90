
SUBROUTINE gopen(File,Buffer,Option)
   IMPLICIT NONE
   INTEGER File , Option
   REAL Buffer(1)
   INTEGER err , inpnor , outnor , outrew
   REAL header(2) , subnam(2)
!
   DATA subnam/4H GOP , 4HEN  /
   DATA outrew , inpnor , outnor/1 , 2 , 3/
!
   CALL open(*100,File,Buffer,Option)
   IF ( Option/=inpnor .AND. Option/=outnor ) THEN
      IF ( Option==outrew ) THEN
         CALL fname(File,header)
         CALL write(File,header,2,1)
      ELSE
         CALL read(*200,*300,File,header,2,1,err)
      ENDIF
   ENDIF
   RETURN
!
 100  err = -1
   GOTO 400
 200  err = -2
   GOTO 400
 300  err = -3
 400  CALL mesage(err,File,subnam)
!
END SUBROUTINE gopen