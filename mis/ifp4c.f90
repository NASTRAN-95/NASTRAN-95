
SUBROUTINE ifp4c(File,Scrt,Buf1,Buf2,Eof)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cls , Clsrew , Rd , Rdrew , Wrt , Wrtrew
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
!
! Dummy argument declarations
!
   LOGICAL Eof
   INTEGER File , Scrt
   INTEGER Buf1(10) , Buf2(10)
!
! Local variable declarations
!
   INTEGER eor , flag , i , name(2) , name2(2) , noeor , trail(7) , work(10)
!
! End of declarations
!
!
!     THIS ROUTINE, CALLED BY IFP4, OPENS THE 2 FILES AND COPIES THE
!     HEADER RECORD FROM -FILE- TO -SCRT-.
!
   DATA name/4HIFP4 , 4HC   / , eor , noeor/1 , 0/
!
   trail(1) = File
   DO i = 2 , 7
      trail(i) = 0
   ENDDO
   CALL rdtrl(trail)
   DO i = 2 , 7
      IF ( trail(i)/=0 ) GOTO 100
   ENDDO
!
!     FILE IS NULL
!
   Eof = .TRUE.
   CALL open(*300,Scrt,Buf2,Wrtrew)
   CALL fname(File,name2)
   CALL write(Scrt,name2,2,eor)
   RETURN
 100  CALL open(*500,File,Buf1,Rdrew)
   Eof = .FALSE.
   CALL open(*300,Scrt,Buf2,Wrtrew)
   DO
      CALL read(*400,*200,File,work,10,noeor,flag)
      CALL write(Scrt,work,10,noeor)
   ENDDO
 200  CALL write(Scrt,work,flag,eor)
   RETURN
!
 300  CALL mesage(-1,Scrt,name)
 400  CALL mesage(-2,File,name)
 500  CALL mesage(-1,File,name)
END SUBROUTINE ifp4c
