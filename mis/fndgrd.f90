
SUBROUTINE fndgrd(Isub,Icomp,Igrid,Ip,Ic,N)
   IMPLICIT NONE
   INTEGER Buf3 , Ierr , Inam(2) , Junk(3) , Junk1(2) , Junk2(2) , Lcore , Score , Scsfil , Z(1)
   COMMON /cmb001/ Junk , Scsfil
   COMMON /cmb002/ Junk1 , Buf3 , Junk2 , Score , Lcore
   COMMON /cmbfnd/ Inam , Ierr
   COMMON /zzzzzz/ Z
   INTEGER Icomp , Igrid , Isub , N
   INTEGER Ic(6) , Ip(6)
   INTEGER aaa(2) , i , lloc , nfil , nrec , nwd
!
   DATA aaa/4HFNDG , 4HRD  /
   CALL open(*200,Scsfil,Z(Buf3),0)
   nfil = Isub - 1
   CALL skpfil(Scsfil,nfil)
   nrec = Icomp - 1
   IF ( nrec/=0 ) THEN
      DO i = 1 , nrec
         CALL fwdrec(*300,Scsfil)
      ENDDO
   ENDIF
   CALL read(*300,*100,Scsfil,Z(Score),Lcore,1,nwd)
   GOTO 400
 100  CALL gridip(Igrid,Score,nwd,Ip,Ic,N,Z,lloc)
   CALL close(Scsfil,1)
   RETURN
 200  CALL mesage(-1,Scsfil,aaa)
 300  CALL mesage(-2,Scsfil,aaa)
 400  CALL mesage(-8,Scsfil,aaa)
END SUBROUTINE fndgrd
