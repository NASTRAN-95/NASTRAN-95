!*==fndgrd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fndgrd(Isub,Icomp,Igrid,Ip,Ic,N)
   USE c_cmb001
   USE c_cmb002
   USE c_cmbfnd
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Isub
   INTEGER :: Icomp
   INTEGER :: Igrid
   INTEGER , DIMENSION(6) :: Ip
   INTEGER , DIMENSION(6) :: Ic
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER :: i , lloc , nfil , nrec , nwd
   EXTERNAL close , fwdrec , gridip , mesage , open , read , skpfil
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   DATA aaa/4HFNDG , 4HRD  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         CALL open(*40,scsfil,z(buf3),0)
         nfil = Isub - 1
         CALL skpfil(scsfil,nfil)
         nrec = Icomp - 1
         IF ( nrec/=0 ) THEN
            DO i = 1 , nrec
               CALL fwdrec(*60,scsfil)
            ENDDO
         ENDIF
         CALL read(*60,*20,scsfil,z(score),lcore,1,nwd)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      CALL gridip(Igrid,score,nwd,Ip,Ic,N,z,lloc)
         CALL close(scsfil,1)
         RETURN
 40      CALL mesage(-1,scsfil,aaa)
 60      CALL mesage(-2,scsfil,aaa)
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(-8,scsfil,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fndgrd
