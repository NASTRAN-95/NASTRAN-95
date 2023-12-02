!*==dswrit.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dswrit(Iunit,Buff,Len,Irec,Iccerr)
   USE i_dsiof
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Len
   INTEGER :: Iunit
   INTEGER , DIMENSION(Len) :: Buff
   INTEGER :: Irec
   INTEGER :: Iccerr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: istat
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
!      print *,' dswrit,len,IREC,UNIT=',len,irec,iunit
         IF ( Irec<=0 ) THEN
            WRITE (iwr,99001) Iunit , Irec , mdsnam(Iunit)
99001       FORMAT (//' ERROR IN DSWRIT, BAD RECORD NO., UNIT=',I4,' REC=',I5,/,' FILE NAME=',A80)
            Iccerr = istat
            CALL dsmsg(101)
            CALL mesage(-61,0,0)
         ELSE
            WRITE (Iunit,REC=Irec,IOSTAT=istat,ERR=20) Buff
            Iccerr = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      WRITE (iwr,99002) Iunit , Irec , istat , mdsnam(Iunit)
99002    FORMAT (//', ERROR ENCOUNTERED IN DSWRCC, UNIT=',I5,' RECORD=',I5,' STATUS=',I9,/' DSNAME=',A80)
         Iccerr = istat
         CALL dsmsg(101)
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 2
      CASE (2)
         numwri = numwri + 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE dswrit
