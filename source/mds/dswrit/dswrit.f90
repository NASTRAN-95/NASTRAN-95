!*==dswrit.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dswrit(Iunit,Buff,Len,Irec,Iccerr)
   IMPLICIT NONE
   USE I_DSIOF
   USE C_SYSTEM
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
!
! End of declarations rewritten by SPAG
!
!      print *,' dswrit,len,IREC,UNIT=',len,irec,iunit
   IF ( Irec<=0 ) THEN
      WRITE (Iwr,99001) Iunit , Irec , mdsnam(Iunit)
99001 FORMAT (//' ERROR IN DSWRIT, BAD RECORD NO., UNIT=',I4,' REC=',I5,/,' FILE NAME=',A80)
      Iccerr = istat
      CALL dsmsg(101)
      CALL mesage(-61,0,0)
   ELSE
      WRITE (Iunit,REC=Irec,IOSTAT=istat,ERR=100) Buff
      Iccerr = 0
      GOTO 200
   ENDIF
 100  WRITE (Iwr,99002) Iunit , Irec , istat , mdsnam(Iunit)
99002 FORMAT (//', ERROR ENCOUNTERED IN DSWRCC, UNIT=',I5,' RECORD=',I5,' STATUS=',I9,/' DSNAME=',A80)
   Iccerr = istat
   CALL dsmsg(101)
   CALL mesage(-61,0,0)
 200  numwri = numwri + 1
END SUBROUTINE dswrit
