!*==pexit.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pexit
   IMPLICIT NONE
   USE C_MACHIN
   USE C_MSGX
   USE C_OUTPUT
   USE C_RESDIC
   USE C_SYSTEM
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(3) :: date
   INTEGER :: hh , i , icpflg , j , mm , nosbe , nout , ss
   REAL :: t
   EXTERNAL cputim , dbmstf , exit , link , msgwrt , waltim
!
! End of declarations rewritten by SPAG
!
!
!DME  19 JAN 2016
!DME  D. Everhart
!DME  EXTERNAL statement to remove collision with GFORTRAN
!DME  implementation of LINK function.
!DME
   !>>>>EQUIVALENCE (Isystm(2),Nout) , (Isystm(76),Nosbe) , (Isystm(82),Icpflg) , (Isystm(15),Date)
!
!     SEE IF ANY MESSAGES ARE IN THE QUEUE
!
   IF ( Nmsg>0 ) CALL msgwrt
   IF ( icpflg/=0 ) WRITE (Irdict,99001)
99001 FORMAT ('$ END OF CHECKPOINT DICTIONARY')
!
!     JOB DONE. PRINT LAST 4 MESSAGE LINES
!
   CALL waltim(i)
   hh = i/3600
   mm = (i-hh*3600)/60
   ss = i - hh*3600 - mm*60
   CALL cputim(i,t,0)
   IF ( Mach==4 ) i = t
   IF ( Le(1)==-1 .AND. Le(2)==-1 ) THEN
!
      j = 5
      IF ( Le(9)>=0 ) j = 3
      WRITE (nout,99002) (Le(i),i=j,8)
99002 FORMAT (//1X,6A4)
   ELSE
      WRITE (nout,99003) Le , date , hh , mm , ss
99003 FORMAT (////40X,'* * * END OF JOB * * *',/1H1,/,' JOB TITLE = ',17A4,/,' DATE:',I3,1H/,I2,1H/,I2,/,' END TIME:',I3,1H:,I2,1H:,&
            & I2)
!
!     CDC TOTAL CPU TIME IS A BIG NUMBER. DON'T PRINT IT
!
      IF ( Mach/=4 .AND. Le(1)/=-1 ) THEN
         IF ( Mach<=5 ) WRITE (nout,99004) i
99004    FORMAT (' TOTAL CPU TIME',I6,' SEC.')
         IF ( Mach>5 ) WRITE (nout,99005) i
99005    FORMAT (' TOTAL WALL CLOCK TIME',I7,' SEC.')
      ENDIF
!
!     FLUSH O/P BUFFERS
!
      WRITE (nout,99006)
99006 FORMAT (1H )
!
      IF ( Mach==4 .AND. nosbe>0 ) CALL link(-1,nosbe,1)
   ENDIF
!
   CALL dbmstf
   DO i = 1 , 4
      CLOSE (i)
   ENDDO
   DO i = 7 , 22
      CLOSE (i)
   ENDDO
!WKBR 8/94 SUN  CALL EXIT
   CALL exit(0)
END SUBROUTINE pexit
