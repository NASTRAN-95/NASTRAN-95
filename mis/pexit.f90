
SUBROUTINE pexit
   IMPLICIT NONE
   INTEGER Date(3) , Icpflg , Irdict , Isystm(100) , Le(17) , Mach , Nmsg , Nosbe , Nout
   COMMON /machin/ Mach
   COMMON /msgx  / Nmsg
   COMMON /output/ Le
   COMMON /resdic/ Irdict
   COMMON /system/ Isystm
   INTEGER hh , i , j , mm , ss
   REAL t
   EXTERNAL link
!
!DME  19 JAN 2016
!DME  D. Everhart
!DME  EXTERNAL statement to remove collision with GFORTRAN
!DME  implementation of LINK function.
!DME
   EQUIVALENCE (Isystm(2),Nout) , (Isystm(76),Nosbe) , (Isystm(82),Icpflg) , (Isystm(15),Date)
!
!     SEE IF ANY MESSAGES ARE IN THE QUEUE
!
   IF ( Nmsg>0 ) CALL msgwrt
   IF ( Icpflg/=0 ) WRITE (Irdict,99001)
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
      WRITE (Nout,99002) (Le(i),i=j,8)
99002 FORMAT (//1X,6A4)
   ELSE
      WRITE (Nout,99003) Le , Date , hh , mm , ss
99003 FORMAT (////40X,'* * * END OF JOB * * *',/1H1,/,' JOB TITLE = ',17A4,/,' DATE:',I3,1H/,I2,1H/,I2,/,' END TIME:',I3,1H:,I2,1H:,&
            & I2)
!
!     CDC TOTAL CPU TIME IS A BIG NUMBER. DON'T PRINT IT
!
      IF ( Mach/=4 .AND. Le(1)/=-1 ) THEN
         IF ( Mach<=5 ) WRITE (Nout,99004) i
99004    FORMAT (' TOTAL CPU TIME',I6,' SEC.')
         IF ( Mach>5 ) WRITE (Nout,99005) i
99005    FORMAT (' TOTAL WALL CLOCK TIME',I7,' SEC.')
      ENDIF
!
!     FLUSH O/P BUFFERS
!
      WRITE (Nout,99006)
99006 FORMAT (1H )
!
      IF ( Mach==4 .AND. Nosbe>0 ) CALL link(-1,Nosbe,1)
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
