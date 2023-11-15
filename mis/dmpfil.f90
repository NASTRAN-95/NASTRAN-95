
SUBROUTINE dmpfil(Ifile,Z,Lz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cls , Clsrew , Rd , Rdrew , Wrt , Wrtrew
   INTEGER Incr , Iout , Irow , Nrow , Outpt , Sysbuf
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Outpt
   COMMON /unpakx/ Iout , Irow , Nrow , Incr
!
! Dummy argument declarations
!
   INTEGER Ifile , Lz
   INTEGER Z(2)
!
! Local variable declarations
!
   INTEGER buf , file , i , i1 , i2 , irec , iwords , j , k , l , lcore , name(2) , ncols , nels
!
! End of declarations
!
!
!     DUMPS A FILE ON DIAG 20 SETTING.
!
   DATA name/4HDMPF , 2HIL/
!
   CALL sswtch(20,l)
   IF ( l==0 ) RETURN
!
   file = iabs(Ifile)
   buf = Lz - Sysbuf + 1
   IF ( buf<6 ) THEN
!
      CALL mesage(8,0,name)
      GOTO 500
   ELSE
      lcore = (buf-1)/5
      lcore = lcore*5
      CALL open(*500,file,Z(buf),Rdrew)
      WRITE (Outpt,99001) file
99001 FORMAT (14H1DUMP OF FILE ,I3)
      IF ( Ifile<=0 ) THEN
!
         CALL read(*400,*600,file,Z,2,1,iwords)
         GOTO 600
      ELSE
!
         irec = 0
      ENDIF
   ENDIF
 100  WRITE (Outpt,99002) irec
99002 FORMAT (8H0RECORD ,I6,6X,100(1H-))
 200  CALL read(*400,*300,file,Z,lcore,0,iwords)
!
   i1 = -9
   DO
      i1 = i1 + 10
      i2 = min0(i1+9,lcore)
      WRITE (Outpt,99009) i1 , (Z(i),i=i1,i2)
      WRITE (Outpt,99010) (Z(i),i=i1,i2)
      WRITE (Outpt,99011) (Z(i),i=i1,i2)
      IF ( lcore<=i2 ) GOTO 200
   ENDDO
!
 300  i1 = -9
   DO
      i1 = i1 + 10
      i2 = min0(i1+9,iwords)
      WRITE (Outpt,99009) i1 , (Z(i),i=i1,i2)
      WRITE (Outpt,99010) (Z(i),i=i1,i2)
      WRITE (Outpt,99011) (Z(i),i=i1,i2)
      IF ( iwords<=i2 ) THEN
         irec = irec + 1
         GOTO 100
      ENDIF
   ENDDO
!
 400  Z(1) = file
   CALL close(file,Clsrew)
   CALL rdtrl(Z)
   WRITE (Outpt,99003) (Z(i),i=1,7)
99003 FORMAT (4H0EOF,//,8H0TRAILER,/,7(1X,I12/))
 500  RETURN
 600  WRITE (Outpt,99004) Z(1) , Z(2)
99004 FORMAT (14H0HEADER RECORD,/1H0,2A4)
   Z(1) = file
   CALL rdtrl(Z)
   ncols = Z(2)
   IF ( ncols>300 ) ncols = 100
   Iout = 1
   Incr = 1
   IF ( ncols>0 ) THEN
      DO j = 1 , ncols
         WRITE (Outpt,99005) j
99005    FORMAT (7H0COLUMN,I5)
         Irow = 0
         Nrow = 0
         CALL unpack(*620,file,Z)
         WRITE (Outpt,99006) Irow , Nrow
99006    FORMAT (1H+,20X,3HROW,I4,11H   THRU ROW,I5)
         IF ( Nrow>300 ) Nrow = 100
         nels = Nrow - Irow + 1
         IF ( nels>0 ) THEN
            WRITE (Outpt,99007) (Z(k),k=1,nels)
99007       FORMAT (1P,10E13.4)
         ENDIF
         CYCLE
 620     WRITE (Outpt,99008)
99008    FORMAT (13H NULL COLUMN )
      ENDDO
   ENDIF
   GOTO 400
!
99009 FORMAT (1H0,I5,10(1X,I10,1X))
99010 FORMAT (1H ,5X,10(1X,1P,E11.4))
99011 FORMAT (1H ,5X,10(4X,A4,4X))
!
END SUBROUTINE dmpfil
