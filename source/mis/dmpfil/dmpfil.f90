!*==dmpfil.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dmpfil(Ifile,Z,Lz)
   IMPLICIT NONE
   USE C_NAMES
   USE C_SYSTEM
   USE C_UNPAKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ifile
   INTEGER , DIMENSION(2) :: Z
   INTEGER :: Lz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf , file , i , i1 , i2 , irec , iwords , j , k , l , lcore , ncols , nels
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , mesage , open , rdtrl , read , sswtch , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DUMPS A FILE ON DIAG 20 SETTING.
!
   DATA name/4HDMPF , 2HIL/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL sswtch(20,l)
         IF ( l==0 ) RETURN
!
         file = iabs(Ifile)
         buf = Lz - Sysbuf + 1
         IF ( buf<6 ) THEN
!
            CALL mesage(8,0,name)
            GOTO 60
         ELSE
            lcore = (buf-1)/5
            lcore = lcore*5
            CALL open(*60,file,Z(buf),Rdrew)
            WRITE (Outpt,99001) file
99001       FORMAT (14H1DUMP OF FILE ,I3)
            IF ( Ifile<=0 ) THEN
!
               CALL read(*40,*80,file,Z,2,1,iwords)
               GOTO 80
            ELSE
!
               irec = 0
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         WRITE (Outpt,99002) irec
99002    FORMAT (8H0RECORD ,I6,6X,100(1H-))
         SPAG_Loop_1_1: DO
            CALL read(*40,*20,file,Z,lcore,0,iwords)
!
            i1 = -9
            DO
               i1 = i1 + 10
               i2 = min0(i1+9,lcore)
               WRITE (Outpt,99009) i1 , (Z(i),i=i1,i2)
               WRITE (Outpt,99010) (Z(i),i=i1,i2)
               WRITE (Outpt,99011) (Z(i),i=i1,i2)
               IF ( lcore<=i2 ) CYCLE SPAG_Loop_1_1
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
 20      i1 = -9
         DO
            i1 = i1 + 10
            i2 = min0(i1+9,iwords)
            WRITE (Outpt,99009) i1 , (Z(i),i=i1,i2)
            WRITE (Outpt,99010) (Z(i),i=i1,i2)
            WRITE (Outpt,99011) (Z(i),i=i1,i2)
            IF ( iwords<=i2 ) THEN
               irec = irec + 1
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
 40      Z(1) = file
         CALL close(file,Clsrew)
         CALL rdtrl(Z)
         WRITE (Outpt,99003) (Z(i),i=1,7)
99003    FORMAT (4H0EOF,//,8H0TRAILER,/,7(1X,I12/))
 60      RETURN
 80      WRITE (Outpt,99004) Z(1) , Z(2)
99004    FORMAT (14H0HEADER RECORD,/1H0,2A4)
         Z(1) = file
         CALL rdtrl(Z)
         ncols = Z(2)
         IF ( ncols>300 ) ncols = 100
         Iout = 1
         Incr = 1
         IF ( ncols>0 ) THEN
            DO j = 1 , ncols
               WRITE (Outpt,99005) j
99005          FORMAT (7H0COLUMN,I5)
               Irow = 0
               Nrow = 0
               CALL unpack(*85,file,Z)
               WRITE (Outpt,99006) Irow , Nrow
99006          FORMAT (1H+,20X,3HROW,I4,11H   THRU ROW,I5)
               IF ( Nrow>300 ) Nrow = 100
               nels = Nrow - Irow + 1
               IF ( nels>0 ) THEN
                  WRITE (Outpt,99007) (Z(k),k=1,nels)
99007             FORMAT (1P,10E13.4)
               ENDIF
               CYCLE
 85            WRITE (Outpt,99008)
99008          FORMAT (13H NULL COLUMN )
            ENDDO
         ENDIF
         GOTO 40
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99009 FORMAT (1H0,I5,10(1X,I10,1X))
99010 FORMAT (1H ,5X,10(1X,1P,E11.4))
99011 FORMAT (1H ,5X,10(4X,A4,4X))
!
END SUBROUTINE dmpfil
