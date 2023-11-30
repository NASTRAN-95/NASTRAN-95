
SUBROUTINE sofi
   IMPLICIT NONE
   INTEGER Dry , Items(2,5) , Iz(1) , Name(2) , Nout , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Dry , Name , Items
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Iz
   INTEGER file(5) , i , ib1 , ib2 , ib3 , iblnk , itest , mcb(7) , modnam(2) , nz , xxxx
   INTEGER korsz
!
!     MODULE USED TO COPY SELECTED ITEMS FROM SELECTED SUBSTRUCTURES
!     ONTO NASTRAN MATRIX FILES.   THE CALLING SEQUENCE TO THE MODULE
!     IS
!     SOFI   /A,B,C,D,E/V,N,DRY/C,N,NAME/C,N,IA/C,N,IB/C,N,IC/C,N,ID/
!                       C,N,IE $
!
   DATA file/201 , 202 , 203 , 204 , 205/
   DATA iblnk , xxxx/4H     , 4HXXXX/
   DATA modnam/4HSOFI , 4H    /
!
   DO i = 1 , 5
      IF ( Items(1,i)==xxxx .OR. Items(1,i)==0 ) Items(1,i) = iblnk
   ENDDO
!
   nz = korsz(Iz)
   IF ( 3*Sysbuf>nz ) CALL mesage(-8,0,modnam(1))
   ib1 = nz - Sysbuf + 1
   ib2 = ib1 - Sysbuf - 1
   ib3 = ib2 - Sysbuf
   CALL sofopn(Iz(ib1),Iz(ib2),Iz(ib3))
   IF ( Dry>=0 ) THEN
!
!     COPY SOF DATA INTO NASTRAN DATA BLOCKS
!
      DO i = 1 , 5
         IF ( Items(1,i)/=iblnk ) THEN
            mcb(1) = file(i)
            CALL rdtrl(mcb)
            IF ( mcb(1)>=0 ) THEN
               CALL mtrxi(file(i),Name(1),Items(1,i),0,itest)
               IF ( itest==1 .OR. itest==6 ) CYCLE
               IF ( itest==3 ) THEN
                  WRITE (Nout,99002) Uwm , Items(1,i) , Name(1) , Name(2)
                  CYCLE
               ELSEIF ( itest==4 ) THEN
                  WRITE (Nout,99003) Uwm , Name(1) , Name(2)
                  Dry = -2
                  EXIT
               ELSEIF ( itest==5 ) THEN
                  WRITE (Nout,99004) Uwm , Items(1,i)
               ELSE
                  WRITE (Nout,99001) Uwm , Items(1,i) , Name(1) , Name(2)
99001             FORMAT (A25,' 6215, MODULE SOFI - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
               ENDIF
               Dry = -2
            ENDIF
         ENDIF
      ENDDO
   ELSE
!
!     CHECK THE EXISTENCE OF THE SOF FILE.
!
      DO i = 1 , 5
         IF ( Items(1,i)/=iblnk ) THEN
            mcb(1) = file(i)
            CALL rdtrl(mcb)
            IF ( mcb(1)>=0 ) THEN
               CALL softrl(Name(1),Items(1,i),mcb)
               itest = mcb(1)
               IF ( itest==1 .OR. itest==2 ) CYCLE
               IF ( itest==4 ) THEN
                  WRITE (Nout,99003) Uwm , Name(1) , Name(2)
                  Dry = -2
                  EXIT
               ELSEIF ( itest==5 ) THEN
                  WRITE (Nout,99004) Uwm , Items(1,i)
               ELSE
                  WRITE (Nout,99002) Uwm , Items(1,i) , Name(1) , Name(2)
               ENDIF
               Dry = -2
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   CALL sofcls
   RETURN
!
!     ERROR MESSAGES.
!
99002 FORMAT (A25,' 6216, MODULE SOFI - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
99003 FORMAT (A25,' 6212, MODULE SOFI - THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
99004 FORMAT (A25,' 6213, MODULE SOFI - ',A4,' IS AN ILLEGAL ITEM NAME')
END SUBROUTINE sofi
