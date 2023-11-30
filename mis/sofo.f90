
SUBROUTINE sofo
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
!     MODULE USED TO TRANSFER NASTRAN DATA BLOCKS TO THE SOF FILE FOR
!     PURPOSES OF SAVING THE DATA FOR SUBSEQUENT RUNS OR SUBSEQUENT
!     EXECUTION STEPS.  THE CALLING SEQUENCE TO THE MODULE IS
!
!     SOFO     A,B,C,D,E//V,N,DRY/C,N,NAME/C,N,IA/C,N,IB/C,N,IC/
!                         C,N,ID/C,N,IE $
!
   DATA file/101 , 102 , 103 , 104 , 105/
   DATA modnam/4HSOFO , 4H    /
   DATA iblnk , xxxx/4H     , 4HXXXX/
!
   DO i = 1 , 5
      IF ( Items(1,i)==xxxx .OR. Items(1,i)==0 ) Items(1,i) = iblnk
   ENDDO
!
   IF ( Dry<0 ) RETURN
   nz = korsz(Iz)
   IF ( 3*Sysbuf>nz ) CALL mesage(-8,0,modnam(1))
   ib1 = nz - Sysbuf + 1
   ib2 = ib1 - Sysbuf - 1
   ib3 = ib2 - Sysbuf
   CALL sofopn(Iz(ib1),Iz(ib2),Iz(ib3))
!
!     COPY MATRICES FROM NASTRAN DATA BLOCKS TO SOF.
!
   DO i = 1 , 5
      IF ( Items(1,i)/=iblnk ) THEN
         mcb(1) = file(i)
         CALL rdtrl(mcb)
         IF ( mcb(1)>=0 ) THEN
            CALL mtrxo(file(i),Name(1),Items(1,i),0,itest)
            IF ( itest==2 .OR. itest==3 .OR. itest==6 ) THEN
            ELSEIF ( itest==4 ) THEN
               WRITE (Nout,99001) Uwm , Name(1) , Name(2)
99001          FORMAT (A25,' 6212, MODULE SOFO - THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
               Dry = -2
               EXIT
            ELSEIF ( itest==5 ) THEN
               WRITE (Nout,99002) Uwm , Items(1,i)
99002          FORMAT (A25,' 6213, MODULE SOFO - ',A4,' IS AN ILLEGAL ITEM NAME')
               Dry = -2
            ELSE
               WRITE (Nout,99003) Uwm , Items(1,i) , Name(1) , Name(2)
!
!     ERROR MESSAGES.
!
99003          FORMAT (A25,' 6211, MODULE SOFO - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
               Dry = -2
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   CALL sofcls
   RETURN
END SUBROUTINE sofo
