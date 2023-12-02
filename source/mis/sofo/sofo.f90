!*==sofo.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sofo
   USE c_blank
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(5) , SAVE :: file
   INTEGER :: i , ib1 , ib2 , ib3 , itest , nz
   INTEGER , SAVE :: iblnk , xxxx
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: modnam
   EXTERNAL korsz , mesage , mtrxo , rdtrl , sofcls , sofopn
!
! End of declarations rewritten by SPAG
!
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
      IF ( items(1,i)==xxxx .OR. items(1,i)==0 ) items(1,i) = iblnk
   ENDDO
!
   IF ( dry<0 ) RETURN
   nz = korsz(iz)
   IF ( 3*sysbuf>nz ) CALL mesage(-8,0,modnam(1))
   ib1 = nz - sysbuf + 1
   ib2 = ib1 - sysbuf - 1
   ib3 = ib2 - sysbuf
   CALL sofopn(iz(ib1),iz(ib2),iz(ib3))
!
!     COPY MATRICES FROM NASTRAN DATA BLOCKS TO SOF.
!
   SPAG_Loop_1_1: DO i = 1 , 5
      IF ( items(1,i)/=iblnk ) THEN
         mcb(1) = file(i)
         CALL rdtrl(mcb)
         IF ( mcb(1)>=0 ) THEN
            CALL mtrxo(file(i),name(1),items(1,i),0,itest)
            IF ( itest==2 .OR. itest==3 .OR. itest==6 ) THEN
            ELSEIF ( itest==4 ) THEN
               WRITE (nout,99001) uwm , name(1) , name(2)
99001          FORMAT (A25,' 6212, MODULE SOFO - THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
               dry = -2
               EXIT SPAG_Loop_1_1
            ELSEIF ( itest==5 ) THEN
               WRITE (nout,99002) uwm , items(1,i)
99002          FORMAT (A25,' 6213, MODULE SOFO - ',A4,' IS AN ILLEGAL ITEM NAME')
               dry = -2
            ELSE
               WRITE (nout,99003) uwm , items(1,i) , name(1) , name(2)
!
!     ERROR MESSAGES.
!
99003          FORMAT (A25,' 6211, MODULE SOFO - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
               dry = -2
            ENDIF
         ENDIF
      ENDIF
   ENDDO SPAG_Loop_1_1
   CALL sofcls
END SUBROUTINE sofo
