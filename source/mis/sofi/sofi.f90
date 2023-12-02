!*==sofi.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sofi
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
   EXTERNAL korsz , mesage , mtrxi , rdtrl , sofcls , sofopn , softrl
!
! End of declarations rewritten by SPAG
!
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
      IF ( items(1,i)==xxxx .OR. items(1,i)==0 ) items(1,i) = iblnk
   ENDDO
!
   nz = korsz(iz)
   IF ( 3*sysbuf>nz ) CALL mesage(-8,0,modnam(1))
   ib1 = nz - sysbuf + 1
   ib2 = ib1 - sysbuf - 1
   ib3 = ib2 - sysbuf
   CALL sofopn(iz(ib1),iz(ib2),iz(ib3))
   IF ( dry>=0 ) THEN
!
!     COPY SOF DATA INTO NASTRAN DATA BLOCKS
!
      SPAG_Loop_1_1: DO i = 1 , 5
         IF ( items(1,i)/=iblnk ) THEN
            mcb(1) = file(i)
            CALL rdtrl(mcb)
            IF ( mcb(1)>=0 ) THEN
               CALL mtrxi(file(i),name(1),items(1,i),0,itest)
               IF ( itest==1 .OR. itest==6 ) CYCLE
               IF ( itest==3 ) THEN
                  WRITE (nout,99002) uwm , items(1,i) , name(1) , name(2)
                  CYCLE
               ELSEIF ( itest==4 ) THEN
                  WRITE (nout,99003) uwm , name(1) , name(2)
                  dry = -2
                  EXIT SPAG_Loop_1_1
               ELSEIF ( itest==5 ) THEN
                  WRITE (nout,99004) uwm , items(1,i)
               ELSE
                  WRITE (nout,99001) uwm , items(1,i) , name(1) , name(2)
99001             FORMAT (A25,' 6215, MODULE SOFI - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
               ENDIF
               dry = -2
            ENDIF
         ENDIF
      ENDDO SPAG_Loop_1_1
   ELSE
!
!     CHECK THE EXISTENCE OF THE SOF FILE.
!
      SPAG_Loop_1_2: DO i = 1 , 5
         IF ( items(1,i)/=iblnk ) THEN
            mcb(1) = file(i)
            CALL rdtrl(mcb)
            IF ( mcb(1)>=0 ) THEN
               CALL softrl(name(1),items(1,i),mcb)
               itest = mcb(1)
               IF ( itest==1 .OR. itest==2 ) CYCLE
               IF ( itest==4 ) THEN
                  WRITE (nout,99003) uwm , name(1) , name(2)
                  dry = -2
                  EXIT SPAG_Loop_1_2
               ELSEIF ( itest==5 ) THEN
                  WRITE (nout,99004) uwm , items(1,i)
               ELSE
                  WRITE (nout,99002) uwm , items(1,i) , name(1) , name(2)
               ENDIF
               dry = -2
            ENDIF
         ENDIF
      ENDDO SPAG_Loop_1_2
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
