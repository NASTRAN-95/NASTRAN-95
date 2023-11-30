
SUBROUTINE sofut
   IMPLICIT NONE
   LOGICAL Ditup
   INTEGER Dry , Items(10) , Iz(1) , Name1(2) , Name2(2) , Nout , Oper(2) , Opt , Prefx(2) , Sysbuf
   REAL Sss(33)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Dry , Name1 , Oper , Opt , Name2 , Prefx , Items
   COMMON /sof   / Sss , Ditup
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Iz
   INTEGER dele , i , ib1 , ib2 , ib3 , idest , iedit , iequiv , ii , iprnt , iscr1 , itask , itest , name(2) , nz , renam
   INTEGER ittype , korsz
   EXTERNAL rename
!
!     THE PURPOSE OF THE MODULE IS TO PERFORM THE TASKS OF ALTERING THE
!     SOF FILE IN ORDER TO EDIT, PURGE, AND EQUIVALENCE THE DATA ITEMS
!     OF SELECTED SUBSTRUCTURES.  THE CALLING SEQUENCE TO THE MODULE IS
!
!     SOFUT     //V,N,DRY/C,N,NAME1/C,N,OPER/C,N,OPT/C,N,NAME2/
!                 C,N,PREFX/C,N,IA/C,N,IB/C,N,IC/C,N,ID/C,N,IE $
!
!DME  19 JAN 2016
!DME  D. Everhart
!DME  External statement to prevent collision with GFORTRAN subroutine.
!DME
   DATA iedit , idest , iequiv/4HEDIT , 4HDEST , 4HEQUI/
   DATA iprnt/4HSOFP/
   DATA dele/4HDELE/
   DATA renam/4HRENA/
   DATA name/4HSOFU , 4HT   /
   DATA iscr1/301/
!
   itask = 0
   IF ( Oper(1)==iedit ) itask = 1
   IF ( Oper(1)==idest ) itask = 2
   IF ( Oper(1)==iequiv ) itask = 3
   IF ( Oper(1)==iprnt ) itask = 4
   IF ( Oper(1)==dele ) itask = 5
   IF ( Oper(1)==renam ) itask = 6
   IF ( itask==0 ) THEN
!
!     ERROR MESSAGES
!
      WRITE (Nout,99001) Uwm , Oper(1) , Oper(2)
99001 FORMAT (A25,' 6217, MODULE SOFUT - ',2A4,' IS AN ILLEGAL ','PARAMETER NAME.')
   ELSE
!
!     ALLOCATE BUFFERS FOR THE SOF UTILITY SUBROUTINES
!
      nz = korsz(Iz)
      IF ( 3*Sysbuf>nz ) CALL mesage(-8,0,name(1))
      ib1 = nz - Sysbuf + 1
      ib2 = ib1 - Sysbuf - 1
      ib3 = ib2 - Sysbuf
      CALL sofopn(Iz(ib1),Iz(ib2),Iz(ib3))
      nz = ib3 - 1
      IF ( itask==2 ) THEN
!
!     DESTROY OPERATION
!
         i = nz/2 + 1
         CALL dstroy(Name1(1),itest,Iz,Iz(i),i-1)
      ELSEIF ( itask==3 ) THEN
!
!     EQUIVALENCE OPERATION
!
         i = nz/2 + 1
         CALL seteq(Name1,Name2,Prefx,Dry,itest,Iz,i-1)
      ELSEIF ( itask==4 ) THEN
!
!     PRINT OPERATIONS
!
         IF ( Opt>0 ) GOTO 100
!
!     PRINT SOF TABLE OF CONTENTS (DIT MDI)
!
         CALL softoc
         IF ( Opt/=0 ) GOTO 100
         GOTO 200
      ELSEIF ( itask==5 ) THEN
!
!     DELETE OPERATION
!
         DO i = 1 , 10
            CALL delete(Name1,Items(i),itest)
         ENDDO
      ELSEIF ( itask==6 ) THEN
!
!     RENAME OPERATION
!
         CALL rename(Name1,Name2,Iz(1),nz,itest)
      ELSE
!
!     EDIT OPERATION
!
         CALL edit(Name1(1),Opt,itest)
      ENDIF
!
!     TEST RETURN CODE
!
      IF ( itest==1 .OR. itest==2 .OR. itest==3 .OR. itest==5 .OR. itest==7 ) GOTO 50
      IF ( itest==6 ) THEN
         WRITE (Nout,99002) Uwm , Name1
!
99002    FORMAT (A25,' 6218, MODULE SOFUT - THE SUBSTRUCTURE ',2A4,1X,'CANNOT BE DESTROYED BECAUSE IT IS AN IMAGE SUBSTRUCTURE.')
      ELSEIF ( itest==8 ) THEN
         WRITE (Nout,99003) Uwm , Name2
!
99003    FORMAT (A25,' 6219, MODULE SOFUT - RUN EQUALS DRY OR STEP AND ','SUBSTRUCTURE ',2A4,/33X,                                  &
                &'OR ONE OF THE NEW NAMES ALREADY EXISTS.')
      ELSEIF ( itest==9 ) THEN
         WRITE (Nout,99004) Uwm , Name2
!
99004    FORMAT (A25,' 6220, MODULE SOFUT - RUN = GO AND SUBSTRUCTURE ',2A4,' OR ONE OF THE NEW NAMES DOES NOT EXIST')
      ELSEIF ( itest/=10 ) THEN
         WRITE (Nout,99005) Uwm , Name1
!
99005    FORMAT (A25,' 6212, MODULE SOFUT - THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
      ENDIF
      Dry = -2
 50   CALL sofcls
   ENDIF
   GOTO 99999
!
!     PRINT SOF DATA ITEMS
!
 100  DO i = 1 , 5
      ii = ittype(Items(2*i-1))
      IF ( ii<0 ) THEN
      ELSEIF ( ii==0 ) THEN
!
!     TABLE ITEM
!
         CALL itmprt(Name1,Items(2*i-1),nz,Opt)
      ELSE
!
!     MATRIX ITEM
!
         CALL matwrt(iscr1,Name1,Items(2*i-1),nz)
      ENDIF
!
   ENDDO
 200  CALL sofcls
!
99999 RETURN
END SUBROUTINE sofut