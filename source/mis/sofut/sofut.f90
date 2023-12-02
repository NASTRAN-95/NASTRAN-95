!*==sofut.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sofut
   USE c_blank
   USE c_sof
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: dele , idest , iedit , iequiv , iprnt , iscr1 , renam
   INTEGER :: i , ib1 , ib2 , ib3 , ii , itask , itest , nz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL delete , dstroy , edit , itmprt , ittype , korsz , matwrt , mesage , rename , seteq , sofcls , sofopn , softoc
!
! End of declarations rewritten by SPAG
!
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
   IF ( oper(1)==iedit ) itask = 1
   IF ( oper(1)==idest ) itask = 2
   IF ( oper(1)==iequiv ) itask = 3
   IF ( oper(1)==iprnt ) itask = 4
   IF ( oper(1)==dele ) itask = 5
   IF ( oper(1)==renam ) itask = 6
   IF ( itask==0 ) THEN
!
!     ERROR MESSAGES
!
      WRITE (nout,99001) uwm , oper(1) , oper(2)
99001 FORMAT (A25,' 6217, MODULE SOFUT - ',2A4,' IS AN ILLEGAL ','PARAMETER NAME.')
   ELSE
!
!     ALLOCATE BUFFERS FOR THE SOF UTILITY SUBROUTINES
!
      nz = korsz(iz)
      IF ( 3*sysbuf>nz ) CALL mesage(-8,0,name(1))
      ib1 = nz - sysbuf + 1
      ib2 = ib1 - sysbuf - 1
      ib3 = ib2 - sysbuf
      CALL sofopn(iz(ib1),iz(ib2),iz(ib3))
      nz = ib3 - 1
      IF ( itask==2 ) THEN
!
!     DESTROY OPERATION
!
         i = nz/2 + 1
         CALL dstroy(name1(1),itest,iz,iz(i),i-1)
      ELSEIF ( itask==3 ) THEN
!
!     EQUIVALENCE OPERATION
!
         i = nz/2 + 1
         CALL seteq(name1,name2,prefx,dry,itest,iz,i-1)
      ELSEIF ( itask==4 ) THEN
!
!     PRINT OPERATIONS
!
         IF ( opt>0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
!
!     PRINT SOF TABLE OF CONTENTS (DIT MDI)
!
         CALL softoc
         IF ( opt==0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         CALL spag_block_1
         RETURN
      ELSEIF ( itask==5 ) THEN
!
!     DELETE OPERATION
!
         DO i = 1 , 10
            CALL delete(name1,items(i),itest)
         ENDDO
      ELSEIF ( itask==6 ) THEN
!
!     RENAME OPERATION
!
         CALL rename(name1,name2,iz(1),nz,itest)
      ELSE
!
!     EDIT OPERATION
!
         CALL edit(name1(1),opt,itest)
      ENDIF
!
!     TEST RETURN CODE
!
      IF ( itest/=1 .AND. itest/=2 .AND. itest/=3 .AND. itest/=5 .AND. itest/=7 ) THEN
         IF ( itest==6 ) THEN
            WRITE (nout,99002) uwm , name1
!
99002       FORMAT (A25,' 6218, MODULE SOFUT - THE SUBSTRUCTURE ',2A4,1X,'CANNOT BE DESTROYED BECAUSE IT IS AN IMAGE SUBSTRUCTURE.')
         ELSEIF ( itest==8 ) THEN
            WRITE (nout,99003) uwm , name2
!
99003       FORMAT (A25,' 6219, MODULE SOFUT - RUN EQUALS DRY OR STEP AND ','SUBSTRUCTURE ',2A4,/33X,                               &
                   &'OR ONE OF THE NEW NAMES ALREADY EXISTS.')
         ELSEIF ( itest==9 ) THEN
            WRITE (nout,99004) uwm , name2
!
99004       FORMAT (A25,' 6220, MODULE SOFUT - RUN = GO AND SUBSTRUCTURE ',2A4,' OR ONE OF THE NEW NAMES DOES NOT EXIST')
         ELSEIF ( itest/=10 ) THEN
            WRITE (nout,99005) uwm , name1
!
99005       FORMAT (A25,' 6212, MODULE SOFUT - THE SUBSTRUCTURE ',2A4,' DOES NOT EXIST.')
         ENDIF
         dry = -2
      ENDIF
      CALL sofcls
   ENDIF
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
!     PRINT SOF DATA ITEMS
!
      DO I = 1 , 5
         Ii = ittype(items(2*I-1))
         IF ( Ii<0 ) THEN
         ELSEIF ( Ii==0 ) THEN
!
!     TABLE ITEM
!
            CALL itmprt(name1,items(2*I-1),Nz,Opt)
         ELSE
!
!     MATRIX ITEM
!
            CALL matwrt(Iscr1,name1,items(2*I-1),Nz)
         ENDIF
!
      ENDDO
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      CALL sofcls
   END SUBROUTINE spag_block_2
!
END SUBROUTINE sofut
