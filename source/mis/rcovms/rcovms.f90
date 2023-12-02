!*==rcovms.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovms
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eog , eoi , i7 , lama , lams , soln , srd , swrt
   INTEGER :: file , i , itype , n , nw , nwds , rc
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fread , fwdrec , mesage , open , read , sfetch , smsg , sofcls , suread , suwrt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE GENERATES THE MODAL SOLUTION ITEM FOR RIGID FORMAT 3
!
   DATA lams , soln/4HLAMS , 4HSOLN/
   DATA srd , swrt , eog , eoi/1 , 2 , 2 , 3/
   DATA lama/102/ , i7/7/
   DATA name/4HRCOV , 4HMS  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     CREATE SOLN FOR RIGID FORMAT 3
!
         IF ( Mrecvr ) THEN
!
!     FOR MODAL RECOVER COPY THE LAMS ITEM TO SOLN
!
            CALL sfetch(Fss,lams,srd,rc)
            IF ( rc/=1 ) THEN
!
!     ERROR RETURNS
!
               CALL smsg(rc-2,lams,Fss)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL suread(Z(1),-2,n,rc)
               IF ( n>Sof3 ) THEN
                  n = 8
                  CALL mesage(n,file,name)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  CALL sfetch(Fss,soln,swrt,rc)
                  CALL suwrt(Z(1),n,eoi)
                  RETURN
               ENDIF
            ENDIF
         ELSE
!
!     WRITE GROUP 0
!
            rc = 3
            CALL sfetch(Fss,soln,swrt,rc)
            CALL suwrt(Fss,2,1)
            CALL suwrt(Rfno,1,1)
            CALL suwrt(Neigv,1,eog)
!
!     IF NO EIGENVALUES, GO HOME
!
            IF ( Neigv<=0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COPY RECORD 2 OF LAMA OR CLAMA TO GROUP 1 OF SOLN
!
            file = lama
            CALL open(*40,lama,Z(Buf1),Rdrew)
            CALL fwdrec(*60,lama)
            CALL fread(lama,itype,1,1)
            nw = 7
            IF ( itype==90 ) nw = 6
            Z(i7) = 0
            i = 1
            DO
               CALL read(*60,*20,lama,Z(1),nw,0,nwds)
               CALL suwrt(Z,7,i)
            ENDDO
         ENDIF
 20      CALL suwrt(0,0,eog)
         CALL close(lama,Rew)
         spag_nextblock_1 = 2
      CASE (2)
!
!     FINISH
!
         CALL suwrt(0,0,eoi)
         RETURN
 40      n = 1
         CALL mesage(n,file,name)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      n = 2
         CALL mesage(n,file,name)
         spag_nextblock_1 = 3
      CASE (3)
         CALL sofcls
         Iopt = -1
         CALL close(lama,Rew)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rcovms
