
SUBROUTINE rcovms
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Dry , Fss(2) , Icore , Iopt , Ireq , Lbasic , Lcore , Loop , Lreq , Lui , Neigv , Norew , Nosort , Rd , Rdrew ,   &
         & Rew , Rfno , Sof3 , Step , Wrt , Wrtrew , Z(1)
   REAL Buf2 , Buf3 , Buf4 , Energy , Eofnrw , Pa , Pthres , Qa , Qthres , Range(2) , Rss(2) , Sof1 , Sof2 , Ua , Uimpro ,          &
      & Uinms(2,5) , Uthres
   LOGICAL Mrecvr
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER eog , eoi , file , i , i7 , itype , lama , lams , n , name(2) , nw , nwds , rc , soln , srd , swrt
!
! End of declarations
!
!
!     THIS ROUTINE GENERATES THE MODAL SOLUTION ITEM FOR RIGID FORMAT 3
!
   DATA lams , soln/4HLAMS , 4HSOLN/
   DATA srd , swrt , eog , eoi/1 , 2 , 2 , 3/
   DATA lama/102/ , i7/7/
   DATA name/4HRCOV , 4HMS  /
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
         GOTO 500
      ELSE
         CALL suread(Z(1),-2,n,rc)
         IF ( n>Sof3 ) THEN
            n = 8
            CALL mesage(n,file,name)
            GOTO 500
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
      IF ( Neigv<=0 ) GOTO 200
!
!     COPY RECORD 2 OF LAMA OR CLAMA TO GROUP 1 OF SOLN
!
      file = lama
      CALL open(*300,lama,Z(Buf1),Rdrew)
      CALL fwdrec(*400,lama)
      CALL fread(lama,itype,1,1)
      nw = 7
      IF ( itype==90 ) nw = 6
      Z(i7) = 0
      i = 1
      DO
         CALL read(*400,*100,lama,Z(1),nw,0,nwds)
         CALL suwrt(Z,7,i)
      ENDDO
   ENDIF
 100  CALL suwrt(0,0,eog)
   CALL close(lama,Rew)
!
!     FINISH
!
 200  CALL suwrt(0,0,eoi)
   RETURN
 300  n = 1
   CALL mesage(n,file,name)
   GOTO 500
 400  n = 2
   CALL mesage(n,file,name)
 500  CALL sofcls
   Iopt = -1
   CALL close(lama,Rew)
END SUBROUTINE rcovms
