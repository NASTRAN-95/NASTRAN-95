!*==errmkn.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE errmkn(N,Ierr)
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
   INTEGER :: Ierr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(26) , SAVE :: isubr
   EXTERNAL mesage , sofcls
!
! End of declarations rewritten by SPAG
!
!
!     SENDS ERROR MESSAGES.  N IS THE INDEX OF THE SUBROUTINE CALLING
!     ERROR, AND IERR IS AN ERROR CODE.
!
   DATA isubr/4HCRSU , 4HB    , 4HDSTR , 4HOY   , 4HFDIT , 4H     , 4HFMDI , 4H     , 4HFNXT , 4H     , 4HGETB , 4HLK   , 4HRETB ,  &
       &4HLK   , 4HSETE , 4HQ    , 4HSJUM , 4HP    , 4HSURE , 4HAD   , 4HRENA , 4HME   , 4HEXO2 , 4H     , 4HEXIO , 4H1   /
!
   WRITE (nout,99001) sfm , isubr(N) , isubr(N+1)
!
99001 FORMAT (A25,' 6224, SOF UTILITY SUBROUTINE ',2A4)
   CALL sofcls
   IF ( Ierr==2 ) THEN
      WRITE (nout,99002)
99002 FORMAT (5X,'ILLEGAL BLOCK NUMBER')
   ELSEIF ( Ierr==3 ) THEN
      WRITE (nout,99003)
99003 FORMAT (5X,'ERROR IN SETTING UP THE LIST IMORE')
   ELSEIF ( Ierr==4 ) THEN
      WRITE (nout,99004)
99004 FORMAT (5X,'NXTCUR IS TOO LARGE')
   ELSEIF ( Ierr==5 ) THEN
      WRITE (nout,99005)
99005 FORMAT (5X,'ERROR IN UPDATING DIT')
   ELSEIF ( Ierr==6 ) THEN
      WRITE (nout,99006)
99006 FORMAT (5X,'ERROR IN UPDATING MDI')
   ELSEIF ( Ierr==7 ) THEN
      WRITE (nout,99007)
99007 FORMAT (5X,'ERROR IN LINKING BLOCKS OF DIT')
   ELSEIF ( Ierr==8 ) THEN
      WRITE (nout,99008)
99008 FORMAT (5X,'LINK THROUGH COMBINED SUBSTRUCTURES IS NOT CIRCULAR')
   ELSEIF ( Ierr==9 ) THEN
      WRITE (nout,99009)
99009 FORMAT (5X,'ERROR IN LINKING SOF BLOCKS')
   ELSEIF ( Ierr==10 ) THEN
      WRITE (nout,99010)
99010 FORMAT (5X,'INTERNAL ARRAY DIMENSION EXCEEDED')
   ELSE
      WRITE (nout,99011)
99011 FORMAT (5X,'I IS TOO LARGE OR NXTTSZ HAS NOT BEEN PROPERLY ','UPDATED')
   ENDIF
   CALL mesage(-61,0,0)
END SUBROUTINE errmkn
