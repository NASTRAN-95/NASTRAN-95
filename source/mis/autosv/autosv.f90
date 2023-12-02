!*==autosv.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE autosv
   IMPLICIT NONE
   USE c_autohd
   USE c_autosm
   USE c_xgpi4
   USE c_xgpic
   USE c_xvps
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ipt , ist , j , l , ll , n1 , n2 , n3 , nwd , osbot , ospnt , osprc
   INTEGER , DIMENSION(1) :: oscar
   INTEGER , DIMENSION(2) , SAVE :: xsav
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE GENERATES OSCAR ENTRIES FOR PARAMTERS
!     THAT ARE TO BE SAVED IMPLICITLY
!
   !>>>>EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Os(5),Oscar(1))
   DATA xsav/4HXSAV , 4HE   /
!
!     UPDATE OSCAR PARAMETERS
!
   ihead = 1
   osprc = osbot
   osbot = oscar(osbot) + osbot
   ospnt = osbot
   iseqn = oscar(osprc+1) + 1
!
!     LOAD HEADER
!
   oscar(ospnt) = 6
   oscar(ospnt+1) = iseqn
   oscar(ospnt+2) = 4 + lshift(8,16)
   oscar(ospnt+3) = xsav(1)
   oscar(ospnt+4) = xsav(2)
   oscar(ospnt+5) = dmpcnt
   CALL xlnkhd
!
!     HAVING THE VPS POINTERS FOR EACH PARAMETER, FIND THE
!     DISPLACEMENT IN COMMON
!
   j = osprc + 6 + 3*oscar(osprc+6) + 1
   IF ( andf(oscar(osprc+2),maskhi)==1 ) j = j + 1 + 3*oscar(j)
   j = j + 1
   n3 = j + 1
   n1 = oscar(j)
   n2 = 1
   oscar(ospnt+6) = nwords
   oscar(ospnt) = oscar(ospnt) + 1
   ipt = 1
   ist = n3
 100  DO
      IF ( oscar(ist)>0 ) THEN
!
!     CONSTANT PARAMETER, SKIP IT
!
         nwd = oscar(ist)
         ist = ist + nwd + 1
         n2 = n2 + nwd
      ELSE
!
!     SEEE IF PARAMETER IS IN SAVE LIST
!
!     LL = ANDF(OSCAR(IST),NOSGN )  REPLACED BY NEXT CARD, OCT. 1983
         ll = andf(oscar(ist),maskhi)
         l = andf(vps(ll-1),maskhi)
         DO i = 1 , nwords
            IF ( andf(oscar(ist),nosgn)==savnam(i) ) GOTO 200
         ENDDO
!
!     NOT TO BE SAVED, GO TO NEXT PARAMETER
!
         ist = ist + 1
         n2 = n2 + l
      ENDIF
   ENDDO
!
!     PARAMETER TO BE SAVED, PUT IN OSCAR
!
 200  oscar(ospnt+6+2*i-1) = savnam(ipt)
   oscar(ospnt+6+2*i) = n2
   oscar(ospnt) = oscar(ospnt) + 2
   ipt = ipt + 1
   ist = ist + 1
   n2 = n2 + l
   IF ( ipt<=nwords ) GOTO 100
   ihead = 0
END SUBROUTINE autosv
