
SUBROUTINE dupart
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Iabit(16) , Ibit(32) , Major(2) , Sub0(2) , Sub1(2)
   COMMON /bitpos/ Ibit , Iabit
   COMMON /blank / Major , Sub0 , Sub1
!
! Local variable declarations
!
   INTEGER i , ib(3) , j , kff , kfs , knn , ksf , kss , name(2) , nogo , scr1 , uset
!
! End of declarations
!
!
!     DRIVER FOR DMAP MODULE UPARTN
!
!     DMAP CALLING SEQUENCE IS
!     UPARTN    USET,KNN/KFF,KSF,KFS,KSS/C,N,N/C,N,F/C,N,S $
!
   DATA name/4HUPAR , 4HTN  /
   DATA uset , knn , kff , ksf , kfs , kss , scr1/101 , 102 , 201 , 202 , 203 , 204 , 301/
!
!
   nogo = 0
!
!     DECIDE IF CHARACTERS ARE LEGAL BIT NUMBERS
!
   ib(1) = Major(1)
   ib(2) = Sub0(1)
   ib(3) = Sub1(1)
!
   DO j = 1 , 3
      DO i = 1 , 32
         IF ( ib(j)==Iabit(i) ) THEN
            ib(j) = Ibit(i)
            GOTO 100
         ENDIF
      ENDDO
!
!     INVALID
      CALL mesage(59,ib(j),name)
      nogo = 1
 100  ENDDO
!
   IF ( nogo==1 ) CALL mesage(-7,0,name)
!
   CALL upart(uset,scr1,ib(1),ib(2),ib(3))
   CALL mpart(knn,kff,ksf,kfs,kss)
END SUBROUTINE dupart
