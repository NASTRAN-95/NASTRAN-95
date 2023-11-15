
SUBROUTINE autosv
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1)
   INTEGER Dmpcnt , Ihead , Iseqn , Junk(25) , Junk1(2) , Junk4(2) , Loscar , Maskhi , Nosgn , Nwords , Os(5) , Osbot , Oscar(1) ,  &
         & Ospnt , Osprc , Savnam(100) , Vps(1)
   COMMON /autohd/ Ihead
   COMMON /autosm/ Nwords , Savnam
   COMMON /xgpi4 / Junk4 , Iseqn , Dmpcnt
   COMMON /xgpic / Junk , Maskhi , Junk1 , Nosgn
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER andf , lshift
   INTEGER i , ipt , ist , j , l , ll , n1 , n2 , n3 , nwd , xsav(2)
   EXTERNAL andf , lshift
!
! End of declarations
!
!
!     THIS ROUTINE GENERATES OSCAR ENTRIES FOR PARAMTERS
!     THAT ARE TO BE SAVED IMPLICITLY
!
   EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Os(5),Oscar(1))
   DATA xsav/4HXSAV , 4HE   /
!
!     UPDATE OSCAR PARAMETERS
!
   Ihead = 1
   Osprc = Osbot
   Osbot = Oscar(Osbot) + Osbot
   Ospnt = Osbot
   Iseqn = Oscar(Osprc+1) + 1
!
!     LOAD HEADER
!
   Oscar(Ospnt) = 6
   Oscar(Ospnt+1) = Iseqn
   Oscar(Ospnt+2) = 4 + lshift(8,16)
   Oscar(Ospnt+3) = xsav(1)
   Oscar(Ospnt+4) = xsav(2)
   Oscar(Ospnt+5) = Dmpcnt
   CALL xlnkhd
!
!     HAVING THE VPS POINTERS FOR EACH PARAMETER, FIND THE
!     DISPLACEMENT IN COMMON
!
   j = Osprc + 6 + 3*Oscar(Osprc+6) + 1
   IF ( andf(Oscar(Osprc+2),Maskhi)==1 ) j = j + 1 + 3*Oscar(j)
   j = j + 1
   n3 = j + 1
   n1 = Oscar(j)
   n2 = 1
   Oscar(Ospnt+6) = Nwords
   Oscar(Ospnt) = Oscar(Ospnt) + 1
   ipt = 1
   ist = n3
 100  DO
      IF ( Oscar(ist)>0 ) THEN
!
!     CONSTANT PARAMETER, SKIP IT
!
         nwd = Oscar(ist)
         ist = ist + nwd + 1
         n2 = n2 + nwd
      ELSE
!
!     SEEE IF PARAMETER IS IN SAVE LIST
!
!     LL = ANDF(OSCAR(IST),NOSGN )  REPLACED BY NEXT CARD, OCT. 1983
         ll = andf(Oscar(ist),Maskhi)
         l = andf(Vps(ll-1),Maskhi)
         DO i = 1 , Nwords
            IF ( andf(Oscar(ist),Nosgn)==Savnam(i) ) GOTO 200
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
 200  Oscar(Ospnt+6+2*i-1) = Savnam(ipt)
   Oscar(Ospnt+6+2*i) = n2
   Oscar(Ospnt) = Oscar(Ospnt) + 2
   ipt = ipt + 1
   ist = ist + 1
   n2 = n2 + l
   IF ( ipt<=Nwords ) GOTO 100
   Ihead = 0
END SUBROUTINE autosv
