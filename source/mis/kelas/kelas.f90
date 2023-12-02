!*==kelas.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE kelas(Ijklmn)
   IMPLICIT NONE
   USE C_SMA1BK
   USE C_SMA1CL
   USE C_SMA1ET
   USE C_SMA1IO
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ijklmn
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iarg , ifile , ii , ind , index , inpdof , inpvt , ipdof , ipvt , iretrn , j , jj
   INTEGER , DIMENSION(5) :: iecpt
   INTEGER , SAVE :: iscalr
   REAL*8 :: ke
   REAL :: save
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE COMPUTES THE ELEMENT STIFFNESS AND STIFFNESS DAMPING
! 1 X 1 MATRICES FOR ELEMENTS ELAS1, ELAS2, ELAS3, ELAS4.
!*****
!
!
!
!              E C P T - S  F O R  E L A S  E L E M E N T S
!
!
!
!                  TYPE             TYPE           TYPE           TYPE
!         CELAS1           CELAS2         CELAS3         CELAS4
! ECPT(1) IELID     I      IELID     I    IELID      I   IELID      I
! ECPT(2) IGP1      I      K         R    IS1        I   K          R
! ECPT(3) IGP2      I      IGP1      I    IS2        I   IS1        I
! ECPT(4) IC1       I      IGP2      I    K          R   IS2        I
! ECPT(5) IC2       I      IC1       I    GSUBE      R
! ECPT(6) K         R      IC2       I    S          R
! ECPT(7) GSUBE     R      GSUBE     R
! ECPT(8) S         R      S         R
!
!
!
!
!
!
!
!
!
!
! SMA1 I/O PARAMETERS
!
!
! SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
! SMA1 PROGRAM CONTROL PARAMETERS
!
!
! ECPT COMMON BLOCK
!
!
!
!
   !>>>>EQUIVALENCE (Iecpt(1),Ecpt(1))
!
!
!
   DATA iscalr/0/
!
!
!
   iarg = Ijklmn
!
! MAKE THE ECPT-S FOR ALL ELAS ELEMENTS LOOK EXACTLY LIKE THE ECPT FOR
! ELAS1
!
   IF ( iarg==1 ) THEN
   ELSEIF ( iarg==3 ) THEN
!
! ELAS3
!
      Ecpt(7) = Ecpt(5)
      Ecpt(6) = Ecpt(4)
      iecpt(4) = 1
      iecpt(5) = 1
   ELSEIF ( iarg==4 ) THEN
!
! ELAS4
!
      Ecpt(6) = Ecpt(2)
      iecpt(2) = iecpt(3)
      iecpt(3) = iecpt(4)
      iecpt(4) = 1
      iecpt(5) = 1
   ELSE
!
! ELAS2
!
      save = Ecpt(2)
      DO i = 3 , 6
         iecpt(i-1) = iecpt(i)
      ENDDO
      Ecpt(6) = save
   ENDIF
!
! DETERMINE WHICH POINT IS THE PIVOT POINT AND SET APPROPRIATE POINTERS
!
   ind = 2
   IF ( iecpt(2)==Npvt ) THEN
!
! CHECK TO SEE IF BOTH POINTS MATCH THE PIVOT POINT.
!
      IF ( iecpt(3)==Npvt ) THEN
         IF ( iscalr==0 ) THEN
            iscalr = 1
            ind = 4
         ELSE
            iscalr = 0
            RETURN
         ENDIF
      ENDIF
      ipvt = 2
      ipdof = 4
      inpvt = 3
      inpdof = 5
      IF ( iecpt(3)==0 ) ind = 1
   ELSE
      IF ( iecpt(3)/=Npvt ) RETURN
      ipvt = 3
      ipdof = 5
      inpvt = 2
      inpdof = 4
      IF ( iecpt(2)==0 ) ind = 1
   ENDIF
   IF ( iecpt(ipdof)<=0 ) iecpt(ipdof) = 1
   IF ( iecpt(inpdof)<=0 ) iecpt(inpdof) = 1
!
! II AND JJ ARE THE ROW AND COLUMN INDICES OF THE MATRIX INTO WHICH THE
! SPRING AND SPRING DAMPING CONSTANTS WILL BE ADDED.
!
   ii = iecpt(ipvt) + iecpt(ipdof) - 1
   jj = iecpt(inpvt) + iecpt(inpdof) - 1
   ke = Ecpt(6)
   index = 6
   ifile = Ifkgg
 100  ASSIGN 300 TO iretrn
   i = ii
   j = ii
 200  CALL sma1b(ke,j,i,ifile,0.0D0)
   IF ( ind==1 ) GOTO 600
   GOTO iretrn
 300  ASSIGN 400 TO iretrn
   ke = -ke
   j = jj
   GOTO 200
 400  IF ( ind/=4 ) GOTO 600
   ASSIGN 500 TO iretrn
   ke = Ecpt(6)
   i = jj
   GOTO 200
 500  ASSIGN 600 TO iretrn
   ke = -ke
   j = ii
   GOTO 200
 600  IF ( index==7 ) RETURN
   IF ( Iopt4==0 .OR. iarg==4 ) RETURN
!
! IF G SUB E IS NON-ZERO, SET PARAMETERS FOR K4GG INSERTION.
!
   IF ( Ecpt(7)==0.0 ) RETURN
   K4ggsw = 1
   ifile = If4gg
   ke = Ecpt(7)*Ecpt(6)
   index = 7
   GOTO 100
END SUBROUTINE kelas
