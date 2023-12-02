!*==rod.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rod
   IMPLICIT NONE
   USE C_CONDAS
   USE C_MATIN
   USE C_MATOUT
   USE C_SSGETT
   USE C_SSGWRK
   USE C_TRIMEX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a
   REAL , DIMENSION(3) :: arry
   REAL , DIMENSION(1) :: gpida1 , gpidb1
   INTEGER :: i , icstma , icstmb , nept
   EXTERNAL basglb , fedt , mat , norm , ssgetd
!
! End of declarations rewritten by SPAG
!
!
!     ELEMENT TEMPERATURE AND DEFORMATION LOADING FOR THE ROD, CONROD,
!     TUBE
!
   !>>>>EQUIVALENCE (Iarry(1),Arry(1)) , (Icstma,Bgpdt(1)) , (Icstmb,Bgpdt(5)) , (Gpida1(1),Bgpdt(2)) , (Gpidb1(1),Bgpdt(6))
!
   nept = 5
   IF ( Eltype==3 ) nept = 4
   a = arry(2)
!
!     RECOMPUTE AREA IF ELEMENT IS TUBE
!
   IF ( nept==4 ) a = Pi*(a-arry(3))*arry(3)
!
   DO i = 1 , 9
      nept = nept + 1
      Bgpdt(i) = arry(nept)
   ENDDO
!
!     OBTAIN THE MATERIAL DATA
!
   Inflag = 1
   Matid = Iarry(1)
   Temp = Bgpdt(9)
   CALL mat(Eid)
   IF ( Itemp/=0 ) THEN
      CALL ssgetd(Eid,Ti,0)
      Tbar = Ti(1) - To1
   ELSE
      Tbar = 0.0
   ENDIF
   IF ( Ideft/=0 ) THEN
      CALL fedt(Eid,Delta,Idefm)
   ELSE
      Delta = 0.0
   ENDIF
   DO i = 1 , 3
      Vect(i) = gpida1(i) - gpidb1(i)
   ENDDO
   CALL norm(Vect(1),Xl)
   Vmag = E1*a*(Delta+Alpha*Xl*Tbar)/Xl
   DO i = 1 , 3
      Vect(i) = -Vect(i)*Vmag
      Force(i) = -Vect(i)
   ENDDO
   IF ( icstmb/=0 ) CALL basglb(Vect(1),Vect(1),gpidb1,icstmb)
   In = Gpidb - 1
   DO i = 1 , 3
      L = In + i
      Core(L) = Core(L) + Vect(i)
   ENDDO
   IF ( icstma/=0 ) CALL basglb(Force(1),Force(1),gpida1,icstma)
   In = Gpida - 1
   DO i = 1 , 3
      L = In + i
      Core(L) = Core(L) + Force(i)
   ENDDO
END SUBROUTINE rod
