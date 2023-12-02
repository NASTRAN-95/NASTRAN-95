!*==rod.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rod
   USE c_condas
   USE c_matin
   USE c_matout
   USE c_ssgett
   USE c_ssgwrk
   USE c_trimex
   USE c_zzzzzz
   IMPLICIT NONE
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
   IF ( eltype==3 ) nept = 4
   a = arry(2)
!
!     RECOMPUTE AREA IF ELEMENT IS TUBE
!
   IF ( nept==4 ) a = pi*(a-arry(3))*arry(3)
!
   DO i = 1 , 9
      nept = nept + 1
      bgpdt(i) = arry(nept)
   ENDDO
!
!     OBTAIN THE MATERIAL DATA
!
   inflag = 1
   matid = iarry(1)
   temp = bgpdt(9)
   CALL mat(eid)
   IF ( itemp/=0 ) THEN
      CALL ssgetd(eid,ti,0)
      tbar = ti(1) - to1
   ELSE
      tbar = 0.0
   ENDIF
   IF ( ideft/=0 ) THEN
      CALL fedt(eid,delta,idefm)
   ELSE
      delta = 0.0
   ENDIF
   DO i = 1 , 3
      vect(i) = gpida1(i) - gpidb1(i)
   ENDDO
   CALL norm(vect(1),xl)
   vmag = e1*a*(delta+alpha*xl*tbar)/xl
   DO i = 1 , 3
      vect(i) = -vect(i)*vmag
      force(i) = -vect(i)
   ENDDO
   IF ( icstmb/=0 ) CALL basglb(vect(1),vect(1),gpidb1,icstmb)
   in = gpidb - 1
   DO i = 1 , 3
      l = in + i
      core(l) = core(l) + vect(i)
   ENDDO
   IF ( icstma/=0 ) CALL basglb(force(1),force(1),gpida1,icstma)
   in = gpida - 1
   DO i = 1 , 3
      l = in + i
      core(l) = core(l) + force(i)
   ENDDO
END SUBROUTINE rod
