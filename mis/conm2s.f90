
SUBROUTINE conm2s
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dmm(2) , Dum(15) , Ecpt(100) , Iner(6) , Mb , Xof , Yof , Zof
   INTEGER Elid , Estid , Iecpt(14) , Ielid , Iprec , Ismb(3) , Nlocs , Nogo
   COMMON /emgdic/ Dmm , Nlocs , Elid , Estid
   COMMON /emgest/ Ecpt
   COMMON /emgprm/ Dum , Ismb , Iprec , Nogo
!
! Local variable declarations
!
   INTEGER dict(11) , i , icidt1 , icidt2 , ij , ip , it , itemp , j , ji
   REAL dict5 , mm(36) , t(36) , tt(36) , x2 , y2 , z2
!
! End of declarations
!
!
! THIS SUBROUTINE COMPUTES THE CONCENTRATED MASS ELEMENTS MASS MATRIX
! FOR THE M2 TYPE ELEMENT
! SINGLE PRECISION VERSION
!
! ECPTNO  NAME       TYPE  DESCRIPTION
! ******  ****       ****  ***********
!
!   1     IELID      I     ELEMENT ID
!   2     IGP        I     GRID POINT NUMBER
!   3     ICIDT2     I     COORDINATE SYSTEM ID FOR T2
!   4     MASS       R     LUMPED MASS
!   5     OFFSET(1)  R
!   6     OFFSET(2)  R     X,Y, AND Z COORDINATES OF THE
!   7     OFFSET(3)  R     OFFSET
!   8     MMI(1,1)   R
!   9     MMI(2,1)   R     MASS MOMENTS OF INERTIA
!  10     MMI(2,2)   R
!  11     MMI(3,1)   R
!  12     MMI(3,2)   R
!  13     MMI(3,3)   R
!  14     ICIDT1     I     COORDINATE SYSTEM ID FOR T1
!  15     X          R
!  16     Y          R
!  17     Z          R
!
!
!
!
!
!
   EQUIVALENCE (Ecpt(1),Iecpt(1),Ielid)
   EQUIVALENCE (dict(5),dict5) , (Ecpt(4),Mb)
   EQUIVALENCE (Ecpt(5),Xof) , (Ecpt(6),Yof) , (Ecpt(7),Zof)
   EQUIVALENCE (Iner(1),Ecpt(8))
!
!     INITIALIZE
!
   IF ( Ismb(2)==0 ) RETURN
   dict(1) = Estid
   dict(2) = 1
   dict(3) = 6
   dict(4) = 63
   dict(5) = 0
   ip = Iprec
!
! COMPUTE NON-TRANSFORMED MASS MATRIX.  INITIALIZE TO ZERO
! THEN FILL IN NON-ZERO TERMS
!
   DO i = 1 , 36
      mm(i) = 0.
   ENDDO
!
   icidt2 = Iecpt(3)
   IF ( icidt2<0 ) THEN
      icidt2 = 0
      DO i = 1 , 3
         Ecpt(i+4) = Ecpt(i+4) - Ecpt(i+14)
      ENDDO
   ENDIF
!
   mm(1) = Mb
   mm(5) = Mb*Zof
   mm(6) = -Mb*Yof
   mm(8) = Mb
   mm(10) = -mm(5)
   mm(12) = Mb*Xof
   mm(15) = Mb
   mm(16) = -mm(6)
   mm(17) = -mm(12)
   mm(20) = mm(10)
   mm(21) = mm(16)
   x2 = Xof**2
   y2 = Yof**2
   z2 = Zof**2
   mm(22) = Iner(1) + (y2+z2)*Mb
   mm(23) = -Iner(2) + mm(6)*Xof
   mm(24) = -Iner(4) + mm(10)*Xof
   mm(25) = mm(5)
   mm(27) = mm(17)
   mm(28) = mm(23)
   mm(29) = Iner(3) + (x2+z2)*Mb
   mm(30) = -Iner(5) + mm(6)*Zof
   mm(31) = mm(6)
   mm(32) = mm(12)
   mm(34) = mm(24)
   mm(35) = mm(30)
   mm(36) = Iner(6) + (x2+y2)*Mb
!
   icidt1 = Iecpt(14)
!
! PERFORM TRANSFORMATIONS.  IF CSIDS 1 AND 2 ARE EQUAL,
! T1 = T2 SO MASS MATRIX IS COMPLETE
!
   IF ( icidt2/=icidt1 ) THEN
!                            T
! NOT EQUAL SO COMPUTE T = (T )(T )
!                            1   2
! GET T1 AND T2 IF NEEDED
      it = 18
      IF ( icidt1==0 ) THEN
! ONLY T2 NEEDED SO T = T2
         it = 9
      ELSE
!
         CALL transs(Ecpt(14),t(1))
      ENDIF
      IF ( icidt2==0 ) THEN
!
! HERE T2 IS IDENTITY AND T1 IS AT T(1) SO
! T = T1 (TRANSPOSE).  SO INSERT INTO T
         DO i = 1 , 3
            DO j = 1 , 3
               ij = 3*(i-1) + j
               ji = i + 3*(j-1) + 18
               t(ji) = t(ij)
            ENDDO
         ENDDO
      ELSE
         itemp = Iecpt(14)
         Iecpt(14) = icidt2
         CALL transs(Ecpt(14),t(10))
         Iecpt(14) = itemp
!
         IF ( icidt1/=0 ) CALL gmmats(t(1),3,3,2,t(10),3,3,0,t(19))
      ENDIF
!
! T = (T ) (T ) IS COMPLETE. INSERT IT IN THE 6X6 TRANSFORMATION MATRIX.
!       1    2
!
      DO i = 1 , 36
         tt(i) = 0.
      ENDDO
!
      DO i = 1 , 3
         ij = i + it
         tt(i) = t(ij)
         tt(i+6) = t(ij+3)
         tt(i+12) = t(ij+6)
         tt(i+21) = t(ij)
         tt(i+27) = t(ij+3)
         tt(i+33) = t(ij+6)
      ENDDO
!           T
! FORM T*M*T  AND STORE IN MM
!
      CALL gmmats(tt(1),6,6,0,mm(1),6,6,0,t(1))
      CALL gmmats(t(1),6,6,0,tt(1),6,6,1,mm(1))
   ENDIF
!
   CALL emgout(mm,mm,36,1,dict,2,ip)
END SUBROUTINE conm2s
