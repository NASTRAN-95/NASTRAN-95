
SUBROUTINE conm1s
   IMPLICIT NONE
   REAL Dmm(2) , Dum(15) , Ecpt(100) , M(1) , Ss , Xof , Yof , Zof
   INTEGER Elid , Estid , Iecpt(25) , Ielid , Ioutpt , Iprec , Ismb(3) , Ksystm(56) , Mb , Nlocs
   LOGICAL Nogo
   COMMON /emgdic/ Dmm , Nlocs , Elid , Estid
   COMMON /emgest/ Ecpt
   COMMON /emgprm/ Dum , Ismb , Iprec , Nogo
   COMMON /system/ Ss , Ioutpt , Ksystm
   INTEGER dict(7) , i , icidt1 , icidt2 , ij , ip , it , j , ji , k
   REAL dict5 , mm(36) , t(36) , tt(36)
!
! THIS SUBROUTINE COMPUTES THE CONCENTRATED MASS ELEMENTS
! MASS MATRIX FOR THE M1 TYPE ELEMENT
! SINGLE PRECISION VERSION
!
!
! ECPT NO.  NAME              TYPE   DESCRIPTION
! 1         IELID             I      ELEMENT ID
! 2         IGP               I      GRID POINT NUMBER
! 3         ICIDT2            I      COORDINATE ID FOR T2
! 4         M(1,1)            R
! 5, 6      M(2,1) TO M(2,2)  R
! 7, 8, 9   M(3,1) TO M(3,3)  R      MASS MATRIX VALUES
! 10 TO 13  M(4,1) TO M(4,4)  R
! 14 TO 18  M(5,1) TO M(5,5)  R
! 19 TO 24  M(6,1) TO M(6,6)  R
! 25        ICIDT1            I      COORDINATE ID FOR T1
! 26        X                 R
! 27        Y                 R      TRANSFORMATION MATRIX
! 28        Z                 R
!
!
!
!
!
   !>>>>EQUIVALENCE (Ecpt(1),Iecpt(1),Ielid)
   !>>>>EQUIVALENCE (Ecpt(4),M(1))
   !>>>>EQUIVALENCE (dict(5),dict5) , (Ecpt(4),Mb)
   !>>>>EQUIVALENCE (Ecpt(5),Xof) , (Yof,Ecpt(6)) , (Zof,Ecpt(7))
!
!     INITIALIZE
!
   IF ( Ismb(2)==0 ) RETURN
   dict(1) = Estid
   dict(2) = 1
   dict(3) = 6
   dict(4) = 63
   dict5 = 0
   ip = Iprec
!
! COMPUTE NON-TRANSFORMED MASS MATRIX. INITIALIZE
! TO ZERO THEN FILL IN NON-ZERO TERMS
!
   DO i = 1 , 36
      mm(i) = 0.
   ENDDO
!
   k = 0
   DO i = 1 , 6
      DO j = 1 , i
         k = k + 1
         ji = (j-1)*6 + i
         ij = (i-1)*6 + j
         mm(ij) = M(k)
         mm(ji) = M(k)
      ENDDO
   ENDDO
!
   icidt1 = Iecpt(25)
   icidt2 = Iecpt(3)
!
! PERFORM TRANSFORMATIONS.  IF CSIDS 1 AND 2 ARE EQUAL,
! T1 = T2 SO MASS MARRIX IS COMPLETE
!
   IF ( icidt2/=icidt1 ) THEN
!                             T
! NOT EQUAL. SO COMPUTE T = (T ) (T )
!                             1    2
! GET T1 AND T2 IF NEEDED
      it = 18
      IF ( icidt1==0 ) THEN
!
! ONLY T2 NEEDED SO T = T2
!
         it = 9
      ELSE
!
         CALL transs(Ecpt(25),t(1))
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
         CALL transs(Ecpt(25),t(10))
!
         IF ( icidt1/=0 ) CALL gmmats(t(1),3,3,1,t(10),3,3,0,t(19))
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
END SUBROUTINE conm1s