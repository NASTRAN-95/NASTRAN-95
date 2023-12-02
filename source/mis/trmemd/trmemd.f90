!*==trmemd.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trmemd
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_emgtrx
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(10) :: dict
   REAL :: dict5
   INTEGER :: i , ielid , iheat , ii , ij , ik , iout , ioutpt , ip , ip1 , isave , j , jj , ka , l
   INTEGER , DIMENSION(3) , SAVE :: ipart
   REAL(REAL64) , DIMENSION(9) :: m , mout
   INTEGER , DIMENSION(50) :: necpt
   INTEGER , DIMENSION(3) :: ngrid
   EXTERNAL ektrmd , emadtq , emgout
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE CALCULATES THE STIFFNESS AND MASS MATRICES FOR
!     THE  TRIANGULAR MEMBRANE ELEMENT.  CALCULATIONS ARE PERFORMED
!     PRIMARILY BY SUBROUTINES EKTRMS AND EMASTQ.
!     DOUBLE PRECISION VERSION
!
!     ECPT FOR THE TRMEM ELEMENT
!***********************************************************************
! INDEX   DESCRIPTION                                       TYPE
! *****   ***********                                       ****
!   1     ELEMENT ID                                         I
!   2-4   GRID POINTS A,B,AND C                              I
!   5     THETA = ANGLE OF MATERIAL                          R
!   6     MATERIAL ID                                        I
!   7     T                                                  R
!   8     NON-STRUCTURAL MASS                                R
!   9     COORDINATE SYSTEM ID 1                             I
! 10-12   X1,Y1,Z1                                           R
!  13     COORDINATE SYSTEM ID 2                             I
! 14-16   X2,Y2,Z2                                           R
!  17     COORDINATE SYSTEM ID 3                             I
! 18-20   X3,Y3,Z3                                           R
!  21     ELEMENT TEMPERATURE                                R
!***********************************************************************
!
!
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1),Ielid) , (dict5,dict(5))
   !>>>>EQUIVALENCE (K(1),M(1)) , (Kout(1),Mout(1)) , (Ksystm(2),Ioutpt)
   !>>>>EQUIVALENCE (Ksystm(56),Iheat) , (Ecpt(2),Ngrid(1))
!
   DATA ipart/1 , 2 , 3/
!
!
!
   ip = iprec
   dict(1) = estid
   SPAG_Loop_1_1: DO
!
!     CREATE AN ARRAY POINTING TO GRID POINTS IN INCREASING ORDER
!
      DO i = 1 , 2
         ip1 = i + 1
         ii = ipart(i)
         DO j = ip1 , 3
            jj = ipart(j)
            IF ( ngrid(ii)>ngrid(jj) ) THEN
               ipart(i) = jj
               ipart(j) = ii
               ii = jj
               CYCLE SPAG_Loop_1_1
            ENDIF
         ENDDO
      ENDDO
!
!     IF STIFFNESS MATRIX IS REQUESTED CALL EKTRMS. OTHERWISE GO TO
!     MASS MATRIX CALCULATION SECTION
!
      IF ( ismb(1)/=0 ) THEN
!
         CALL ektrmd(0)
!
         IF ( nogo ) RETURN
!
!     RE-ORDER  THE STIFFNESS MATRIX BY INCREASING SIL VALUE
!
         IF ( heat ) THEN
!
!     OUTPUT HEAT MATRIX HERE
!
            DO i = 1 , 3
               DO j = 1 , 3
                  iout = (i-1)*3 + j
                  ik = (ipart(i)-1)*3 + ipart(j)
                  k(iout) = ksave(ik)
               ENDDO
            ENDDO
!     OUTPUT   HEAT  K
            dict(2) = 1
            dict(3) = 3
            dict(4) = 1
!
            CALL emgout(k,k,9,1,dict,1,ip)
         ELSE
            DO i = 1 , 3
               ii = ipart(i)
               DO j = 1 , 3
                  jj = ipart(j)
                  DO ka = 1 , 3
                     DO l = 1 , 3
                        isave = (ii-1)*27 + (jj-1)*9 + (ka-1)*3 + l
                        iout = (i-1)*27 + (j-1)*3 + (ka-1)*9 + l
                        k(iout) = ksave(isave)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
!    OUTPUT THE MATRIX
            dict(2) = 1
            dict(3) = 9
            dict(4) = 7
!
            CALL emgout(k,k,81,1,dict,1,ip)
         ENDIF
      ENDIF
!
!     PERFORM MASS MATRIX CALCULATIONS HERE
!
      IF ( ismb(2)==0 ) RETURN
!
!     CONVENTIONAL MASS MATRIX
!
      CALL emadtq(4,m)
!     REORDER THE MASS MATRIX
      IF ( .NOT.(heat) ) EXIT SPAG_Loop_1_1
!
!     HEAT FORMULATION
!
      DO i = 1 , 3
         j = ipart(i)
         mout(i) = m(j)
      ENDDO
      dict(2) = 2
      dict(3) = 3
      dict(4) = 1
!
      CALL emgout(mout,mout,3,1,dict,2,ip)
      RETURN
   ENDDO SPAG_Loop_1_1
   DO i = 1 , 3
      ii = (i-1)*3
      ij = ipart(i)
      jj = (ij-1)*3
      DO j = 1 , 3
         iout = ii + j
         ik = jj + j
         mout(iout) = m(ik)
      ENDDO
   ENDDO
!
   dict(2) = 2
   dict(3) = 9
   dict(4) = 7
!
   CALL emgout(mout,mout,9,1,dict,2,ip)
!
END SUBROUTINE trmemd
