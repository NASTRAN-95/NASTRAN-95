
SUBROUTINE trmems
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(225) , Bfact , Dm(15) , E(18) , Ecpt(50) , K(324) , Kout(324) , Ksave(81) , M(1) , Mout(1) , Prod9(9) , Qq(3) , Temp9(9) ,&
      & Xsub(3)
   INTEGER Elid , Estid , Icmbar , Ielid , Iheat , Ioutpt , Iprec , Ismb(3) , Ksystm(60) , Necpt(50) , Ngrid(3)
   LOGICAL Heat , Nogo
   COMMON /emgdic/ Qq , Elid , Estid
   COMMON /emgest/ Ecpt
   COMMON /emgprm/ Dm , Ismb , Iprec , Nogo , Heat , Icmbar
   COMMON /emgtrx/ A , Prod9 , Temp9 , Xsub , Bfact , E , K , Kout , Ksave
   COMMON /system/ Ksystm
!
! Local variable declarations
!
   INTEGER dict(10) , i , ii , ij , ik , iout , ip , ip1 , ipart(3) , isave , j , jj , ka , l
   REAL dict5
!
! End of declarations
!
!
!     THIS SUBROUTINE CALCULATES THE STIFFNESS AND MASS MATRICES FOR
!     THE  TRIANGULAR MEMBRANE ELEMENT.  CALCULATIONS ARE PERFORMED
!     PRIMARILY BY SUBROUTINES EKTRMS AND EMASTQ.
!     SINGLE PRECISION VERSION
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
   EQUIVALENCE (Ecpt(1),Necpt(1),Ielid) , (dict5,dict(5))
   EQUIVALENCE (K(1),M(1)) , (Kout(1),Mout(1)) , (Ksystm(2),Ioutpt)
   EQUIVALENCE (Ksystm(56),Iheat) , (Ecpt(2),Ngrid(1))
!
   DATA ipart/1 , 2 , 3/
!
!
!
   ip = Iprec
   dict(1) = Estid
   DO
!
!     CREATE AN ARRAY POINTING TO GRID POINTS IN INCREASING ORDER
!
      DO i = 1 , 2
         ip1 = i + 1
         ii = ipart(i)
         DO j = ip1 , 3
            jj = ipart(j)
            IF ( Ngrid(ii)>Ngrid(jj) ) THEN
               ipart(i) = jj
               ipart(j) = ii
               ii = jj
               GOTO 100
            ENDIF
         ENDDO
      ENDDO
!
!     IF STIFFNESS MATRIX IS REQUESTED CALL EKTRMS. OTHERWISE GO TO
!     MASS MATRIX CALCULATION SECTION
!
      IF ( Ismb(1)/=0 ) THEN
!
         CALL ektrms(0)
!
         IF ( Nogo ) RETURN
!
!     RE-ORDER  THE STIFFNESS MATRIX BY INCREASING SIL VALUE
!
         IF ( Heat ) THEN
!
!     OUTPUT HEAT MATRIX HERE
!
            DO i = 1 , 3
               DO j = 1 , 3
                  iout = (i-1)*3 + j
                  ik = (ipart(i)-1)*3 + ipart(j)
                  K(iout) = Ksave(ik)
               ENDDO
            ENDDO
!     OUTPUT   HEAT  K
            dict(2) = 1
            dict(3) = 3
            dict(4) = 1
!
            CALL emgout(K,K,9,1,dict,1,ip)
         ELSE
            DO i = 1 , 3
               ii = ipart(i)
               DO j = 1 , 3
                  jj = ipart(j)
                  DO ka = 1 , 3
                     DO l = 1 , 3
                        isave = (ii-1)*27 + (jj-1)*9 + (ka-1)*3 + l
                        iout = (i-1)*27 + (j-1)*3 + (ka-1)*9 + l
                        K(iout) = Ksave(isave)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
!    OUTPUT THE MATRIX
            dict(2) = 1
            dict(3) = 9
            dict(4) = 7
!
            CALL emgout(K,K,81,1,dict,1,ip)
         ENDIF
      ENDIF
!
!     PERFORM MASS MATRIX CALCULATIONS HERE
!
      IF ( Ismb(2)==0 ) RETURN
!
!     CONVENTIONAL MASS MATRIX
!
      CALL emastq(4,M)
!     REORDER THE MASS MATRIX
      IF ( .NOT.(Heat) ) EXIT
!
!     HEAT FORMULATION
!
      DO i = 1 , 3
         j = ipart(i)
         Mout(i) = M(j)
      ENDDO
      dict(2) = 2
      dict(3) = 3
      dict(4) = 1
!
      CALL emgout(Mout,Mout,3,1,dict,2,ip)
      GOTO 99999
 100  ENDDO
   DO i = 1 , 3
      ii = (i-1)*3
      ij = ipart(i)
      jj = (ij-1)*3
      DO j = 1 , 3
         iout = ii + j
         ik = jj + j
         Mout(iout) = M(ik)
      ENDDO
   ENDDO
!
   dict(2) = 2
   dict(3) = 9
   dict(4) = 7
!
   CALL emgout(Mout,Mout,9,1,dict,2,ip)
   RETURN
!
99999 END SUBROUTINE trmems
