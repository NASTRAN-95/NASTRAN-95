
SUBROUTINE trbscd
   IMPLICIT NONE
   DOUBLE PRECISION A(225) , Bfact , E(18) , Kk(324) , Kout(324) , Ksav(81) , M(324) , Mout(324) , Prod(9) , Sxubc , Temp9(9) ,     &
                  & Xsubb , Ysubc
   REAL Dm(12) , Ecpt(25) , Heat , Qq
   INTEGER Elid , Estid , Icmbar , Ielid , Ioutpt , Iprec , Ismb(3) , Ixtr , Jcore , Ksystm(60) , Ldict , Ncore , Necpt(25) ,       &
         & Ngrid(3) , Ngrids
   LOGICAL Iheat , Nogo
   COMMON /emgdic/ Qq , Ldict , Ngrids , Elid , Estid
   COMMON /emgest/ Ielid , Ngrid
   COMMON /emgprm/ Ixtr , Jcore , Ncore , Dm , Ismb , Iprec , Nogo , Heat , Icmbar
   COMMON /emgtrx/ A , Prod , Temp9 , Xsubb , Sxubc , Ysubc , Bfact , E , Kout , Kk , Ksav
   COMMON /system/ Ksystm
   INTEGER dict(9) , i , ia , ii , ij , ik , iout , ip , ip1 , ipart(3) , j , jj , k , l
   REAL dict5
!
!     THIS SUBROUTINE CALCULATES THE STIFFNESS AND MASS MATRICES FOR
!     THE BASIC BENDING TRIANGLE.  THE MASS MATRIX MAY BE CALCULATED
!     EITHER BY THE CONVENTIONAL OR THE CONSISTENT MASS METHODS (USING
!     EMASTQ OR INCLUDED CODE) ACCORDING TO THE PARAMETER ICMBAR.
!     THIS ELEMENT MAY NOT BE USED IN A HEAT PROBLEM.
!
!     DOUBLE PRECISION VERSION
!
!     ECPT FOR THIS ELEMENT
!
!     INDEX  NAME      TYPE      DESCRIPTION
!     ----- -------    ----    ------------------
!      1    IELID        I     ELEMENT ID
!      2    NGRID(1)     I     FIRST GRID POINT
!      3    NGRID(2)     I     SECOND GRID POINT
!      4    NGRID(3)     I     THIRD GRID POINT
!      5    ANGLE        R     ANGLE OF MATERIAL
!      6    MATID1       I     MATERIAL ID 1
!      7    EYE          R     MOMENT OF INERTIA
!      8    MATID2       I     MATERIAL ID 2
!      9    T2           R     T2
!     10    FMU          R     NON-STRUCTURAL MASS
!     11    Z11          R     Z1
!     12    Z22          R     Z2
!     13    NECPT(13)    I     COORD  SYSTEM ID 1
!     14    X1           R
!     15    Y1           R     COORDINATES
!     16    Z1           R
!     17    NECPT(17)    I     COORD SYSTEM ID 2
!     18    X2           R
!     19    Y2           R     COORDINATES
!     20    Z2           R
!     21    NECPT(21)    I     COORD SYSTEM ID 3
!     22    X3           R
!     23    Y3           R     COORDINATES
!     24    Z3           R
!     25    ELTEMP       R     ELEMENT TEMPERATURE
!
   !>>>>EQUIVALENCE (Ksystm(2),Ioutpt) , (Ksystm(56),Iheat) , (Ecpt(1),Necpt(1),Ielid) , (dict5,dict(5)) , (Kk(1),Mout(1)) ,             &
!>>>>    & (Kout(1),M(1))
   DATA ipart/1 , 2 , 3/
!
   ip = Iprec
!
!     IF THIS IS A HEAT PROBLEM THIS SHOULD NOT CALL US, SO RETURN
!
   IF ( Iheat ) RETURN
   DO
!
!     CREATE AN ARRAY POINTING TO THE GRID POINTS IN INCREASING  SIL
!     ORDER
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
!     IF STIFFNESS MATRIX IS DESIRED CALL ETRBKD, OTHERWISE ONLY MASS
!     MATRIX IS DESIRED
!
      IF ( Ismb(1)/=0 ) THEN
!
         CALL etrbkd(0)
         IF ( Nogo ) RETURN
         dict5 = Bfact
!
!     RE ORDER THE MATRIX BY INCREASING SIL VALUE.    NOTE THAT
!
!     KK  = KK(1 TO  9)     KK   = KK(10 TO 18)     KK   = KK(19 TO  27)
!       AA                    AB                      AC
!
!     KK  = KK(28 TO  36)  KK   = KK(37 TO  45)   KK   =  KK(46 TO  54)
!       BA                   BB                     BC
!
!     KK  = KK(55 TO  63)  KK   = KK(64 TO  72)   KK  =  KK(73 TO  81)
!       CA                   CB                     CC
!
!     AND
!
!     KOUT  = KOUT(1 - 36) KOUT  = KOUT( 4 - 6)   KOUT  = KOUT( 7 -  9)
!         I I    (10 - 12)     I I     (13 - 15)      I I     (16 - 18)
!          1 1   (19 - 21)      1 2    (22- 24)        1 3    (25 - 27)
!
!     ETC
!
!
         DO i = 1 , 3
            ii = ipart(i)
            DO j = 1 , 3
               jj = ipart(j)
               DO k = 1 , 3
                  DO l = 1 , 3
                     ik = (ii-1)*27 + (jj-1)*9 + (k-1)*3 + l
                     iout = (i-1)*27 + (j-1)*3 + (k-1)*9 + l
                     Kout(iout) = Kk(ik)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
!
!     NOW OUTPUT THE MATRIX
!
         dict(1) = Estid
         dict(2) = 1
         dict(3) = 9
         dict(4) = 4 + 8 + 16
!
         CALL emgout(Kout,Kout,81,1,dict,1,ip)
      ENDIF
!
!     NOW CALCULATE THE MASS MATRIX IF NEEDED
!
      IF ( Ismb(2)==0 ) RETURN
!
!     WHICH MASS METHOD TO BE USED (CONVENTIONAL OR CONSISTENT)
!
      IF ( Icmbar<0 ) EXIT
!
!     THE COUPLED MASS MATRIX CALCULATIONS ARE MADE HERE VIA ETRBMD
!
      CALL etrbmd
      IF ( Nogo ) RETURN
!
!     INSERT THE MATRICES INTO THE OUTPUT MATRIX IN INCREASING SIL ORDER
!
      DO i = 1 , 3
         ii = ipart(i)
         DO j = 1 , 3
            jj = ipart(j)
            DO k = 1 , 3
               DO l = 1 , 3
                  ia = (ii-1)*36 + (jj-ii)*9 + (k-1)*3 + l
                  iout = (i-1)*27 + (j-1)*3 + (k-1)*9 + l
                  Mout(iout) = M(ia)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
!
!     NOW OUTPUT THE MASS MATRIX
!
      dict(1) = Estid
      dict(2) = 1
      dict(3) = 9
      dict(4) = 4 + 8 + 16
!
      CALL emgout(Mout,Mout,81,1,dict,2,ip)
      GOTO 99999
 100  ENDDO
!
   CALL emadtq(3,M)
!
!     REORDER THE DIAGONAL MASS MATRIX
!
   DO i = 1 , 3
      ii = (i-1)*3 + 1
      ij = ipart(i)
      jj = (ij-1)*3 + 1
      DO j = 1 , 3
         iout = ii + j - 1
         ik = jj + j - 1
         Mout(iout) = M(ik)
      ENDDO
   ENDDO
!
!     NOW OUTPUT THE MATRIX
!
   dict(1) = Estid
   dict(2) = 2
   dict(3) = 9
   dict(4) = 7
!
   CALL emgout(Mout,Mout,9,1,dict,2,ip)
!
   RETURN
!
99999 RETURN
END SUBROUTINE trbscd