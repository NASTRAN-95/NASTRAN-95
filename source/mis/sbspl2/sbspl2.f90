!*==sbspl2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sbspl2(Ntype,Ti)
   USE c_sdr2de
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_sdr2x9
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ntype
   REAL , DIMENSION(6) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bsc , lld , lsub , plt , qd , tr
   REAL :: f1 , ff , zovi
   LOGICAL :: flag
   REAL , DIMENSION(2) , SAVE :: frlast
   INTEGER , DIMENSION(7) :: ished
   INTEGER , DIMENSION(2) :: istyp
   INTEGER :: jst , k , k1 , n1 , nelid , npts
   INTEGER , DIMENSION(1) :: nsil
   REAL , DIMENSION(3) :: sdelta
   REAL , DIMENSION(1) :: si
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
!
!
!     PHASE TWO STRESS DATA RECOVERY BASIC BENDING TRIANGLE.
!
!     NTYPE = 0  IMPLIES BASIC BENDING TRIANGLE
!     NTYPE = 3 IMPLIES TRI-PLATE IS CALLING
!     NTYPE = 4 IMPLIES QUAD-PLATE IS CALLING
!
!
!
   !>>>>EQUIVALENCE (Si(1),Est(9)) , (Nsil(1),Est(2))
   !>>>>EQUIVALENCE (Nelid,Est(1))
   !>>>>EQUIVALENCE (f1,n1) , (ished(6),frlast(1)) , (ished(1),lsub) , (ished(2),lld)
!
   DATA tr , qd , bsc , plt/4H  TR , 4H  QD , 4HBSC  , 4HPLT /
   DATA lld , lsub , frlast/2* - 100 , -1.0E30 , -1.0E30/
!
!     ******************************************************************
!  . ZERO OUT FORCE AND PRECISION CHECK VECTOR...
   DO i = 1 , 6
      forvec(i) = 0.0E0
      cfrvec(i) = 0.0E0
      cfrvec(i+6) = 0.0E0
   ENDDO
   forvec(1) = est(1)
!
   npts = 3
   IF ( Ntype==4 ) npts = 4
!
!                          NPTS
!         FORCE VECTOR = SUMMATION (S )(U )
!                          I=1       I   I
!
   DO i = 1 , npts
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
      npoint = ivec + nsil(i) - 1
!
      CALL smmats(si(30*i-29),5,6,0,z(npoint),6,1,0,vec,cvec)
!
      DO j = 1 , 5
         cfrvec(j+1) = cfrvec(j+1) + cvec(j)
         forvec(j+1) = forvec(j+1) + vec(j)
      ENDDO
   ENDDO
   IF ( tloads/=0 ) THEN
      flag = .FALSE.
      jst = 98
      IF ( Ntype==4 ) jst = 128
      f1 = Ti(6)
      IF ( n1==1 ) THEN
         forvec(2) = forvec(2) + Ti(2)*est(jst+1)
         forvec(3) = forvec(3) + Ti(2)*est(jst+2)
         forvec(4) = forvec(4) + Ti(2)*est(jst+3)
         IF ( Ti(3)==0.0 .AND. Ti(4)==0.0 ) flag = .TRUE.
      ELSE
         forvec(2) = forvec(2) - Ti(2)
         forvec(3) = forvec(3) - Ti(3)
         forvec(4) = forvec(4) - Ti(4)
         IF ( Ti(5)==0.0 .AND. Ti(6)==0.0 ) flag = .TRUE.
      ENDIF
   ENDIF
!
!     FORCE VECTOR COMPLETE AND CONTAINS M , M , M  , V , V
!                                         X   Y   XY   X   Y
!
!     AND ALSO INCLUDES THE ELEMENT ID AS THE FIRST ENTRY
!     ******************************************************************
!
!     STRESSES AT FIBER DISTANCES Z1 AND Z2 = - M * Z / I
!
   stres(2) = est(7)
   stres(11) = est(8)
   eye = est(6)
   i = 2
   k = 7
   k1 = 0
   SPAG_Loop_1_1: DO
      zoveri = -stres(i)/eye
      zovi = abs(zoveri)
      IF ( tloads==0 .OR. flag ) THEN
!
         sdelta(1) = 0.0
         sdelta(2) = 0.0
         sdelta(3) = 0.0
      ELSE
         j = 98
         IF ( Ntype==4 ) j = 128
         IF ( n1==1 ) THEN
!
            ff = (Ti(k1+3)-stres(i)*Ti(2)-Ti(1))/eye
            sdelta(1) = est(jst+1)*ff
            sdelta(2) = est(jst+2)*ff
            sdelta(3) = est(jst+3)*ff
         ELSE
!
            ff = Ti(k1+5) - Ti(1)
            sdelta(1) = (est(jst+1)*ff+Ti(2)*stres(i))/eye
            sdelta(2) = (est(jst+2)*ff+Ti(3)*stres(i))/eye
            sdelta(3) = (est(jst+3)*ff+Ti(4)*stres(i))/eye
         ENDIF
      ENDIF
      stres(i+1) = forvec(2)*zoveri - sdelta(1)
      stres(i+2) = forvec(3)*zoveri - sdelta(2)
      stres(i+3) = forvec(4)*zoveri - sdelta(3)
      cfrvec(k) = cfrvec(2)*zovi
      cfrvec(k+1) = cfrvec(3)*zovi
      cfrvec(k+2) = cfrvec(4)*zovi
!
!     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
      temp = stres(i+1) - stres(i+2)
      stres(i+7) = sqrt((temp/2.0E0)**2+stres(i+3)**2)
      delta = (stres(i+1)+stres(i+2))/2.0E0
      stres(i+5) = delta + stres(i+7)
      stres(i+6) = delta - stres(i+7)
      delta = 2.0E0*stres(i+3)
      IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
         stres(i+4) = 0.0E0
      ELSE
         stres(i+4) = atan2(delta,temp)*28.6478898E0
      ENDIF
      IF ( i==11 ) THEN
         stres(1) = est(1)
!
!     ABOVE COMPLETES 2 VECTORS EACH WITH...
!
!     ELEM ID, Z, SIGMA X, SIGMA Y, SIGMA XY, PHI, SIG 1, SIG 2, TAU-MAX
!
!     STRESSES AND FORCES COMPLETE
!
!
!     ADDITON TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
!
         DO i = 10 , 17
            stres(i) = stres(i+1)
         ENDDO
!
!  . STRESS CHECK...
!
         IF ( nchk>0 ) THEN
            cfrvec(1) = est(1)
            k = 0
!
!  . FORCES...
            CALL sdrchk(forvec(2),cfrvec(2),5,k)
!
!  . STRESSES...
            CALL sdrchk(stres(3),cfrvec(7),3,k)
            CALL sdrchk(stres(11),cfrvec(10),3,k)
            IF ( k==0 ) EXIT SPAG_Loop_1_1
!
!  . LIMITS EXCEEDED...
            j = 0
            istyp(1) = tr
            IF ( ieltyp==15 ) istyp(1) = qd
            istyp(2) = plt
            IF ( ieltyp==7 ) istyp(2) = bsc
!
            IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
!
               lsub = isub
               lld = ild
               frlast(1) = frtmei(1)
               frlast(2) = frtmei(2)
               j = 2
               CALL page1
!
            ELSEIF ( eject(2)==0 ) THEN
               GOTO 10
            ENDIF
            CALL sd2rhd(ished,j)
            line = line + 1
            WRITE (nout,99001)
99001       FORMAT (7X,4HTYPE,5X,3HEID,5X,2HMX,5X,2HMY,4X,3HMXY,5X,2HVX,5X,2HVY,4X,3HSX1,4X,3HSY1,3X,4HSXY1,4X,3HSX2,4X,3HSY2,3X,   &
                   &4HSXY2)
 10         WRITE (nout,99002) istyp , nelid , (cfrvec(i),i=2,12)
99002       FORMAT (1H0,4X,A4,A3,I7,11F7.1)
         ENDIF
         EXIT SPAG_Loop_1_1
      ELSE
         i = 11
         k1 = 1
         k = 10
      ENDIF
   ENDDO SPAG_Loop_1_1
!
END SUBROUTINE sbspl2
