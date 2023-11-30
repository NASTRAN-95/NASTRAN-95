
SUBROUTINE sbspl2(Ntype,Ti)
   IMPLICIT NONE
   REAL Cfrvec(12) , Cvec(5) , Delta , Dum11(11) , Dummy(35) , Est(100) , Eye , Fnchk , Forvec(25) , Frtmei(2) , Si(1) , Skp(8) ,   &
      & Stres(100) , Temp , Twotop , Vec(5) , Z(1) , Zoveri
   INTEGER I , Ibfsz , Idm(9) , Ieltyp , Ild , Isub , Ivec , J , Line , Nchk , Nelid , Nout , Npoint , Nsil(1) , Tloads
   COMMON /sdr2de/ Skp , Ieltyp
   COMMON /sdr2x4/ Dummy , Ivec , Dum11 , Tloads
   COMMON /sdr2x7/ Est , Stres , Forvec
   COMMON /sdr2x8/ Eye , I , J , Npoint , Vec , Zoveri , Temp , Delta , Cvec , Cfrvec
   COMMON /sdr2x9/ Nchk , Isub , Ild , Frtmei , Twotop , Fnchk
   COMMON /system/ Ibfsz , Nout , Idm , Line
   COMMON /zzzzzz/ Z
   INTEGER Ntype
   REAL Ti(6)
   INTEGER bsc , ished(7) , istyp(2) , jst , k , k1 , lld , lsub , n1 , npts , plt , qd , tr
   INTEGER eject
   REAL f1 , ff , frlast(2) , sdelta(3) , zovi
   LOGICAL flag
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
   DO I = 1 , 6
      Forvec(I) = 0.0E0
      Cfrvec(I) = 0.0E0
      Cfrvec(I+6) = 0.0E0
   ENDDO
   Forvec(1) = Est(1)
!
   npts = 3
   IF ( Ntype==4 ) npts = 4
!
!                          NPTS
!         FORCE VECTOR = SUMMATION (S )(U )
!                          I=1       I   I
!
   DO I = 1 , npts
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
      Npoint = Ivec + Nsil(I) - 1
!
      CALL smmats(Si(30*I-29),5,6,0,Z(Npoint),6,1,0,Vec,Cvec)
!
      DO J = 1 , 5
         Cfrvec(J+1) = Cfrvec(J+1) + Cvec(J)
         Forvec(J+1) = Forvec(J+1) + Vec(J)
      ENDDO
   ENDDO
   IF ( Tloads/=0 ) THEN
      flag = .FALSE.
      jst = 98
      IF ( Ntype==4 ) jst = 128
      f1 = Ti(6)
      IF ( n1==1 ) THEN
         Forvec(2) = Forvec(2) + Ti(2)*Est(jst+1)
         Forvec(3) = Forvec(3) + Ti(2)*Est(jst+2)
         Forvec(4) = Forvec(4) + Ti(2)*Est(jst+3)
         IF ( Ti(3)==0.0 .AND. Ti(4)==0.0 ) flag = .TRUE.
      ELSE
         Forvec(2) = Forvec(2) - Ti(2)
         Forvec(3) = Forvec(3) - Ti(3)
         Forvec(4) = Forvec(4) - Ti(4)
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
   Stres(2) = Est(7)
   Stres(11) = Est(8)
   Eye = Est(6)
   I = 2
   k = 7
   k1 = 0
   DO
      Zoveri = -Stres(I)/Eye
      zovi = abs(Zoveri)
      IF ( Tloads==0 .OR. flag ) THEN
!
         sdelta(1) = 0.0
         sdelta(2) = 0.0
         sdelta(3) = 0.0
      ELSE
         J = 98
         IF ( Ntype==4 ) J = 128
         IF ( n1==1 ) THEN
!
            ff = (Ti(k1+3)-Stres(I)*Ti(2)-Ti(1))/Eye
            sdelta(1) = Est(jst+1)*ff
            sdelta(2) = Est(jst+2)*ff
            sdelta(3) = Est(jst+3)*ff
         ELSE
!
            ff = Ti(k1+5) - Ti(1)
            sdelta(1) = (Est(jst+1)*ff+Ti(2)*Stres(I))/Eye
            sdelta(2) = (Est(jst+2)*ff+Ti(3)*Stres(I))/Eye
            sdelta(3) = (Est(jst+3)*ff+Ti(4)*Stres(I))/Eye
         ENDIF
      ENDIF
      Stres(I+1) = Forvec(2)*Zoveri - sdelta(1)
      Stres(I+2) = Forvec(3)*Zoveri - sdelta(2)
      Stres(I+3) = Forvec(4)*Zoveri - sdelta(3)
      Cfrvec(k) = Cfrvec(2)*zovi
      Cfrvec(k+1) = Cfrvec(3)*zovi
      Cfrvec(k+2) = Cfrvec(4)*zovi
!
!     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
      Temp = Stres(I+1) - Stres(I+2)
      Stres(I+7) = sqrt((Temp/2.0E0)**2+Stres(I+3)**2)
      Delta = (Stres(I+1)+Stres(I+2))/2.0E0
      Stres(I+5) = Delta + Stres(I+7)
      Stres(I+6) = Delta - Stres(I+7)
      Delta = 2.0E0*Stres(I+3)
      IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
         Stres(I+4) = 0.0E0
      ELSE
         Stres(I+4) = atan2(Delta,Temp)*28.6478898E0
      ENDIF
      IF ( I==11 ) THEN
         Stres(1) = Est(1)
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
         DO I = 10 , 17
            Stres(I) = Stres(I+1)
         ENDDO
!
!  . STRESS CHECK...
!
         IF ( Nchk>0 ) THEN
            Cfrvec(1) = Est(1)
            k = 0
!
!  . FORCES...
            CALL sdrchk(Forvec(2),Cfrvec(2),5,k)
!
!  . STRESSES...
            CALL sdrchk(Stres(3),Cfrvec(7),3,k)
            CALL sdrchk(Stres(11),Cfrvec(10),3,k)
            IF ( k==0 ) EXIT
!
!  . LIMITS EXCEEDED...
            J = 0
            istyp(1) = tr
            IF ( Ieltyp==15 ) istyp(1) = qd
            istyp(2) = plt
            IF ( Ieltyp==7 ) istyp(2) = bsc
!
            IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
!
               lsub = Isub
               lld = Ild
               frlast(1) = Frtmei(1)
               frlast(2) = Frtmei(2)
               J = 2
               CALL page1
!
            ELSEIF ( eject(2)==0 ) THEN
               GOTO 10
            ENDIF
            CALL sd2rhd(ished,J)
            Line = Line + 1
            WRITE (Nout,99001)
99001       FORMAT (7X,4HTYPE,5X,3HEID,5X,2HMX,5X,2HMY,4X,3HMXY,5X,2HVX,5X,2HVY,4X,3HSX1,4X,3HSY1,3X,4HSXY1,4X,3HSX2,4X,3HSY2,3X,   &
                   &4HSXY2)
 10         WRITE (Nout,99002) istyp , Nelid , (Cfrvec(I),I=2,12)
99002       FORMAT (1H0,4X,A4,A3,I7,11F7.1)
         ENDIF
         EXIT
      ELSE
         I = 11
         k1 = 1
         k = 10
      ENDIF
   ENDDO
!
END SUBROUTINE sbspl2