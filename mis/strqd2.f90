
SUBROUTINE strqd2(Npts,Ti)
   IMPLICIT NONE
   REAL Cfrvec(12) , Chpout(30) , Cvec(5) , Deform , Delta , Dum8(8) , Dummy(35) , Fnchk , Forvec(6) , Frtmei(2) , Ftemp ,          &
      & Ph1out(200) , Si(36) , Skp2de(8) , Str(18) , Stress(3) , Tem , Temp , Twotop , Vec(5) , Z(1) , Z1ovri , Z2ovri
   INTEGER I , Ibfsz , Idm(9) , Idummy(10) , Ieltyp , Ifrvec(12) , Ild , Ist(10) , Isub , Ivec , Ivecn , J , Ldtemp , Line , Nchk , &
         & Nout , Nph1ou(2) , Npoint , Npt1 , Nsil(4) , Tloads
   LOGICAL Strain
   COMMON /blank / Idummy , Strain
   COMMON /sdr2de/ Skp2de , Ieltyp
   COMMON /sdr2x4/ Dummy , Ivec , Ivecn , Ldtemp , Deform , Dum8 , Tloads
   COMMON /sdr2x7/ Ph1out , Forvec
   COMMON /sdr2x8/ Temp , Delta , Npoint , I , J , Npt1 , Vec , Tem , Z1ovri , Z2ovri , Stress , Chpout , Cvec , Cfrvec
   COMMON /sdr2x9/ Nchk , Isub , Ild , Frtmei , Twotop , Fnchk
   COMMON /system/ Ibfsz , Nout , Idm , Line
   COMMON /zzzzzz/ Z
   INTEGER Npts
   REAL Ti(6)
   INTEGER eject
   REAL f1 , ff , frlast(2) , sdelta(3) , sigx1 , sigx2 , sigxy1 , sigxy2 , sigy1 , sigy2 , sstrss(3) , z1 , z1i , z2 , z2i
   LOGICAL flag
   INTEGER iblank , ii , iretrn , ished(7) , istyp(2) , jst , k , k1 , lld , lsub , n1 , ontw(2) , qu , tr
!
!     ****PHASE II OF STRESS DATA RECOVERY*********
!
!     NPTS = 3 IMPLIES STRIA1 OR STRIA2  (PHASE II)
!     NPTS = 4 IMPLIES SQUAD1 OR SQUAD2  (PHASE II)
!
!
!
!
!
!
   EQUIVALENCE (Nsil(1),Ph1out(2)) , (Nph1ou(1),Ph1out(1)) , (Si(1),Ph1out(9)) , (Str(1),Ph1out(101)) , (Ldtemp,Ftemp) , (f1,n1) ,  &
    & (Cfrvec(1),Ifrvec(1)) , (ished(6),frlast(1)) , (ished(1),lsub) , (ished(2),lld)
   EQUIVALENCE (Str(1),Ist(1))
!
   DATA tr , qu , ontw/4HTRIA , 4HQUAD , 1H1 , 1H2/ , lld/ - 100/
   DATA lsub , frlast/ - 100 , -1.0E30 , -1.0E30/
   DATA iblank/4H    /
! **********************************************************************
! **********************************************************************
!
!     PHASE I OUTPUT FROM THE PLATE IS THE FOLLWOING
!
!     PH1OUT(1)                        ELEMENT ID
!     PH1OUT(2 THRU 5)                 3 SILS AND DUMMY OR 4 SILS
!     PH1OUT(6)                        I
!     PH1OUT(7 THRU 8)                 Z1 AND Z2
!     PH1OUT(9 THRU 30*NPTS+8)         3 OR 4 S SUB I  5X6 ARRAYS
!     PH1OUT(30*NPTS+9 THRU 30*NPTS+11)  S SUB T MATRIX
!
! **********************************************************************
!
!     PHASE I OUTPUT FROM THE MEMBRANE IS THE FOLLOWING
!     NOTE..BEGIN = 30*NPTS+11
!
!     PH1OUT(BEGIN + 1)                ELEMENT ID
!     PH1OUT(BEGIN + 2 THRU BEGIN + 5) 3 SILS AND DUMMY OR 4 SILS
!     PH1OUT(BEGIN + 6)                T SUB 0
!     PH1OUT(BEGIN + 7 THRU BEGIN + 9) S SUB T  3X1 ARRAY
!     PH1OUT(BEGIN + 10 THRU BEGIN + 9*NPTS+9) 3 OR 4 S SUB I 3X3 ARRAYS
!
! **********************************************************************
! **********************************************************************
!
!     THE ABOVE ELEMENTS ARE COMPOSED OF PLATES AND MEMBRANES...
!     SOME MAY ONLY CONTAIN PLATES WHILE OTHERS MAY ONLY CONTAIN
!     MEMBRANES.
!
!     A CHECK FOR A ZERO FIRST SIL IN THE PHASE I OUTPUT, WHICH
!     INDICATES WHETHER ONE OR THE OTHER HAS BEEN OMITTED, IS MADE BELOW
!
!
!
!     FIRST GET FORCE VECTOR FOR THE PLATE CONSIDERATION
!
!     M ,  M ,  M  ,  V ,  V
!      X    Y    XY    X    Y
!
!                                NPTS
!     THE  5X1 FORCE VECTOR = SUMMATION  (S )(U )
!                                I=1       I   I
!
! *********************************************************************
!
!  . ZERO FORVEC AND PRECISION CHECK STORAGE...
!
   DO I = 1 , 6
      Forvec(I) = 0.0E0
      Cfrvec(I) = 0.0E0
      Cfrvec(I+6) = 0.0E0
   ENDDO
   Forvec(1) = Ph1out(1)
!
!     ZERO OUT LOCAL STRESSES
!
   sigx1 = 0.0E0
   sigy1 = 0.0E0
   sigxy1 = 0.0E0
   sigx2 = 0.0E0
   sigy2 = 0.0E0
   sigxy2 = 0.0E0
!
   IF ( Nsil(1)==0 ) THEN
      z1 = 0.0E0
      z2 = 0.0E0
      GOTO 300
   ELSE
!
!     FORM SUMMATION
!
      DO I = 1 , Npts
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
         Npoint = Ivec + Nsil(I) - 1
!
         CALL smmats(Si(30*I-29),5,6,0,Z(Npoint),6,1,0,Vec(1),Cvec(1))
         DO J = 2 , 6
            Cfrvec(J) = Cfrvec(J) + Cvec(J-1)
            Forvec(J) = Forvec(J) + Vec(J-1)
         ENDDO
!
      ENDDO
      IF ( Strain ) THEN
!
!     SPECIAL CALCULATIONS FOR STRAINS.
!
         sigx2 = Forvec(2)
         sigy2 = Forvec(3)
         sigxy2 = Forvec(4)
         GOTO 300
      ELSE
         IF ( Tloads/=0 ) THEN
            jst = 30*Npts + 8
            flag = .FALSE.
            f1 = Ti(6)
            IF ( n1==1 ) THEN
               Forvec(2) = Forvec(2) + Ti(2)*Ph1out(jst+1)
               Forvec(3) = Forvec(3) + Ti(2)*Ph1out(jst+2)
               Forvec(4) = Forvec(4) + Ti(2)*Ph1out(jst+3)
               IF ( Ti(3)==0.0 .AND. Ti(4)==0.0 ) flag = .TRUE.
            ELSE
               Forvec(2) = Forvec(2) - Ti(2)
               Forvec(3) = Forvec(3) - Ti(3)
               Forvec(4) = Forvec(4) - Ti(4)
               IF ( Ti(5)==0.0 .AND. Ti(6)==0.0 ) flag = .TRUE.
            ENDIF
         ENDIF
!
!     FORCE VECTOR IS NOW COMPLETE
!
         z1 = Ph1out(7)
         z2 = Ph1out(8)
!
         Z1ovri = -Ph1out(7)/Ph1out(6)
         Z2ovri = -Ph1out(8)/Ph1out(6)
         z1i = abs(Z1ovri)
         z2i = abs(Z2ovri)
!
         k1 = 0
         ASSIGN 100 TO iretrn
         GOTO 400
      ENDIF
   ENDIF
!
 100  sigx1 = Forvec(2)*Z1ovri - sdelta(1)
   sigy1 = Forvec(3)*Z1ovri - sdelta(2)
   sigxy1 = Forvec(4)*Z1ovri - sdelta(3)
   Cfrvec(7) = Cfrvec(2)*z1i
   Cfrvec(8) = Cfrvec(3)*z1i
   Cfrvec(9) = Cfrvec(4)*z1i
!
   k1 = 1
   ASSIGN 200 TO iretrn
   GOTO 400
!
 200  sigx2 = Forvec(2)*Z2ovri - sdelta(1)
   sigy2 = Forvec(3)*Z2ovri - sdelta(2)
   sigxy2 = Forvec(4)*Z2ovri - sdelta(3)
   Cfrvec(10) = Cfrvec(2)*z2i
   Cfrvec(11) = Cfrvec(3)*z2i
!     *******************************
!
   Cfrvec(12) = Cfrvec(4)*z2i
!
!     FIND SIG X, SIG Y, SIG XY, FOR MEMBRANE CONSIDERATION
 300  IF ( Nph1ou(30*Npts+13)/=0 ) THEN
!
!     ZERO STRESS VECTOR STORAGE
!
      Stress(1) = 0.0E0
      Stress(2) = 0.0E0
      Stress(3) = 0.0E0
      sstrss(1) = 0.0E0
      sstrss(2) = 0.0E0
      sstrss(3) = 0.0E0
!
!                            I=NPTS
!        STRESS VECTOR = (  SUMMATION(S )(U )  ) - (S )(LDTEMP - T )
!                            I=1       I   I         T            0
!
      DO I = 1 , Npts
!
!     POINTER TO I-TH SIL IN PH1OUT
         Npoint = 30*Npts + 12 + I
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
         Npoint = Ivec + Nph1ou(Npoint) - 1
!
!     POINTER TO S SUB I 3X3
         Npt1 = 30*Npts + 12 + 9*I
!
         CALL smmats(Ph1out(Npt1),3,3,0,Z(Npoint),3,1,0,Vec(1),Cvec(1))
         DO J = 1 , 3
            sstrss(J) = sstrss(J) + Cvec(J)
            Stress(J) = Stress(J) + Vec(J)
         ENDDO
!
      ENDDO
!
      IF ( Strain ) THEN
!
         sigx1 = Stress(1)
         sigy1 = Stress(2)
         sigxy1 = Stress(3)
      ELSE
         IF ( Ldtemp/=(-1) ) THEN
!
!     POINTER TO T SUB 0 = 30*NPTS + 17
!
            Tem = Ftemp - Ph1out(30*Npts+17)
            DO I = 1 , 3
               Npoint = 30*Npts + 17 + I
               Stress(I) = Stress(I) - Ph1out(Npoint)*Tem
            ENDDO
         ENDIF
!
!     ADD MEMBRANE STRESSES TO PLATE STRESSES
!
         sigx1 = sigx1 + Stress(1)
         sigy1 = sigy1 + Stress(2)
         sigxy1 = sigxy1 + Stress(3)
         sigx2 = sigx2 + Stress(1)
         sigy2 = sigy2 + Stress(2)
         sigxy2 = sigxy2 + Stress(3)
         Cfrvec(7) = Cfrvec(7) + sstrss(1)
         Cfrvec(8) = Cfrvec(8) + sstrss(2)
         Cfrvec(9) = Cfrvec(9) + sstrss(3)
         Cfrvec(10) = Cfrvec(10) + sstrss(1)
         Cfrvec(11) = Cfrvec(11) + sstrss(2)
         Cfrvec(12) = Cfrvec(12) + sstrss(3)
      ENDIF
   ENDIF
!
!     STRESS OUTPUT VECTOR IS THE FOLLOWING
!
!      1) ELEMENT ID
!      2) Z1 = FIBER DISTANCE 1
!      3) SIG X  1
!      4) SIG Y  1
!      5) SIG XY 1
!      6) ANGLE OF ZERO SHEAR AT Z1
!      7) SIG P1 AT Z1
!      8) SIG P2 AT Z1
!      9) TAU MAX = MAXIMUM SHEAR STRESS AT Z1
!
!     10) ELEMENT ID
!     11) Z2 = FIBER DISTANCE 2
!     12) SIG X  2
!     13) SIG Y  2
!     14) SIG XY 2
!     15) ANGLE OF ZERO SHEAR AT Z2
!     16) SIG P1 AT Z2
!     17) SIG P2 AT Z2
!     S7) SIG P2 AT Z2
!     18) TAU MAX = MAXIMUM SHEAR STRESS AT Z2
!
!
   IF ( Nph1ou(2)==0 .AND. Nph1ou(30*Npts+13)==0 ) THEN
      DO I = 2 , 18
         Str(I) = 0.0E0
      ENDDO
   ELSE
!
!     COMPUTE PRINCIPAL STRESSES
!
      Str(1) = Ph1out(1)
      Str(2) = z1
      Str(3) = sigx1
      Str(4) = sigy1
      Str(5) = sigxy1
      Str(10) = Ph1out(1)
      Str(11) = z2
      Str(12) = sigx2
      Str(13) = sigy2
      Str(14) = sigxy2
!
      DO I = 3 , 12 , 9
         Temp = Str(I) - Str(I+1)
         Str(I+6) = sqrt((Temp/2.0E0)**2+Str(I+2)**2)
         Delta = (Str(I)+Str(I+1))/2.0E0
         Str(I+4) = Delta + Str(I+6)
         Str(I+5) = Delta - Str(I+6)
         Delta = 2.0E0*Str(I+2)
         IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
            Str(I+3) = 0.0E0
         ELSE
            Str(I+3) = atan2(Delta,Temp)*28.6478898E0
         ENDIF
!
      ENDDO
   ENDIF
   Str(1) = Ph1out(1)
   Str(10) = Ph1out(1)
!
!
!     ADDITION TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
!
   DO I = 10 , 17
      Str(I) = Str(I+1)
   ENDDO
   IF ( Strain ) THEN
      Ist(2) = iblank
      Str(5) = 2.0*Str(5)
      Str(9) = 2.0*Str(9)
      Ist(10) = iblank
      Str(13) = 2.0*Str(13)
      Str(17) = 2.0*Str(17)
   ENDIF
!
!  . STRESS CHECK...
!
   IF ( Nchk>0 ) THEN
      Cfrvec(1) = Ph1out(1)
      k = 0
!  . FORCES...
      CALL sdrchk(Forvec(2),Cfrvec(2),5,k)
!  . STRESSES...
      CALL sdrchk(Str(3),Cfrvec(7),3,k)
      CALL sdrchk(Str(11),Cfrvec(10),3,k)
!
      IF ( k==0 ) GOTO 99999
!
!  . LIMITS EXCEEDED...
      J = 0
      istyp(1) = tr
      istyp(2) = ontw(1)
      IF ( Ieltyp>17 ) istyp(1) = qu
      IF ( iabs(Ieltyp-17)<2 ) istyp(2) = ontw(2)
!
      IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
!
         lsub = Isub
         lld = Ild
         frlast(1) = Frtmei(1)
         frlast(2) = Frtmei(2)
         J = 1
         CALL page1
!
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 350
      ENDIF
      CALL sd2rhd(ished,J)
      Line = Line + 1
      WRITE (Nout,99001)
99001 FORMAT (7X,51HTYPE     EID     MX     MY    MXY     VX     VY    ,38HSX1    SY1   SXY1    SX2    SY2   SXY2)
!
 350  WRITE (Nout,99002) istyp , Ifrvec(1) , (Cfrvec(ii),ii=2,12)
!
99002 FORMAT (1H0,5X,A4,A2,I7,11F7.1)
   ENDIF
   GOTO 99999
!
!     INTERNAL SUBROUTINE
!
 400  IF ( Tloads==0 .OR. flag ) THEN
      sdelta(1) = 0.0
      sdelta(2) = 0.0
      sdelta(3) = 0.0
   ELSE
      jst = 30*Npts + 8
      IF ( n1==1 ) THEN
         ff = (Ti(k1+3)-Ph1out(k1+7)*Ti(2)-Ti(1))/Ph1out(6)
         sdelta(1) = Ph1out(jst+1)*ff
         sdelta(2) = Ph1out(jst+2)*ff
         sdelta(3) = Ph1out(jst+3)*ff
      ELSE
         ff = Ti(k1+5) - Ti(1)
         sdelta(1) = (Ph1out(jst+1)*ff+Ti(2)*Ph1out(k1+7))/Ph1out(6)
         sdelta(2) = (Ph1out(jst+2)*ff+Ti(3)*Ph1out(k1+7))/Ph1out(6)
         sdelta(3) = (Ph1out(jst+3)*ff+Ti(4)*Ph1out(k1+7))/Ph1out(6)
      ENDIF
   ENDIF
   GOTO iretrn
99999 RETURN
END SUBROUTINE strqd2
