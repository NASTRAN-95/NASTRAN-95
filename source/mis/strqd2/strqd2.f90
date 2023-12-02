!*==strqd2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strqd2(Npts,Ti)
   USE c_blank
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
   INTEGER :: Npts
   REAL , DIMENSION(6) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: f1 , ff , ftemp , sigx1 , sigx2 , sigxy1 , sigxy2 , sigy1 , sigy2 , z1 , z1i , z2 , z2i
   LOGICAL :: flag
   REAL , DIMENSION(2) , SAVE :: frlast
   INTEGER , SAVE :: iblank , lld , lsub , qu , tr
   INTEGER , DIMENSION(12) :: ifrvec
   INTEGER :: ii , iretrn , jst , k , k1 , n1
   INTEGER , DIMENSION(7) :: ished
   INTEGER , DIMENSION(10) :: ist
   INTEGER , DIMENSION(2) :: istyp , nph1ou
   INTEGER , DIMENSION(4) :: nsil
   INTEGER , DIMENSION(2) , SAVE :: ontw
   REAL , DIMENSION(3) :: sdelta , sstrss
   REAL , DIMENSION(36) :: si
   REAL , DIMENSION(18) :: str
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   !>>>>EQUIVALENCE (Nsil(1),Ph1out(2)) , (Nph1ou(1),Ph1out(1)) , (Si(1),Ph1out(9)) , (Str(1),Ph1out(101)) , (Ldtemp,Ftemp) , (f1,n1) ,  &
!>>>>    & (Cfrvec(1),Ifrvec(1)) , (ished(6),frlast(1)) , (ished(1),lsub) , (ished(2),lld)
   !>>>>EQUIVALENCE (Str(1),Ist(1))
!
   DATA tr , qu , ontw/4HTRIA , 4HQUAD , 1H1 , 1H2/ , lld/ - 100/
   DATA lsub , frlast/ - 100 , -1.0E30 , -1.0E30/
   DATA iblank/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         DO i = 1 , 6
            forvec(i) = 0.0E0
            cfrvec(i) = 0.0E0
            cfrvec(i+6) = 0.0E0
         ENDDO
         forvec(1) = ph1out(1)
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
         IF ( nsil(1)==0 ) THEN
            z1 = 0.0E0
            z2 = 0.0E0
            spag_nextblock_1 = 2
         ELSE
!
!     FORM SUMMATION
!
            DO i = 1 , Npts
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
               npoint = ivec + nsil(i) - 1
!
               CALL smmats(si(30*i-29),5,6,0,z(npoint),6,1,0,vec(1),cvec(1))
               DO j = 2 , 6
                  cfrvec(j) = cfrvec(j) + cvec(j-1)
                  forvec(j) = forvec(j) + vec(j-1)
               ENDDO
!
            ENDDO
            IF ( strain ) THEN
!
!     SPECIAL CALCULATIONS FOR STRAINS.
!
               sigx2 = forvec(2)
               sigy2 = forvec(3)
               sigxy2 = forvec(4)
               spag_nextblock_1 = 2
            ELSE
               IF ( tloads/=0 ) THEN
                  jst = 30*Npts + 8
                  flag = .FALSE.
                  f1 = Ti(6)
                  IF ( n1==1 ) THEN
                     forvec(2) = forvec(2) + Ti(2)*ph1out(jst+1)
                     forvec(3) = forvec(3) + Ti(2)*ph1out(jst+2)
                     forvec(4) = forvec(4) + Ti(2)*ph1out(jst+3)
                     IF ( Ti(3)==0.0 .AND. Ti(4)==0.0 ) flag = .TRUE.
                  ELSE
                     forvec(2) = forvec(2) - Ti(2)
                     forvec(3) = forvec(3) - Ti(3)
                     forvec(4) = forvec(4) - Ti(4)
                     IF ( Ti(5)==0.0 .AND. Ti(6)==0.0 ) flag = .TRUE.
                  ENDIF
               ENDIF
!
!     FORCE VECTOR IS NOW COMPLETE
!
               z1 = ph1out(7)
               z2 = ph1out(8)
!
               z1ovri = -ph1out(7)/ph1out(6)
               z2ovri = -ph1out(8)/ph1out(6)
               z1i = abs(z1ovri)
               z2i = abs(z2ovri)
!
               k1 = 0
               ASSIGN 20 TO iretrn
               spag_nextblock_1 = 3
            ENDIF
         ENDIF
         CYCLE
!
 20      sigx1 = forvec(2)*z1ovri - sdelta(1)
         sigy1 = forvec(3)*z1ovri - sdelta(2)
         sigxy1 = forvec(4)*z1ovri - sdelta(3)
         cfrvec(7) = cfrvec(2)*z1i
         cfrvec(8) = cfrvec(3)*z1i
         cfrvec(9) = cfrvec(4)*z1i
!
         k1 = 1
         ASSIGN 40 TO iretrn
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 40      sigx2 = forvec(2)*z2ovri - sdelta(1)
         sigy2 = forvec(3)*z2ovri - sdelta(2)
         sigxy2 = forvec(4)*z2ovri - sdelta(3)
         cfrvec(10) = cfrvec(2)*z2i
         cfrvec(11) = cfrvec(3)*z2i
!     *******************************
!
         cfrvec(12) = cfrvec(4)*z2i
         spag_nextblock_1 = 2
      CASE (2)
!
!     FIND SIG X, SIG Y, SIG XY, FOR MEMBRANE CONSIDERATION
         IF ( nph1ou(30*Npts+13)/=0 ) THEN
!
!     ZERO STRESS VECTOR STORAGE
!
            stress(1) = 0.0E0
            stress(2) = 0.0E0
            stress(3) = 0.0E0
            sstrss(1) = 0.0E0
            sstrss(2) = 0.0E0
            sstrss(3) = 0.0E0
!
!                            I=NPTS
!        STRESS VECTOR = (  SUMMATION(S )(U )  ) - (S )(LDTEMP - T )
!                            I=1       I   I         T            0
!
            DO i = 1 , Npts
!
!     POINTER TO I-TH SIL IN PH1OUT
               npoint = 30*Npts + 12 + i
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
               npoint = ivec + nph1ou(npoint) - 1
!
!     POINTER TO S SUB I 3X3
               npt1 = 30*Npts + 12 + 9*i
!
               CALL smmats(ph1out(npt1),3,3,0,z(npoint),3,1,0,vec(1),cvec(1))
               DO j = 1 , 3
                  sstrss(j) = sstrss(j) + cvec(j)
                  stress(j) = stress(j) + vec(j)
               ENDDO
!
            ENDDO
!
            IF ( strain ) THEN
!
               sigx1 = stress(1)
               sigy1 = stress(2)
               sigxy1 = stress(3)
            ELSE
               IF ( ldtemp/=(-1) ) THEN
!
!     POINTER TO T SUB 0 = 30*NPTS + 17
!
                  tem = ftemp - ph1out(30*Npts+17)
                  DO i = 1 , 3
                     npoint = 30*Npts + 17 + i
                     stress(i) = stress(i) - ph1out(npoint)*tem
                  ENDDO
               ENDIF
!
!     ADD MEMBRANE STRESSES TO PLATE STRESSES
!
               sigx1 = sigx1 + stress(1)
               sigy1 = sigy1 + stress(2)
               sigxy1 = sigxy1 + stress(3)
               sigx2 = sigx2 + stress(1)
               sigy2 = sigy2 + stress(2)
               sigxy2 = sigxy2 + stress(3)
               cfrvec(7) = cfrvec(7) + sstrss(1)
               cfrvec(8) = cfrvec(8) + sstrss(2)
               cfrvec(9) = cfrvec(9) + sstrss(3)
               cfrvec(10) = cfrvec(10) + sstrss(1)
               cfrvec(11) = cfrvec(11) + sstrss(2)
               cfrvec(12) = cfrvec(12) + sstrss(3)
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
         IF ( nph1ou(2)==0 .AND. nph1ou(30*Npts+13)==0 ) THEN
            DO i = 2 , 18
               str(i) = 0.0E0
            ENDDO
         ELSE
!
!     COMPUTE PRINCIPAL STRESSES
!
            str(1) = ph1out(1)
            str(2) = z1
            str(3) = sigx1
            str(4) = sigy1
            str(5) = sigxy1
            str(10) = ph1out(1)
            str(11) = z2
            str(12) = sigx2
            str(13) = sigy2
            str(14) = sigxy2
!
            DO i = 3 , 12 , 9
               temp = str(i) - str(i+1)
               str(i+6) = sqrt((temp/2.0E0)**2+str(i+2)**2)
               delta = (str(i)+str(i+1))/2.0E0
               str(i+4) = delta + str(i+6)
               str(i+5) = delta - str(i+6)
               delta = 2.0E0*str(i+2)
               IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
                  str(i+3) = 0.0E0
               ELSE
                  str(i+3) = atan2(delta,temp)*28.6478898E0
               ENDIF
!
            ENDDO
         ENDIF
         str(1) = ph1out(1)
         str(10) = ph1out(1)
!
!
!     ADDITION TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
!
         DO i = 10 , 17
            str(i) = str(i+1)
         ENDDO
         IF ( strain ) THEN
            ist(2) = iblank
            str(5) = 2.0*str(5)
            str(9) = 2.0*str(9)
            ist(10) = iblank
            str(13) = 2.0*str(13)
            str(17) = 2.0*str(17)
         ENDIF
!
!  . STRESS CHECK...
!
         IF ( nchk>0 ) THEN
            cfrvec(1) = ph1out(1)
            k = 0
!  . FORCES...
            CALL sdrchk(forvec(2),cfrvec(2),5,k)
!  . STRESSES...
            CALL sdrchk(str(3),cfrvec(7),3,k)
            CALL sdrchk(str(11),cfrvec(10),3,k)
!
            IF ( k==0 ) RETURN
!
!  . LIMITS EXCEEDED...
            j = 0
            istyp(1) = tr
            istyp(2) = ontw(1)
            IF ( ieltyp>17 ) istyp(1) = qu
            IF ( iabs(ieltyp-17)<2 ) istyp(2) = ontw(2)
!
            IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
!
               lsub = isub
               lld = ild
               frlast(1) = frtmei(1)
               frlast(2) = frtmei(2)
               j = 1
               CALL page1
!
            ELSEIF ( eject(2)==0 ) THEN
               GOTO 50
            ENDIF
            CALL sd2rhd(ished,j)
            line = line + 1
            WRITE (nout,99001)
99001       FORMAT (7X,51HTYPE     EID     MX     MY    MXY     VX     VY    ,38HSX1    SY1   SXY1    SX2    SY2   SXY2)
!
 50         WRITE (nout,99002) istyp , ifrvec(1) , (cfrvec(ii),ii=2,12)
!
99002       FORMAT (1H0,5X,A4,A2,I7,11F7.1)
         ENDIF
         RETURN
      CASE (3)
!
!     INTERNAL SUBROUTINE
!
         IF ( tloads==0 .OR. flag ) THEN
            sdelta(1) = 0.0
            sdelta(2) = 0.0
            sdelta(3) = 0.0
         ELSE
            jst = 30*Npts + 8
            IF ( n1==1 ) THEN
               ff = (Ti(k1+3)-ph1out(k1+7)*Ti(2)-Ti(1))/ph1out(6)
               sdelta(1) = ph1out(jst+1)*ff
               sdelta(2) = ph1out(jst+2)*ff
               sdelta(3) = ph1out(jst+3)*ff
            ELSE
               ff = Ti(k1+5) - Ti(1)
               sdelta(1) = (ph1out(jst+1)*ff+Ti(2)*ph1out(k1+7))/ph1out(6)
               sdelta(2) = (ph1out(jst+2)*ff+Ti(3)*ph1out(k1+7))/ph1out(6)
               sdelta(3) = (ph1out(jst+3)*ff+Ti(4)*ph1out(k1+7))/ph1out(6)
            ENDIF
         ENDIF
         GOTO iretrn
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE strqd2
