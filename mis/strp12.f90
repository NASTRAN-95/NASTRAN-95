
SUBROUTINE strp12(Ti)
   IMPLICIT NONE
   REAL Deform , Delta , Dum8(8) , Dummy(35) , Forvec(24) , Ftemp , Ph1out(990) , Si(36) , Stress(18) , Tem , Temp , Vec(5) , Z(1) ,&
      & Z1ovri , Z2ovri
   INTEGER Ij1 , Ij2 , Ivec , Ivecn , Ldtemp , Maxsiz , Nph1ou(990) , Npoint , Npt1 , Nsil(6) , Tloads
   COMMON /sdr2x4/ Dummy , Ivec , Ivecn , Ldtemp , Deform , Dum8 , Tloads , Maxsiz
   COMMON /sdr2x7/ Ph1out , Forvec
   COMMON /sdr2x8/ Temp , Delta , Npoint , Ij1 , Ij2 , Npt1 , Vec , Tem , Z1ovri , Z2ovri , Stress
   COMMON /zzzzzz/ Z
   REAL Ti(6)
   REAL centhk , f1 , ff , ff1 , ff2 , ff3 , reali(4) , sdelta(3) , sigx1 , sigx2 , sigxy1 , sigxy2 , sigy1 , sigy2 , stout(68) ,   &
      & str(18) , z1 , z2 , ziovri
   LOGICAL flag
   INTEGER i , i1 , i2 , ii , ii1 , ij , ijk , iretrn , j , j1 , j2 , jst , k1 , n1 , npts
!
!     PHASE II OF STRESS DATA RECOVERY
!
   EQUIVALENCE (Nsil(1),Ph1out(2)) , (Nph1ou(1),Ph1out(1)) , (Si(1),Ph1out(19)) , (Ldtemp,Ftemp) , (f1,n1)
!
!     FIRST GET FORCE VECTOR FOR THE PLATE CONSIDERATION
!
!     M ,  M ,  M ,  V ,  V    FOR ALL SIX GRID POINTS
!      X   Y   XY   X   Y
!
!                                NPTS
!     THE 5X1 FORCE VECTOR = SUMMATION  (S )(U )   FOR EACH POINT
!                                I=1       I   I
!
   npts = 6
   DO i = 1 , 24
      Forvec(i) = 0.0
   ENDDO
   Forvec(1) = Ph1out(1)
   Forvec(7) = Ph1out(1)
   Forvec(13) = Ph1out(1)
   Forvec(19) = Ph1out(1)
!
!     DO 155 II = 1,4
!
   ii = 0
 100  ii = ii + 1
   IF ( ii>4 ) THEN
      DO i = 1 , 17
         Ph1out(100+i) = stout(i)
      ENDDO
      DO j = 1 , 3
         DO i = 1 , 16
            j1 = 117 + (j-1)*16 + i
            j2 = (j-1)*17 + i + 18
            Ph1out(j1) = stout(j2)
         ENDDO
      ENDDO
      DO i = 1 , 6
         Ph1out(200+i) = Forvec(i)
      ENDDO
      DO i = 1 , 5
         Ph1out(206+i) = Forvec(i+7)
         Ph1out(211+i) = Forvec(i+13)
      ENDDO
      RETURN
   ELSE
!
!     ZERO OUT LOCAL STRESSES
!
      sigx1 = 0.0
      sigy1 = 0.0
      sigxy1 = 0.0
      sigx2 = 0.0
      sigy2 = 0.0
      sigxy2 = 0.0
!
      IF ( Nsil(1)==0 ) THEN
         z1 = 0.0
         z2 = 0.0
         GOTO 400
      ELSE
!
!     FORM SUMMATION
!
         DO i = 1 , 6
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
            Npoint = Ivec + Nsil(i) - 1
!
            ii1 = (ii-1)*180 + 30*i - 29
            CALL gmmats(Si(ii1),5,6,0,Z(Npoint),6,1,0,Vec(1))
!
            DO j = 2 , 6
               ij = (ii-1)*6 + j
               Forvec(ij) = Forvec(ij) + Vec(j-1)
            ENDDO
         ENDDO
!
         IF ( Tloads/=0 ) THEN
            jst = (ii-1)*3 + 738
            i1 = (ii-1)*6
            flag = .FALSE.
            f1 = Ti(6)
            IF ( n1==1 ) THEN
               Forvec(i1+2) = Forvec(i1+2) + Ti(2)*Ph1out(jst+1)
               Forvec(i1+3) = Forvec(i1+3) + Ti(2)*Ph1out(jst+2)
               Forvec(i1+4) = Forvec(i1+4) + Ti(2)*Ph1out(jst+3)
               IF ( Ti(3)==0.0 .AND. Ti(4)==0.0 ) flag = .TRUE.
            ELSE
               Forvec(i1+2) = Forvec(i1+2) - Ti(2)
               Forvec(i1+3) = Forvec(i1+3) - Ti(3)
               Forvec(i1+4) = Forvec(i1+4) - Ti(4)
               IF ( Ti(5)==0.0 .AND. Ti(6)==0.0 ) flag = .TRUE.
            ENDIF
         ENDIF
!
!     FORCE VECTOR IS NOW COMPLETE
!
         IF ( ii==4 ) THEN
            ziovri = -1.5/Ph1out(17)**2
            Z2ovri = -Z1ovri
         ELSE
            i1 = ii*2 + 9
            i2 = i1 + 1
            Z1ovri = -12.0*Ph1out(i1)/Ph1out(7+ii)**3
            Z2ovri = -12.0*Ph1out(i2)/Ph1out(7+ii)**3
         ENDIF
         ii1 = (ii-1)*6
!
         k1 = 0
         ASSIGN 200 TO iretrn
         GOTO 500
      ENDIF
   ENDIF
!
 200  sigx1 = Forvec(ii1+2)*Z1ovri - sdelta(1)
   sigy1 = Forvec(ii1+3)*Z1ovri - sdelta(2)
   sigxy1 = Forvec(ii1+4)*Z1ovri - sdelta(3)
!
   k1 = 1
   ASSIGN 300 TO iretrn
   GOTO 500
!
 300  sigx2 = Forvec(ii1+2)*Z2ovri - sdelta(1)
   sigy2 = Forvec(ii1+3)*Z2ovri - sdelta(2)
!
   sigxy2 = Forvec(ii1+4)*Z2ovri - sdelta(3)
!
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
 400  IF ( Nph1ou(2)==0 ) THEN
      DO i = 2 , 18
         str(i) = 0.0
      ENDDO
   ELSE
!
!     COMPUTE PRINCIPAL STRESSES
!
      str(1) = Ph1out(1)
      str(2) = Ph1out(ii*2+9)
      str(3) = sigx1
      str(4) = sigy1
      str(5) = sigxy1
      str(10) = Ph1out(1)
      str(11) = Ph1out(ii*2+10)
      str(12) = sigx2
      str(13) = sigy2
      str(14) = sigxy2
!
      DO i = 3 , 12 , 9
         Temp = str(i) - str(i+1)
         str(i+6) = sqrt((Temp/2.0)**2+str(i+2)**2)
         Delta = (str(i)+str(i+1))/2.0
         str(i+4) = Delta + str(i+6)
         str(i+5) = Delta - str(i+6)
         Delta = 2.0*str(i+2)
         IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
            str(i+3) = 0.0
         ELSE
            str(i+3) = atan2(Delta,Temp)*28.6478898E0
         ENDIF
      ENDDO
   ENDIF
   str(1) = Ph1out(1)
   str(10) = Ph1out(1)
!
!     ADDITION TO ELIMINATE 2ND ELEMENT ID IN OUTPUT
!
   ijk = (ii-1)*17
   stout(ijk+1) = Ph1out(1)
   DO i = 2 , 9
      stout(ijk+i) = str(i)
   ENDDO
   DO i = 10 , 17
      stout(ijk+i) = str(i+1)
   ENDDO
!
   GOTO 100
!
!     INTERNAL SUBROUTINE
!
 500  IF ( .NOT.(Tloads==0 .OR. flag) ) THEN
      jst = 738 + (ii-1)*3
      reali(1) = Ph1out(8)**3/12.0
      reali(2) = Ph1out(9)**3/12.0
      reali(3) = Ph1out(10)**3/12.0
      centhk = Ph1out(17)*2.0
      reali(4) = centhk**3/12.0
      IF ( n1/=1 ) THEN
         ff = Ti(k1+5) - Ti(1)
         IF ( abs(Ph1out(k1+9+2*ii))>1.0E-07 ) THEN
            sdelta(1) = (Ph1out(jst+1)*ff+Ti(2)*Ph1out(k1+9+2*ii))/reali(ii)
            sdelta(2) = (Ph1out(jst+2)*ff+Ti(3)*Ph1out(k1+9+2*ii))/reali(ii)
            sdelta(3) = (Ph1out(jst+3)*ff+Ti(4)*Ph1out(k1+9+2*ii))/reali(ii)
            GOTO 600
         ENDIF
      ELSEIF ( abs(Ph1out(k1+9+2*ii))>1.0E-07 ) THEN
         ff1 = (Ti(k1+3)-Ph1out(k1+9+2*ii)*Ti(2)-Ti(1))/reali(ii)
         ff2 = (Ti(k1+3)-Ph1out(k1+9+2*ii)*Ti(2)-Ti(1))/reali(ii)
         ff3 = (Ti(k1+3)-Ph1out(k1+9+2*ii)*Ti(2)-Ti(1))/reali(ii)
         sdelta(1) = Ph1out(jst+1)*ff1
         sdelta(2) = Ph1out(jst+2)*ff2
         sdelta(3) = Ph1out(jst+3)*ff3
         GOTO 600
      ENDIF
   ENDIF
   sdelta(1) = 0.0
   sdelta(2) = 0.0
   sdelta(3) = 0.0
 600  GOTO iretrn
END SUBROUTINE strp12
