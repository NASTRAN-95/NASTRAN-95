
SUBROUTINE selbo2(Ti)
   IMPLICIT NONE
   REAL A , Betar , C , Dum8(8) , Eldefm , Fa(6) , Fb(6) , Fj , Fx , Fxbt , I1 , I2 , K1a , K1b , K2a , K2b , L , M1a , M1b , M2a , &
      & M2bt , P1 , Q , R , R1 , R2 , R3 , R4 , Sa(36) , Sb(36) , Sdelta , Sig1a , Sig1b , Sig2a , Sig2b , Sig3a , Sig3b , Sig4a ,  &
      & Sig4b , Sigamn , Sigamx , Sigax , Sigbmn , Sigbmx , Sigbx , Sigmac , Sigmat , St , T , T1 , T2 , T3 , T4 , Tbt , Therm(4) , &
      & Tsub0 , V1 , V1bt , V2 , W , Xxxxxx(33) , Yyyyyy(83) , Zz(1)
   INTEGER Icstm , Idisp , Ifelid , Iselid , Iua , Iub , Ivec , Ivecn , Jelid , Jsilno(2) , Ldtemp , Mscom , Msten , Ncstm , Tloads
   COMMON /sdr2x4/ Xxxxxx , Icstm , Ncstm , Ivec , Ivecn , Ldtemp , Eldefm , Dum8 , Tloads
   COMMON /sdr2x7/ Jelid , Jsilno , Sa , Sb , St , Sdelta , A , Fj , I1 , I2 , C , R1 , T1 , R2 , T2 , R3 , T3 , R4 , T4 , Tsub0 ,  &
                 & Sigmat , Sigmac , L , R , Betar , Therm , Iselid , Sig1a , Sig2a , Sig3a , Sig4a , Sigax , Sigamx , Sigamn ,     &
                 & Msten , Sig1b , Sig2b , Sig3b , Sig4b , Sigbx , Sigbmx , Sigbmn , Mscom , Yyyyyy , Ifelid , M1a , M2a , V1 , V2 ,&
                 & Fx , T , M1b , M2bt , V1bt , Fxbt , Tbt
   COMMON /sdr2x8/ Fa , Fb , Idisp , Iua , Iub , P1 , K1a , K2a , K1b , K2b , Q , W
   COMMON /zzzzzz/ Zz
   REAL Ti(14)
   REAL c1 , c2 , d1 , d2 , dcr , dt , ealf , f1 , f2 , g1 , g2 , i12 , m2b , smcom , smten , tb , tbar , templd , x , xl , yl
   REAL cod , sid
   INTEGER i
!
!     THIS ROUTINE IS THE PHASE II SUBROUTINE OF STRESS DATA RECOVERY
!     FOR THE BEAM ELEMENT.
!
   EQUIVALENCE (Ldtemp,templd) , (Msten,smten) , (Mscom,smcom)
!
!     THE FIRST 100 LOCATIONS OF THE SDR2X7 BLOCK ARE RESERVED FOR INPUT
!     PARAMETERS, THE SECOND 100 FOR STRESS OUTPUT PARAMETERS, AND FORCE
!     OUTPUT PARAMETERS BEGIN AT LOCATION 201.
!
!
   DATA dcr/.017453292/
!
   sid(x) = sin(x*dcr)
   cod(x) = cos(x*dcr)
!
   x = 1.0
   yl = R*(1.-cod(Betar))
   xl = R*sid(Betar)
   i12 = 0.
   Idisp = Ivec - 1
   Iua = Idisp + Jsilno(1)
   CALL gmmats(Sa(1),6,6,0,Zz(Iua),6,1,0,Fa(1))
   Iub = Idisp + Jsilno(2)
   CALL gmmats(Sb(1),6,6,0,Zz(Iub),6,1,0,Fb(1))
   Fx = -Fa(1) - Fb(1)
   V1 = -Fa(2) - Fb(2)
   V2 = -Fa(3) - Fb(3)
   T = -Fa(4) - Fb(4)
   M2a = Fa(5) + Fb(5)
   M1a = -Fa(6) - Fb(6)
!
!     IF LDTEMP = -1, THE LOADING TEMPERATURE IS UNDEFINED
!
   IF ( Tloads/=0 ) THEN
      tbar = Ti(1)
      dt = tbar - Tsub0
      DO i = 1 , 6
         Fa(i) = dt*Therm(i)
      ENDDO
      Fx = Fx + Fa(1)
      V1 = V1 + Fa(2)
      M1a = M1a + Fa(6)
   ENDIF
   M1b = M1a - V1*xl + Fx*yl
   m2b = M2a - V2*xl
   tb = T - V2*yl
!
!     TRANSFORM FORCES AT B-END TO A COORD. SYS TANGENT TO B-END
!
   Fxbt = V1*sid(Betar) + Fx*abs(cod(Betar))
   V1bt = V1*abs(cod(Betar)) - Fx*sid(Betar)
   M2bt = m2b*abs(cod(Betar)) + tb*sid(Betar)
   Tbt = -m2b*sid(Betar) + tb*abs(cod(Betar))
!
!     COMPUTE ELEMENT STRESSES AT 4 POINTS
!
!
!     COMPUTE K1A AND K2A
!
   IF ( i12/=0.0 ) THEN
      K1a = (M2a*i12-M1a*I2)/(I1*I2-i12**2)
      K2a = (M1a*i12-M2a*I1)/(I1*I2-i12**2)
   ELSE
      IF ( I1/=0.0 ) THEN
         K1a = -M1a/I1
      ELSE
         K1a = 0.0
      ENDIF
      IF ( I2/=0.0 ) THEN
         K2a = -M2a/I2
!
!     CHANGE STRESS RECOVERY CONSTANTS FROM CYL. TO RECT. COORD.
!
         c1 = R1*sid(T1)
         c2 = R1*cod(T1)
         d1 = R2*sid(T2)
         d2 = R2*cod(T2)
         f1 = R3*sid(T3)
         f2 = R3*cod(T3)
         g1 = R4*sid(T4)
         g2 = R4*cod(T4)
      ELSE
         K2a = 0.0
      ENDIF
   ENDIF
!
!     COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
!
   Sig1a = K1a*c1*C + K2a*c2
   Sig2a = K1a*d1*C + K2a*d2
   Sig3a = K1a*f1*C + K2a*f2
   Sig4a = K1a*g1*C + K2a*g2
!
!     COMPUTE K1B AND K2B
!
   IF ( i12/=0.0 ) THEN
      K1b = (M2bt*i12-M1b*I2)/(I1*I2-i12**2)
      K2b = (M1b*i12-M2bt*I1)/(I1*I2-i12**2)
   ELSE
      IF ( I1/=0.0 ) THEN
         K1b = -M1b/I1
      ELSE
         K1b = 0.0
      ENDIF
      IF ( I2/=0.0 ) THEN
         K2b = -M2bt/I2
      ELSE
         K2b = 0.0
      ENDIF
   ENDIF
!
!     COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
!
   Sig1b = K1b*c1*C + K2b*c2
   Sig2b = K1b*d1*C + K2b*d2
   Sig3b = K1b*f1*C + K2b*f2
   Sig4b = K1b*g1*C + K2b*g2
   IF ( Tloads/=0 ) THEN
!
!     TEST IF AT LEAST ONE POINT TEMPERATURE IS GIVEN
!
      DO i = 7 , 14
         IF ( Ti(i)/=0.0 ) GOTO 100
      ENDDO
   ENDIF
   GOTO 200
 100  IF ( A/=0.0 ) THEN
      ealf = -St/A
      Sig1a = Sig1a + ealf*(Ti(7)-Ti(3)*c1*C-Ti(5)*c2-Ti(1))
      Sig2a = Sig2a + ealf*(Ti(8)-Ti(3)*d1*C-Ti(5)*d2-Ti(1))
      Sig3a = Sig3a + ealf*(Ti(9)-Ti(3)*f1*C-Ti(5)*f2-Ti(1))
      Sig4a = Sig4a + ealf*(Ti(10)-Ti(3)*g1*C-Ti(5)*g2-Ti(1))
      Sig1b = Sig1b + ealf*(Ti(11)-Ti(4)*c1*C-Ti(6)*c2-Ti(2))
      Sig2b = Sig2b + ealf*(Ti(12)-Ti(4)*d1*C-Ti(6)*d2-Ti(2))
      Sig3b = Sig3b + ealf*(Ti(13)-Ti(4)*f1*C-Ti(6)*f2-Ti(2))
      Sig4b = Sig4b + ealf*(Ti(14)-Ti(4)*g1*C-Ti(6)*g2-Ti(2))
   ENDIF
!
!     COMPUTE AXIAL STRESS
!
 200  Sigax = 0.0
   Sigbx = 0.0
   IF ( A/=0.0 ) Sigax = Fx/A
   IF ( A/=0.0 ) Sigbx = Fxbt/A
!
!     COMPUTE MAXIMA AND MINIMA
!
   Sigamx = Sigax + amax1(Sig1a,Sig2a,Sig3a,Sig4a)
   Sigbmx = Sigbx + amax1(Sig1b,Sig2b,Sig3b,Sig4b)
   Sigamn = Sigax + amin1(Sig1a,Sig2a,Sig3a,Sig4a)
   Sigbmn = Sigbx + amin1(Sig1b,Sig2b,Sig3b,Sig4b)
!
!     COMPUTE MARGIN OF SAFETY IN TENSION
!
   IF ( Sigmat<=0.0 ) THEN
      Msten = 1
   ELSEIF ( amax1(Sigamx,Sigbmx)<=0.0 ) THEN
      Msten = 1
   ELSE
      Q = Sigmat/amax1(Sigamx,Sigbmx)
      smten = Q - 1.0
   ENDIF
!
!     COMPUTE MARGIN OF SAFETY IN COMPRESSION
!
   IF ( Sigmac<=0.0 ) THEN
      Mscom = 1
   ELSEIF ( amin1(Sigamn,Sigbmn)>=0.0 ) THEN
      Mscom = 1
   ELSE
      W = -Sigmac/amin1(Sigamn,Sigbmn)
      smcom = W - 1.0
   ENDIF
   Iselid = Jelid
   Ifelid = Jelid
END SUBROUTINE selbo2
