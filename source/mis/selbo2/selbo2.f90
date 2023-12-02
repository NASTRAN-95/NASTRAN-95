!*==selbo2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE selbo2(Ti)
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(14) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c1 , c2 , d1 , d2 , dt , ealf , f1 , f2 , g1 , g2 , i12 , m2b , smcom , smten , tb , tbar , x , xl , yl
   REAL :: cod , sid
   REAL , SAVE :: dcr
   INTEGER :: i
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS THE PHASE II SUBROUTINE OF STRESS DATA RECOVERY
!     FOR THE BEAM ELEMENT.
!
   !>>>>EQUIVALENCE (Ldtemp,templd) , (Msten,smten) , (Mscom,smcom)
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
   yl = r*(1.-cod(betar))
   xl = r*sid(betar)
   i12 = 0.
   idisp = ivec - 1
   iua = idisp + jsilno(1)
   CALL gmmats(sa(1),6,6,0,zz(iua),6,1,0,fa(1))
   iub = idisp + jsilno(2)
   CALL gmmats(sb(1),6,6,0,zz(iub),6,1,0,fb(1))
   fx = -fa(1) - fb(1)
   v1 = -fa(2) - fb(2)
   v2 = -fa(3) - fb(3)
   t = -fa(4) - fb(4)
   m2a = fa(5) + fb(5)
   m1a = -fa(6) - fb(6)
!
!     IF LDTEMP = -1, THE LOADING TEMPERATURE IS UNDEFINED
!
   IF ( tloads/=0 ) THEN
      tbar = Ti(1)
      dt = tbar - tsub0
      DO i = 1 , 6
         fa(i) = dt*therm(i)
      ENDDO
      fx = fx + fa(1)
      v1 = v1 + fa(2)
      m1a = m1a + fa(6)
   ENDIF
   m1b = m1a - v1*xl + fx*yl
   m2b = m2a - v2*xl
   tb = t - v2*yl
!
!     TRANSFORM FORCES AT B-END TO A COORD. SYS TANGENT TO B-END
!
   fxbt = v1*sid(betar) + fx*abs(cod(betar))
   v1bt = v1*abs(cod(betar)) - fx*sid(betar)
   m2bt = m2b*abs(cod(betar)) + tb*sid(betar)
   tbt = -m2b*sid(betar) + tb*abs(cod(betar))
!
!     COMPUTE ELEMENT STRESSES AT 4 POINTS
!
!
!     COMPUTE K1A AND K2A
!
   IF ( i12/=0.0 ) THEN
      k1a = (m2a*i12-m1a*i2)/(i1*i2-i12**2)
      k2a = (m1a*i12-m2a*i1)/(i1*i2-i12**2)
   ELSE
      IF ( i1/=0.0 ) THEN
         k1a = -m1a/i1
      ELSE
         k1a = 0.0
      ENDIF
      IF ( i2/=0.0 ) THEN
         k2a = -m2a/i2
!
!     CHANGE STRESS RECOVERY CONSTANTS FROM CYL. TO RECT. COORD.
!
         c1 = r1*sid(t1)
         c2 = r1*cod(t1)
         d1 = r2*sid(t2)
         d2 = r2*cod(t2)
         f1 = r3*sid(t3)
         f2 = r3*cod(t3)
         g1 = r4*sid(t4)
         g2 = r4*cod(t4)
      ELSE
         k2a = 0.0
      ENDIF
   ENDIF
!
!     COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
!
   sig1a = k1a*c1*c + k2a*c2
   sig2a = k1a*d1*c + k2a*d2
   sig3a = k1a*f1*c + k2a*f2
   sig4a = k1a*g1*c + k2a*g2
!
!     COMPUTE K1B AND K2B
!
   IF ( i12/=0.0 ) THEN
      k1b = (m2bt*i12-m1b*i2)/(i1*i2-i12**2)
      k2b = (m1b*i12-m2bt*i1)/(i1*i2-i12**2)
   ELSE
      IF ( i1/=0.0 ) THEN
         k1b = -m1b/i1
      ELSE
         k1b = 0.0
      ENDIF
      IF ( i2/=0.0 ) THEN
         k2b = -m2bt/i2
      ELSE
         k2b = 0.0
      ENDIF
   ENDIF
!
!     COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
!
   sig1b = k1b*c1*c + k2b*c2
   sig2b = k1b*d1*c + k2b*d2
   sig3b = k1b*f1*c + k2b*f2
   sig4b = k1b*g1*c + k2b*g2
   IF ( tloads/=0 ) THEN
!
!     TEST IF AT LEAST ONE POINT TEMPERATURE IS GIVEN
!
      DO i = 7 , 14
         IF ( Ti(i)/=0.0 ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDDO
   ENDIF
   CALL spag_block_2
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      IF ( a/=0.0 ) THEN
         Ealf = -st/a
         Sig1a = Sig1a + Ealf*(Ti(7)-Ti(3)*C1*C-Ti(5)*C2-Ti(1))
         Sig2a = Sig2a + Ealf*(Ti(8)-Ti(3)*D1*C-Ti(5)*D2-Ti(1))
         Sig3a = Sig3a + Ealf*(Ti(9)-Ti(3)*F1*C-Ti(5)*F2-Ti(1))
         Sig4a = Sig4a + Ealf*(Ti(10)-Ti(3)*G1*C-Ti(5)*G2-Ti(1))
         Sig1b = Sig1b + Ealf*(Ti(11)-Ti(4)*C1*C-Ti(6)*C2-Ti(2))
         Sig2b = Sig2b + Ealf*(Ti(12)-Ti(4)*D1*C-Ti(6)*D2-Ti(2))
         Sig3b = Sig3b + Ealf*(Ti(13)-Ti(4)*F1*C-Ti(6)*F2-Ti(2))
         Sig4b = Sig4b + Ealf*(Ti(14)-Ti(4)*G1*C-Ti(6)*G2-Ti(2))
      ENDIF
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     COMPUTE AXIAL STRESS
!
      sigax = 0.0
      sigbx = 0.0
      IF ( a/=0.0 ) sigax = Fx/a
      IF ( a/=0.0 ) sigbx = Fxbt/a
!
!     COMPUTE MAXIMA AND MINIMA
!
      sigamx = sigax + amax1(Sig1a,Sig2a,Sig3a,Sig4a)
      sigbmx = sigbx + amax1(Sig1b,Sig2b,Sig3b,Sig4b)
      sigamn = sigax + amin1(Sig1a,Sig2a,Sig3a,Sig4a)
      sigbmn = sigbx + amin1(Sig1b,Sig2b,Sig3b,Sig4b)
!
!     COMPUTE MARGIN OF SAFETY IN TENSION
!
      IF ( sigmat<=0.0 ) THEN
         msten = 1
      ELSEIF ( amax1(sigamx,sigbmx)<=0.0 ) THEN
         msten = 1
      ELSE
         q = sigmat/amax1(sigamx,sigbmx)
         Smten = q - 1.0
      ENDIF
!
!     COMPUTE MARGIN OF SAFETY IN COMPRESSION
!
      IF ( sigmac<=0.0 ) THEN
         mscom = 1
      ELSEIF ( amin1(sigamn,sigbmn)>=0.0 ) THEN
         mscom = 1
      ELSE
         w = -sigmac/amin1(sigamn,sigbmn)
         Smcom = w - 1.0
      ENDIF
      iselid = jelid
      ifelid = jelid
   END SUBROUTINE spag_block_2
END SUBROUTINE selbo2
