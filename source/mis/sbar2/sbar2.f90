!*==sbar2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sbar2(Ti)
   IMPLICIT NONE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_SDR2X9
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(14) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ealf , smcom , smten , tsave
   REAL , DIMENSION(2) , SAVE :: frlast
   INTEGER :: i , ieid , j , k
   INTEGER , DIMENSION(7) :: ished
   INTEGER , SAVE :: lld , lsub
   EXTERNAL eject , gmmats , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!******
! THIS ROUTINE IS THE PHASE II SUBROUTINE OF STRESS DATA RECOVERY FOR
! THE BEAM ELEMENT.
!******
!
!
!
!
! SDR2 VARIABLE CORE
!
!
! BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
!
!
! THE FIRST 100 LOCATIONS OF THE SDR2X7 BLOCK ARE RESERVED FOR INPUT
! PARAMETERS, THE SECOND 100 FOR STRESS OUTPUT PARAMETERS, AND FORCE
! OUTPUT PARAMETERS BEGIN AT LOCATION 201.
!
!
!     THERM ACTUALLY HAS 30 VALUES
!
!
! SDR2 SCRATCH BLOCK
!
!
!  STRESS/FORCE PRECISION CHECK
!
!
   !>>>>EQUIVALENCE (Ldtemp,Templd) , (Msten,Smten) , (Mscom,Smcom) , (ished(6),frlast(1)) , (Ieid,Cfrvec(1)) , (ished(1),lsub) ,        &
!>>>>    & (ished(2),lld)
!
   DATA lld , lsub , frlast/2* - 100 , -1.0E30 , -1.0E30/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Idisp = Ivec - 1
         Iua = Idisp + Jsilno(1)
         CALL smmats(Sa(1),6,6,0,Zz(Iua),6,1,0,Fa,Cfa)
         Iub = Idisp + Jsilno(2)
         CALL smmats(Sb(1),6,6,0,Zz(Iub),6,1,0,Fb,Cfb)
         P1 = Fa(1) + Fb(1)
         V1 = -Fa(2) - Fb(2)
         V2 = -Fa(3) - Fb(3)
         T = -Fa(4) - Fb(4)
         M2a = Fa(5) + Fb(5)
         M1a = -Fa(6) - Fb(6)
         Fx = -P1 - Sdelta*Eldefm
         Cfrvec(2) = Cfa(6) + Cfb(6)
         Cfrvec(3) = Cfa(5) + Cfb(5)
         Cfrvec(9) = Cfa(4) + Cfb(4)
         Cfrvec(7) = Cfa(3) + Cfb(3)
         Cfrvec(6) = Cfa(2) + Cfb(2)
         Cfrvec(8) = Cfa(1) + Cfb(1)
!
! IF LDTEMP = -1, THE LOADING TEMPERATURE IS UNDEFINED
!
         IF ( Tloads/=0 ) THEN
            tsave = Ti(2)
            Ti(2) = (Ti(1)+Ti(2))/2.0 - Tsub0
            CALL gmmats(Therm,6,5,0,Ti(2),5,1,0,Fa(1))
            Ti(2) = tsave
            Fx = Fx - Fa(1)
            V1 = V1 - Fa(2)
            V2 = V2 - Fa(3)
            T = T - Fa(4)
            M2a = M2a + Fa(5)
            M1a = M1a - Fa(6)
         ENDIF
         M1b = M1a - V1*L
         M2b = M2a - V2*L
         Cfrvec(4) = Cfrvec(2) + Cfrvec(6)*L
         Cfrvec(5) = Cfrvec(3) + Cfrvec(7)*L
         Frvec(2) = M1a
         Frvec(3) = M2a
         Frvec(4) = M1b
         Frvec(5) = M2b
         Frvec(6) = V1
         Frvec(7) = V2
         Frvec(8) = Fx
         Frvec(9) = T
!*****
! COMPUTE ELEMENT STRESSES AT 4 POINTS
!*****
!
! COMPUTE K1A AND K2A
!
         IF ( I12/=0.0 ) THEN
            K1a = (M2a*I12-M1a*I2)/(I1*I2-I12**2)
            K2a = (M1a*I12-M2a*I1)/(I1*I2-I12**2)
         ELSE
            IF ( I1/=0.0 ) THEN
               K1a = -M1a/I1
            ELSE
               K1a = 0.0
            ENDIF
            IF ( I2/=0.0 ) THEN
               K2a = -M2a/I2
            ELSE
               K2a = 0.0
            ENDIF
         ENDIF
!
! COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
!
         Sig1a = K1a*C1 + K2a*C2
         Sig2a = K1a*D1 + K2a*D2
         Sig3a = K1a*F1 + K2a*F2
         Sig4a = K1a*G1 + K2a*G2
!
! COMPUTE K1B AND K2B
!
         IF ( I12/=0.0 ) THEN
            K1b = (M2b*I12-M1b*I2)/(I1*I2-I12**2)
            K2b = (M1b*I12-M2b*I1)/(I1*I2-I12**2)
         ELSE
            IF ( I1/=0.0 ) THEN
               K1b = -M1b/I1
            ELSE
               K1b = 0.0
            ENDIF
            IF ( I2/=0.0 ) THEN
               K2b = -M2b/I2
            ELSE
               K2b = 0.0
            ENDIF
         ENDIF
!
! COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
!
         Sig1b = K1b*C1 + K2b*C2
         Sig2b = K1b*D1 + K2b*D2
         Sig3b = K1b*F1 + K2b*F2
         Sig4b = K1b*G1 + K2b*G2
         IF ( Tloads/=0 ) THEN
!
!     TEST IF AT LEAST ONE POINT TEMPERATURE IS GIVEN
!
            DO i = 7 , 14
               IF ( Ti(i)/=0.0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         IF ( A/=0.0 ) THEN
            ealf = -St/A
            Sig1a = Sig1a + ealf*(Ti(7)-Ti(3)*C1-Ti(5)*C2-Ti(1))
            Sig2a = Sig2a + ealf*(Ti(8)-Ti(3)*D1-Ti(5)*D2-Ti(1))
            Sig3a = Sig3a + ealf*(Ti(9)-Ti(3)*F1-Ti(5)*F2-Ti(1))
            Sig4a = Sig4a + ealf*(Ti(10)-Ti(3)*G1-Ti(5)*G2-Ti(1))
            Sig1b = Sig1b + ealf*(Ti(11)-Ti(4)*C1-Ti(6)*C2-Ti(2))
            Sig2b = Sig2b + ealf*(Ti(12)-Ti(4)*D1-Ti(6)*D2-Ti(2))
            Sig3b = Sig3b + ealf*(Ti(13)-Ti(4)*F1-Ti(6)*F2-Ti(2))
            Sig4b = Sig4b + ealf*(Ti(14)-Ti(4)*G1-Ti(6)*G2-Ti(2))
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
! COMPUTE AXIAL STRESS
!
         Cfrvec(10) = 0.0
         Sigax = 0.0
         IF ( A/=0.0 ) Sigax = Fx/A
         IF ( A/=0.0 ) Cfrvec(10) = Cfrvec(8)/A
         Frvec(10) = Sigax
!
! COMPUTE MAXIMA AND MINIMA
!
         Sigamx = Sigax + amax1(Sig1a,Sig2a,Sig3a,Sig4a)
         Sigbmx = Sigax + amax1(Sig1b,Sig2b,Sig3b,Sig4b)
         Sigamn = Sigax + amin1(Sig1a,Sig2a,Sig3a,Sig4a)
         Sigbmn = Sigax + amin1(Sig1b,Sig2b,Sig3b,Sig4b)
!
! COMPUTE MARGIN OF SAFETY IN TENSION
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
!      COMPUTE MARGIN OF SAFETY IN COMPRESSION
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
!
!  . STRESS CHECK...
!
         IF ( Nchk>0 ) THEN
            ieid = Jelid
            k = 0
            CALL sdrchk(Frvec(2),Cfrvec(2),9,k)
!
            IF ( k==0 ) RETURN
!
!  . LIMITS EXCEEDED...
            j = 0
            IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
               lsub = Isub
               lld = Ild
               frlast(1) = Frtmei(1)
               frlast(2) = Frtmei(2)
               j = 1
               CALL page1
!
            ELSEIF ( eject(2)==0 ) THEN
               GOTO 10
            ENDIF
            CALL sd2rhd(ished,j)
            Line = Line + 1
            WRITE (Nout,99001)
99001       FORMAT (7X,47HTYPE     EID    M1A    M2A    M1B    M2B     V1,5X,23HV2     FA      T     SA)
 10         WRITE (Nout,99002) ieid , (Cfrvec(i),i=2,10)
99002       FORMAT (1H0,7X,3HBAR,I8,9F7.1)
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE sbar2
