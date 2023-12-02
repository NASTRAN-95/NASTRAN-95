!*==sbar2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sbar2(Ti)
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
         idisp = ivec - 1
         iua = idisp + jsilno(1)
         CALL smmats(sa(1),6,6,0,zz(iua),6,1,0,fa,cfa)
         iub = idisp + jsilno(2)
         CALL smmats(sb(1),6,6,0,zz(iub),6,1,0,fb,cfb)
         p1 = fa(1) + fb(1)
         v1 = -fa(2) - fb(2)
         v2 = -fa(3) - fb(3)
         t = -fa(4) - fb(4)
         m2a = fa(5) + fb(5)
         m1a = -fa(6) - fb(6)
         fx = -p1 - sdelta*eldefm
         cfrvec(2) = cfa(6) + cfb(6)
         cfrvec(3) = cfa(5) + cfb(5)
         cfrvec(9) = cfa(4) + cfb(4)
         cfrvec(7) = cfa(3) + cfb(3)
         cfrvec(6) = cfa(2) + cfb(2)
         cfrvec(8) = cfa(1) + cfb(1)
!
! IF LDTEMP = -1, THE LOADING TEMPERATURE IS UNDEFINED
!
         IF ( tloads/=0 ) THEN
            tsave = Ti(2)
            Ti(2) = (Ti(1)+Ti(2))/2.0 - tsub0
            CALL gmmats(therm,6,5,0,Ti(2),5,1,0,fa(1))
            Ti(2) = tsave
            fx = fx - fa(1)
            v1 = v1 - fa(2)
            v2 = v2 - fa(3)
            t = t - fa(4)
            m2a = m2a + fa(5)
            m1a = m1a - fa(6)
         ENDIF
         m1b = m1a - v1*l
         m2b = m2a - v2*l
         cfrvec(4) = cfrvec(2) + cfrvec(6)*l
         cfrvec(5) = cfrvec(3) + cfrvec(7)*l
         frvec(2) = m1a
         frvec(3) = m2a
         frvec(4) = m1b
         frvec(5) = m2b
         frvec(6) = v1
         frvec(7) = v2
         frvec(8) = fx
         frvec(9) = t
!*****
! COMPUTE ELEMENT STRESSES AT 4 POINTS
!*****
!
! COMPUTE K1A AND K2A
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
            ELSE
               k2a = 0.0
            ENDIF
         ENDIF
!
! COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
!
         sig1a = k1a*c1 + k2a*c2
         sig2a = k1a*d1 + k2a*d2
         sig3a = k1a*f1 + k2a*f2
         sig4a = k1a*g1 + k2a*g2
!
! COMPUTE K1B AND K2B
!
         IF ( i12/=0.0 ) THEN
            k1b = (m2b*i12-m1b*i2)/(i1*i2-i12**2)
            k2b = (m1b*i12-m2b*i1)/(i1*i2-i12**2)
         ELSE
            IF ( i1/=0.0 ) THEN
               k1b = -m1b/i1
            ELSE
               k1b = 0.0
            ENDIF
            IF ( i2/=0.0 ) THEN
               k2b = -m2b/i2
            ELSE
               k2b = 0.0
            ENDIF
         ENDIF
!
! COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
!
         sig1b = k1b*c1 + k2b*c2
         sig2b = k1b*d1 + k2b*d2
         sig3b = k1b*f1 + k2b*f2
         sig4b = k1b*g1 + k2b*g2
         IF ( tloads/=0 ) THEN
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
      CASE (2)
         IF ( a/=0.0 ) THEN
            ealf = -st/a
            sig1a = sig1a + ealf*(Ti(7)-Ti(3)*c1-Ti(5)*c2-Ti(1))
            sig2a = sig2a + ealf*(Ti(8)-Ti(3)*d1-Ti(5)*d2-Ti(1))
            sig3a = sig3a + ealf*(Ti(9)-Ti(3)*f1-Ti(5)*f2-Ti(1))
            sig4a = sig4a + ealf*(Ti(10)-Ti(3)*g1-Ti(5)*g2-Ti(1))
            sig1b = sig1b + ealf*(Ti(11)-Ti(4)*c1-Ti(6)*c2-Ti(2))
            sig2b = sig2b + ealf*(Ti(12)-Ti(4)*d1-Ti(6)*d2-Ti(2))
            sig3b = sig3b + ealf*(Ti(13)-Ti(4)*f1-Ti(6)*f2-Ti(2))
            sig4b = sig4b + ealf*(Ti(14)-Ti(4)*g1-Ti(6)*g2-Ti(2))
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
! COMPUTE AXIAL STRESS
!
         cfrvec(10) = 0.0
         sigax = 0.0
         IF ( a/=0.0 ) sigax = fx/a
         IF ( a/=0.0 ) cfrvec(10) = cfrvec(8)/a
         frvec(10) = sigax
!
! COMPUTE MAXIMA AND MINIMA
!
         sigamx = sigax + amax1(sig1a,sig2a,sig3a,sig4a)
         sigbmx = sigax + amax1(sig1b,sig2b,sig3b,sig4b)
         sigamn = sigax + amin1(sig1a,sig2a,sig3a,sig4a)
         sigbmn = sigax + amin1(sig1b,sig2b,sig3b,sig4b)
!
! COMPUTE MARGIN OF SAFETY IN TENSION
!
         IF ( sigmat<=0.0 ) THEN
            msten = 1
         ELSEIF ( amax1(sigamx,sigbmx)<=0.0 ) THEN
            msten = 1
         ELSE
            q = sigmat/amax1(sigamx,sigbmx)
            smten = q - 1.0
         ENDIF
!
!      COMPUTE MARGIN OF SAFETY IN COMPRESSION
!
         IF ( sigmac<=0.0 ) THEN
            mscom = 1
         ELSEIF ( amin1(sigamn,sigbmn)>=0.0 ) THEN
            mscom = 1
         ELSE
            w = -sigmac/amin1(sigamn,sigbmn)
            smcom = w - 1.0
         ENDIF
         iselid = jelid
         ifelid = jelid
!
!  . STRESS CHECK...
!
         IF ( nchk>0 ) THEN
            ieid = jelid
            k = 0
            CALL sdrchk(frvec(2),cfrvec(2),9,k)
!
            IF ( k==0 ) RETURN
!
!  . LIMITS EXCEEDED...
            j = 0
            IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
               lsub = isub
               lld = ild
               frlast(1) = frtmei(1)
               frlast(2) = frtmei(2)
               j = 1
               CALL page1
!
            ELSEIF ( eject(2)==0 ) THEN
               GOTO 10
            ENDIF
            CALL sd2rhd(ished,j)
            line = line + 1
            WRITE (nout,99001)
99001       FORMAT (7X,47HTYPE     EID    M1A    M2A    M1B    M2B     V1,5X,23HV2     FA      T     SA)
 10         WRITE (nout,99002) ieid , (cfrvec(i),i=2,10)
99002       FORMAT (1H0,7X,3HBAR,I8,9F7.1)
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE sbar2
