
SUBROUTINE opt2b(Ipr,Pr,Pl,Rr)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Conv , Core(1) , Eps , Gama , Skp1(2) , Skp2(6) , Skp3(6) , Y(1) , Z(8)
   INTEGER Count , Iprnt , Iy(1) , Max , Nklw , Nprw , Ntotl , Nwdsp , Outtap , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Skp1 , Count , Skp2 , Nwdsp , Skp3 , Nprw , Nklw , Ntotl , Conv
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Ipr(1)
   REAL Pl(1) , Pr(1) , Rr(1)
!
! Local variable declarations
!
   REAL alph , ch , delp , pnew
   INTEGER i , icp , irr , kpl , nmes , np
!
! End of declarations
!
!
   EQUIVALENCE (Core(1),Z(1),Max) , (Eps,Z(2)) , (Gama,Z(3)) , (Iprnt,Z(7)) , (Iy(1),Y(1),Z(8))
!     EQUIVALENT ARE  (IPR,PR)
!
   nmes = 0
   ch = 1.0
!
   DO np = 1 , Nprw , Nwdsp
      alph = Pr(np+4)
      i = 1
      icp = Ntotl - 4
      DO
         icp = icp + 4
         IF ( Iy(icp)<=0 ) EXIT
         IF ( Iy(icp)==np ) THEN
!
!     SPECIAL HANDLING OF TRIM6
!
            alph = Y(icp+i)
            EXIT
         ENDIF
      ENDDO
!
 50   IF ( alph<0 ) THEN
!
!     NO CHANGE IN ALPH (-1.0 DETECTED)
!
         alph = -1.0E0
         IF ( np/=Iy(icp) ) GOTO 150
         GOTO 100
      ELSEIF ( alph==0 ) THEN
!
!     ZERO STRESS INPUT, CHANGE ALPH TO 0.0001
!
         IF ( Iprnt/=0 .AND. nmes<100 ) THEN
            nmes = nmes + 1
            CALL page2(-2)
            WRITE (Outtap,99001) Uwm , Ipr(np)
99001       FORMAT (A25,' 2303, FULLY-STRESSED DESIGN DETECTED ZERO STRESS ','FOR PROPERTY',I9,/5X,                                 &
                   &'CHECK PROPERTY CARD OR UNLOADED ','ELEMENT(S)')
         ENDIF
         alph = 1.0E-4
      ENDIF
!
!     POSITIVE ALPHA, CALCULATE PNEW
!
      irr = (np+Nwdsp)/Nwdsp
      IF ( abs(Gama-1.0)<1.0E-4 ) ch = 0.25*Rr(irr) + 0.75
      pnew = Pr(np+3)*((alph/(alph+(1.0-alph)*Gama))**ch)
      IF ( Ipr(np+5)/=0 ) THEN
!
!     COMPARE TO LIMIT DATA
!
         kpl = Ipr(np+5)
         delp = pnew/Pr(np+2)
         IF ( delp>=Pl(kpl) ) THEN
            kpl = kpl + 1
            IF ( delp<=Pl(kpl) .OR. Pl(kpl)==0 ) GOTO 100
         ENDIF
!
!     RECALCULATE ALPHA, PNEW  BASED ON THE LIMIT
!
         pnew = Pr(np+2)*Pl(kpl)
         alph = -pnew*Gama/(pnew*(1.0-Gama)-Pr(np+3))
      ENDIF
!
 100  Pr(np+4) = alph
      IF ( np==Iy(icp) ) Y(icp+i) = alph
!
 150  IF ( np==Iy(icp) ) THEN
         i = i + 1
         IF ( i<=3 ) THEN
            alph = Y(icp+i)
            GOTO 50
         ELSE
            icp = icp + 4
         ENDIF
      ENDIF
!
   ENDDO
!
END SUBROUTINE opt2b
