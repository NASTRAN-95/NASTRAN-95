
SUBROUTINE sqdm12
   IMPLICIT NONE
   REAL Cstrs(4) , Cvc(3) , Deform , Delta , Dummy(35) , Est(100) , Fnchk , Forvec(25) , Frtmei(2) , Ftemp , Ph1out(45) , S(36) ,   &
      & St(3) , Stres(100) , Stress(3) , Tem , Temp , Tsub0 , Twotop , Vec(3) , Z(1)
   INTEGER Ibfsz , Idm(9) , Ild , Isub , Ivec , Ivecn , Ldtemp , Line , Nchk , Nout , Npoint , Nsil(4) , Nsize
   COMMON /sdr2x4/ Dummy , Ivec , Ivecn , Ldtemp , Deform
   COMMON /sdr2x7/ Est , Stres , Forvec
   COMMON /sdr2x8/ Stress , Vec , Tem , Temp , Npoint , Delta , Nsize , Cstrs , Cvc
   COMMON /sdr2x9/ Nchk , Isub , Ild , Frtmei , Twotop , Fnchk
   COMMON /system/ Ibfsz , Nout , Idm , Line
   COMMON /zzzzzz/ Z
   INTEGER eject
   REAL frlast(2)
   INTEGER i , ished(7) , istyp(2) , j , k , lld , lsub
!
!      PHASE TWO STRESS DATA RECOVERY QUADRILATERAL MEMBRANE
!
!      ELEMENT ID
!      4 SILS
!      T SUB 0
!      S SUB T 3X1
!      4 S ARRAYS EACH 3X3
!
!
!     STRES(1) - PH1OUT(1)
!     STRES(2) - SIGMA X
!     STRES(3) - SIGMA Y
!     STRES(4) - SIGMA XY
!     STRES(5) - PHI 1 ANGLE OF PRINCIPAL DIRECTION OF STRESS
!     STRES(6) - SIGMA 1
!     STRES(7) - SIGMA 2
!     STRES(8) - TAU MAXIMUM SHEAR STRESS
!
!
!
   !>>>>EQUIVALENCE (Ph1out(1),Est(1)) , (Nsil(1),Ph1out(2)) , (Tsub0,Ph1out(6)) , (St(1),Ph1out(7)) , (S(1),Ph1out(10)) , (Ftemp,Ldtemp)&
!>>>>    & , (ished(1),lsub) , (ished(2),lld) , (ished(6),frlast(1))
   DATA istyp/4HQDME , 2HM1/
   DATA lsub , lld , frlast/2* - 1 , -1.0E30 , -1.0E30/
!
!      ZERO OUT THE STRESS VECTOR
!
   Stress(1) = 0.
   Stress(2) = 0.
   Stress(3) = 0.
   Cstrs(2) = 0.0E0
   Cstrs(3) = 0.0E0
   Cstrs(4) = 0.0E0
!
!                           I=4                      -
!         STRESS VECTOR =(SUMMATION (S )(U )) - (S )(T - T)
!                           I=1       I   I       T       0
   DO i = 1 , 4
      Npoint = Ivec + Nsil(i) - 1
      CALL smmats(S(9*i-8),3,3,0,Z(Npoint),3,1,0,Vec(1),Cvc(1))
      DO j = 1 , 3
         IF ( Nchk>0 ) Cstrs(j+1) = Cstrs(j+1) + Cvc(j)
         Stress(j) = Stress(j) + Vec(j)
      ENDDO
   ENDDO
   Stres(1) = Ph1out(1)
   Stres(2) = Stress(1)
   Stres(3) = Stress(2)
   Stres(4) = Stress(3)
   Cstrs(1) = Stres(1)
!
!      ADD IN TEMPERATURE EFFECTS
!
   IF ( Ldtemp/=(-1) ) THEN
      Tem = Ftemp - Tsub0
      DO i = 2 , 4
         Stres(i) = Stres(i) - St(i-1)*Tem
      ENDDO
   ENDIF
!
!      STRESS VECTOR COMPLETE AND CONTAINS SIGMA X ,  SIGMA Y ,  SIGMA X
!
!      PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
!
   Temp = Stres(2) - Stres(3)
!
!     COMPUTE TAU
!
   Stres(8) = sqrt((Temp/2.0E0)**2+Stres(4)**2)
   Delta = (Stres(2)+Stres(3))/2.0E0
!
!     COMPUTE SIGMA 1 AND SIGMA 2
!
   Stres(6) = Delta + Stres(8)
   Stres(7) = Delta - Stres(8)
   Delta = 2.0E0*Stres(4)
   IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
      Stres(5) = 0.0E0
   ELSEIF ( abs(Temp)<1.0E-15 ) THEN
      Stres(5) = 45.
   ELSE
!
!     COMPUTE PHI 1 DEPENDING ON WHETHER OR NOT SIGMA XY AND/OR
!               (SIGMA 1 - SIGMA 2) ARE ZERO
!
      Stres(5) = atan2(Delta,Temp)*28.6478898E00
   ENDIF
   IF ( Nchk>0 ) THEN
!
!  . STRESS PRECISION CHECK...
!
      k = 0
      CALL sdrchk(Stres(2),Cstrs(2),3,k)
      IF ( k==0 ) GOTO 99999
!
!  . LIMITS EXCEEDED...
      j = 0
      IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
!
         lsub = Isub
         frlast(1) = Frtmei(1)
         frlast(2) = Frtmei(2)
         lld = Ild
         j = 1
         CALL page1
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
      CALL sd2rhd(ished,j)
      WRITE (Nout,99001)
99001 FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSX,5X,2HSY,4X,3HSXY)
      Line = Line + 1
!
 50   WRITE (Nout,99002) istyp , Cstrs
99002 FORMAT (1H0,5X,A4,A2,I7,4F7.1)
   ENDIF
!
99999 RETURN
END SUBROUTINE sqdm12