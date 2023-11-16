
SUBROUTINE stqme2(Ntype)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Cstr(4) , Cvec(3) , Deform , Delta , Dummy(35) , Est(100) , Fnchk , Forvec(25) , Frtmei(2) , Ftemp , Ph1out(45) , Si(36) ,  &
      & St(3) , Stres(100) , Stress(3) , Tem , Temp , Tsub0 , Twotop , Vec(3) , Z(1)
   INTEGER Ibfsz , Idm(9) , Ild , Isub , Ivec , Ivecn , Ldtemp , Line , Nchk , Nout , Npoint , Nsil(4) , Nsize
   COMMON /sdr2x4/ Dummy , Ivec , Ivecn , Ldtemp , Deform
   COMMON /sdr2x7/ Est , Stres , Forvec
   COMMON /sdr2x8/ Stress , Vec , Tem , Temp , Npoint , Delta , Nsize , Cvec , Cstr
   COMMON /sdr2x9/ Nchk , Isub , Ild , Frtmei , Twotop , Fnchk
   COMMON /system/ Ibfsz , Nout , Idm , Line
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ntype
!
! Local variable declarations
!
   INTEGER eject
   REAL frlast(2)
   INTEGER i , ishd(7) , istyp(2) , j , k , lld , lsub , typ(3)
!
! End of declarations
!
!
!     PHASE TWO STRESS DATA RECOVERY TRIANGULAR MEMBRANE
!
!     NTYPE = 1 TRI-MEMBRANE
!     NTYPE = 2 QUAD-MEMBRANE
!
!     PH1OUT CONTAINS THE FOLLOWING
!     *** NTYPE = 1 ***
!     ELEMENT ID
!     3 SILS
!     1 DUMMY
!     T SUB 0
!     S SUB T 3X1
!     3 S ARRAYS EACH 3X3
!
!     *** NTYPE = 2 ***
!     ELEMENT ID
!     4 SILS
!     T SUB 0
!     S SUB T 3X1
!     4 S ARRAYS EACH 3X3
!
!
   EQUIVALENCE (Ph1out(1),Est(1)) , (Nsil(1),Ph1out(2)) , (Tsub0,Ph1out(6)) , (St(1),Ph1out(7)) , (Si(1),Ph1out(10)) ,              &
    & (Ftemp,Ldtemp) , (ishd(1),lsub) , (ishd(2),lld) , (ishd(6),frlast(1))
!
   DATA lsub , lld , frlast/2* - 1 , -1.0E30 , -1.0E30/
   DATA typ/2HTR , 2HQD , 3HMEM/
!     ******************************************************************
!     ZERO OUT THE STRESS VECTOR
   DO i = 1 , 3
      Stress(i) = 0.0E0
      Cstr(i+1) = 0.0E0
   ENDDO
!
!                          I=NSIZE
!        STRESS VECTOR =(SUMMATION (S )(U )) - (S )(LDTEMP - T SUB 0)
!                          I=1       I   I       T
   Nsize = Ntype + 2
   DO i = 1 , Nsize
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
      Npoint = Ivec + Nsil(i) - 1
!
      CALL smmats(Si(9*i-8),3,3,0,Z(Npoint),3,1,0,Vec,Cvec)
      DO j = 1 , 3
         Cstr(j+1) = Cstr(j+1) + Cvec(j)
         Stress(j) = Stress(j) + Vec(j)
      ENDDO
!
   ENDDO
!
   Stres(1) = Ph1out(1)
   Stres(2) = Stress(1)
   Stres(3) = Stress(2)
   Stres(4) = Stress(3)
!
!     ADD IN TEMPERATURE EFFECTS
!
   IF ( Ldtemp/=(-1) ) THEN
      Tem = Ftemp - Tsub0
      DO i = 2 , 4
         Stres(i) = Stres(i) - St(i-1)*Tem
      ENDDO
   ENDIF
!     STRESS VECTOR COMPLETE AND CONTAINS SIGMA X ,  SIGMA Y ,  SIGMA XY
!
!     ******************************************************************
!
!     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
   Temp = Stres(2) - Stres(3)
   Stres(8) = sqrt((Temp/2.0E0)**2+Stres(4)**2)
   Delta = (Stres(2)+Stres(3))/2.0E0
   Stres(6) = Delta + Stres(8)
   Stres(7) = Delta - Stres(8)
   Delta = 2.0E0*Stres(4)
   IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
      Stres(5) = 0.0E0
      IF ( Nchk>0 ) THEN
!
!  . CHECK PRECISION...
!
         Cstr(1) = Ph1out(1)
         k = 0
!
         CALL sdrchk(Stres(2),Cstr(2),3,k)
         IF ( k==0 ) GOTO 99999
!
!  . LIMITS EXCEEDED...
         istyp(1) = typ(Ntype)
         istyp(2) = typ(3)
         j = 0
         IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
            lsub = Isub
            lld = Ild
            frlast(1) = Frtmei(1)
            frlast(2) = Frtmei(2)
            j = 2
            CALL page1
         ELSEIF ( eject(2)==0 ) THEN
            GOTO 20
         ENDIF
         CALL sd2rhd(ishd,j)
         Line = Line + 1
         WRITE (Nout,99001)
99001    FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSX,5X,2HSY,4X,3HSXY)
 20      WRITE (Nout,99002) istyp , Cstr
99002    FORMAT (1H0,6X,A2,A3,I7,3F7.1)
      ENDIF
   ELSE
      Stres(5) = atan2(Delta,Temp)*28.6478898E00
      RETURN
   ENDIF
!
99999 RETURN
END SUBROUTINE stqme2
