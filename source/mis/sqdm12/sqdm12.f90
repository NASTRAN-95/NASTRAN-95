!*==sqdm12.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sqdm12
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_sdr2x9
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) , SAVE :: frlast
   REAL :: ftemp , tsub0
   INTEGER :: i , j , k
   INTEGER , DIMENSION(7) :: ished
   INTEGER , DIMENSION(2) , SAVE :: istyp
   INTEGER , SAVE :: lld , lsub
   INTEGER , DIMENSION(4) :: nsil
   REAL , DIMENSION(45) :: ph1out
   REAL , DIMENSION(36) :: s
   REAL , DIMENSION(3) :: st
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
!
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
   stress(1) = 0.
   stress(2) = 0.
   stress(3) = 0.
   cstrs(2) = 0.0E0
   cstrs(3) = 0.0E0
   cstrs(4) = 0.0E0
!
!                           I=4                      -
!         STRESS VECTOR =(SUMMATION (S )(U )) - (S )(T - T)
!                           I=1       I   I       T       0
   DO i = 1 , 4
      npoint = ivec + nsil(i) - 1
      CALL smmats(s(9*i-8),3,3,0,z(npoint),3,1,0,vec(1),cvc(1))
      DO j = 1 , 3
         IF ( nchk>0 ) cstrs(j+1) = cstrs(j+1) + cvc(j)
         stress(j) = stress(j) + vec(j)
      ENDDO
   ENDDO
   stres(1) = ph1out(1)
   stres(2) = stress(1)
   stres(3) = stress(2)
   stres(4) = stress(3)
   cstrs(1) = stres(1)
!
!      ADD IN TEMPERATURE EFFECTS
!
   IF ( ldtemp/=(-1) ) THEN
      tem = ftemp - tsub0
      DO i = 2 , 4
         stres(i) = stres(i) - st(i-1)*tem
      ENDDO
   ENDIF
!
!      STRESS VECTOR COMPLETE AND CONTAINS SIGMA X ,  SIGMA Y ,  SIGMA X
!
!      PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
!
   temp = stres(2) - stres(3)
!
!     COMPUTE TAU
!
   stres(8) = sqrt((temp/2.0E0)**2+stres(4)**2)
   delta = (stres(2)+stres(3))/2.0E0
!
!     COMPUTE SIGMA 1 AND SIGMA 2
!
   stres(6) = delta + stres(8)
   stres(7) = delta - stres(8)
   delta = 2.0E0*stres(4)
   IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
      stres(5) = 0.0E0
   ELSEIF ( abs(temp)<1.0E-15 ) THEN
      stres(5) = 45.
   ELSE
!
!     COMPUTE PHI 1 DEPENDING ON WHETHER OR NOT SIGMA XY AND/OR
!               (SIGMA 1 - SIGMA 2) ARE ZERO
!
      stres(5) = atan2(delta,temp)*28.6478898E00
   ENDIF
   IF ( nchk>0 ) THEN
!
!  . STRESS PRECISION CHECK...
!
      k = 0
      CALL sdrchk(stres(2),cstrs(2),3,k)
      IF ( k/=0 ) THEN
!
!  . LIMITS EXCEEDED...
         j = 0
         IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
!
            lsub = isub
            frlast(1) = frtmei(1)
            frlast(2) = frtmei(2)
            lld = ild
            j = 1
            CALL page1
         ELSEIF ( eject(2)==0 ) THEN
            GOTO 20
         ENDIF
         CALL sd2rhd(ished,j)
         WRITE (nout,99001)
99001    FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSX,5X,2HSY,4X,3HSXY)
         line = line + 1
!
 20      WRITE (nout,99002) istyp , cstrs
99002    FORMAT (1H0,5X,A4,A2,I7,4F7.1)
      ENDIF
   ENDIF
!
END SUBROUTINE sqdm12
