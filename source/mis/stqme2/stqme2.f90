!*==stqme2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stqme2(Ntype)
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
   INTEGER :: Ntype
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) , SAVE :: frlast
   REAL :: ftemp , tsub0
   INTEGER :: i , j , k
   INTEGER , DIMENSION(7) :: ishd
   INTEGER , DIMENSION(2) :: istyp
   INTEGER , SAVE :: lld , lsub
   INTEGER , DIMENSION(4) :: nsil
   REAL , DIMENSION(45) :: ph1out
   REAL , DIMENSION(36) :: si
   REAL , DIMENSION(3) :: st
   INTEGER , DIMENSION(3) , SAVE :: typ
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Ph1out(1),Est(1)) , (Nsil(1),Ph1out(2)) , (Tsub0,Ph1out(6)) , (St(1),Ph1out(7)) , (Si(1),Ph1out(10)) ,              &
!>>>>    & (Ftemp,Ldtemp) , (ishd(1),lsub) , (ishd(2),lld) , (ishd(6),frlast(1))
!
   DATA lsub , lld , frlast/2* - 1 , -1.0E30 , -1.0E30/
   DATA typ/2HTR , 2HQD , 3HMEM/
!     ******************************************************************
!     ZERO OUT THE STRESS VECTOR
   DO i = 1 , 3
      stress(i) = 0.0E0
      cstr(i+1) = 0.0E0
   ENDDO
!
!                          I=NSIZE
!        STRESS VECTOR =(SUMMATION (S )(U )) - (S )(LDTEMP - T SUB 0)
!                          I=1       I   I       T
   nsize = Ntype + 2
   DO i = 1 , nsize
!
!     POINTER TO DISPLACEMENT VECTOR IN VARIABLE CORE
!
      npoint = ivec + nsil(i) - 1
!
      CALL smmats(si(9*i-8),3,3,0,z(npoint),3,1,0,vec,cvec)
      DO j = 1 , 3
         cstr(j+1) = cstr(j+1) + cvec(j)
         stress(j) = stress(j) + vec(j)
      ENDDO
!
   ENDDO
!
   stres(1) = ph1out(1)
   stres(2) = stress(1)
   stres(3) = stress(2)
   stres(4) = stress(3)
!
!     ADD IN TEMPERATURE EFFECTS
!
   IF ( ldtemp/=(-1) ) THEN
      tem = ftemp - tsub0
      DO i = 2 , 4
         stres(i) = stres(i) - st(i-1)*tem
      ENDDO
   ENDIF
!     STRESS VECTOR COMPLETE AND CONTAINS SIGMA X ,  SIGMA Y ,  SIGMA XY
!
!     ******************************************************************
!
!     PRINCIPAL STRESSES AND ANGLE OF ACTION PHI
   temp = stres(2) - stres(3)
   stres(8) = sqrt((temp/2.0E0)**2+stres(4)**2)
   delta = (stres(2)+stres(3))/2.0E0
   stres(6) = delta + stres(8)
   stres(7) = delta - stres(8)
   delta = 2.0E0*stres(4)
   IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
      stres(5) = 0.0E0
      IF ( nchk>0 ) THEN
!
!  . CHECK PRECISION...
!
         cstr(1) = ph1out(1)
         k = 0
!
         CALL sdrchk(stres(2),cstr(2),3,k)
         IF ( k==0 ) RETURN
!
!  . LIMITS EXCEEDED...
         istyp(1) = typ(Ntype)
         istyp(2) = typ(3)
         j = 0
         IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
            lsub = isub
            lld = ild
            frlast(1) = frtmei(1)
            frlast(2) = frtmei(2)
            j = 2
            CALL page1
         ELSEIF ( eject(2)==0 ) THEN
            GOTO 20
         ENDIF
         CALL sd2rhd(ishd,j)
         line = line + 1
         WRITE (nout,99001)
99001    FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSX,5X,2HSY,4X,3HSXY)
 20      WRITE (nout,99002) istyp , cstr
99002    FORMAT (1H0,6X,A2,A3,I7,3F7.1)
      ENDIF
   ELSE
      stres(5) = atan2(delta,temp)*28.6478898E00
      RETURN
   ENDIF
!
END SUBROUTINE stqme2
