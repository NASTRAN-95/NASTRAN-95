
SUBROUTINE strap2(Ti)
   IMPLICIT NONE
   REAL Ak(144) , Disp(12) , Dum1(33) , Dum3(225) , Eforc(12) , Eldefm , Estres(20) , Force(25) , Sel(240) , Stres(100) , Templd ,  &
      & Ts(4) , Tz , Zz(1)
   INTEGER Icstm , Idel , Iforce(25) , Igp(4) , Istres(100) , Ivec , Ivecn , Ldtemp , Ncstm
   COMMON /sdr2x4/ Dum1 , Icstm , Ncstm , Ivec , Ivecn , Templd , Eldefm
   COMMON /sdr2x7/ Idel , Igp , Tz , Sel , Ts , Ak
   COMMON /sdr2x8/ Disp , Eforc , Estres
   COMMON /zzzzzz/ Zz
   REAL Ti(4)
   REAL dt
   INTEGER i , iloc , j , k , kk , n , ncomp , ndof , ns , nsp , numpt
!
!
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE TRAPEZOIDAL
! CROSS SECTION RING
!*****
!
!
!
!
!
! SDR2 VARIABLE CORE
!
!
!
! SDR2 BLOCK FOR POINTERS AND LOADING TEMPERATURES
!
!
!
! SDR2 INPUT AND OUTPUT BLOCK
!
!
!
! SCRATCH BLOCK
!
!
!
   EQUIVALENCE (Dum3(1),Idel)
   EQUIVALENCE (Dum3(101),Stres(1),Istres(1))
   EQUIVALENCE (Dum3(201),Force(1),Iforce(1))
   EQUIVALENCE (Ldtemp,Templd)
!
!
! INITIALIZE COUNTERS
!
   ndof = 3
   numpt = 4
   n = ndof*numpt
   nsp = 5
   ncomp = 4
   ns = nsp*ncomp
!
!
! LOCATE THE DISPLACEMENTS
!
   k = 0
   DO i = 1 , numpt
      iloc = Ivec + Igp(i) - 2
      DO j = 1 , ndof
         iloc = iloc + 1
         k = k + 1
         Disp(k) = Zz(iloc)
      ENDDO
   ENDDO
!
!
! COMPUTE THE GRID POINT FORCES
!
   CALL gmmats(Ak(1),n,n,0,Disp(1),n,1,0,Eforc(1))
!
!
! COMPUTE THE STRESSES
!
   CALL gmmats(Sel(1),ns,n,0,Disp(1),n,1,0,Estres(1))
!
!
! COMPUTE THERMAL STRESS IF THERMAL LOAD EXISTS
! AND SUBTRACT FROM APPARENT STRESS
!
   IF ( Ldtemp/=(-1) ) THEN
!
      k = 0
      DO i = 1 , nsp
         dt = Ti(i) - Tz
         IF ( i==5 ) dt = (Ti(1)+Ti(2)+Ti(3)+Ti(4))/4.0E0 - Tz
         DO j = 1 , ncomp
            k = k + 1
            Estres(k) = Estres(k) - dt*Ts(j)
         ENDDO
      ENDDO
   ENDIF
!
!
!
! STORE RESULTS FOR OUTPUT PRINT
!
   k = 0
   j = 1
   Istres(1) = Idel
   DO kk = 1 , nsp
      DO i = 1 , ncomp
         j = j + 1
         k = k + 1
         Stres(j) = Estres(k)
      ENDDO
   ENDDO
!
!
   k = 0
   j = 1
   Iforce(1) = Idel
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         Force(j) = Eforc(k)
      ENDDO
   ENDDO
!
END SUBROUTINE strap2
