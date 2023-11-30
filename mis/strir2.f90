
SUBROUTINE strir2(Ti)
   IMPLICIT NONE
   REAL Ak(81) , Disp(9) , Dum1(33) , Dum2(99) , Dum3(225) , Eforc(9) , Eldefm , Estres(4) , Force(25) , Sel(36) , Stres(100) ,     &
      & Templd , Ts(4) , Tz , Zz(1)
   INTEGER Icstm , Idel , Iforce(25) , Igp(3) , Istres(100) , Ivec , Ivecn , Ldtemp , Ncstm
   COMMON /sdr2x4/ Dum1 , Icstm , Ncstm , Ivec , Ivecn , Templd , Eldefm
   COMMON /sdr2x7/ Idel , Igp , Tz , Sel , Ts , Ak , Dum2
   COMMON /sdr2x8/ Disp , Eforc , Estres
   COMMON /zzzzzz/ Zz
   REAL Ti(3)
   REAL dt
   INTEGER i , iloc , j , k , kk , n , ncomp , ndof , ns , nsp , numpt
!
!*****
! THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE TRIANGULAR
!
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
   numpt = 3
   n = ndof*numpt
   nsp = 1
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
      dt = (Ti(1)+Ti(2)+Ti(3))/3.0E0 - Tz
      DO i = 1 , ns
         Estres(i) = Estres(i) - dt*Ts(i)
      ENDDO
   ENDIF
!
!
!
! STORE RESULTS FOR OUTPUT PRINT
!
   j = 1
   Istres(1) = Idel
   DO i = 1 , ncomp
      j = j + 1
      Stres(j) = Estres(i)
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
END SUBROUTINE strir2
