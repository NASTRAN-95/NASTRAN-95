!*==strax2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strax2(Sorc,Ti)
   USE c_condas
   USE c_sdr2de
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Sorc
   REAL , DIMENSION(3) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(9) :: akph2 , d9 , selp3
   REAL , DIMENSION(27) :: akuph , selp2
   REAL , DIMENSION(3) :: d3 , dispp , echrg , eflux
   REAL , DIMENSION(6) :: d6
   REAL :: degrad , dt
   REAL , DIMENSION(225) :: dum3
   REAL , DIMENSION(25) :: force
   INTEGER :: i , iloc , ilocp , iwa , j , k , k3 , kk , ldtemp , ncomp , ndof , ns , nsp , numpt
   INTEGER , DIMENSION(22,14) :: iblock , iclock
   INTEGER , DIMENSION(25) :: iforce
   INTEGER , SAVE :: iosorc
   INTEGER , DIMENSION(100) :: istres
   LOGICAL :: lsys78 , zero
   REAL , DIMENSION(18) :: selp1
   REAL , DIMENSION(100) :: stres
   LOGICAL , SAVE :: zeron
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS PHASE II OF STRESS DATA FOR THE TRIANGULAR
!     CROSS SECTION RING
!
!     OUTPUTS FROM PHASE I ARE THE FOLLOWING -
!     IDEL IGP(3) TZ SEL(54) TS(4) AK(81) PHI(14)
!     AKUPH(27) AKPH2(9) SELP1(18) SELP2(27) SELP3(9)
!
!     ANY GROUP OF STATEMENTS PREFACED BY AN IF STATEMENT CONTAINING
!     ...KSYS78 OR LSYS78 ...  INDICATES CODING NECESSARY FOR THIS
!     ELEMENT*S PIEZOELECTRIC CAPABILITY
!
!     KSYS78 = 0   ELASTIC, NON-PIEZOELECTRIC MATERIAL
!     KSYS78 = 1   ELECTRICAL-ELASTIC COUPLED, PIEZOELETRIC MATERIAL
!     KSYS78 = 2   ELASTIC ONLY, PIEZOELECTRIC MATERIAL
!     LSYS78 = .TRUE. IF KSYS78 = 0, OR 2
!
!
!     SDR2 VARIABLE CORE
!
!
!     SDR2 BLOCK FOR POINTERS AND LOADING  TEMPERATURES
!
!
!     SCRATCH BLOCK
!
!
!     SDR2 INPUT AND OUTPUT BLOCK
!
!
   !>>>>EQUIVALENCE (Iblock(1,1),Block(1,1)) , (Iclock(1,1),Clock(1,1)) , (Dum3(1),Idel) , (Ldtemp,Templd) ,                             &
!>>>>    & (Dum3(109),Stres(9),Istres(9),Eflux(1)) , (Dum3(201),Force(1),Iforce(1)) , (Dum2(1),Selp1(1)) , (Dum2(19),Akph2(1)) ,         &
!>>>>    & (Dum2(28),Akuph(1)) , (Dum2(55),Selp2(1)) , (Dum2(82),Selp3(1)) , (Consts(4),Degrad) , (Unu(1),D3(1)) , (Unu(4),D6(1)) ,      &
!>>>>    & (Unu(10),D9(1))
   DATA zeron/.FALSE./
   DATA iosorc/0/
!
   lsys78 = .FALSE.
   IF ( ksys78==0 .OR. ksys78==2 ) lsys78 = .TRUE.
!
   elemid = idel/1000
   nelhar = idel - elemid*1000
!
!     SET BLOCK = 0 IF HARMONIC = 0
!
   n = nelhar - 1
   IF ( n==0 ) THEN
      IF ( n==0 .AND. zeron .AND. iosorc/=Sorc ) THEN
         zeron = .FALSE.
      ELSE
         zeron = .TRUE.
         iosorc = Sorc
         DO i = 2 , 22
            DO j = 1 , 14
               IF ( ktype/=2 .OR. ipart/=2 ) block(i,j) = 0.0
               clock(i,j) = 0.0
            ENDDO
         ENDDO
!
!     SET ANGLES CONTROL FOR SUMMATION
!
         zero = .FALSE.
         j = 0
         DO i = 1 , 14
            IF ( phi(i)==0 ) THEN
               IF ( zero ) CYCLE
               zero = .TRUE.
            ENDIF
            j = j + 1
            block(1,j) = phi(i)
            clock(1,j) = phi(i)
         ENDDO
         j = j + 1
         IF ( j<=14 ) THEN
            iblock(1,j) = 1
            iclock(1,j) = 1
         ENDIF
      ENDIF
   ENDIF
   harm = n
!
!     INITIALIZE LOCAT VARIABLES
!
   ndof = 3
   numpt = 3
   n = ndof*numpt
   nsp = 1
   ncomp = 6
   ns = nsp*ncomp
!
!     FIND GRID POINTS DISPLACEMENTS
!
   k = 0
   DO i = 1 , numpt
      iloc = ivec + igp(i) - 2
!
      IF ( .NOT.(lsys78) ) THEN
         ilocp = iloc + 4
         dispp(i) = zz(ilocp)
      ENDIF
      DO j = 1 , ndof
         iloc = iloc + 1
         k = k + 1
         disp(k) = zz(iloc)
      ENDDO
   ENDDO
!
!     COMPUTE THE GRID POINT FORCES
!
   CALL gmmats(ak(1),n,n,0,disp(1),n,1,0,eforc(1))
!
   DO i = 1 , 3
      echrg(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(akuph(1),n,numpt,0,dispp(1),numpt,1,0,d9(1))
      DO i = 1 , 9
         eforc(i) = eforc(i) + d9(i)
      ENDDO
!
      CALL gmmats(akuph(1),n,numpt,1,disp(1),n,1,0,d3(1))
      CALL gmmats(akph2(1),numpt,numpt,0,dispp(1),numpt,1,0,echrg(1))
      DO i = 1 , 3
         echrg(i) = echrg(i) + d3(i)
      ENDDO
   ENDIF
!
!     COMPUTE THE STRESSES
!
   CALL gmmats(sel(1),ns,n,0,disp(1),n,1,0,estres(1))
!
   DO i = 1 , 3
      eflux(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(selp1(1),ns,numpt,0,dispp(1),numpt,1,0,d6(1))
      DO i = 1 , 6
         estres(i) = estres(i) + d6(i)
      ENDDO
!
      CALL gmmats(selp2(1),numpt,n,0,disp(1),n,1,0,eflux(1))
      CALL gmmats(selp3(1),numpt,numpt,0,dispp(1),numpt,1,0,d3(1))
!
      DO i = 1 , 3
         eflux(i) = eflux(i) + d3(i)
      ENDDO
   ENDIF
!
!     COMPUTE THERMAL STRESS IF IT IS EXISTS
!
   IF ( ldtemp/=-1 ) THEN
      dt = tz
      IF ( harm>0.0 ) dt = 0.0
      dt = (Ti(1)+Ti(2)+Ti(3))/3.0 - dt
      DO i = 1 , ns
         estres(i) = estres(i) - dt*ts(i)
      ENDDO
   ENDIF
!
!     BRANCH TO INSERT HARMONIC STRESSES AND FORCES INTO BLOCK OR CLOCK
!
!     KTYPE = 1 - REAL OUTPUT, STORED IN BLOCK, NOTHING IN CLOCK
!     KTYPE = 2 - COMPLEX OUTPUT
!     IPART = 1 - IMAGINARY PART OF COMPLEX OUTPUT, STORED IN BLOCK
!     IPART = 2 - REAL PART OF COMPLEX OUTPUT, STORED IN CLOCK
!
   IF ( ktype/=2 .OR. ipart/=2 ) THEN
!
!     INSERT HARMONIC STRESSES AND FORCES INTO BLOCK
!
      SPAG_Loop_1_1: DO i = 1 , 14
         IF ( iblock(1,i)==1 ) EXIT SPAG_Loop_1_1
         IF ( harm/=0.0 ) THEN
            nphi = harm*block(1,i)*degrad
            sinphi = sin(nphi)
            conphi = cos(nphi)
!
            IF ( Sorc==1 ) THEN
               block(2,i) = block(2,i) + sinphi*estres(1)
               block(3,i) = block(3,i) + sinphi*estres(2)
               block(4,i) = block(4,i) + sinphi*estres(3)
               block(5,i) = block(5,i) + sinphi*estres(4)
               block(6,i) = block(6,i) - conphi*estres(5)
               block(7,i) = block(7,i) - conphi*estres(6)
               block(8,i) = block(8,i) + sinphi*eforc(1)
               block(9,i) = block(9,i) - conphi*eforc(2)
               block(10,i) = block(10,i) + sinphi*eforc(3)
               block(11,i) = block(11,i) + sinphi*eforc(4)
               block(12,i) = block(12,i) - conphi*eforc(5)
               block(13,i) = block(13,i) + sinphi*eforc(6)
               block(14,i) = block(14,i) + sinphi*eforc(7)
               block(15,i) = block(15,i) - conphi*eforc(8)
               block(16,i) = block(16,i) - sinphi*eforc(9)
               IF ( .NOT.(lsys78) ) THEN
                  block(17,i) = block(17,i) + sinphi*eflux(1)
                  block(18,i) = block(18,i) + sinphi*eflux(2)
                  block(19,i) = block(19,i) - conphi*eflux(3)
                  block(20,i) = block(20,i) + sinphi*echrg(1)
                  block(21,i) = block(21,i) + sinphi*echrg(2)
                  block(22,i) = block(22,i) + sinphi*echrg(3)
               ENDIF
            ELSE
!
               block(2,i) = block(2,i) + conphi*estres(1)
               block(3,i) = block(3,i) + conphi*estres(2)
               block(4,i) = block(4,i) + conphi*estres(3)
               block(5,i) = block(5,i) + conphi*estres(4)
               block(6,i) = block(6,i) + sinphi*estres(5)
               block(7,i) = block(7,i) + sinphi*estres(6)
               block(8,i) = block(8,i) + conphi*eforc(1)
               block(9,i) = block(9,i) + sinphi*eforc(2)
               block(10,i) = block(10,i) + conphi*eforc(3)
               block(11,i) = block(11,i) + conphi*eforc(4)
               block(12,i) = block(12,i) + sinphi*eforc(5)
               block(13,i) = block(13,i) + conphi*eforc(6)
               block(14,i) = block(14,i) + conphi*eforc(7)
               block(15,i) = block(15,i) + sinphi*eforc(8)
               block(16,i) = block(16,i) + conphi*eforc(9)
               IF ( .NOT.(lsys78) ) THEN
                  block(17,i) = block(17,i) + conphi*eflux(1)
                  block(18,i) = block(18,i) + conphi*eflux(2)
                  block(19,i) = block(19,i) + sinphi*eflux(3)
                  block(20,i) = block(20,i) + conphi*echrg(1)
                  block(21,i) = block(21,i) + conphi*echrg(2)
                  block(22,i) = block(22,i) + conphi*echrg(3)
               ENDIF
            ENDIF
         ELSE
            DO iwa = 1 , 6
               block(iwa+1,i) = estres(iwa)
               block(iwa+7,i) = eforc(iwa)
            ENDDO
            block(14,i) = eforc(7)
            block(15,i) = eforc(8)
            block(16,i) = eforc(9)
!
            IF ( .NOT.(lsys78) ) THEN
               block(17,i) = eflux(1)
               block(18,i) = eflux(2)
               block(19,i) = eflux(3)
               block(20,i) = echrg(1)
               block(21,i) = echrg(2)
               block(22,i) = echrg(3)
            ENDIF
         ENDIF
      ENDDO SPAG_Loop_1_1
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!     FLUXES ARE EQUIVALENCED INTO STRES(J)
!     CHARGES ARE WRITTEN INTO FORCE(J)
!
      j = 2
      istres(1) = elemid
      istres(2) = nelhar
      DO i = 1 , ncomp
         j = j + 1
         stres(j) = estres(i)
      ENDDO
      k = 0
      j = 2
      iforce(1) = elemid
      iforce(2) = nelhar
      DO i = 1 , numpt
         DO kk = 1 , ndof
            j = j + 1
            k = k + 1
            force(j) = eforc(k)
!
            IF ( k==3 .OR. k==6 .OR. k==9 ) THEN
               j = j + 1
               k3 = k/3
               force(j) = echrg(k3)
            ENDIF
         ENDDO
      ENDDO
!
      IF ( ktype==1 .OR. (ktype==2 .AND. ipart==1) ) RETURN
   ENDIF
!
!     INSERT HARMONIC STRESSES AND FORCES INTO CLOCK
!
   SPAG_Loop_1_2: DO i = 1 , 14
      IF ( iclock(1,i)==1 ) EXIT SPAG_Loop_1_2
      IF ( harm/=0.0 ) THEN
         nphi = harm*clock(1,i)*degrad
         sinphi = sin(nphi)
         conphi = cos(nphi)
!
         IF ( Sorc==1 ) THEN
!
            clock(2,i) = clock(2,i) + sinphi*estres(1)
            clock(3,i) = clock(3,i) + sinphi*estres(2)
            clock(4,i) = clock(4,i) + sinphi*estres(3)
            clock(5,i) = clock(5,i) + sinphi*estres(4)
            clock(6,i) = clock(6,i) - conphi*estres(5)
            clock(7,i) = clock(7,i) - conphi*estres(6)
            clock(8,i) = clock(8,i) + sinphi*eforc(1)
            clock(9,i) = clock(9,i) - conphi*eforc(2)
            clock(10,i) = clock(10,i) + sinphi*eforc(3)
            clock(11,i) = clock(11,i) + sinphi*eforc(4)
            clock(12,i) = clock(12,i) - conphi*eforc(5)
            clock(13,i) = clock(13,i) + sinphi*eforc(6)
            clock(14,i) = clock(14,i) + sinphi*eforc(7)
            clock(15,i) = clock(15,i) - conphi*eforc(8)
            clock(16,i) = clock(16,i) + sinphi*eforc(9)
            IF ( .NOT.(lsys78) ) THEN
               clock(17,i) = clock(17,i) + sinphi*eflux(1)
               clock(18,i) = clock(18,i) + sinphi*eflux(2)
               clock(19,i) = clock(19,i) - conphi*eflux(3)
               clock(20,i) = clock(20,i) + sinphi*echrg(1)
               clock(21,i) = clock(21,i) + sinphi*echrg(2)
               clock(22,i) = clock(22,i) + sinphi*echrg(3)
            ENDIF
         ELSE
!
            clock(2,i) = clock(2,i) + conphi*estres(1)
            clock(3,i) = clock(3,i) + conphi*estres(2)
            clock(4,i) = clock(4,i) + conphi*estres(3)
            clock(5,i) = clock(5,i) + conphi*estres(4)
            clock(6,i) = clock(6,i) + sinphi*estres(5)
            clock(7,i) = clock(7,i) + sinphi*estres(6)
            clock(8,i) = clock(8,i) + conphi*eforc(1)
            clock(9,i) = clock(9,i) + sinphi*eforc(2)
            clock(10,i) = clock(10,i) + conphi*eforc(3)
            clock(11,i) = clock(11,i) + conphi*eforc(4)
            clock(12,i) = clock(12,i) + sinphi*eforc(5)
            clock(13,i) = clock(13,i) + conphi*eforc(6)
            clock(14,i) = clock(14,i) + conphi*eforc(7)
            clock(15,i) = clock(15,i) + sinphi*eforc(8)
            clock(16,i) = clock(16,i) + conphi*eforc(9)
!
            IF ( .NOT.(lsys78) ) THEN
               clock(17,i) = clock(17,i) + conphi*eflux(1)
               clock(18,i) = clock(18,i) + conphi*eflux(2)
               clock(19,i) = clock(19,i) + sinphi*eflux(3)
               clock(20,i) = clock(20,i) + conphi*echrg(1)
               clock(21,i) = clock(21,i) + conphi*echrg(2)
               clock(22,i) = clock(22,i) + conphi*echrg(3)
            ENDIF
         ENDIF
      ELSE
         DO iwa = 1 , 6
            clock(iwa+1,i) = estres(iwa)
            clock(iwa+7,i) = eforc(iwa)
         ENDDO
         clock(14,i) = eforc(7)
         clock(15,i) = eforc(8)
         clock(16,i) = eforc(9)
!
         IF ( .NOT.(lsys78) ) THEN
            clock(17,i) = eflux(1)
            clock(18,i) = eflux(2)
            clock(19,i) = eflux(3)
            clock(20,i) = echrg(1)
            clock(21,i) = echrg(2)
            clock(22,i) = echrg(3)
         ENDIF
      ENDIF
   ENDDO SPAG_Loop_1_2
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!     FLUXES ARE EQUIVALENCED INTO STRES(J)
!     CHARGES ARE WRITTEN INTO FORCE(J)
!
   j = 2
   istres(1) = elemid
   istres(2) = nelhar
   DO i = 1 , ncomp
      j = j + 1
      stres(j) = estres(i)
   ENDDO
   k = 0
   j = 2
   iforce(1) = elemid
   iforce(2) = nelhar
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         force(j) = eforc(k)
!
         IF ( k==3 .OR. k==6 .OR. k==9 ) THEN
            j = j + 1
            k3 = k/3
            force(j) = echrg(k3)
         ENDIF
      ENDDO
   ENDDO
!
END SUBROUTINE strax2
