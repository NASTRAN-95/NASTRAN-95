!*==strax2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE strax2(Sorc,Ti)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_SDR2DE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_SYSTEM
   USE C_ZZZZZZ
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
   IF ( Ksys78==0 .OR. Ksys78==2 ) lsys78 = .TRUE.
!
   Elemid = Idel/1000
   Nelhar = Idel - Elemid*1000
!
!     SET BLOCK = 0 IF HARMONIC = 0
!
   N = Nelhar - 1
   IF ( N==0 ) THEN
      IF ( N==0 .AND. zeron .AND. iosorc/=Sorc ) THEN
         zeron = .FALSE.
      ELSE
         zeron = .TRUE.
         iosorc = Sorc
         DO i = 2 , 22
            DO j = 1 , 14
               IF ( Ktype/=2 .OR. Ipart/=2 ) Block(i,j) = 0.0
               Clock(i,j) = 0.0
            ENDDO
         ENDDO
!
!     SET ANGLES CONTROL FOR SUMMATION
!
         zero = .FALSE.
         j = 0
         DO i = 1 , 14
            IF ( Phi(i)==0 ) THEN
               IF ( zero ) CYCLE
               zero = .TRUE.
            ENDIF
            j = j + 1
            Block(1,j) = Phi(i)
            Clock(1,j) = Phi(i)
         ENDDO
         j = j + 1
         IF ( j<=14 ) THEN
            iblock(1,j) = 1
            iclock(1,j) = 1
         ENDIF
      ENDIF
   ENDIF
   Harm = N
!
!     INITIALIZE LOCAT VARIABLES
!
   ndof = 3
   numpt = 3
   N = ndof*numpt
   nsp = 1
   ncomp = 6
   ns = nsp*ncomp
!
!     FIND GRID POINTS DISPLACEMENTS
!
   k = 0
   DO i = 1 , numpt
      iloc = Ivec + Igp(i) - 2
!
      IF ( .NOT.(lsys78) ) THEN
         ilocp = iloc + 4
         dispp(i) = Zz(ilocp)
      ENDIF
      DO j = 1 , ndof
         iloc = iloc + 1
         k = k + 1
         Disp(k) = Zz(iloc)
      ENDDO
   ENDDO
!
!     COMPUTE THE GRID POINT FORCES
!
   CALL gmmats(Ak(1),N,N,0,Disp(1),N,1,0,Eforc(1))
!
   DO i = 1 , 3
      echrg(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(akuph(1),N,numpt,0,dispp(1),numpt,1,0,d9(1))
      DO i = 1 , 9
         Eforc(i) = Eforc(i) + d9(i)
      ENDDO
!
      CALL gmmats(akuph(1),N,numpt,1,Disp(1),N,1,0,d3(1))
      CALL gmmats(akph2(1),numpt,numpt,0,dispp(1),numpt,1,0,echrg(1))
      DO i = 1 , 3
         echrg(i) = echrg(i) + d3(i)
      ENDDO
   ENDIF
!
!     COMPUTE THE STRESSES
!
   CALL gmmats(Sel(1),ns,N,0,Disp(1),N,1,0,Estres(1))
!
   DO i = 1 , 3
      eflux(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(selp1(1),ns,numpt,0,dispp(1),numpt,1,0,d6(1))
      DO i = 1 , 6
         Estres(i) = Estres(i) + d6(i)
      ENDDO
!
      CALL gmmats(selp2(1),numpt,N,0,Disp(1),N,1,0,eflux(1))
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
      dt = Tz
      IF ( Harm>0.0 ) dt = 0.0
      dt = (Ti(1)+Ti(2)+Ti(3))/3.0 - dt
      DO i = 1 , ns
         Estres(i) = Estres(i) - dt*Ts(i)
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
   IF ( Ktype/=2 .OR. Ipart/=2 ) THEN
!
!     INSERT HARMONIC STRESSES AND FORCES INTO BLOCK
!
      SPAG_Loop_1_1: DO i = 1 , 14
         IF ( iblock(1,i)==1 ) EXIT SPAG_Loop_1_1
         IF ( Harm/=0.0 ) THEN
            Nphi = Harm*Block(1,i)*degrad
            Sinphi = sin(Nphi)
            Conphi = cos(Nphi)
!
            IF ( Sorc==1 ) THEN
               Block(2,i) = Block(2,i) + Sinphi*Estres(1)
               Block(3,i) = Block(3,i) + Sinphi*Estres(2)
               Block(4,i) = Block(4,i) + Sinphi*Estres(3)
               Block(5,i) = Block(5,i) + Sinphi*Estres(4)
               Block(6,i) = Block(6,i) - Conphi*Estres(5)
               Block(7,i) = Block(7,i) - Conphi*Estres(6)
               Block(8,i) = Block(8,i) + Sinphi*Eforc(1)
               Block(9,i) = Block(9,i) - Conphi*Eforc(2)
               Block(10,i) = Block(10,i) + Sinphi*Eforc(3)
               Block(11,i) = Block(11,i) + Sinphi*Eforc(4)
               Block(12,i) = Block(12,i) - Conphi*Eforc(5)
               Block(13,i) = Block(13,i) + Sinphi*Eforc(6)
               Block(14,i) = Block(14,i) + Sinphi*Eforc(7)
               Block(15,i) = Block(15,i) - Conphi*Eforc(8)
               Block(16,i) = Block(16,i) - Sinphi*Eforc(9)
               IF ( .NOT.(lsys78) ) THEN
                  Block(17,i) = Block(17,i) + Sinphi*eflux(1)
                  Block(18,i) = Block(18,i) + Sinphi*eflux(2)
                  Block(19,i) = Block(19,i) - Conphi*eflux(3)
                  Block(20,i) = Block(20,i) + Sinphi*echrg(1)
                  Block(21,i) = Block(21,i) + Sinphi*echrg(2)
                  Block(22,i) = Block(22,i) + Sinphi*echrg(3)
               ENDIF
            ELSE
!
               Block(2,i) = Block(2,i) + Conphi*Estres(1)
               Block(3,i) = Block(3,i) + Conphi*Estres(2)
               Block(4,i) = Block(4,i) + Conphi*Estres(3)
               Block(5,i) = Block(5,i) + Conphi*Estres(4)
               Block(6,i) = Block(6,i) + Sinphi*Estres(5)
               Block(7,i) = Block(7,i) + Sinphi*Estres(6)
               Block(8,i) = Block(8,i) + Conphi*Eforc(1)
               Block(9,i) = Block(9,i) + Sinphi*Eforc(2)
               Block(10,i) = Block(10,i) + Conphi*Eforc(3)
               Block(11,i) = Block(11,i) + Conphi*Eforc(4)
               Block(12,i) = Block(12,i) + Sinphi*Eforc(5)
               Block(13,i) = Block(13,i) + Conphi*Eforc(6)
               Block(14,i) = Block(14,i) + Conphi*Eforc(7)
               Block(15,i) = Block(15,i) + Sinphi*Eforc(8)
               Block(16,i) = Block(16,i) + Conphi*Eforc(9)
               IF ( .NOT.(lsys78) ) THEN
                  Block(17,i) = Block(17,i) + Conphi*eflux(1)
                  Block(18,i) = Block(18,i) + Conphi*eflux(2)
                  Block(19,i) = Block(19,i) + Sinphi*eflux(3)
                  Block(20,i) = Block(20,i) + Conphi*echrg(1)
                  Block(21,i) = Block(21,i) + Conphi*echrg(2)
                  Block(22,i) = Block(22,i) + Conphi*echrg(3)
               ENDIF
            ENDIF
         ELSE
            DO iwa = 1 , 6
               Block(iwa+1,i) = Estres(iwa)
               Block(iwa+7,i) = Eforc(iwa)
            ENDDO
            Block(14,i) = Eforc(7)
            Block(15,i) = Eforc(8)
            Block(16,i) = Eforc(9)
!
            IF ( .NOT.(lsys78) ) THEN
               Block(17,i) = eflux(1)
               Block(18,i) = eflux(2)
               Block(19,i) = eflux(3)
               Block(20,i) = echrg(1)
               Block(21,i) = echrg(2)
               Block(22,i) = echrg(3)
            ENDIF
         ENDIF
      ENDDO SPAG_Loop_1_1
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!     FLUXES ARE EQUIVALENCED INTO STRES(J)
!     CHARGES ARE WRITTEN INTO FORCE(J)
!
      j = 2
      istres(1) = Elemid
      istres(2) = Nelhar
      DO i = 1 , ncomp
         j = j + 1
         stres(j) = Estres(i)
      ENDDO
      k = 0
      j = 2
      iforce(1) = Elemid
      iforce(2) = Nelhar
      DO i = 1 , numpt
         DO kk = 1 , ndof
            j = j + 1
            k = k + 1
            force(j) = Eforc(k)
!
            IF ( k==3 .OR. k==6 .OR. k==9 ) THEN
               j = j + 1
               k3 = k/3
               force(j) = echrg(k3)
            ENDIF
         ENDDO
      ENDDO
!
      IF ( Ktype==1 .OR. (Ktype==2 .AND. Ipart==1) ) RETURN
   ENDIF
!
!     INSERT HARMONIC STRESSES AND FORCES INTO CLOCK
!
   SPAG_Loop_1_2: DO i = 1 , 14
      IF ( iclock(1,i)==1 ) EXIT SPAG_Loop_1_2
      IF ( Harm/=0.0 ) THEN
         Nphi = Harm*Clock(1,i)*degrad
         Sinphi = sin(Nphi)
         Conphi = cos(Nphi)
!
         IF ( Sorc==1 ) THEN
!
            Clock(2,i) = Clock(2,i) + Sinphi*Estres(1)
            Clock(3,i) = Clock(3,i) + Sinphi*Estres(2)
            Clock(4,i) = Clock(4,i) + Sinphi*Estres(3)
            Clock(5,i) = Clock(5,i) + Sinphi*Estres(4)
            Clock(6,i) = Clock(6,i) - Conphi*Estres(5)
            Clock(7,i) = Clock(7,i) - Conphi*Estres(6)
            Clock(8,i) = Clock(8,i) + Sinphi*Eforc(1)
            Clock(9,i) = Clock(9,i) - Conphi*Eforc(2)
            Clock(10,i) = Clock(10,i) + Sinphi*Eforc(3)
            Clock(11,i) = Clock(11,i) + Sinphi*Eforc(4)
            Clock(12,i) = Clock(12,i) - Conphi*Eforc(5)
            Clock(13,i) = Clock(13,i) + Sinphi*Eforc(6)
            Clock(14,i) = Clock(14,i) + Sinphi*Eforc(7)
            Clock(15,i) = Clock(15,i) - Conphi*Eforc(8)
            Clock(16,i) = Clock(16,i) + Sinphi*Eforc(9)
            IF ( .NOT.(lsys78) ) THEN
               Clock(17,i) = Clock(17,i) + Sinphi*eflux(1)
               Clock(18,i) = Clock(18,i) + Sinphi*eflux(2)
               Clock(19,i) = Clock(19,i) - Conphi*eflux(3)
               Clock(20,i) = Clock(20,i) + Sinphi*echrg(1)
               Clock(21,i) = Clock(21,i) + Sinphi*echrg(2)
               Clock(22,i) = Clock(22,i) + Sinphi*echrg(3)
            ENDIF
         ELSE
!
            Clock(2,i) = Clock(2,i) + Conphi*Estres(1)
            Clock(3,i) = Clock(3,i) + Conphi*Estres(2)
            Clock(4,i) = Clock(4,i) + Conphi*Estres(3)
            Clock(5,i) = Clock(5,i) + Conphi*Estres(4)
            Clock(6,i) = Clock(6,i) + Sinphi*Estres(5)
            Clock(7,i) = Clock(7,i) + Sinphi*Estres(6)
            Clock(8,i) = Clock(8,i) + Conphi*Eforc(1)
            Clock(9,i) = Clock(9,i) + Sinphi*Eforc(2)
            Clock(10,i) = Clock(10,i) + Conphi*Eforc(3)
            Clock(11,i) = Clock(11,i) + Conphi*Eforc(4)
            Clock(12,i) = Clock(12,i) + Sinphi*Eforc(5)
            Clock(13,i) = Clock(13,i) + Conphi*Eforc(6)
            Clock(14,i) = Clock(14,i) + Conphi*Eforc(7)
            Clock(15,i) = Clock(15,i) + Sinphi*Eforc(8)
            Clock(16,i) = Clock(16,i) + Conphi*Eforc(9)
!
            IF ( .NOT.(lsys78) ) THEN
               Clock(17,i) = Clock(17,i) + Conphi*eflux(1)
               Clock(18,i) = Clock(18,i) + Conphi*eflux(2)
               Clock(19,i) = Clock(19,i) + Sinphi*eflux(3)
               Clock(20,i) = Clock(20,i) + Conphi*echrg(1)
               Clock(21,i) = Clock(21,i) + Conphi*echrg(2)
               Clock(22,i) = Clock(22,i) + Conphi*echrg(3)
            ENDIF
         ENDIF
      ELSE
         DO iwa = 1 , 6
            Clock(iwa+1,i) = Estres(iwa)
            Clock(iwa+7,i) = Eforc(iwa)
         ENDDO
         Clock(14,i) = Eforc(7)
         Clock(15,i) = Eforc(8)
         Clock(16,i) = Eforc(9)
!
         IF ( .NOT.(lsys78) ) THEN
            Clock(17,i) = eflux(1)
            Clock(18,i) = eflux(2)
            Clock(19,i) = eflux(3)
            Clock(20,i) = echrg(1)
            Clock(21,i) = echrg(2)
            Clock(22,i) = echrg(3)
         ENDIF
      ENDIF
   ENDDO SPAG_Loop_1_2
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!     FLUXES ARE EQUIVALENCED INTO STRES(J)
!     CHARGES ARE WRITTEN INTO FORCE(J)
!
   j = 2
   istres(1) = Elemid
   istres(2) = Nelhar
   DO i = 1 , ncomp
      j = j + 1
      stres(j) = Estres(i)
   ENDDO
   k = 0
   j = 2
   iforce(1) = Elemid
   iforce(2) = Nelhar
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         force(j) = Eforc(k)
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
