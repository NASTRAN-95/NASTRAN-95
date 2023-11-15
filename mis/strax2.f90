
SUBROUTINE strax2(Sorc,Ti)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Ak(81) , Akph2(9) , Akuph(27) , Block(22,14) , Clock(22,14) , Conphi , Consts(5) , D3(3) , D6(6) , D9(9) , Degrad , Disp(9) &
      & , Dum1(33) , Dum2(90) , Dum3(225) , Dum4(12) , Dum5(33) , Eflux(3) , Eforc(9) , Eldefm , Estres(9) , Force(25) , Harm ,     &
      & Nphi , Phi(14) , Sel(54) , Selp1(18) , Selp2(27) , Selp3(9) , Sinphi , Stres(100) , Templd , Ts(6) , Tz , Unu(123) , Zz(1)
   INTEGER Elemid , Iblock(22,14) , Iclock(22,14) , Icstm , Idel , Iforce(25) , Igp(3) , Ipart , Istres(100) , Ivec , Ivecn ,       &
         & Ksys78 , Ksystm(77) , Ktype , Ldtemp , N , Nangle , Ncstm , Nelhar
   COMMON /condas/ Consts
   COMMON /sdr2de/ Dum5 , Ipart
   COMMON /sdr2x4/ Dum1 , Icstm , Ncstm , Ivec , Ivecn , Templd , Eldefm , Dum4 , Ktype
   COMMON /sdr2x7/ Idel , Igp , Tz , Sel , Ts , Ak , Phi , Dum2 , Block , Clock
   COMMON /sdr2x8/ Disp , Eforc , Estres , Harm , N , Sinphi , Conphi , Nphi , Nangle , Elemid , Unu , Nelhar
   COMMON /system/ Ksystm , Ksys78
   COMMON /zzzzzz/ Zz
!
! Dummy argument declarations
!
   INTEGER Sorc
   REAL Ti(3)
!
! Local variable declarations
!
   REAL dispp(3) , dt , echrg(3)
   INTEGER i , iloc , ilocp , iosorc , iwa , j , k , k3 , kk , ncomp , ndof , ns , nsp , numpt
   LOGICAL lsys78 , zero , zeron
!
! End of declarations
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
   EQUIVALENCE (Iblock(1,1),Block(1,1)) , (Iclock(1,1),Clock(1,1)) , (Dum3(1),Idel) , (Ldtemp,Templd) ,                             &
    & (Dum3(109),Stres(9),Istres(9),Eflux(1)) , (Dum3(201),Force(1),Iforce(1)) , (Dum2(1),Selp1(1)) , (Dum2(19),Akph2(1)) ,         &
    & (Dum2(28),Akuph(1)) , (Dum2(55),Selp2(1)) , (Dum2(82),Selp3(1)) , (Consts(4),Degrad) , (Unu(1),D3(1)) , (Unu(4),D6(1)) ,      &
    & (Unu(10),D9(1))
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
            Iblock(1,j) = 1
            Iclock(1,j) = 1
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
      CALL gmmats(Akuph(1),N,numpt,0,dispp(1),numpt,1,0,D9(1))
      DO i = 1 , 9
         Eforc(i) = Eforc(i) + D9(i)
      ENDDO
!
      CALL gmmats(Akuph(1),N,numpt,1,Disp(1),N,1,0,D3(1))
      CALL gmmats(Akph2(1),numpt,numpt,0,dispp(1),numpt,1,0,echrg(1))
      DO i = 1 , 3
         echrg(i) = echrg(i) + D3(i)
      ENDDO
   ENDIF
!
!     COMPUTE THE STRESSES
!
   CALL gmmats(Sel(1),ns,N,0,Disp(1),N,1,0,Estres(1))
!
   DO i = 1 , 3
      Eflux(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(Selp1(1),ns,numpt,0,dispp(1),numpt,1,0,D6(1))
      DO i = 1 , 6
         Estres(i) = Estres(i) + D6(i)
      ENDDO
!
      CALL gmmats(Selp2(1),numpt,N,0,Disp(1),N,1,0,Eflux(1))
      CALL gmmats(Selp3(1),numpt,numpt,0,dispp(1),numpt,1,0,D3(1))
!
      DO i = 1 , 3
         Eflux(i) = Eflux(i) + D3(i)
      ENDDO
   ENDIF
!
!     COMPUTE THERMAL STRESS IF IT IS EXISTS
!
   IF ( Ldtemp/=-1 ) THEN
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
      DO i = 1 , 14
         IF ( Iblock(1,i)==1 ) EXIT
         IF ( Harm/=0.0 ) THEN
            Nphi = Harm*Block(1,i)*Degrad
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
                  Block(17,i) = Block(17,i) + Sinphi*Eflux(1)
                  Block(18,i) = Block(18,i) + Sinphi*Eflux(2)
                  Block(19,i) = Block(19,i) - Conphi*Eflux(3)
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
                  Block(17,i) = Block(17,i) + Conphi*Eflux(1)
                  Block(18,i) = Block(18,i) + Conphi*Eflux(2)
                  Block(19,i) = Block(19,i) + Sinphi*Eflux(3)
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
               Block(17,i) = Eflux(1)
               Block(18,i) = Eflux(2)
               Block(19,i) = Eflux(3)
               Block(20,i) = echrg(1)
               Block(21,i) = echrg(2)
               Block(22,i) = echrg(3)
            ENDIF
         ENDIF
      ENDDO
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!     FLUXES ARE EQUIVALENCED INTO STRES(J)
!     CHARGES ARE WRITTEN INTO FORCE(J)
!
      j = 2
      Istres(1) = Elemid
      Istres(2) = Nelhar
      DO i = 1 , ncomp
         j = j + 1
         Stres(j) = Estres(i)
      ENDDO
      k = 0
      j = 2
      Iforce(1) = Elemid
      Iforce(2) = Nelhar
      DO i = 1 , numpt
         DO kk = 1 , ndof
            j = j + 1
            k = k + 1
            Force(j) = Eforc(k)
!
            IF ( k==3 .OR. k==6 .OR. k==9 ) THEN
               j = j + 1
               k3 = k/3
               Force(j) = echrg(k3)
            ENDIF
         ENDDO
      ENDDO
!
      IF ( Ktype==1 .OR. (Ktype==2 .AND. Ipart==1) ) GOTO 99999
   ENDIF
!
!     INSERT HARMONIC STRESSES AND FORCES INTO CLOCK
!
   DO i = 1 , 14
      IF ( Iclock(1,i)==1 ) EXIT
      IF ( Harm/=0.0 ) THEN
         Nphi = Harm*Clock(1,i)*Degrad
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
               Clock(17,i) = Clock(17,i) + Sinphi*Eflux(1)
               Clock(18,i) = Clock(18,i) + Sinphi*Eflux(2)
               Clock(19,i) = Clock(19,i) - Conphi*Eflux(3)
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
               Clock(17,i) = Clock(17,i) + Conphi*Eflux(1)
               Clock(18,i) = Clock(18,i) + Conphi*Eflux(2)
               Clock(19,i) = Clock(19,i) + Sinphi*Eflux(3)
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
            Clock(17,i) = Eflux(1)
            Clock(18,i) = Eflux(2)
            Clock(19,i) = Eflux(3)
            Clock(20,i) = echrg(1)
            Clock(21,i) = echrg(2)
            Clock(22,i) = echrg(3)
         ENDIF
      ENDIF
   ENDDO
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!     FLUXES ARE EQUIVALENCED INTO STRES(J)
!     CHARGES ARE WRITTEN INTO FORCE(J)
!
   j = 2
   Istres(1) = Elemid
   Istres(2) = Nelhar
   DO i = 1 , ncomp
      j = j + 1
      Stres(j) = Estres(i)
   ENDDO
   k = 0
   j = 2
   Iforce(1) = Elemid
   Iforce(2) = Nelhar
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         Force(j) = Eforc(k)
!
         IF ( k==3 .OR. k==6 .OR. k==9 ) THEN
            j = j + 1
            k3 = k/3
            Force(j) = echrg(k3)
         ENDIF
      ENDDO
   ENDDO
!
99999 END SUBROUTINE strax2
