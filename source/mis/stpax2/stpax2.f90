!*==stpax2.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpax2(Sorc,Ti)
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
   REAL , DIMENSION(4) :: Ti
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(16) :: akph2
   REAL , DIMENSION(48) :: akuph
   REAL , DIMENSION(15) :: d15 , eflux
   REAL , DIMENSION(30) :: d30
   REAL , DIMENSION(4) :: d4 , dispp , echrg
   REAL :: degrad , dt , t
   REAL , DIMENSION(225) :: dum3
   REAL , DIMENSION(25) :: force
   INTEGER :: i , ie , ii , iloc , ilocp , ir , j , k , ke , kepz , kepz2 , kk , kr , kr3 , krpz , l , ldtemp , ncomp , ndof , ns , &
            & nsp , numpt
   INTEGER , DIMENSION(62,14) :: iblock , iclock
   INTEGER , DIMENSION(25) :: iforce
   INTEGER , SAVE :: iosorc
   INTEGER , DIMENSION(100) :: istres
   LOGICAL :: lsys78 , zero
   REAL , DIMENSION(120) :: selp1
   REAL , DIMENSION(180) :: selp2
   REAL , DIMENSION(60) :: selp3
   REAL , DIMENSION(100) :: stres
   LOGICAL , SAVE :: zeron
   EXTERNAL gmmats
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS PHASE II OF STRESS RECOVERY FOR THE TRAPEZOIDAL
!     CROSS SECTION RING
!
!     OUTPUTS FROM PHASE I ARE THE FOLLOWING..
!     IDEL, IGP(4), TZ, SEL(360), TS(06), AK(144), PHI(14)
!     AKUPH(48), AKPH2(16), SELP1(120), SELP2(180), SELP3(60)
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
   !>>>>EQUIVALENCE (Iblock(1,1),Block(1,1)) , (Iclock(1,1),Clock(1,1)) , (Dum3(1),Idel) , (Dum3(101),Stres(1),Istres(1)) ,              &
!>>>>    & (Dum3(201),Force(1),Iforce(1)) , (Consts(4),Degrad) , (Ldtemp,Templd) , (Dum2(1),Akuph(1)) , (Dum2(49),Akph2(1)) ,            &
!>>>>    & (Dum2(65),Selp1(1)) , (Dum2(185),Selp2(1)) , (Dum2(365),Selp3(1)) , (Unu(1),D4(1)) , (Unu(5),D15(1)) , (Unu(20),D30(1))
   DATA zeron/.FALSE./
   DATA iosorc/0/
!
   Elemid = Idel/1000
   Nelhar = Idel - Elemid*1000
   Klemid = Elemid
   lsys78 = .FALSE.
   IF ( Ksys78==0 .OR. Ksys78==2 ) lsys78 = .TRUE.
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
         DO i = 2 , 62
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
!     INITIALIZE LOCAL VARIABLES
!
   ndof = 3
   numpt = 4
   N = ndof*numpt
   nsp = 5
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
!
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
   DO i = 1 , 4
      echrg(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(akuph(1),N,numpt,0,dispp(1),numpt,1,0,d15(1))
      DO i = 1 , 12
         Eforc(i) = Eforc(i) + d15(i)
      ENDDO
!
      CALL gmmats(akuph(1),N,numpt,1,Disp(1),N,1,0,d4(1))
      CALL gmmats(akph2(1),numpt,numpt,0,dispp(1),numpt,1,0,echrg(1))
      DO i = 1 , 4
         echrg(i) = echrg(i) + d4(i)
      ENDDO
   ENDIF
!
!     COMPUTE THE STRESSES
!
   CALL gmmats(Sel(1),ns,N,0,Disp(1),N,1,0,Estres(1))
!
   DO i = 1 , 15
      eflux(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(selp1(1),ns,numpt,0,dispp(1),numpt,1,0,d30(1))
      DO i = 1 , 30
         Estres(i) = Estres(i) + d30(i)
      ENDDO
!
      CALL gmmats(selp2(1),15,N,0,Disp(1),N,1,0,eflux(1))
      CALL gmmats(selp3(1),15,numpt,0,dispp(1),numpt,1,0,d15(1))
      DO i = 1 , 15
         eflux(i) = eflux(i) + d15(i)
      ENDDO
   ENDIF
!
!     COMPUTE THERMAL STRESS IF IT IS EXISTS
!
   IF ( ldtemp/=-1 ) THEN
      k = 0
      t = Tz
      IF ( Harm>0.0 ) t = 0.0
      DO i = 1 , nsp
         dt = Ti(i) - t
         IF ( i==5 ) dt = (Ti(1)+Ti(2)+Ti(3)+Ti(4))/4.0 - t
         DO j = 1 , ncomp
            k = k + 1
            Estres(k) = Estres(k) - dt*Ts(j)
         ENDDO
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
         IF ( Harm==0.0 ) THEN
!
            DO ie = 1 , 5
               ke = 9*(ie-1)
               kepz = 6*(ie-1)
               Block(2+ke,i) = Estres(1+kepz)
               Block(3+ke,i) = Estres(2+kepz)
               Block(4+ke,i) = Estres(3+kepz)
               Block(5+ke,i) = Estres(4+kepz)
               Block(6+ke,i) = Estres(5+kepz)
               Block(7+ke,i) = Estres(6+kepz)
!
               IF ( .NOT.(lsys78) ) THEN
                  kepz2 = kepz/2
                  Block(8+ke,i) = eflux(1+kepz2)
                  Block(9+ke,i) = eflux(2+kepz2)
                  Block(10+ke,i) = eflux(3+kepz2)
               ENDIF
            ENDDO
!
            DO ir = 1 , 4
               kr = 4*(ir-1)
               krpz = 3*(ir-1)
               Block(47+kr,i) = Eforc(1+krpz)
               Block(48+kr,i) = Eforc(2+krpz)
               Block(49+kr,i) = Eforc(3+krpz)
               kr3 = 1 + krpz/3
               IF ( .NOT.lsys78 ) Block(50+kr,i) = echrg(kr3)
            ENDDO
         ELSE
            Nphi = Harm*Block(1,i)*degrad
            Sinphi = sin(Nphi)
            Conphi = cos(Nphi)
            IF ( Sorc==1 ) THEN
!
               DO ie = 1 , 5
                  ke = 9*(ie-1)
                  kepz = 6*(ie-1)
                  Block(2+ke,i) = Block(2+ke,i) + Sinphi*Estres(1+kepz)
                  Block(3+ke,i) = Block(3+ke,i) + Sinphi*Estres(2+kepz)
                  Block(4+ke,i) = Block(4+ke,i) + Sinphi*Estres(3+kepz)
                  Block(5+ke,i) = Block(5+ke,i) + Sinphi*Estres(4+kepz)
                  Block(6+ke,i) = Block(6+ke,i) - Conphi*Estres(5+kepz)
                  Block(7+ke,i) = Block(7+ke,i) - Conphi*Estres(6+kepz)
!
                  IF ( .NOT.(lsys78) ) THEN
                     kepz2 = kepz/2
                     Block(8+ke,i) = Block(8+ke,i) + Sinphi*eflux(1+kepz2)
                     Block(9+ke,i) = Block(9+ke,i) + Sinphi*eflux(2+kepz2)
                     Block(10+ke,i) = Block(10+ke,i) - Conphi*eflux(3+kepz2)
                  ENDIF
               ENDDO
!
               DO ir = 1 , 4
                  kr = 4*(ir-1)
                  krpz = 3*(ir-1)
                  Block(47+kr,i) = Block(47+kr,i) + Sinphi*Eforc(1+krpz)
                  Block(48+kr,i) = Block(48+kr,i) - Conphi*Eforc(2+krpz)
                  Block(49+kr,i) = Block(49+kr,i) + Sinphi*Eforc(3+krpz)
                  kr3 = 1 + krpz/3
                  IF ( .NOT.lsys78 ) Block(50+kr,i) = Block(50+kr,i) + Sinphi*echrg(kr3)
               ENDDO
            ELSE
!
               DO ie = 1 , 5
                  ke = 9*(ie-1)
                  kepz = 6*(ie-1)
                  Block(2+ke,i) = Block(2+ke,i) + Conphi*Estres(1+kepz)
                  Block(3+ke,i) = Block(3+ke,i) + Conphi*Estres(2+kepz)
                  Block(4+ke,i) = Block(4+ke,i) + Conphi*Estres(3+kepz)
                  Block(5+ke,i) = Block(5+ke,i) + Conphi*Estres(4+kepz)
                  Block(6+ke,i) = Block(6+ke,i) + Sinphi*Estres(5+kepz)
                  Block(7+ke,i) = Block(7+ke,i) + Sinphi*Estres(6+kepz)
!
                  IF ( .NOT.(lsys78) ) THEN
                     kepz2 = kepz/2
                     Block(8+ke,i) = Block(8+ke,i) + Conphi*eflux(1+kepz2)
                     Block(9+ke,i) = Block(9+ke,i) + Conphi*eflux(2+kepz2)
                     Block(10+ke,i) = Block(10+ke,i) + Sinphi*eflux(3+kepz2)
                  ENDIF
               ENDDO
!
               DO ir = 1 , 4
                  kr = 4*(ir-1)
                  krpz = 3*(ir-1)
                  Block(47+kr,i) = Block(47+kr,i) + Conphi*Eforc(1+krpz)
                  Block(48+kr,i) = Block(48+kr,i) + Sinphi*Eforc(2+krpz)
                  Block(49+kr,i) = Block(49+kr,i) + Conphi*Eforc(3+krpz)
                  kr3 = 1 + krpz/3
                  IF ( .NOT.lsys78 ) Block(50+kr,i) = Block(50+kr,i) + Conphi*echrg(kr3)
               ENDDO
            ENDIF
         ENDIF
!
      ENDDO SPAG_Loop_1_1
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!
      j = 2
      k = 1
      l = 0
      istres(1) = Elemid
      istres(2) = Nelhar
      DO i = 1 , ns
         j = j + 1
         stres(j) = Estres(i)
!
         IF ( i/6==k ) THEN
            k = k + 1
            DO ii = 1 , 3
               j = j + 1
               l = l + 1
               stres(j) = eflux(l)
            ENDDO
         ENDIF
!
      ENDDO
      k = 0
      j = 2
      l = 1
      iforce(1) = Elemid
      iforce(2) = Nelhar
      DO i = 1 , numpt
         DO kk = 1 , ndof
            j = j + 1
            k = k + 1
            force(j) = Eforc(k)
!
            IF ( k/3==l ) THEN
               j = j + 1
               force(j) = echrg(l)
               l = l + 1
            ENDIF
!
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
      IF ( Harm==0.0 ) THEN
!
         DO ie = 1 , 5
            ke = 9*(ie-1)
            kepz = 6*(ie-1)
            Clock(2+ke,i) = Estres(1+kepz)
            Clock(3+ke,i) = Estres(2+kepz)
            Clock(4+ke,i) = Estres(3+kepz)
            Clock(5+ke,i) = Estres(4+kepz)
            Clock(6+ke,i) = Estres(5+kepz)
            Clock(7+ke,i) = Estres(6+kepz)
!
            IF ( .NOT.(lsys78) ) THEN
               kepz2 = kepz/2
               Clock(8+ke,i) = eflux(1+kepz2)
               Clock(9+ke,i) = eflux(2+kepz2)
               Clock(10+ke,i) = eflux(3+kepz2)
            ENDIF
         ENDDO
!
         DO ir = 1 , 4
            kr = 4*(ir-1)
            krpz = 3*(ir-1)
            Clock(47+kr,i) = Eforc(1+krpz)
            Clock(48+kr,i) = Eforc(2+krpz)
            Clock(49+kr,i) = Eforc(3+krpz)
            kr3 = 1 + krpz/3
            IF ( .NOT.lsys78 ) Clock(50+kr,i) = echrg(kr3)
         ENDDO
      ELSE
         Nphi = Harm*Clock(1,i)*degrad
         Sinphi = sin(Nphi)
         Conphi = cos(Nphi)
         IF ( Sorc==1 ) THEN
!
            DO ie = 1 , 5
               ke = 9*(ie-1)
               kepz = 6*(ie-1)
               Clock(2+ke,i) = Clock(2+ke,i) + Sinphi*Estres(1+kepz)
               Clock(3+ke,i) = Clock(3+ke,i) + Sinphi*Estres(2+kepz)
               Clock(4+ke,i) = Clock(4+ke,i) + Sinphi*Estres(3+kepz)
               Clock(5+ke,i) = Clock(5+ke,i) + Sinphi*Estres(4+kepz)
               Clock(6+ke,i) = Clock(6+ke,i) - Conphi*Estres(5+kepz)
               Clock(7+ke,i) = Clock(7+ke,i) - Conphi*Estres(6+kepz)
!
               IF ( .NOT.(lsys78) ) THEN
                  kepz2 = kepz/2
                  Clock(8+ke,i) = Clock(8+ke,i) + Sinphi*eflux(1+kepz2)
                  Clock(9+ke,i) = Clock(9+ke,i) + Sinphi*eflux(2+kepz2)
                  Clock(10+ke,i) = Clock(10+ke,i) - Conphi*eflux(3+kepz2)
               ENDIF
            ENDDO
!
            DO ir = 1 , 4
               kr = 4*(ir-1)
               krpz = 3*(ir-1)
               Clock(47+kr,i) = Clock(47+kr,i) + Sinphi*Eforc(1+krpz)
               Clock(48+kr,i) = Clock(48+kr,i) - Conphi*Eforc(2+krpz)
               Clock(49+kr,i) = Clock(49+kr,i) + Sinphi*Eforc(3+krpz)
               kr3 = 1 + krpz/3
               IF ( .NOT.lsys78 ) Clock(50+kr,i) = Clock(50+kr,i) + Sinphi*echrg(kr3)
            ENDDO
         ELSE
!
            DO ie = 1 , 5
               ke = 9*(ie-1)
               kepz = 6*(ie-1)
               Clock(2+ke,i) = Clock(2+ke,i) + Conphi*Estres(1+kepz)
               Clock(3+ke,i) = Clock(3+ke,i) + Conphi*Estres(2+kepz)
               Clock(4+ke,i) = Clock(4+ke,i) + Conphi*Estres(3+kepz)
               Clock(5+ke,i) = Clock(5+ke,i) + Conphi*Estres(4+kepz)
               Clock(6+ke,i) = Clock(6+ke,i) + Sinphi*Estres(5+kepz)
               Clock(7+ke,i) = Clock(7+ke,i) + Sinphi*Estres(6+kepz)
!
               IF ( .NOT.(lsys78) ) THEN
                  kepz2 = kepz/2
                  Clock(8+ke,i) = Clock(8+ke,i) + Conphi*eflux(1+kepz2)
                  Clock(9+ke,i) = Clock(9+ke,i) + Conphi*eflux(2+kepz2)
                  Clock(10+ke,i) = Clock(10+ke,i) + Sinphi*eflux(3+kepz2)
               ENDIF
            ENDDO
!
            DO ir = 1 , 4
               kr = 4*(ir-1)
               krpz = 3*(ir-1)
               Clock(47+kr,i) = Clock(47+kr,i) + Conphi*Eforc(1+krpz)
               Clock(48+kr,i) = Clock(48+kr,i) + Sinphi*Eforc(2+krpz)
               Clock(49+kr,i) = Clock(49+kr,i) + Conphi*Eforc(3+krpz)
               kr3 = 1 + krpz/3
               IF ( .NOT.lsys78 ) Clock(50+kr,i) = Clock(50+kr,i) + Conphi*echrg(kr3)
            ENDDO
         ENDIF
      ENDIF
!
   ENDDO SPAG_Loop_1_2
!
!     COPY STRESSES AND FORCES INTO OUTPUT BLOCKS
!
   j = 2
   k = 1
   l = 0
   istres(1) = Elemid
   istres(2) = Nelhar
   DO i = 1 , ns
      j = j + 1
      stres(j) = Estres(i)
!
      IF ( i/6==k ) THEN
         k = k + 1
         DO ii = 1 , 3
            j = j + 1
            l = l + 1
            stres(j) = eflux(l)
         ENDDO
      ENDIF
   ENDDO
!
   k = 0
   j = 2
   l = 1
   iforce(1) = Elemid
   iforce(2) = Nelhar
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         force(j) = Eforc(k)
!
         IF ( k/3==l ) THEN
            j = j + 1
            force(j) = echrg(l)
            l = l + 1
         ENDIF
      ENDDO
   ENDDO
!
!
END SUBROUTINE stpax2
