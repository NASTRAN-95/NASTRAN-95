!*==stpax2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpax2(Sorc,Ti)
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
   elemid = idel/1000
   nelhar = idel - elemid*1000
   klemid = elemid
   lsys78 = .FALSE.
   IF ( ksys78==0 .OR. ksys78==2 ) lsys78 = .TRUE.
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
         DO i = 2 , 62
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
!     INITIALIZE LOCAL VARIABLES
!
   ndof = 3
   numpt = 4
   n = ndof*numpt
   nsp = 5
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
!
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
   DO i = 1 , 4
      echrg(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(akuph(1),n,numpt,0,dispp(1),numpt,1,0,d15(1))
      DO i = 1 , 12
         eforc(i) = eforc(i) + d15(i)
      ENDDO
!
      CALL gmmats(akuph(1),n,numpt,1,disp(1),n,1,0,d4(1))
      CALL gmmats(akph2(1),numpt,numpt,0,dispp(1),numpt,1,0,echrg(1))
      DO i = 1 , 4
         echrg(i) = echrg(i) + d4(i)
      ENDDO
   ENDIF
!
!     COMPUTE THE STRESSES
!
   CALL gmmats(sel(1),ns,n,0,disp(1),n,1,0,estres(1))
!
   DO i = 1 , 15
      eflux(i) = 0.0
   ENDDO
!
   IF ( .NOT.(lsys78) ) THEN
      CALL gmmats(selp1(1),ns,numpt,0,dispp(1),numpt,1,0,d30(1))
      DO i = 1 , 30
         estres(i) = estres(i) + d30(i)
      ENDDO
!
      CALL gmmats(selp2(1),15,n,0,disp(1),n,1,0,eflux(1))
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
      t = tz
      IF ( harm>0.0 ) t = 0.0
      DO i = 1 , nsp
         dt = Ti(i) - t
         IF ( i==5 ) dt = (Ti(1)+Ti(2)+Ti(3)+Ti(4))/4.0 - t
         DO j = 1 , ncomp
            k = k + 1
            estres(k) = estres(k) - dt*ts(j)
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
   IF ( ktype/=2 .OR. ipart/=2 ) THEN
!
!     INSERT HARMONIC STRESSES AND FORCES INTO BLOCK
!
      SPAG_Loop_1_1: DO i = 1 , 14
         IF ( iblock(1,i)==1 ) EXIT SPAG_Loop_1_1
         IF ( harm==0.0 ) THEN
!
            DO ie = 1 , 5
               ke = 9*(ie-1)
               kepz = 6*(ie-1)
               block(2+ke,i) = estres(1+kepz)
               block(3+ke,i) = estres(2+kepz)
               block(4+ke,i) = estres(3+kepz)
               block(5+ke,i) = estres(4+kepz)
               block(6+ke,i) = estres(5+kepz)
               block(7+ke,i) = estres(6+kepz)
!
               IF ( .NOT.(lsys78) ) THEN
                  kepz2 = kepz/2
                  block(8+ke,i) = eflux(1+kepz2)
                  block(9+ke,i) = eflux(2+kepz2)
                  block(10+ke,i) = eflux(3+kepz2)
               ENDIF
            ENDDO
!
            DO ir = 1 , 4
               kr = 4*(ir-1)
               krpz = 3*(ir-1)
               block(47+kr,i) = eforc(1+krpz)
               block(48+kr,i) = eforc(2+krpz)
               block(49+kr,i) = eforc(3+krpz)
               kr3 = 1 + krpz/3
               IF ( .NOT.lsys78 ) block(50+kr,i) = echrg(kr3)
            ENDDO
         ELSE
            nphi = harm*block(1,i)*degrad
            sinphi = sin(nphi)
            conphi = cos(nphi)
            IF ( Sorc==1 ) THEN
!
               DO ie = 1 , 5
                  ke = 9*(ie-1)
                  kepz = 6*(ie-1)
                  block(2+ke,i) = block(2+ke,i) + sinphi*estres(1+kepz)
                  block(3+ke,i) = block(3+ke,i) + sinphi*estres(2+kepz)
                  block(4+ke,i) = block(4+ke,i) + sinphi*estres(3+kepz)
                  block(5+ke,i) = block(5+ke,i) + sinphi*estres(4+kepz)
                  block(6+ke,i) = block(6+ke,i) - conphi*estres(5+kepz)
                  block(7+ke,i) = block(7+ke,i) - conphi*estres(6+kepz)
!
                  IF ( .NOT.(lsys78) ) THEN
                     kepz2 = kepz/2
                     block(8+ke,i) = block(8+ke,i) + sinphi*eflux(1+kepz2)
                     block(9+ke,i) = block(9+ke,i) + sinphi*eflux(2+kepz2)
                     block(10+ke,i) = block(10+ke,i) - conphi*eflux(3+kepz2)
                  ENDIF
               ENDDO
!
               DO ir = 1 , 4
                  kr = 4*(ir-1)
                  krpz = 3*(ir-1)
                  block(47+kr,i) = block(47+kr,i) + sinphi*eforc(1+krpz)
                  block(48+kr,i) = block(48+kr,i) - conphi*eforc(2+krpz)
                  block(49+kr,i) = block(49+kr,i) + sinphi*eforc(3+krpz)
                  kr3 = 1 + krpz/3
                  IF ( .NOT.lsys78 ) block(50+kr,i) = block(50+kr,i) + sinphi*echrg(kr3)
               ENDDO
            ELSE
!
               DO ie = 1 , 5
                  ke = 9*(ie-1)
                  kepz = 6*(ie-1)
                  block(2+ke,i) = block(2+ke,i) + conphi*estres(1+kepz)
                  block(3+ke,i) = block(3+ke,i) + conphi*estres(2+kepz)
                  block(4+ke,i) = block(4+ke,i) + conphi*estres(3+kepz)
                  block(5+ke,i) = block(5+ke,i) + conphi*estres(4+kepz)
                  block(6+ke,i) = block(6+ke,i) + sinphi*estres(5+kepz)
                  block(7+ke,i) = block(7+ke,i) + sinphi*estres(6+kepz)
!
                  IF ( .NOT.(lsys78) ) THEN
                     kepz2 = kepz/2
                     block(8+ke,i) = block(8+ke,i) + conphi*eflux(1+kepz2)
                     block(9+ke,i) = block(9+ke,i) + conphi*eflux(2+kepz2)
                     block(10+ke,i) = block(10+ke,i) + sinphi*eflux(3+kepz2)
                  ENDIF
               ENDDO
!
               DO ir = 1 , 4
                  kr = 4*(ir-1)
                  krpz = 3*(ir-1)
                  block(47+kr,i) = block(47+kr,i) + conphi*eforc(1+krpz)
                  block(48+kr,i) = block(48+kr,i) + sinphi*eforc(2+krpz)
                  block(49+kr,i) = block(49+kr,i) + conphi*eforc(3+krpz)
                  kr3 = 1 + krpz/3
                  IF ( .NOT.lsys78 ) block(50+kr,i) = block(50+kr,i) + conphi*echrg(kr3)
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
      istres(1) = elemid
      istres(2) = nelhar
      DO i = 1 , ns
         j = j + 1
         stres(j) = estres(i)
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
      iforce(1) = elemid
      iforce(2) = nelhar
      DO i = 1 , numpt
         DO kk = 1 , ndof
            j = j + 1
            k = k + 1
            force(j) = eforc(k)
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
      IF ( ktype==1 .OR. (ktype==2 .AND. ipart==1) ) RETURN
   ENDIF
!
!     INSERT HARMONIC STRESSES AND FORCES INTO CLOCK
!
   SPAG_Loop_1_2: DO i = 1 , 14
      IF ( iclock(1,i)==1 ) EXIT SPAG_Loop_1_2
      IF ( harm==0.0 ) THEN
!
         DO ie = 1 , 5
            ke = 9*(ie-1)
            kepz = 6*(ie-1)
            clock(2+ke,i) = estres(1+kepz)
            clock(3+ke,i) = estres(2+kepz)
            clock(4+ke,i) = estres(3+kepz)
            clock(5+ke,i) = estres(4+kepz)
            clock(6+ke,i) = estres(5+kepz)
            clock(7+ke,i) = estres(6+kepz)
!
            IF ( .NOT.(lsys78) ) THEN
               kepz2 = kepz/2
               clock(8+ke,i) = eflux(1+kepz2)
               clock(9+ke,i) = eflux(2+kepz2)
               clock(10+ke,i) = eflux(3+kepz2)
            ENDIF
         ENDDO
!
         DO ir = 1 , 4
            kr = 4*(ir-1)
            krpz = 3*(ir-1)
            clock(47+kr,i) = eforc(1+krpz)
            clock(48+kr,i) = eforc(2+krpz)
            clock(49+kr,i) = eforc(3+krpz)
            kr3 = 1 + krpz/3
            IF ( .NOT.lsys78 ) clock(50+kr,i) = echrg(kr3)
         ENDDO
      ELSE
         nphi = harm*clock(1,i)*degrad
         sinphi = sin(nphi)
         conphi = cos(nphi)
         IF ( Sorc==1 ) THEN
!
            DO ie = 1 , 5
               ke = 9*(ie-1)
               kepz = 6*(ie-1)
               clock(2+ke,i) = clock(2+ke,i) + sinphi*estres(1+kepz)
               clock(3+ke,i) = clock(3+ke,i) + sinphi*estres(2+kepz)
               clock(4+ke,i) = clock(4+ke,i) + sinphi*estres(3+kepz)
               clock(5+ke,i) = clock(5+ke,i) + sinphi*estres(4+kepz)
               clock(6+ke,i) = clock(6+ke,i) - conphi*estres(5+kepz)
               clock(7+ke,i) = clock(7+ke,i) - conphi*estres(6+kepz)
!
               IF ( .NOT.(lsys78) ) THEN
                  kepz2 = kepz/2
                  clock(8+ke,i) = clock(8+ke,i) + sinphi*eflux(1+kepz2)
                  clock(9+ke,i) = clock(9+ke,i) + sinphi*eflux(2+kepz2)
                  clock(10+ke,i) = clock(10+ke,i) - conphi*eflux(3+kepz2)
               ENDIF
            ENDDO
!
            DO ir = 1 , 4
               kr = 4*(ir-1)
               krpz = 3*(ir-1)
               clock(47+kr,i) = clock(47+kr,i) + sinphi*eforc(1+krpz)
               clock(48+kr,i) = clock(48+kr,i) - conphi*eforc(2+krpz)
               clock(49+kr,i) = clock(49+kr,i) + sinphi*eforc(3+krpz)
               kr3 = 1 + krpz/3
               IF ( .NOT.lsys78 ) clock(50+kr,i) = clock(50+kr,i) + sinphi*echrg(kr3)
            ENDDO
         ELSE
!
            DO ie = 1 , 5
               ke = 9*(ie-1)
               kepz = 6*(ie-1)
               clock(2+ke,i) = clock(2+ke,i) + conphi*estres(1+kepz)
               clock(3+ke,i) = clock(3+ke,i) + conphi*estres(2+kepz)
               clock(4+ke,i) = clock(4+ke,i) + conphi*estres(3+kepz)
               clock(5+ke,i) = clock(5+ke,i) + conphi*estres(4+kepz)
               clock(6+ke,i) = clock(6+ke,i) + sinphi*estres(5+kepz)
               clock(7+ke,i) = clock(7+ke,i) + sinphi*estres(6+kepz)
!
               IF ( .NOT.(lsys78) ) THEN
                  kepz2 = kepz/2
                  clock(8+ke,i) = clock(8+ke,i) + conphi*eflux(1+kepz2)
                  clock(9+ke,i) = clock(9+ke,i) + conphi*eflux(2+kepz2)
                  clock(10+ke,i) = clock(10+ke,i) + sinphi*eflux(3+kepz2)
               ENDIF
            ENDDO
!
            DO ir = 1 , 4
               kr = 4*(ir-1)
               krpz = 3*(ir-1)
               clock(47+kr,i) = clock(47+kr,i) + conphi*eforc(1+krpz)
               clock(48+kr,i) = clock(48+kr,i) + sinphi*eforc(2+krpz)
               clock(49+kr,i) = clock(49+kr,i) + conphi*eforc(3+krpz)
               kr3 = 1 + krpz/3
               IF ( .NOT.lsys78 ) clock(50+kr,i) = clock(50+kr,i) + conphi*echrg(kr3)
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
   istres(1) = elemid
   istres(2) = nelhar
   DO i = 1 , ns
      j = j + 1
      stres(j) = estres(i)
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
   iforce(1) = elemid
   iforce(2) = nelhar
   DO i = 1 , numpt
      DO kk = 1 , ndof
         j = j + 1
         k = k + 1
         force(j) = eforc(k)
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
