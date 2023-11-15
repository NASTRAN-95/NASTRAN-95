
SUBROUTINE ifs1p(*,*,*)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Abort , Baddat , Badfor , Iax , Lharm , Slot
   INTEGER B1 , Bardf2 , Bardf5 , Bardf6 , Bardf7 , Bardf8 , E(40) , I(100) , Iaxf , Id , Idrdl , K , Klotdf(5) , Km , Kn , Knt ,   &
         & Ksystm(80) , Kx , Ky , Ll(6) , M(100) , M1(100) , M1f(100) , Mf(100) , N , Nax , Naxf , Nopen , Nout , Nparam
   REAL Gc(7) , Rm(100) , Slotdf(5)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / E
   COMMON /cifs1p/ B1 , Bardf2 , Bardf5 , Bardf6 , Bardf7 , Bardf8 , Km , Slot , Idrdl
   COMMON /ifpdta/ Id , N , K , Kx , Ky , I , Rm , Mf , M1 , M1f , Kn , Baddat , Badfor , Nopen , Nparam , Iax , Nax , Iaxf , Naxf ,&
                 & Lharm , Knt , Slotdf , Gc , Ll
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
!
! Local variable declarations
!
   INTEGER bcdc , bcdr , bcds , blk , hbdyix(7) , hbdynm(2,7) , icell , it1 , it2 , it3 , iz , k914 , kl , kz , l , l1 , l2 , l3 ,  &
         & l4 , l50 , nmo , thru
   LOGICAL ifpdco
!
! End of declarations
!
!
   EQUIVALENCE (Ksystm(2),Nout) , (Ksystm(3),Abort) , (M(1),Rm(1)) , (Slotdf(1),Klotdf(1))
   DATA hbdynm/4HPOIN , 4HT    , 4HLINE , 4H     , 4HREV  , 4H     , 4HAREA , 4H3    , 4HAREA , 4H4    , 4HELCY , 4HL    , 4HFTUB , &
       &4HE   /
   DATA hbdyix/1 , 2 , 2 , 3 , 4 , 2 , 2/
   DATA thru/4HTHRU/
   DATA blk , bcdc , bcdr , bcds/1H  , 1HC , 1HR , 1HS/
   DATA it1 , it2 , it3/2HT1 , 2HT2 , 2HT3/
!
   IF ( K<=100 ) THEN
      IF ( K==1 .OR. K==2 .OR. K==3 .OR. K==12 .OR. K==13 .OR. K==17 .OR. K==28 .OR. K==32 .OR. K==51 .OR. K==79 .OR. K==80 .OR.    &
         & K==82 .OR. K==83 .OR. K==84 .OR. K==85 .OR. K==86 .OR. K==87 .OR. K==88 .OR. K==89 .OR. K==90 .OR. K==91 .OR. K==92 .OR. &
         & K==93 .OR. K==94 .OR. K==95 .OR. K==96 .OR. K==97 .OR. K==98 .OR. K==99 .OR. K==100 ) GOTO 100
      IF ( K==4 ) GOTO 700
      IF ( K==5 ) THEN
!
!*****         5-CORD1R        *****************************************
!
         l50 = 1
         GOTO 900
      ELSEIF ( K==6 ) THEN
!
!*****         6-CORD1C        *****************************************
!
         l50 = 2
         GOTO 900
      ELSEIF ( K==7 ) THEN
!
!*****         7-CORD1S        *****************************************
!
         l50 = 3
         GOTO 900
      ELSEIF ( K==8 ) THEN
!
!*****         8-CORD2R        *****************************************
!
         I(2) = 1
         GOTO 1000
      ELSEIF ( K==9 ) THEN
!
!*****         9-CORD2C        *****************************************
!
         I(2) = 2
         GOTO 1000
      ELSEIF ( K==10 ) THEN
!
!*****         10-CORD2S       *****************************************
!
         I(2) = 3
         GOTO 1000
      ELSEIF ( K==11 ) THEN
         GOTO 1100
      ELSEIF ( K==14 .OR. K==15 ) THEN
!
!*****         14-SUPORT,15-OMIT,215-ASET         **********************
!
         l = 1
         GOTO 1700
      ELSEIF ( K==16 ) THEN
         GOTO 2000
      ELSEIF ( K==18 .OR. K==19 ) THEN
!
!*******       18-FORCE,19-MOMENT   **************************
!
         IF ( M(2)>0 ) GOTO 2200
         GOTO 300
      ELSEIF ( K==20 .OR. K==21 ) THEN
!
!*****         20-FORCE1,21-MOMENT1   **********************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(4)<=0 .OR. M(5)<=0 ) GOTO 300
         IF ( M(4)==M(5) ) GOTO 300
         N = 5
         GOTO 400
      ELSEIF ( K==22 .OR. K==23 ) THEN
!
!*****         22-FORCE2,23-MOMENT2   **********************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(4)<=0 ) GOTO 300
         IF ( M(5)<=0 .OR. M(6)<=0 .OR. M(7)<=0 ) GOTO 300
         IF ( M(4)==M(5) .OR. M(6)==M(7) .OR. M(4)==M(6) .AND. M(5)==M(7) .OR. M(4)==M(7) .AND. M(5)==M(6) ) GOTO 300
         N = 7
         GOTO 400
      ELSEIF ( K==24 ) THEN
!
!*****         24-PLOAD        *****************************************
!
         IF ( M(1)<=0 .OR. M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 ) GOTO 300
         IF ( M(6)<0 .OR. M(6)==0 .AND. Mf(6)/=0 ) GOTO 300
         DO l = 4 , 6
            DO l1 = l , 6
               IF ( M(l-1)==M(l1) ) GOTO 300
            ENDDO
         ENDDO
         N = 6
         GOTO 400
      ELSEIF ( K==25 .OR. K==27 .OR. K==81 ) THEN
!
!*****         25-SLOAD,27-TEMP,81-DEFORM    ***************************
!
         IF ( M(1)<=0 ) GOTO 300
         DO l = 2 , 6 , 2
            IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
               IF ( M(l)<=0 ) GOTO 300
               N = N + 3
               I(N-2) = M(1)
               I(N-1) = M(l)
               I(N) = M(l+1)
               IF ( N>3 ) THEN
                  DO l1 = 6 , N , 3
                     IF ( I(N-1)==I(l1-4) ) GOTO 300
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
         IF ( N>0 ) GOTO 500
         GOTO 300
      ELSEIF ( K==26 ) THEN
!
!*****         26-GRAV         *****************************************
!
         IF ( M(1)<=0 .OR. M(2)<0 ) GOTO 300
         IF ( M(4)/=0 .OR. M(5)/=0 .OR. M(6)/=0 ) THEN
            N = 6
            GOTO 400
         ELSE
            IF ( M(3)/=0 ) GOTO 300
            Rm(4) = 1.0
            N = 6
            GOTO 400
         ENDIF
      ELSEIF ( K==29 ) THEN
!
!*****         29-PROD         *****************************************
!
         N = 6
         GOTO 2300
      ELSEIF ( K==30 ) THEN
!
!*****         30-PTUBE        *****************************************
!
         N = 5
         IF ( Rm(3)<=0.0 .OR. Rm(4)<0.0 .OR. Rm(4)>0.5*Rm(3) ) GOTO 300
         IF ( Rm(4)==0.0 ) Rm(4) = 0.5*Rm(3)
         GOTO 2300
      ELSEIF ( K==31 ) THEN
         kl = 33
!
!*****         31-PVISC        *****************************************
!
         DO l = 1 , 5 , 4
            IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
               IF ( M(l)<=0 ) GOTO 300
               N = N + 3
               I(N-2) = M(l)
               I(N-1) = M(l+1)
               I(N) = M(l+2)
               IF ( E(kl)>=0 ) THEN
                  IF ( M(l)>E(kl) ) THEN
                     E(kl) = M(l)
                  ELSE
                     E(kl) = -M(l)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( N>0 ) GOTO 500
         GOTO 300
      ELSEIF ( K==33 .OR. K==38 ) THEN
!
!*****         33-PTRIA1,38-PQUAD1    **********************************
!
         IF ( M(2)<0 .OR. M(4)<0 .OR. M(6)<0 ) GOTO 300
         IF ( M(2)==0 .AND. M(4)==0 .AND. M(6)==0 ) GOTO 300
         DO l = 2 , 6 , 2
            IF ( M(l)==0 .AND. M(l+1)/=0 ) GOTO 300
         ENDDO
         N = 10
         GOTO 2400
      ELSEIF ( K==34 ) THEN
         kl = 30
         GOTO 2500
      ELSEIF ( K==35 .OR. K==36 .OR. K==40 ) THEN
!
!*****         35-PTRBSC,36-PTRPLT,40-PQDPLT     ***********************
!
         IF ( M(2)<0 .OR. M(4)<0 .OR. M(2)==0 .AND. M(4)==0 ) GOTO 300
         DO l = 2 , 4 , 2
            IF ( M(l)==0 .AND. M(l+1)/=0 ) GOTO 300
         ENDDO
         N = 8
         GOTO 2400
      ELSEIF ( K==37 ) THEN
         kl = 31
         GOTO 2500
      ELSEIF ( K==39 ) THEN
         kl = 27
         GOTO 2500
      ELSEIF ( K==41 ) THEN
         kl = 24
         GOTO 2500
      ELSEIF ( K==42 ) THEN
         kl = 28
         GOTO 2500
      ELSEIF ( K==43 ) THEN
         kl = 32
         GOTO 2500
      ELSEIF ( K==44 ) THEN
         kl = 23
         GOTO 2600
      ELSEIF ( K==45 ) THEN
         kl = 21
         GOTO 2600
      ELSEIF ( K==46 ) THEN
         kl = 22
!
!*****         46-PELAS             ************************************
!
         DO l = 1 , 5 , 4
            IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
               IF ( M(l)<=0 ) GOTO 300
               N = N + 4
               I(N-3) = M(l)
               I(N-2) = M(l+1)
               I(N-1) = M(l+2)
               I(N) = M(l+3)
               IF ( E(kl)>=0 ) THEN
                  IF ( M(l)>E(kl) ) THEN
                     E(kl) = M(l)
                  ELSE
                     E(kl) = -M(l)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( N>0 ) GOTO 500
         GOTO 300
      ELSEIF ( K==47 ) THEN
!
!*****         47-CONROD       *****************************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<=0 .OR. M(4)<=0 ) GOTO 300
         IF ( M(2)==M(3) ) GOTO 300
         N = 8
         GOTO 400
      ELSEIF ( K==48 ) THEN
         kl = 1
         GOTO 2700
      ELSEIF ( K==49 ) THEN
         kl = 2
         GOTO 2700
      ELSEIF ( K==50 ) THEN
         kl = 3
         GOTO 2700
      ELSEIF ( K==52 .OR. K==53 .OR. K==54 .OR. K==55 .OR. K==56 ) THEN
         GOTO 2800
      ELSEIF ( K==57 .OR. K==58 .OR. K==59 .OR. K==60 ) THEN
         GOTO 2900
      ELSEIF ( K==61 .OR. K==62 ) THEN
!
!*****         61-CSHEAR,62-CTWIST    **********************************
!
         IF ( M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 .OR. M(6)<=0 ) GOTO 300
         IF ( M(3)==M(4) .OR. M(4)==M(5) .OR. M(5)==M(6) .OR. M(3)==M(5) .OR. M(4)==M(6) .OR. M(3)==M(6) ) GOTO 300
         N = 6
         IF ( Mf(2)==0 ) M(2) = M(1)
         GOTO 2300
      ELSEIF ( K==63 ) THEN
!
!*****         63-CONM1        *****************************************
!
         IF ( M(1)<0 .OR. M(2)<=0 .OR. M(3)<0 ) GOTO 300
         N = 24
         GOTO 400
      ELSEIF ( K==64 ) THEN
!
!*****         64-CONM2        *****************************************
!
         IF ( M(1)<0 .OR. M(2)<=0 ) GOTO 300
         DO l = 1 , 7
            I(l) = M(l)
         ENDDO
         DO l = 8 , 13
            I(l) = M(l+1)
         ENDDO
         N = 13
         GOTO 500
      ELSEIF ( K==65 .OR. K==69 .OR. K==73 ) THEN
!
!*****         65-CMASS1,69-CDAMP1,73-CELAS1,70-CDAMP2,66-CMASS2    ****
!
         IF ( Mf(2)==0 ) M(2) = M(1)
         IF ( M(2)<=0 ) GOTO 300
         N = 6
         GOTO 3000
      ELSEIF ( K==66 .OR. K==70 ) THEN
         N = 6
         GOTO 3000
      ELSEIF ( K==67 ) THEN
         kl = 4
         GOTO 3100
      ELSEIF ( K==68 ) THEN
         kl = 7
         GOTO 3200
      ELSEIF ( K==71 ) THEN
         kl = 5
         GOTO 3100
      ELSEIF ( K==72 ) THEN
         kl = 8
         GOTO 3200
      ELSEIF ( K==74 ) THEN
!
!*****         74-CELAS2       *****************************************
!
         N = 8
         GOTO 3000
      ELSEIF ( K==75 ) THEN
         kl = 6
         GOTO 3100
      ELSEIF ( K==76 ) THEN
         kl = 9
         GOTO 3200
      ELSEIF ( K==77 ) THEN
!
!*****         77-MAT1         *****************************************
!
         IF ( M(1)<=0 .OR. (Rm(2)==0 .AND. Rm(3)==0) ) GOTO 300
         IF ( (Rm(2)<0. .OR. Rm(3)<0.) .AND. Ksystm(78)>=0 ) GOTO 300
         N = 12
         IF ( M(12)<0 ) GOTO 300
         l = 3
         IF ( Mf(2)==0 .OR. Rm(2)==0. ) l = l - 1
         IF ( Mf(3)==0 .OR. Rm(3)==0. ) l = l - 1
         IF ( Mf(4)==0 .OR. Rm(4)==0. ) l = l - 1
         IF ( l<2 ) THEN
            CALL page2(3)
            WRITE (Nout,99001) Uwm , M(1)
99001       FORMAT (A25,' 2251, TWO OF THE E, G AND NU ON MAT1 CARD ',I8,' ARE ZEROS OR BLANKS.',/5X,                               &
                   &'POTENTIAL ERROR MAY OCCUR LATER')
         ENDIF
         IF ( Mf(2)/=2 .OR. Mf(3)/=2 .OR. Mf(4)/=2 ) THEN
            IF ( Mf(2)==0 ) Rm(2) = 2.0*Rm(3)*(1.0+Rm(4))
            IF ( Mf(3)==0 ) Rm(3) = Rm(2)/(2.0*(1.0+Rm(4)))
            IF ( Mf(4)==0 ) Rm(4) = Rm(2)/(2.0*Rm(3)) - 1.0
            IF ( Rm(4)<-1.0 .OR. Rm(4)>0.5 ) THEN
               CALL page2(2)
               WRITE (Nout,99002) Uwm , M(1) , Rm(4)
99002          FORMAT (A25,' 2251, PHYSICALLY UNREALISTIC VALUE FOR NU ON MAT1 ','CARD ',I8,'.  VALUE = ',1P,E16.4)
            ENDIF
         ENDIF
         GOTO 400
      ELSEIF ( K==78 ) THEN
!
!*****         78-MAT2         *****************************************
!
         N = 17
         IF ( M(17)<0 ) GOTO 300
         IF ( M(1)>0 ) GOTO 400
         GOTO 300
      ENDIF
   ENDIF
   IF ( Kx<=100 ) THEN
      IF ( Kx==1 .OR. Kx==2 .OR. Kx==3 .OR. Kx==4 .OR. Kx==5 .OR. Kx==6 .OR. Kx==7 .OR. Kx==8 .OR. Kx==9 .OR. Kx==10 .OR.           &
         & Kx==11 .OR. Kx==12 .OR. Kx==13 .OR. Kx==14 .OR. Kx==15 .OR. Kx==16 .OR. Kx==17 .OR. Kx==18 .OR. Kx==19 .OR. Kx==20 .OR.  &
         & Kx==22 .OR. Kx==23 .OR. Kx==24 .OR. Kx==26 .OR. Kx==31 .OR. Kx==32 .OR. Kx==33 .OR. Kx==34 .OR. Kx==38 .OR. Kx==39 .OR.  &
         & Kx==40 .OR. Kx==41 .OR. Kx==43 .OR. Kx==44 .OR. Kx==45 .OR. Kx==46 .OR. Kx==47 .OR. Kx==48 .OR. Kx==49 .OR. Kx==50 .OR.  &
         & Kx==51 .OR. Kx==52 .OR. Kx==53 .OR. Kx==54 .OR. Kx==55 .OR. Kx==56 .OR. Kx==57 .OR. Kx==59 .OR. Kx==60 .OR. Kx==61 .OR.  &
         & Kx==62 .OR. Kx==63 .OR. Kx==64 .OR. Kx==65 .OR. Kx==67 .OR. Kx==68 .OR. Kx==69 .OR. Kx==70 .OR. Kx==71 .OR. Kx==72 .OR.  &
         & Kx==73 .OR. Kx==74 .OR. Kx==75 .OR. Kx==76 .OR. Kx==77 .OR. Kx==78 .OR. Kx==82 .OR. Kx==83 .OR. Kx==84 .OR. Kx==85 .OR.  &
         & Kx==86 .OR. Kx==87 .OR. Kx==88 .OR. Kx==89 .OR. Kx==91 .OR. Kx==92 .OR. Kx==93 .OR. Kx==94 .OR. Kx==95 .OR. Kx==96 .OR.  &
         & Kx==97 .OR. Kx==98 .OR. Kx==99 .OR. Kx==100 ) GOTO 100
      IF ( Kx==21 ) THEN
         kl = 29
         GOTO 2500
      ELSEIF ( Kx==25 ) THEN
         GOTO 1200
      ELSEIF ( Kx==27 ) THEN
         GOTO 1300
      ELSEIF ( Kx==28 ) THEN
!
!*****         128-NOLIN2        ***************************************
!
         IF ( M(8)<0 .OR. Mf(8)/=1 .AND. Mf(8)/=0 ) GOTO 300
         IF ( .NOT.((M(8)>6 .AND. M(8)<10) .OR. M(8)>16) ) GOTO 1500
         GOTO 300
      ELSEIF ( Kx==29 .OR. Kx==30 ) THEN
!
!*****         129-NOLIN3,130-NOLIN4        ****************************
!
         IF ( .NOT.(Mf(8)/=0 .OR. Mf(7)/=2 .AND. Mf(7)/=0) ) GOTO 1400
         GOTO 300
      ELSEIF ( Kx==35 ) THEN
         GOTO 700
      ELSEIF ( Kx==36 ) THEN
!
!*****         136-TF         ******************************************
!
         IF ( Km==0 ) THEN
            nmo = 5
            Id = M(1)
            IF ( Id<=0 .OR. M(2)<=0 .OR. M(3)<0 ) THEN
               Baddat = .TRUE.
               GOTO 1900
            ELSE
               IF ( Mf(1)/=1 .OR. Mf(2)/=1 .OR. Mf(3)>1 ) Badfor = .TRUE.
               IF ( (Mf(4)/=2 .AND. Mf(4)/=0) .OR. (Mf(5)/=2 .AND. Mf(5)/=0) .OR. (Mf(6)/=2 .AND. Mf(6)/=0) ) Badfor = .TRUE.
               N = 6
               GOTO 1600
            ENDIF
         ELSEIF ( M(1)<=0 .OR. M(2)<0 ) THEN
            Baddat = .TRUE.
            GOTO 1900
         ELSE
            IF ( Mf(1)/=1 .OR. Mf(2)>1 ) Badfor = .TRUE.
            IF ( (Mf(3)/=2 .AND. Mf(3)/=0) .OR. (Mf(4)/=2 .AND. Mf(4)/=0) .OR. (Mf(5)/=2 .AND. Mf(5)/=0) ) Badfor = .TRUE.
            N = 5
            GOTO 1600
         ENDIF
      ELSEIF ( Kx==37 ) THEN
!
!*****         137-TIC        ******************************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 .OR. M(3)>6 ) GOTO 300
         N = 5
         GOTO 400
      ELSEIF ( Kx==42 ) THEN
!
!*****         142-TSTEP        ****************************************
!
         IF ( Mf(5)/=0 .OR. Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 200
         IF ( Km/=0 ) THEN
            IF ( Mf(1)==0 ) GOTO 1800
            Baddat = .TRUE.
            GOTO 1900
         ELSE
            nmo = 3
            Id = M(1)
            IF ( Id<=0 .OR. Mf(1)/=1 ) THEN
               Baddat = .TRUE.
               GOTO 1900
            ELSE
               N = 1
               I(N) = M(1)
               GOTO 1800
            ENDIF
         ENDIF
      ELSEIF ( Kx==58 ) THEN
!
!*****         158-EIGP        *****************************************
!
         IF ( M(1)<=0 ) GOTO 300
         DO l = 2 , 5 , 3
            IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
               IF ( M(l+2)<=0 ) GOTO 300
               N = N + 4
               I(N-3) = M(1)
               I(N-2) = M(l)
               I(N-1) = M(l+1)
               I(N) = M(l+2)
            ENDIF
         ENDDO
         IF ( N>0 ) GOTO 500
         GOTO 300
      ELSEIF ( Kx==66 ) THEN
!
!***********       166-FREQ2      **************************************
!
         IF ( Rm(2)>0 ) GOTO 1200
         GOTO 300
      ELSEIF ( Kx==79 ) THEN
!
!*****         179-BAROR       *****************************************
!
         IF ( B1==0 ) GOTO 300
         B1 = 0
         IF ( M(2)==0 .AND. M(5)==0 .AND. M(6)==0 .AND. M(7)==0 .AND. M(8)==0 ) GOTO 300
         IF ( M(2)<0 .OR. M(8)<0 .OR. M(8)>2 ) GOTO 300
         IF ( Mf(8)/=0 ) GOTO 800
         IF ( Mf(5)==1 .AND. Mf(6)/=0 .AND. Mf(7)/=0 ) GOTO 300
         IF ( Mf(5)==1 .AND. Mf(6)==0 .AND. Mf(7)==0 ) M(8) = 2
         IF ( Mf(5)==2 .OR. Mf(6)==2 .OR. Mf(7)==2 ) M(8) = 1
         GOTO 800
      ELSEIF ( Kx==80 ) THEN
!
!*****         180-CBAR        *****************************************
!
         IF ( Mf(2)==0 ) THEN
            IF ( Bardf2==0 ) THEN
               M(2) = M(1)
            ELSE
               M(2) = Bardf2
            ENDIF
         ENDIF
         IF ( Mf(5)==0 ) M(5) = Bardf5
         IF ( Mf(8)==0 ) M(8) = Bardf8
         IF ( Mf(5)>=3 .OR. Mf(6)>=3 .OR. Mf(7)>=3 ) GOTO 300
         IF ( M(8)==0 .AND. (Mf(5)==2 .OR. Mf(6)==2 .OR. Mf(7)==2) ) M(8) = 1
         IF ( M(8)==0 .AND. Mf(5)==1 .AND. Mf(6)+Mf(7)==0 ) M(8) = 2
         IF ( M(8)<=0 .OR. M(8)>2 ) GOTO 300
         IF ( M(8)/=2 ) THEN
            IF ( Mf(6)==0 ) M(6) = Bardf6
            IF ( Mf(7)==0 ) M(7) = Bardf7
         ENDIF
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<=0 .OR. M(4)<=0 ) GOTO 300
         IF ( M(8)==1 .AND. (Mf(5)/=2 .AND. Mf(5)/=0 .OR. M(5)==0 .AND. M(6)==0 .AND. M(7)==0) ) GOTO 300
         IF ( (M(8)==2 .OR. M(8)==3) .AND. (Mf(5)/=1 .AND. Mf(5)/=0 .OR. M(5)<=0 .OR. M(6)/=0 .OR. M(7)/=0) ) GOTO 300
         IF ( ifpdco(M(9)) ) GOTO 300
         IF ( M(9)>65432 ) GOTO 300
         IF ( ifpdco(M(10)) ) GOTO 300
         IF ( M(10)>65432 ) GOTO 300
         IF ( M(3)==M(4) .OR. M(3)==M(5) .AND. M(8)==2 ) GOTO 300
         IF ( M(8)==2 .AND. M(4)==M(5) ) GOTO 300
         N = 16
         GOTO 400
      ELSEIF ( Kx==81 ) THEN
!
!*****         181-PBAR        *****************************************
!
         N = 19
         IF ( Rm(4)>=0. .AND. Rm(5)>=0. .AND. Rm(4)*Rm(5)>=Rm(19)**2 ) GOTO 2300
         GOTO 300
      ELSEIF ( Kx==90 ) THEN
         GOTO 2200
      ENDIF
   ENDIF
   IF ( Ky<=100 ) THEN
      IF ( Ky==1 .OR. Ky==2 .OR. Ky==3 .OR. Ky==4 .OR. Ky==5 .OR. Ky==6 .OR. Ky==7 .OR. Ky==8 .OR. Ky==9 .OR. Ky==10 .OR.           &
         & Ky==11 .OR. Ky==12 .OR. Ky==13 .OR. Ky==14 .OR. Ky==16 .OR. Ky==17 .OR. Ky==18 .OR. Ky==19 .OR. Ky==20 .OR. Ky==21 .OR.  &
         & Ky==22 .OR. Ky==39 .OR. Ky==42 .OR. Ky==44 .OR. Ky==45 .OR. Ky==46 .OR. Ky==47 .OR. Ky==48 .OR. Ky==51 .OR. Ky==52 .OR.  &
         & Ky==53 .OR. Ky==54 .OR. Ky==55 .OR. Ky==61 .OR. Ky==62 .OR. Ky==63 .OR. Ky==64 .OR. Ky==65 .OR. Ky==66 .OR. Ky==67 .OR.  &
         & Ky==69 .OR. Ky==70 .OR. Ky==71 .OR. Ky==72 .OR. Ky==73 .OR. Ky==74 .OR. Ky==75 .OR. Ky==76 .OR. Ky==77 .OR. Ky==78 .OR.  &
         & Ky==79 .OR. Ky==80 .OR. Ky==81 .OR. Ky==82 .OR. Ky==83 .OR. Ky==84 .OR. Ky==85 .OR. Ky==86 .OR. Ky==87 .OR. Ky==88 .OR.  &
         & Ky==90 .OR. Ky==95 .OR. Ky==96 .OR. Ky==97 .OR. Ky==98 ) GOTO 100
      IF ( Ky==15 ) THEN
         l = 1
         GOTO 1700
      ELSEIF ( Ky==23 ) THEN
!
!*****         223-AXSLOT         **************************************
!
         IF ( Slot ) GOTO 300
         Slot = .TRUE.
         Iaxf = Iaxf + 2
         Slotdf(1) = Rm(1)
         Slotdf(2) = Rm(2)
         IF ( M(3)<0 ) Baddat = .TRUE.
         Klotdf(3) = M(3)
         Slotdf(4) = Rm(4)
         IF ( M(5)<0 ) Baddat = .TRUE.
         Klotdf(5) = M(5)
         N = 5
         GOTO 400
      ELSEIF ( Ky==24 ) THEN
!
!*****         224-CAXIF2         **************************************
!
         IF ( Mf(4)/=0 .OR. Mf(5)/=0 ) GOTO 200
         N = 3
         GOTO 3400
      ELSEIF ( Ky==25 ) THEN
!
!*****         225-CAXIF3         **************************************
!
         IF ( Mf(5)/=0 ) GOTO 200
         N = 4
         GOTO 3400
      ELSEIF ( Ky==26 ) THEN
!
!*****         226-CAXIF4         **************************************
!
         N = 5
         GOTO 3400
      ELSEIF ( Ky==27 ) THEN
!
!*****         227-CSLOT3         **************************************
!
         IF ( Mf(5)/=0 ) GOTO 200
         N = 4
         GOTO 3500
      ELSEIF ( Ky==28 ) THEN
!
!*****         228-CSLOT4         **************************************
!
         N = 5
         GOTO 3500
      ELSEIF ( Ky==29 ) THEN
!
!*****         229-GRIDF          **************************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( Rm(2)<=0.0 ) GOTO 300
         N = 3
         GOTO 400
      ELSEIF ( Ky==30 ) THEN
!
!*****         230-GRIDS          **************************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( M(5)<0 ) GOTO 300
         IF ( Mf(4)==0 ) Rm(4) = Slotdf(4)
         N = 5
         GOTO 400
      ELSEIF ( Ky==31 ) THEN
!
!*****         231-SLBDY          **************************************
!
         IF ( Km/=0 ) THEN
            iz = 1
         ELSE
            Km = 1
            IF ( Mf(1)/=2 .AND. Mf(1)/=0 ) Badfor = .TRUE.
            IF ( Mf(1)==0 ) M(1) = Klotdf(1)
            IF ( Mf(2)/=1 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            IF ( Mf(2)==0 ) M(2) = Klotdf(5)
            IF ( M(2)<0 ) Baddat = .TRUE.
            I(1) = M(1)
            I(2) = M(2)
            N = 2
            iz = 3
         ENDIF
         DO l = iz , 8
            IF ( Mf(l)==0 ) GOTO 3700
            IF ( M(l)<=0 ) Baddat = .TRUE.
            N = N + 1
            I(N) = M(l)
         ENDDO
         GOTO 3600
      ELSEIF ( Ky==32 ) THEN
!
!*****         232-CHBDY           *************************************
!
         IF ( M(1)>0 ) THEN
            I(1) = M(1)
            IF ( M(2)>=0 ) THEN
               I(2) = M(2)
               DO l = 1 , 7
                  IF ( M(3)==hbdynm(1,l) .AND. M(4)==hbdynm(2,l) ) GOTO 3800
               ENDDO
            ENDIF
         ENDIF
         GOTO 300
      ELSEIF ( Ky==33 ) THEN
!
!*****         233-QHBDY           *************************************
!
         IF ( M(1)>0 ) THEN
            I(1) = M(1)
            DO l = 1 , 5
               IF ( M(2)==hbdynm(1,l) .AND. M(3)==hbdynm(2,l) ) GOTO 3900
            ENDDO
         ENDIF
         GOTO 300
      ELSEIF ( Ky==34 ) THEN
         GOTO 3300
      ELSEIF ( Ky==35 ) THEN
!
!*****         235-MAT5            *************************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( Rm(8)<=0.0 .AND. Mf(8)==2 ) GOTO 300
         N = 8
         GOTO 400
      ELSEIF ( Ky==36 ) THEN
!
!*****         236-PHBDY           *************************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( M(2)<0 ) GOTO 300
         IF ( Rm(3)<0.0 ) GOTO 300
         IF ( Rm(4)<0.0 .OR. Rm(4)>1.0 ) GOTO 300
         IF ( Rm(5)<0.0 .OR. Rm(5)>1.0 ) GOTO 300
         IF ( Mf(5)==0 ) Rm(5) = Rm(4)
         N = 7
         GOTO 400
      ELSEIF ( Ky==37 ) THEN
!
!*****         237-MATT4           *************************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( M(2)<0 ) GOTO 300
         N = 2
         GOTO 400
      ELSEIF ( Ky==38 ) THEN
!
!*****         238-MATT5           *************************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( Mf(8)/=0 ) GOTO 200
         N = 7
         GOTO 400
      ELSEIF ( Ky==40 ) THEN
!
!*****         240-QBDY2           *************************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( M(2)<=0 ) GOTO 300
         N = 6
         GOTO 400
      ELSEIF ( Ky==41 ) THEN
!
!*****         241-QVECT           *************************************
!
         IF ( Km/=0 ) THEN
            l = 1
         ELSE
            IF ( M(1)<=0 ) Baddat = .TRUE.
            IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            I(1) = M(1)
            I(2) = M(2)
            DO l = 3 , 6
               IF ( Mf(l)==1 ) THEN
                  IF ( M(l)<0 ) Baddat = .TRUE.
                  I(l) = M(l)
               ELSE
                  IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) Badfor = .TRUE.
                  I(l) = M(l)
               ENDIF
            ENDDO
            l = 6
            k914 = 209
         ENDIF
         Km = 1
         Kn = 1
         N = 6
         l4 = l
         IF ( Mf(l)/=1 ) Badfor = .TRUE.
         IF ( M(l4)<=0 ) Baddat = .TRUE.
         DO WHILE ( l/=8 )
            IF ( Mf(l)==3 ) THEN
               IF ( Mf(l+1)==1 .AND. M(l4)==thru ) THEN
                  IF ( M(l4-1)<M(l4+2) ) THEN
                     l1 = M(l4-1) + 1
                     l2 = M(l4+2) - 1
                     IF ( l2>l1 ) THEN
                        DO
                           l3 = l1
                           I(N) = l3
                           CALL write(k914,I,N,0)
                           l1 = l1 + 1
                           IF ( l1>l2 ) EXIT
                        ENDDO
                     ENDIF
                     l = l + 1
                     l4 = l4 + 2
                     CYCLE
                  ENDIF
               ENDIF
               Baddat = .TRUE.
               l = l + 1
               l4 = l4 + 2
            ELSEIF ( Mf(l+1)==0 ) THEN
               IF ( M1(1)==0 .AND. M1(2)==0 ) Badfor = .TRUE.
               EXIT
            ELSE
               IF ( M(l4)<=0 ) Baddat = .TRUE.
               I(N) = M(l4)
               l = l + 1
               l4 = l4 + 1
               CALL write(k914,I,N,0)
            ENDIF
         ENDDO
         GOTO 4000
      ELSEIF ( Ky==43 ) THEN
!
!*****         243-RADLST          *************************************
!
         IF ( Km==0 ) THEN
            IF ( Idrdl==1 ) Badfor = .TRUE.
            Idrdl = 1
            k914 = 214
         ENDIF
         GOTO 4100
      ELSEIF ( Ky==49 .OR. Ky==59 ) THEN
         GOTO 2900
      ELSEIF ( Ky==50 ) THEN
         kl = 25
         GOTO 2500
      ELSEIF ( Ky==56 ) THEN
         GOTO 2000
      ELSEIF ( Ky==57 ) THEN
!
!*****         257-CYJOIN       ***************************************
!
         IF ( Km/=0 ) GOTO 4100
         IF ( M(1)/=1 .AND. M(1)/=2 ) Baddat = .TRUE.
         I(1) = M(1)
         IF ( Mf(2)==3 ) THEN
            IF ( M(2)/=bcdc .AND. M(2)/=bcdr .AND. M(2)/=bcds .AND. M(2)/=it1 .AND. M(2)/=it2 .AND. M(2)/=it3 ) Baddat = .TRUE.
            I(2) = M(2)
            l4 = 4
         ELSE
            IF ( Mf(2)/=0 ) Badfor = .TRUE.
            I(2) = blk
            l4 = 3
         ENDIF
         Km = 1
         I(3) = blk
         N = 3
         l = 3
         k914 = 210
         IF ( Mf(l)/=1 ) Badfor = .TRUE.
         IF ( M(l4)<=0 ) Baddat = .TRUE.
         GOTO 4300
      ELSEIF ( Ky==58 ) THEN
!
!*****         258-CNGRNT          *************************************
!
         IF ( Km==0 ) k914 = 208
         GOTO 4100
      ELSEIF ( Ky==60 ) THEN
         kl = 26
         GOTO 2500
      ELSEIF ( Ky==68 ) THEN
!
!*****         268-SET1       ******************************************
!
         IF ( Km/=0 ) GOTO 4100
         IF ( Mf(1)/=1 ) Badfor = .TRUE.
         I(1) = M(1)
         N = 1
         l = 2
         k914 = 204
         GOTO 4200
      ELSEIF ( Ky==89 ) THEN
!
!*****                  289-VIEW                 ***************
!
         N = 6
         IF ( M(1)<=0 ) GOTO 300
         GOTO 400
      ELSEIF ( Ky==91 ) THEN
         GOTO 4700
      ELSEIF ( Ky==92 ) THEN
!
!*****       292-PTRIM6         ****************************************
!
         IF ( M(2)<0 .OR. Rm(3)<0.0 .OR. Rm(4)<0.0 .OR. Rm(5)<0.0 ) GOTO 300
         IF ( Rm(3)==0.0 ) GOTO 300
         IF ( Mf(1)/=1 .AND. Mf(2)/=1 ) GOTO 200
         IF ( Mf(3)/=2 ) GOTO 200
         DO l = 4 , 6
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) GOTO 200
         ENDDO
         N = 6
!
!*****       293-CTRPLT1        ****************************************
!
         GOTO 2400
      ELSEIF ( Ky==93 ) THEN
         GOTO 4700
      ELSEIF ( Ky==94 ) THEN
!
!*****       294-PTRPLT1        ****************************************
!
         IF ( M(2)<0 .OR. M(6)<0 .OR. M(2)==0 .AND. M(6)==0 ) GOTO 300
         IF ( M(2)==0 .AND. M(3)/=0 ) GOTO 300
         IF ( M(6)==0 .AND. M(7)/=0 ) GOTO 300
         IF ( Mf(1)/=1 .AND. Mf(2)/=1 ) GOTO 200
         IF ( Mf(6)/=0 .AND. Mf(6)/=1 ) GOTO 200
         IF ( Mf(3)/=2 ) GOTO 200
         IF ( Mf(4)/=0 .AND. Mf(4)/=2 ) GOTO 200
         IF ( Mf(5)/=0 .AND. Mf(5)/=2 ) GOTO 200
         DO l = 7 , 16
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) GOTO 200
         ENDDO
         N = 16
!
!*****       295-CTRSHL         ****************************************
!
         GOTO 2400
      ELSEIF ( Ky==99 ) THEN
         GOTO 4700
      ELSEIF ( Ky==100 ) THEN
!
!*****       296-PTRSHL         ****************************************
!
         IF ( M(2)<0 .OR. M(6)<0 .OR. M(10)<0 .OR. M(2)==0 .AND. M(6)==0 .AND. M(10)==0 ) GOTO 300
         IF ( M(2)==0 .AND. Rm(3)/=0.0 ) GOTO 300
         IF ( M(6)==0 .AND. Rm(7)/=0.0 ) GOTO 300
         IF ( M(10)==0 .AND. Rm(11)/=0.0 ) GOTO 300
         IF ( Rm(3)<0.0 .OR. Rm(4)<0.0 .OR. Rm(5)<0.0 ) GOTO 300
         IF ( Rm(7)<0.0 .OR. Rm(8)<0.0 .OR. Rm(9)<0.0 ) GOTO 300
         IF ( Rm(11)<0.0 .OR. Rm(12)<0.0 .OR. Rm(13)<0.0 ) GOTO 300
         IF ( Mf(10)/=0 .AND. Mf(10)/=1 ) GOTO 200
         IF ( Mf(1)/=1 ) GOTO 200
         IF ( Mf(2)/=0 .AND. Mf(2)/=1 ) GOTO 200
         IF ( Mf(6)/=0 .AND. Mf(6)/=1 ) GOTO 200
         DO l = 3 , 11 , 4
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) GOTO 200
            IF ( Mf(l+1)/=0 .AND. Mf(l+1)/=2 ) GOTO 200
            IF ( Mf(l+2)/=0 .AND. Mf(l+2)/=2 ) GOTO 200
         ENDDO
         N = 20
         GOTO 2400
      ENDIF
   ENDIF
   kz = Ky - 100
   IF ( kz<=59 ) THEN
      IF ( kz==15 .OR. kz==19 ) THEN
!
!*****  315-MATPZ1,   319-MAT6  ****************************************
!
         IF ( M(1)<=0 ) GOTO 300
         N = 15
         IF ( K==319 ) N = 31
         GOTO 400
      ELSEIF ( kz==16 ) THEN
!
!*****    316-MATPZ2        ********************************************
!
         IF ( M(1)<=0 ) GOTO 300
         N = 52
         GOTO 400
      ELSEIF ( kz==17 .OR. kz==20 ) THEN
!
!*****     317-MTTPZ1,   320-MATT6   ***********************************
!
         N = 15
         IF ( K==320 ) N = 31
         DO l = 1 , N
            IF ( M(l)<0 ) GOTO 300
         ENDDO
         IF ( M(1)/=0 ) GOTO 400
         GOTO 300
      ELSEIF ( kz==18 ) THEN
!
!*****   318-MTTPZ2    *************************************************
!
         DO l = 1 , 52
            IF ( M(l)<0 ) GOTO 300
         ENDDO
         IF ( M(1)==0 ) GOTO 300
         N = 52
         GOTO 400
      ELSEIF ( kz==31 ) THEN
         GOTO 1100
      ELSEIF ( kz==37 ) THEN
         GOTO 3300
      ELSEIF ( kz==40 ) THEN
!
!*****             340-NOLIN5         **********************************
!
         IF ( Km/=0 ) THEN
            N = 8
            nmo = nmo + 8
            DO l = 1 , 8
               I(l) = M(l)
               IF ( Mf(l)/=0 ) THEN
                  IF ( Mf(l)/=1 .OR. M(l)<=0 ) Baddat = .TRUE.
               ENDIF
            ENDDO
         ELSE
            Km = 1
            Kn = 1
            nmo = 8
            IF ( Mf(1)/=1 .OR. M(1)<=0 ) Baddat = .TRUE.
            IF ( Mf(2)/=2 .OR. Rm(2)<=0. ) Baddat = .TRUE.
            IF ( Mf(3)/=2 .OR. Rm(3)<=0. ) Baddat = .TRUE.
            IF ( Mf(4)/=2 .OR. Rm(4)<=0. ) Baddat = .TRUE.
            IF ( Mf(5)==1 .AND. M(5)<0 ) Baddat = .TRUE.
            IF ( Mf(6)==1 .AND. M(6)<0 ) Baddat = .TRUE.
            IF ( Mf(7)==1 .AND. M(7)<0 ) Baddat = .TRUE.
            IF ( Mf(8)==1 .AND. M(8)<0 ) Baddat = .TRUE.
            IF ( Mf(5)==2 .AND. Rm(5)<0. ) Baddat = .TRUE.
            IF ( Mf(6)==2 .AND. Rm(6)<0. ) Baddat = .TRUE.
            IF ( Mf(7)==2 .AND. Rm(7)<0. ) Baddat = .TRUE.
            IF ( Mf(8)==2 .AND. Rm(8)<0. ) Baddat = .TRUE.
            N = 8
            DO l = 1 , 8
               I(l) = M(l)
            ENDDO
         ENDIF
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Km = 0
            Kn = 0
            IF ( nmo/=16 ) THEN
               IF ( nmo>16 ) Baddat = .TRUE.
               DO l = 1 , 8
                  N = N + 1
                  I(N) = 0
               ENDDO
            ENDIF
         ENDIF
         GOTO 600
      ELSEIF ( kz==41 ) THEN
         GOTO 1300
      ELSEIF ( kz==42 ) THEN
!
!*********       342-CFTUBE    *****************************************
!
         N = 4
         IF ( M(1)<=0 .OR. M(3)<=0 .OR. M(4)<=0 ) GOTO 300
         IF ( Mf(2)==0 ) M(2) = M(1)
         IF ( M(2)>3 .AND. M(3)/=M(4) ) GOTO 400
         GOTO 300
      ELSEIF ( kz==43 ) THEN
!
!*********       343-PFTUBE    ****************************************
!
         N = 5
         IF ( M(1)<=0 ) GOTO 300
         IF ( Rm(2)<=0. .OR. Rm(3)<0. .OR. Rm(4)<=0. ) GOTO 300
         IF ( Rm(5)==0. ) Rm(5) = Rm(4)
         IF ( Rm(5)>=0. ) GOTO 400
         GOTO 300
      ELSEIF ( kz==44 ) THEN
!
!*********       344-NFTUBE    *****************************************
!
         N = 5
         IF ( Mf(2)/=1 .OR. M(1)<=0 ) GOTO 300
         IF ( Mf(2)/=1 .OR. M(2)<=0 ) GOTO 300
         IF ( Mf(3)/=1 .OR. M(3)<=0 ) GOTO 300
         IF ( M(2)==M(3) ) GOTO 300
         IF ( Mf(4)/=0 .AND. Mf(4)/=2 ) GOTO 300
         IF ( Mf(5)==1 .AND. M(5)<0 ) GOTO 300
         IF ( Mf(5)<=2 ) GOTO 400
         GOTO 200
      ELSEIF ( kz==56 ) THEN
         GOTO 2700
      ELSEIF ( kz==57 ) THEN
         GOTO 2800
      ELSEIF ( kz==58 ) THEN
         GOTO 2900
      ELSEIF ( kz==59 ) THEN
!
!*********     359-PPSE       ******************************************
!
         N = 5
         IF ( M(1)<=0 ) GOTO 300
         IF ( Rm(2)==0. ) GOTO 300
         Rm(3) = 0.0
         Rm(4) = 0.0
         Rm(5) = 0.0
         GOTO 400
      ENDIF
   ENDIF
 100  CALL page2(2)
   WRITE (Nout,99003) Sfm
99003 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS1P.')
   Abort = .TRUE.
   RETURN 1
 200  Badfor = .TRUE.
   RETURN 1
 300  Baddat = .TRUE.
   RETURN 1
 400  DO l = 1 , N
      I(l) = M(l)
   ENDDO
 500  RETURN
 600  RETURN 3
!
!*****         4-SEQGP,135-SEQEP    ************************************
!
 700  DO l = 1 , 7 , 2
      IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
         IF ( M(l)<=0 .OR. M(l+1)<=0 ) GOTO 300
         N = N + 2
         I(N-1) = M(l)
         I(N) = M(l+1)
         IF ( N>2 ) THEN
            DO l1 = 4 , N , 2
               IF ( I(N-1)==I(l1-3) .OR. I(N)==I(l1-2) ) GOTO 300
            ENDDO
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
 800  Bardf2 = M(2)
   Bardf5 = M(5)
   Bardf6 = M(6)
   Bardf7 = M(7)
   Bardf8 = M(8)
   RETURN 2
 900  DO l = 1 , 5 , 4
      IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
         IF ( M(l)<=0 .OR. M(l+1)<=0 .OR. M(l+2)<=0 .OR. M(l+3)<=0 ) GOTO 300
         IF ( M(l+1)==M(l+2) .OR. M(l+1)==M(l+3) .OR. M(l+3)==M(l+2) ) GOTO 300
         N = N + 6
         IF ( N>6 .AND. M(l)==M(l-4) ) GOTO 300
         I(N-5) = M(l)
         I(N-4) = l50
         I(N-3) = 1
         I(N-2) = M(l+1)
         I(N-1) = M(l+2)
         I(N) = M(l+3)
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
 1000 I(1) = M(1)
   IF ( M(1)<=0 .OR. M(2)<0 ) GOTO 300
   IF ( M(3)==M(6) .AND. M(4)==M(7) .AND. M(5)==M(8) ) GOTO 300
   IF ( M(3)==M(9) .AND. M(4)==M(10) .AND. M(5)==M(11) ) GOTO 300
   IF ( M(6)==M(9) .AND. M(7)==M(10) .AND. M(8)==M(11) ) GOTO 300
   I(3) = 2
   DO l = 2 , 11
      I(l+2) = M(l)
   ENDDO
   N = 13
   GOTO 500
 1100 kl = 10
!
!*****   11-PLOTEL,   331-CFFREE   *************************************
!
   DO l = 1 , 5 , 4
      IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
         IF ( M(l)<=0 .OR. M(l+1)<=0 .OR. M(l+2)<=0 ) GOTO 300
         IF ( M(l+1)==M(l+2) ) GOTO 300
         N = N + 3
         I(N-2) = M(l)
         I(N-1) = M(l+1)
         I(N) = M(l+2)
         IF ( E(kl)>=0 ) THEN
            IF ( M(l)>E(kl) ) THEN
               E(kl) = M(l)
            ELSE
               E(kl) = -M(l)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
!
!***********       125-FREQ1      **************************************
!
 1200 IF ( M(1)<=0 .OR. Rm(2)<0. .OR. Rm(3)<=0. .OR. M(4)<=0 ) GOTO 300
   N = 4
   GOTO 400
!
!*****         127-NOLIN1,341-NOLIN6    ********************************
!
 1300 IF ( Mf(8)==0 ) GOTO 1500
   GOTO 300
 1400 IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 .OR. M(5)<=0 .OR. M(6)<0 ) GOTO 300
   IF ( M(3)>6 ) GOTO 300
   IF ( (M(6)>6 .AND. M(6)<10) .OR. M(6)>16 ) GOTO 300
   N = 8
   GOTO 400
 1500 IF ( Mf(7)==1 .AND. M(7)>0 ) GOTO 1400
   GOTO 300
 1600 DO l = 1 , N
      I(l) = M(l)
   ENDDO
   GOTO 1900
 1700 IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
      IF ( M(l)<=0 ) GOTO 300
      IF ( ifpdco(M(l+1)) ) GOTO 300
      iz = 6
      IF ( M(l+1)==0 ) iz = 1
      DO l2 = 1 , iz
         IF ( iz==1 .OR. Ll(l2)/=0 ) THEN
            N = N + 2
            I(N-1) = M(l)
            I(N) = Ll(l2)
            IF ( N>2 ) THEN
               DO l1 = 4 , N , 2
                  IF ( I(N-1)==I(l1-3) .AND. I(N)==I(l1-2) ) GOTO 300
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   l = l + 2
   IF ( l<=7 ) GOTO 1700
   IF ( N>0 ) GOTO 500
   GOTO 300
 1800 IF ( Mf(2)/=1 .OR. Mf(4)/=1 .OR. Mf(3)/=2 ) THEN
      Baddat = .TRUE.
   ELSEIF ( M(4)<=0 .OR. Rm(3)<=0. .OR. M(2)<M(4) ) THEN
      Baddat = .TRUE.
   ELSE
      N = N + 3
      I(N-2) = M(2)
      I(N-1) = M(3)
      I(N) = M(4)
   ENDIF
 1900 IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Km = 1
      Kn = 1
   ELSE
      Km = 0
      Kn = 0
      IF ( nmo>0 ) THEN
         DO l = 1 , nmo
            N = N + 1
            I(N) = -1
         ENDDO
      ENDIF
   ENDIF
   GOTO 600
!
!*****         16-SPC , 256-SPCD ***********************************
!
 2000 IF ( M(1)<=0 ) GOTO 300
   l = 2
 2100 IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
      IF ( M(l)<=0 .OR. M(l+1)<0 ) GOTO 300
      IF ( ifpdco(M(l+1)) ) GOTO 300
      N = N + 4
      IF ( N>4 .AND. M(l)==M(l-3) .AND. M(l+1)==M(l-2) ) GOTO 300
      I(N-3) = M(1)
      I(N-2) = M(l)
      I(N-1) = M(l+1)
      I(N) = M(l+2)
   ENDIF
   l = l + 3
   IF ( l==5 ) GOTO 2100
   IF ( N>0 ) GOTO 500
   GOTO 300
!
!***************        190-RFORCE    *****************************
!
 2200 IF ( Mf(3)/=0 .AND. Mf(3)/=1 ) GOTO 300
   IF ( M(1)<=0 .OR. M(2)<0 .OR. M(3)<0 ) GOTO 300
   IF ( M(5)/=0 .OR. M(6)/=0 .OR. M(7)/=0 ) THEN
      N = 7
      GOTO 400
   ELSE
      IF ( M(4)/=0 ) GOTO 300
      Rm(5) = 1.0
      N = 7
      GOTO 400
   ENDIF
!WKBDB 2/95 SPR94015
!      IF (K .NE. 190) GO TO 3
!      IF (M(8) .EQ. 0) M(8) = 1
!      IF (M(8) .LT.0 .OR. M(8).GT.2) GO TO 8
!      N = 8
!WKBDE 2/95 SPR94015
 2300 IF ( M(2)<=0 ) GOTO 300
 2400 IF ( M(1)>0 ) GOTO 400
   GOTO 300
!
!*****      34-PTRIA2,37-PTRMEM,39-PQUAD2,41-PQDMEM  *******************
!*****      42-PSHEAR,43-PTWIST,121-PTORDRG,250-PQDMEM1    *************
!*****      260-PQDMEM2
!
 2500 DO l = 1 , 5 , 4
      IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
         IF ( M(l)<=0 .OR. M(l+1)<=0 ) GOTO 300
         IF ( Rm(l+2)<=0.0 ) GOTO 300
         N = N + 4
         I(N-3) = M(l)
         I(N-2) = M(l+1)
         I(N-1) = M(l+2)
         I(N) = M(l+3)
         IF ( E(kl)>=0 ) THEN
            IF ( M(l)>E(kl) ) THEN
               E(kl) = M(l)
            ELSE
               E(kl) = -M(l)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
!
!*****         44-PMASS,45-PDAMP    ***********************************
!
 2600 DO l = 1 , 7 , 2
      IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
         IF ( M(l)<=0 ) GOTO 300
         N = N + 2
         I(N-1) = M(l)
         I(N) = M(l+1)
         IF ( E(kl)>=0 ) THEN
            IF ( M(l)>E(kl) ) THEN
               E(kl) = M(l)
            ELSE
               E(kl) = -M(l)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
!
!*****         48-CROD,49-CTUBE,50-CVISC,356-CPSE2    *****************
!
 2700 DO l = 1 , 5 , 4
      IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
         IF ( M(l)<=0 .OR. M(l+2)<=0 .OR. M(l+3)<=0 ) GOTO 300
         IF ( Mf(l+1)==0 ) M(l+1) = M(l)
         IF ( M(l+1)<=0 .OR. M(l+2)==M(l+3) ) GOTO 300
         N = N + 4
         I(N-3) = M(l)
         I(N-2) = M(l+1)
         I(N-1) = M(l+2)
         I(N) = M(l+3)
         IF ( E(kl)>=0 ) THEN
            IF ( M(l)>E(kl) ) THEN
               E(kl) = M(l)
            ELSE
               E(kl) = -M(l)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
!
!*****       52-CTRIA1,53-CTRIA2,54-CTRBSC,55-CTRPLT,56-CTRMEM    ****
!            357-CPSE3
!
 2800 IF ( M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 ) GOTO 300
   IF ( M(3)==M(4) .OR. M(4)==M(5) .OR. M(3)==M(5) ) GOTO 300
   N = 6
   IF ( K==357 ) N = 5
   IF ( Mf(2)==0 ) M(2) = M(1)
   GOTO 2300
!
!*****       57-CQUAD1,58-CQUAD2,59-CQDPLT,60-CQDMEM,249-CQDMEM1    ****
!*****       259-CQDMEM2,358-CPSE4
!
 2900 IF ( M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 .OR. M(6)<=0 ) GOTO 300
   IF ( M(3)==M(4) .OR. M(4)==M(5) .OR. M(5)==M(6) .OR. M(3)==M(5) .OR. M(4)==M(6) .OR. M(3)==M(6) ) GOTO 300
   N = 7
   IF ( K==358 ) N = 6
   IF ( Mf(2)==0 ) M(2) = M(1)
   GOTO 2300
 3000 IF ( M(1)<=0 ) GOTO 300
   IF ( M(3)<0 .OR. M(4)<0 .OR. M(5)<0 .OR. M(6)<0 ) GOTO 300
   IF ( M(4)>6 .OR. M(6)>6 .OR. M(3)==0 .AND. M(5)==0 ) GOTO 300
   IF ( M(3)==0 .AND. M(4)/=0 .OR. M(5)==0 .AND. M(6)/=0 ) GOTO 300
   IF ( M(3)==M(5) .AND. M(4)==M(6) ) GOTO 300
   icell = M(4)
   M(4) = M(5)
   M(5) = icell
   GOTO 400
!
!*****         67-CMASS3,75-CELAS3,71-CDAMP3    ************************
!
 3100 DO l = 1 , 5 , 4
      IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
         IF ( M(l)<=0 .OR. M(l+2)<0 .OR. M(l+3)<0 ) GOTO 300
         IF ( Mf(l+1)==0 ) M(l+1) = M(l)
         IF ( M(l+1)<=0 .OR. M(l+2)==M(l+3) ) GOTO 300
         N = N + 4
         I(N-3) = M(l)
         I(N-2) = M(l+1)
         I(N-1) = M(l+2)
         I(N) = M(l+3)
         IF ( E(kl)>=0 ) THEN
            IF ( M(l)>E(kl) ) THEN
               E(kl) = M(l)
            ELSE
               E(kl) = -M(l)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
!
!*****         68-CMASS4,76-CELAS4,72-CDAMP4    ************************
!
 3200 DO l = 1 , 5 , 4
      IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
         IF ( M(l)<=0 .OR. M(l+2)<0 .OR. M(l+3)<0 ) GOTO 300
         IF ( M(l+2)==M(l+3) ) GOTO 300
         N = N + 4
         I(N-3) = M(l)
         I(N-2) = M(l+1)
         I(N-1) = M(l+2)
         I(N) = M(l+3)
         IF ( E(kl)>=0 ) THEN
            IF ( M(l)>E(kl) ) THEN
               E(kl) = M(l)
            ELSE
               E(kl) = -M(l)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
!
!*****     234-MAT4,   337-MATF   **************************************
!
 3300 IF ( M(1)<=0 ) GOTO 300
   IF ( Rm(2)<=0.0 ) GOTO 300
   IF ( Rm(3)<=0.0 .AND. Mf(3)==2 ) GOTO 300
   N = 3
   GOTO 400
 3400 IF ( M(1)<=0 ) GOTO 300
   IF ( Mf(6)==0 ) Rm(6) = Slotdf(1)
   IF ( Mf(7)==0 ) Rm(7) = Slotdf(2)
   IF ( Mf(8)==0 ) M(8) = Klotdf(3)
   DO l = 2 , N
      IF ( M(l)<=0 ) GOTO 300
      IF ( l/=2 ) THEN
         DO l1 = 3 , l
            IF ( M(l1-1)==M(l) ) GOTO 300
         ENDDO
      ENDIF
   ENDDO
!     CHECK FOR RHO .GE. 0.0
!     CHECK FOR B .GE. 0.0
!     CHECK FOR N .GE. 0
   DO l = 6 , 8
      l1 = l + N - 5
      I(l1) = M(l)
   ENDDO
   DO l = 1 , N
      I(l) = M(l)
   ENDDO
   N = N + 3
   GOTO 500
 3500 IF ( Mf(6)==0 ) Rm(6) = Slotdf(1)
   IF ( Mf(7)==0 ) Rm(7) = Slotdf(2)
   IF ( Mf(8)==0 ) M(8) = Klotdf(5)
!     CHECK FOR ALL KINDS OF THINGS
   DO l = 6 , 8
      l1 = l + N - 5
      I(l1) = M(l)
   ENDDO
   DO l = 1 , N
      I(l) = M(l)
   ENDDO
   N = N + 4
   I(N) = Klotdf(3)
   GOTO 500
 3600 IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      Km = 0
      N = N + 1
      I(N) = -1
      Kn = 0
   ENDIF
   GOTO 600
 3700 iz = l + 1
   DO l = iz , 8
      IF ( Mf(l)/=0 ) Badfor = .TRUE.
   ENDDO
   IF ( M1(1)==0 .AND. M1(2)==0 ) Badfor = .TRUE.
   GOTO 3600
 3800 I(3) = l
   l1 = hbdyix(l)
   DO l2 = 1 , l1
      IF ( M(l2+4)<=0 .OR. M(l2+9)<0 ) GOTO 300
      I(l2+3) = M(l2+4)
      I(l2+7) = M(l2+9)
   ENDDO
   IF ( l1/=4 ) THEN
      DO l2 = l1 , 3
         IF ( M(l2+5)/=0 .OR. M(l2+10)/=0 ) GOTO 300
         I(l2+4) = 0
         I(l2+8) = 0
      ENDDO
   ENDIF
   DO l2 = 12 , 14
      I(l2) = M(l2+2)
   ENDDO
   N = 15
   I(15) = M(9)
   GOTO 500
 3900 I(2) = l
   l1 = hbdyix(l)
   DO l2 = 1 , l1
      IF ( M(l2+5)<=0 ) GOTO 300
      I(l2+4) = M(l2+5)
   ENDDO
   IF ( l1/=4 ) THEN
      DO l2 = l1 , 3
         IF ( M(l2+6)/=0 ) GOTO 300
         I(l2+5) = 0
      ENDDO
   ENDIF
   I(3) = M(4)
   IF ( l>=3 .AND. Mf(4)/=0 ) GOTO 200
   IF ( l<3 .AND. Rm(5)<=0.0 ) GOTO 300
   I(4) = M(5)
   N = 8
   GOTO 500
 4000 IF ( Mf(l)/=1 ) Badfor = .TRUE.
   I(N) = M(l4)
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      Kn = 0
      Km = 0
   ENDIF
   GOTO 600
 4100 l = 1
   N = 0
 4200 Km = 1
   l4 = l
   IF ( Mf(l)/=1 ) Badfor = .TRUE.
   IF ( M(l4)<=0 ) Baddat = .TRUE.
 4300 DO WHILE ( l<=8 )
      IF ( Mf(l)==0 ) THEN
         DO l2 = l , 8
            IF ( Mf(l2)/=0 ) Badfor = .TRUE.
         ENDDO
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 4600
         Badfor = .TRUE.
         Kn = 1
         GOTO 600
      ELSEIF ( Mf(l)==3 ) THEN
         IF ( l==8 ) GOTO 4500
         IF ( Mf(l+1)/=1 .OR. M(l4)/=thru ) GOTO 4500
         IF ( M(l4-1)>=M(l4+2) ) GOTO 4500
         l1 = M(l4-1) + 1
         l2 = M(l4+2)
         GOTO 4400
      ELSE
         IF ( M(l4)<=0 ) Baddat = .TRUE.
         IF ( N>=49 ) THEN
            CALL write(k914,I,N,0)
            N = 0
         ENDIF
         N = N + 1
         I(N) = M(l4)
         l = l + 1
         l4 = l4 + 1
      ENDIF
   ENDDO
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 4600
   Kn = 1
   GOTO 600
 4400 DO
      l3 = l1
      IF ( N>=49 ) THEN
         CALL write(k914,I,N,0)
         N = 0
      ENDIF
      N = N + 1
      I(N) = l3
      l1 = l1 + 1
      IF ( l1>l2 ) THEN
         l = l + 2
         l4 = l4 + 3
         GOTO 4300
      ENDIF
   ENDDO
 4500 Baddat = .TRUE.
   l = l + 1
   l4 = l4 + 2
   GOTO 4300
 4600 Km = 0
   N = N + 1
   I(N) = -1
   Kn = 0
   GOTO 600
!
!*****       291-CTRIM6         ****************************************
!
 4700 IF ( M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 .OR. M(6)<=0 .OR. M(7)<=0 .OR. M(8)<=0 ) GOTO 300
   IF ( M(3)==M(4) .OR. M(3)==M(5) .OR. M(3)==M(6) .OR. M(3)==M(7) .OR. M(3)==M(8) .OR. M(4)==M(5) .OR. M(4)==M(6) .OR. M(4)==M(7)  &
      & .OR. M(4)==M(8) ) GOTO 300
   IF ( M(5)==M(6) .OR. M(5)==M(7) .OR. M(5)==M(8) .OR. M(6)==M(7) .OR. M(6)==M(8) .OR. M(7)==M(8) ) GOTO 300
   DO l = 1 , 8
      IF ( Mf(l)/=1 ) GOTO 200
   ENDDO
   IF ( Mf(9)/=0 .AND. Mf(9)/=2 ) GOTO 200
   IF ( Mf(2)==0 ) M(2) = M(1)
   N = 9
   GOTO 2300
!
END SUBROUTINE ifs1p
