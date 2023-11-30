
SUBROUTINE ifs4p(*,*,*)
   IMPLICIT NONE
   LOGICAL Abort , Baddat , Badfor , Fphys1 , Lflsym , Lharm
   REAL Gc(7) , Slotdf(5) , Z(100)
   INTEGER I(100) , Iax , Iaxf , Id , J(20) , K , Km , Kn , Knt , Kout , Kx , Ky , Ll(6) , M(100) , M1(100) , M1f(100) , Mach ,     &
         & Mf(100) , N , Nax , Naxf , Nbuf , Ncds , Nopen , Nout , Nparam , T1(2,310) , T4(2,314)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /cifs4p/ J , Km , Lflsym , Fphys1
   COMMON /ifpdta/ Id , N , K , Kx , Ky , I , M , Mf , M1 , M1f , Kn , Baddat , Badfor , Nopen , Nparam , Iax , Nax , Iaxf , Naxf , &
                 & Lharm , Knt , Slotdf , Gc , Ll
   COMMON /ifpx1 / Ncds , T1
   COMMON /ifpx3 / T4
   COMMON /machin/ Mach
   COMMON /system/ Nbuf , Nout , Abort
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   INTEGER bcda , bcdaxi , bcdno , bcdnon , bcds , bcdyes , i1 , iem , ifo , ii , in , iqvl , isid , ity1 , k2078 , kfl , kz , l ,  &
         & l1 , l2 , l3 , l4 , l5 , l6 , lll , lp1 , n1 , nm(2) , nn , npts , ntot , ret , save(24) , thru , ty1 , ty2
   REAL dc1 , dc2 , dc3 , dl1 , dl2 , dlc , dx1 , dx2 , dy1 , dy2 , dz1 , dz2 , zz
   LOGICAL ifpdco
!
   EQUIVALENCE (Z(1),M(1)) , (Kout,J(2))
   DATA thru , bcdyes , bcdno/4HTHRU , 4HYES  , 4HNO  /
   DATA bcds , bcda , bcdnon/4HS    , 4HA    , 4HNONE/
   DATA bcdaxi/4HAXIS/
!
   IF ( K<=100 ) THEN
      IF ( K==1 .OR. K==2 .OR. K==3 .OR. K==4 .OR. K==5 .OR. K==6 .OR. K==7 .OR. K==8 .OR. K==9 .OR. K==10 .OR. K==11 .OR.          &
         & K==12 .OR. K==13 .OR. K==14 .OR. K==15 .OR. K==16 .OR. K==17 .OR. K==18 .OR. K==19 .OR. K==20 .OR. K==21 .OR. K==22 .OR. &
         & K==23 .OR. K==24 .OR. K==25 .OR. K==26 .OR. K==27 .OR. K==28 .OR. K==29 .OR. K==30 .OR. K==31 .OR. K==32 .OR. K==33 .OR. &
         & K==34 .OR. K==35 .OR. K==36 .OR. K==37 .OR. K==38 .OR. K==39 .OR. K==40 .OR. K==41 .OR. K==42 .OR. K==43 .OR. K==44 .OR. &
         & K==45 .OR. K==46 .OR. K==47 .OR. K==48 .OR. K==49 .OR. K==50 .OR. K==51 .OR. K==52 .OR. K==53 .OR. K==54 .OR. K==55 .OR. &
         & K==56 .OR. K==57 .OR. K==58 .OR. K==59 .OR. K==60 .OR. K==61 .OR. K==62 .OR. K==63 .OR. K==64 .OR. K==65 .OR. K==66 .OR. &
         & K==67 .OR. K==68 .OR. K==69 .OR. K==70 .OR. K==71 .OR. K==72 .OR. K==73 .OR. K==74 .OR. K==75 .OR. K==76 .OR. K==77 .OR. &
         & K==78 .OR. K==81 .OR. K==82 .OR. K==83 .OR. K==84 .OR. K==85 .OR. K==86 .OR. K==87 .OR. K==88 .OR. K==89 .OR. K==92 .OR. &
         & K==93 .OR. K==94 .OR. K==95 .OR. K==96 .OR. K==97 .OR. K==99 .OR. K==100 ) GOTO 100
      IF ( K==79 ) THEN
!
!******              79-CTRIARG,80-CTRAPRG             ****************
!
         i1 = 4
         GOTO 700
      ELSEIF ( K==80 ) THEN
         i1 = 5
         GOTO 700
      ELSEIF ( K==90 .OR. K==91 ) THEN
!
!*******       MATS1,MATT1        **************************************
!
         DO l = 1 , 11
            IF ( M(l)<0 ) GOTO 300
            I(l) = M(l)
         ENDDO
         N = 11
         GOTO 500
      ELSEIF ( K==98 ) THEN
!
!*******       TEMPD              **************************************
!
         DO l = 1 , 7 , 2
            IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
               IF ( M(l)<=0 ) GOTO 300
               N = N + 2
               I(N-1) = M(l)
               I(N) = M(l+1)
               IF ( N>2 ) THEN
                  DO l1 = 4 , N , 2
                     IF ( I(N-1)==I(l1-3) ) GOTO 300
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
         IF ( N>0 ) GOTO 500
         GOTO 300
      ENDIF
   ENDIF
   IF ( Kx<=100 ) THEN
      IF ( Kx==1 .OR. Kx==3 .OR. Kx==6 .OR. Kx==7 .OR. Kx==8 .OR. Kx==9 .OR. Kx==10 .OR. Kx==11 .OR. Kx==12 .OR. Kx==13 .OR.        &
         & Kx==14 .OR. Kx==15 .OR. Kx==16 .OR. Kx==17 .OR. Kx==18 .OR. Kx==19 .OR. Kx==20 .OR. Kx==21 .OR. Kx==23 .OR. Kx==25 .OR.  &
         & Kx==26 .OR. Kx==27 .OR. Kx==28 .OR. Kx==29 .OR. Kx==30 .OR. Kx==31 .OR. Kx==32 .OR. Kx==33 .OR. Kx==34 .OR. Kx==35 .OR.  &
         & Kx==36 .OR. Kx==37 .OR. Kx==38 .OR. Kx==39 .OR. Kx==40 .OR. Kx==41 .OR. Kx==42 .OR. Kx==43 .OR. Kx==44 .OR. Kx==45 .OR.  &
         & Kx==46 .OR. Kx==47 .OR. Kx==48 .OR. Kx==49 .OR. Kx==50 .OR. Kx==51 .OR. Kx==52 .OR. Kx==53 .OR. Kx==54 .OR. Kx==55 .OR.  &
         & Kx==56 .OR. Kx==57 .OR. Kx==58 .OR. Kx==59 .OR. Kx==60 .OR. Kx==61 .OR. Kx==62 .OR. Kx==63 .OR. Kx==64 .OR. Kx==65 .OR.  &
         & Kx==66 .OR. Kx==67 .OR. Kx==68 .OR. Kx==69 .OR. Kx==70 .OR. Kx==71 .OR. Kx==72 .OR. Kx==73 .OR. Kx==74 .OR. Kx==75 .OR.  &
         & Kx==76 .OR. Kx==77 .OR. Kx==78 .OR. Kx==79 .OR. Kx==80 .OR. Kx==81 .OR. Kx==82 .OR. Kx==83 .OR. Kx==84 .OR. Kx==85 .OR.  &
         & Kx==86 .OR. Kx==87 .OR. Kx==88 .OR. Kx==90 .OR. Kx==91 .OR. Kx==92 .OR. Kx==93 .OR. Kx==94 .OR. Kx==97 .OR. Kx==98 .OR.  &
         & Kx==100 ) GOTO 100
      IF ( Kx==2 .OR. Kx==89 ) THEN
!
!**************    MATT2,189-MATT3     *********************************
!
         DO l = 1 , 16
            IF ( M(l)<0 ) GOTO 300
            I(l) = M(l)
         ENDDO
         IF ( M(1)==0 ) GOTO 300
         N = 16
         GOTO 500
      ELSEIF ( Kx==4 ) THEN
!
!******           104-CTORDRG           ************************
!
         IF ( M(1)<=0 .OR. M(3)<=0 .OR. M(4)<=0 .OR. M(3)==M(4) .OR. Z(5)<0.0 .OR. Z(5)>180.0 .OR. Z(6)<0.0 .OR. Z(6)>180.0 )       &
            & GOTO 300
         IF ( Mf(2)==0 ) M(2) = M(1)
         IF ( M(2)<=0 ) GOTO 300
         N = 7
         GOTO 400
      ELSEIF ( Kx==5 .OR. Kx==24 ) THEN
!
!*******       SPOINT,124-EPOINT    ************************************
!
         IF ( Mf(2)==3 ) THEN
            IF ( M(2)/=thru ) GOTO 300
            IF ( Mf(1)/=1 .OR. Mf(3)/=1 ) GOTO 200
            k2078 = 208
            IF ( K==124 ) k2078 = 207
            l1 = 1
            l2 = 4
            DO l = l2 , 8
               IF ( Mf(l)/=0 ) GOTO 200
            ENDDO
            IF ( M(l2)>9999999 ) GOTO 300
            ii = M(l1) - 1
            l2 = M(l2) - M(l1)
            IF ( ii<0 .OR. l2<=0 ) GOTO 300
            l1 = 1
            DO l = 1 , l2
               ii = ii + 1
               CALL write(k2078,ii,1,0)
            ENDDO
            I(1) = ii + 1
            N = 1
            GOTO 500
         ELSE
            DO l = 1 , 8
               IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) GOTO 200
               IF ( M(l)<0 ) GOTO 300
               IF ( M(l)/=0 ) THEN
                  IF ( M(l)>999999 ) GOTO 300
                  N = N + 1
                  I(N) = M(l)
                  IF ( N>1 ) THEN
                     DO l1 = 2 , N
                        IF ( I(N)==I(l1-1) ) GOTO 300
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
            IF ( N>0 ) GOTO 500
            GOTO 300
         ENDIF
      ELSEIF ( Kx==22 ) THEN
!
!*******         122-MAT3        *****************************
!
         IF ( M(1)<=0 .OR. Z(2)<0. .OR. Z(3)<0. .OR. Z(4)<0. .OR. Z(9)<0. .OR. Z(10)<0. .OR. Z(11)<0. ) GOTO 300
         IF ( abs(Z(5))>1. .OR. abs(Z(6))>1. .OR. abs(Z(7))>1. ) THEN
            CALL page2(2)
            WRITE (Nout,99001) Uwm , T1(1,K) , T1(2,K) , Knt
99001       FORMAT (A25,' 301, BULK DATA CARD ',2A4,' CONTAINS INCONSISTENT',' DATA.',10X,'SORTED CARD COUNT =',I7)
         ENDIF
         N = 16
         GOTO 400
      ELSEIF ( Kx==95 ) THEN
!
!
!*******       195-RANDPS       ****************************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<M(2) .OR. M(6)<0 ) GOTO 300
         IF ( M(2)==M(3) .AND. Z(5)/=0.0 ) GOTO 300
         N = 6
         IF ( Kout<=2 ) GOTO 800
         IF ( M(1)==J(Kout) ) GOTO 400
         IF ( Kout/=J(1) ) GOTO 800
         GOTO 300
      ELSEIF ( Kx==96 ) THEN
!
!*******       196-RANDT1       ****************************************
!
         IF ( Kout>2 ) THEN
            DO in = 3 , Kout
               IF ( M(1)==J(in) ) GOTO 900
            ENDDO
         ENDIF
         GOTO 300
      ELSEIF ( Kx==99 ) THEN
         GOTO 1000
      ENDIF
   ENDIF
   IF ( Ky<=100 ) THEN
      IF ( Ky==1 ) THEN
!
!**********          201-TEMPP1          *******************************
!
         IF ( Km/=0 ) GOTO 1500
         nn = 6
         N = 6
         Id = M(1)
         IF ( Mf(5)==-32767 ) GOTO 1300
         IF ( Mf(7)/=0 .OR. Mf(8)/=0 ) Badfor = .TRUE.
         GOTO 1100
      ELSEIF ( Ky==2 ) THEN
!
!*******       202-TEMPP2         **************************************
!
         IF ( Km/=0 ) GOTO 1500
         nn = 8
         N = 8
         Id = M(1)
         IF ( Mf(5)==-32767 ) GOTO 1300
         IF ( Mf(7)/=0 .AND. Mf(7)/=2 .OR. Mf(8)/=0 .AND. Mf(8)/=2 ) Badfor = .TRUE.
         GOTO 1100
      ELSEIF ( Ky==3 ) THEN
         GOTO 1600
      ELSEIF ( Ky==4 ) THEN
!
!*******       204-TEMPRB         **************************************
!
         IF ( Km/=0 ) THEN
            IF ( Km>1 ) GOTO 1500
            N = 0
            DO l = 1 , 8
               IF ( Mf(l)==-32767 ) GOTO 1800
               IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) Badfor = .TRUE.
               I(l+8) = M(l)
               save(l+8) = M(l)
            ENDDO
            GOTO 1700
         ELSE
            nn = 16
            N = 0
            Id = M(1)
            l1 = 1
            IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
            DO l = 3 , 8
               IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
                  IF ( Mf(l)==-32767 ) THEN
                     DO l5 = l , 8
                        M(l5) = 0
                     ENDDO
                  ELSE
                     Badfor = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
            DO l = 1 , 8
               I(l) = M(l)
               save(l) = M(l)
            ENDDO
            GOTO 1700
         ENDIF
      ELSEIF ( Ky==5 ) THEN
!
!*******       205-GRIDB          **************************************
!
         ASSIGN 2000 TO ret
         GOTO 3600
      ELSEIF ( Ky==6 ) THEN
         GOTO 2100
      ELSEIF ( Ky==7 ) THEN
!
!*******       207-RINGFL         **************************************
!
         ASSIGN 3000 TO ret
         GOTO 3600
      ELSEIF ( Ky==8 ) THEN
!
!*******       208-PRESPT         **************************************
!
         ASSIGN 3100 TO ret
         GOTO 3600
      ELSEIF ( Ky==9 ) THEN
!
!*******       209-CFLUID2        **************************************
!
         kfl = 2
         ASSIGN 3200 TO ret
         GOTO 3600
      ELSEIF ( Ky==10 ) THEN
!
!*******       210-CFLUID3        **************************************
!
         kfl = 3
         ASSIGN 3200 TO ret
         GOTO 3600
      ELSEIF ( Ky==11 ) THEN
!
!*******       211-CFLUID4        **************************************
!
         kfl = 4
         ASSIGN 3200 TO ret
         GOTO 3600
      ELSEIF ( Ky==12 ) THEN
!
!*******       212-AXIF           **************************************
!
         N = 0
         IF ( Km==0 ) THEN
            IF ( Iaxf>0 ) THEN
               CALL page2(2)
               WRITE (Nout,99002) Ufm
99002          FORMAT (A23,' 4121, ONLY ONE (1) AXIF CARD ALLOWED IN BULK DATA.')
               Abort = .TRUE.
            ELSE
               Iaxf = Iaxf + 1
               IF ( Mf(1)/=1 .OR. Mf(2)/=0 .AND. Mf(2)/=2 .OR. Mf(3)/=0 .AND. Mf(3)/=2 .OR. Mf(4)/=0 .AND. Mf(4)/=2 .OR. Mf(5)/=3 ) &
                  & Badfor = .TRUE.
               IF ( Mf(7)/=0 .OR. Mf(8)/=0 .OR. Mf(6)/=0 .AND. Mf(6)/=3 ) Badfor = .TRUE.
               IF ( Mf(3)==0 ) M(3) = 1
               IF ( M(5)/=bcdyes .AND. M(5)/=bcdno ) Baddat = .TRUE.
               IF ( M(5)==bcdyes ) M(5) = 1
               IF ( M(5)==bcdno ) M(5) = 0
               CALL write(215,M,5,0)
               IF ( Mf(6)==3 ) THEN
                  IF ( M(7)/=bcdnon ) Baddat = .TRUE.
                  IF ( M1(1)==0 .AND. M1(2)==0 ) Baddat = .TRUE.
               ELSE
                  IF ( M1(1)/=0 .OR. M1(2)/=0 ) Baddat = .TRUE.
               ENDIF
            ENDIF
            GOTO 3500
         ELSEIF ( Mf(2)/=3 ) THEN
            DO l = 1 , 8
               IF ( Mf(l)/=0 ) THEN
                  IF ( Mf(l)==1 ) THEN
                     IF ( M(l)<=Naxf ) Baddat = .TRUE.
                     N = N + 1
                     Naxf = M(l)
                     I(N) = M(l)
                  ELSE
                     Badfor = .TRUE.
                     N = 0
                     GOTO 3500
                  ENDIF
               ENDIF
            ENDDO
            IF ( N<=0 ) Baddat = .TRUE.
            GOTO 3500
         ELSEIF ( Mf(4)==3 ) THEN
            l1 = M(7)
            l2 = l1
            IF ( Mf(1)/=1 .OR. Mf(3)/=1 .OR. Mf(5)/=1 .OR. Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 3300
            IF ( M(1)<M(4) .AND. M(7)>0 .AND. M(7)<=M(4) .AND. mod(M(4)-M(1),M(7))==0 ) GOTO 3400
            Baddat = .TRUE.
            GOTO 3500
         ELSE
            l1 = 1
            l2 = 1
            IF ( Mf(1)/=1 .OR. Mf(3)/=1 ) GOTO 3300
            DO l = 4 , 8
               IF ( Mf(l)/=0 ) GOTO 3300
            ENDDO
            IF ( M(1)<M(4) .AND. M(1)>=0 ) GOTO 3400
            Baddat = .TRUE.
            GOTO 3500
         ENDIF
      ELSEIF ( Ky==13 ) THEN
         GOTO 2100
      ELSEIF ( Ky==14 ) THEN
         ASSIGN 3100 TO ret
         GOTO 3600
      ELSEIF ( Ky==15 .OR. Ky==16 .OR. Ky==23 .OR. Ky==24 .OR. Ky==25 .OR. Ky==26 .OR. Ky==27 .OR. Ky==28 .OR. Ky==29 .OR.          &
             & Ky==30 .OR. Ky==31 .OR. Ky==32 .OR. Ky==33 .OR. Ky==34 .OR. Ky==35 .OR. Ky==36 .OR. Ky==37 .OR. Ky==38 .OR.          &
             & Ky==40 .OR. Ky==41 .OR. Ky==43 .OR. Ky==44 .OR. Ky==45 .OR. Ky==46 .OR. Ky==47 .OR. Ky==48 .OR. Ky==49 .OR.          &
             & Ky==50 .OR. Ky==51 .OR. Ky==52 .OR. Ky==53 .OR. Ky==54 .OR. Ky==55 .OR. Ky==56 .OR. Ky==57 .OR. Ky==58 .OR.          &
             & Ky==59 .OR. Ky==60 .OR. Ky==61 .OR. Ky==62 .OR. Ky==63 .OR. Ky==64 .OR. Ky==65 .OR. Ky==66 .OR. Ky==67 .OR.          &
             & Ky==68 .OR. Ky==69 .OR. Ky==70 .OR. Ky==71 .OR. Ky==72 .OR. Ky==73 .OR. Ky==74 .OR. Ky==75 .OR. Ky==76 .OR.          &
             & Ky==77 .OR. Ky==78 .OR. Ky==79 .OR. Ky==80 .OR. Ky==81 .OR. Ky==82 .OR. Ky==83 .OR. Ky==84 .OR. Ky==85 .OR.          &
             & Ky==86 .OR. Ky==87 .OR. Ky==88 .OR. Ky==89 .OR. Ky==90 .OR. Ky==91 .OR. Ky==92 .OR. Ky==93 .OR. Ky==94 .OR.          &
             & Ky==97 .OR. Ky==98 .OR. Ky==99 .OR. Ky==100 ) THEN
         GOTO 100
      ELSEIF ( Ky==17 ) THEN
!
!*******       217-CTETRA,  335-CFTETRA  *******************************
!
         N = 6
         GOTO 3700
      ELSEIF ( Ky==18 ) THEN
!
!*******       218-CWEDGE,  336-CFWEDGE  *******************************
!
         N = 8
         GOTO 3700
      ELSEIF ( Ky==19 ) THEN
         GOTO 3800
      ELSEIF ( Ky==20 ) THEN
         GOTO 3900
      ELSEIF ( Ky==21 ) THEN
!
!*******       221-DMIAX          **************************************
!
         IF ( Fphys1 ) THEN
            Fphys1 = .FALSE.
            nm(1) = 0
            nm(2) = 0
         ENDIF
         IF ( Km/=0 ) THEN
            IF ( M(1)<=0 .OR. M(2)<0 .OR. M(2)>6 ) GOTO 4300
            IF ( Mf(1)/=1 .OR. Mf(2)/=1 .AND. Mf(2)/=0 ) GOTO 4300
            IF ( Mf(4)/=0 .AND. Mf(4)+ity1/=4 ) GOTO 4300
            IF ( Mf(5)/=0 .AND. ty1/=3 .AND. ty1/=4 ) GOTO 4300
            IF ( iabs(M(3))>Naxf ) GOTO 4300
            IF ( Mf(3)/=1 .AND. Mf(3)/=0 ) GOTO 4300
            N = 3
            I(2) = M(2)
            l1 = 3
            l2 = 3
            ASSIGN 4100 TO ret
            GOTO 4400
         ELSEIF ( M(3)==0 ) THEN
            ASSIGN 4000 TO ret
            GOTO 3600
         ELSE
            IF ( M(1)/=nm(1) .OR. M(2)/=nm(2) ) GOTO 4200
            IF ( Mf(2)/=1 .OR. Mf(3)/=1 .AND. Mf(3)/=0 ) GOTO 4300
            IF ( Mf(4)/=1 .AND. Mf(4)/=0 ) GOTO 4300
            IF ( M(3)<=0 .OR. M(4)<0 .OR. M(4)>6 ) GOTO 4300
            IF ( iabs(M(5))>Naxf ) GOTO 4300
            IF ( Mf(5)/=0 .OR. Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 4300
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 4300
            N = 2
            I(2) = M(4)
            l1 = 4
            l2 = 5
            ASSIGN 4600 TO ret
            GOTO 4400
         ENDIF
      ELSEIF ( Ky==22 ) THEN
!
!*******       222-FLSYM          **************************************
!
         IF ( Lflsym ) THEN
            CALL page2(2)
            WRITE (Nout,99003) Ufm
99003       FORMAT (A23,' 4123, ONLY ONE (1) FLSYM CARD ALLOWED IN BULK DATA')
            Abort = .TRUE.
            GOTO 500
         ELSE
            Lflsym = .TRUE.
            ASSIGN 4700 TO ret
            GOTO 3600
         ENDIF
      ELSEIF ( Ky==39 .OR. Ky==42 ) THEN
         GOTO 1000
      ELSEIF ( Ky==95 ) THEN
         GOTO 1600
      ELSEIF ( Ky==96 ) THEN
!
!*******      296-TEMPP4    ********************************************
!
         IF ( Km/=0 ) THEN
            IF ( Km>1 ) GOTO 1500
            N = 0
            l3 = 8*Km
            IF ( Mf(7)/=0 .AND. Mf(8)/=0 ) Badfor = .TRUE.
            DO l = 1 , 6
               IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
                  IF ( Mf(l)==-32767 ) THEN
                     DO l6 = l , 6
                        M(l6) = 0
                     ENDDO
                  ELSE
                     Badfor = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            DO l = 1 , 6
               l5 = l3 + l
               I(l5) = M(l)
               save(l5) = M(l)
            ENDDO
            GOTO 1900
         ELSE
            nn = 14
            N = 0
            Id = M(1)
            l1 = 1
            IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
            DO l = 3 , 8
               IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
                  IF ( Mf(l)==-32767 ) THEN
                     DO l5 = l , 8
                        M(l5) = 0
                     ENDDO
                  ELSE
                     Badfor = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
            DO l = 1 , 8
               I(l) = M(l)
               save(l) = M(l)
            ENDDO
            GOTO 1900
         ENDIF
      ENDIF
   ENDIF
   kz = K - 300
   IF ( kz<=39 ) THEN
      IF ( kz==21 ) THEN
!
!*******     321-CEMLOOP     *******************************************
!
         IF ( M(1)<=0 .OR. M(13)<0 ) GOTO 300
         IF ( M(3)==0 ) THEN
            dx1 = Z(4) - Z(10)
            dy1 = Z(5) - Z(11)
            dz1 = Z(6) - Z(12)
            dx2 = Z(7) - Z(10)
            dy2 = Z(8) - Z(11)
            dz2 = Z(9) - Z(12)
            dl1 = dx1**2 + dy1**2 + dz1**2
            dl2 = dx2**2 + dy2**2 + dz2**2
            IF ( abs(dl1-dl2)>1.E-4 ) GOTO 300
            dc1 = dy1*dz2 - dy2*dz1
            dc2 = dx2*dz1 - dx1*dz2
            dc3 = dx1*dy2 - dy1*dx2
            dlc = sqrt(dc1**2+dc2**2+dc3**2)
            IF ( dlc/sqrt(dl2)<.0001 ) GOTO 300
            N = 13
            GOTO 400
         ELSE
            IF ( M(5)/=0 ) GOTO 300
            DO iem = 7 , 13
               IF ( M(iem)/=0 ) GOTO 300
            ENDDO
            N = 13
            GOTO 400
         ENDIF
      ELSEIF ( kz==22 .OR. kz==26 ) THEN
!
!*******    322-SPCFLD,   326-REMFLUX      *****************************
!
         IF ( M(1)<=0 ) GOTO 300
         IF ( M(2)<0 ) GOTO 300
         IF ( M(6)==-1 ) THEN
            DO l = 7 , 8
               IF ( Mf(l)/=0 ) GOTO 200
            ENDDO
            N = 6
            GOTO 400
         ELSEIF ( Mf(7)==3 ) THEN
            IF ( M(7)/=thru ) GOTO 300
            IF ( Mf(6)/=1 .OR. Mf(8)/=1 ) GOTO 200
            l1 = 6
            l2 = 9
            ii = M(l1) - 1
            l2 = M(l2) - M(l1)
            IF ( ii<0 .OR. l2<=0 ) GOTO 300
            l1 = 1
            DO l = 1 , 5
               I(l) = M(l)
            ENDDO
            N = 6
            DO l = l1 , l2
               I(6) = l + ii
               CALL write(209,I,N,0)
            ENDDO
            I(6) = ii + l2 + 1
            GOTO 500
         ELSE
            DO l = 6 , 8
               IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) GOTO 200
               IF ( M(l)<0 ) GOTO 300
               IF ( M(l)/=0 ) THEN
                  N = N + 6
                  I(N-5) = M(1)
                  I(N-4) = M(2)
                  I(N-3) = M(3)
                  I(N-2) = M(4)
                  I(N-1) = M(5)
                  I(N) = M(l)
               ENDIF
            ENDDO
            IF ( N>0 ) GOTO 500
            GOTO 300
         ENDIF
      ELSEIF ( kz==23 ) THEN
!
!*****   323-CIS2D8   **************************************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 300
         IF ( M(11)<0 .OR. Z(12)<0. ) GOTO 300
         IF ( M(11)==0 ) M(11) = 2
         IF ( M(11)/=2 .AND. M(11)/=3 ) GOTO 300
         DO l = 3 , 10
            IF ( M(l)<=0 ) GOTO 300
         ENDDO
         DO l = 3 , 9
            lp1 = l + 1
            DO lll = lp1 , 10
               IF ( M(l)==M(lll) ) GOTO 300
            ENDDO
         ENDDO
         N = 12
         GOTO 400
      ELSEIF ( kz==24 ) THEN
!
!*****   324-PIS2D8   **************************************************
!
         IF ( Z(3)<=0. ) GOTO 300
         IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 300
         N = 3
         GOTO 400
      ELSEIF ( kz==25 ) THEN
!
!*****   325-GEMLOOP   *************************************************
!
         IF ( Mf(1)/=1 ) GOTO 200
         IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) GOTO 200
         IF ( Mf(3)/=1 .AND. Mf(3)/=0 ) GOTO 200
         IF ( M(1)<=0 .OR. M(3)<0 ) GOTO 300
!
!     FOR NOW, CID MUST BE 0
!
         IF ( M(3)/=0 ) GOTO 200
         npts = 0
         DO l = 4 , 49 , 3
            IF ( Mf(l)==3 ) GOTO 4800
            npts = npts + 1
            IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) GOTO 200
            IF ( Mf(l+1)/=2 .AND. Mf(l+1)/=0 ) GOTO 200
            IF ( Mf(l+2)/=2 .AND. Mf(l+2)/=0 ) GOTO 200
         ENDDO
         GOTO 300
      ELSEIF ( kz==27 ) THEN
!
!*****   327-BFIELD   **************************************************
!
         IF ( M(1)<0 ) GOTO 300
         IF ( M(2)==-1 ) THEN
            DO l = 3 , 8
               IF ( Mf(l)/=0 ) GOTO 200
            ENDDO
            N = 2
            GOTO 400
         ELSEIF ( Mf(3)==3 ) THEN
            IF ( M(3)/=thru ) GOTO 200
            IF ( Mf(2)/=1 .OR. Mf(4)/=1 ) GOTO 200
            l1 = 2
            l2 = 5
            ii = M(l1) - 1
            l2 = M(l2) - M(l1)
            IF ( ii<0 .OR. l2<=0 ) GOTO 300
            l1 = 1
            I(1) = M(1)
            N = 2
            DO l = l1 , l2
               I(2) = l + ii
               CALL write(201,I,N,0)
            ENDDO
            I(2) = ii + l2 + 1
            GOTO 500
         ELSE
            DO l = 2 , 8
               IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) GOTO 200
               IF ( M(l)<0 ) GOTO 300
               IF ( M(l)/=0 ) THEN
                  N = N + 2
                  I(N-1) = M(1)
                  I(N) = M(l)
               ENDIF
            ENDDO
            IF ( N>0 ) GOTO 500
            GOTO 300
         ENDIF
      ELSEIF ( kz==28 ) THEN
!
!*****   328-MDIPOLE     ***********************************************
!
         IF ( M(1)<=0 .OR. M(2)<0 ) GOTO 300
         IF ( Z(9)<0. .OR. Z(10)<0. ) GOTO 300
         N = 10
         GOTO 400
      ELSEIF ( kz==33 ) THEN
         GOTO 3800
      ELSEIF ( kz==34 ) THEN
         GOTO 3900
      ELSEIF ( kz==35 ) THEN
         N = 6
         GOTO 3700
      ELSEIF ( kz==36 ) THEN
         N = 8
         GOTO 3700
      ENDIF
   ENDIF
 100  CALL page2(2)
   WRITE (Nout,99004) Sfm
99004 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS4P.')
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
 700  IF ( M(1)<=0 .OR. M(i1+2)<=0 ) GOTO 300
   DO l = 2 , i1
      IF ( M(l)<=0 ) GOTO 300
      IF ( l/=2 ) THEN
         DO l1 = l , i1
            IF ( M(l-1)==M(l1) ) GOTO 300
         ENDDO
      ENDIF
   ENDDO
   N = i1 + 2
   GOTO 400
 800  Kout = Kout + 1
   J(Kout) = M(1)
   GOTO 400
 900  IF ( M(1)<=0 .OR. M(2)<=0 .OR. Z(3)<0.0 .OR. Z(4)<=Z(3) ) GOTO 300
   N = 4
   GOTO 400
!
!*****         199-PLOAD2,239-QBDY1,242-QVOL   *************************
!
 1000 IF ( Km/=0 ) THEN
      l = 1
   ELSE
      IF ( Mf(1)/=1 .OR. Mf(2)/=2 .AND. Mf(2)/=0 ) GOTO 200
      IF ( M(1)<=0 ) GOTO 300
      l = 3
      isid = M(1)
      iqvl = M(2)
   ENDIF
   IF ( Mf(8)==3 ) GOTO 200
   ntot = 0
   k2078 = 209
   DO WHILE ( M(l)/=0 )
      IF ( M(l)<0 ) GOTO 300
      IF ( Mf(l)==3 ) GOTO 200
      IF ( Mf(l+1)==3 ) THEN
         IF ( M(l+1)/=thru ) GOTO 300
         IF ( Mf(l+3)/=1 .AND. Mf(l+3)/=0 ) GOTO 200
         l1 = M(l) - 1
         l2 = M(l+3) - l1
         IF ( l2<=1 .OR. l1<0 ) GOTO 300
         DO ii = 1 , l2
            N = N + 3
            I(N-2) = isid
            I(N-1) = iqvl
            I(N) = ii + l1
            IF ( N>=48 ) THEN
               CALL write(k2078,I,N,0)
               ntot = ntot + N
               N = 0
            ENDIF
         ENDDO
         l = l + 4
      ELSE
         IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) GOTO 200
         N = N + 3
         I(N-2) = isid
         I(N-1) = iqvl
         I(N) = M(l)
         l = l + 1
         IF ( N>=48 ) THEN
            CALL write(k2078,I,N,0)
            ntot = ntot + N
            N = 0
         ENDIF
      ENDIF
      IF ( l>8 ) EXIT
   ENDDO
   T4(2,K) = T4(2,K) + ntot
   IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Km = 1
   ELSE
      Km = 0
   ENDIF
   GOTO 500
 1100 DO l = 3 , 6
      IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) Badfor = .TRUE.
   ENDDO
 1200 IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
   IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
   DO l = 1 , N
      I(l) = M(l)
      save(l) = M(l)
   ENDDO
   GOTO 1400
 1300 DO l = 3 , 4
      IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) Badfor = .TRUE.
   ENDDO
   GOTO 1200
 1400 IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Kn = 1
      Km = Km + 1
   ELSE
      Km = 0
      Kn = 0
   ENDIF
   GOTO 600
 1500 IF ( Mf(2)==3 .OR. Mf(5)==3 ) THEN
      N = 0
      IF ( Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
         IF ( Mf(4)/=0 .OR. Mf(5)/=-32767 ) THEN
            Badfor = .TRUE.
            GOTO 1400
         ENDIF
      ENDIF
      l1 = -1
      DO l = 1 , 4 , 3
         IF ( Mf(l)/=0 .OR. Mf(l+1)/=0 .OR. Mf(l+2)/=0 ) THEN
            IF ( Mf(l)==1 .AND. Mf(l+1)==3 .AND. Mf(l+2)==1 ) THEN
               l1 = l1 + 1
               l2 = l1 + l
               IF ( M(l2)>0 .AND. M(l2+1)==thru .AND. M(l2+3)>M(l2) ) THEN
                  l3 = M(l2)
                  l4 = M(l2+3)
                  DO l5 = l3 , l4
                     save(2) = l5
                     CALL write(209,save,nn,0)
                  ENDDO
               ELSE
                  Baddat = .TRUE.
               ENDIF
            ELSE
               IF ( Mf(l+1)==-32767 ) EXIT
               Badfor = .TRUE.
            ENDIF
         ENDIF
      ENDDO
   ELSE
      N = 0
      DO l = 1 , 8
         IF ( Mf(l)/=0 ) THEN
            IF ( Mf(l)/=1 ) THEN
               IF ( Mf(l)==-32767 ) EXIT
               Badfor = .TRUE.
            ELSEIF ( M(l)>0 ) THEN
               save(2) = M(l)
               CALL write(209,save,nn,0)
            ELSE
               Baddat = .TRUE.
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   GOTO 1400
!
!*******       203-TEMPP3         **************************************
!
 1600 IF ( Km/=0 ) THEN
      IF ( Km>2 ) GOTO 1500
      N = 0
      l3 = 8*Km
      DO l = 1 , 7 , 2
         IF ( Mf(l)/=0 .OR. Mf(l+1)/=0 ) THEN
            IF ( Mf(l)/=-32767 ) THEN
               IF ( Mf(l)/=0 .AND. Mf(l)/=2 .OR. Mf(l+1)/=0 .AND. Mf(l+1)/=2 ) Badfor = .TRUE.
               IF ( zz>=Z(l) ) Baddat = .TRUE.
            ELSE
               Mf(7) = 0
               Mf(8) = 0
            ENDIF
         ENDIF
         zz = Z(l)
         l5 = l3 + l
         I(l5) = M(l)
         save(l5) = M(l)
         I(l5+1) = M(l+1)
         save(l5+1) = M(l+1)
      ENDDO
   ELSE
      nn = 24
      N = 0
      Id = M(1)
      l1 = 1
      IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) Badfor = .TRUE.
      DO l = 3 , 8
         IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
            IF ( Mf(l)==-32767 ) THEN
               DO l5 = l , 8
                  M(l5) = 0
               ENDDO
               Mf(7) = 0
               Mf(8) = 0
            ELSE
               Badfor = .TRUE.
            ENDIF
         ENDIF
      ENDDO
      IF ( M(1)<=0 .OR. M(2)<=0 ) Baddat = .TRUE.
      IF ( Z(3)>=Z(5) ) Baddat = .TRUE.
      zz = Z(5)
      IF ( Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
         IF ( zz>=Z(7) ) Baddat = .TRUE.
      ENDIF
      zz = Z(7)
      DO l = 1 , 8
         I(l) = M(l)
         save(l) = M(l)
      ENDDO
   ENDIF
   l1 = l1 + 8
   IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Km = Km + 1
      Kn = 1
      IF ( Km<3 ) GOTO 600
   ELSE
      Km = 0
      Kn = 0
   ENDIF
   IF ( l1<=nn ) THEN
      DO l = l1 , nn
         I(l) = 0
         save(l) = 0
      ENDDO
   ENDIF
   N = nn
   GOTO 600
 1700 l1 = l1 + 8
   IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Km = Km + 1
      Kn = 1
      IF ( Km<2 ) GOTO 600
   ELSE
      Km = 0
      Kn = 0
   ENDIF
   IF ( l1<=nn ) THEN
      DO l = l1 , nn
         I(l) = 0
         save(l) = 0
      ENDDO
   ENDIF
   N = nn
   GOTO 600
 1800 DO l = 5 , 8
      I(l+8) = 0
      save(l+8) = 0
   ENDDO
!
!    TEMPG IS MODELLED AFTER TEMPP3
!    TEMPP4 IS MODELLED AFTER TEMPP1,EXCEPT THAT TEMPP1 HAS ONE LESS C
!
!
!*******      295-TEMPG     ********************************************
!
   GOTO 1700
 1900 l1 = l1 + 8
   IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Km = Km + 1
      Kn = 1
      IF ( Km<2 ) GOTO 600
   ELSE
      Km = 0
      Kn = 0
   ENDIF
   IF ( l1<=nn ) THEN
      DO l = l1 , nn
         I(l) = 0
         save(l) = 0
      ENDDO
   ENDIF
   N = nn
   GOTO 600
 2000 IF ( M(1)<=0 .OR. M(6)<0 .OR. M(8)<=0 ) GOTO 300
   IF ( ifpdco(M(7)) ) GOTO 300
   N = 5
   I(1) = M(1)
   I(2) = M(4)
   I(3) = M(6)
   I(4) = M(7)
   I(5) = M(8)
   GOTO 500
!
!*******       206-FSLIST         **************************************
!
 2100 IF ( Km/=0 ) THEN
      l1 = 1
      l2 = 0
      GOTO 2500
   ELSE
      ASSIGN 2200 TO ret
      GOTO 3600
   ENDIF
 2200 IF ( Mf(1)==0 .OR. Mf(1)==2 ) THEN
      IF ( .NOT.(Mf(1)==0 .OR. (Mf(1)==2 .AND. Z(1)>0.0)) ) GOTO 2400
      IF ( Mf(1)==0 ) M(1) = 1
      I(1) = M(1)
      N = 1
      l1 = 2
      l2 = 0
      IF ( Mf(2)/=3 ) GOTO 2500
      IF ( M(2)/=bcdaxi ) GOTO 2400
      N = N + 1
      I(N) = 0
      l1 = l1 + 1
      l2 = 1
      GOTO 2500
   ENDIF
 2300 Badfor = .TRUE.
   GOTO 2900
 2400 Baddat = .TRUE.
   GOTO 2900
 2500 DO l = l1 , 8
      l3 = l + l2
      IF ( Mf(l)==3 ) GOTO 2700
      IF ( Mf(l)==0 ) GOTO 2800
      IF ( Mf(l)/=1 ) GOTO 2300
      IF ( M(l3)<=0 ) GOTO 2400
      N = N + 1
      I(N) = M(l3)
   ENDDO
 2600 IF ( N>0 ) GOTO 2900
   GOTO 2400
 2700 IF ( M(l3)/=bcdaxi ) GOTO 2400
   N = N + 1
   I(N) = 0
 2800 IF ( l/=8 ) THEN
      l = l + 1
      DO l2 = l , 8
         IF ( Mf(l2)/=0 ) GOTO 2300
      ENDDO
   ENDIF
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 2600
   GOTO 2400
 2900 IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Kn = 1
      Km = Km + 1
   ELSE
      Km = 0
      Kn = 0
      N = N + 1
      I(N) = -1
   ENDIF
   GOTO 600
 3000 DO l = 1 , 5 , 4
      IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
         IF ( M(l)<=0 .OR. Z(l+1)<=0.0 ) GOTO 300
         N = N + 4
         IF ( N>4 .AND. M(l)==M(l-4) ) GOTO 300
         IF ( M(l)<=99999 ) THEN
            I(N-3) = M(l)
            I(N-2) = M(l+1)
            I(N-1) = M(l+2)
            I(N) = M(l+3)
         ELSE
            CALL page2(2)
            WRITE (Nout,99008) Ufm
            GOTO 300
         ENDIF
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
 3100 IF ( M(1)<=0 ) GOTO 300
   DO l = 3 , 7 , 2
      IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
         IF ( M(l)<=0 ) GOTO 300
         N = N + 3
         I(N-2) = M(1)
         I(N-1) = M(l)
         I(N) = M(l+1)
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 500
   GOTO 300
 3200 IF ( M(1)<=0 ) GOTO 300
   IF ( M(1)<=99999 ) THEN
      DO l = 2 , kfl
         IF ( M(l)<=0 ) GOTO 300
         IF ( l/=kfl ) THEN
            l2 = l + 1
            DO l1 = l2 , kfl
               IF ( M(l)==M(l1) ) GOTO 300
            ENDDO
         ENDIF
      ENDDO
      I(1) = M(1)
      N = kfl + 3
      IF ( Mf(6)==0 ) M(6) = 1
      IF ( Mf(7)==0 ) M(7) = 1
      I(kfl+2) = M(6)
      I(kfl+3) = M(7)
      DO l = 1 , kfl
         I(l+1) = M(l+1)
      ENDDO
      GOTO 500
   ELSE
      CALL page2(2)
      WRITE (Nout,99008) Ufm
      GOTO 300
   ENDIF
 3300 Badfor = .TRUE.
   GOTO 3500
 3400 IF ( M(1)<=Naxf ) Baddat = .TRUE.
   IF ( M(1)>0 ) THEN
      l2 = M(1)
   ELSE
      CALL write(215,0,1,0)
   ENDIF
   l3 = M(4)
   DO l = l2 , l3 , l1
      CALL write(215,l,1,0)
   ENDDO
   Naxf = l3
 3500 IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Kn = 1
      Km = Km + 1
   ELSE
      Km = 0
      Kn = 0
      IF ( Naxf>=100 ) THEN
         CALL page2(2)
         WRITE (Nout,99005) Ufm , Naxf
99005    FORMAT (A23,' 4125, MAXIMUM ALLOWABLE HARMONIC ID IS 99.  DATA ','CONTAINS MAXIMUM =',I20)
         Abort = .TRUE.
      ENDIF
      N = N + 1
      I(N) = -1
   ENDIF
   GOTO 600
 3600 IF ( Iaxf<=0 ) THEN
      IF ( Lharm ) CALL page2(2)
      IF ( Lharm ) WRITE (Nout,99006) Ufm
99006 FORMAT (A23,' 4122, AXIF CARD REQUIRED.')
      Lharm = .FALSE.
      Abort = .TRUE.
   ENDIF
!
!*******       213-BDYLIST        **************************************
!
!
!*******       214-FREEPT         **************************************
!
   GOTO ret
 3700 DO l = 1 , N
      IF ( M(l)<=0 ) GOTO 300
   ENDDO
   n1 = N - 1
   DO l = 3 , n1
      l2 = l + 1
      DO l1 = l2 , N
         IF ( M(l)==M(l1) ) GOTO 300
      ENDDO
   ENDDO
   GOTO 400
!
!*******       219-CHEXA1,  333-CFHEX1   *******************************
!
 3800 IF ( Mf(15)/=0 .OR. Mf(16)/=0 ) GOTO 200
   N = 10
   GOTO 3700
!
!*******       220-CHEXA2,  334-CFHEX2   *******************************
!
 3900 IF ( Mf(15)/=0 .OR. Mf(16)/=0 ) GOTO 200
   N = 10
   GOTO 3700
 4000 IF ( Mf(1)/=3 .OR. M(1)==nm(1) .AND. M(2)==nm(2) ) GOTO 4200
   ifo = M(4)
   ty1 = M(5)
   ity1 = 2*mod(ty1,2)
   ty2 = M(6)
   IF ( Mach==12 ) THEN
      IF ( ty2==2 .OR. ty2==4 ) ty2 = ty2 - 1
   ENDIF
   IF ( ifo/=1 .AND. ifo/=2 .AND. ifo/=6 ) GOTO 4200
   IF ( ty1<=0 .OR. ty1>4 .OR. ty2<=0 .OR. ty2>4 ) GOTO 4200
   IF ( ty2==1 .AND. ty1==3 ) GOTO 4200
   nm(1) = M(1)
   nm(2) = M(2)
   IF ( Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 4300
   IF ( M1f(2)/=3 .OR. M1(3)/=nm(1) .OR. M1(4)/=nm(2) ) GOTO 4300
   N = 9
   GOTO 400
 4100 I(3) = M(4)
   IF ( ty1/=1 ) THEN
      N = 4
      I(4) = M(5)
      IF ( ty1/=2 .AND. ty1/=3 ) THEN
         N = 6
         I(5) = M(6)
         I(6) = M(7)
      ENDIF
   ENDIF
   IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 4600
   N = N + 2
   I(N-1) = -1
   I(N) = -1
   IF ( M1(1)/=T1(1,K) .OR. M1(2)/=T1(2,K) .OR. M1(3)/=nm(1) .OR. M1(4)/=nm(2) ) THEN
      N = N + 2
      I(N-1) = -1
      I(N) = -1
   ENDIF
   GOTO 4500
 4200 nm(1) = M(1)
   nm(2) = M(2)
 4300 Abort = .TRUE.
   CALL page2(2)
   WRITE (Nout,99007) Ufm , nm(1) , nm(2)
99007 FORMAT (A23,' 4126, BAD DATA OR FORMAT OR NON-UNIQUE NAME, DMIAX',1X,2A4)
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 4500
   GOTO 4600
 4400 IF ( Mf(l1)/=1 ) THEN
      I(1) = M(l2-2)
   ELSEIF ( M(l2)<0 ) THEN
      I(1) = 500000*(1-M(l2)*2) + M(l2-2)
   ELSE
      I(1) = 1000000*(1+M(l2)) + M(l2-2)
   ENDIF
   GOTO ret
 4500 Km = 0
   Kn = 0
   GOTO 500
 4600 Kn = 1
   Km = Km + 1
   GOTO 500
 4700 IF ( Mf(1)/=1 .OR. Mf(2)/=3 .OR. Mf(3)/=3 ) Badfor = .TRUE.
   DO l = 4 , 8
      IF ( Mf(l)/=0 ) Badfor = .TRUE.
   ENDDO
   IF ( M(1)<2 .OR. M(2)/=bcds .AND. M(2)/=bcda .OR. M(4)/=bcds .AND. M(4)/=bcda ) Baddat = .TRUE.
   IF ( mod(M(1),2)/=0 ) Baddat = .TRUE.
   IF ( M(2)==bcds ) M(2) = +1
   IF ( M(2)==bcda ) M(2) = -1
   IF ( M(4)==bcds ) M(3) = +1
   IF ( M(4)==bcda ) M(3) = -1
   N = 3
   GOTO 400
 4800 IF ( npts<2 ) GOTO 300
   DO lll = l , 49
      M(lll) = 0
   ENDDO
   DO l = 1 , 3
      I(l) = M(l)
   ENDDO
   I(4) = npts
   DO l = 4 , 48
      I(l+1) = M(l)
   ENDDO
   N = 49
   GOTO 500
99008 FORMAT (A23,' 5004, FLUID POINT ID ON CFLUID OR RINGFL CARD ','EXCEEDS 999999 LIMIT')
!
END SUBROUTINE ifs4p
