
SUBROUTINE ifs5p(*,*,*)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Abort , Baddat , Badfor
   INTEGER C , Gc(7) , I(100) , Iaero , Iax , Iaxf , Icont , Id , Ipopt , Junk(42) , K , Kb(32,2) , Kdumel(9) , Km , Kn , Knt , Kx ,&
         & Ky , Lharm , Ll(6) , M(100) , M1(100) , M1f(100) , Mf(100) , N , Nax , Naxf , Nbuf , Ncds , Nopen , Nout , Nparam , P ,  &
         & T1(2,310)
   REAL Rm(100) , Slotdf(5)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /bitpos/ Kb
   COMMON /cifs5p/ Km , C , P , Icont , Iaero , Ipopt
   COMMON /ifpdta/ Id , N , K , Kx , Ky , I , Rm , Mf , M1 , M1f , Kn , Baddat , Badfor , Nopen , Nparam , Iax , Nax , Iaxf , Naxf ,&
                 & Lharm , Knt , Slotdf , Gc , Ll
   COMMON /ifpx1 / Ncds , T1
   COMMON /system/ Nbuf , Nout , Abort , Junk , Kdumel
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
!
! Local variable declarations
!
   REAL angsin , tmag , tr1 , tr2 , tr3 , v11 , v12 , v13 , v1mag , v21 , v22 , v23 , v2mag
   INTEGER blank , ialt , iblank , idumel , ig1 , ig2 , in1 , in2 , in3 , in4 , in5 , ino , irange , iscal(4) , itype(12) , iyes ,  &
         & j , j1 , kdlh , kdx , kz , l , l1 , l2 , l3 , ldh , lz , met(4) , ml , mot(3) , ms , n1 , nb , nbpc , ncpw , ndumc ,     &
         & ndumd , ndumg , ndump , nfdh(10) , nm1 , nm2 , nmt , nsht , nss , ret , thru
   LOGICAL ifpdco
   INTEGER lshift , orf , rshift
   EXTERNAL lshift , orf , rshift
!
! End of declarations
!
!
   EQUIVALENCE (M(1),Rm(1)) , (blank,iblank)
   DATA thru/4HTHRU/
   DATA blank/1H /
   DATA iyes , ino/4HYES  , 4HNO  /
   DATA ms , ml/4HS    , 4HL   /
   DATA mot/1HZ , 1HY , 2HZY/
   DATA met/1HK , 2HPK , 2HKE , 3HINV/
   DATA nmt/4/
   DATA itype , iscal/4HFX   , 4HFY   , 4HFZ   , 4HFXE  , 4HFYE  , 4HFZE  , 4HMX   , 4HMY   , 4HMZ   , 4HMXE  , 4HMYE  , 4HMZE  ,   &
       &4HLE   , 4HFR   , 4HLEPR , 4HFRPR/
!
   IF ( K<=100 ) THEN
      IF ( K==1 .OR. K==2 .OR. K==4 .OR. K==5 .OR. K==6 .OR. K==7 .OR. K==8 .OR. K==9 .OR. K==10 .OR. K==11 .OR. K==12 .OR.         &
         & K==13 .OR. K==14 .OR. K==15 .OR. K==16 .OR. K==17 .OR. K==18 .OR. K==19 .OR. K==20 .OR. K==21 .OR. K==22 .OR. K==23 .OR. &
         & K==24 .OR. K==25 .OR. K==26 .OR. K==27 .OR. K==28 .OR. K==29 .OR. K==30 .OR. K==31 .OR. K==33 .OR. K==34 .OR. K==35 .OR. &
         & K==36 .OR. K==37 .OR. K==38 .OR. K==39 .OR. K==40 .OR. K==41 .OR. K==42 .OR. K==43 .OR. K==44 .OR. K==45 .OR. K==46 .OR. &
         & K==47 .OR. K==48 .OR. K==49 .OR. K==50 .OR. K==52 .OR. K==53 .OR. K==54 .OR. K==55 .OR. K==56 .OR. K==57 .OR. K==58 .OR. &
         & K==59 .OR. K==60 .OR. K==61 .OR. K==62 .OR. K==63 .OR. K==64 .OR. K==65 .OR. K==66 .OR. K==67 .OR. K==68 .OR. K==69 .OR. &
         & K==70 .OR. K==71 .OR. K==72 .OR. K==73 .OR. K==74 .OR. K==75 .OR. K==76 .OR. K==77 .OR. K==78 .OR. K==79 .OR. K==80 .OR. &
         & K==81 .OR. K==82 .OR. K==83 .OR. K==84 .OR. K==85 .OR. K==86 .OR. K==87 .OR. K==89 .OR. K==90 .OR. K==91 .OR. K==92 .OR. &
         & K==93 .OR. K==94 .OR. K==95 .OR. K==96 .OR. K==97 .OR. K==98 ) GOTO 200
      IF ( K==3 ) THEN
!
!*****         3-ADUM1        ******************************************
!
         idumel = 1
      ELSEIF ( K==32 ) THEN
!
!*****         32-ADUM2       ******************************************
!
         idumel = 2
      ELSEIF ( K==51 ) THEN
!
!*****         51-ADUM3       ******************************************
!
         idumel = 3
      ELSEIF ( K==88 ) THEN
!
!*****         88-ADUM4       ******************************************
!
         idumel = 4
      ELSEIF ( K==99 ) THEN
!
!*****         99-ADUM5       ******************************************
!
         idumel = 5
      ELSEIF ( K==100 ) THEN
!
!*****         100-ADUM6      ******************************************
!
         idumel = 6
      ELSE
         GOTO 100
      ENDIF
      GOTO 3700
   ENDIF
 100  IF ( Kx<=100 ) THEN
      IF ( Kx==1 ) THEN
!
!*****         101-ADUM7      ******************************************
!
         idumel = 7
         GOTO 3700
      ELSEIF ( Kx==2 .OR. Kx==4 .OR. Kx==5 .OR. Kx==19 .OR. Kx==20 .OR. Kx==21 .OR. Kx==22 .OR. Kx==23 .OR. Kx==24 .OR. Kx==25 .OR. &
             & Kx==26 .OR. Kx==27 .OR. Kx==28 .OR. Kx==29 .OR. Kx==30 .OR. Kx==31 .OR. Kx==32 .OR. Kx==33 .OR. Kx==34 .OR.          &
             & Kx==35 .OR. Kx==36 .OR. Kx==37 .OR. Kx==38 .OR. Kx==39 .OR. Kx==40 .OR. Kx==41 .OR. Kx==42 .OR. Kx==43 .OR.          &
             & Kx==44 .OR. Kx==45 .OR. Kx==46 .OR. Kx==47 .OR. Kx==48 .OR. Kx==49 .OR. Kx==50 .OR. Kx==51 .OR. Kx==52 .OR.          &
             & Kx==53 .OR. Kx==54 .OR. Kx==55 .OR. Kx==56 .OR. Kx==57 .OR. Kx==58 .OR. Kx==62 .OR. Kx==66 .OR. Kx==79 .OR.          &
             & Kx==80 .OR. Kx==81 .OR. Kx==82 .OR. Kx==83 .OR. Kx==84 .OR. Kx==85 .OR. Kx==88 .OR. Kx==89 .OR. Kx==90 .OR.          &
             & Kx==91 .OR. Kx==92 .OR. Kx==95 .OR. Kx==96 .OR. Kx==97 .OR. Kx==99 .OR. Kx==100 ) THEN
         GOTO 200
      ELSEIF ( Kx==3 ) THEN
!
!*****         103-ADUM8      ******************************************
!
         idumel = 8
         GOTO 3700
      ELSEIF ( Kx==6 ) THEN
!
!*****         106-ADUM9      ******************************************
!
         idumel = 9
         GOTO 3700
      ELSEIF ( Kx==7 ) THEN
!
!*****         107-CDUM1      ******************************************
!
         idumel = 1
!
!     ******************************************************************
!
!     PROCESS CDUM-I CARDS.
!
!
!     ==============
!     ONLY DO THIS FOR FIRST ONE IF I CAN FIGURE OUT HOW
!
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==8 ) THEN
!
!*****         108-CDUM2      ******************************************
!
         idumel = 2
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==9 ) THEN
!
!*****         109-CDUM3      ******************************************
!
         idumel = 3
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==10 ) THEN
!
!*****         110-CDUM4      ******************************************
!
         idumel = 4
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==11 ) THEN
!
!*****         111-CDUM5      ******************************************
!
         idumel = 5
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==12 ) THEN
!
!*****         112-CDUM6      ******************************************
!
         idumel = 6
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==13 ) THEN
!
!*****         113-CDUM7      ******************************************
!
         idumel = 7
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==14 ) THEN
!
!*****         114-CDUM8      ******************************************
!
         idumel = 8
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==15 ) THEN
!
!*****         115-CDUM9      ******************************************
!
         idumel = 9
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( Kx==16 ) THEN
!
!*****         116-PDUM1      ******************************************
!
         idumel = 1
!
!     ******************************************************************
!
!     PROCESS PDUM-I CARDS.
!
!
!     ==============
!     ONLY DO THIS FOR FIRST ONE IF I CAN FIGURE OUT HOW
!
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==17 ) THEN
!
!*****         117-PDUM2      ******************************************
!
         idumel = 2
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==18 ) THEN
!
!*****         118-PDUM3      ******************************************
!
         idumel = 3
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==59 ) THEN
!
!*****         159-PDUM4      ******************************************
!
         idumel = 4
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==60 ) THEN
!
!*****         160-PDUM5      ******************************************
!
         idumel = 5
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==61 ) THEN
!
!*****         161-PDUM6      ******************************************
!
         idumel = 6
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==63 ) THEN
!
!*****         163-PDUM7      ******************************************
!
         idumel = 7
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==64 ) THEN
!
!*****         164-PDUM8      ******************************************
!
         idumel = 8
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==65 ) THEN
!
!*****         165-PDUM9      ******************************************
!
         idumel = 9
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( Kx==67 ) THEN
!
!*****         167-CONCT1     ******************************************
!
         IF ( Km==1 ) THEN
            Km = 0
            DO l = 1 , 8
               IF ( Mf(l)>1 ) GOTO 300
               IF ( M(l)<=0 .AND. Mf(l)==1 ) GOTO 400
            ENDDO
            DO l = 2 , 8
               IF ( Mf(l)==1 .AND. nfdh(l-1)==0 ) GOTO 400
            ENDDO
            I(1) = M(1)
            N = 1
            DO l = 2 , 8
               IF ( nfdh(l-1)/=0 ) THEN
                  N = N + 1
                  I(N) = M(l)
               ENDIF
            ENDDO
            Kn = 1
            Km = 1
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
               Kn = 0
               Km = 0
               N = N + 1
               I(N) = -1
            ENDIF
            GOTO 600
         ELSE
            nss = 0
            IF ( Mf(1)/=1 ) GOTO 300
            DO l = 2 , 8
               IF ( Mf(l)/=3 .AND. Mf(l)/=0 ) GOTO 300
               IF ( Mf(l)==3 ) nss = nss + 1
            ENDDO
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 300
            IF ( M(1)<=0 ) GOTO 400
            IF ( nss==1 ) GOTO 400
            I(1) = nss
            I(2) = M(1)
            N = 2
            nb = 0
            DO l = 2 , 8
               IF ( Mf(l)==0 ) THEN
                  nb = nb + 1
                  nfdh(l-1) = 0
               ELSE
                  N = N + 2
                  nfdh(l-1) = 1
                  I(N-1) = M(N-2+nb)
                  I(N) = M(N-1+nb)
               ENDIF
            ENDDO
            Km = 1
            GOTO 600
         ENDIF
      ELSEIF ( Kx==68 ) THEN
!
!*****         168-CONCT      ******************************************
!
         IF ( Km==1 ) THEN
            DO l = 1 , 8
               IF ( Mf(l)/=0 .AND. Mf(l)/=1 ) GOTO 300
            ENDDO
            DO l = 1 , 8
               IF ( Mf(l)==1 .AND. M(l)<=0 ) GOTO 400
            ENDDO
            N = 0
            DO l = 1 , 8 , 2
               kdlh = Mf(l) + Mf(l+1)
               IF ( kdlh/=0 .AND. kdlh/=2 ) GOTO 400
               IF ( kdlh/=0 ) THEN
                  N = N + 2
                  I(N-1) = M(N-1)
                  I(N) = M(N)
               ENDIF
            ENDDO
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
               N = N + 2
               I(N-1) = -1
               I(N) = -1
               Km = 0
            ENDIF
            GOTO 600
         ELSE
            Km = 1
            DO l = 1 , 2
               IF ( Mf(l)/=1 ) GOTO 300
               IF ( Mf(l+2)/=3 ) GOTO 300
               IF ( M(l)<=0 ) GOTO 400
            ENDDO
            DO l = 1 , 6
               I(l) = M(l)
            ENDDO
            N = 6
            IF ( M1(1)==0 .OR. M1(2)==0 ) GOTO 600
            GOTO 300
         ENDIF
      ELSEIF ( Kx==69 ) THEN
!
!*****         169-TRANS      ******************************************
!
         IF ( Mf(1)/=1 .OR. Mf(2)/=0 ) GOTO 300
         DO l = 3 , 11
            IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) GOTO 300
         ENDDO
         IF ( M(1)<=0 ) GOTO 400
         v11 = Rm(6) - Rm(3)
         v12 = Rm(7) - Rm(4)
         v13 = Rm(8) - Rm(5)
         v21 = Rm(9) - Rm(3)
         v22 = Rm(10) - Rm(4)
         v23 = Rm(11) - Rm(5)
         tr1 = v12*v23 - v13*v22
         tr2 = v11*v23 - v13*v21
         tr3 = v11*v22 - v12*v21
         tmag = sqrt(tr1**2+tr2**2+tr3**2)
         v1mag = sqrt(v11**2+v12**2+v13**2)
         v2mag = sqrt(v21**2+v22**2+v23**2)
         IF ( v1mag==0.0 ) GOTO 400
         IF ( v2mag==0.0 ) GOTO 400
         angsin = tmag/v1mag/v2mag
         IF ( angsin<0.087 ) GOTO 400
         I(1) = M(1)
         DO l = 3 , 11
            I(l-1) = M(l)
         ENDDO
         N = 10
         GOTO 600
      ELSEIF ( Kx==70 ) THEN
         GOTO 800
      ELSEIF ( Kx==71 ) THEN
!
!*****         171-LOADC      ******************************************
!
         IF ( Km==1 ) THEN
            N = 0
            ldh = 2
            GOTO 900
         ELSE
            Km = 1
            IF ( (Mf(1)/=0 .AND. Mf(1)/=1) .OR. (Mf(2)/=0 .AND. Mf(2)/=2) ) GOTO 300
            IF ( M(1)<=0 .OR. M(2)==0 ) GOTO 400
            IF ( Mf(3)/=3 .OR. (Mf(6)/=3 .AND. Mf(6)/=0) ) GOTO 300
            I(1) = M(1)
            I(2) = M(2)
            N = 2
            ldh = 0
            GOTO 900
         ENDIF
      ELSEIF ( Kx==72 ) THEN
         GOTO 1000
      ELSEIF ( Kx==73 ) THEN
!
!*****         173-SPCS1      ******************************************
!
         IF ( Km==1 ) THEN
            l1 = 1
!
!*****         174-SPCS       ******************************************
!
!
!     SAME AS RELES DATA CARD
!
            j1 = 1
         ELSE
            Km = 1
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            IF ( Mf(2)/=3 ) Badfor = .TRUE.
            IF ( M(4)<0 ) Baddat = .TRUE.
            CALL write(210,M,4,0)
            j1 = 4
            l1 = 5
         ENDIF
         GOTO 1200
      ELSEIF ( Kx==74 ) THEN
         GOTO 800
      ELSEIF ( Kx==75 ) THEN
!
!*****         175-BDYC       ******************************************
         IF ( Km==1 ) THEN
            IF ( Mf(1)==0 .AND. Mf(8)==0 ) GOTO 1100
            GOTO 300
         ELSE
!
            IF ( Mf(8)/=0 .OR. Mf(1)/=1 ) GOTO 300
            IF ( M(1)>0 ) GOTO 1100
            GOTO 400
         ENDIF
      ELSEIF ( Kx==76 ) THEN
!
!*****         176-MPCS       ******************************************
!
         IF ( Km==1 ) THEN
            IF ( Mf(1)/=0 ) GOTO 300
            IF ( Mf(2)/=3 ) GOTO 300
            DO l = 3 , 6 , 3
               IF ( Mf(l)+Mf(l+2)+Mf(l+1)/=0 ) THEN
                  IF ( Mf(l)/=1 .OR. Mf(l+1)/=1 ) GOTO 300
                  IF ( Mf(l+2)/=2 ) GOTO 300
                  IF ( M(l+1)<=0 .AND. Mf(l+2)<=0 ) GOTO 400
               ENDIF
            ENDDO
            N = 0
            DO l = 3 , 8 , 3
               kdlh = Mf(l) + Mf(l+1) + Mf(l+2)
               IF ( kdlh/=0 .AND. kdlh/=4 ) GOTO 400
               IF ( kdlh/=0 ) THEN
                  I(N+1) = M(2)
                  I(N+2) = M(3)
                  N = N + 5
                  I(N-2) = M(l+1)
                  I(N-1) = M(l+2)
                  I(N) = M(l+3)
               ENDIF
            ENDDO
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
               I(N+1) = iblank
               I(N+2) = iblank
               N = N + 5
               I(N-2) = -1
               I(N-1) = -1
               I(N) = -1
               Km = 0
            ENDIF
            GOTO 600
         ELSE
            Km = 1
            IF ( Mf(1)/=1 ) GOTO 300
            IF ( Mf(2)/=3 ) GOTO 300
            IF ( Mf(3)/=1 ) GOTO 300
            IF ( Mf(4)/=1 ) GOTO 300
            IF ( Mf(5)/=2 ) GOTO 300
            IF ( M(1)<=0 ) GOTO 400
            IF ( M(4)<=0 ) GOTO 400
            IF ( M(5)<0 ) GOTO 400
            IF ( M(6)==0 ) GOTO 400
            DO l = 1 , 6
               I(l) = M(l)
            ENDDO
            N = 6
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 300
            GOTO 600
         ENDIF
      ELSEIF ( Kx==77 ) THEN
!
!*****         177-BDYS       ******************************************
!
         DO l = 1 , 7
            IF ( Mf(l)/=1 .AND. Mf(l)/=0 ) GOTO 300
            IF ( Mf(l)==1 .AND. M(l)<=0 ) GOTO 400
         ENDDO
         IF ( Mf(1)==0 ) GOTO 300
         N = 1
         I(N) = M(1)
         DO l = 2 , 7 , 2
            kdlh = Mf(l) + Mf(l+1)
            IF ( kdlh/=2 .AND. kdlh/=0 ) GOTO 400
            IF ( kdlh/=0 ) THEN
               N = N + 2
               I(N-1) = M(N-1)
               I(N) = M(N)
            ENDIF
         ENDDO
         N = N + 2
         I(N-1) = -1
         I(N) = -1
         GOTO 600
      ELSEIF ( Kx==78 ) THEN
!
!*****         178-BDYS1      ******************************************
!
         IF ( Km==1 ) THEN
            j1 = 1
            l1 = 1
         ELSE
            Km = 1
            IF ( Mf(1)/=1 .OR. Mf(2)>1 ) Badfor = .TRUE.
            IF ( M(1)<1 .OR. M(2)<0 ) Baddat = .TRUE.
            CALL write(210,M,2,0)
            j1 = 3
            l1 = 3
         ENDIF
         GOTO 1200
      ELSEIF ( Kx==86 ) THEN
!
!*****         186-GNEW       ******************************************
!
         IF ( Mf(1)/=1 ) GOTO 300
         IF ( Mf(2)/=3 ) GOTO 300
         IF ( Mf(3)/=1 .AND. Mf(3)/=0 ) GOTO 300
         IF ( Mf(4)/=1 ) GOTO 300
         IF ( Mf(5)/=1 ) GOTO 300
         IF ( M(1)<=0 ) GOTO 400
         IF ( M(4)<0 ) GOTO 400
         IF ( M(5)<=0 ) GOTO 400
         IF ( M(6)<=0 ) GOTO 400
         N = 6
         GOTO 500
      ELSEIF ( Kx==87 ) THEN
!
!*****         187-GTRAN      ******************************************
!
         IF ( Mf(1)/=1 ) GOTO 300
         IF ( Mf(2)/=3 ) GOTO 300
         IF ( Mf(3)/=1 ) GOTO 300
         IF ( Mf(4)/=1 .AND. Mf(4)/=0 ) GOTO 300
         IF ( M(1)<=0 ) GOTO 400
         IF ( M(4)<=0 ) GOTO 400
         IF ( M(5)<0 ) GOTO 400
         N = 5
         GOTO 500
      ELSEIF ( Kx==93 ) THEN
!
!*****         193-USET       ******************************************
!
         ASSIGN 1700 TO ret
         GOTO 1500
      ELSEIF ( Kx==94 ) THEN
!
!*****         194-USET1      ******************************************
!
         IF ( Km/=0 ) THEN
            l1 = 1
            l3 = 0
            l2 = 8
            GOTO 1900
         ELSE
            Km = 1
            ASSIGN 1800 TO ret
            GOTO 1500
         ENDIF
      ELSEIF ( Kx==98 ) THEN
!
!*****         198-PLOAD1       ****************************************
!
         IF ( M(1)>0 .AND. M(2)>0 ) THEN
            I(1) = M(1)
            I(2) = M(2)
            DO l = 1 , 12
               IF ( M(3)==itype(l) ) GOTO 3300
            ENDDO
         ENDIF
         GOTO 400
      ENDIF
   ENDIF
   IF ( Ky<=100 ) THEN
      IF ( Ky==1 .OR. Ky==2 .OR. Ky==3 .OR. Ky==4 .OR. Ky==5 .OR. Ky==6 .OR. Ky==7 .OR. Ky==8 .OR. Ky==9 .OR. Ky==10 .OR.           &
         & Ky==11 .OR. Ky==12 .OR. Ky==13 .OR. Ky==14 .OR. Ky==15 .OR. Ky==16 .OR. Ky==17 .OR. Ky==18 .OR. Ky==19 .OR. Ky==20 .OR.  &
         & Ky==21 .OR. Ky==22 .OR. Ky==23 .OR. Ky==24 .OR. Ky==25 .OR. Ky==26 .OR. Ky==27 .OR. Ky==28 .OR. Ky==29 .OR. Ky==30 .OR.  &
         & Ky==31 .OR. Ky==32 .OR. Ky==33 .OR. Ky==34 .OR. Ky==35 .OR. Ky==36 .OR. Ky==37 .OR. Ky==38 .OR. Ky==39 .OR. Ky==40 .OR.  &
         & Ky==41 .OR. Ky==42 .OR. Ky==43 .OR. Ky==44 .OR. Ky==47 .OR. Ky==48 .OR. Ky==56 .OR. Ky==57 .OR. Ky==58 .OR. Ky==59 .OR.  &
         & Ky==60 .OR. Ky==61 .OR. Ky==62 .OR. Ky==68 .OR. Ky==73 .OR. Ky==74 .OR. Ky==79 .OR. Ky==80 .OR. Ky==81 .OR. Ky==82 .OR.  &
         & Ky==83 .OR. Ky==84 .OR. Ky==85 .OR. Ky==86 .OR. Ky==87 .OR. Ky==88 .OR. Ky==89 .OR. Ky==90 .OR. Ky==91 .OR. Ky==92 .OR.  &
         & Ky==93 .OR. Ky==94 .OR. Ky==95 .OR. Ky==96 .OR. Ky==97 .OR. Ky==98 .OR. Ky==99 .OR. Ky==100 ) GOTO 200
      IF ( Ky==45 .OR. Ky==46 ) THEN
!
!*****         245-SAME 246-NOSAME          ****************************
!
         ialt = 1
         IF ( M(3)==thru ) ialt = 3
         kdx = ialt + Icont
         IF ( kdx==3 ) THEN
!
            IF ( Mf(1)/=1 .OR. Mf(2)/=1 .OR. Mf(3)/=3 .OR. Mf(4)/=1 .OR. Mf(5)/=1 .OR. Mf(6)/=1 .OR. Mf(7)/=3 .OR. Mf(8)/=1 )       &
               & Badfor = .TRUE.
            IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(5)<=0 .OR. M(6)<=0 .OR. M(7)<=0 .OR. M(8)/=thru .OR. M(10)<=0 ) Baddat = .TRUE.
            IF ( M(5)<=M(2) .OR. M(10)<=M(7) ) Baddat = .TRUE.
            irange = M(5) - M(2)
            IF ( (M(10)-M(7))/=irange ) Baddat = .TRUE.
            I(1) = -1
            I(2) = irange + 1
            I(3) = M(1)
            I(4) = M(2)
            I(5) = M(6)
            I(6) = M(7)
            N = 6
         ELSEIF ( kdx==4 ) THEN
            DO in1 = 1 , 6 , 5
               in2 = in1 + 1
               in3 = in2 + 1
               in4 = in3 + 1
               in5 = in4 + 1
               IF ( Mf(in1)/=0 .OR. Mf(in2)/=0 .OR. Mf(in3)/=0 .OR. Mf(in4)/=0 ) THEN
                  IF ( Mf(in1)/=1 .OR. Mf(in2)/=1 .OR. Mf(in3)/=3 .OR. Mf(in4)/=1 ) Badfor = .TRUE.
                  IF ( M(in1)<=0 .OR. M(in2)<=0 .OR. M(in3)/=thru .OR. M(in5)<=0 ) Baddat = .TRUE.
                  IF ( M(in5)<=M(in2) .OR. M(in5)-M(in2)/=irange ) Baddat = .TRUE.
                  I(N+1) = M(in1)
                  I(N+2) = M(in2)
                  N = N + 2
               ENDIF
            ENDDO
         ELSE
            DO in1 = 1 , 8 , 2
               in2 = in1 + 1
               IF ( Mf(in1)/=0 .OR. Mf(in2)/=0 ) THEN
                  IF ( Mf(in1)/=1 .OR. Mf(in2)/=1 ) Badfor = .TRUE.
                  IF ( M(in1)<=0 .OR. M(in2)<=0 ) Baddat = .TRUE.
!
                  N = N + 2
                  I(N-1) = M(in1)
                  I(N) = M(in2)
               ENDIF
            ENDDO
         ENDIF
!
         IF ( M1f(1)==0 .AND. M1f(2)==0 ) THEN
            Icont = 1
         ELSE
            Icont = 0
            I(N+1) = -1
            I(N+2) = -1
            N = N + 2
         ENDIF
         GOTO 700
      ELSEIF ( Ky==49 ) THEN
         GOTO 300
      ELSEIF ( Ky==50 ) THEN
         GOTO 400
      ELSEIF ( Ky==51 ) THEN
!
!*****         251-CIHEX1     ******************************************
!
         N = 10
         GOTO 2200
      ELSEIF ( Ky==52 ) THEN
!
!*****         252-CIHEX2     ******************************************
!
         N = 22
         GOTO 2200
      ELSEIF ( Ky==53 ) THEN
!
!*****         253-CIHEX3     ******************************************
!
         N = 34
         GOTO 2200
      ELSEIF ( Ky==54 ) THEN
!
!*****         254-PIHEX      ******************************************
!
         N = 7
         IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 400
         IF ( M(3)<0 ) GOTO 400
         IF ( (M(4)<2 .OR. M(4)>4) .AND. M(4)/=0 ) GOTO 400
         DO l = 5 , 7
            IF ( Mf(l)==0 ) THEN
               Rm(l) = -1.0
            ELSE
               IF ( Mf(l)/=2 ) GOTO 300
               IF ( Rm(l)<0.0 ) GOTO 400
            ENDIF
         ENDDO
         IF ( Rm(5)>=0.0 .AND. Rm(5)<1.0 ) GOTO 400
         IF ( Rm(6)<=180.0 .AND. Rm(7)<=180.0 ) GOTO 500
         GOTO 400
      ELSEIF ( Ky==55 ) THEN
!
!*****         255-PLOAD3     ******************************************
!
         IF ( M(1)<=0 ) GOTO 400
         DO l = 3 , 6 , 3
            IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 ) THEN
               IF ( M(l)<0 .OR. M(l+1)<0 .OR. M(l+2)<0 ) GOTO 400
               N = N + 5
               I(N-4) = M(1)
               I(N-3) = M(2)
               I(N-2) = M(l)
               I(N-1) = M(l+1)
               I(N) = M(l+2)
            ENDIF
         ENDDO
         IF ( N>0 ) GOTO 600
         GOTO 400
      ELSEIF ( Ky==63 ) THEN
         GOTO 2300
      ELSEIF ( Ky==64 ) THEN
!
!*****         264-PAERO1     ******************************************
!
         IF ( M(1)<=0 ) GOTO 400
         DO l = 2 , 8
            IF ( M(l)<0 ) GOTO 400
         ENDDO
         N = 8
         GOTO 500
      ELSEIF ( Ky==65 ) THEN
!
!*****         265-AERO       ******************************************
!
         IF ( Iaero/=0 ) GOTO 400
         Iaero = 1
         IF ( M(1)<0 ) GOTO 400
         N = 6
         GOTO 500
      ELSEIF ( Ky==66 ) THEN
!
!*****         266-SPLINE1    ******************************************
!
         IF ( M(2)<=0 .OR. M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 .OR. M(1)<=0 .OR. Rm(6)<0.0 ) GOTO 400
         N = 6
         GOTO 500
      ELSEIF ( Ky==67 ) THEN
!
!*****         267-SPLINE2    ******************************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 .OR. M(8)<0 ) GOTO 400
         N = 10
         GOTO 500
      ELSEIF ( Ky==69 ) THEN
!
!*****         269-SET2       ******************************************
!
         IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 400
         N = 8
         GOTO 500
      ELSEIF ( Ky==70 ) THEN
!
!*****         270-MKAERO2    ******************************************
!
         N = 0
         DO l = 2 , 8 , 2
            IF ( Mf(l)/=0 .OR. Mf(l-1)/=0 ) THEN
               IF ( Mf(l)==0 .OR. Mf(l-1)==0 ) GOTO 300
               N = N + 2
               I(N-1) = M(l-1)
               IF ( Rm(l)<=0.0 ) GOTO 400
               I(N) = M(l)
            ENDIF
         ENDDO
         IF ( N/=0 ) GOTO 600
         GOTO 400
      ELSEIF ( Ky==71 ) THEN
!
!*****         271-MKAERO1    ******************************************
!
         IF ( Mf(1)/=2 .OR. Mf(9)/=2 ) GOTO 300
         IF ( Rm(9)<=0.0 ) GOTO 400
         DO l = 2 , 8
            IF ( Mf(l)==0 ) M(l) = -1
            IF ( Mf(l+8)/=0 .AND. Rm(l+8)<=0.0 ) GOTO 400
            IF ( Mf(l+8)==0 ) M(l+8) = -1
         ENDDO
         N = 16
         GOTO 500
      ELSEIF ( Ky==72 ) THEN
!
!*****         257-FLUTTER    ******************************************
!
         IF ( M(1)>0 .AND. M(4)>=0 .AND. M(5)>=0 .AND. M(6)>=0 ) THEN
            DO l = 1 , nmt
               IF ( M(2)==met(l) ) GOTO 3200
            ENDDO
         ENDIF
         GOTO 400
      ELSEIF ( Ky==75 ) THEN
!
!*****         275-CBARAO       ****************************************
!
         IF ( M(1)>0 ) THEN
            I(1) = M(1)
            DO l = 1 , 2
               IF ( M(2)==iscal(l) ) GOTO 3500
            ENDDO
         ENDIF
         GOTO 400
      ELSEIF ( Ky==76 ) THEN
!
!*****         276-PLIMIT       ****************************************
!
         IF ( Mf(1)/=3 ) GOTO 300
         IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) GOTO 300
         IF ( Rm(3)<0.0 ) GOTO 400
         IF ( Rm(3)==0.0 .AND. Rm(4)==0.0 ) GOTO 400
         IF ( Rm(4)==0.0 ) GOTO 3600
         IF ( Mf(3)==2 .AND. Rm(4)>Rm(3) ) GOTO 3600
         GOTO 400
      ELSEIF ( Ky==77 ) THEN
!
!*****         277-POPT         ****************************************
!
         IF ( M(1)<=0 .OR. M(4)==0 ) GOTO 400
         IF ( Ipopt/=0 ) GOTO 400
         Ipopt = 1
         IF ( Rm(2)<0.0 ) GOTO 400
         IF ( Rm(3)<=0.0 ) GOTO 400
         IF ( M(5)/=iyes .AND. M(5)/=ino ) GOTO 400
         N = 6
         GOTO 500
      ELSEIF ( Ky==78 ) THEN
!
!******       278  PLOADX   ******************************************
!
         IF ( M(1)<=0 ) GOTO 400
         IF ( M(4)<=0 .OR. M(5)<=0 .OR. M(6)<=0 ) GOTO 400
         N = 6
         GOTO 500
      ENDIF
   ENDIF
   kz = Ky - 100
   IF ( kz<=39 ) THEN
      IF ( kz==1 .OR. kz==2 .OR. kz==3 .OR. kz==9 ) GOTO 2300
      IF ( kz==4 ) THEN
!
!*****      304 - PAERO2    ***************
!
         IF ( M(1)>0 ) THEN
            DO l = 1 , 3
               IF ( M(2)==mot(l) ) GOTO 2600
            ENDDO
         ENDIF
         GOTO 400
      ELSEIF ( kz==5 ) THEN
!
!*****      305 - PAERO3    ****************
!
         IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 ) GOTO 400
         IF ( M(2)>50 ) GOTO 400
         N = 0
         IF ( M(3)==0 ) N = 4
         IF ( M(3)==1 ) N = 12
         IF ( M(3)==2 ) N = 16
         IF ( N==0 ) GOTO 400
         M(4) = N
         N = N + 4
         IF ( N==8 ) GOTO 500
         DO l = 9 , N
            IF ( Mf(l)==-32767 ) GOTO 400
         ENDDO
         IF ( Rm(12)<Rm(10) ) GOTO 400
         IF ( Rm(16)<Rm(14) ) GOTO 400
         IF ( N==16 ) GOTO 500
         IF ( Rm(20)>=Rm(18) ) GOTO 500
         GOTO 400
      ELSEIF ( kz==6 ) THEN
!
!*****      306 - PAERO4   **********************
!
         IF ( Km/=0 ) THEN
            l1 = 1
            GOTO 2700
         ELSE
            Km = 1
            IF ( Mf(1)/=1 .OR. M(1)<=0 ) GOTO 2900
            DO l = 2 , 5
               IF ( Mf(1)<0 .OR. Mf(l)>1 ) GOTO 2900
            ENDDO
            IF ( M(3)<0 ) GOTO 2900
            IF ( M(2)==0 .AND. M(3)/=0 ) GOTO 2900
            IF ( M(2)>0 .AND. M(3)==0 ) GOTO 2900
            IF ( M(2)/=0 .AND. M(4)/=0 ) GOTO 2900
            IF ( M(4)<0 .OR. M(4)>3 ) GOTO 2900
            IF ( M(4)==0 .AND. M(5)/=0 ) GOTO 2900
            IF ( M(4)>0 .AND. M(5)==0 ) GOTO 2900
            DO l = 1 , 5
               I(l) = M(l)
            ENDDO
            N = 5
            l1 = 6
            GOTO 2700
         ENDIF
      ELSEIF ( kz==7 ) THEN
!
!*****    307 - SPLINE3       *********************
!
         IF ( Km/=0 ) THEN
            l1 = 1
            GOTO 3100
         ELSE
            Km = 1
            IF ( Mf(1)/=1 .OR. Mf(2)/=1 .OR. Mf(3)/=1 .OR. Mf(4)/=1 ) GOTO 2900
            IF ( M(2)<=0 .OR. M(3)<0 ) GOTO 2900
            IF ( ifpdco(M(4)) ) GOTO 2900
            IF ( Gc(2)/=0 ) GOTO 2900
            DO l = 1 , 4
               I(l) = M(l)
            ENDDO
            N = 4
            l1 = 5
            GOTO 3100
         ENDIF
      ELSEIF ( kz==8 ) THEN
!
!******    308 - GUST
!
         IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 400
         IF ( Rm(3)==0.0 .OR. Rm(5)==0.0 ) GOTO 400
         N = 5
         GOTO 500
      ELSEIF ( kz==10 ) THEN
!
!*****   310 - PAERO5  ************
!
         IF ( Km/=0 ) THEN
            l1 = 1
            GOTO 2700
         ELSE
            Km = 1
            DO l = 1 , 3
               IF ( Mf(l)/=1 .OR. M(l)<=0 ) GOTO 2900
            ENDDO
            DO l = 4 , 7
               IF ( Mf(l)<0 .OR. Mf(l)>1 ) GOTO 2900
            ENDDO
            IF ( M(4)/=0 .AND. M(5)==0 ) GOTO 2900
            IF ( M(6)/=0 .AND. M(7)==0 ) GOTO 2900
            DO l = 1 , 7
               I(l) = M(l)
            ENDDO
            N = 7
            GOTO 2800
         ENDIF
      ELSEIF ( kz==11 .OR. kz==12 .OR. kz==13 ) THEN
         GOTO 1000
      ELSEIF ( kz==14 ) THEN
!
!*****         314-TICS         **************************************
!
         IF ( M(1)<=0 ) GOTO 400
         IF ( M(4)<=0 ) GOTO 400
         IF ( M(5)<0 ) GOTO 400
         DO l = 8 , 11
            M(l) = -1
         ENDDO
         N = 11
         GOTO 500
      ELSEIF ( kz==38 ) THEN
!
!*****         338-CELBOW     ******************************************
!
         IF ( M(2)==0 ) M(2) = M(1)
         N = 8
         GOTO 500
      ELSEIF ( kz==39 ) THEN
!
!*****         339-PELBOW     ******************************************
!
         N = 24
         GOTO 500
      ENDIF
   ENDIF
 200  CALL page2(2)
   WRITE (Nout,99001) Sfm
99001 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS5P.')
   Abort = .TRUE.
   IF ( K==0 ) GOTO 99999
   RETURN 1
 300  Badfor = .TRUE.
   RETURN 1
 400  Baddat = .TRUE.
   RETURN 1
 500  DO l = 1 , N
      I(l) = M(l)
   ENDDO
 600  RETURN
 700  RETURN 3
!
!*****         170-RELES      ******************************************
!
 800  IF ( Km==1 ) THEN
      N = 0
      l1 = 1
   ELSE
      Km = 1
      IF ( Mf(1)/=1 ) GOTO 300
      IF ( Mf(2)/=3 ) GOTO 300
      IF ( M(1)<=0 ) GOTO 400
      I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      l1 = 3
      N = 3
   ENDIF
   DO l = l1 , 8 , 2
      kdlh = Mf(l) + Mf(l+1)
      IF ( kdlh/=0 .AND. kdlh/=2 ) GOTO 400
      IF ( kdlh/=0 ) THEN
         N = N + 2
         I(N-1) = M(N-1)
         I(N) = M(N)
      ENDIF
   ENDDO
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      N = N + 2
      I(N-1) = -1
      I(N) = -1
      Km = 0
   ENDIF
   GOTO 600
 900  DO l = 3 , 8 , 3
      kdlh = Mf(l) + Mf(l+1) + Mf(l+2)
      IF ( kdlh/=0 .AND. kdlh/=6 ) GOTO 400
      IF ( kdlh/=0 ) THEN
         N = N + 4
         I(N-3) = M(N-3+ldh)
         I(N-2) = M(N-2+ldh)
         I(N-1) = M(N-1+ldh)
         I(N) = M(N+ldh)
      ENDIF
   ENDDO
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      N = N + 4
      I(N-3) = iblank
      I(N-2) = iblank
      I(N-1) = -1
      I(N) = -1
      Km = 0
   ENDIF
   GOTO 600
!
!*****         172-SPCSD ,  311-DAREAS          **********************
!              312-DELAYS,  313-DPHASES
!
 1000 IF ( M(1)<=0 ) GOTO 400
   IF ( M(4)<=0 ) GOTO 400
   IF ( M(5)<0 ) GOTO 400
   IF ( M(7)<0 ) GOTO 400
   IF ( M(8)<0 ) GOTO 400
   N = 12
   IF ( M(7)==0 ) N = 9
   M(N-2) = -1
   M(N-1) = -1
   M(N) = -1
   GOTO 500
 1100 DO l = 2 , 7 , 2
      IF ( Mf(l)/=0 .AND. Mf(l)/=3 ) GOTO 300
      IF ( Mf(l+1)/=0 .AND. Mf(l+1)/=1 ) GOTO 300
   ENDDO
   I(1) = M(1)
   N = 1
   j1 = 1
   IF ( Km==1 ) j1 = 0
   DO l = 2 , 7 , 2
      kdlh = Mf(l) + Mf(l+1)
      IF ( kdlh/=0 .AND. kdlh/=4 ) GOTO 400
      IF ( kdlh/=0 ) THEN
         N = N + 3
         j1 = j1 + 3
         I(j1-2) = M(N-2)
         I(j1-1) = M(N-1)
         I(j1) = M(N)
      ENDIF
   ENDDO
   N = j1
   Km = 1
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      Km = 0
      N = N + 3
      j1 = j1 + 3
      I(j1-2) = iblank
      I(j1-1) = iblank
      I(j1) = -1
   ENDIF
   GOTO 600
!
!     COMMON PROCESSING FOR SPCS1 AND BDYS1 CARDS
!
 1200 IF ( Mf(j1)==0 ) THEN
      j1 = j1 + 1
      l1 = l1 + 1
   ELSEIF ( Mf(j1)==1 ) THEN
      IF ( j1<=6 ) THEN
         IF ( Mf(j1+1)==3 ) THEN
            IF ( M(l1+1)/=thru ) THEN
               Baddat = .TRUE.
               GOTO 1400
            ELSEIF ( Mf(j1+2)/=1 ) THEN
               Badfor = .TRUE.
               GOTO 1400
            ELSEIF ( M(l1+3)>M(l1) ) THEN
               ig1 = M(l1)
               ig2 = M(l1+3)
               DO j = ig1 , ig2
                  CALL write(210,j,1,0)
               ENDDO
               j1 = j1 + 3
               l1 = l1 + 4
               GOTO 1300
            ELSE
               Baddat = .TRUE.
               GOTO 1400
            ENDIF
         ENDIF
      ENDIF
      CALL write(210,M(l1),1,0)
      j1 = j1 + 1
      l1 = l1 + 1
   ELSE
      Badfor = .TRUE.
      GOTO 1400
   ENDIF
 1300 IF ( j1<=8 ) GOTO 1200
 1400 IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      Km = 0
      Kn = 0
      N = 1
      I(1) = -1
   ELSE
      Kn = 1
      N = 0
   ENDIF
   GOTO 700
 1500 N = 0
   IF ( M(2)==blank ) THEN
      DO l = 1 , 32
         IF ( M(1)==Kb(l,2) ) GOTO 1600
      ENDDO
   ENDIF
   GOTO 400
 1600 Id = Kb(l,1)
   GOTO ret
 1700 DO l = 3 , 7 , 2
      IF ( M(l)/=0 .OR. M(l+1)/=0 ) THEN
         IF ( M(l)<=0 ) GOTO 400
         IF ( ifpdco(M(l+1)) ) GOTO 400
         lz = 6
         IF ( M(l+1)==0 ) lz = 1
         DO l2 = 1 , lz
            IF ( lz==1 .OR. Ll(l2)/=0 ) THEN
               N = N + 3
               I(N-2) = Id
               I(N-1) = M(l)
               I(N) = Ll(l2)
               IF ( N>3 ) THEN
                  DO l1 = 6 , N , 3
                     IF ( I(N-1)==I(l1-4) .AND. I(N)==I(l1-3) ) GOTO 400
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDDO
   IF ( N>0 ) GOTO 600
   GOTO 400
 1800 N = 2
   I(1) = Id
   IF ( Mf(2)/=0 .AND. Mf(2)/=1 ) Badfor = .TRUE.
   IF ( ifpdco(M(3)) ) Baddat = .TRUE.
   I(2) = M(3)
   IF ( Mf(4)/=3 .OR. M(5)/=thru ) THEN
      l1 = 4
      l3 = -1
      l2 = 9
   ELSEIF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      IF ( Mf(3)/=1 .OR. Mf(5)/=1 ) Badfor = .TRUE.
      IF ( M(4)<=0 .OR. M(7)<=M(4) ) Baddat = .TRUE.
      DO l = 1 , 3
         IF ( Mf(l+5)/=0 ) Badfor = .TRUE.
      ENDDO
      IF ( .NOT.(Badfor .OR. Baddat) ) THEN
         CALL write(210,I,2,0)
         l1 = M(4)
         l2 = M(7)
         DO l = l1 , l2
            CALL write(210,l,1,0)
         ENDDO
         N = 0
      ENDIF
      GOTO 2100
   ELSE
      Kn = 1
      Badfor = .TRUE.
      GOTO 700
   ENDIF
 1900 DO l = l1 , l2
      IF ( Mf(l+l3)/=0 .AND. Mf(l+l3)/=1 ) Badfor = .TRUE.
   ENDDO
   DO l = l1 , l2
      IF ( Mf(l+l3)==1 ) GOTO 2000
   ENDDO
   Baddat = .TRUE.
 2000 DO l = l1 , l2
      IF ( M(l)<0 ) THEN
         Baddat = .TRUE.
      ELSEIF ( M(l)/=0 ) THEN
         N = N + 1
         I(N) = M(l)
      ENDIF
   ENDDO
   Kn = 1
   IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 700
 2100 Km = 0
   N = N + 1
   I(N) = -1
   Kn = 0
   GOTO 700
 2200 DO l = 1 , N
      IF ( M(l)<=0 ) GOTO 400
   ENDDO
   n1 = N - 1
   DO l = 3 , n1
      l2 = l + 1
      DO l1 = l2 , N
         IF ( M(l)==M(l1) ) GOTO 400
      ENDDO
   ENDDO
   GOTO 500
!
!*****    263-CAERO1, 301-CAERO2, 302-CAERO3, 303-CAERO4  *******
!         309-CAERO5
!
 2300 IF ( M(1)<=0 ) GOTO 400
   IF ( M(2)<=0 ) GOTO 400
   DO l = 3 , 8
      IF ( M(l)<0 ) GOTO 400
   ENDDO
   IF ( K==302 ) THEN
!
!*****     CAERO3      ************************************************
!
      IF ( M(4)==0 ) GOTO 400
      IF ( Rm(12)==0. ) GOTO 400
   ELSE
      IF ( K==303 ) GOTO 2500
      IF ( K==309 ) GOTO 2500
      IF ( M(4)==0 .AND. M(6)==0 ) GOTO 400
      IF ( M(5)==0 .AND. M(7)==0 ) GOTO 400
      IF ( M(8)<=0 ) GOTO 400
   ENDIF
 2400 IF ( Rm(12)<0.0 ) GOTO 400
   IF ( Rm(16)<0.0 ) GOTO 400
   IF ( Rm(12)==0.0 .AND. Rm(16)==0.0 ) GOTO 400
   N = 16
   GOTO 500
!
!*****     CAERO4   CAERO5    ******************************************
!
 2500 IF ( M(4)==0 .AND. M(5)==0 ) GOTO 400
   IF ( M(6)<=2 ) GOTO 2400
   GOTO 400
 2600 IF ( Rm(4)<=0.0 ) GOTO 400
   IF ( Rm(5)<=0.0 ) GOTO 400
   DO l = 6 , 15
      IF ( M(l)<0 ) GOTO 400
   ENDDO
   N = 15
   GOTO 500
 2700 DO l = l1 , 8
      IF ( Mf(l)==0 ) GOTO 3000
      IF ( Mf(l)/=2 ) GOTO 2900
      IF ( Rm(l)<0. ) GOTO 2900
      N = N + 1
      I(N) = M(l)
   ENDDO
 2800 Kn = 1
   IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      Kn = 0
      Km = 0
      N = N + 1
      I(N) = -1
   ENDIF
   GOTO 700
 2900 Baddat = .TRUE.
   GOTO 2800
 3000 IF ( M1(1)==0 .AND. M1(2)==0 ) Baddat = .TRUE.
   GOTO 2800
 3100 DO l = l1 , 8 , 4
      IF ( Mf(l)/=0 ) THEN
         IF ( Mf(l)/=1 ) GOTO 2900
         IF ( Mf(l+1)/=1 ) GOTO 2900
         IF ( ifpdco(M(l+1)) ) GOTO 2900
         IF ( Gc(2)/=0 ) GOTO 2900
         IF ( Mf(l+2)/=2 ) GOTO 2900
         IF ( M(l)<=0 ) GOTO 2900
         N = N + 3
         I(N) = M(l+2)
         I(N-1) = M(l+1)
         I(N-2) = M(l)
      ENDIF
   ENDDO
   GOTO 2800
 3200 IF ( M(7)/=ms .AND. M(7)/=ml ) GOTO 400
   N = 10
   GOTO 500
 3300 I(3) = l
   DO l = 1 , 4
      IF ( M(5)==iscal(l) ) GOTO 3400
   ENDDO
   GOTO 400
 3400 I(4) = l
   IF ( Rm(9)==0.0 ) Rm(9) = Rm(7)
   IF ( Rm(9)<Rm(7) ) GOTO 400
   DO l = 7 , 10
      I(l-2) = M(l)
   ENDDO
   N = 8
   GOTO 600
 3500 I(2) = l
   DO l = 4 , 9
      I(l-1) = M(l)
   ENDDO
   N = 9
   IF ( Mf(3)==2 ) THEN
      I(9) = 1
      DO l = 4 , 9
         IF ( Rm(l)<0.0 ) GOTO 400
      ENDDO
      GOTO 600
   ELSE
      IF ( Mf(3)/=1 ) GOTO 300
      IF ( I(3)<=0 ) GOTO 400
      IF ( I(3)>20 ) I(3) = 20
      IF ( Rm(5)<=0.0 .OR. Rm(6)<=0.0 ) GOTO 400
      I(9) = -1
      GOTO 600
   ENDIF
 3600 IF ( Mf(5)==3 ) THEN
      IF ( M(6)/=thru ) GOTO 400
      IF ( Mf(4)/=1 .OR. Mf(6)/=1 ) GOTO 300
      IF ( M(8)<=M(5) ) GOTO 400
      N = 9
      GOTO 500
   ELSE
      DO l = 4 , 8
         IF ( Mf(l)/=0 .AND. Mf(l)/=1 ) GOTO 300
         IF ( M(l+1)<0 ) GOTO 400
      ENDDO
      N = 9
      GOTO 500
   ENDIF
!
!
!
!     ******************************************************************
!
!     PROCESS ADUM-I CARDS.
!
 3700 IF ( M(1)<=0 ) GOTO 400
   IF ( M(2)<0 ) GOTO 400
   IF ( M(3)<0 ) GOTO 400
   IF ( M(4)/=3 .AND. M(4)/=6 ) GOTO 400
   IF ( Mf(5)/=0 .OR. Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 300
   Kdumel(idumel) = M(4) + 10*(M(3)+1000*(M(2)+1000*M(1)))
!
!     PUT IN CONNECTION AND PROPERTY CARD NAME IF SUPPLIED BY USER
!
   IF ( Mf(5)==3 ) THEN
      nbpc = Junk(36)
      ncpw = Junk(38)
      nsht = nbpc*(ncpw-1)
      nm1 = T1(1,K)
      nm2 = T1(2,K)
      nm1 = rshift(lshift(nm1,nbpc),nbpc)
      C = lshift(rshift(C,nsht),nsht)
      nm1 = orf(nm1,C)
      P = lshift(rshift(P,nsht),nsht)
      DO l = 1 , Ncds
         IF ( nm1==T1(1,l) .AND. nm2==T1(2,l) ) GOTO 3800
      ENDDO
   ENDIF
   GOTO 4000
 3800 T1(1,l) = M(5)
   T1(2,l) = M(6)
   nm1 = orf(P,rshift(lshift(nm1,nbpc),nbpc))
   DO l = 1 , Ncds
      IF ( nm1==T1(1,l) .AND. nm2==T1(2,l) ) GOTO 3900
   ENDDO
   GOTO 4000
 3900 M(5) = orf(P,rshift(lshift(M(5),nbpc),nbpc))
   T1(1,l) = M(5)
   T1(2,l) = M(6)
 4000 RETURN 3
!     ==============
!
 4100 IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) GOTO 300
   IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 400
   l1 = ndumg + 2
   DO l = 3 , l1
      IF ( Mf(l)/=1 ) GOTO 300
      IF ( M(l)<=0 ) GOTO 400
      IF ( l/=3 ) THEN
         l3 = l - 1
         DO l2 = 3 , l3
            IF ( M(l2)==M(l) ) GOTO 400
         ENDDO
      ENDIF
   ENDDO
   N = ndumc
   GOTO 500
!     ==============
!
 4200 IF ( Mf(1)/=1 .OR. Mf(2)/=1 ) GOTO 300
   IF ( M(1)<=0 .OR. M(2)<=0 ) GOTO 400
   N = ndump
   GOTO 500
!
!     ******************************************************************
!
!     DECODE ADUM-I CARD CONTENTS AS PACKED INTO /SYSTEM/
!
 4300 ndumg = Kdumel(idumel)/10000000
   ndumd = Kdumel(idumel) - 10000000*ndumg
   ndumc = ndumd/10000
   ndump = (ndumd-ndumc*10000)/10
   ndumd = Kdumel(idumel) - (Kdumel(idumel)/10)*10
   ndumc = ndumg + ndumc + 2
   ndump = ndump + 2
   IF ( ndumc>24 ) GOTO 400
   IF ( ndump>24 ) GOTO 400
   GOTO ret
!
99999 RETURN
END SUBROUTINE ifs5p
