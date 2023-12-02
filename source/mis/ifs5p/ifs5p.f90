!*==ifs5p.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ifs5p() !HIDESTARS (*,*,*)
   IMPLICIT NONE
   USE c_bitpos
   USE c_cifs5p
   USE c_ifpdta
   USE c_ifpx1
   USE c_system
   USE c_xmssg
!
! Local variable declarations rewritten by SPAG
!
   REAL :: angsin , tmag , tr1 , tr2 , tr3 , v11 , v12 , v13 , v1mag , v21 , v22 , v23 , v2mag
   INTEGER , SAVE :: blank , ino , iyes , ml , ms , nmt , thru
   INTEGER :: ialt , iblank , idumel , ig1 , ig2 , in1 , in2 , in3 , in4 , in5 , irange , j , j1 , kdlh , kdx , kz , l , l1 , l2 ,  &
            & l3 , ldh , lz , n1 , nb , nbpc , ncpw , ndumc , ndumd , ndumg , ndump , nm1 , nm2 , nsht , nss , ret
   INTEGER , DIMENSION(4) , SAVE :: iscal , met
   INTEGER , DIMENSION(12) , SAVE :: itype
   INTEGER , DIMENSION(100) :: m
   INTEGER , DIMENSION(3) , SAVE :: mot
   INTEGER , DIMENSION(10) :: nfdh
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (M(1),Rm(1)) , (blank,iblank)
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
   IF ( k<=100 ) THEN
      IF ( k==1 .OR. k==2 .OR. k==4 .OR. k==5 .OR. k==6 .OR. k==7 .OR. k==8 .OR. k==9 .OR. k==10 .OR. k==11 .OR. k==12 .OR.         &
         & k==13 .OR. k==14 .OR. k==15 .OR. k==16 .OR. k==17 .OR. k==18 .OR. k==19 .OR. k==20 .OR. k==21 .OR. k==22 .OR. k==23 .OR. &
         & k==24 .OR. k==25 .OR. k==26 .OR. k==27 .OR. k==28 .OR. k==29 .OR. k==30 .OR. k==31 .OR. k==33 .OR. k==34 .OR. k==35 .OR. &
         & k==36 .OR. k==37 .OR. k==38 .OR. k==39 .OR. k==40 .OR. k==41 .OR. k==42 .OR. k==43 .OR. k==44 .OR. k==45 .OR. k==46 .OR. &
         & k==47 .OR. k==48 .OR. k==49 .OR. k==50 .OR. k==52 .OR. k==53 .OR. k==54 .OR. k==55 .OR. k==56 .OR. k==57 .OR. k==58 .OR. &
         & k==59 .OR. k==60 .OR. k==61 .OR. k==62 .OR. k==63 .OR. k==64 .OR. k==65 .OR. k==66 .OR. k==67 .OR. k==68 .OR. k==69 .OR. &
         & k==70 .OR. k==71 .OR. k==72 .OR. k==73 .OR. k==74 .OR. k==75 .OR. k==76 .OR. k==77 .OR. k==78 .OR. k==79 .OR. k==80 .OR. &
         & k==81 .OR. k==82 .OR. k==83 .OR. k==84 .OR. k==85 .OR. k==86 .OR. k==87 .OR. k==89 .OR. k==90 .OR. k==91 .OR. k==92 .OR. &
         & k==93 .OR. k==94 .OR. k==95 .OR. k==96 .OR. k==97 .OR. k==98 ) GOTO 200
      IF ( k==3 ) THEN
!
!*****         3-ADUM1        ******************************************
!
         idumel = 1
      ELSEIF ( k==32 ) THEN
!
!*****         32-ADUM2       ******************************************
!
         idumel = 2
      ELSEIF ( k==51 ) THEN
!
!*****         51-ADUM3       ******************************************
!
         idumel = 3
      ELSEIF ( k==88 ) THEN
!
!*****         88-ADUM4       ******************************************
!
         idumel = 4
      ELSEIF ( k==99 ) THEN
!
!*****         99-ADUM5       ******************************************
!
         idumel = 5
      ELSEIF ( k==100 ) THEN
!
!*****         100-ADUM6      ******************************************
!
         idumel = 6
      ELSE
         GOTO 100
      ENDIF
      GOTO 3700
   ENDIF
 100  IF ( kx<=100 ) THEN
      IF ( kx==1 ) THEN
!
!*****         101-ADUM7      ******************************************
!
         idumel = 7
         GOTO 3700
      ELSEIF ( kx==2 .OR. kx==4 .OR. kx==5 .OR. kx==19 .OR. kx==20 .OR. kx==21 .OR. kx==22 .OR. kx==23 .OR. kx==24 .OR. kx==25 .OR. &
             & kx==26 .OR. kx==27 .OR. kx==28 .OR. kx==29 .OR. kx==30 .OR. kx==31 .OR. kx==32 .OR. kx==33 .OR. kx==34 .OR.          &
             & kx==35 .OR. kx==36 .OR. kx==37 .OR. kx==38 .OR. kx==39 .OR. kx==40 .OR. kx==41 .OR. kx==42 .OR. kx==43 .OR.          &
             & kx==44 .OR. kx==45 .OR. kx==46 .OR. kx==47 .OR. kx==48 .OR. kx==49 .OR. kx==50 .OR. kx==51 .OR. kx==52 .OR.          &
             & kx==53 .OR. kx==54 .OR. kx==55 .OR. kx==56 .OR. kx==57 .OR. kx==58 .OR. kx==62 .OR. kx==66 .OR. kx==79 .OR.          &
             & kx==80 .OR. kx==81 .OR. kx==82 .OR. kx==83 .OR. kx==84 .OR. kx==85 .OR. kx==88 .OR. kx==89 .OR. kx==90 .OR.          &
             & kx==91 .OR. kx==92 .OR. kx==95 .OR. kx==96 .OR. kx==97 .OR. kx==99 .OR. kx==100 ) THEN
         GOTO 200
      ELSEIF ( kx==3 ) THEN
!
!*****         103-ADUM8      ******************************************
!
         idumel = 8
         GOTO 3700
      ELSEIF ( kx==6 ) THEN
!
!*****         106-ADUM9      ******************************************
!
         idumel = 9
         GOTO 3700
      ELSEIF ( kx==7 ) THEN
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
      ELSEIF ( kx==8 ) THEN
!
!*****         108-CDUM2      ******************************************
!
         idumel = 2
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==9 ) THEN
!
!*****         109-CDUM3      ******************************************
!
         idumel = 3
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==10 ) THEN
!
!*****         110-CDUM4      ******************************************
!
         idumel = 4
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==11 ) THEN
!
!*****         111-CDUM5      ******************************************
!
         idumel = 5
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==12 ) THEN
!
!*****         112-CDUM6      ******************************************
!
         idumel = 6
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==13 ) THEN
!
!*****         113-CDUM7      ******************************************
!
         idumel = 7
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==14 ) THEN
!
!*****         114-CDUM8      ******************************************
!
         idumel = 8
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==15 ) THEN
!
!*****         115-CDUM9      ******************************************
!
         idumel = 9
         ASSIGN 4100 TO ret
         GOTO 4300
      ELSEIF ( kx==16 ) THEN
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
      ELSEIF ( kx==17 ) THEN
!
!*****         117-PDUM2      ******************************************
!
         idumel = 2
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==18 ) THEN
!
!*****         118-PDUM3      ******************************************
!
         idumel = 3
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==59 ) THEN
!
!*****         159-PDUM4      ******************************************
!
         idumel = 4
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==60 ) THEN
!
!*****         160-PDUM5      ******************************************
!
         idumel = 5
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==61 ) THEN
!
!*****         161-PDUM6      ******************************************
!
         idumel = 6
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==63 ) THEN
!
!*****         163-PDUM7      ******************************************
!
         idumel = 7
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==64 ) THEN
!
!*****         164-PDUM8      ******************************************
!
         idumel = 8
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==65 ) THEN
!
!*****         165-PDUM9      ******************************************
!
         idumel = 9
         ASSIGN 4200 TO ret
         GOTO 4300
      ELSEIF ( kx==67 ) THEN
!
!*****         167-CONCT1     ******************************************
!
         IF ( km==1 ) THEN
            km = 0
            DO l = 1 , 8
               IF ( mf(l)>1 ) GOTO 300
               IF ( m(l)<=0 .AND. mf(l)==1 ) GOTO 400
            ENDDO
            DO l = 2 , 8
               IF ( mf(l)==1 .AND. nfdh(l-1)==0 ) GOTO 400
            ENDDO
            i(1) = m(1)
            n = 1
            DO l = 2 , 8
               IF ( nfdh(l-1)/=0 ) THEN
                  n = n + 1
                  i(n) = m(l)
               ENDIF
            ENDDO
            kn = 1
            km = 1
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
               kn = 0
               km = 0
               n = n + 1
               i(n) = -1
            ENDIF
            GOTO 600
         ELSE
            nss = 0
            IF ( mf(1)/=1 ) GOTO 300
            DO l = 2 , 8
               IF ( mf(l)/=3 .AND. mf(l)/=0 ) GOTO 300
               IF ( mf(l)==3 ) nss = nss + 1
            ENDDO
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) GOTO 300
            IF ( m(1)<=0 ) GOTO 400
            IF ( nss==1 ) GOTO 400
            i(1) = nss
            i(2) = m(1)
            n = 2
            nb = 0
            DO l = 2 , 8
               IF ( mf(l)==0 ) THEN
                  nb = nb + 1
                  nfdh(l-1) = 0
               ELSE
                  n = n + 2
                  nfdh(l-1) = 1
                  i(n-1) = m(n-2+nb)
                  i(n) = m(n-1+nb)
               ENDIF
            ENDDO
            km = 1
            GOTO 600
         ENDIF
      ELSEIF ( kx==68 ) THEN
!
!*****         168-CONCT      ******************************************
!
         IF ( km==1 ) THEN
            DO l = 1 , 8
               IF ( mf(l)/=0 .AND. mf(l)/=1 ) GOTO 300
            ENDDO
            DO l = 1 , 8
               IF ( mf(l)==1 .AND. m(l)<=0 ) GOTO 400
            ENDDO
            n = 0
            DO l = 1 , 8 , 2
               kdlh = mf(l) + mf(l+1)
               IF ( kdlh/=0 .AND. kdlh/=2 ) GOTO 400
               IF ( kdlh/=0 ) THEN
                  n = n + 2
                  i(n-1) = m(n-1)
                  i(n) = m(n)
               ENDIF
            ENDDO
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
               n = n + 2
               i(n-1) = -1
               i(n) = -1
               km = 0
            ENDIF
            GOTO 600
         ELSE
            km = 1
            DO l = 1 , 2
               IF ( mf(l)/=1 ) GOTO 300
               IF ( mf(l+2)/=3 ) GOTO 300
               IF ( m(l)<=0 ) GOTO 400
            ENDDO
            DO l = 1 , 6
               i(l) = m(l)
            ENDDO
            n = 6
            IF ( m1(1)==0 .OR. m1(2)==0 ) GOTO 600
            GOTO 300
         ENDIF
      ELSEIF ( kx==69 ) THEN
!
!*****         169-TRANS      ******************************************
!
         IF ( mf(1)/=1 .OR. mf(2)/=0 ) GOTO 300
         DO l = 3 , 11
            IF ( mf(l)/=2 .AND. mf(l)/=0 ) GOTO 300
         ENDDO
         IF ( m(1)<=0 ) GOTO 400
         v11 = rm(6) - rm(3)
         v12 = rm(7) - rm(4)
         v13 = rm(8) - rm(5)
         v21 = rm(9) - rm(3)
         v22 = rm(10) - rm(4)
         v23 = rm(11) - rm(5)
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
         i(1) = m(1)
         DO l = 3 , 11
            i(l-1) = m(l)
         ENDDO
         n = 10
         GOTO 600
      ELSEIF ( kx==70 ) THEN
         GOTO 800
      ELSEIF ( kx==71 ) THEN
!
!*****         171-LOADC      ******************************************
!
         IF ( km==1 ) THEN
            n = 0
            ldh = 2
            GOTO 900
         ELSE
            km = 1
            IF ( (mf(1)/=0 .AND. mf(1)/=1) .OR. (mf(2)/=0 .AND. mf(2)/=2) ) GOTO 300
            IF ( m(1)<=0 .OR. m(2)==0 ) GOTO 400
            IF ( mf(3)/=3 .OR. (mf(6)/=3 .AND. mf(6)/=0) ) GOTO 300
            i(1) = m(1)
            i(2) = m(2)
            n = 2
            ldh = 0
            GOTO 900
         ENDIF
      ELSEIF ( kx==72 ) THEN
         GOTO 1000
      ELSEIF ( kx==73 ) THEN
!
!*****         173-SPCS1      ******************************************
!
         IF ( km==1 ) THEN
            l1 = 1
!
!*****         174-SPCS       ******************************************
!
!
!     SAME AS RELES DATA CARD
!
            j1 = 1
         ELSE
            km = 1
            IF ( mf(1)/=1 ) badfor = .TRUE.
            IF ( mf(2)/=3 ) badfor = .TRUE.
            IF ( m(4)<0 ) baddat = .TRUE.
            CALL write(210,m,4,0)
            j1 = 4
            l1 = 5
         ENDIF
         GOTO 1200
      ELSEIF ( kx==74 ) THEN
         GOTO 800
      ELSEIF ( kx==75 ) THEN
!
!*****         175-BDYC       ******************************************
         IF ( km==1 ) THEN
            IF ( mf(1)==0 .AND. mf(8)==0 ) GOTO 1100
            GOTO 300
         ELSE
!
            IF ( mf(8)/=0 .OR. mf(1)/=1 ) GOTO 300
            IF ( m(1)>0 ) GOTO 1100
            GOTO 400
         ENDIF
      ELSEIF ( kx==76 ) THEN
!
!*****         176-MPCS       ******************************************
!
         IF ( km==1 ) THEN
            IF ( mf(1)/=0 ) GOTO 300
            IF ( mf(2)/=3 ) GOTO 300
            DO l = 3 , 6 , 3
               IF ( mf(l)+mf(l+2)+mf(l+1)/=0 ) THEN
                  IF ( mf(l)/=1 .OR. mf(l+1)/=1 ) GOTO 300
                  IF ( mf(l+2)/=2 ) GOTO 300
                  IF ( m(l+1)<=0 .AND. mf(l+2)<=0 ) GOTO 400
               ENDIF
            ENDDO
            n = 0
            DO l = 3 , 8 , 3
               kdlh = mf(l) + mf(l+1) + mf(l+2)
               IF ( kdlh/=0 .AND. kdlh/=4 ) GOTO 400
               IF ( kdlh/=0 ) THEN
                  i(n+1) = m(2)
                  i(n+2) = m(3)
                  n = n + 5
                  i(n-2) = m(l+1)
                  i(n-1) = m(l+2)
                  i(n) = m(l+3)
               ENDIF
            ENDDO
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
               i(n+1) = iblank
               i(n+2) = iblank
               n = n + 5
               i(n-2) = -1
               i(n-1) = -1
               i(n) = -1
               km = 0
            ENDIF
            GOTO 600
         ELSE
            km = 1
            IF ( mf(1)/=1 ) GOTO 300
            IF ( mf(2)/=3 ) GOTO 300
            IF ( mf(3)/=1 ) GOTO 300
            IF ( mf(4)/=1 ) GOTO 300
            IF ( mf(5)/=2 ) GOTO 300
            IF ( m(1)<=0 ) GOTO 400
            IF ( m(4)<=0 ) GOTO 400
            IF ( m(5)<0 ) GOTO 400
            IF ( m(6)==0 ) GOTO 400
            DO l = 1 , 6
               i(l) = m(l)
            ENDDO
            n = 6
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) GOTO 300
            GOTO 600
         ENDIF
      ELSEIF ( kx==77 ) THEN
!
!*****         177-BDYS       ******************************************
!
         DO l = 1 , 7
            IF ( mf(l)/=1 .AND. mf(l)/=0 ) GOTO 300
            IF ( mf(l)==1 .AND. m(l)<=0 ) GOTO 400
         ENDDO
         IF ( mf(1)==0 ) GOTO 300
         n = 1
         i(n) = m(1)
         DO l = 2 , 7 , 2
            kdlh = mf(l) + mf(l+1)
            IF ( kdlh/=2 .AND. kdlh/=0 ) GOTO 400
            IF ( kdlh/=0 ) THEN
               n = n + 2
               i(n-1) = m(n-1)
               i(n) = m(n)
            ENDIF
         ENDDO
         n = n + 2
         i(n-1) = -1
         i(n) = -1
         GOTO 600
      ELSEIF ( kx==78 ) THEN
!
!*****         178-BDYS1      ******************************************
!
         IF ( km==1 ) THEN
            j1 = 1
            l1 = 1
         ELSE
            km = 1
            IF ( mf(1)/=1 .OR. mf(2)>1 ) badfor = .TRUE.
            IF ( m(1)<1 .OR. m(2)<0 ) baddat = .TRUE.
            CALL write(210,m,2,0)
            j1 = 3
            l1 = 3
         ENDIF
         GOTO 1200
      ELSEIF ( kx==86 ) THEN
!
!*****         186-GNEW       ******************************************
!
         IF ( mf(1)/=1 ) GOTO 300
         IF ( mf(2)/=3 ) GOTO 300
         IF ( mf(3)/=1 .AND. mf(3)/=0 ) GOTO 300
         IF ( mf(4)/=1 ) GOTO 300
         IF ( mf(5)/=1 ) GOTO 300
         IF ( m(1)<=0 ) GOTO 400
         IF ( m(4)<0 ) GOTO 400
         IF ( m(5)<=0 ) GOTO 400
         IF ( m(6)<=0 ) GOTO 400
         n = 6
         GOTO 500
      ELSEIF ( kx==87 ) THEN
!
!*****         187-GTRAN      ******************************************
!
         IF ( mf(1)/=1 ) GOTO 300
         IF ( mf(2)/=3 ) GOTO 300
         IF ( mf(3)/=1 ) GOTO 300
         IF ( mf(4)/=1 .AND. mf(4)/=0 ) GOTO 300
         IF ( m(1)<=0 ) GOTO 400
         IF ( m(4)<=0 ) GOTO 400
         IF ( m(5)<0 ) GOTO 400
         n = 5
         GOTO 500
      ELSEIF ( kx==93 ) THEN
!
!*****         193-USET       ******************************************
!
         ASSIGN 1700 TO ret
         GOTO 1500
      ELSEIF ( kx==94 ) THEN
!
!*****         194-USET1      ******************************************
!
         IF ( km/=0 ) THEN
            l1 = 1
            l3 = 0
            l2 = 8
            GOTO 1900
         ELSE
            km = 1
            ASSIGN 1800 TO ret
            GOTO 1500
         ENDIF
      ELSEIF ( kx==98 ) THEN
!
!*****         198-PLOAD1       ****************************************
!
         IF ( m(1)>0 .AND. m(2)>0 ) THEN
            i(1) = m(1)
            i(2) = m(2)
            DO l = 1 , 12
               IF ( m(3)==itype(l) ) GOTO 3300
            ENDDO
         ENDIF
         GOTO 400
      ENDIF
   ENDIF
   IF ( ky<=100 ) THEN
      IF ( ky==1 .OR. ky==2 .OR. ky==3 .OR. ky==4 .OR. ky==5 .OR. ky==6 .OR. ky==7 .OR. ky==8 .OR. ky==9 .OR. ky==10 .OR.           &
         & ky==11 .OR. ky==12 .OR. ky==13 .OR. ky==14 .OR. ky==15 .OR. ky==16 .OR. ky==17 .OR. ky==18 .OR. ky==19 .OR. ky==20 .OR.  &
         & ky==21 .OR. ky==22 .OR. ky==23 .OR. ky==24 .OR. ky==25 .OR. ky==26 .OR. ky==27 .OR. ky==28 .OR. ky==29 .OR. ky==30 .OR.  &
         & ky==31 .OR. ky==32 .OR. ky==33 .OR. ky==34 .OR. ky==35 .OR. ky==36 .OR. ky==37 .OR. ky==38 .OR. ky==39 .OR. ky==40 .OR.  &
         & ky==41 .OR. ky==42 .OR. ky==43 .OR. ky==44 .OR. ky==47 .OR. ky==48 .OR. ky==56 .OR. ky==57 .OR. ky==58 .OR. ky==59 .OR.  &
         & ky==60 .OR. ky==61 .OR. ky==62 .OR. ky==68 .OR. ky==73 .OR. ky==74 .OR. ky==79 .OR. ky==80 .OR. ky==81 .OR. ky==82 .OR.  &
         & ky==83 .OR. ky==84 .OR. ky==85 .OR. ky==86 .OR. ky==87 .OR. ky==88 .OR. ky==89 .OR. ky==90 .OR. ky==91 .OR. ky==92 .OR.  &
         & ky==93 .OR. ky==94 .OR. ky==95 .OR. ky==96 .OR. ky==97 .OR. ky==98 .OR. ky==99 .OR. ky==100 ) GOTO 200
      IF ( ky==45 .OR. ky==46 ) THEN
!
!*****         245-SAME 246-NOSAME          ****************************
!
         ialt = 1
         IF ( m(3)==thru ) ialt = 3
         kdx = ialt + icont
         IF ( kdx==3 ) THEN
!
            IF ( mf(1)/=1 .OR. mf(2)/=1 .OR. mf(3)/=3 .OR. mf(4)/=1 .OR. mf(5)/=1 .OR. mf(6)/=1 .OR. mf(7)/=3 .OR. mf(8)/=1 )       &
               & badfor = .TRUE.
            IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(5)<=0 .OR. m(6)<=0 .OR. m(7)<=0 .OR. m(8)/=thru .OR. m(10)<=0 ) baddat = .TRUE.
            IF ( m(5)<=m(2) .OR. m(10)<=m(7) ) baddat = .TRUE.
            irange = m(5) - m(2)
            IF ( (m(10)-m(7))/=irange ) baddat = .TRUE.
            i(1) = -1
            i(2) = irange + 1
            i(3) = m(1)
            i(4) = m(2)
            i(5) = m(6)
            i(6) = m(7)
            n = 6
         ELSEIF ( kdx==4 ) THEN
            DO in1 = 1 , 6 , 5
               in2 = in1 + 1
               in3 = in2 + 1
               in4 = in3 + 1
               in5 = in4 + 1
               IF ( mf(in1)/=0 .OR. mf(in2)/=0 .OR. mf(in3)/=0 .OR. mf(in4)/=0 ) THEN
                  IF ( mf(in1)/=1 .OR. mf(in2)/=1 .OR. mf(in3)/=3 .OR. mf(in4)/=1 ) badfor = .TRUE.
                  IF ( m(in1)<=0 .OR. m(in2)<=0 .OR. m(in3)/=thru .OR. m(in5)<=0 ) baddat = .TRUE.
                  IF ( m(in5)<=m(in2) .OR. m(in5)-m(in2)/=irange ) baddat = .TRUE.
                  i(n+1) = m(in1)
                  i(n+2) = m(in2)
                  n = n + 2
               ENDIF
            ENDDO
         ELSE
            DO in1 = 1 , 8 , 2
               in2 = in1 + 1
               IF ( mf(in1)/=0 .OR. mf(in2)/=0 ) THEN
                  IF ( mf(in1)/=1 .OR. mf(in2)/=1 ) badfor = .TRUE.
                  IF ( m(in1)<=0 .OR. m(in2)<=0 ) baddat = .TRUE.
!
                  n = n + 2
                  i(n-1) = m(in1)
                  i(n) = m(in2)
               ENDIF
            ENDDO
         ENDIF
!
         IF ( m1f(1)==0 .AND. m1f(2)==0 ) THEN
            icont = 1
         ELSE
            icont = 0
            i(n+1) = -1
            i(n+2) = -1
            n = n + 2
         ENDIF
         GOTO 700
      ELSEIF ( ky==49 ) THEN
         GOTO 300
      ELSEIF ( ky==50 ) THEN
         GOTO 400
      ELSEIF ( ky==51 ) THEN
!
!*****         251-CIHEX1     ******************************************
!
         n = 10
         GOTO 2200
      ELSEIF ( ky==52 ) THEN
!
!*****         252-CIHEX2     ******************************************
!
         n = 22
         GOTO 2200
      ELSEIF ( ky==53 ) THEN
!
!*****         253-CIHEX3     ******************************************
!
         n = 34
         GOTO 2200
      ELSEIF ( ky==54 ) THEN
!
!*****         254-PIHEX      ******************************************
!
         n = 7
         IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 400
         IF ( m(3)<0 ) GOTO 400
         IF ( (m(4)<2 .OR. m(4)>4) .AND. m(4)/=0 ) GOTO 400
         DO l = 5 , 7
            IF ( mf(l)==0 ) THEN
               rm(l) = -1.0
            ELSE
               IF ( mf(l)/=2 ) GOTO 300
               IF ( rm(l)<0.0 ) GOTO 400
            ENDIF
         ENDDO
         IF ( rm(5)>=0.0 .AND. rm(5)<1.0 ) GOTO 400
         IF ( rm(6)<=180.0 .AND. rm(7)<=180.0 ) GOTO 500
         GOTO 400
      ELSEIF ( ky==55 ) THEN
!
!*****         255-PLOAD3     ******************************************
!
         IF ( m(1)<=0 ) GOTO 400
         DO l = 3 , 6 , 3
            IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
               IF ( m(l)<0 .OR. m(l+1)<0 .OR. m(l+2)<0 ) GOTO 400
               n = n + 5
               i(n-4) = m(1)
               i(n-3) = m(2)
               i(n-2) = m(l)
               i(n-1) = m(l+1)
               i(n) = m(l+2)
            ENDIF
         ENDDO
         IF ( n>0 ) GOTO 600
         GOTO 400
      ELSEIF ( ky==63 ) THEN
         GOTO 2300
      ELSEIF ( ky==64 ) THEN
!
!*****         264-PAERO1     ******************************************
!
         IF ( m(1)<=0 ) GOTO 400
         DO l = 2 , 8
            IF ( m(l)<0 ) GOTO 400
         ENDDO
         n = 8
         GOTO 500
      ELSEIF ( ky==65 ) THEN
!
!*****         265-AERO       ******************************************
!
         IF ( iaero/=0 ) GOTO 400
         iaero = 1
         IF ( m(1)<0 ) GOTO 400
         n = 6
         GOTO 500
      ELSEIF ( ky==66 ) THEN
!
!*****         266-SPLINE1    ******************************************
!
         IF ( m(2)<=0 .OR. m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 .OR. m(1)<=0 .OR. rm(6)<0.0 ) GOTO 400
         n = 6
         GOTO 500
      ELSEIF ( ky==67 ) THEN
!
!*****         267-SPLINE2    ******************************************
!
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 .OR. m(8)<0 ) GOTO 400
         n = 10
         GOTO 500
      ELSEIF ( ky==69 ) THEN
!
!*****         269-SET2       ******************************************
!
         IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 400
         n = 8
         GOTO 500
      ELSEIF ( ky==70 ) THEN
!
!*****         270-MKAERO2    ******************************************
!
         n = 0
         DO l = 2 , 8 , 2
            IF ( mf(l)/=0 .OR. mf(l-1)/=0 ) THEN
               IF ( mf(l)==0 .OR. mf(l-1)==0 ) GOTO 300
               n = n + 2
               i(n-1) = m(l-1)
               IF ( rm(l)<=0.0 ) GOTO 400
               i(n) = m(l)
            ENDIF
         ENDDO
         IF ( n/=0 ) GOTO 600
         GOTO 400
      ELSEIF ( ky==71 ) THEN
!
!*****         271-MKAERO1    ******************************************
!
         IF ( mf(1)/=2 .OR. mf(9)/=2 ) GOTO 300
         IF ( rm(9)<=0.0 ) GOTO 400
         DO l = 2 , 8
            IF ( mf(l)==0 ) m(l) = -1
            IF ( mf(l+8)/=0 .AND. rm(l+8)<=0.0 ) GOTO 400
            IF ( mf(l+8)==0 ) m(l+8) = -1
         ENDDO
         n = 16
         GOTO 500
      ELSEIF ( ky==72 ) THEN
!
!*****         257-FLUTTER    ******************************************
!
         IF ( m(1)>0 .AND. m(4)>=0 .AND. m(5)>=0 .AND. m(6)>=0 ) THEN
            DO l = 1 , nmt
               IF ( m(2)==met(l) ) GOTO 3200
            ENDDO
         ENDIF
         GOTO 400
      ELSEIF ( ky==75 ) THEN
!
!*****         275-CBARAO       ****************************************
!
         IF ( m(1)>0 ) THEN
            i(1) = m(1)
            DO l = 1 , 2
               IF ( m(2)==iscal(l) ) GOTO 3500
            ENDDO
         ENDIF
         GOTO 400
      ELSEIF ( ky==76 ) THEN
!
!*****         276-PLIMIT       ****************************************
!
         IF ( mf(1)/=3 ) GOTO 300
         IF ( mf(2)/=2 .AND. mf(2)/=0 ) GOTO 300
         IF ( rm(3)<0.0 ) GOTO 400
         IF ( rm(3)==0.0 .AND. rm(4)==0.0 ) GOTO 400
         IF ( rm(4)==0.0 ) GOTO 3600
         IF ( mf(3)==2 .AND. rm(4)>rm(3) ) GOTO 3600
         GOTO 400
      ELSEIF ( ky==77 ) THEN
!
!*****         277-POPT         ****************************************
!
         IF ( m(1)<=0 .OR. m(4)==0 ) GOTO 400
         IF ( ipopt/=0 ) GOTO 400
         ipopt = 1
         IF ( rm(2)<0.0 ) GOTO 400
         IF ( rm(3)<=0.0 ) GOTO 400
         IF ( m(5)/=iyes .AND. m(5)/=ino ) GOTO 400
         n = 6
         GOTO 500
      ELSEIF ( ky==78 ) THEN
!
!******       278  PLOADX   ******************************************
!
         IF ( m(1)<=0 ) GOTO 400
         IF ( m(4)<=0 .OR. m(5)<=0 .OR. m(6)<=0 ) GOTO 400
         n = 6
         GOTO 500
      ENDIF
   ENDIF
   kz = ky - 100
   IF ( kz<=39 ) THEN
      IF ( kz==1 .OR. kz==2 .OR. kz==3 .OR. kz==9 ) GOTO 2300
      IF ( kz==4 ) THEN
!
!*****      304 - PAERO2    ***************
!
         IF ( m(1)>0 ) THEN
            DO l = 1 , 3
               IF ( m(2)==mot(l) ) GOTO 2600
            ENDDO
         ENDIF
         GOTO 400
      ELSEIF ( kz==5 ) THEN
!
!*****      305 - PAERO3    ****************
!
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 ) GOTO 400
         IF ( m(2)>50 ) GOTO 400
         n = 0
         IF ( m(3)==0 ) n = 4
         IF ( m(3)==1 ) n = 12
         IF ( m(3)==2 ) n = 16
         IF ( n==0 ) GOTO 400
         m(4) = n
         n = n + 4
         IF ( n==8 ) GOTO 500
         DO l = 9 , n
            IF ( mf(l)==-32767 ) GOTO 400
         ENDDO
         IF ( rm(12)<rm(10) ) GOTO 400
         IF ( rm(16)<rm(14) ) GOTO 400
         IF ( n==16 ) GOTO 500
         IF ( rm(20)>=rm(18) ) GOTO 500
         GOTO 400
      ELSEIF ( kz==6 ) THEN
!
!*****      306 - PAERO4   **********************
!
         IF ( km/=0 ) THEN
            l1 = 1
            GOTO 2700
         ELSE
            km = 1
            IF ( mf(1)/=1 .OR. m(1)<=0 ) GOTO 2900
            DO l = 2 , 5
               IF ( mf(1)<0 .OR. mf(l)>1 ) GOTO 2900
            ENDDO
            IF ( m(3)<0 ) GOTO 2900
            IF ( m(2)==0 .AND. m(3)/=0 ) GOTO 2900
            IF ( m(2)>0 .AND. m(3)==0 ) GOTO 2900
            IF ( m(2)/=0 .AND. m(4)/=0 ) GOTO 2900
            IF ( m(4)<0 .OR. m(4)>3 ) GOTO 2900
            IF ( m(4)==0 .AND. m(5)/=0 ) GOTO 2900
            IF ( m(4)>0 .AND. m(5)==0 ) GOTO 2900
            DO l = 1 , 5
               i(l) = m(l)
            ENDDO
            n = 5
            l1 = 6
            GOTO 2700
         ENDIF
      ELSEIF ( kz==7 ) THEN
!
!*****    307 - SPLINE3       *********************
!
         IF ( km/=0 ) THEN
            l1 = 1
            GOTO 3100
         ELSE
            km = 1
            IF ( mf(1)/=1 .OR. mf(2)/=1 .OR. mf(3)/=1 .OR. mf(4)/=1 ) GOTO 2900
            IF ( m(2)<=0 .OR. m(3)<0 ) GOTO 2900
            IF ( ifpdco(m(4)) ) GOTO 2900
            IF ( gc(2)/=0 ) GOTO 2900
            DO l = 1 , 4
               i(l) = m(l)
            ENDDO
            n = 4
            l1 = 5
            GOTO 3100
         ENDIF
      ELSEIF ( kz==8 ) THEN
!
!******    308 - GUST
!
         IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 400
         IF ( rm(3)==0.0 .OR. rm(5)==0.0 ) GOTO 400
         n = 5
         GOTO 500
      ELSEIF ( kz==10 ) THEN
!
!*****   310 - PAERO5  ************
!
         IF ( km/=0 ) THEN
            l1 = 1
            GOTO 2700
         ELSE
            km = 1
            DO l = 1 , 3
               IF ( mf(l)/=1 .OR. m(l)<=0 ) GOTO 2900
            ENDDO
            DO l = 4 , 7
               IF ( mf(l)<0 .OR. mf(l)>1 ) GOTO 2900
            ENDDO
            IF ( m(4)/=0 .AND. m(5)==0 ) GOTO 2900
            IF ( m(6)/=0 .AND. m(7)==0 ) GOTO 2900
            DO l = 1 , 7
               i(l) = m(l)
            ENDDO
            n = 7
            GOTO 2800
         ENDIF
      ELSEIF ( kz==11 .OR. kz==12 .OR. kz==13 ) THEN
         GOTO 1000
      ELSEIF ( kz==14 ) THEN
!
!*****         314-TICS         **************************************
!
         IF ( m(1)<=0 ) GOTO 400
         IF ( m(4)<=0 ) GOTO 400
         IF ( m(5)<0 ) GOTO 400
         DO l = 8 , 11
            m(l) = -1
         ENDDO
         n = 11
         GOTO 500
      ELSEIF ( kz==38 ) THEN
!
!*****         338-CELBOW     ******************************************
!
         IF ( m(2)==0 ) m(2) = m(1)
         n = 8
         GOTO 500
      ELSEIF ( kz==39 ) THEN
!
!*****         339-PELBOW     ******************************************
!
         n = 24
         GOTO 500
      ENDIF
   ENDIF
 200  CALL page2(2)
   WRITE (nout,99001) sfm
99001 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS5P.')
   abort = .TRUE.
   IF ( k==0 ) GOTO 99999
   RETURN 1
 300  badfor = .TRUE.
   RETURN 1
 400  baddat = .TRUE.
   RETURN 1
 500  DO l = 1 , n
      i(l) = m(l)
   ENDDO
 600  RETURN
 700  RETURN 3
!
!*****         170-RELES      ******************************************
!
 800  IF ( km==1 ) THEN
      n = 0
      l1 = 1
   ELSE
      km = 1
      IF ( mf(1)/=1 ) GOTO 300
      IF ( mf(2)/=3 ) GOTO 300
      IF ( m(1)<=0 ) GOTO 400
      i(1) = m(1)
      i(2) = m(2)
      i(3) = m(3)
      l1 = 3
      n = 3
   ENDIF
   DO l = l1 , 8 , 2
      kdlh = mf(l) + mf(l+1)
      IF ( kdlh/=0 .AND. kdlh/=2 ) GOTO 400
      IF ( kdlh/=0 ) THEN
         n = n + 2
         i(n-1) = m(n-1)
         i(n) = m(n)
      ENDIF
   ENDDO
   IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
      n = n + 2
      i(n-1) = -1
      i(n) = -1
      km = 0
   ENDIF
   GOTO 600
 900  DO l = 3 , 8 , 3
      kdlh = mf(l) + mf(l+1) + mf(l+2)
      IF ( kdlh/=0 .AND. kdlh/=6 ) GOTO 400
      IF ( kdlh/=0 ) THEN
         n = n + 4
         i(n-3) = m(n-3+ldh)
         i(n-2) = m(n-2+ldh)
         i(n-1) = m(n-1+ldh)
         i(n) = m(n+ldh)
      ENDIF
   ENDDO
   IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
      n = n + 4
      i(n-3) = iblank
      i(n-2) = iblank
      i(n-1) = -1
      i(n) = -1
      km = 0
   ENDIF
   GOTO 600
!
!*****         172-SPCSD ,  311-DAREAS          **********************
!              312-DELAYS,  313-DPHASES
!
 1000 IF ( m(1)<=0 ) GOTO 400
   IF ( m(4)<=0 ) GOTO 400
   IF ( m(5)<0 ) GOTO 400
   IF ( m(7)<0 ) GOTO 400
   IF ( m(8)<0 ) GOTO 400
   n = 12
   IF ( m(7)==0 ) n = 9
   m(n-2) = -1
   m(n-1) = -1
   m(n) = -1
   GOTO 500
 1100 DO l = 2 , 7 , 2
      IF ( mf(l)/=0 .AND. mf(l)/=3 ) GOTO 300
      IF ( mf(l+1)/=0 .AND. mf(l+1)/=1 ) GOTO 300
   ENDDO
   i(1) = m(1)
   n = 1
   j1 = 1
   IF ( km==1 ) j1 = 0
   DO l = 2 , 7 , 2
      kdlh = mf(l) + mf(l+1)
      IF ( kdlh/=0 .AND. kdlh/=4 ) GOTO 400
      IF ( kdlh/=0 ) THEN
         n = n + 3
         j1 = j1 + 3
         i(j1-2) = m(n-2)
         i(j1-1) = m(n-1)
         i(j1) = m(n)
      ENDIF
   ENDDO
   n = j1
   km = 1
   IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
      km = 0
      n = n + 3
      j1 = j1 + 3
      i(j1-2) = iblank
      i(j1-1) = iblank
      i(j1) = -1
   ENDIF
   GOTO 600
!
!     COMMON PROCESSING FOR SPCS1 AND BDYS1 CARDS
!
 1200 IF ( mf(j1)==0 ) THEN
      j1 = j1 + 1
      l1 = l1 + 1
   ELSEIF ( mf(j1)==1 ) THEN
      IF ( j1<=6 ) THEN
         IF ( mf(j1+1)==3 ) THEN
            IF ( m(l1+1)/=thru ) THEN
               baddat = .TRUE.
            ELSEIF ( mf(j1+2)/=1 ) THEN
               badfor = .TRUE.
            ELSEIF ( m(l1+3)>m(l1) ) THEN
               ig1 = m(l1)
               ig2 = m(l1+3)
               DO j = ig1 , ig2
                  CALL write(210,j,1,0)
               ENDDO
               j1 = j1 + 3
               l1 = l1 + 4
               GOTO 1300
            ELSE
               baddat = .TRUE.
            ENDIF
            GOTO 1400
         ENDIF
      ENDIF
      CALL write(210,m(l1),1,0)
      j1 = j1 + 1
      l1 = l1 + 1
   ELSE
      badfor = .TRUE.
      GOTO 1400
   ENDIF
 1300 IF ( j1<=8 ) GOTO 1200
 1400 IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
      km = 0
      kn = 0
      n = 1
      i(1) = -1
   ELSE
      kn = 1
      n = 0
   ENDIF
   GOTO 700
 1500 n = 0
   IF ( m(2)==blank ) THEN
      DO l = 1 , 32
         IF ( m(1)==kb(l,2) ) GOTO 1600
      ENDDO
   ENDIF
   GOTO 400
 1600 id = kb(l,1)
   GOTO ret
 1700 DO l = 3 , 7 , 2
      IF ( m(l)/=0 .OR. m(l+1)/=0 ) THEN
         IF ( m(l)<=0 ) GOTO 400
         IF ( ifpdco(m(l+1)) ) GOTO 400
         lz = 6
         IF ( m(l+1)==0 ) lz = 1
         DO l2 = 1 , lz
            IF ( lz==1 .OR. ll(l2)/=0 ) THEN
               n = n + 3
               i(n-2) = id
               i(n-1) = m(l)
               i(n) = ll(l2)
               IF ( n>3 ) THEN
                  DO l1 = 6 , n , 3
                     IF ( i(n-1)==i(l1-4) .AND. i(n)==i(l1-3) ) GOTO 400
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDDO
   IF ( n>0 ) GOTO 600
   GOTO 400
 1800 n = 2
   i(1) = id
   IF ( mf(2)/=0 .AND. mf(2)/=1 ) badfor = .TRUE.
   IF ( ifpdco(m(3)) ) baddat = .TRUE.
   i(2) = m(3)
   IF ( mf(4)/=3 .OR. m(5)/=thru ) THEN
      l1 = 4
      l3 = -1
      l2 = 9
   ELSEIF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
      IF ( mf(3)/=1 .OR. mf(5)/=1 ) badfor = .TRUE.
      IF ( m(4)<=0 .OR. m(7)<=m(4) ) baddat = .TRUE.
      DO l = 1 , 3
         IF ( mf(l+5)/=0 ) badfor = .TRUE.
      ENDDO
      IF ( .NOT.(badfor .OR. baddat) ) THEN
         CALL write(210,i,2,0)
         l1 = m(4)
         l2 = m(7)
         DO l = l1 , l2
            CALL write(210,l,1,0)
         ENDDO
         n = 0
      ENDIF
      GOTO 2100
   ELSE
      kn = 1
      badfor = .TRUE.
      GOTO 700
   ENDIF
 1900 DO l = l1 , l2
      IF ( mf(l+l3)/=0 .AND. mf(l+l3)/=1 ) badfor = .TRUE.
   ENDDO
   DO l = l1 , l2
      IF ( mf(l+l3)==1 ) GOTO 2000
   ENDDO
   baddat = .TRUE.
 2000 DO l = l1 , l2
      IF ( m(l)<0 ) THEN
         baddat = .TRUE.
      ELSEIF ( m(l)/=0 ) THEN
         n = n + 1
         i(n) = m(l)
      ENDIF
   ENDDO
   kn = 1
   IF ( m1(1)==0 .AND. m1(2)==0 ) GOTO 700
 2100 km = 0
   n = n + 1
   i(n) = -1
   kn = 0
   GOTO 700
 2200 DO l = 1 , n
      IF ( m(l)<=0 ) GOTO 400
   ENDDO
   n1 = n - 1
   DO l = 3 , n1
      l2 = l + 1
      DO l1 = l2 , n
         IF ( m(l)==m(l1) ) GOTO 400
      ENDDO
   ENDDO
   GOTO 500
!
!*****    263-CAERO1, 301-CAERO2, 302-CAERO3, 303-CAERO4  *******
!         309-CAERO5
!
 2300 IF ( m(1)<=0 ) GOTO 400
   IF ( m(2)<=0 ) GOTO 400
   DO l = 3 , 8
      IF ( m(l)<0 ) GOTO 400
   ENDDO
   IF ( k==302 ) THEN
!
!*****     CAERO3      ************************************************
!
      IF ( m(4)==0 ) GOTO 400
      IF ( rm(12)==0. ) GOTO 400
   ELSE
      IF ( k==303 ) GOTO 2500
      IF ( k==309 ) GOTO 2500
      IF ( m(4)==0 .AND. m(6)==0 ) GOTO 400
      IF ( m(5)==0 .AND. m(7)==0 ) GOTO 400
      IF ( m(8)<=0 ) GOTO 400
   ENDIF
 2400 IF ( rm(12)<0.0 ) GOTO 400
   IF ( rm(16)<0.0 ) GOTO 400
   IF ( rm(12)==0.0 .AND. rm(16)==0.0 ) GOTO 400
   n = 16
   GOTO 500
!
!*****     CAERO4   CAERO5    ******************************************
!
 2500 IF ( m(4)==0 .AND. m(5)==0 ) GOTO 400
   IF ( m(6)<=2 ) GOTO 2400
   GOTO 400
 2600 IF ( rm(4)<=0.0 ) GOTO 400
   IF ( rm(5)<=0.0 ) GOTO 400
   DO l = 6 , 15
      IF ( m(l)<0 ) GOTO 400
   ENDDO
   n = 15
   GOTO 500
 2700 DO l = l1 , 8
      IF ( mf(l)==0 ) GOTO 3000
      IF ( mf(l)/=2 ) GOTO 2900
      IF ( rm(l)<0. ) GOTO 2900
      n = n + 1
      i(n) = m(l)
   ENDDO
 2800 kn = 1
   IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
      kn = 0
      km = 0
      n = n + 1
      i(n) = -1
   ENDIF
   GOTO 700
 2900 baddat = .TRUE.
   GOTO 2800
 3000 IF ( m1(1)==0 .AND. m1(2)==0 ) baddat = .TRUE.
   GOTO 2800
 3100 DO l = l1 , 8 , 4
      IF ( mf(l)/=0 ) THEN
         IF ( mf(l)/=1 ) GOTO 2900
         IF ( mf(l+1)/=1 ) GOTO 2900
         IF ( ifpdco(m(l+1)) ) GOTO 2900
         IF ( gc(2)/=0 ) GOTO 2900
         IF ( mf(l+2)/=2 ) GOTO 2900
         IF ( m(l)<=0 ) GOTO 2900
         n = n + 3
         i(n) = m(l+2)
         i(n-1) = m(l+1)
         i(n-2) = m(l)
      ENDIF
   ENDDO
   GOTO 2800
 3200 IF ( m(7)/=ms .AND. m(7)/=ml ) GOTO 400
   n = 10
   GOTO 500
 3300 i(3) = l
   DO l = 1 , 4
      IF ( m(5)==iscal(l) ) GOTO 3400
   ENDDO
   GOTO 400
 3400 i(4) = l
   IF ( rm(9)==0.0 ) rm(9) = rm(7)
   IF ( rm(9)<rm(7) ) GOTO 400
   DO l = 7 , 10
      i(l-2) = m(l)
   ENDDO
   n = 8
   GOTO 600
 3500 i(2) = l
   DO l = 4 , 9
      i(l-1) = m(l)
   ENDDO
   n = 9
   IF ( mf(3)==2 ) THEN
      i(9) = 1
      DO l = 4 , 9
         IF ( rm(l)<0.0 ) GOTO 400
      ENDDO
      GOTO 600
   ELSE
      IF ( mf(3)/=1 ) GOTO 300
      IF ( i(3)<=0 ) GOTO 400
      IF ( i(3)>20 ) i(3) = 20
      IF ( rm(5)<=0.0 .OR. rm(6)<=0.0 ) GOTO 400
      i(9) = -1
      GOTO 600
   ENDIF
 3600 IF ( mf(5)==3 ) THEN
      IF ( m(6)/=thru ) GOTO 400
      IF ( mf(4)/=1 .OR. mf(6)/=1 ) GOTO 300
      IF ( m(8)<=m(5) ) GOTO 400
      n = 9
      GOTO 500
   ELSE
      DO l = 4 , 8
         IF ( mf(l)/=0 .AND. mf(l)/=1 ) GOTO 300
         IF ( m(l+1)<0 ) GOTO 400
      ENDDO
      n = 9
      GOTO 500
   ENDIF
!
!
!
!     ******************************************************************
!
!     PROCESS ADUM-I CARDS.
!
 3700 IF ( m(1)<=0 ) GOTO 400
   IF ( m(2)<0 ) GOTO 400
   IF ( m(3)<0 ) GOTO 400
   IF ( m(4)/=3 .AND. m(4)/=6 ) GOTO 400
   IF ( mf(5)/=0 .OR. mf(6)/=0 .OR. mf(7)/=0 .OR. mf(8)/=0 ) GOTO 300
   kdumel(idumel) = m(4) + 10*(m(3)+1000*(m(2)+1000*m(1)))
!
!     PUT IN CONNECTION AND PROPERTY CARD NAME IF SUPPLIED BY USER
!
   IF ( mf(5)==3 ) THEN
      nbpc = junk(36)
      ncpw = junk(38)
      nsht = nbpc*(ncpw-1)
      nm1 = t1(1,k)
      nm2 = t1(2,k)
      nm1 = rshift(lshift(nm1,nbpc),nbpc)
      c = lshift(rshift(c,nsht),nsht)
      nm1 = orf(nm1,c)
      p = lshift(rshift(p,nsht),nsht)
      DO l = 1 , ncds
         IF ( nm1==t1(1,l) .AND. nm2==t1(2,l) ) GOTO 3800
      ENDDO
   ENDIF
   GOTO 4000
 3800 t1(1,l) = m(5)
   t1(2,l) = m(6)
   nm1 = orf(p,rshift(lshift(nm1,nbpc),nbpc))
   DO l = 1 , ncds
      IF ( nm1==t1(1,l) .AND. nm2==t1(2,l) ) GOTO 3900
   ENDDO
   GOTO 4000
 3900 m(5) = orf(p,rshift(lshift(m(5),nbpc),nbpc))
   t1(1,l) = m(5)
   t1(2,l) = m(6)
 4000 RETURN 3
!     ==============
!
 4100 IF ( mf(1)/=1 .OR. mf(2)/=1 ) GOTO 300
   IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 400
   l1 = ndumg + 2
   DO l = 3 , l1
      IF ( mf(l)/=1 ) GOTO 300
      IF ( m(l)<=0 ) GOTO 400
      IF ( l/=3 ) THEN
         l3 = l - 1
         DO l2 = 3 , l3
            IF ( m(l2)==m(l) ) GOTO 400
         ENDDO
      ENDIF
   ENDDO
   n = ndumc
   GOTO 500
!     ==============
!
 4200 IF ( mf(1)/=1 .OR. mf(2)/=1 ) GOTO 300
   IF ( m(1)<=0 .OR. m(2)<=0 ) GOTO 400
   n = ndump
   GOTO 500
!
!     ******************************************************************
!
!     DECODE ADUM-I CARD CONTENTS AS PACKED INTO /SYSTEM/
!
 4300 ndumg = kdumel(idumel)/10000000
   ndumd = kdumel(idumel) - 10000000*ndumg
   ndumc = ndumd/10000
   ndump = (ndumd-ndumc*10000)/10
   ndumd = kdumel(idumel) - (kdumel(idumel)/10)*10
   ndumc = ndumg + ndumc + 2
   ndump = ndump + 2
   IF ( ndumc>24 ) GOTO 400
   IF ( ndump>24 ) GOTO 400
   GOTO ret
!
99999 END SUBROUTINE ifs5p
