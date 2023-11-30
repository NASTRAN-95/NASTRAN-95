
SUBROUTINE ifs2p(*,*,*)
!
   IMPLICIT NONE
   INTEGER A(4) , Bandit , Gc(7) , I(100) , I0 , Ibcds , Ibuf(1) , Icfiat , Id , Ifiat(2) , Ifist(1) , Ipfist , K , Km , Kn , Knt , &
         & Kprec , Ksystm(77) , Kx , Ky , L15 , L8 , Ll(6) , M(100) , M1(100) , M1f(100) , Mach , Mf(100) , N , Nax , Naxf , Nbuf , &
         & Nopen , Nout , Nparam , Nt1 , P(3) , Slotdf(5) , T1(2,1)
   LOGICAL Abort , Baddat , Badfor , Dmiflg , Fphys , Fphys1 , Fphys2 , Fthru , Iax , Iaxf , Lharm
   DOUBLE PRECISION Da(2)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Xm(100) , Z(100)
   COMMON /cifs2p/ Fphys , Fphys1 , Km , Dmiflg , Ibcds , Fthru , Fphys2
   COMMON /ifpdta/ Id , N , K , Kx , Ky , I , M , Mf , M1 , M1f , Kn , Baddat , Badfor , Nopen , Nparam , Iax , Nax , Iaxf , Naxf , &
                 & Lharm , Knt , Slotdf , Gc , Ll
   COMMON /ifpx1 / Nt1 , T1
   COMMON /l15l8 / L15 , L8
   COMMON /machin/ Mach
   COMMON /system/ Ksystm
   COMMON /xdpl  / P
   COMMON /xfiat / Ifiat
   COMMON /xfist / Ifist
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /xpfist/ Ipfist
   COMMON /zblpkx/ A , I0
   COMMON /zzzzzz/ Ibuf
   INTEGER bcdblk , bcddet , bcdfer , bcdgiv , bcdhes , bcdinv , bcdll , bcdls , bcdmas , bcdmax , bcdmgv , bcdpoi , bcdq , bcdsdt ,&
         & bcdsin , bcdsl , bcdt , bcdudt , bcduin , bcdz , dmi , dmig , dti , eigb , eigr , endrc1 , endrc2 , endt , failur , i2 , &
         & iall(2) , icomp , ierr , ifo , ihill(2) , ihoff(2) , ii , imem(2) , ino , iold1 , iold2 , iold3 , ioldm2 , ioldm4 ,      &
         & ioldm6 , iolmf2 , iolmf4 , iout , ip , iprint , iscr1 , istrn(2) , istrs(2) , isym(2) , isymm(2) , items , itrlt ,       &
         & itsai(2) , ity1 , iyes , j , j0 , kword1 , kword2 , kz , l , l1 , l1f , l2 , l3 , lamopt , lf , lp1 , lp2 , lx , nam(2) ,&
         & nbuf2 , ncols , ndpl , nfiat
   LOGICAL ec , flshal , flush , int , secd
   INTEGER lshift , orf
   INTEGER nm(2) , nrows , nwords , onm(2) , pool , r , r1 , skip , t(7) , thru , ty1 , ty2
   REAL oldxm3 , x1 , x2 , xl , xl1 , zseq , zseq1
   EXTERNAL lshift , orf
!
!     P(1) = NEXT AVAILABLE FILE ON POOL
!     P(2) = TOTAL NUMBER OF POSSIBLE ENTRYS
!     P(3) = CURRENT NUMBER OF ENTRYS PRESENT
!     P(4) - P(3*P(2)+3) = THREE WORDS FOR EACH ENTRY AS FOLLOWS...
!            1.  NAME(1)
!            2.  NAME(2)
!            3.  EQUIV FLAG, SIZE/1000, FILE NO. ON POOL
!
   EQUIVALENCE (Ksystm(1),Nbuf) , (Ksystm(24),Icfiat) , (Ksystm(2),Nout) , (Ksystm(55),Kprec) , (Ksystm(3),Abort) ,                 &
    & (Ksystm(77),Bandit) , (nrows,t(3)) , (ifo,t(4)) , (ty2,t(5)) , (Z(1),I(1)) , (Xm(1),M(1)) , (Da(1),A(1))
!
   DATA nam/4HISF2 , 4HP   /
   DATA endt/4HENDT/ , skip/4HSKIP/ , pool/4HPOOL/
   DATA bcdblk/4H    / , bcddet/4HDET / , bcdsdt/4HSDET/ , bcdudt/4HUDET/ , bcdinv/4HINV / , bcdsin/4HSINV/ , bcduin/4HUINV/ ,      &
      & bcdgiv/4HGIV / , bcdmgv/4HMGIV/ , bcdhes/4HHESS/ , bcdfer/4HFEER/ , bcdmas/4HMASS/ , bcdmax/4HMAX / , bcdpoi/4HPOIN/ ,      &
       &bcdq/4H-Q  / , bcdt/4HT   / , bcdz/4H-X  / , bcdll/4HLL  / , bcdsl/4HSL  / , bcdls/4HLS  /
   DATA thru/4HTHRU/ , eigr/4HEIGR/ , eigb/4HEIGB/
   DATA dmi/4H DMI/ , dti/4H DTI/ , dmig/4HDMIG/
   DATA endrc1 , endrc2/4HENDR , 4HEC  /
   DATA iscr1/301/ , icomp/1/
   DATA ihill , ihoff , itsai , istrs , istrn/4HHILL , 4H     , 4HHOFF , 4H     , 4HTSAI , 4H     , 4HSTRE , 4HSS   , 4HSTRA ,      &
       &4HIN  /
   DATA iall , isym , imem , isymm/4HALL  , 4H     , 4HSYM  , 4H     , 4HMEM  , 4H     , 4HSYMM , 4HEM  /
   DATA iyes , ino/4HYES  , 4HNO  /
!
!     =======================================================
!     DMI AND DMIG MUST ACCOMODATE ALL KINDS OF SPECIAL FORMS
!     E.G., IDENTITY MATRIX
!     =======================================================
!
   IF ( K<=100 ) THEN
      IF ( K==1 .OR. K==2 .OR. K==3 .OR. K==4 .OR. K==5 .OR. K==6 .OR. K==7 .OR. K==8 .OR. K==9 .OR. K==10 .OR. K==11 .OR.          &
         & K==12 .OR. K==13 .OR. K==14 .OR. K==15 .OR. K==16 .OR. K==17 .OR. K==18 .OR. K==19 .OR. K==20 .OR. K==21 .OR. K==22 .OR. &
         & K==23 .OR. K==24 .OR. K==25 .OR. K==26 .OR. K==27 .OR. K==28 .OR. K==29 .OR. K==30 .OR. K==31 .OR. K==32 .OR. K==33 .OR. &
         & K==34 .OR. K==35 .OR. K==36 .OR. K==37 .OR. K==38 .OR. K==39 .OR. K==40 .OR. K==41 .OR. K==42 .OR. K==43 .OR. K==44 .OR. &
         & K==45 .OR. K==46 .OR. K==47 .OR. K==48 .OR. K==49 .OR. K==50 .OR. K==51 .OR. K==52 .OR. K==53 .OR. K==54 .OR. K==55 .OR. &
         & K==56 .OR. K==57 .OR. K==58 .OR. K==59 .OR. K==60 .OR. K==61 .OR. K==62 .OR. K==63 .OR. K==64 .OR. K==65 .OR. K==66 .OR. &
         & K==67 .OR. K==68 .OR. K==69 .OR. K==70 .OR. K==71 .OR. K==72 .OR. K==73 .OR. K==74 .OR. K==75 .OR. K==76 .OR. K==77 .OR. &
         & K==78 .OR. K==79 .OR. K==80 .OR. K==81 .OR. K==82 .OR. K==83 .OR. K==84 .OR. K==88 .OR. K==90 .OR. K==91 .OR. K==92 .OR. &
         & K==98 .OR. K==99 .OR. K==100 ) GOTO 100
      IF ( K==85 .OR. K==86 ) THEN
!
!*******       85-EIGR, 86-EIGB      ***********************************
!
         IF ( M(1)<=0 ) GOTO 200
         IF ( M(3)/=bcdblk .AND. M(2)/=bcdfer ) GOTO 200
         IF ( M(2)/=bcddet .AND. M(2)/=bcdsdt .AND. M(2)/=bcdudt .AND. M(2)/=bcdinv .AND. M(2)/=bcdsin .AND. M(2)/=bcduin .AND.     &
            & M(2)/=bcdgiv .AND. M(2)/=bcdmgv .AND. M(2)/=bcdfer ) GOTO 200
         IF ( M(2)==bcdfer .AND. (M(3)/=bcdblk .AND. M(3)/=bcdq .AND. M(3)/=bcdz) ) GOTO 200
         IF ( M(10)+M(11)==0 ) GOTO 600
         IF ( M(10)==bcdblk .AND. M(11)==bcdblk ) GOTO 600
         IF ( (M(10)/=bcdmas .OR. M(11)/=bcdblk) .AND. (M(10)/=bcdmax .OR. M(11)/=bcdblk) .AND. (M(10)/=bcdpoi .OR. M(11)/=bcdt) )  &
            & GOTO 200
         IF ( M(10)/=bcdpoi .AND. (M(12)/=0 .OR. M(13)/=0) ) GOTO 200
         IF ( .NOT.(M(10)==bcdpoi .AND. (M(12)<=0 .OR. M(13)<0)) ) GOTO 700
         GOTO 200
      ELSEIF ( K==87 ) THEN
!
!*****         87-EIGC          **************************************
!
         IF ( Km/=0 ) THEN
            DO l = 1 , 5
               IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) GOTO 900
            ENDDO
            IF ( Mf(6)/=1 .AND. Mf(6)/=0 .OR. Mf(7)/=1 .AND. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 900
            IF ( Xm(5)<=0. ) Xm(5) = 1.0
            IF ( M(6)<0 .OR. M(7)<0 ) GOTO 900
            N = 7
            GOTO 800
         ELSE
            IF ( Mf(1)/=1 .OR. Mf(2)/=3 .OR. Mf(3)/=3 .OR. Mf(4)/=1 .AND. Mf(4)/=0 .OR. Mf(5)/=1 .AND. Mf(5)/=0 .OR. Mf(6)/=2 .AND. &
               & Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 900
            IF ( M(1)<=0 .OR. M(2)/=bcddet .AND. M(2)/=bcdinv .AND. M(2)/=bcdhes .AND. M(2)/=bcdfer .OR. M(4)/=bcdmax .AND.         &
               & (M(4)/=bcdpoi .OR. M(5)/=bcdt) .OR. Xm(8)<0. ) GOTO 900
            IF ( M(4)==bcdmax .AND. (M(6)/=0 .OR. M(7)/=0) ) GOTO 900
            IF ( M(4)==bcdpoi .AND. (M(6)<=0 .OR. M(7)<0) ) GOTO 900
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 900
            N = 10
            GOTO 800
         ENDIF
      ELSEIF ( K==89 ) THEN
!
!*******       -BLANK CARD-        *************************************
!
         IF ( Ibcds/=0 ) RETURN 2
         Ibcds = 1
         CALL page2(2)
         WRITE (Nout,99001) Uwm
99001    FORMAT (A25,' 324, BLANK CARD(S) IGNORED.')
         RETURN 2
      ELSEIF ( K==93 .OR. K==94 .OR. K==95 .OR. K==97 ) THEN
         GOTO 1100
      ELSEIF ( K==96 ) THEN
         GOTO 1700
      ENDIF
   ENDIF
   IF ( Kx<=100 ) THEN
      IF ( Kx==1 .OR. Kx==2 .OR. Kx==3 .OR. Kx==4 .OR. Kx==5 .OR. Kx==6 .OR. Kx==7 .OR. Kx==8 .OR. Kx==9 .OR. Kx==10 .OR.           &
         & Kx==11 .OR. Kx==12 .OR. Kx==13 .OR. Kx==14 .OR. Kx==15 .OR. Kx==16 .OR. Kx==17 .OR. Kx==18 .OR. Kx==21 .OR. Kx==22 .OR.  &
         & Kx==23 .OR. Kx==24 .OR. Kx==25 .OR. Kx==26 .OR. Kx==27 .OR. Kx==28 .OR. Kx==29 .OR. Kx==30 .OR. Kx==31 .OR. Kx==32 .OR.  &
         & Kx==35 .OR. Kx==36 .OR. Kx==37 .OR. Kx==38 .OR. Kx==39 .OR. Kx==42 .OR. Kx==43 .OR. Kx==44 .OR. Kx==45 .OR. Kx==46 .OR.  &
         & Kx==47 .OR. Kx==48 .OR. Kx==49 .OR. Kx==50 .OR. Kx==51 .OR. Kx==52 .OR. Kx==53 .OR. Kx==54 .OR. Kx==55 .OR. Kx==56 .OR.  &
         & Kx==57 .OR. Kx==58 .OR. Kx==59 .OR. Kx==60 .OR. Kx==61 .OR. Kx==63 .OR. Kx==64 .OR. Kx==65 .OR. Kx==66 .OR. Kx==67 .OR.  &
         & Kx==68 .OR. Kx==69 .OR. Kx==70 .OR. Kx==71 .OR. Kx==72 .OR. Kx==73 .OR. Kx==74 .OR. Kx==75 .OR. Kx==76 .OR. Kx==77 .OR.  &
         & Kx==78 .OR. Kx==79 .OR. Kx==80 .OR. Kx==81 .OR. Kx==82 .OR. Kx==83 .OR. Kx==84 .OR. Kx==85 .OR. Kx==86 .OR. Kx==87 .OR.  &
         & Kx==89 .OR. Kx==90 .OR. Kx==93 .OR. Kx==94 .OR. Kx==95 .OR. Kx==96 .OR. Kx==97 .OR. Kx==98 .OR. Kx==99 ) GOTO 100
      IF ( Kx==19 ) THEN
!
!******         119-DMI          ************************************
!
         IF ( Km/=0 ) THEN
            IF ( j0<=0 .OR. j0>ncols ) GOTO 2700
            l1 = 1
            l1f = 0
            l2 = 8
            IF ( ty1==2 .OR. ty1==4 ) l2 = 16
            l = l1
            GOTO 2200
         ELSEIF ( Fphys ) THEN
            IF ( P(1)>1 ) Dmiflg = .TRUE.
            Fphys = .FALSE.
            nm(1) = 0
            nm(2) = 0
            IF ( Bandit/=-1 .AND. Bandit/=-2 ) Bandit = +9
            GOTO 1900
         ELSEIF ( M(1)==nm(1) .AND. M(2)==nm(2) ) THEN
            IF ( .NOT.ec ) GOTO 2700
            IF ( flush ) GOTO 2800
            ec = .FALSE.
            IF ( M(3)<=j0 ) GOTO 2700
            DO
               j0 = j0 + 1
               IF ( M(3)==j0 ) THEN
                  I0 = 1
                  l1 = 4
                  l1f = -1
                  l2 = 9
                  IF ( ty1==2 .OR. ty1==4 ) l2 = 14
                  IF ( Mf(3)/=1 .OR. M(4)<I0 ) GOTO 2700
                  I0 = M(4) - 1
                  int = .FALSE.
                  CALL bldpk(ty1,ty2,iscr1,0,0)
                  l = l1
                  GOTO 2200
               ELSE
                  CALL bldpk(ty1,ty2,iscr1,0,0)
                  CALL bldpkn(iscr1,0,t)
               ENDIF
            ENDDO
         ELSE
            ASSIGN 1900 TO r
            GOTO 3000
         ENDIF
      ELSEIF ( Kx==20 ) THEN
         GOTO 3200
      ELSEIF ( Kx==33 .OR. Kx==34 .OR. Kx==40 .OR. Kx==62 .OR. Kx==91 ) THEN
         GOTO 1100
      ELSEIF ( Kx==41 ) THEN
         GOTO 1700
      ELSEIF ( Kx==88 ) THEN
!
!*****     188-TABRNDG       **************************************
!
         IF ( M(1)<0 ) GOTO 200
         IF ( M(2)<1 .OR. M(2)>2 ) GOTO 200
         I(1) = M(1)
         I(2) = M(2)
         I(3) = M(3)
         I(4) = M(4)
         I(5) = 0
         I(6) = 0
         I(7) = 0
         I(8) = 0
         I(9) = -1
         I(10) = -1
         N = 10
         GOTO 400
      ELSEIF ( Kx==92 ) THEN
!
!*******      192-PLOAD4     ****************************************
!
         IF ( Km==1 ) THEN
!
            IF ( Mf(1)>1 ) Badfor = .TRUE.
            DO l = 2 , 4
               IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) Badfor = .TRUE.
            ENDDO
            IF ( Mf(1)==0 ) M(1) = 0
            IF ( M(1)<0 ) Baddat = .TRUE.
            DO l = 1 , 4
               I(l) = M(l)
            ENDDO
            N = 4
            Km = 0
            Kn = 0
         ELSE
            Km = 1
            Kn = 1
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            IF ( Mf(2)==2 .AND. Mf(3)==1 .AND. Mf(4)==0 .AND. Mf(5)==0 .AND. Mf(6)==0 ) THEN
!
!     SPECIAL - ALLOWING PLOAD4 TO TAKE ON PLOAD2 FORMAT
!     (PLOAD4,SID,P1,E1,blank,blank,blank,"THRU",E2) FOR QUICK INPUT
!     DATA SWITCHING.  INTERCHAGNE 2ND AND 3RD FIELDS
!
               Mf(2) = 1
               Mf(3) = 2
               l = M(2)
               M(2) = M(3)
               M(3) = l
            ENDIF
            IF ( Mf(2)/=1 ) Badfor = .TRUE.
            DO l = 3 , 6
               IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) Badfor = .TRUE.
            ENDDO
            IF ( Mf(7)/=3 .AND. Mf(7)/=0 .AND. .NOT.(Mf(7)==1 .AND. M(7)==0) ) Badfor = .TRUE.
            IF ( Mf(8)/=1 .AND. Mf(8)/=0 ) Badfor = .TRUE.
            IF ( Mf(7)==0 .AND. Mf(8)/=0 ) Badfor = .TRUE.
            IF ( Mf(7)==3 .AND. Mf(8)/=1 ) Badfor = .TRUE.
            IF ( M(1)<=0 ) Baddat = .TRUE.
            IF ( M(2)<=0 ) Baddat = .TRUE.
            IF ( Mf(7)==3 .AND. M(7)/=thru ) Baddat = .TRUE.
            IF ( Mf(7)==3 .AND. M(9)<=0 ) Baddat = .TRUE.
            IF ( Mf(7)==3 .AND. M(9)<=M(2) ) Baddat = .TRUE.
            l1 = 0
            IF ( Mf(7)==3 ) l1 = 1
            DO l = 1 , 6
               I(l) = M(l)
            ENDDO
            I(7) = -1
            IF ( l1==1 ) I(7) = 0
            I(8) = M(l1+8)
            N = 8
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
               DO l = 9 , 12
                  I(l) = 0
               ENDDO
               N = 12
               Km = 0
               Kn = 0
            ENDIF
         ENDIF
         GOTO 500
      ELSEIF ( Kx==100 ) THEN
!
!*******        200 - DTI       ****************************************
!
         IF ( Km/=0 ) THEN
            l1 = 1
            l1f = 0
            GOTO 4300
         ELSEIF ( Fphys2 ) THEN
            IF ( P(1)>1 ) Dmiflg = .TRUE.
            Fphys2 = .FALSE.
            nm(1) = 0
            nm(2) = 0
            GOTO 4000
         ELSEIF ( M(1)==nm(1) .AND. M(2)==nm(2) ) THEN
            j0 = j0 + 1
            IF ( M(3)/=j0 ) THEN
               flush = .TRUE.
               GOTO 4400
            ELSE
               l1 = 4
               l1f = -1
               GOTO 4300
            ENDIF
         ELSE
            ASSIGN 4000 TO r
            GOTO 4600
         ENDIF
      ENDIF
   ENDIF
   IF ( Ky<=100 ) THEN
      IF ( Ky==1 .OR. Ky==2 .OR. Ky==3 .OR. Ky==4 .OR. Ky==5 .OR. Ky==6 .OR. Ky==7 .OR. Ky==8 .OR. Ky==9 .OR. Ky==10 .OR.           &
         & Ky==11 .OR. Ky==12 .OR. Ky==13 .OR. Ky==14 .OR. Ky==15 .OR. Ky==16 .OR. Ky==17 .OR. Ky==18 .OR. Ky==19 .OR. Ky==20 .OR.  &
         & Ky==21 .OR. Ky==22 .OR. Ky==23 .OR. Ky==24 .OR. Ky==25 .OR. Ky==26 .OR. Ky==27 .OR. Ky==28 .OR. Ky==29 .OR. Ky==30 .OR.  &
         & Ky==31 .OR. Ky==32 .OR. Ky==33 .OR. Ky==34 .OR. Ky==35 .OR. Ky==36 .OR. Ky==37 .OR. Ky==38 .OR. Ky==39 .OR. Ky==40 .OR.  &
         & Ky==41 .OR. Ky==42 .OR. Ky==43 .OR. Ky==44 .OR. Ky==45 .OR. Ky==46 .OR. Ky==47 .OR. Ky==48 .OR. Ky==49 .OR. Ky==50 .OR.  &
         & Ky==51 .OR. Ky==52 .OR. Ky==53 .OR. Ky==54 .OR. Ky==55 .OR. Ky==56 .OR. Ky==57 .OR. Ky==58 .OR. Ky==59 .OR. Ky==60 .OR.  &
         & Ky==63 .OR. Ky==64 .OR. Ky==65 .OR. Ky==66 .OR. Ky==67 .OR. Ky==68 .OR. Ky==69 .OR. Ky==70 .OR. Ky==71 .OR. Ky==72 .OR.  &
         & Ky==73 .OR. Ky==74 .OR. Ky==75 .OR. Ky==76 .OR. Ky==77 .OR. Ky==78 .OR. Ky==79 .OR. Ky==84 .OR. Ky==85 .OR. Ky==86 .OR.  &
         & Ky==87 .OR. Ky==88 .OR. Ky==89 .OR. Ky==90 .OR. Ky==91 .OR. Ky==92 .OR. Ky==93 .OR. Ky==94 .OR. Ky==95 .OR. Ky==96 .OR.  &
         & Ky==97 .OR. Ky==98 .OR. Ky==99 .OR. Ky==100 ) GOTO 100
      IF ( Ky==61 ) THEN
!
!*******       261-CQUAD4    ****************************************
!
         IF ( Mf(2)==0 ) M(2) = M(1)
         I(1) = M(1)
         DO l = 2 , 6
            IF ( Mf(l)/=1 ) Badfor = .TRUE.
            IF ( M(l)<=0 ) Baddat = .TRUE.
            I(l) = M(l)
         ENDDO
         l1 = 6
         DO l = 11 , 14
            l1 = l1 + 1
            I(l1) = M(l)
         ENDDO
         IF ( Mf(7)/=1 .AND. Mf(7)/=2 .AND. Mf(7)/=0 ) Badfor = .TRUE.
         IF ( Mf(7)==1 .AND. (M(7)<0 .OR. M(7)>=1000000) ) Baddat = .TRUE.
         I(11) = M(7)
         I(12) = 0
         IF ( Mf(7)==1 ) I(12) = 1
         I(13) = M(8)
         N = 13
         GOTO 500
      ELSEIF ( Ky==62 ) THEN
!
!*******        262-MAT8      ****************************************
!
         IF ( Mf(2)==0 .OR. Mf(3)==0 .OR. Mf(5)==0 ) THEN
            Badfor = .TRUE.
            RETURN 1
         ELSE
            IF ( M(1)<=0 ) GOTO 200
            IF ( Xm(2)==0.0 .OR. Xm(3)==0.0 ) GOTO 200
            IF ( Xm(5)<=0.0 ) GOTO 200
            IF ( Mf(12)==2 .AND. Xm(12)<=0.0 ) GOTO 200
            IF ( Mf(14)==2 .AND. Xm(14)<=0.0 ) GOTO 200
            IF ( Mf(16)==2 .AND. Xm(16)<=0.0 ) GOTO 200
            IF ( Mf(13)==0 ) Xm(13) = Xm(12)
            IF ( Mf(15)==0 ) Xm(15) = Xm(14)
            N = 18
            GOTO 300
         ENDIF
      ELSEIF ( Ky==80 ) THEN
!
!*******        280-PCOMP     ****************************************
!
         Kn = 1
         IF ( icomp>1 ) THEN
!
            N = 0
            DO l = 1 , 2
               l1 = 4*(l-1)
               l2 = l1
               DO l3 = 1 , 4
                  IF ( Mf(l1+l3)/=0 ) GOTO 5
               ENDDO
               IF ( l==1 ) Badfor = .TRUE.
               GOTO 4900
 5             IF ( l==2 .AND. Mf(4)==3 ) l2 = l2 + 1
               IF ( icomp==3 ) THEN
                  IF ( Mf(l1+1)/=1 .AND. Mf(l1+1)/=0 ) Badfor = .TRUE.
                  IF ( Mf(l1+2)/=2 .AND. Mf(l1+2)/=0 ) Badfor = .TRUE.
                  IF ( Mf(l1+3)/=2 .AND. Mf(l1+3)/=0 ) Badfor = .TRUE.
                  IF ( Mf(l1+1)==1 .AND. M(l2+1)<=0 ) Baddat = .TRUE.
                  IF ( Mf(l1+1)==0 ) M(l2+1) = iold1
                  IF ( Mf(l1+2)==2 .AND. Xm(l2+2)<=0.0 ) Baddat = .TRUE.
                  IF ( Mf(l1+2)==0 ) M(l2+2) = iold2
                  IF ( Mf(l1+3)==0 ) M(l2+3) = iold3
               ELSE
                  icomp = 3
                  IF ( Mf(1)/=1 ) Badfor = .TRUE.
                  IF ( Mf(2)/=2 ) Badfor = .TRUE.
                  IF ( Mf(3)/=2 .AND. Mf(3)/=0 ) Badfor = .TRUE.
                  IF ( M(1)<=0 ) Baddat = .TRUE.
                  IF ( Xm(2)<=0.0 ) Baddat = .TRUE.
               ENDIF
               IF ( Mf(l1+4)/=3 .AND. Mf(l1+4)/=0 ) Badfor = .TRUE.
               IF ( Mf(l1+4)==3 .AND. (M(l2+4)/=iyes .AND. M(l2+4)/=ino) ) Baddat = .TRUE.
               iout = 0
               IF ( M(l2+4)==iyes ) iout = 1
               I(N+1) = M(l2+1)
               I(N+2) = M(l2+2)
               I(N+3) = M(l2+3)
               I(N+4) = iout
               iold1 = M(l2+1)
               iold2 = M(l2+2)
               iold3 = M(l2+3)
               N = N + 4
            ENDDO
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 4900
            GOTO 500
         ELSE
            icomp = 2
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            IF ( Mf(3)/=2 .AND. Mf(3)/=0 ) Badfor = .TRUE.
            IF ( Mf(4)/=2 .AND. Mf(4)/=0 ) Badfor = .TRUE.
            IF ( Mf(5)/=3 .AND. Mf(5)/=0 ) Badfor = .TRUE.
            l = 0
            IF ( Mf(5)==3 ) l = 1
            IF ( Mf(6)/=0 ) Badfor = .TRUE.
            IF ( Mf(7)/=0 ) Badfor = .TRUE.
            IF ( Mf(8)/=3 .AND. Mf(8)/=0 ) Badfor = .TRUE.
            IF ( M(1)<=0 .OR. M(1)>=1000000 ) Baddat = .TRUE.
            IF ( Mf(5)==3 .AND. Xm(4)<=0.0 ) Baddat = .TRUE.
            failur = -1
            IF ( Mf(5)==0 ) failur = 0
            IF ( failur/=0 ) THEN
               IF ( M(5)==ihill(1) .AND. M(6)==ihill(2) ) failur = 1
               IF ( M(5)==ihoff(1) .AND. M(6)==ihoff(2) ) failur = 2
               IF ( M(5)==itsai(1) .AND. M(6)==itsai(2) ) failur = 3
               IF ( M(5)==istrs(1) .AND. M(6)==istrs(2) ) failur = 4
               IF ( M(5)==istrn(1) .AND. M(6)==istrn(2) ) failur = 5
               IF ( failur==-1 ) Baddat = .TRUE.
            ENDIF
            lamopt = -1
            IF ( Mf(8)==0 ) lamopt = 0
            IF ( lamopt/=0 ) THEN
               IF ( M(l+8)==iall(1) .AND. M(l+9)==iall(2) ) lamopt = 0
               IF ( M(l+8)==isym(1) .AND. M(l+9)==isym(2) ) lamopt = 1
               IF ( M(l+8)==imem(1) .AND. M(l+9)==imem(2) ) lamopt = 2
               IF ( M(l+8)==isymm(1) .AND. M(l+9)==isymm(2) ) lamopt = 3
               IF ( lamopt==-1 ) Baddat = .TRUE.
            ENDIF
            IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
               I(1) = M(1)
               I(2) = M(2)
               I(3) = M(3)
               I(4) = M(4)
               I(5) = failur
               I(6) = 0
               I(7) = 0
               I(8) = lamopt
               N = 8
            ELSE
               Badfor = .TRUE.
               Kn = 0
               icomp = 1
               N = 0
            ENDIF
            GOTO 500
         ENDIF
      ELSEIF ( Ky==81 ) THEN
!
!*******        281-PCOMP1    ****************************************
!
         Kn = 1
         IF ( icomp>1 ) THEN
!
            N = 0
            DO l = 1 , 8
               IF ( Mf(l)/=0 ) THEN
                  IF ( icomp==3 ) THEN
                     IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) Badfor = .TRUE.
                     IF ( Mf(l)==0 ) M(l) = iold1
                  ELSE
                     icomp = 3
                     IF ( Mf(1)/=2 ) Badfor = .TRUE.
                  ENDIF
                  I(N+1) = M(l)
                  iold1 = M(l)
                  N = N + 1
               ELSE
                  IF ( l==1 ) Badfor = .TRUE.
                  GOTO 5000
               ENDIF
            ENDDO
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 5000
            GOTO 500
         ELSE
            icomp = 2
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            IF ( Mf(3)/=2 .AND. Mf(3)/=0 ) Badfor = .TRUE.
            IF ( Mf(4)/=2 .AND. Mf(4)/=0 ) Badfor = .TRUE.
            IF ( Mf(5)/=3 .AND. Mf(5)/=0 ) Badfor = .TRUE.
            l = 0
            IF ( Mf(5)==3 ) l = 1
            IF ( Mf(6)/=1 ) Badfor = .TRUE.
            IF ( Mf(7)/=2 ) Badfor = .TRUE.
            IF ( Mf(8)/=3 .AND. Mf(8)/=0 ) Badfor = .TRUE.
            IF ( M(1)<=0 .OR. M(1)>=1000000 ) Baddat = .TRUE.
            IF ( Mf(5)==3 .AND. Xm(4)<=0.0 ) Baddat = .TRUE.
            failur = -1
            IF ( Mf(5)==0 ) failur = 0
            IF ( failur/=0 ) THEN
               IF ( M(5)==ihill(1) .AND. M(6)==ihill(2) ) failur = 1
               IF ( M(5)==ihoff(1) .AND. M(6)==ihoff(2) ) failur = 2
               IF ( M(5)==itsai(1) .AND. M(6)==itsai(2) ) failur = 3
               IF ( M(5)==istrs(1) .AND. M(6)==istrs(2) ) failur = 4
               IF ( M(5)==istrn(1) .AND. M(6)==istrn(2) ) failur = 5
               IF ( failur==-1 ) Baddat = .TRUE.
            ENDIF
            IF ( M(l+6)<=0 ) Baddat = .TRUE.
            IF ( Xm(l+7)<=0.0 ) Baddat = .TRUE.
            lamopt = -1
            IF ( Mf(8)==0 ) lamopt = 0
            IF ( lamopt/=0 ) THEN
               IF ( M(l+8)==iall(1) .AND. M(l+9)==iall(2) ) lamopt = 0
               IF ( M(l+8)==isym(1) .AND. M(l+9)==isym(2) ) lamopt = 1
               IF ( M(l+8)==imem(1) .AND. M(l+9)==imem(2) ) lamopt = 2
               IF ( M(l+8)==isymm(1) .AND. M(l+9)==isymm(2) ) lamopt = 3
               IF ( lamopt==-1 ) Baddat = .TRUE.
            ENDIF
            IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
               I(1) = M(1)
               I(2) = M(2)
               I(3) = M(3)
               I(4) = M(4)
               I(5) = failur
               I(6) = M(l+6)
               I(7) = M(l+7)
               I(8) = lamopt
               N = 8
            ELSE
               Badfor = .TRUE.
               Kn = 0
               icomp = 1
               N = 0
            ENDIF
            GOTO 500
         ENDIF
      ELSEIF ( Ky==82 ) THEN
!
!*******        282-PCOMP2    ****************************************
!
         Kn = 1
         IF ( icomp>1 ) THEN
!
            N = 0
            DO l = 1 , 4
               l1 = 2*(l-1)
               DO l3 = 1 , 2
                  IF ( Mf(l1+l3)/=0 ) GOTO 10
               ENDDO
               IF ( l==1 ) Badfor = .TRUE.
               GOTO 5100
 10            IF ( icomp==3 ) THEN
                  IF ( Mf(l1+1)/=2 .AND. Mf(l1+1)/=0 ) Badfor = .TRUE.
                  IF ( Mf(l1+2)/=2 .AND. Mf(l1+2)/=0 ) Badfor = .TRUE.
                  IF ( Mf(l1+1)==2 .AND. Xm(l1+1)<=.0 ) Baddat = .TRUE.
                  IF ( Mf(l1+1)==0 ) M(l1+1) = iold1
                  IF ( Mf(l1+2)==0 ) M(l1+2) = iold2
               ELSE
                  icomp = 3
                  IF ( Mf(1)/=2 ) Badfor = .TRUE.
                  IF ( Mf(2)/=2 ) Badfor = .TRUE.
                  IF ( Xm(1)<=0.0 ) Baddat = .TRUE.
               ENDIF
               I(N+1) = M(l1+1)
               I(N+2) = M(l1+2)
               iold1 = M(l1+1)
               iold2 = M(l1+2)
               N = N + 2
            ENDDO
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) GOTO 5100
            GOTO 500
         ELSE
            icomp = 2
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            IF ( Mf(3)/=2 .AND. Mf(3)/=0 ) Badfor = .TRUE.
            IF ( Mf(4)/=2 .AND. Mf(4)/=0 ) Badfor = .TRUE.
            IF ( Mf(5)/=3 .AND. Mf(5)/=0 ) Badfor = .TRUE.
            l = 0
            IF ( Mf(5)==3 ) l = 1
            IF ( Mf(6)/=1 ) Badfor = .TRUE.
            IF ( Mf(7)/=0 ) Badfor = .TRUE.
            IF ( Mf(8)/=3 .AND. Mf(8)/=0 ) Badfor = .TRUE.
            IF ( M(1)<=0 .OR. M(1)>=1000000 ) Baddat = .TRUE.
            IF ( Mf(5)==3 .AND. Xm(4)<=0.0 ) Baddat = .TRUE.
            failur = -1
            IF ( Mf(5)==0 ) failur = 0
            IF ( failur/=0 ) THEN
               IF ( M(5)==ihill(1) .AND. M(6)==ihill(2) ) failur = 1
               IF ( M(5)==ihoff(1) .AND. M(6)==ihoff(2) ) failur = 2
               IF ( M(5)==itsai(1) .AND. M(6)==itsai(2) ) failur = 3
               IF ( M(5)==istrs(1) .AND. M(6)==istrs(2) ) failur = 4
               IF ( M(5)==istrn(1) .AND. M(6)==istrn(2) ) failur = 5
               IF ( failur==-1 ) Baddat = .TRUE.
            ENDIF
            IF ( M(l+6)<=0 ) Baddat = .TRUE.
            lamopt = -1
            IF ( Mf(8)==0 ) lamopt = 0
            IF ( lamopt/=0 ) THEN
               IF ( M(l+8)==iall(1) .AND. M(l+9)==iall(2) ) lamopt = 0
               IF ( M(l+8)==isym(1) .AND. M(l+9)==isym(2) ) lamopt = 1
               IF ( M(l+8)==imem(1) .AND. M(l+9)==imem(2) ) lamopt = 2
               IF ( M(l+8)==isymm(1) .AND. M(l+9)==isymm(2) ) lamopt = 3
               IF ( lamopt==-1 ) Baddat = .TRUE.
            ENDIF
            IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
               I(1) = M(1)
               I(2) = M(2)
               I(3) = M(3)
               I(4) = M(4)
               I(5) = failur
               I(6) = M(l+6)
               I(7) = 0
               I(8) = lamopt
               N = 8
            ELSE
               Badfor = .TRUE.
               Kn = 0
               icomp = 1
               N = 0
            ENDIF
            GOTO 500
         ENDIF
      ELSEIF ( Ky==83 ) THEN
!
!*******        283-PSHELL    ****************************************
!
         IF ( Km==1 ) THEN
!
            IF ( Mf(1)/=2 .AND. Mf(1)/=0 ) Badfor = .TRUE.
            IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            IF ( Mf(3)/=1 .AND. Mf(3)/=0 ) Badfor = .TRUE.
            IF ( Mf(4)/=1 .AND. Mf(4)/=2 .AND. Mf(4)/=0 ) Badfor = .TRUE.
            IF ( Mf(5)/=1 .AND. Mf(5)/=2 .AND. Mf(5)/=0 ) Badfor = .TRUE.
            IF ( Mf(6)/=2 .AND. Mf(6)/=0 ) Badfor = .TRUE.
            IF ( Mf(1)==0 ) Xm(1) = -0.5*oldxm3
            IF ( Mf(2)==0 ) Xm(2) = 0.5*oldxm3
            IF ( Mf(3)==1 .AND. M(3)<=0 ) Baddat = .TRUE.
            IF ( Mf(3)/=0 .AND. (iolmf2==0 .OR. iolmf4==0) ) Baddat = .TRUE.
            IF ( Mf(3)/=0 .AND. (M(3)==ioldm2 .OR. M(3)==ioldm4) ) Baddat = .TRUE.
            IF ( Mf(4)==1 .AND. M(4)<0 ) Baddat = .TRUE.
            IF ( Mf(5)==1 .AND. M(5)<0 ) Baddat = .TRUE.
            IF ( ioldm2==0 .AND. ioldm4==0 .AND. ioldm6==0 .AND. M(3)==0 ) Baddat = .TRUE.
            DO l = 1 , 4
               I(l) = M(l)
            ENDDO
            I(5) = 0
            IF ( Mf(4)==1 ) I(5) = 1
!
!     I(6) IS THE INTEGRATION ORDER (SET TO 0)
!
!     NOTE
!     ----
!
!     THE INTEGRATION ORDER IS NOT USED IN THE PROGRAM,
!     BUT THIS WORD IS REQUIRED BECAUSE OF THE DESIGN
!     OF THE EST DATA FOR THE CQUAD4 ELEMENT.
!
            I(6) = 0
            I(7) = M(5)
            I(8) = 0
            IF ( Mf(5)==1 ) I(8) = 1
            I(9) = M(6)
            N = 9
            Km = 0
            Kn = 0
         ELSE
            Km = 1
            Kn = 1
            IF ( Mf(1)/=1 ) Badfor = .TRUE.
            IF ( Mf(2)/=1 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            IF ( Mf(3)/=2 .AND. Mf(3)/=0 ) Badfor = .TRUE.
            IF ( Mf(4)/=1 .AND. Mf(4)/=0 ) Badfor = .TRUE.
            IF ( Mf(5)/=2 .AND. Mf(5)/=0 ) Badfor = .TRUE.
            IF ( Mf(6)/=1 .AND. Mf(6)/=0 ) Badfor = .TRUE.
            IF ( Mf(7)/=2 .AND. Mf(7)/=0 ) Badfor = .TRUE.
            IF ( Mf(8)/=2 .AND. Mf(8)/=0 ) Badfor = .TRUE.
            IF ( M(1)<=0 ) Baddat = .TRUE.
            IF ( Mf(2)==1 .AND. M(2)<=0 ) Baddat = .TRUE.
            IF ( Mf(4)==1 .AND. M(4)<=0 ) Baddat = .TRUE.
            IF ( Mf(4)/=0 .AND. Mf(5)==0 ) Xm(5) = 1.0
            IF ( Mf(6)==1 .AND. M(6)<=0 ) Baddat = .TRUE.
            IF ( Mf(6)/=0 .AND. Mf(4)==0 ) Baddat = .TRUE.
            IF ( Mf(6)/=0 .AND. Mf(7)==0 ) Xm(7) = 0.833333
            DO l = 2 , 6 , 2
               IF ( M(l)==0 .AND. Xm(l+1)>0.0 ) Baddat = .TRUE.
            ENDDO
            DO l = 1 , 8
               I(l) = M(l)
            ENDDO
            iolmf2 = Mf(2)
            iolmf4 = Mf(4)
            ioldm2 = M(2)
            ioldm4 = M(4)
            ioldm6 = M(6)
            oldxm3 = Xm(3)
            N = 8
            IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
               Z(9) = -0.5*oldxm3
               Z(10) = 0.5*oldxm3
               DO l = 11 , 17
                  I(l) = 0
               ENDDO
               N = 17
               Km = 0
               Kn = 0
            ENDIF
         ENDIF
         GOTO 500
      ENDIF
   ENDIF
   kz = K - 300
   IF ( kz<=60 ) THEN
      IF ( kz==54 ) THEN
!
!*******       354-CTRIA3      **************************************
!
         IF ( Mf(2)==0 ) M(2) = M(1)
         I(1) = M(1)
         DO l = 2 , 5
            IF ( Mf(l)/=1 ) Badfor = .TRUE.
            IF ( M(l)<=0 ) Baddat = .TRUE.
            I(l) = M(l)
         ENDDO
         IF ( Mf(6)/=1 .AND. Mf(6)/=2 .AND. Mf(6)/=0 ) Badfor = .TRUE.
         IF ( Mf(6)==1 .AND. (M(6)<0 .OR. M(6)>=1000000) ) Baddat = .TRUE.
         I(6) = M(11)
         I(7) = M(12)
         I(8) = M(13)
         I(9) = M(6)
         I(10) = 0
         I(11) = M(7)
         IF ( Mf(6)==1 ) I(10) = 1
         N = 11
         GOTO 500
      ELSEIF ( kz==57 ) THEN
         GOTO 1100
      ENDIF
   ENDIF
 100  CALL page2(2)
   WRITE (Nout,99002) Sfm
99002 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS2P.')
   Abort = .TRUE.
   RETURN 1
 200  Baddat = .TRUE.
   RETURN 1
 300  DO l = 1 , N
      I(l) = M(l)
   ENDDO
 400  RETURN
 500  RETURN 3
 600  nm(1) = eigr
   nm(2) = bcdmas
   IF ( K/=85 ) THEN
      nm(1) = eigb
      nm(2) = bcdmax
   ENDIF
   M(10) = nm(2)
   M(12) = 0
   M(13) = 0
   CALL mesage(30,222,nm)
 700  IF ( M(6)==0 .AND. M(2)/=bcdgiv .AND. M(2)/=bcdmgv .AND. M(2)/=bcdfer ) GOTO 200
   IF ( K==86 .AND. (M(2)==bcdgiv .OR. M(2)==bcdmgv) ) GOTO 200
   IF ( (M(2)==bcddet .OR. M(2)==bcdsdt) .AND. Xm(4)<0.0 ) GOTO 200
   IF ( M(2)==bcdudt .AND. Xm(4)<0.0 ) GOTO 200
   IF ( K==85 .AND. M(2)/=bcdgiv .AND. M(2)/=bcdmgv .AND. Xm(4)<0.0 ) GOTO 200
   IF ( M(2)/=bcdgiv .AND. M(2)/=bcdmgv .AND. M(2)/=bcdfer .AND. Xm(5)<=0.0 ) GOTO 200
   IF ( M(2)/=bcdgiv .AND. M(2)/=bcdmgv .AND. M(2)/=bcdfer .AND. Xm(4)>=Xm(5) ) GOTO 200
   N = 18
   GOTO 300
 800  DO l = 1 , N
      I(l) = M(l)
   ENDDO
   GOTO 1000
 900  Baddat = .TRUE.
 1000 IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Kn = 1
      Km = 1
   ELSE
      DO l = 1 , 7
         N = N + 1
         I(N) = -1
      ENDDO
      Km = 0
      Kn = 0
   ENDIF
   GOTO 500
!
!*******       93- TABLEM1, 94-TABLEM2, 95-TABLEM3  ********************
!              133-TABLED1,134-TABLED2,140-TABLED3
!              162-TABDMP1, 97-TABLES1,191-TABRND1
!              357-TABLEM5
!                 (TABLEM5 IS DESIGNED FOR THERMAL COEFFICIENT WHICH IS
!                          FUNCTION OF TIME
!                          THIS PROJECT TEMPORARY HALTS HERE  6/90)
!
 1100 IF ( Km/=0 ) THEN
      l1 = 0
      DO l = 1 , 7 , 2
         IF ( Mf(l)==3 .OR. Mf(l+1)==3 ) THEN
            IF ( Mf(l)==3 ) THEN
               l1 = l1 + 2
               lp1 = l1 - 1
               kword1 = M(lp1)
            ELSE
               l1 = l1 + 1
               lp1 = l1
               kword1 = 0
            ENDIF
            IF ( Mf(l+1)==3 ) THEN
               l1 = l1 + 2
               lp2 = l1 - 1
               kword2 = M(lp2)
            ELSE
               l1 = l1 + 1
               lp2 = l1
               kword2 = 0
            ENDIF
            IF ( kword1==endt .OR. kword2==endt ) GOTO 1400
            IF ( kword1/=skip .AND. kword2/=skip ) THEN
               Baddat = .TRUE.
               EXIT
            ENDIF
         ELSE
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 .OR. Mf(l+1)/=0 .AND. Mf(l+1)/=2 ) GOTO 1200
            items = items + 1
            N = N + 2
            l1 = l1 + 2
            I(N-1) = M(l1-1)
            I(N) = M(l1)
            IF ( items>2 ) THEN
               xl1 = xl
               xl = Z(N-1)
               zseq1 = sign(1.0,xl-xl1)
               IF ( zseq1/=zseq .AND. xl/=xl1 ) Baddat = .TRUE.
            ELSEIF ( items>1 ) THEN
               x2 = Z(N-1)
               xl1 = xl
               xl = x2
               zseq = sign(1.0,x2-x1)
               IF ( x2==x1 ) Baddat = .TRUE.
            ELSE
               x1 = Z(N-1)
               xl = x1
            ENDIF
         ENDIF
      ENDDO
      GOTO 1300
   ELSE
      i2 = M(1)
      items = 0
      N = 8
      IF ( M(1)<=0 ) Baddat = .TRUE.
      IF ( Mf(1)/=1 ) Badfor = .TRUE.
      I(1) = i2
      DO l = 2 , 7
         IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) Badfor = .TRUE.
         I(l) = M(l)
      ENDDO
!
!     LOGARITHMIC SCALE
!     I(8) = 0, LINEAR-LINEAR SCALE (ALL TABLES)
!          = 1, LOG-LOG SCALE (TABLE-1 ONLY)
!          = 2, LINEAR-LOG SCALE (TABLE-1, TABLE-2 AND TABLE-3)
!          = 3, LOG-LINEAR SCALE (TABLE-1 ONLY)
!     TABLE-1 INCLUDES TABLED1, TABLEM1, TABLES1, TABDMP1 AND TABRND1
!     TABLE-2 INCLUDES TABLED2 AND TABLEM2
!
      I(8) = 0
      IF ( Mf(8)==3 ) THEN
         IF ( M(8)==bcdll ) I(8) = 1
         IF ( M(8)==bcdsl ) I(8) = 2
         IF ( M(8)==bcdls ) I(8) = 3
         IF ( M(8)/=bcdsl .AND. (K==94 .OR. K==95 .OR. K==134 .OR. K==140) ) Baddat = .TRUE.
      ENDIF
!
      IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
         Baddat = .TRUE.
         Kn = 0
         Km = 0
      ELSE
         Kn = 1
         Km = 1
      ENDIF
      GOTO 1600
   ENDIF
 1200 Badfor = .TRUE.
 1300 IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
      Kn = 0
      Km = 0
      Baddat = .TRUE.
   ENDIF
   GOTO 1600
 1400 N = N + 2
   I(N-1) = -1
   I(N) = -1
   IF ( xl==xl1 ) Baddat = .TRUE.
   IF ( items<2 ) Baddat = .TRUE.
 1500 IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
      Kn = 1
      Km = 1
      Baddat = .TRUE.
   ELSE
      Kn = 0
      Km = 0
   ENDIF
 1600 IF ( .NOT.(Baddat .OR. Badfor) ) GOTO 400
   M(1) = i2
   GOTO 200
!
!*******      96-TABLEM4, 141-TABLED4     ******************************
!
 1700 IF ( Km/=0 ) THEN
      l1 = 0
      DO l = 1 , 8
         kword1 = 0
         IF ( Mf(l)==3 ) THEN
            l1 = l1 + 2
            kword1 = M(l1-1)
            IF ( kword1==endt ) GOTO 1800
            Baddat = .TRUE.
            EXIT
         ELSE
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) GOTO 1200
            N = N + 1
            items = items + 1
            l1 = l1 + 1
            I(N) = M(l1)
         ENDIF
      ENDDO
      GOTO 1300
   ELSE
      items = 0
      i2 = M(1)
      N = 8
      IF ( M(1)<=0 ) Baddat = .TRUE.
      IF ( Mf(1)/=1 ) Badfor = .TRUE.
      I(1) = i2
      IF ( M(3)==0 ) Baddat = .TRUE.
      DO l = 2 , 8
         IF ( Mf(l)/=0 .AND. Mf(l)/=2 .OR. l>=6 .AND. Mf(l)/=0 ) Badfor = .TRUE.
         I(l) = M(l)
      ENDDO
      I(8) = 0
      IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
         Baddat = .TRUE.
         Kn = 0
         Km = 0
      ELSE
         Kn = 1
         Km = 1
      ENDIF
      GOTO 1600
   ENDIF
 1800 N = N + 1
   I(N) = -1
   IF ( items<1 ) Baddat = .TRUE.
   GOTO 1500
 1900 flush = .FALSE.
   flshal = .FALSE.
   ec = .TRUE.
   secd = .FALSE.
   t(1) = iscr1
   DO l = 2 , 7
      t(l) = 0
   ENDDO
   IF ( M(3)/=0 ) flush = .TRUE.
   onm(1) = nm(1)
   onm(2) = nm(2)
   IF ( Mf(1)/=3 .OR. M(1)==onm(1) .AND. M(2)==onm(2) ) flush = .TRUE.
   nm(1) = M(1)
   nm(2) = M(2)
   iprint = 0
   j0 = 0
   IF ( P(1)>P(2) ) THEN
      flush = .TRUE.
      flshal = .TRUE.
   ENDIF
   ASSIGN 2000 TO r1
   ASSIGN 2100 TO r
   GOTO 4700
 2000 flush = .TRUE.
 2100 IF ( flush ) GOTO 2800
   ifo = M(4)
   ty1 = M(5)
   ty2 = M(6)
   IF ( ty2==0 .AND. mod(ty1,2)==1 ) ty2 = ty1 + Kprec - 1
   IF ( ty2==0 .AND. mod(ty1,2)==0 ) ty2 = ty1
   IF ( Mach==12 ) THEN
      IF ( ty2==2 .OR. ty2==4 ) ty2 = ty2 - 1
   ENDIF
   IF ( ty1<1 .OR. ty1>4 .OR. ty2<1 .OR. ty2>4 ) GOTO 2700
   IF ( ty1>=3 .AND. ty2<=2 ) WRITE (Nout,99008) Uwm , dmi , nam(1) , nam(2) , Knt
   nrows = M(8)
   ncols = M(9)
   IF ( ifo>8 ) GOTO 2700
   IF ( Mf(6)/=0 ) GOTO 2700
   IF ( nrows<=0 .OR. ncols<=0 ) GOTO 2700
   IF ( (ifo==1 .OR. ifo==6 .OR. ifo==8) .AND. (nrows/=ncols) ) GOTO 2700
   nbuf2 = 2*Nbuf
   CALL open(*3100,iscr1,Ibuf(nbuf2+1),1)
   CALL write(iscr1,nm,2,1)
   IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 2700
   IF ( ifo==8 ) THEN
      IF ( M1(1)/=T1(1,K) .OR. M1(2)/=T1(2,K) .OR. M1(3)/=nm(1) .OR. M1(4)/=nm(2) ) GOTO 2800
      GOTO 2700
   ELSE
      IF ( M1(1)==T1(1,K) .AND. M1(2)==T1(2,K) ) GOTO 2800
      GOTO 2700
   ENDIF
 2200 lf = l + l1f
   IF ( .NOT.(Fthru) ) THEN
      IF ( Mf(lf)==0 ) GOTO 2500
      IF ( Mf(lf)==2 .OR. Mf(lf)==4 ) THEN
         IF ( ty1==2 ) THEN
!   . REAL DOUBLE PRECISION
            IF ( Mf(lf)==2 ) GOTO 2700
            A(1) = M(l)
            A(2) = M(l+1)
            l = l + 1
            l1f = l1f - 1
!
!     PACK AN ELEMENT
!
            IF ( .NOT.(flush .OR. Da(1)==0.0D0) ) CALL zblpki
         ELSEIF ( ty1==3 ) THEN
!   . COMPLEX SINGLE PRECISION
            IF ( Mf(lf)==4 ) GOTO 2700
            IF ( secd ) THEN
               A(2) = M(l)
               secd = .FALSE.
               IF ( .NOT.(A(1)==0 .AND. A(2)==0 .OR. flush) ) CALL zblpki
            ELSE
               A(1) = M(l)
               secd = .TRUE.
               GOTO 2300
            ENDIF
         ELSEIF ( ty1==4 ) THEN
!   . COMPLEX DOUBLE PRECISION
            IF ( Mf(lf)==2 ) GOTO 2700
            IF ( secd ) THEN
               A(3) = M(l)
               A(4) = M(l+1)
               l = l + 1
               l1f = l1f - 1
               secd = .FALSE.
               IF ( .NOT.(flush .OR. Da(1)==0.0D0 .AND. Da(2)==0.0D0) ) CALL zblpki
            ELSE
               A(1) = M(l)
               A(2) = M(l+1)
               l = l + 1
               l1f = l1f - 1
               secd = .TRUE.
               GOTO 2300
            ENDIF
         ELSE
!   . REAL SINGLE PRECISION
            IF ( Mf(lf)==4 ) GOTO 2700
            IF ( .NOT.(flush .OR. M(l)==0) ) THEN
               A(1) = M(l)
               CALL zblpki
            ENDIF
         ENDIF
         int = .FALSE.
         I0 = I0 + 1
         IF ( I0>nrows ) GOTO 2500
         IF ( l+1>l2 ) GOTO 2300
         IF ( Mf(lf+1)/=3 ) GOTO 2300
         l = l + 1
      ELSE
         IF ( Mf(lf)==-32767 ) GOTO 2400
         IF ( int ) GOTO 2700
         IF ( Mf(lf)/=3 ) THEN
            IF ( Mf(lf)/=1 .OR. M(l)<I0 .OR. M(l)>nrows ) GOTO 2700
            I0 = M(l)
            int = .TRUE.
            GOTO 2300
         ENDIF
      ENDIF
      IF ( M(l)/=thru ) GOTO 2700
      Fthru = .TRUE.
      l1f = l1f - 1
      l2 = l2 + 1
      l = l + 1
      IF ( l>=l2 ) GOTO 2400
      l = l + 1
      lf = l + l1f
   ENDIF
   IF ( Mf(lf)/=1 .OR. M(l)<I0 .OR. M(l)>nrows ) GOTO 2700
   DO
      CALL zblpki
      I0 = I0 + 1
      IF ( I0>M(l) ) THEN
         Fthru = .FALSE.
         IF ( I0<=nrows ) EXIT
         GOTO 2500
      ENDIF
   ENDDO
 2300 l = l + 1
   IF ( l<=l2 ) GOTO 2200
 2400 IF ( .NOT.(M1(1)==0 .AND. M1(2)==0 .OR. int) ) GOTO 2600
   GOTO 2800
 2500 IF ( l/=l2 ) THEN
      lf = lf + 1
      DO lx = lf , 8
         IF ( Mf(lx)==-32767 ) EXIT
         IF ( Mf(lx)/=0 ) GOTO 2700
      ENDDO
   ENDIF
   IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 2700
 2600 IF ( flush ) THEN
      ec = .TRUE.
      GOTO 2800
   ELSEIF ( .NOT.(secd) ) THEN
      IF ( .NOT.(Fthru) ) THEN
         CALL bldpkn(iscr1,0,t)
         ec = .TRUE.
         GOTO 2800
      ENDIF
   ENDIF
 2700 flush = .TRUE.
 2800 IF ( .NOT.(M1(1)==0 .AND. M1(2)==0 .OR. M1(1)==T1(1,K) .AND. M1(2)==T1(2,K)) ) THEN
      ASSIGN 2900 TO r
      GOTO 3000
   ENDIF
 2900 N = 0
   IF ( .NOT.(.NOT.flush .OR. iprint/=0) ) THEN
      CALL page2(2)
      WRITE (Nout,99003) Ufm , nm(1) , nm(2) , Knt
99003 FORMAT (A23,' 325, BAD DATA OR FORMAT OR NON-UNIQUE NAME. DMI ',2A4,10X,' SORTED CARD COUNT =',I7)
      iprint = 1
   ENDIF
   GOTO 3700
 3000 IF ( flshal ) THEN
      WRITE (Nout,99004) Sfm , nm(1) , nm(2)
99004 FORMAT (A25,' 326, NO ROOM IN /XDPL/ FOR DMI ',2A4)
      CALL page2(2)
      Abort = .TRUE.
   ELSE
      IF ( .NOT.(flush) ) THEN
         IF ( ifo==8 ) THEN
            t(2) = ncols
         ELSE
            DO
               j0 = j0 + 1
               IF ( j0>ncols ) THEN
                  IF ( ncols==t(2) ) EXIT
                  flush = .TRUE.
                  GOTO 3050
               ELSE
                  CALL bldpk(ty1,ty2,iscr1,0,0)
                  CALL bldpkn(iscr1,0,t)
               ENDIF
            ENDDO
         ENDIF
         CALL close(iscr1,1)
         CALL wrttrl(t)
         CALL rdtrl(t)
         IF ( Icfiat==11 ) THEN
            j = 6
         ELSE
            DO lx = 1 , 3
               t(lx+1) = orf(lshift(t(2*lx),16),t(2*lx+1))
            ENDDO
            j = 3
         ENDIF
         CALL write(pool,nm,2,0)
         CALL write(pool,t(2),j,1)
         IF ( L8/=0 ) WRITE (Nout,99009) nm , dmi , (t(ip+1),ip=1,j)
         CALL gopen(iscr1,Ibuf(2*Nbuf+1),2)
         CALL cpyfil(iscr1,pool,Ibuf(3*Nbuf+1),Nopen,nwords)
         CALL close(iscr1,1)
         CALL eof(pool)
         Dmiflg = .TRUE.
         P(1) = P(1) + 1
      ENDIF
 3050 ip = 3*P(3) + 4
      P(ip) = nm(1)
      P(ip+1) = nm(2)
      IF ( flush ) nwords = 0
      P(ip+2) = orf(lshift(nwords/1000,16),P(1)-1)
      P(3) = P(3) + 1
      IF ( flush ) THEN
         CALL close(iscr1,1)
         CALL eof(pool)
         P(1) = P(1) + 1
         CALL skpfil(pool,-1)
         IF ( Dmiflg ) CALL eof(pool)
         Abort = .TRUE.
      ENDIF
   ENDIF
   GOTO r
 3100 CALL mesage(-1,iscr1,nm)
!
!******          120-DMIG          ********************************
!
 3200 IF ( Fphys1 ) THEN
      Fphys1 = .FALSE.
      nm(1) = 0
      nm(2) = 0
   ENDIF
   ierr = 0
   IF ( Km/=0 ) THEN
      lf = 1
      l = 1
   ELSEIF ( M(3)==0 ) THEN
      IF ( Mf(1)/=3 .OR. M(1)==nm(1) .AND. M(2)==nm(2) ) GOTO 3500
      ifo = M(4)
      ty1 = M(5)
      ity1 = 2*mod(ty1,2)
      ty2 = M(6)
      IF ( ty2==0 .AND. mod(ty1,2)==1 ) ty2 = ty1 + Kprec - 1
      IF ( ty2==0 .AND. mod(ty1,2)==0 ) ty2 = ty1
      IF ( Mach==12 ) THEN
         IF ( ty2==2 .OR. ty2==4 ) ty2 = ty2 - 1
      ENDIF
      IF ( ty1<=0 .OR. ty1>4 .OR. ty2<=0 .OR. ty2>4 ) GOTO 3500
      IF ( ty1>=3 .AND. ty2<=2 ) WRITE (Nout,99008) Uwm , dmig , nm(1) , nm(2) , Knt
      IF ( ifo/=1 .AND. ifo/=2 .AND. ifo/=6 ) GOTO 3500
      IF ( ty2==1 .AND. ty1==3 ) GOTO 3500
      nm(1) = M(1)
      nm(2) = M(2)
      IF ( Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) GOTO 3600
      IF ( M1f(2)/=3 .OR. M1(3)/=nm(1) .OR. M1(4)/=nm(2) ) GOTO 3600
      M(6) = ty2
      N = 9
      GOTO 300
   ELSE
      IF ( M(1)/=nm(1) .OR. M(2)/=nm(2) ) GOTO 3500
      IF ( Mf(2)/=1 .OR. Mf(3)/=1 .AND. Mf(3)/=0 ) GOTO 3500
      IF ( M(3)<=0 .OR. M(4)<0 .OR. M(4)>6 ) GOTO 3500
      IF ( Mf(4)/=0 ) GOTO 3500
      IF ( Mf(5)/=1 .OR. Mf(6)/=1 .AND. Mf(6)/=0 ) GOTO 3500
      IF ( M(6)<=0 .OR. M(7)<0 .OR. M(7)>6 ) GOTO 3500
      IF ( Mf(7)+ity1/=4 .AND. Mf(7)/=0 ) GOTO 3500
      IF ( (ty1==1 .OR. ty1==2) .AND. Mf(8)/=0 .OR. ty1==3 .AND. Mf(8)/=2 .AND. Mf(8)/=0 .OR. ty1==4 .AND. Mf(8)/=4 .AND. Mf(8)/=0 )&
         & GOTO 3500
      N = 5
      I(N-4) = M(3)
      I(N-3) = M(4)
      I(N-2) = M(6)
      I(N-1) = M(7)
      I(N) = M(8)
      IF ( ty1/=1 ) THEN
         N = 6
         I(N) = M(9)
         IF ( ty1==4 ) THEN
            N = 8
            I(N-1) = M(10)
            I(N) = M(11)
         ENDIF
      ENDIF
      IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 3900
      N = N + 2
      I(N-1) = -1
      I(N) = -1
      GOTO 3400
   ENDIF
 3300 IF ( M(l)/=0 .OR. M(l+1)/=0 .OR. M(l+2)/=0 .OR. M(l+3)/=0 ) THEN
      IF ( M(l)<=0 .OR. M(l+1)<0 .OR. M(l+1)>6 ) GOTO 3600
      IF ( Mf(lf)/=1 .OR. Mf(lf+1)/=1 .AND. Mf(lf+1)/=0 ) GOTO 3600
      ierr = 1
      IF ( Mf(lf+2)+ity1/=4 .AND. Mf(lf+2)/=0 ) GOTO 3600
      IF ( Mf(lf+3)/=0 .AND. ty1/=3 .AND. ty1/=4 ) GOTO 3600
      N = N + 3
      I(N-2) = M(l)
      I(N-1) = M(l+1)
      I(N) = M(l+2)
      lf = lf + 4
      l = l + 4
      IF ( ty1/=1 ) THEN
         N = N + 1
         I(N) = M(l-1)
         IF ( ty1==2 ) l = l + 1
         IF ( ty1==4 ) THEN
            N = N + 2
            I(N-1) = M(l)
            I(N) = M(l+1)
            l = l + 2
         ENDIF
      ENDIF
   ELSE
      lf = lf + 4
      l = l + 4
   ENDIF
   IF ( lf<=7 ) GOTO 3300
   IF ( N<=0 ) GOTO 3600
   IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 3900
   N = N + 2
   I(N-1) = -1
   I(N) = -1
 3400 IF ( M1(1)/=T1(1,K) .OR. M1(2)/=T1(2,K) .OR. M1(3)/=nm(1) .OR. M1(4)/=nm(2) ) THEN
      N = N + 2
      I(N-1) = -1
      I(N) = -1
   ENDIF
   GOTO 3800
 3500 nm(1) = M(1)
   nm(2) = M(2)
 3600 Abort = .TRUE.
   CALL page2(2)
   WRITE (Nout,99005) Ufm , nm(1) , nm(2) , Knt
99005 FORMAT (A23,' 327, BAD DATA OR FORMAT OR NON-UNIQUE NAME. DMIG ',2A4,10X,' SORTED CARD COUNT =',I7)
   IF ( ierr==1 ) WRITE (Nout,99006)
99006 FORMAT (5X,'INPUT MATRIX TYPE (TIN) AND INPUT DATA (XIJ OR YIJ) ','ARE NOT CONSISTANT')
 3700 IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 3900
 3800 Km = 0
   Kn = 0
   GOTO 400
 3900 Kn = 1
   Km = 1
   GOTO 400
 4000 flush = .FALSE.
   flshal = .FALSE.
   IF ( M(3)/=0 ) flush = .TRUE.
   onm(1) = nm(1)
   onm(2) = nm(2)
   IF ( Mf(1)/=3 .OR. M(1)==onm(1) .AND. M(2)==onm(2) ) flush = .TRUE.
   nm(1) = M(1)
   nm(2) = M(2)
   iprint = 0
   nwords = 2
   j0 = 0
   IF ( P(1)>P(2) ) THEN
      flush = .TRUE.
      flshal = .TRUE.
   ENDIF
   ASSIGN 4100 TO r1
   ASSIGN 4200 TO r
   GOTO 4700
 4100 flush = .TRUE.
 4200 IF ( flush ) GOTO 4400
   itrlt = 0
   DO l = 2 , 7
      itrlt = itrlt + M(l+2)
      IF ( Icfiat==8 .AND. (M(l+2)<0 .OR. M(l+2)>65535) ) flush = .TRUE.
!     2147483647 = 2**31-1
      IF ( Icfiat==11 .AND. (M(l+2)<0 .OR. M(l+2)>2147483647) ) flush = .TRUE.
      t(l) = M(l+2)
   ENDDO
   IF ( itrlt==0 ) THEN
      DO l = 2 , 7
         t(l) = 32767
      ENDDO
   ENDIF
   CALL write(pool,nm,2,0)
   IF ( Icfiat==11 ) THEN
      l = 6
   ELSE
      DO lx = 1 , 3
         t(lx+1) = orf(lshift(t(2*lx),16),t(2*lx+1))
      ENDDO
      l = 3
   ENDIF
   CALL write(pool,t(2),l,1)
   CALL write(pool,nm,2,0)
   IF ( L8/=0 ) WRITE (Nout,99009) nm , dti , (t(ip+1),ip=1,j)
   IF ( M1(1)==T1(1,K) .AND. M1(2)==T1(2,K) ) CALL write(pool,nm,0,1)
   GOTO 4500
 4300 l = l1
   lf = l + l1f
   DO WHILE ( Mf(lf)/=3 .OR. M(l)/=endrc1 .OR. M(l+1)/=endrc2 )
      IF ( Mf(lf)>2 ) l = l + 1
      l = l + 1
      lf = lf + 1
      IF ( Mf(lf)<0 ) THEN
         CALL write(pool,M(l1),l-l1,0)
         nwords = nwords + l - l1
         IF ( M1(1)==0 .AND. M1(2)==0 ) GOTO 4500
         flush = .TRUE.
         GOTO 4400
      ENDIF
   ENDDO
   CALL write(pool,M(l1),l-l1,1)
   nwords = nwords + l - l1
   IF ( M1(1)==0 .AND. M1(2)==0 ) flush = .TRUE.
 4400 IF ( .NOT.(M1(1)==0 .AND. M1(2)==0 .OR. M1(1)==T1(1,K) .AND. M1(2)==T1(2,K)) ) THEN
      ASSIGN 4500 TO r
      GOTO 4600
   ENDIF
 4500 N = 0
   IF ( .NOT.(.NOT.flush .OR. iprint/=0) ) THEN
      CALL page2(2)
      WRITE (Nout,99010) Ufm , nm(1) , nm(2) , Knt
      iprint = 1
   ENDIF
   GOTO 3700
 4600 IF ( flshal ) THEN
      WRITE (Nout,99007) Sfm , nm(1) , nm(2)
99007 FORMAT (A25,' 318, NO ROOM IN /XDPL/ FOR DTI ',2A4)
      CALL page2(2)
      Abort = .TRUE.
   ELSE
      IF ( .NOT.(flush) ) THEN
         CALL eof(pool)
         Dmiflg = .TRUE.
         P(1) = P(1) + 1
      ENDIF
      ip = 3*P(3) + 4
      P(ip) = nm(1)
      P(ip+1) = nm(2)
      IF ( flush ) nwords = 0
      P(ip+2) = orf(lshift(nwords/1000,16),P(1)-1)
      P(3) = P(3) + 1
      IF ( flush ) THEN
         CALL page2(2)
         WRITE (Nout,99010) Ufm , nm(1) , nm(2) , Knt
         CALL eof(pool)
         P(1) = P(1) + 1
         CALL skpfil(pool,-1)
         IF ( Dmiflg ) CALL skpfil(pool,+1)
         Abort = .TRUE.
      ENDIF
   ENDIF
   GOTO r
!
!     ******************************************************************
!
!     CHECK NAME FOR UNIQUENESS AMONG DMI CARDS, DTI CARDS, ETC. AND
!     RESERVED NAMES
!
!
!     CHECK  FIST, FIAT, DPL FOR A NAME MATCH
!
 4700 DO ii = 1 , Ipfist
      IF ( nm(1)==Ifist(2*ii+1) .AND. nm(2)==bcdblk ) GOTO 4800
   ENDDO
   nfiat = Icfiat*Ifiat(2) - 2
   DO ii = 4 , nfiat , Icfiat
      IF ( nm(1)==Ifiat(ii) .AND. nm(2)==Ifiat(ii+1) ) GOTO 4800
   ENDDO
   ndpl = P(3)*3 + 1
   DO ii = 4 , ndpl , 3
      IF ( nm(1)==P(ii) .AND. nm(2)==P(ii+1) ) GOTO 4800
   ENDDO
   GOTO r
 4800 GOTO r1
 4900 Kn = 0
   icomp = 1
   I(N+1) = -1
   N = N + 1
   GOTO 500
 5000 Kn = 0
   icomp = 1
   I(N+1) = -1
   N = N + 1
   GOTO 500
 5100 Kn = 0
   icomp = 1
   I(N+1) = -1
   N = N + 1
   GOTO 500
99008 FORMAT (A25,' 327A, ',A4,' CARD ',2A4,', SORTED CARD COUNT =',I7,' SPECIFYING COMPLEX DATA INPUT',/5X,                        &
             &'AND REAL MATRIX OUTPUT MAY NOT MAKE SENSE',/)
99009 FORMAT ('0*** DIAG  8 MESSAGE -- TRAILER FOR DATA BLOCK ',2A4,' (VIA ',A4,' CARDS) = ',5I7,I9)
99010 FORMAT (A23,' 317, BAD DATA OR FORMAT OR NON-UNIQUE NAME FOR DTI ',2A4,10X,'SORTED CARD COUNT =',I7)
!
END SUBROUTINE ifs2p
