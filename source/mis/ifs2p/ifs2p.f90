!*==ifs2p.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifs2p(*,*,*)
!
   USE c_cifs2p
   USE c_ifpdta
   USE c_ifpx1
   USE c_l15l8
   USE c_machin
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   USE c_xmssg
   USE c_xpfist
   USE c_zblpkx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: abort , ec , flshal , flush , int , secd
   INTEGER :: bandit , failur , i2 , icfiat , ierr , ifo , ii , iold1 , iold2 , iold3 , ioldm2 , ioldm4 , ioldm6 , iolmf2 , iolmf4 ,&
            & iout , ip , iprint , items , itrlt , ity1 , j , j0 , kprec , kword1 , kword2 , kz , l , l1 , l1f , l2 , l3 , lamopt , &
            & lf , lp1 , lp2 , lx , nbuf , nbuf2 , ncols , ndpl , nfiat , nout , nrows , nwords , r , r1 , ty1 , ty2
   INTEGER , SAVE :: bcdblk , bcddet , bcdfer , bcdgiv , bcdhes , bcdinv , bcdll , bcdls , bcdmas , bcdmax , bcdmgv , bcdpoi ,      &
                   & bcdq , bcdsdt , bcdsin , bcdsl , bcdt , bcdudt , bcduin , bcdz , dmi , dmig , dti , eigb , eigr , endrc1 ,     &
                   & endrc2 , endt , icomp , ino , iscr1 , iyes , pool , skip , thru
   REAL(REAL64) , DIMENSION(2) :: da
   INTEGER , DIMENSION(2) , SAVE :: iall , ihill , ihoff , imem , istrn , istrs , isym , isymm , itsai , nam
   INTEGER , DIMENSION(2) :: nm , onm
   REAL :: oldxm3 , x1 , x2 , xl , xl1 , zseq , zseq1
   INTEGER , DIMENSION(7) :: t
   REAL , DIMENSION(100) :: xm , z
   EXTERNAL bldpk , bldpkn , close , cpyfil , eof , gopen , lshift , mesage , open , orf , page2 , rdtrl , skpfil , write , wrttrl ,&
          & zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     P(1) = NEXT AVAILABLE FILE ON POOL
!     P(2) = TOTAL NUMBER OF POSSIBLE ENTRYS
!     P(3) = CURRENT NUMBER OF ENTRYS PRESENT
!     P(4) - P(3*P(2)+3) = THREE WORDS FOR EACH ENTRY AS FOLLOWS...
!            1.  NAME(1)
!            2.  NAME(2)
!            3.  EQUIV FLAG, SIZE/1000, FILE NO. ON POOL
!
   !>>>>EQUIVALENCE (Ksystm(1),Nbuf) , (Ksystm(24),Icfiat) , (Ksystm(2),Nout) , (Ksystm(55),Kprec) , (Ksystm(3),Abort) ,                 &
!>>>>    & (Ksystm(77),Bandit) , (nrows,t(3)) , (ifo,t(4)) , (ty2,t(5)) , (Z(1),I(1)) , (Xm(1),M(1)) , (Da(1),A(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     =======================================================
!     DMI AND DMIG MUST ACCOMODATE ALL KINDS OF SPECIAL FORMS
!     E.G., IDENTITY MATRIX
!     =======================================================
!
         IF ( k<=100 ) THEN
            IF ( k==1 .OR. k==2 .OR. k==3 .OR. k==4 .OR. k==5 .OR. k==6 .OR. k==7 .OR. k==8 .OR. k==9 .OR. k==10 .OR. k==11 .OR.    &
               & k==12 .OR. k==13 .OR. k==14 .OR. k==15 .OR. k==16 .OR. k==17 .OR. k==18 .OR. k==19 .OR. k==20 .OR. k==21 .OR.      &
               & k==22 .OR. k==23 .OR. k==24 .OR. k==25 .OR. k==26 .OR. k==27 .OR. k==28 .OR. k==29 .OR. k==30 .OR. k==31 .OR.      &
               & k==32 .OR. k==33 .OR. k==34 .OR. k==35 .OR. k==36 .OR. k==37 .OR. k==38 .OR. k==39 .OR. k==40 .OR. k==41 .OR.      &
               & k==42 .OR. k==43 .OR. k==44 .OR. k==45 .OR. k==46 .OR. k==47 .OR. k==48 .OR. k==49 .OR. k==50 .OR. k==51 .OR.      &
               & k==52 .OR. k==53 .OR. k==54 .OR. k==55 .OR. k==56 .OR. k==57 .OR. k==58 .OR. k==59 .OR. k==60 .OR. k==61 .OR.      &
               & k==62 .OR. k==63 .OR. k==64 .OR. k==65 .OR. k==66 .OR. k==67 .OR. k==68 .OR. k==69 .OR. k==70 .OR. k==71 .OR.      &
               & k==72 .OR. k==73 .OR. k==74 .OR. k==75 .OR. k==76 .OR. k==77 .OR. k==78 .OR. k==79 .OR. k==80 .OR. k==81 .OR.      &
               & k==82 .OR. k==83 .OR. k==84 .OR. k==88 .OR. k==90 .OR. k==91 .OR. k==92 .OR. k==98 .OR. k==99 .OR. k==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( k==85 .OR. k==86 ) THEN
!
!*******       85-EIGR, 86-EIGB      ***********************************
!
               IF ( m(1)<=0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(3)/=bcdblk .AND. m(2)/=bcdfer ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(2)/=bcddet .AND. m(2)/=bcdsdt .AND. m(2)/=bcdudt .AND. m(2)/=bcdinv .AND. m(2)/=bcdsin .AND.                  &
                  & m(2)/=bcduin .AND. m(2)/=bcdgiv .AND. m(2)/=bcdmgv .AND. m(2)/=bcdfer ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(2)==bcdfer .AND. (m(3)/=bcdblk .AND. m(3)/=bcdq .AND. m(3)/=bcdz) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(10)+m(11)==0 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(10)==bcdblk .AND. m(11)==bcdblk ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( (m(10)/=bcdmas .OR. m(11)/=bcdblk) .AND. (m(10)/=bcdmax .OR. m(11)/=bcdblk) .AND.                               &
                  & (m(10)/=bcdpoi .OR. m(11)/=bcdt) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(10)/=bcdpoi .AND. (m(12)/=0 .OR. m(13)/=0) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(10)==bcdpoi .AND. (m(12)<=0 .OR. m(13)<0) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==87 ) THEN
!
!*****         87-EIGC          **************************************
!
               IF ( km/=0 ) THEN
                  DO l = 1 , 5
                     IF ( mf(l)/=2 .AND. mf(l)/=0 ) THEN
                        spag_nextblock_1 = 10
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( mf(6)/=1 .AND. mf(6)/=0 .OR. mf(7)/=1 .AND. mf(7)/=0 .OR. mf(8)/=0 ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( xm(5)<=0. ) xm(5) = 1.0
                  IF ( m(6)<0 .OR. m(7)<0 ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  n = 7
                  spag_nextblock_1 = 9
               ELSE
                  IF ( mf(1)/=1 .OR. mf(2)/=3 .OR. mf(3)/=3 .OR. mf(4)/=1 .AND. mf(4)/=0 .OR. mf(5)/=1 .AND. mf(5)/=0 .OR.          &
                     & mf(6)/=2 .AND. mf(6)/=0 .OR. mf(7)/=0 .OR. mf(8)/=0 ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(1)<=0 .OR. m(2)/=bcddet .AND. m(2)/=bcdinv .AND. m(2)/=bcdhes .AND. m(2)/=bcdfer .OR. m(4)/=bcdmax .AND.   &
                     & (m(4)/=bcdpoi .OR. m(5)/=bcdt) .OR. xm(8)<0. ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(4)==bcdmax .AND. (m(6)/=0 .OR. m(7)/=0) ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m(4)==bcdpoi .AND. (m(6)<=0 .OR. m(7)<0) ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
                     spag_nextblock_1 = 10
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  n = 10
                  spag_nextblock_1 = 9
               ENDIF
               CYCLE
            ELSEIF ( k==89 ) THEN
!
!*******       -BLANK CARD-        *************************************
!
               IF ( ibcds/=0 ) RETURN 2
               ibcds = 1
               CALL page2(2)
               WRITE (nout,99001) uwm
99001          FORMAT (A25,' 324, BLANK CARD(S) IGNORED.')
               RETURN 2
            ELSEIF ( k==93 .OR. k==94 .OR. k==95 .OR. k==97 ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==96 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( kx<=100 ) THEN
            IF ( kx==1 .OR. kx==2 .OR. kx==3 .OR. kx==4 .OR. kx==5 .OR. kx==6 .OR. kx==7 .OR. kx==8 .OR. kx==9 .OR. kx==10 .OR.     &
               & kx==11 .OR. kx==12 .OR. kx==13 .OR. kx==14 .OR. kx==15 .OR. kx==16 .OR. kx==17 .OR. kx==18 .OR. kx==21 .OR.        &
               & kx==22 .OR. kx==23 .OR. kx==24 .OR. kx==25 .OR. kx==26 .OR. kx==27 .OR. kx==28 .OR. kx==29 .OR. kx==30 .OR.        &
               & kx==31 .OR. kx==32 .OR. kx==35 .OR. kx==36 .OR. kx==37 .OR. kx==38 .OR. kx==39 .OR. kx==42 .OR. kx==43 .OR.        &
               & kx==44 .OR. kx==45 .OR. kx==46 .OR. kx==47 .OR. kx==48 .OR. kx==49 .OR. kx==50 .OR. kx==51 .OR. kx==52 .OR.        &
               & kx==53 .OR. kx==54 .OR. kx==55 .OR. kx==56 .OR. kx==57 .OR. kx==58 .OR. kx==59 .OR. kx==60 .OR. kx==61 .OR.        &
               & kx==63 .OR. kx==64 .OR. kx==65 .OR. kx==66 .OR. kx==67 .OR. kx==68 .OR. kx==69 .OR. kx==70 .OR. kx==71 .OR.        &
               & kx==72 .OR. kx==73 .OR. kx==74 .OR. kx==75 .OR. kx==76 .OR. kx==77 .OR. kx==78 .OR. kx==79 .OR. kx==80 .OR.        &
               & kx==81 .OR. kx==82 .OR. kx==83 .OR. kx==84 .OR. kx==85 .OR. kx==86 .OR. kx==87 .OR. kx==89 .OR. kx==90 .OR.        &
               & kx==93 .OR. kx==94 .OR. kx==95 .OR. kx==96 .OR. kx==97 .OR. kx==98 .OR. kx==99 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( kx==19 ) THEN
!
!******         119-DMI          ************************************
!
               IF ( km/=0 ) THEN
                  IF ( j0<=0 .OR. j0>ncols ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  l1 = 1
                  l1f = 0
                  l2 = 8
                  IF ( ty1==2 .OR. ty1==4 ) l2 = 16
                  l = l1
                  spag_nextblock_1 = 20
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( fphys ) THEN
                  IF ( p(1)>1 ) dmiflg = .TRUE.
                  fphys = .FALSE.
                  nm(1) = 0
                  nm(2) = 0
                  IF ( bandit/=-1 .AND. bandit/=-2 ) bandit = +9
                  GOTO 20
               ELSEIF ( m(1)==nm(1) .AND. m(2)==nm(2) ) THEN
                  IF ( .NOT.ec ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( flush ) THEN
                     spag_nextblock_1 = 26
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ec = .FALSE.
                  IF ( m(3)<=j0 ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  DO
                     j0 = j0 + 1
                     IF ( m(3)==j0 ) THEN
                        i0 = 1
                        l1 = 4
                        l1f = -1
                        l2 = 9
                        IF ( ty1==2 .OR. ty1==4 ) l2 = 14
                        IF ( mf(3)/=1 .OR. m(4)<i0 ) THEN
                           spag_nextblock_1 = 25
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        i0 = m(4) - 1
                        int = .FALSE.
                        CALL bldpk(ty1,ty2,iscr1,0,0)
                        l = l1
                        spag_nextblock_1 = 20
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        CALL bldpk(ty1,ty2,iscr1,0,0)
                        CALL bldpkn(iscr1,0,t)
                     ENDIF
                  ENDDO
               ELSE
                  ASSIGN 20 TO r
                  spag_nextblock_1 = 27
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( kx==20 ) THEN
               spag_nextblock_1 = 28
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==33 .OR. kx==34 .OR. kx==40 .OR. kx==62 .OR. kx==91 ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==41 ) THEN
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==88 ) THEN
!
!*****     188-TABRNDG       **************************************
!
               IF ( m(1)<0 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m(2)<1 .OR. m(2)>2 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               i(1) = m(1)
               i(2) = m(2)
               i(3) = m(3)
               i(4) = m(4)
               i(5) = 0
               i(6) = 0
               i(7) = 0
               i(8) = 0
               i(9) = -1
               i(10) = -1
               n = 10
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==92 ) THEN
!
!*******      192-PLOAD4     ****************************************
!
               IF ( km==1 ) THEN
!
                  IF ( mf(1)>1 ) badfor = .TRUE.
                  DO l = 2 , 4
                     IF ( mf(l)/=2 .AND. mf(l)/=0 ) badfor = .TRUE.
                  ENDDO
                  IF ( mf(1)==0 ) m(1) = 0
                  IF ( m(1)<0 ) baddat = .TRUE.
                  DO l = 1 , 4
                     i(l) = m(l)
                  ENDDO
                  n = 4
                  km = 0
                  kn = 0
               ELSE
                  km = 1
                  kn = 1
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( mf(2)==2 .AND. mf(3)==1 .AND. mf(4)==0 .AND. mf(5)==0 .AND. mf(6)==0 ) THEN
!
!     SPECIAL - ALLOWING PLOAD4 TO TAKE ON PLOAD2 FORMAT
!     (PLOAD4,SID,P1,E1,blank,blank,blank,"THRU",E2) FOR QUICK INPUT
!     DATA SWITCHING.  INTERCHAGNE 2ND AND 3RD FIELDS
!
                     mf(2) = 1
                     mf(3) = 2
                     l = m(2)
                     m(2) = m(3)
                     m(3) = l
                  ENDIF
                  IF ( mf(2)/=1 ) badfor = .TRUE.
                  DO l = 3 , 6
                     IF ( mf(l)/=2 .AND. mf(l)/=0 ) badfor = .TRUE.
                  ENDDO
                  IF ( mf(7)/=3 .AND. mf(7)/=0 .AND. .NOT.(mf(7)==1 .AND. m(7)==0) ) badfor = .TRUE.
                  IF ( mf(8)/=1 .AND. mf(8)/=0 ) badfor = .TRUE.
                  IF ( mf(7)==0 .AND. mf(8)/=0 ) badfor = .TRUE.
                  IF ( mf(7)==3 .AND. mf(8)/=1 ) badfor = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  IF ( m(2)<=0 ) baddat = .TRUE.
                  IF ( mf(7)==3 .AND. m(7)/=thru ) baddat = .TRUE.
                  IF ( mf(7)==3 .AND. m(9)<=0 ) baddat = .TRUE.
                  IF ( mf(7)==3 .AND. m(9)<=m(2) ) baddat = .TRUE.
                  l1 = 0
                  IF ( mf(7)==3 ) l1 = 1
                  DO l = 1 , 6
                     i(l) = m(l)
                  ENDDO
                  i(7) = -1
                  IF ( l1==1 ) i(7) = 0
                  i(8) = m(l1+8)
                  n = 8
                  IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
                     DO l = 9 , 12
                        i(l) = 0
                     ENDDO
                     n = 12
                     km = 0
                     kn = 0
                  ENDIF
               ENDIF
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kx==100 ) THEN
!
!*******        200 - DTI       ****************************************
!
               IF ( km/=0 ) THEN
                  l1 = 1
                  l1f = 0
                  spag_nextblock_1 = 35
               ELSEIF ( fphys2 ) THEN
                  IF ( p(1)>1 ) dmiflg = .TRUE.
                  fphys2 = .FALSE.
                  nm(1) = 0
                  nm(2) = 0
                  GOTO 120
               ELSEIF ( m(1)==nm(1) .AND. m(2)==nm(2) ) THEN
                  j0 = j0 + 1
                  IF ( m(3)/=j0 ) THEN
                     flush = .TRUE.
                     spag_nextblock_1 = 36
                  ELSE
                     l1 = 4
                     l1f = -1
                     spag_nextblock_1 = 35
                  ENDIF
               ELSE
                  ASSIGN 120 TO r
                  spag_nextblock_1 = 37
               ENDIF
               CYCLE
            ENDIF
         ENDIF
         IF ( ky<=100 ) THEN
            IF ( ky==1 .OR. ky==2 .OR. ky==3 .OR. ky==4 .OR. ky==5 .OR. ky==6 .OR. ky==7 .OR. ky==8 .OR. ky==9 .OR. ky==10 .OR.     &
               & ky==11 .OR. ky==12 .OR. ky==13 .OR. ky==14 .OR. ky==15 .OR. ky==16 .OR. ky==17 .OR. ky==18 .OR. ky==19 .OR.        &
               & ky==20 .OR. ky==21 .OR. ky==22 .OR. ky==23 .OR. ky==24 .OR. ky==25 .OR. ky==26 .OR. ky==27 .OR. ky==28 .OR.        &
               & ky==29 .OR. ky==30 .OR. ky==31 .OR. ky==32 .OR. ky==33 .OR. ky==34 .OR. ky==35 .OR. ky==36 .OR. ky==37 .OR.        &
               & ky==38 .OR. ky==39 .OR. ky==40 .OR. ky==41 .OR. ky==42 .OR. ky==43 .OR. ky==44 .OR. ky==45 .OR. ky==46 .OR.        &
               & ky==47 .OR. ky==48 .OR. ky==49 .OR. ky==50 .OR. ky==51 .OR. ky==52 .OR. ky==53 .OR. ky==54 .OR. ky==55 .OR.        &
               & ky==56 .OR. ky==57 .OR. ky==58 .OR. ky==59 .OR. ky==60 .OR. ky==63 .OR. ky==64 .OR. ky==65 .OR. ky==66 .OR.        &
               & ky==67 .OR. ky==68 .OR. ky==69 .OR. ky==70 .OR. ky==71 .OR. ky==72 .OR. ky==73 .OR. ky==74 .OR. ky==75 .OR.        &
               & ky==76 .OR. ky==77 .OR. ky==78 .OR. ky==79 .OR. ky==84 .OR. ky==85 .OR. ky==86 .OR. ky==87 .OR. ky==88 .OR.        &
               & ky==89 .OR. ky==90 .OR. ky==91 .OR. ky==92 .OR. ky==93 .OR. ky==94 .OR. ky==95 .OR. ky==96 .OR. ky==97 .OR.        &
               & ky==98 .OR. ky==99 .OR. ky==100 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ky==61 ) THEN
!
!*******       261-CQUAD4    ****************************************
!
               IF ( mf(2)==0 ) m(2) = m(1)
               i(1) = m(1)
               DO l = 2 , 6
                  IF ( mf(l)/=1 ) badfor = .TRUE.
                  IF ( m(l)<=0 ) baddat = .TRUE.
                  i(l) = m(l)
               ENDDO
               l1 = 6
               DO l = 11 , 14
                  l1 = l1 + 1
                  i(l1) = m(l)
               ENDDO
               IF ( mf(7)/=1 .AND. mf(7)/=2 .AND. mf(7)/=0 ) badfor = .TRUE.
               IF ( mf(7)==1 .AND. (m(7)<0 .OR. m(7)>=1000000) ) baddat = .TRUE.
               i(11) = m(7)
               i(12) = 0
               IF ( mf(7)==1 ) i(12) = 1
               i(13) = m(8)
               n = 13
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( ky==62 ) THEN
!
!*******        262-MAT8      ****************************************
!
               IF ( mf(2)==0 .OR. mf(3)==0 .OR. mf(5)==0 ) THEN
                  badfor = .TRUE.
                  RETURN 1
               ELSE
                  IF ( m(1)<=0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( xm(2)==0.0 .OR. xm(3)==0.0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( xm(5)<=0.0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(12)==2 .AND. xm(12)<=0.0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(14)==2 .AND. xm(14)<=0.0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(16)==2 .AND. xm(16)<=0.0 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( mf(13)==0 ) xm(13) = xm(12)
                  IF ( mf(15)==0 ) xm(15) = xm(14)
                  n = 18
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( ky==80 ) THEN
!
!*******        280-PCOMP     ****************************************
!
               kn = 1
               IF ( icomp>1 ) THEN
!
                  n = 0
                  DO l = 1 , 2
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           l1 = 4*(l-1)
                           l2 = l1
                           DO l3 = 1 , 4
                              IF ( mf(l1+l3)/=0 ) THEN
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDDO
                           IF ( l==1 ) badfor = .TRUE.
                           spag_nextblock_1 = 40
                           CYCLE SPAG_DispatchLoop_1
                        CASE (2)
                           IF ( l==2 .AND. mf(4)==3 ) l2 = l2 + 1
                           IF ( icomp==3 ) THEN
                              IF ( mf(l1+1)/=1 .AND. mf(l1+1)/=0 ) badfor = .TRUE.
                              IF ( mf(l1+2)/=2 .AND. mf(l1+2)/=0 ) badfor = .TRUE.
                              IF ( mf(l1+3)/=2 .AND. mf(l1+3)/=0 ) badfor = .TRUE.
                              IF ( mf(l1+1)==1 .AND. m(l2+1)<=0 ) baddat = .TRUE.
                              IF ( mf(l1+1)==0 ) m(l2+1) = iold1
                              IF ( mf(l1+2)==2 .AND. xm(l2+2)<=0.0 ) baddat = .TRUE.
                              IF ( mf(l1+2)==0 ) m(l2+2) = iold2
                              IF ( mf(l1+3)==0 ) m(l2+3) = iold3
                           ELSE
                              icomp = 3
                              IF ( mf(1)/=1 ) badfor = .TRUE.
                              IF ( mf(2)/=2 ) badfor = .TRUE.
                              IF ( mf(3)/=2 .AND. mf(3)/=0 ) badfor = .TRUE.
                              IF ( m(1)<=0 ) baddat = .TRUE.
                              IF ( xm(2)<=0.0 ) baddat = .TRUE.
                           ENDIF
                           IF ( mf(l1+4)/=3 .AND. mf(l1+4)/=0 ) badfor = .TRUE.
                           IF ( mf(l1+4)==3 .AND. (m(l2+4)/=iyes .AND. m(l2+4)/=ino) ) baddat = .TRUE.
                           iout = 0
                           IF ( m(l2+4)==iyes ) iout = 1
                           i(n+1) = m(l2+1)
                           i(n+2) = m(l2+2)
                           i(n+3) = m(l2+3)
                           i(n+4) = iout
                           iold1 = m(l2+1)
                           iold2 = m(l2+2)
                           iold3 = m(l2+3)
                           n = n + 4
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
                  ENDDO
                  IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 40
               ELSE
                  icomp = 2
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( mf(2)/=2 .AND. mf(2)/=0 ) badfor = .TRUE.
                  IF ( mf(3)/=2 .AND. mf(3)/=0 ) badfor = .TRUE.
                  IF ( mf(4)/=2 .AND. mf(4)/=0 ) badfor = .TRUE.
                  IF ( mf(5)/=3 .AND. mf(5)/=0 ) badfor = .TRUE.
                  l = 0
                  IF ( mf(5)==3 ) l = 1
                  IF ( mf(6)/=0 ) badfor = .TRUE.
                  IF ( mf(7)/=0 ) badfor = .TRUE.
                  IF ( mf(8)/=3 .AND. mf(8)/=0 ) badfor = .TRUE.
                  IF ( m(1)<=0 .OR. m(1)>=1000000 ) baddat = .TRUE.
                  IF ( mf(5)==3 .AND. xm(4)<=0.0 ) baddat = .TRUE.
                  failur = -1
                  IF ( mf(5)==0 ) failur = 0
                  IF ( failur/=0 ) THEN
                     IF ( m(5)==ihill(1) .AND. m(6)==ihill(2) ) failur = 1
                     IF ( m(5)==ihoff(1) .AND. m(6)==ihoff(2) ) failur = 2
                     IF ( m(5)==itsai(1) .AND. m(6)==itsai(2) ) failur = 3
                     IF ( m(5)==istrs(1) .AND. m(6)==istrs(2) ) failur = 4
                     IF ( m(5)==istrn(1) .AND. m(6)==istrn(2) ) failur = 5
                     IF ( failur==-1 ) baddat = .TRUE.
                  ENDIF
                  lamopt = -1
                  IF ( mf(8)==0 ) lamopt = 0
                  IF ( lamopt/=0 ) THEN
                     IF ( m(l+8)==iall(1) .AND. m(l+9)==iall(2) ) lamopt = 0
                     IF ( m(l+8)==isym(1) .AND. m(l+9)==isym(2) ) lamopt = 1
                     IF ( m(l+8)==imem(1) .AND. m(l+9)==imem(2) ) lamopt = 2
                     IF ( m(l+8)==isymm(1) .AND. m(l+9)==isymm(2) ) lamopt = 3
                     IF ( lamopt==-1 ) baddat = .TRUE.
                  ENDIF
                  IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                     i(1) = m(1)
                     i(2) = m(2)
                     i(3) = m(3)
                     i(4) = m(4)
                     i(5) = failur
                     i(6) = 0
                     i(7) = 0
                     i(8) = lamopt
                     n = 8
                  ELSE
                     badfor = .TRUE.
                     kn = 0
                     icomp = 1
                     n = 0
                  ENDIF
                  spag_nextblock_1 = 6
               ENDIF
               CYCLE
            ELSEIF ( ky==81 ) THEN
!
!*******        281-PCOMP1    ****************************************
!
               kn = 1
               IF ( icomp>1 ) THEN
!
                  n = 0
                  DO l = 1 , 8
                     IF ( mf(l)/=0 ) THEN
                        IF ( icomp==3 ) THEN
                           IF ( mf(l)/=2 .AND. mf(l)/=0 ) badfor = .TRUE.
                           IF ( mf(l)==0 ) m(l) = iold1
                        ELSE
                           icomp = 3
                           IF ( mf(1)/=2 ) badfor = .TRUE.
                        ENDIF
                        i(n+1) = m(l)
                        iold1 = m(l)
                        n = n + 1
                     ELSE
                        IF ( l==1 ) badfor = .TRUE.
                        spag_nextblock_1 = 41
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 41
               ELSE
                  icomp = 2
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( mf(2)/=2 .AND. mf(2)/=0 ) badfor = .TRUE.
                  IF ( mf(3)/=2 .AND. mf(3)/=0 ) badfor = .TRUE.
                  IF ( mf(4)/=2 .AND. mf(4)/=0 ) badfor = .TRUE.
                  IF ( mf(5)/=3 .AND. mf(5)/=0 ) badfor = .TRUE.
                  l = 0
                  IF ( mf(5)==3 ) l = 1
                  IF ( mf(6)/=1 ) badfor = .TRUE.
                  IF ( mf(7)/=2 ) badfor = .TRUE.
                  IF ( mf(8)/=3 .AND. mf(8)/=0 ) badfor = .TRUE.
                  IF ( m(1)<=0 .OR. m(1)>=1000000 ) baddat = .TRUE.
                  IF ( mf(5)==3 .AND. xm(4)<=0.0 ) baddat = .TRUE.
                  failur = -1
                  IF ( mf(5)==0 ) failur = 0
                  IF ( failur/=0 ) THEN
                     IF ( m(5)==ihill(1) .AND. m(6)==ihill(2) ) failur = 1
                     IF ( m(5)==ihoff(1) .AND. m(6)==ihoff(2) ) failur = 2
                     IF ( m(5)==itsai(1) .AND. m(6)==itsai(2) ) failur = 3
                     IF ( m(5)==istrs(1) .AND. m(6)==istrs(2) ) failur = 4
                     IF ( m(5)==istrn(1) .AND. m(6)==istrn(2) ) failur = 5
                     IF ( failur==-1 ) baddat = .TRUE.
                  ENDIF
                  IF ( m(l+6)<=0 ) baddat = .TRUE.
                  IF ( xm(l+7)<=0.0 ) baddat = .TRUE.
                  lamopt = -1
                  IF ( mf(8)==0 ) lamopt = 0
                  IF ( lamopt/=0 ) THEN
                     IF ( m(l+8)==iall(1) .AND. m(l+9)==iall(2) ) lamopt = 0
                     IF ( m(l+8)==isym(1) .AND. m(l+9)==isym(2) ) lamopt = 1
                     IF ( m(l+8)==imem(1) .AND. m(l+9)==imem(2) ) lamopt = 2
                     IF ( m(l+8)==isymm(1) .AND. m(l+9)==isymm(2) ) lamopt = 3
                     IF ( lamopt==-1 ) baddat = .TRUE.
                  ENDIF
                  IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                     i(1) = m(1)
                     i(2) = m(2)
                     i(3) = m(3)
                     i(4) = m(4)
                     i(5) = failur
                     i(6) = m(l+6)
                     i(7) = m(l+7)
                     i(8) = lamopt
                     n = 8
                  ELSE
                     badfor = .TRUE.
                     kn = 0
                     icomp = 1
                     n = 0
                  ENDIF
                  spag_nextblock_1 = 6
               ENDIF
               CYCLE
            ELSEIF ( ky==82 ) THEN
!
!*******        282-PCOMP2    ****************************************
!
               kn = 1
               IF ( icomp>1 ) THEN
!
                  n = 0
                  DO l = 1 , 4
                     spag_nextblock_3 = 1
                     SPAG_DispatchLoop_3: DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
                           l1 = 2*(l-1)
                           DO l3 = 1 , 2
                              IF ( mf(l1+l3)/=0 ) THEN
                                 spag_nextblock_3 = 2
                                 CYCLE SPAG_DispatchLoop_3
                              ENDIF
                           ENDDO
                           IF ( l==1 ) badfor = .TRUE.
                           spag_nextblock_1 = 42
                           CYCLE SPAG_DispatchLoop_1
                        CASE (2)
                           IF ( icomp==3 ) THEN
                              IF ( mf(l1+1)/=2 .AND. mf(l1+1)/=0 ) badfor = .TRUE.
                              IF ( mf(l1+2)/=2 .AND. mf(l1+2)/=0 ) badfor = .TRUE.
                              IF ( mf(l1+1)==2 .AND. xm(l1+1)<=.0 ) baddat = .TRUE.
                              IF ( mf(l1+1)==0 ) m(l1+1) = iold1
                              IF ( mf(l1+2)==0 ) m(l1+2) = iold2
                           ELSE
                              icomp = 3
                              IF ( mf(1)/=2 ) badfor = .TRUE.
                              IF ( mf(2)/=2 ) badfor = .TRUE.
                              IF ( xm(1)<=0.0 ) baddat = .TRUE.
                           ENDIF
                           i(n+1) = m(l1+1)
                           i(n+2) = m(l1+2)
                           iold1 = m(l1+1)
                           iold2 = m(l1+2)
                           n = n + 2
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
                  ENDDO
                  IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 42
               ELSE
                  icomp = 2
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( mf(2)/=2 .AND. mf(2)/=0 ) badfor = .TRUE.
                  IF ( mf(3)/=2 .AND. mf(3)/=0 ) badfor = .TRUE.
                  IF ( mf(4)/=2 .AND. mf(4)/=0 ) badfor = .TRUE.
                  IF ( mf(5)/=3 .AND. mf(5)/=0 ) badfor = .TRUE.
                  l = 0
                  IF ( mf(5)==3 ) l = 1
                  IF ( mf(6)/=1 ) badfor = .TRUE.
                  IF ( mf(7)/=0 ) badfor = .TRUE.
                  IF ( mf(8)/=3 .AND. mf(8)/=0 ) badfor = .TRUE.
                  IF ( m(1)<=0 .OR. m(1)>=1000000 ) baddat = .TRUE.
                  IF ( mf(5)==3 .AND. xm(4)<=0.0 ) baddat = .TRUE.
                  failur = -1
                  IF ( mf(5)==0 ) failur = 0
                  IF ( failur/=0 ) THEN
                     IF ( m(5)==ihill(1) .AND. m(6)==ihill(2) ) failur = 1
                     IF ( m(5)==ihoff(1) .AND. m(6)==ihoff(2) ) failur = 2
                     IF ( m(5)==itsai(1) .AND. m(6)==itsai(2) ) failur = 3
                     IF ( m(5)==istrs(1) .AND. m(6)==istrs(2) ) failur = 4
                     IF ( m(5)==istrn(1) .AND. m(6)==istrn(2) ) failur = 5
                     IF ( failur==-1 ) baddat = .TRUE.
                  ENDIF
                  IF ( m(l+6)<=0 ) baddat = .TRUE.
                  lamopt = -1
                  IF ( mf(8)==0 ) lamopt = 0
                  IF ( lamopt/=0 ) THEN
                     IF ( m(l+8)==iall(1) .AND. m(l+9)==iall(2) ) lamopt = 0
                     IF ( m(l+8)==isym(1) .AND. m(l+9)==isym(2) ) lamopt = 1
                     IF ( m(l+8)==imem(1) .AND. m(l+9)==imem(2) ) lamopt = 2
                     IF ( m(l+8)==isymm(1) .AND. m(l+9)==isymm(2) ) lamopt = 3
                     IF ( lamopt==-1 ) baddat = .TRUE.
                  ENDIF
                  IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                     i(1) = m(1)
                     i(2) = m(2)
                     i(3) = m(3)
                     i(4) = m(4)
                     i(5) = failur
                     i(6) = m(l+6)
                     i(7) = 0
                     i(8) = lamopt
                     n = 8
                  ELSE
                     badfor = .TRUE.
                     kn = 0
                     icomp = 1
                     n = 0
                  ENDIF
                  spag_nextblock_1 = 6
               ENDIF
               CYCLE
            ELSEIF ( ky==83 ) THEN
!
!*******        283-PSHELL    ****************************************
!
               IF ( km==1 ) THEN
!
                  IF ( mf(1)/=2 .AND. mf(1)/=0 ) badfor = .TRUE.
                  IF ( mf(2)/=2 .AND. mf(2)/=0 ) badfor = .TRUE.
                  IF ( mf(3)/=1 .AND. mf(3)/=0 ) badfor = .TRUE.
                  IF ( mf(4)/=1 .AND. mf(4)/=2 .AND. mf(4)/=0 ) badfor = .TRUE.
                  IF ( mf(5)/=1 .AND. mf(5)/=2 .AND. mf(5)/=0 ) badfor = .TRUE.
                  IF ( mf(6)/=2 .AND. mf(6)/=0 ) badfor = .TRUE.
                  IF ( mf(1)==0 ) xm(1) = -0.5*oldxm3
                  IF ( mf(2)==0 ) xm(2) = 0.5*oldxm3
                  IF ( mf(3)==1 .AND. m(3)<=0 ) baddat = .TRUE.
                  IF ( mf(3)/=0 .AND. (iolmf2==0 .OR. iolmf4==0) ) baddat = .TRUE.
                  IF ( mf(3)/=0 .AND. (m(3)==ioldm2 .OR. m(3)==ioldm4) ) baddat = .TRUE.
                  IF ( mf(4)==1 .AND. m(4)<0 ) baddat = .TRUE.
                  IF ( mf(5)==1 .AND. m(5)<0 ) baddat = .TRUE.
                  IF ( ioldm2==0 .AND. ioldm4==0 .AND. ioldm6==0 .AND. m(3)==0 ) baddat = .TRUE.
                  DO l = 1 , 4
                     i(l) = m(l)
                  ENDDO
                  i(5) = 0
                  IF ( mf(4)==1 ) i(5) = 1
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
                  i(6) = 0
                  i(7) = m(5)
                  i(8) = 0
                  IF ( mf(5)==1 ) i(8) = 1
                  i(9) = m(6)
                  n = 9
                  km = 0
                  kn = 0
               ELSE
                  km = 1
                  kn = 1
                  IF ( mf(1)/=1 ) badfor = .TRUE.
                  IF ( mf(2)/=1 .AND. mf(2)/=0 ) badfor = .TRUE.
                  IF ( mf(3)/=2 .AND. mf(3)/=0 ) badfor = .TRUE.
                  IF ( mf(4)/=1 .AND. mf(4)/=0 ) badfor = .TRUE.
                  IF ( mf(5)/=2 .AND. mf(5)/=0 ) badfor = .TRUE.
                  IF ( mf(6)/=1 .AND. mf(6)/=0 ) badfor = .TRUE.
                  IF ( mf(7)/=2 .AND. mf(7)/=0 ) badfor = .TRUE.
                  IF ( mf(8)/=2 .AND. mf(8)/=0 ) badfor = .TRUE.
                  IF ( m(1)<=0 ) baddat = .TRUE.
                  IF ( mf(2)==1 .AND. m(2)<=0 ) baddat = .TRUE.
                  IF ( mf(4)==1 .AND. m(4)<=0 ) baddat = .TRUE.
                  IF ( mf(4)/=0 .AND. mf(5)==0 ) xm(5) = 1.0
                  IF ( mf(6)==1 .AND. m(6)<=0 ) baddat = .TRUE.
                  IF ( mf(6)/=0 .AND. mf(4)==0 ) baddat = .TRUE.
                  IF ( mf(6)/=0 .AND. mf(7)==0 ) xm(7) = 0.833333
                  DO l = 2 , 6 , 2
                     IF ( m(l)==0 .AND. xm(l+1)>0.0 ) baddat = .TRUE.
                  ENDDO
                  DO l = 1 , 8
                     i(l) = m(l)
                  ENDDO
                  iolmf2 = mf(2)
                  iolmf4 = mf(4)
                  ioldm2 = m(2)
                  ioldm4 = m(4)
                  ioldm6 = m(6)
                  oldxm3 = xm(3)
                  n = 8
                  IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
                     z(9) = -0.5*oldxm3
                     z(10) = 0.5*oldxm3
                     DO l = 11 , 17
                        i(l) = 0
                     ENDDO
                     n = 17
                     km = 0
                     kn = 0
                  ENDIF
               ENDIF
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         kz = k - 300
         IF ( kz<=60 ) THEN
            IF ( kz==54 ) THEN
!
!*******       354-CTRIA3      **************************************
!
               IF ( mf(2)==0 ) m(2) = m(1)
               i(1) = m(1)
               DO l = 2 , 5
                  IF ( mf(l)/=1 ) badfor = .TRUE.
                  IF ( m(l)<=0 ) baddat = .TRUE.
                  i(l) = m(l)
               ENDDO
               IF ( mf(6)/=1 .AND. mf(6)/=2 .AND. mf(6)/=0 ) badfor = .TRUE.
               IF ( mf(6)==1 .AND. (m(6)<0 .OR. m(6)>=1000000) ) baddat = .TRUE.
               i(6) = m(11)
               i(7) = m(12)
               i(8) = m(13)
               i(9) = m(6)
               i(10) = 0
               i(11) = m(7)
               IF ( mf(6)==1 ) i(10) = 1
               n = 11
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( kz==57 ) THEN
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         CALL page2(2)
         WRITE (nout,99002) sfm
99002    FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS2P.')
         abort = .TRUE.
         RETURN 1
      CASE (3)
         baddat = .TRUE.
         RETURN 1
      CASE (4)
         DO l = 1 , n
            i(l) = m(l)
         ENDDO
         spag_nextblock_1 = 5
      CASE (5)
         RETURN
      CASE (6)
         RETURN 3
      CASE (7)
         nm(1) = eigr
         nm(2) = bcdmas
         IF ( k/=85 ) THEN
            nm(1) = eigb
            nm(2) = bcdmax
         ENDIF
         m(10) = nm(2)
         m(12) = 0
         m(13) = 0
         CALL mesage(30,222,nm)
         spag_nextblock_1 = 8
      CASE (8)
         IF ( m(6)==0 .AND. m(2)/=bcdgiv .AND. m(2)/=bcdmgv .AND. m(2)/=bcdfer ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k==86 .AND. (m(2)==bcdgiv .OR. m(2)==bcdmgv) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( (m(2)==bcddet .OR. m(2)==bcdsdt) .AND. xm(4)<0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m(2)==bcdudt .AND. xm(4)<0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k==85 .AND. m(2)/=bcdgiv .AND. m(2)/=bcdmgv .AND. xm(4)<0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m(2)/=bcdgiv .AND. m(2)/=bcdmgv .AND. m(2)/=bcdfer .AND. xm(5)<=0.0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( m(2)/=bcdgiv .AND. m(2)/=bcdmgv .AND. m(2)/=bcdfer .AND. xm(4)>=xm(5) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = 18
         spag_nextblock_1 = 4
      CASE (9)
         DO l = 1 , n
            i(l) = m(l)
         ENDDO
         spag_nextblock_1 = 11
      CASE (10)
         baddat = .TRUE.
         spag_nextblock_1 = 11
      CASE (11)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            kn = 1
            km = 1
         ELSE
            DO l = 1 , 7
               n = n + 1
               i(n) = -1
            ENDDO
            km = 0
            kn = 0
         ENDIF
         spag_nextblock_1 = 6
      CASE (12)
!
!*******       93- TABLEM1, 94-TABLEM2, 95-TABLEM3  ********************
!              133-TABLED1,134-TABLED2,140-TABLED3
!              162-TABDMP1, 97-TABLES1,191-TABRND1
!              357-TABLEM5
!                 (TABLEM5 IS DESIGNED FOR THERMAL COEFFICIENT WHICH IS
!                          FUNCTION OF TIME
!                          THIS PROJECT TEMPORARY HALTS HERE  6/90)
!
         IF ( km/=0 ) THEN
            l1 = 0
            SPAG_Loop_1_1: DO l = 1 , 7 , 2
               IF ( mf(l)==3 .OR. mf(l+1)==3 ) THEN
                  IF ( mf(l)==3 ) THEN
                     l1 = l1 + 2
                     lp1 = l1 - 1
                     kword1 = m(lp1)
                  ELSE
                     l1 = l1 + 1
                     lp1 = l1
                     kword1 = 0
                  ENDIF
                  IF ( mf(l+1)==3 ) THEN
                     l1 = l1 + 2
                     lp2 = l1 - 1
                     kword2 = m(lp2)
                  ELSE
                     l1 = l1 + 1
                     lp2 = l1
                     kword2 = 0
                  ENDIF
                  IF ( kword1==endt .OR. kword2==endt ) THEN
                     spag_nextblock_1 = 15
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( kword1/=skip .AND. kword2/=skip ) THEN
                     baddat = .TRUE.
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ELSE
                  IF ( mf(l)/=0 .AND. mf(l)/=2 .OR. mf(l+1)/=0 .AND. mf(l+1)/=2 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  items = items + 1
                  n = n + 2
                  l1 = l1 + 2
                  i(n-1) = m(l1-1)
                  i(n) = m(l1)
                  IF ( items>2 ) THEN
                     xl1 = xl
                     xl = z(n-1)
                     zseq1 = sign(1.0,xl-xl1)
                     IF ( zseq1/=zseq .AND. xl/=xl1 ) baddat = .TRUE.
                  ELSEIF ( items>1 ) THEN
                     x2 = z(n-1)
                     xl1 = xl
                     xl = x2
                     zseq = sign(1.0,x2-x1)
                     IF ( x2==x1 ) baddat = .TRUE.
                  ELSE
                     x1 = z(n-1)
                     xl = x1
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_1
            spag_nextblock_1 = 14
         ELSE
            i2 = m(1)
            items = 0
            n = 8
            IF ( m(1)<=0 ) baddat = .TRUE.
            IF ( mf(1)/=1 ) badfor = .TRUE.
            i(1) = i2
            DO l = 2 , 7
               IF ( mf(l)/=0 .AND. mf(l)/=2 ) badfor = .TRUE.
               i(l) = m(l)
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
            i(8) = 0
            IF ( mf(8)==3 ) THEN
               IF ( m(8)==bcdll ) i(8) = 1
               IF ( m(8)==bcdsl ) i(8) = 2
               IF ( m(8)==bcdls ) i(8) = 3
               IF ( m(8)/=bcdsl .AND. (k==94 .OR. k==95 .OR. k==134 .OR. k==140) ) baddat = .TRUE.
            ENDIF
!
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
               baddat = .TRUE.
               kn = 0
               km = 0
            ELSE
               kn = 1
               km = 1
            ENDIF
            spag_nextblock_1 = 17
         ENDIF
      CASE (13)
         badfor = .TRUE.
         spag_nextblock_1 = 14
      CASE (14)
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            kn = 0
            km = 0
            baddat = .TRUE.
         ENDIF
         spag_nextblock_1 = 17
      CASE (15)
         n = n + 2
         i(n-1) = -1
         i(n) = -1
         IF ( xl==xl1 ) baddat = .TRUE.
         IF ( items<2 ) baddat = .TRUE.
         spag_nextblock_1 = 16
      CASE (16)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            kn = 1
            km = 1
            baddat = .TRUE.
         ELSE
            kn = 0
            km = 0
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
         IF ( .NOT.(baddat .OR. badfor) ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         m(1) = i2
         spag_nextblock_1 = 3
      CASE (18)
!
!*******      96-TABLEM4, 141-TABLED4     ******************************
!
         IF ( km/=0 ) THEN
            l1 = 0
            SPAG_Loop_1_2: DO l = 1 , 8
               kword1 = 0
               IF ( mf(l)==3 ) THEN
                  l1 = l1 + 2
                  kword1 = m(l1-1)
                  IF ( kword1==endt ) THEN
                     spag_nextblock_1 = 19
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  baddat = .TRUE.
                  EXIT SPAG_Loop_1_2
               ELSE
                  IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  n = n + 1
                  items = items + 1
                  l1 = l1 + 1
                  i(n) = m(l1)
               ENDIF
            ENDDO SPAG_Loop_1_2
            spag_nextblock_1 = 14
         ELSE
            items = 0
            i2 = m(1)
            n = 8
            IF ( m(1)<=0 ) baddat = .TRUE.
            IF ( mf(1)/=1 ) badfor = .TRUE.
            i(1) = i2
            IF ( m(3)==0 ) baddat = .TRUE.
            DO l = 2 , 8
               IF ( mf(l)/=0 .AND. mf(l)/=2 .OR. l>=6 .AND. mf(l)/=0 ) badfor = .TRUE.
               i(l) = m(l)
            ENDDO
            i(8) = 0
            IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
               baddat = .TRUE.
               kn = 0
               km = 0
            ELSE
               kn = 1
               km = 1
            ENDIF
            spag_nextblock_1 = 17
         ENDIF
      CASE (19)
         n = n + 1
         i(n) = -1
         IF ( items<1 ) baddat = .TRUE.
         spag_nextblock_1 = 16
         CYCLE SPAG_DispatchLoop_1
 20      flush = .FALSE.
         flshal = .FALSE.
         ec = .TRUE.
         secd = .FALSE.
         t(1) = iscr1
         DO l = 2 , 7
            t(l) = 0
         ENDDO
         IF ( m(3)/=0 ) flush = .TRUE.
         onm(1) = nm(1)
         onm(2) = nm(2)
         IF ( mf(1)/=3 .OR. m(1)==onm(1) .AND. m(2)==onm(2) ) flush = .TRUE.
         nm(1) = m(1)
         nm(2) = m(2)
         iprint = 0
         j0 = 0
         IF ( p(1)>p(2) ) THEN
            flush = .TRUE.
            flshal = .TRUE.
         ENDIF
         ASSIGN 40 TO r1
         ASSIGN 60 TO r
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
 40      flush = .TRUE.
 60      IF ( flush ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ifo = m(4)
         ty1 = m(5)
         ty2 = m(6)
         IF ( ty2==0 .AND. mod(ty1,2)==1 ) ty2 = ty1 + kprec - 1
         IF ( ty2==0 .AND. mod(ty1,2)==0 ) ty2 = ty1
         IF ( mach==12 ) THEN
            IF ( ty2==2 .OR. ty2==4 ) ty2 = ty2 - 1
         ENDIF
         IF ( ty1<1 .OR. ty1>4 .OR. ty2<1 .OR. ty2>4 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ty1>=3 .AND. ty2<=2 ) WRITE (nout,99008) uwm , dmi , nam(1) , nam(2) , knt
         nrows = m(8)
         ncols = m(9)
         IF ( ifo>8 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( mf(6)/=0 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nrows<=0 .OR. ncols<=0 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( (ifo==1 .OR. ifo==6 .OR. ifo==8) .AND. (nrows/=ncols) ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nbuf2 = 2*nbuf
         CALL open(*100,iscr1,ibuf(nbuf2+1),1)
         CALL write(iscr1,nm,2,1)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ifo==8 ) THEN
            IF ( m1(1)==t1(1,k) .AND. m1(2)==t1(2,k) .AND. m1(3)==nm(1) .AND. m1(4)==nm(2) ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 26
         ELSE
            IF ( m1(1)/=t1(1,k) .OR. m1(2)/=t1(2,k) ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 26
         ENDIF
      CASE (20)
         lf = l + l1f
         IF ( .NOT.(fthru) ) THEN
            IF ( mf(lf)==0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(lf)==2 .OR. mf(lf)==4 ) THEN
               IF ( ty1==2 ) THEN
!   . REAL DOUBLE PRECISION
                  IF ( mf(lf)==2 ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  a(1) = m(l)
                  a(2) = m(l+1)
                  l = l + 1
                  l1f = l1f - 1
!
!     PACK AN ELEMENT
!
                  IF ( .NOT.(flush .OR. da(1)==0.0D0) ) CALL zblpki
               ELSEIF ( ty1==3 ) THEN
!   . COMPLEX SINGLE PRECISION
                  IF ( mf(lf)==4 ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( secd ) THEN
                     a(2) = m(l)
                     secd = .FALSE.
                     IF ( .NOT.(a(1)==0 .AND. a(2)==0 .OR. flush) ) CALL zblpki
                  ELSE
                     a(1) = m(l)
                     secd = .TRUE.
                     spag_nextblock_1 = 21
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( ty1==4 ) THEN
!   . COMPLEX DOUBLE PRECISION
                  IF ( mf(lf)==2 ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( secd ) THEN
                     a(3) = m(l)
                     a(4) = m(l+1)
                     l = l + 1
                     l1f = l1f - 1
                     secd = .FALSE.
                     IF ( .NOT.(flush .OR. da(1)==0.0D0 .AND. da(2)==0.0D0) ) CALL zblpki
                  ELSE
                     a(1) = m(l)
                     a(2) = m(l+1)
                     l = l + 1
                     l1f = l1f - 1
                     secd = .TRUE.
                     spag_nextblock_1 = 21
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSE
!   . REAL SINGLE PRECISION
                  IF ( mf(lf)==4 ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  IF ( .NOT.(flush .OR. m(l)==0) ) THEN
                     a(1) = m(l)
                     CALL zblpki
                  ENDIF
               ENDIF
               int = .FALSE.
               i0 = i0 + 1
               IF ( i0>nrows ) THEN
                  spag_nextblock_1 = 23
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( l+1>l2 ) THEN
                  spag_nextblock_1 = 21
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(lf+1)/=3 ) THEN
                  spag_nextblock_1 = 21
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               l = l + 1
            ELSE
               IF ( mf(lf)==-32767 ) THEN
                  spag_nextblock_1 = 22
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( int ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(lf)/=3 ) THEN
                  IF ( mf(lf)/=1 .OR. m(l)<i0 .OR. m(l)>nrows ) THEN
                     spag_nextblock_1 = 25
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  i0 = m(l)
                  int = .TRUE.
                  spag_nextblock_1 = 21
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            IF ( m(l)/=thru ) THEN
               spag_nextblock_1 = 25
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            fthru = .TRUE.
            l1f = l1f - 1
            l2 = l2 + 1
            l = l + 1
            IF ( l>=l2 ) THEN
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            l = l + 1
            lf = l + l1f
         ENDIF
         IF ( mf(lf)/=1 .OR. m(l)<i0 .OR. m(l)>nrows ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_3: DO
            CALL zblpki
            i0 = i0 + 1
            IF ( i0>m(l) ) THEN
               fthru = .FALSE.
               IF ( i0<=nrows ) EXIT SPAG_Loop_1_3
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_3
         spag_nextblock_1 = 21
      CASE (21)
         l = l + 1
         IF ( l<=l2 ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
         IF ( m1(1)==0 .AND. m1(2)==0 .OR. int ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 24
      CASE (23)
         IF ( l/=l2 ) THEN
            lf = lf + 1
            SPAG_Loop_1_4: DO lx = lf , 8
               IF ( mf(lx)==-32767 ) EXIT SPAG_Loop_1_4
               IF ( mf(lx)/=0 ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO SPAG_Loop_1_4
         ENDIF
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 24
      CASE (24)
         IF ( flush ) THEN
            ec = .TRUE.
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( .NOT.(secd) ) THEN
            IF ( .NOT.(fthru) ) THEN
               CALL bldpkn(iscr1,0,t)
               ec = .TRUE.
               spag_nextblock_1 = 26
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 25
      CASE (25)
         flush = .TRUE.
         spag_nextblock_1 = 26
      CASE (26)
         IF ( .NOT.(m1(1)==0 .AND. m1(2)==0 .OR. m1(1)==t1(1,k) .AND. m1(2)==t1(2,k)) ) THEN
            ASSIGN 80 TO r
            spag_nextblock_1 = 27
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 80      n = 0
         IF ( .NOT.(.NOT.flush .OR. iprint/=0) ) THEN
            CALL page2(2)
            WRITE (nout,99003) ufm , nm(1) , nm(2) , knt
99003       FORMAT (A23,' 325, BAD DATA OR FORMAT OR NON-UNIQUE NAME. DMI ',2A4,10X,' SORTED CARD COUNT =',I7)
            iprint = 1
         ENDIF
         spag_nextblock_1 = 32
      CASE (27)
         IF ( flshal ) THEN
            WRITE (nout,99004) sfm , nm(1) , nm(2)
99004       FORMAT (A25,' 326, NO ROOM IN /XDPL/ FOR DMI ',2A4)
            CALL page2(2)
            abort = .TRUE.
         ELSE
            IF ( .NOT.(flush) ) THEN
               IF ( ifo==8 ) THEN
                  t(2) = ncols
               ELSE
                  SPAG_Loop_1_5: DO
                     j0 = j0 + 1
                     IF ( j0>ncols ) THEN
                        IF ( ncols==t(2) ) EXIT SPAG_Loop_1_5
                        flush = .TRUE.
                        GOTO 90
                     ELSE
                        CALL bldpk(ty1,ty2,iscr1,0,0)
                        CALL bldpkn(iscr1,0,t)
                     ENDIF
                  ENDDO SPAG_Loop_1_5
               ENDIF
               CALL close(iscr1,1)
               CALL wrttrl(t)
               CALL rdtrl(t)
               IF ( icfiat==11 ) THEN
                  j = 6
               ELSE
                  DO lx = 1 , 3
                     t(lx+1) = orf(lshift(t(2*lx),16),t(2*lx+1))
                  ENDDO
                  j = 3
               ENDIF
               CALL write(pool,nm,2,0)
               CALL write(pool,t(2),j,1)
               IF ( l8/=0 ) WRITE (nout,99009) nm , dmi , (t(ip+1),ip=1,j)
               CALL gopen(iscr1,ibuf(2*nbuf+1),2)
               CALL cpyfil(iscr1,pool,ibuf(3*nbuf+1),nopen,nwords)
               CALL close(iscr1,1)
               CALL eof(pool)
               dmiflg = .TRUE.
               p(1) = p(1) + 1
            ENDIF
 90         ip = 3*p(3) + 4
            p(ip) = nm(1)
            p(ip+1) = nm(2)
            IF ( flush ) nwords = 0
            p(ip+2) = orf(lshift(nwords/1000,16),p(1)-1)
            p(3) = p(3) + 1
            IF ( flush ) THEN
               CALL close(iscr1,1)
               CALL eof(pool)
               p(1) = p(1) + 1
               CALL skpfil(pool,-1)
               IF ( dmiflg ) CALL eof(pool)
               abort = .TRUE.
            ENDIF
         ENDIF
         GOTO r
 100     CALL mesage(-1,iscr1,nm)
         spag_nextblock_1 = 28
      CASE (28)
!
!******          120-DMIG          ********************************
!
         IF ( fphys1 ) THEN
            fphys1 = .FALSE.
            nm(1) = 0
            nm(2) = 0
         ENDIF
         ierr = 0
         IF ( km/=0 ) THEN
            lf = 1
            l = 1
         ELSEIF ( m(3)==0 ) THEN
            IF ( mf(1)/=3 .OR. m(1)==nm(1) .AND. m(2)==nm(2) ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ifo = m(4)
            ty1 = m(5)
            ity1 = 2*mod(ty1,2)
            ty2 = m(6)
            IF ( ty2==0 .AND. mod(ty1,2)==1 ) ty2 = ty1 + kprec - 1
            IF ( ty2==0 .AND. mod(ty1,2)==0 ) ty2 = ty1
            IF ( mach==12 ) THEN
               IF ( ty2==2 .OR. ty2==4 ) ty2 = ty2 - 1
            ENDIF
            IF ( ty1<=0 .OR. ty1>4 .OR. ty2<=0 .OR. ty2>4 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ty1>=3 .AND. ty2<=2 ) WRITE (nout,99008) uwm , dmig , nm(1) , nm(2) , knt
            IF ( ifo/=1 .AND. ifo/=2 .AND. ifo/=6 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ty2==1 .AND. ty1==3 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            nm(1) = m(1)
            nm(2) = m(2)
            IF ( mf(6)/=0 .OR. mf(7)/=0 .OR. mf(8)/=0 ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m1f(2)/=3 .OR. m1(3)/=nm(1) .OR. m1(4)/=nm(2) ) THEN
               spag_nextblock_1 = 31
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            m(6) = ty2
            n = 9
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( m(1)/=nm(1) .OR. m(2)/=nm(2) ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(2)/=1 .OR. mf(3)/=1 .AND. mf(3)/=0 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m(3)<=0 .OR. m(4)<0 .OR. m(4)>6 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(4)/=0 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(5)/=1 .OR. mf(6)/=1 .AND. mf(6)/=0 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( m(6)<=0 .OR. m(7)<0 .OR. m(7)>6 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( mf(7)+ity1/=4 .AND. mf(7)/=0 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( (ty1==1 .OR. ty1==2) .AND. mf(8)/=0 .OR. ty1==3 .AND. mf(8)/=2 .AND. mf(8)/=0 .OR. ty1==4 .AND. mf(8)/=4 .AND.     &
               & mf(8)/=0 ) THEN
               spag_nextblock_1 = 30
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            n = 5
            i(n-4) = m(3)
            i(n-3) = m(4)
            i(n-2) = m(6)
            i(n-1) = m(7)
            i(n) = m(8)
            IF ( ty1/=1 ) THEN
               n = 6
               i(n) = m(9)
               IF ( ty1==4 ) THEN
                  n = 8
                  i(n-1) = m(10)
                  i(n) = m(11)
               ENDIF
            ENDIF
            IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
               spag_nextblock_1 = 34
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            n = n + 2
            i(n-1) = -1
            i(n) = -1
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_6: DO
            IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
               IF ( m(l)<=0 .OR. m(l+1)<0 .OR. m(l+1)>6 ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(lf)/=1 .OR. mf(lf+1)/=1 .AND. mf(lf+1)/=0 ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ierr = 1
               IF ( mf(lf+2)+ity1/=4 .AND. mf(lf+2)/=0 ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( mf(lf+3)/=0 .AND. ty1/=3 .AND. ty1/=4 ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = n + 3
               i(n-2) = m(l)
               i(n-1) = m(l+1)
               i(n) = m(l+2)
               lf = lf + 4
               l = l + 4
               IF ( ty1/=1 ) THEN
                  n = n + 1
                  i(n) = m(l-1)
                  IF ( ty1==2 ) l = l + 1
                  IF ( ty1==4 ) THEN
                     n = n + 2
                     i(n-1) = m(l)
                     i(n) = m(l+1)
                     l = l + 2
                  ENDIF
               ENDIF
            ELSE
               lf = lf + 4
               l = l + 4
            ENDIF
            IF ( lf>7 ) THEN
               IF ( n<=0 ) THEN
                  spag_nextblock_1 = 31
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               n = n + 2
               i(n-1) = -1
               i(n) = -1
               EXIT SPAG_Loop_1_6
            ENDIF
         ENDDO SPAG_Loop_1_6
         spag_nextblock_1 = 29
      CASE (29)
         IF ( m1(1)/=t1(1,k) .OR. m1(2)/=t1(2,k) .OR. m1(3)/=nm(1) .OR. m1(4)/=nm(2) ) THEN
            n = n + 2
            i(n-1) = -1
            i(n) = -1
         ENDIF
         spag_nextblock_1 = 33
      CASE (30)
         nm(1) = m(1)
         nm(2) = m(2)
         spag_nextblock_1 = 31
      CASE (31)
         abort = .TRUE.
         CALL page2(2)
         WRITE (nout,99005) ufm , nm(1) , nm(2) , knt
99005    FORMAT (A23,' 327, BAD DATA OR FORMAT OR NON-UNIQUE NAME. DMIG ',2A4,10X,' SORTED CARD COUNT =',I7)
         IF ( ierr==1 ) WRITE (nout,99006)
99006    FORMAT (5X,'INPUT MATRIX TYPE (TIN) AND INPUT DATA (XIJ OR YIJ) ','ARE NOT CONSISTANT')
         spag_nextblock_1 = 32
      CASE (32)
         IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 33
      CASE (33)
         km = 0
         kn = 0
         spag_nextblock_1 = 5
      CASE (34)
         kn = 1
         km = 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     flush = .FALSE.
         flshal = .FALSE.
         IF ( m(3)/=0 ) flush = .TRUE.
         onm(1) = nm(1)
         onm(2) = nm(2)
         IF ( mf(1)/=3 .OR. m(1)==onm(1) .AND. m(2)==onm(2) ) flush = .TRUE.
         nm(1) = m(1)
         nm(2) = m(2)
         iprint = 0
         nwords = 2
         j0 = 0
         IF ( p(1)>p(2) ) THEN
            flush = .TRUE.
            flshal = .TRUE.
         ENDIF
         ASSIGN 140 TO r1
         ASSIGN 160 TO r
         spag_nextblock_1 = 38
         CYCLE SPAG_DispatchLoop_1
 140     flush = .TRUE.
 160     IF ( flush ) THEN
            spag_nextblock_1 = 36
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         itrlt = 0
         DO l = 2 , 7
            itrlt = itrlt + m(l+2)
            IF ( icfiat==8 .AND. (m(l+2)<0 .OR. m(l+2)>65535) ) flush = .TRUE.
!     2147483647 = 2**31-1
            IF ( icfiat==11 .AND. (m(l+2)<0 .OR. m(l+2)>2147483647) ) flush = .TRUE.
            t(l) = m(l+2)
         ENDDO
         IF ( itrlt==0 ) THEN
            DO l = 2 , 7
               t(l) = 32767
            ENDDO
         ENDIF
         CALL write(pool,nm,2,0)
         IF ( icfiat==11 ) THEN
            l = 6
         ELSE
            DO lx = 1 , 3
               t(lx+1) = orf(lshift(t(2*lx),16),t(2*lx+1))
            ENDDO
            l = 3
         ENDIF
         CALL write(pool,t(2),l,1)
         CALL write(pool,nm,2,0)
         IF ( l8/=0 ) WRITE (nout,99009) nm , dti , (t(ip+1),ip=1,j)
         IF ( m1(1)==t1(1,k) .AND. m1(2)==t1(2,k) ) CALL write(pool,nm,0,1)
         GOTO 180
      CASE (35)
         l = l1
         lf = l + l1f
         DO WHILE ( mf(lf)/=3 .OR. m(l)/=endrc1 .OR. m(l+1)/=endrc2 )
            IF ( mf(lf)>2 ) l = l + 1
            l = l + 1
            lf = lf + 1
            IF ( mf(lf)<0 ) THEN
               CALL write(pool,m(l1),l-l1,0)
               nwords = nwords + l - l1
               IF ( m1(1)==0 .AND. m1(2)==0 ) GOTO 180
               flush = .TRUE.
               spag_nextblock_1 = 36
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL write(pool,m(l1),l-l1,1)
         nwords = nwords + l - l1
         IF ( m1(1)==0 .AND. m1(2)==0 ) flush = .TRUE.
         spag_nextblock_1 = 36
      CASE (36)
         IF ( .NOT.(m1(1)==0 .AND. m1(2)==0 .OR. m1(1)==t1(1,k) .AND. m1(2)==t1(2,k)) ) THEN
            ASSIGN 180 TO r
            spag_nextblock_1 = 37
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 180     n = 0
         IF ( .NOT.(.NOT.flush .OR. iprint/=0) ) THEN
            CALL page2(2)
            WRITE (nout,99010) ufm , nm(1) , nm(2) , knt
            iprint = 1
         ENDIF
         spag_nextblock_1 = 32
      CASE (37)
         IF ( flshal ) THEN
            WRITE (nout,99007) sfm , nm(1) , nm(2)
99007       FORMAT (A25,' 318, NO ROOM IN /XDPL/ FOR DTI ',2A4)
            CALL page2(2)
            abort = .TRUE.
         ELSE
            IF ( .NOT.(flush) ) THEN
               CALL eof(pool)
               dmiflg = .TRUE.
               p(1) = p(1) + 1
            ENDIF
            ip = 3*p(3) + 4
            p(ip) = nm(1)
            p(ip+1) = nm(2)
            IF ( flush ) nwords = 0
            p(ip+2) = orf(lshift(nwords/1000,16),p(1)-1)
            p(3) = p(3) + 1
            IF ( flush ) THEN
               CALL page2(2)
               WRITE (nout,99010) ufm , nm(1) , nm(2) , knt
               CALL eof(pool)
               p(1) = p(1) + 1
               CALL skpfil(pool,-1)
               IF ( dmiflg ) CALL skpfil(pool,+1)
               abort = .TRUE.
            ENDIF
         ENDIF
         GOTO r
      CASE (38)
!
!     ******************************************************************
!
!     CHECK NAME FOR UNIQUENESS AMONG DMI CARDS, DTI CARDS, ETC. AND
!     RESERVED NAMES
!
!
!     CHECK  FIST, FIAT, DPL FOR A NAME MATCH
!
         DO ii = 1 , ipfist
            IF ( nm(1)==ifist(2*ii+1) .AND. nm(2)==bcdblk ) THEN
               spag_nextblock_1 = 39
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         nfiat = icfiat*ifiat(2) - 2
         DO ii = 4 , nfiat , icfiat
            IF ( nm(1)==ifiat(ii) .AND. nm(2)==ifiat(ii+1) ) THEN
               spag_nextblock_1 = 39
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         ndpl = p(3)*3 + 1
         DO ii = 4 , ndpl , 3
            IF ( nm(1)==p(ii) .AND. nm(2)==p(ii+1) ) THEN
               spag_nextblock_1 = 39
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         GOTO r
      CASE (39)
         GOTO r1
      CASE (40)
         kn = 0
         icomp = 1
         i(n+1) = -1
         n = n + 1
         spag_nextblock_1 = 6
      CASE (41)
         kn = 0
         icomp = 1
         i(n+1) = -1
         n = n + 1
         spag_nextblock_1 = 6
      CASE (42)
         kn = 0
         icomp = 1
         i(n+1) = -1
         n = n + 1
         spag_nextblock_1 = 6
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99008 FORMAT (A25,' 327A, ',A4,' CARD ',2A4,', SORTED CARD COUNT =',I7,' SPECIFYING COMPLEX DATA INPUT',/5X,                        &
             &'AND REAL MATRIX OUTPUT MAY NOT MAKE SENSE',/)
99009 FORMAT ('0*** DIAG  8 MESSAGE -- TRAILER FOR DATA BLOCK ',2A4,' (VIA ',A4,' CARDS) = ',5I7,I9)
99010 FORMAT (A23,' 317, BAD DATA OR FORMAT OR NON-UNIQUE NAME FOR DTI ',2A4,10X,'SORTED CARD COUNT =',I7)
!
END SUBROUTINE ifs2p
