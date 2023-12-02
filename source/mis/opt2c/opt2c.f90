!*==opt2c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE opt2c(Pt,Iel,Ipr,Pr,Rr)
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_NAMES
   USE C_OPTPW2
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2,1) :: Pt
   INTEGER , DIMENSION(1) :: Iel
   INTEGER , DIMENSION(1) :: Ipr
   REAL , DIMENSION(1) :: Pr
   REAL , DIMENSION(1) :: Rr
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , c , ch , pc
   REAL , SAVE :: blk , plus , yes
   INTEGER :: eid , etyp , headng , i , icp , ie1 , ie2 , ig10 , ii , ip1 , ipl , iprnt , irr , itp , j , jj , k , k1 , k2 , k3 ,   &
            & kk , kount , l , lel , locf , max , n , npcard , nwds
   REAL , DIMENSION(2,10) :: g
   INTEGER , DIMENSION(1) :: iy
   INTEGER , DIMENSION(100) :: iz
   LOGICAL :: kpun
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(21) , SAVE :: neop
   INTEGER , SAVE :: nmes , quad4 , tria3 , trim6 , tube
   REAL , DIMENSION(8) :: parm
   REAL , DIMENSION(2,21) , SAVE :: pcd
   INTEGER , DIMENSION(42) , SAVE :: wdopt
   REAL , DIMENSION(1) :: y
   EXTERNAL eject , eof , fp2a8 , int2a8 , khrfn3 , mesage , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Iz(1),Z(1)) , (Eid,Z(1)) , (Core(1),Parm(1),Max) , (G(1,1),Iz(100)) , (G(1,10),Ig10) , (Iprnt,Parm(7)) ,            &
!>>>>    & (Iy(1),Y(1),Parm(8))
!     EQUIVALENT ARE  (IPR,PR)
!
!
!     NOTE - CHANGE EQUIVALENCE IF AN ELEMENT TO BE OPTIMIZED HAS EST
!     (EPT ONLY) ENTRIES BEYOND 100 WORDS.
!
   DATA name/4H OPT , 4H2C  /
   DATA nmes , yes , plus , blk/0 , 4HYES  , 4H+AAA , 4H    /
   DATA tube , quad4 , trim6 , tria3/3 , 64 , 73 , 83/
   DATA pcd/4HPBAR , 4H     , 4HPELB , 4HOW   , 4HPIS2 , 4HD8   , 4HPQDM , 4HEM   , 4HPQDM , 4HEM1  , 4HPQDM , 4HEM2  , 4HPQDP ,    &
       &4HLT   , 4HPQUA , 4HD1   , 4HPQUA , 4HD2   , 4HPROD , 4H     , 4HPSHE , 4HAR   , 4HPTRB , 4HSC   , 4HPTRI , 4HA1   ,        &
      & 4HPTRI , 4HA2   , 4HPTRI , 4HM6   , 4HPTRM , 4HEM   , 4HPTRP , 4HLT   , 4HPTUB , 4HE    , 4HPSHE , 4HLL   , 4HPSHE ,        &
      & 4HLL   , 4HYYYY , 4H    /
!
!     POINTERS TO WORDS ON EST TO CONVERT.  NEOP(ITP) IS POINTER INTO
!     -WDOPT- ARRAY.  THE -WDOPT- FIRST ENTRY FOR THE ELEMENT IS THE
!     NUMBER OF ENTRIES ON -EST- TO CONVERT FOLLOWED BY THE WORD NUMBERS
!     TO OPTIMIZE.
!
   DATA neop/21 , 30 , 39 , 15 , 15 , 15 , 27 , 17 , 15 , 1 , 6 , 12 , 8 , 6 , 35 , 6 , 12 , 4 , 41 , 41 , 0/
!
!     ROD (A,J)
!
!     TUBE (O.D.)
!
!     SHEAR(T), TRMEM(T), TRIA2(T)
!
!     TRIA1(T1,T2,I)
!
!     TRBSC(T2,I),TRPLT(T2,I)
!
!     QDMEM(T), QDMEM1(T), QDMEM2(T), QUAD2(T)
!
!     QUAD1(T1,T2,I)
!
!     BAR(A,J,I1,I2,I12)
!
!     QDPLT(T2,I)
!
!     ELBOW(A,J,I1,I2)
!
!     TRIM6(T1,T3,T5)
!
!     IS2D8(T)
!
!     QUAD4(T), TRIA3(T) PSHELL ONLY
   DATA wdopt/2 , 5 , 6 , 1 , 5 , 1 , 7 , 3 , 7 , 9 , 11 , 2 , 7 , 9 , 1 , 8 , 3 , 8 , 10 , 12 , 5 , 17 , 18 , 19 , 20 , 33 , 2 ,   &
      & 8 , 10 , 4 , 9 , 10 , 11 , 12 , 3 , 10 , 11 , 12 , 1 , 13 , 1 , 14/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DETERMINE IF PROPTETY CARDS ARE TO BE PUNCHED
!
         kpun = .FALSE.
         kount = 0
         headng = 0
         ch = 1.0
         icp = Ntotl
         IF ( Count==max .OR. Conv==2.0 ) kpun = .TRUE.
         IF ( parm(5)/=yes ) kpun = .FALSE.
         IF ( iprnt/=0 ) Nlines = Nlpp
         ie2 = 1
         lel = 0
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ EST1 ELEMENT TYPE
!
         CALL read(*180,*100,Est1,etyp,1,Noeor,i)
         CALL write(Est2,etyp,1,Noeor)
         itp = iy(etyp)
         IF ( itp/=0 ) THEN
            ie1 = Pt(1,itp)
!
!     CHECK IF CORE ELEMENTS SKIPPED BECAUSE TYPE NOT ON EST
!
            IF ( ie1>ie2 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ie2 = Pt(1,itp+1)
            lel = Iel(ie1)
            ip1 = Pt(2,itp) - 1
            IF ( ie2>ie1 ) THEN
!
!     ELEMENT TYPE HAS CORE ENTRIES
!
               nwds = Incr*(etyp-1) + 12
               nwds = Ne(nwds)
               npcard = 0
               IF ( nwds>Zcor ) CALL mesage(-8,Zcor,name)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     SKIP THIS ELEMENT TYPE.  COPY RECORD TO EST2
!
         j = 1
         n = Zcor
         CALL read(*20,*20,Est1,Z,Zcor,Noeor,n)
         j = 0
 20      CALL write(Est2,Z(1),n,j)
         IF ( j/=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         SPAG_Loop_1_1: DO
!
!     READ ONE EST1 ELEMENT INTO CORE
!
            CALL read(*80,*60,Est1,Z,nwds,Noeor,i)
            IF ( eid<lel ) THEN
!
!     ELEMENT ID NOT IN CORE
!
               CALL write(Est2,iz(1),nwds,Noeor)
            ELSEIF ( eid==lel ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 5
      CASE (5)
!
!     ELEMENT IN CORE NOT ON EST
!
         i = eject(2)
         IF ( i/=0 ) THEN
            IF ( Count==max .OR. Conv==2.0 ) THEN
               WRITE (Outtap,99006) Count
            ELSE
               WRITE (Outtap,99007) Count
            ENDIF
         ENDIF
         WRITE (Outtap,99001) Sfm , etyp , lel , name
99001    FORMAT (A25,' 2297, INCORRECT LOGIC FOR ELEMENT TYPE',I4,', ELEMENT',I8,2H (,2A4,2H).)
         CALL mesage(-61,lel,name)
         spag_nextblock_1 = 6
      CASE (6)
!
!     ELEMENT IN CORE - CONVERT THE ENTRIES
!
         ipl = Iel(ie1+4) + ip1
         ie1 = ie1 + Nwdse
         lel = Iel(ie1)
         IF ( ie1>ie2 ) lel = 100000000
         a = Pr(ipl+4)
         IF ( a>0.0 ) THEN
!
            locf = neop(itp)
            j = locf
            k = wdopt(locf)
            irr = (ipl+Nwdsp)/Nwdsp
            IF ( abs(parm(3)-1.0)<0.0001 ) ch = 0.25*Rr(irr) + 0.75
            c = (a/(a+(1.0-a)*parm(3)))**ch
            IF ( etyp/=trim6 ) THEN
!
               DO i = 1 , k
                  j = j + 1
                  l = wdopt(j)
                  Z(l) = c*Z(l)
               ENDDO
               IF ( etyp==quad4 .OR. etyp==tria3 ) THEN
                  Z(l+6) = 0.5*Z(l)
                  Z(l+7) = -0.5*Z(l)
               ENDIF
               IF ( etyp==tube .AND. Z(l)<2.*Z(l+1) ) Z(l+1) = .5*Z(l)
            ELSE
!
!     SPECIAL HANDLING FOR TRIM6
!     IF THICKNESS-3 OR THICKNESS-5 IS ZERO, SET EQUAL TO THICKNESS-1
!
               DO jj = 1 , k
                  j = j + 1
                  l = wdopt(j)
                  IF ( jj/=k .AND. abs(Z(l+1))<1.E-7 ) Z(l+1) = Z(l)
                  pc = y(icp+jj)
                  Z(l) = Z(l)*(pc/(pc+(1.0-pc)*parm(3)))
               ENDDO
               icp = icp + 4
            ENDIF
            CALL write(Est2,Z(1),nwds,Noeor)
!
!     PUNCH AND/OR PRINT PROPERTY CARDS
!
            IF ( iprnt==0 .OR. Ipr(ipl)<=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( itp==2 ) THEN
!
!     PELBOW
!
               k1 = 02222211
               k2 = 22222222
               k3 = 22222222
               GOTO 40
            ELSEIF ( itp==3 ) THEN
!
!     PIS2D8
!
               k1 = 00000211
            ELSEIF ( itp==4 .OR. itp==5 .OR. itp==6 .OR. itp==9 .OR. itp==11 .OR. itp==14 .OR. itp==16 ) THEN
!
!     PQDMEM, PQDMEM1, PQDMEM2, PQUAD2, PSHEAR, PTRIA2, PTRMEM
!
               k1 = 00002211
            ELSEIF ( itp==7 .OR. itp==12 .OR. itp==17 ) THEN
!
!     PQDPLT, PTRBSC, PTRPLT
!
               k1 = 22221211
            ELSEIF ( itp==8 .OR. itp==13 .OR. itp==19 .OR. itp==20 ) THEN
!
!     PQUAD1, PTRIA1, PSHELL
!
               k1 = 22121211
               k2 = 00000022
               GOTO 30
            ELSEIF ( itp==10 .OR. itp==15 ) THEN
!
!     PROD, PTRIM6
!
               k1 = 00222211
            ELSEIF ( itp==18 ) THEN
!
!     PTUBE
!
               k1 = 00022211
            ELSE
!
!     PBAR
!
               k1 = 02222211
               k2 = 22222222
               k3 = 00000222
               GOTO 40
            ENDIF
!
!     OUTPUT THE CARD(S)
!
            k2 = 0
 30         k3 = 0
 40         ii = wdopt(locf+1) - 4
            kk = k1
            g(1,1) = pcd(1,itp)
            g(2,1) = pcd(2,itp)
            iz(ii+2) = Ipr(ipl)
            Ipr(ipl) = -Ipr(ipl)
         ELSE
            nmes = nmes + 1
            IF ( iprnt/=0 .AND. nmes<=100 ) THEN
               i = eject(2)
               IF ( i/=0 ) THEN
                  IF ( Count==max .OR. Conv==2.0 ) THEN
                     WRITE (Outtap,99007) Count
                  ELSE
                     WRITE (Outtap,99007) Count
                  ENDIF
               ENDIF
               WRITE (Outtap,99002) Uim , eid
99002          FORMAT (A29,' 2305, OPTPR2 DETECTED NEGATIVE ALPHA FOR ELEMENT',I8)
               CALL write(Est2,iz(1),nwds,Noeor)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL write(Est2,iz(1),nwds,Noeor)
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         DO i = 2 , 9
            g(1,i) = blk
            g(2,i) = blk
            j = mod(kk,10)
            IF ( j/=0 ) THEN
               IF ( j==1 ) CALL int2a8(*120,iz(i+ii),g(1,i))
               IF ( j==2 ) CALL fp2a8(*160,Z(i+ii),g(1,i))
            ENDIF
            kk = kk/10
         ENDDO
         g(1,10) = blk
         g(2,10) = blk
         IF ( k2==0 .OR. (k2==-1 .AND. k3==0) .OR. k3==-1 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         kount = kount + 1
         CALL int2a8(*140,kount,g(1,10))
         g(2,10) = g(1,10)
         ig10 = khrfn3(g(1,1),plus,-3,1)
         IF ( headng==0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         WRITE (Outtap,99003) g
99003    FORMAT (5X,10(2A4,1X))
         IF ( kpun ) THEN
            WRITE (Lpch,99004) g
99004       FORMAT (20A4)
            Ncard = Ncard + 1
         ENDIF
!
!     SET UP FOR CONTINUATION CARD(S)
!
         IF ( k2==0 .OR. (k2==-1 .AND. k3==0) .OR. k3==-1 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         g(1,1) = g(1,10)
         g(2,1) = g(2,10)
         ii = ii + 8
         IF ( k2<0 ) THEN
            kk = k3
            k3 = -1
         ELSEIF ( k2==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            kk = k2
            k2 = -1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
!     PRINT HEADING
!
         headng = 1
         IF ( eject(1)/=0 ) THEN
            IF ( Count==max .OR. Conv==2.0 ) THEN
               WRITE (Outtap,99006) Count
            ELSE
               WRITE (Outtap,99007) Count
            ENDIF
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
!
!     EOR ON EST1
!
 60      CALL write(Est2,0,0,Nweor)
         IF ( ie1<ie2 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ERRORS
!
 80      CALL mesage(-2,Est1,name)
 100     CALL mesage(-3,Est1,name)
 120     j = 370
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 140     j = 375
         i = kount
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 160     j = 380
         spag_nextblock_1 = 10
      CASE (10)
         WRITE (Outtap,99005) j , g(1,1) , g(2,1) , i , ii , iz(i+ii) , Z(i+ii)
99005    FORMAT (16H0*** OPT2C/ERROR,I5,9X,5HELEM ,2A4,3I9,E10.4)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
 180     CALL eof(Est2)
         mcb(1) = Est1
         CALL rdtrl(mcb)
         mcb(1) = Est2
         CALL wrttrl(mcb)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99006 FORMAT (1H0,8X,38HPROPERTIES USED DURING FINAL ITERATION,I5,10H BY OPTPR2/)
99007 FORMAT (1H0,8X,45HPROPERTIES USED DURING INTERMEDIATE ITERATION,I5,10H BY OPTPR2/)
END SUBROUTINE opt2c
