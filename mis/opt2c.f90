
SUBROUTINE opt2c(Pt,Iel,Ipr,Pr,Rr)
   IMPLICIT NONE
   INTEGER B1 , Count , Eid , Est1 , Est2 , Ig10 , Incr , Iprnt , Iy(1) , Iz(100) , Last , Lpch , Max , Ncard , Ne(1) , Nelop ,     &
         & Nelw , Nklw , Nlines , Nlpp , Noeor , Nprw , Nrd , Ntotl , Ntypes , Nwdse , Nwdsp , Nweor , Nwrt , Outtap , Sysbuf ,     &
         & Ycor , Zcor
   REAL Conv , Core(1) , G(2,10) , Parm(8) , Skp1(2) , Skp2 , Skp3(2) , Skp4 , Skps1(6) , Skps2(2) , Skps3(78) , Y(1) , Z(100)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Skp1 , Count , Ncard , Skp2 , Ycor , B1 , Nelop , Nwdse , Nwdsp , Skp3 , Est1 , Skp4 , Est2 , Nelw , Nprw ,      &
                 & Nklw , Ntotl , Conv
   COMMON /gpta1 / Ntypes , Last , Incr , Ne
   COMMON /names / Nrd , Noeor , Nwrt , Nweor
   COMMON /optpw2/ Zcor , Z
   COMMON /system/ Sysbuf , Outtap , Skps1 , Nlpp , Skps2 , Nlines , Skps3 , Lpch
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Core
   INTEGER Iel(1) , Ipr(1) , Pt(2,1)
   REAL Pr(1) , Rr(1)
   REAL a , blk , c , ch , pc , pcd(2,21) , plus , yes
   INTEGER eject , khrfn3
   INTEGER etyp , headng , i , icp , ie1 , ie2 , ii , ip1 , ipl , irr , itp , j , jj , k , k1 , k2 , k3 , kk , kount , l , lel ,    &
         & locf , mcb(7) , n , name(2) , neop(21) , nmes , npcard , nwds , quad4 , tria3 , trim6 , tube , wdopt(42)
   LOGICAL kpun
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
!
!     DETERMINE IF PROPTETY CARDS ARE TO BE PUNCHED
!
   kpun = .FALSE.
   kount = 0
   headng = 0
   ch = 1.0
   icp = Ntotl
   IF ( Count==Max .OR. Conv==2.0 ) kpun = .TRUE.
   IF ( Parm(5)/=yes ) kpun = .FALSE.
   IF ( Iprnt/=0 ) Nlines = Nlpp
   ie2 = 1
   lel = 0
!
!     READ EST1 ELEMENT TYPE
!
 100  CALL read(*1800,*1300,Est1,etyp,1,Noeor,i)
   CALL write(Est2,etyp,1,Noeor)
   itp = Iy(etyp)
   IF ( itp/=0 ) THEN
      ie1 = Pt(1,itp)
!
!     CHECK IF CORE ELEMENTS SKIPPED BECAUSE TYPE NOT ON EST
!
      IF ( ie1>ie2 ) GOTO 500
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
         GOTO 400
      ENDIF
   ENDIF
!
!     SKIP THIS ELEMENT TYPE.  COPY RECORD TO EST2
!
 200  j = 1
   n = Zcor
   CALL read(*300,*300,Est1,Z,Zcor,Noeor,n)
   j = 0
 300  CALL write(Est2,Z(1),n,j)
   IF ( j==0 ) GOTO 200
   GOTO 100
 400  DO
!
!     READ ONE EST1 ELEMENT INTO CORE
!
      CALL read(*1200,*1100,Est1,Z,nwds,Noeor,i)
      IF ( Eid<lel ) THEN
!
!     ELEMENT ID NOT IN CORE
!
         CALL write(Est2,Iz(1),nwds,Noeor)
      ELSEIF ( Eid==lel ) THEN
         GOTO 600
      ELSE
         EXIT
      ENDIF
   ENDDO
!
!     ELEMENT IN CORE NOT ON EST
!
 500  i = eject(2)
   IF ( i/=0 ) THEN
      IF ( Count==Max .OR. Conv==2.0 ) THEN
         WRITE (Outtap,99006) Count
      ELSE
         WRITE (Outtap,99007) Count
      ENDIF
   ENDIF
   WRITE (Outtap,99001) Sfm , etyp , lel , name
99001 FORMAT (A25,' 2297, INCORRECT LOGIC FOR ELEMENT TYPE',I4,', ELEMENT',I8,2H (,2A4,2H).)
   CALL mesage(-61,lel,name)
!
!     ELEMENT IN CORE - CONVERT THE ENTRIES
!
 600  ipl = Iel(ie1+4) + ip1
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
      IF ( abs(Parm(3)-1.0)<0.0001 ) ch = 0.25*Rr(irr) + 0.75
      c = (a/(a+(1.0-a)*Parm(3)))**ch
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
            pc = Y(icp+jj)
            Z(l) = Z(l)*(pc/(pc+(1.0-pc)*Parm(3)))
         ENDDO
         icp = icp + 4
      ENDIF
      CALL write(Est2,Z(1),nwds,Noeor)
!
!     PUNCH AND/OR PRINT PROPERTY CARDS
!
      IF ( Iprnt==0 .OR. Ipr(ipl)<=0 ) GOTO 400
      IF ( itp==2 ) THEN
!
!     PELBOW
!
         k1 = 02222211
         k2 = 22222222
         k3 = 22222222
         GOTO 700
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
         GOTO 650
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
         GOTO 700
      ENDIF
!
!     OUTPUT THE CARD(S)
!
      k2 = 0
 650  k3 = 0
 700  ii = wdopt(locf+1) - 4
      kk = k1
      G(1,1) = pcd(1,itp)
      G(2,1) = pcd(2,itp)
      Iz(ii+2) = Ipr(ipl)
      Ipr(ipl) = -Ipr(ipl)
   ELSE
      nmes = nmes + 1
      IF ( Iprnt/=0 .AND. nmes<=100 ) THEN
         i = eject(2)
         IF ( i/=0 ) THEN
            IF ( Count==Max .OR. Conv==2.0 ) THEN
               WRITE (Outtap,99007) Count
            ELSE
               WRITE (Outtap,99007) Count
            ENDIF
         ENDIF
         WRITE (Outtap,99002) Uim , Eid
99002    FORMAT (A29,' 2305, OPTPR2 DETECTED NEGATIVE ALPHA FOR ELEMENT',I8)
         CALL write(Est2,Iz(1),nwds,Noeor)
         GOTO 400
      ENDIF
      CALL write(Est2,Iz(1),nwds,Noeor)
      GOTO 400
   ENDIF
 800  DO i = 2 , 9
      G(1,i) = blk
      G(2,i) = blk
      j = mod(kk,10)
      IF ( j/=0 ) THEN
         IF ( j==1 ) CALL int2a8(*1400,Iz(i+ii),G(1,i))
         IF ( j==2 ) CALL fp2a8(*1600,Z(i+ii),G(1,i))
      ENDIF
      kk = kk/10
   ENDDO
   G(1,10) = blk
   G(2,10) = blk
   IF ( k2==0 .OR. (k2==-1 .AND. k3==0) .OR. k3==-1 ) GOTO 1000
   kount = kount + 1
   CALL int2a8(*1500,kount,G(1,10))
   G(2,10) = G(1,10)
   Ig10 = khrfn3(G(1,1),plus,-3,1)
   IF ( headng==0 ) GOTO 1000
 900  WRITE (Outtap,99003) G
99003 FORMAT (5X,10(2A4,1X))
   IF ( kpun ) THEN
      WRITE (Lpch,99004) G
99004 FORMAT (20A4)
      Ncard = Ncard + 1
   ENDIF
!
!     SET UP FOR CONTINUATION CARD(S)
!
   IF ( k2==0 .OR. (k2==-1 .AND. k3==0) .OR. k3==-1 ) GOTO 400
   G(1,1) = G(1,10)
   G(2,1) = G(2,10)
   ii = ii + 8
   IF ( k2<0 ) THEN
      kk = k3
      k3 = -1
   ELSEIF ( k2==0 ) THEN
      GOTO 400
   ELSE
      kk = k2
      k2 = -1
   ENDIF
   GOTO 800
!
!     PRINT HEADING
!
 1000 headng = 1
   IF ( eject(1)/=0 ) THEN
      IF ( Count==Max .OR. Conv==2.0 ) THEN
         WRITE (Outtap,99006) Count
      ELSE
         WRITE (Outtap,99007) Count
      ENDIF
   ENDIF
   GOTO 900
!
!     EOR ON EST1
!
 1100 CALL write(Est2,0,0,Nweor)
   IF ( ie1>=ie2 ) GOTO 100
   GOTO 500
!
!     ERRORS
!
 1200 CALL mesage(-2,Est1,name)
 1300 CALL mesage(-3,Est1,name)
 1400 j = 370
   GOTO 1700
 1500 j = 375
   i = kount
   GOTO 1700
 1600 j = 380
 1700 WRITE (Outtap,99005) j , G(1,1) , G(2,1) , i , ii , Iz(i+ii) , Z(i+ii)
99005 FORMAT (16H0*** OPT2C/ERROR,I5,9X,5HELEM ,2A4,3I9,E10.4)
   GOTO 400
!
 1800 CALL eof(Est2)
   mcb(1) = Est1
   CALL rdtrl(mcb)
   mcb(1) = Est2
   CALL wrttrl(mcb)
99006 FORMAT (1H0,8X,38HPROPERTIES USED DURING FINAL ITERATION,I5,10H BY OPTPR2/)
99007 FORMAT (1H0,8X,45HPROPERTIES USED DURING INTERMEDIATE ITERATION,I5,10H BY OPTPR2/)
END SUBROUTINE opt2c