
SUBROUTINE ifp1b
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bits(1) , Casecc , Core(2) , Corey(401) , Iben , Iblank , Ibuf , Icase(200,2) , Icc , Ieor , Is , Istr , Isub , Lbd ,    &
         & Lcc , Lencc , Ncpw4 , Nmodes , Nout , Nset , Nsym , Nwpc , Two1(32)
   REAL Corex(1) , Equal , Scr1 , Zzzzbb
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /ifp1a / Scr1 , Casecc , Is , Nwpc , Ncpw4 , Nmodes , Icc , Nset , Nsym , Zzzzbb , Istr , Isub , Lencc , Iben , Equal ,   &
                 & Ieor
   COMMON /ifpx0 / Lbd , Lcc , Bits
   COMMON /system/ Ibuf , Nout
   COMMON /two   / Two1
   COMMON /xifp1 / Iblank
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Corex
!
! Local variable declarations
!
   INTEGER andf , korsz , orf
   INTEGER case , cc , i , ib1 , ib11 , ib2 , ib21 , ibit(200) , ibuf1 , ibuf2 , icrq , iecase , ieoptp , ifcase , ifirod , ifirst ,&
         & iflag , ifoptp , ifreq , ifreq1 , ihop , ik1 , ik11 , ik2 , ik21 , iloop , im1 , im11 , im2 , im21 , impc , impc1 ,      &
         & imtd , imtd1 , in , ioloop , iopn , ip1 , ip2 , ipcase , ipoptp , iqcase , iqoptp , ispc , ispc1 , itfl , itfl1 ,        &
         & iword(200) , ix , iy , j , jump , k , l , leng1 , leng2 , lsym , m , m0 , m1 , m2 , mm , name(2) , nz , optp , ss
   LOGICAL debug , new
   EXTERNAL andf , orf
!
! End of declarations
!
!
!     THIS ROUTINE DETERMINES THE LOOP CONDITIONS AND CASE CONTROL
!     REQUEST CHNGES
!     LOOP$ -- THE CURRENT PROBLEM WILL LOOP
!
!     LOOP1$-- THE OLD PROBLEM WAS A LOOP AND CASE CONTROL IS CHANGED
!     IN LENGTH
!
!     COMMENTS FROM G.C.  10/92
!     IWORD AND IBIT 200 WORDS EACH CORRESPOND TO 200 WORDS IN CASECC
!     ZERO IN IWORD MEANS NO FURTHER CHECKING
!     INTEGER VALUE IN IBIT POINTS TO RESTART BIT POSITION, AND WILL BE
!     SAVED IN BITS(17) AND BITS(18), BITS FOR LCC. (LBD = 16)
!
!     LAST REVISED  7/91, BY G.CHAN/UNISYS, TO ALLOW HUGE THRU-RANGE ON
!     SET IN CASE CONTROL SECTION FOR PRINTOUT OR PLOTTING
!
   EQUIVALENCE (Corex(1),Corey(1),Icase(1,1)) , (Core(1),Corey(401))
   DATA name/4HIFP1 , 4HB   /
   DATA case , cc/4HCASE , 4HCC  /
   DATA ss/4HSS  /
   DATA optp/4HOPTP/
   DATA iword/ - 1 , 01 , 01 , 01 , 01 , 01 , 01 , 01 , 01 , 00 , 01 , 01 , 01 , 01 , 01 , -1 , 00 , 01 , 01 , 00 , 01 , 01 , 00 ,  &
      & 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 01 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 00 , -1 , -1 , 01 , 01 , 01 , 01 , 01 , 01 , 01 , 00 , 00 , -1 , 01 ,&
      & 01 , -1 , 00 , 01 , 01 , 00 , 01 , 01 , 00 , 01 , 01 , 01 , -1 , -1 , 01 , 01 , 01 , -1 , 00 , 01 , 01 , 00 , 01 , 01 , 00 ,&
      & 01 , 01 , 00 , 01 , 01 , 01 , 00 , 01 , 01 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,&
      & -1 , -1/
   DATA ibit/00 , 02 , 03 , 04 , 05 , 06 , 07 , 08 , 09 , 10 , 10 , 10 , 13 , 14 , 15 , 00 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 ,&
      & 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 17 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 16 , 00 , 00 , 20 , 21 , 21 , 22 , 22 , 23 , 23 , 18 , 18 , 00 , 24 , 25 ,&
      & 00 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 27 , 00 , 00 , 30 , 26 , 29 , 00 , 18 , 18 , 18 , 18 , 18 , 18 , 10 , 10 ,&
      & 10 , 10 , 10 , 10 , 33 , 18 , 18 , 18 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 , 00 ,&
      & 00/
   DATA new , debug/2*.FALSE./
!
   k = Lbd + 1
   ifirod = 0
   ioloop = 0
   iloop = 0
   ifirst = 0
   ieoptp = 0
!
!     ALLOCATE GINO BUFFERS
!
   nz = korsz(Core)
   ibuf1 = nz - Ibuf + 1
   ibuf2 = ibuf1 - Ibuf
   nz = nz - 2*Ibuf
   icrq = -nz
   IF ( nz<=0 ) GOTO 2400
   iecase = 0
!
!     TRY TO FIND CASECC ON OPTP - TRY TO ASSUME PROPER POSITION
!
   CALL open(*1200,optp,Core(ibuf1),2)
   iopn = 0
   DO
!
!     FIND CASECC
!
      CALL read(*1500,*1600,optp,Core(1),2,1,iflag)
      IF ( Core(1)==case .AND. Core(2)==cc ) THEN
!
!     CASECC FOUND ON OLD PROB TAPE
!
!     OPEN CASECC AND SKIP CASESS IF PRESENT
!
         CALL open(*1900,Casecc,Core(ibuf2),0)
         EXIT
      ELSEIF ( Core(1)==case .AND. Core(2)==ss ) THEN
         DO
!
!     CASESS FOUND ON OPTP - SKIP TO CASECC
!
            CALL read(*1500,*1600,optp,Core(1),2,1,iflag)
            IF ( Core(1)==case .AND. Core(2)==cc ) THEN
               CALL open(*1900,Casecc,Core(ibuf2),0)
               GOTO 100
            ENDIF
         ENDDO
      ELSE
         IF ( iopn==0 ) CALL rewind(optp)
         iopn = 1
         CALL skpfil(optp,1)
      ENDIF
   ENDDO
 100  DO
      CALL read(*2100,*2200,Casecc,Core(1),2,1,iflag)
      IF ( Core(1)==case .AND. Core(2)==cc ) THEN
         ASSIGN 200 TO ihop
         EXIT
      ENDIF
   ENDDO
 200  DO
      CALL read(*1100,*1600,optp,Icase(1,2),Lencc,0,iflag)
      IF ( Icase(16,2)==0 ) THEN
         IF ( Icase(Lencc,2)/=0 ) THEN
            lsym = Icase(Lencc,2)
            CALL read(*1500,*1600,optp,Core(1),-lsym,0,iflag)
         ENDIF
         CALL read(*1500,*300,optp,Core(1),nz,1,ifoptp)
         icrq = nz
         GOTO 2400
      ELSE
         CALL fwdrec(*1500,optp)
      ENDIF
   ENDDO
 300  DO
      CALL read(*900,*2200,Casecc,Icase(1,1),Lencc,0,iflag)
      IF ( Icase(16,1)==0 ) THEN
         IF ( Icase(Lencc,1)/=0 ) THEN
            lsym = Icase(Lencc,1)
            CALL read(*2100,*2200,Casecc,Core(ifoptp+1),-lsym,0,iflag)
         ENDIF
         CALL read(*2100,*400,Casecc,Core(ifoptp+1),nz-ifoptp,1,ifcase)
         icrq = nz - ifoptp
         GOTO 2400
      ELSE
         CALL fwdrec(*2100,Casecc)
      ENDIF
   ENDDO
!
!     CHECK FOR LOOPING PROBLEM
!
 400  IF ( ifirst==0 ) THEN
      ifirst = 1
      ispc = Icase(3,1)
      impc = Icase(2,1)
      imtd = Icase(5,1)
      ifreq = Icase(14,1)
      itfl = Icase(15,1)
      ik1 = Icase(139,1)
      ik2 = Icase(140,1)
      im1 = Icase(141,1)
      im2 = Icase(142,1)
      ib1 = Icase(143,1)
      ib2 = Icase(144,1)
      IF ( Icase(165,1)<=0 .AND. Icase(164,1)<=0 ) GOTO 500
   ELSEIF ( Icase(3,1)==ispc ) THEN
      IF ( Icase(2,1)==impc ) THEN
         IF ( Icase(5,1)==imtd ) THEN
            IF ( Icase(139,1)==ik1 .AND. Icase(140,1)==ik2 ) THEN
               IF ( Icase(141,1)==im1 .AND. Icase(142,1)==im2 ) THEN
                  IF ( Icase(143,1)==ib1 .AND. Icase(144,1)==ib2 ) THEN
                     IF ( Icase(15,1)==itfl ) THEN
                        IF ( Icase(14,1)==ifreq ) THEN
                           IF ( Icase(138,1)<=0 ) THEN
                              IF ( Icase(38,1)==0 ) GOTO 500
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!
!     SET LOOP$
!
   Bits(k) = orf(Bits(k),Two1(11))
   iloop = 1
!
!     DETERMINE IF OLD PROBLEM WOULD HAVE LOOPED
!
 500  IF ( ifirod==0 ) THEN
      ifirod = 1
      ispc1 = Icase(3,2)
      impc1 = Icase(2,2)
      imtd1 = Icase(5,2)
      ik11 = Icase(139,2)
      ik21 = Icase(140,2)
      im11 = Icase(141,2)
      im21 = Icase(142,2)
      ib11 = Icase(143,2)
      ib21 = Icase(144,2)
      itfl1 = Icase(15,2)
      ifreq1 = Icase(14,2)
      IF ( Icase(164,2)>0 .OR. Icase(165,2)>0 ) ioloop = 1
      GOTO 700
   ENDIF
!
!     SECOND RECORD APPLY LOOP RULES
!
 600  IF ( Icase(3,2)/=ispc1 ) THEN
      ioloop = 1
   ELSEIF ( Icase(2,2)/=impc1 ) THEN
      ioloop = 1
   ELSEIF ( Icase(5,2)/=imtd1 ) THEN
      ioloop = 1
   ELSEIF ( Icase(139,2)/=ik11 .OR. Icase(140,2)/=ik21 ) THEN
      ioloop = 1
   ELSEIF ( Icase(141,2)/=im11 .OR. Icase(142,2)/=im21 ) THEN
      ioloop = 1
   ELSEIF ( Icase(143,2)/=ib11 .OR. Icase(144,2)/=ib21 ) THEN
      ioloop = 1
   ELSEIF ( Icase(138,2)>0 ) THEN
      ioloop = 1
   ELSEIF ( Icase(38,2)/=0 ) THEN
      ioloop = 1
   ELSEIF ( Icase(15,2)/=itfl1 ) THEN
      ioloop = 1
   ELSEIF ( Icase(14,2)/=ifreq1 ) THEN
      ioloop = 1
   ENDIF
 700  IF ( iecase/=1 ) THEN
!
!     CHECK FOR CHANGES -
!
      IF ( ieoptp==1 ) ieoptp = 2
      DO i = 1 , Lencc
         IF ( ibit(i)==0 ) CYCLE
         l = ibit(i)
         IF ( l<=32 .AND. andf(Bits(k),Two1(l))/=0 ) CYCLE
         IF ( iword(i)==0 ) THEN
!
!     CHECK FOR PRESENCE OF PRINT AND PLOT REQUESTS
!
            IF ( i/=135 .AND. Icase(i,1)==0 ) CYCLE
            IF ( i/=135 ) THEN
               IF ( ibit(i)==18 ) Bits(k+1) = orf(Bits(k+1),Two1(3))
               IF ( ibit(i)==10 ) Bits(k+1) = orf(Bits(k+1),Two1(4))
               IF ( ieoptp/=2 ) THEN
                  IF ( Icase(i,1)<0 .AND. Icase(i,2)<0 ) CYCLE
                  IF ( Icase(i,1)>=0 .OR. Icase(i,2)<0 ) THEN
                     IF ( Icase(i,1)<=0 .OR. Icase(i,2)>0 ) THEN
                        ipcase = ifoptp + 1
                        DO WHILE ( ipcase<=ifoptp+ifcase )
                           IF ( Core(ipcase)==Icase(i,1) ) THEN
                              ipoptp = 1
                              DO WHILE ( ipoptp<=ifoptp )
                                 IF ( Core(ipoptp)==Icase(i,2) ) GOTO 740
                                 ipoptp = ipoptp + Core(ipoptp+1) + 2
                              ENDDO
                              GOTO 1700
                           ELSE
                              ipcase = ipcase + Core(ipcase+1) + 2
                           ENDIF
                        ENDDO
                        GOTO 2300
                     ENDIF
                  ENDIF
               ENDIF
            ELSEIF ( Icase(i,1)==0 .AND. Icase(i,2)==0 ) THEN
               CYCLE
            ENDIF
         ELSEIF ( Icase(i,1)==Icase(i,2) ) THEN
            CYCLE
         ENDIF
 720     IF ( l>32 ) THEN
!
!     SECOND CASECC WORD
!
            l = l - 31
            Bits(k+1) = orf(Bits(k+1),Two1(l))
         ELSE
            Bits(k) = orf(Bits(k),Two1(l))
         ENDIF
         CYCLE
 740     iqcase = ifoptp + ifcase + 1
         ix = ipcase
         iy = iqcase
         ASSIGN 760 TO jump
         IF ( debug ) WRITE (Nout,99001)
99001    FORMAT (/,' ------ NPTP PASS ------')
         GOTO 820
 760     iqoptp = iy
         ix = ipoptp
         ASSIGN 780 TO jump
         IF ( debug ) WRITE (Nout,99002)
99002    FORMAT (/,' ------ OPTP PASS ------')
         GOTO 820
 780     leng1 = iqoptp - iqcase
         leng2 = iy - iqoptp
         IF ( debug ) WRITE (Nout,99003) Core(ipcase) , leng1 , leng2 , iy , iqoptp , iqcase
99003    FORMAT (//,' IFP1B/@310  CHECKING SETS',I9,' FROM NPTP AND OPTP',/5X,'LENG1,LENG2, IY,IQOPTP,IQCASE =',2I5,3I7)
         IF ( leng1==leng2 ) THEN
            DO mm = 1 , leng1
               IF ( Core(iqcase+mm-1)/=Core(iqoptp+mm-1) ) GOTO 800
            ENDDO
            IF ( debug ) WRITE (Nout,99004) Core(ipcase)
99004       FORMAT (' ... NO DIFFERENCES IN SET',I8)
            CYCLE
         ENDIF
 800     WRITE (Nout,99005) Uim , Core(ipcase)
99005    FORMAT (A29,', SET',I9,' DEFINITION HAS BEEN CHANGED IN RESTART')
         GOTO 720
!
!     A NEW NON-EXPANDING METHOD IS IMPLEMENTED HERE BY  G.CAHN/UNISYS
!     8/91, IN CASE THE ORIGINAL LOGIC RUNS OUT OF CORE SPACE
!
!     THE NEW METHOD WILL CONCATINATE VARIATIONS OF SET DEFINITION TO
!     THE SIMPLEST FORM.  E.G. THE NEXT 3 LINES SPECIFY THE SAME SET
!     10 THRU 400000  (THIS IS THE SIMPLEST FORM)
!     10, 11, 12 THRU 400008, 400009, 400000
!     10 THRU 20, 21, 22, 23 THRU 200, 201 THRU 500, 501 502 THRU 400000
!
 820     IF ( new ) THEN
!
!     NEW LOGIC WITHOUT THRU RANGE EXPANSION
!
            in = Core(ix+1)
            ix = ix + 2
            m0 = iy
            Core(iy) = Core(ix)
            iy = iy + 1
            IF ( in==1 ) GOTO 860
            Core(iy) = Core(ix+1)
            IF ( Core(iy)==Core(iy-1)+1 ) Core(iy) = -Core(iy)
            iy = iy + 1
            m = 1
            DO
               m = m + 1
               IF ( m>=in ) THEN
                  icrq = iy - nz
                  IF ( iy>nz ) GOTO 2400
                  m1 = iy - 1
                  IF ( debug ) WRITE (Nout,99006) Core(ix-2) , (Core(j),j=m0,m1)
99006             FORMAT (/,' IFP1B/@480    SET',I8,/,(2X,15I8))
                  GOTO 860
               ELSE
                  m1 = Core(ix+m)
                  m2 = iabs(m1)
                  IF ( debug ) WRITE (Nout,99007) m , in , ix , iy , m1 , Core(iy-1)
99007             FORMAT (' @440   M,IN,IX,IY,M1,CORE(IY-1) =',6I8)
                  IF ( m1<0 ) THEN
                     IF ( Core(iy-1)<=0 ) THEN
                        Core(iy-1) = -m2
                        CYCLE
                     ENDIF
                  ELSEIF ( m1==1-Core(iy-1) ) THEN
                     Core(iy-1) = -m2
                     CYCLE
                  ENDIF
                  Core(iy) = m1
                  IF ( m1==Core(iy-1)+1 ) Core(iy) = -m2
                  iy = iy + 1
               ENDIF
            ENDDO
         ELSE
            in = Core(ix+1)
            ix = ix + 2
            m = 0
         ENDIF
 840     DO
            m = m + 1
            IF ( m<in ) THEN
               IF ( Core(ix+m)>0 ) EXIT
               m1 = Core(ix+m-1)
               m2 = -Core(ix+m)
               icrq = iy + m2 - m1 - nz
               IF ( icrq>0 ) THEN
!
!     INSUFFICIENT CORE SPACE, SWITCH TO NEW METHOD
!
                  new = .TRUE.
                  GOTO 740
               ELSE
                  DO mm = m1 , m2
                     Core(iy) = mm
                     iy = iy + 1
                  ENDDO
                  m = m + 1
               ENDIF
            ELSEIF ( m==in ) THEN
               EXIT
            ELSE
               GOTO 860
            ENDIF
         ENDDO
         icrq = iy - nz
         IF ( iy>nz ) GOTO 2400
         Core(iy) = Core(ix+m-1)
         iy = iy + 1
         GOTO 840
!
 860     GOTO jump
!
      ENDDO
      GOTO ihop
   ELSE
      IF ( ioloop==1 ) GOTO 1000
      DO
         CALL read(*1000,*1600,optp,Icase(1,2),Lencc,1,iflag)
         IF ( Icase(16,2)==0 ) GOTO 600
      ENDDO
   ENDIF
!
!     EOF ON CASECC
!
 900  CALL close(Casecc,1)
   IF ( ieoptp==0 ) THEN
      iecase = 1
      GOTO 600
   ENDIF
 1000 CALL close(optp,2)
!
!     SET LOOP1  THIS SHOULD REEXECUTE THE ENTIRE LOOP
!
   IF ( ieoptp/=1 .AND. ioloop/=0 ) Bits(k) = orf(Bits(k),Two1(12))
!
!     CHECK FOR LOOP$ IF NOT ON SET NOLOOP$
!
   IF ( iloop==0 ) Bits(k) = orf(Bits(k),Two1(32))
   RETURN
!
!     EOF ON  OPTP
!
 1100 ASSIGN 300 TO ihop
   ieoptp = 1
   GOTO 300
!
!     ERROR MESSAGES
!
 1200 ip1 = -1
 1300 ip2 = optp
 1400 CALL mesage(ip1,ip2,name)
   RETURN
!
 1500 ip1 = -2
   GOTO 1300
 1600 ip1 = -3
   GOTO 1300
 1700 Core(1) = optp
   Core(2) = Iblank
 1800 WRITE (Nout,99008) Sfm , Core(1) , Core(2)
99008 FORMAT (A25,' 651, LOGIC ERROR IN SUBROUTINE IFP1B WHILE ','PROCESSING SET DATA ON ',2A4,' FILE.')
   ip1 = -37
   GOTO 1400
 1900 ip1 = -1
 2000 ip2 = Casecc
   GOTO 1400
 2100 ip1 = -2
   GOTO 2000
 2200 ip1 = -3
   GOTO 2000
 2300 Core(1) = case
   Core(2) = cc
   GOTO 1800
 2400 ip1 = -8
   ip2 = icrq
   GOTO 1400
END SUBROUTINE ifp1b
