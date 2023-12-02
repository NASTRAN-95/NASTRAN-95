!*==rcovem.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovem(Noexcl,Nrowe)
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
   USE C_MPYADX
   USE C_NAMES
   USE C_PACKX
   USE C_PARMEG
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_SADDX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL :: Noexcl
   INTEGER :: Nrowe
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(2) :: cz
   REAL :: dkd , dks , freq , s2 , wk2
   COMPLEX :: dkdc , dksc , sc , sc2
   INTEGER :: i , i1 , i2 , icode , icol , icvec1 , im , imode , irh , item , ivec1 , ivec2 , j , n , ncode , ncol , nmode , nword ,&
            & rc
   INTEGER , SAVE :: lams , phis , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , soln
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(5) :: rz
   EXTERNAL close , gopen , korsz , makmcb , mesage , mpyad , mtrxi , pack , partn , rdtrl , sadd , sfetch , sjump , smsg , sofcls ,&
          & sofopn , suread , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE CALCULATES THE ENERGIES ON THE MODAL COORDINATES
!     THAT WERE EXCLUDED FROM THE MODAL REDUCTION PROCESSING
!
   !>>>>EQUIVALENCE (Z(1),Rz(1),Cz(1)) , (Cz(2),Sc)
   DATA lams , phis , soln/4HLAMS , 4HPHIS , 4HSOLN/
   DATA scr3 , scr4 , scr5 , scr6 , scr7 , scr8/303 , 304 , 305 , 306 , 307 , 308/
   DATA name/4HRCOV , 4HEM  /
!
!     INITILIZE
!
   Lcorez = korsz(Z)
!
!     FROM THE LAST GROUP ON LAMS CREATE A PARTITIONING VECTOR TO
!     DIFFERENTIATE THE INCLUDED AND EXCLUDED MODES
!
   Nrowe = 0
   item = lams
   CALL sfetch(Rss,lams,1,rc)
   IF ( rc/=1 ) THEN
!
!     ERRORS
!
      CALL smsg(rc-2,item,Rss)
   ELSE
      n = 2
      CALL sjump(n)
      IF ( n<0 ) THEN
         CALL smsg(7,item,Rss)
      ELSE
         i = 0
         SPAG_Loop_1_1: DO
!
            CALL suread(icode,1,n,rc)
            IF ( rc==1 ) THEN
               i = i + 1
               IF ( i>Buf1 ) THEN
                  CALL mesage(8,0,name)
                  EXIT SPAG_Loop_1_1
               ELSE
                  rz(i) = 1.0
                  IF ( icode/=1 ) THEN
                     rz(i) = 0.0
                     Nrowe = Nrowe + 1
                  ENDIF
               ENDIF
!
            ELSEIF ( Nrowe==0 ) THEN
!
!     NO EXECLUDED MODES EXIST
!
               Noexcl = .TRUE.
               RETURN
            ELSE
               IF ( Qa+Pa/=0 ) THEN
                  Itinp = Rsp
                  Itoutp = Rsp
                  Irp = 1
                  Nrp = i
                  Incrp = 1
                  CALL makmcb(Mcba,scr8,Nrp,Rect,Rsp)
                  CALL gopen(scr8,Z(Buf1),Wrtrew)
                  CALL pack(rz(1),scr8,Mcba)
                  CALL close(scr8,Rew)
                  CALL wrttrl(Mcba)
!
!     PARTITION THE EIGENVECTOR TO GET THE EXCLUDED MODES OUT
!
                  item = phis
                  CALL mtrxi(scr7,Rss,phis,0,rc)
                  IF ( rc/=1 ) THEN
                     CALL smsg(rc-2,item,Rss)
                  ELSE
                     Rule = 0
                     Mcbp(1) = scr7
                     CALL rdtrl(Mcbp)
                     IF ( Mcbp(2)/=Nrp ) THEN
                        WRITE (Nout,99001) Uwm , Rss
99001                   FORMAT (A25,' 6372, THE PHIS AND LAMS ITEMS ARE INCONSISTANT FOR',' SUBSTRUCTURE ',2A4)
                     ELSE
                        CALL makmcb(Mcbp11,scr6,Mcbp(3),Rect,Mcbp(5))
                        Mcbp11(2) = Nrowe
                        Mcbp21(1) = 0
                        Mcbp12(1) = 0
                        Mcbp22(1) = 0
!
!     SETUP NULL COLUMN PARTITONING VECTOR
!
                        CALL makmcb(Mcbb,0,Mcbp(3),Rect,Rsp)
                        Mcbb(2) = 1
                        Mrgz = Lcorez
                        CALL sofcls
!
                        CALL partn(Mcba,Mcbb,Z(1))
!
                        CALL wrttrl(Mcbp11)
!
!     IF BOTH LOADS AND SINGLE POINT CONSTRAINT FORCES EXIST, ADD
!     THEM TOGETHER
!
                        irh = Qa + Pa
                        IF ( Qa/=0 .AND. Pa/=0 ) THEN
                           Nomat = 2
                           Typa = 1
                           Alpha = 1.0
                           Mcbaa(1) = Qa
                           CALL rdtrl(Mcbaa)
                           Typb = 1
                           Beta = 1.0
                           Mcbbb(1) = Pa
                           CALL rdtrl(Mcbbb)
                           CALL makmcb(Mcbxx,scr7,Mcbaa(3),Rect,Mcbaa(5))
                           Mcbxx(2) = Mcbaa(2)
!
                           CALL sadd(Z(1),Z(1))
!
                           CALL wrttrl(Mcbxx)
                           irh = scr7
                        ENDIF
!
!     MULTIPLY   PK = QK(T)*(PA + QA)
!
                        DO i = 1 , 7
                           Mcba(i) = Mcbp11(i)
                        ENDDO
                        Mcbb(i) = irh
                        CALL rdtrl(Mcbb)
                        Mcbc(1) = 0
                        Mpyz = Lcorez
                        Tflag = 1
                        Signab = 1
                        Signc = 1
                        Mprec = 0
                        Mscr = scr8
                        CALL makmcb(Mcbd,scr5,Nrowe,Rect,Mcba(5))
!
                        CALL mpyad(Z(1),Z(1),Z(1))
!
!     READ MODAL MASS AND TWOPHI*FREQUENCY FOR EACH OF THE EXCLUDED
!     MODES MODES FROM LAMS
!     IF MODE WAS EXCLUDED BECAUSE OF NON-PARTICIPATION, SET ITS
!     FREQUENCY TO ZERO
!
                        CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
                        item = lams
                        CALL sfetch(Rss,lams,1,rc)
                        IF ( rc/=1 ) THEN
                           CALL smsg(rc-2,item,Rss)
                        ELSE
                           n = 1
                           CALL sjump(n)
                           IF ( n<=0 ) THEN
                              CALL smsg(7,item,Rss)
                           ELSE
                              imode = 8
                              CALL suread(Z(imode),-1,n,rc)
                              IF ( rc==2 ) THEN
                                 nmode = imode + n - 1
                                 IF ( nmode>Buf3 ) THEN
                                    CALL mesage(8,0,name)
                                    EXIT SPAG_Loop_1_1
                                 ELSE
                                    icode = nmode + 1
                                    CALL suread(Z(icode),-1,n,rc)
                                    IF ( rc==2 .OR. rc==3 ) THEN
                                       ncode = icode + n - 1
                                       IF ( ncode>Buf3 ) THEN
                                         CALL mesage(8,0,name)
                                         EXIT SPAG_Loop_1_1
                                       ELSE
!
                                         i1 = imode - 7
                                         i2 = imode - 2
                                         DO i = icode , ncode
                                         i1 = i1 + 7
                                         IF ( Z(i)/=1 ) THEN
                                         i2 = i2 + 2
                                         rz(i2) = rz(i1+3)
                                         IF ( Z(i)==2 .OR. rz(i2)<=0.001 ) rz(i2) = 0.0
                                         rz(i2+1) = rz(i1+5)
                                         ENDIF
                                         ENDDO
                                         nmode = i2 + 1
!
!     POSITION SOLN ITEM TO SOLUTION DATA
!
                                         item = soln
                                         CALL sfetch(Rss,soln,1,rc)
                                         IF ( rc/=1 ) THEN
                                         CALL smsg(rc-2,item,Rss)
                                         EXIT SPAG_Loop_1_1
                                         ELSE
                                         n = 1
                                         CALL sjump(n)
                                         IF ( n<0 ) THEN
                                         CALL smsg(7,item,Rss)
                                         EXIT SPAG_Loop_1_1
                                         ELSE
!
!     SET UP TO LOOP OVER COLUMNS
!
                                         ncol = Mcbd(2)
                                         nword = 1
                                         IF ( Mcbd(5)>=3 ) nword = 2
                                         ivec1 = (nmode/2)*2 + 3
                                         icvec1 = ivec1/2 + 1
                                         ivec2 = ivec1 + (Nrowe*nword/2)*2 + 1
                                         IF ( ivec2+Nrowe>Buf3 ) THEN
                                         CALL mesage(8,0,name)
                                         EXIT SPAG_Loop_1_1
                                         ELSE
!
                                         CALL gopen(scr5,Z(Buf1),Rdrew)
                                         CALL gopen(scr3,Z(Buf2),Wrtrew)
                                         CALL gopen(scr4,Z(Buf3),Wrtrew)
                                         CALL makmcb(Mcba,scr3,Nrowe,Rect,Rsp)
                                         CALL makmcb(Mcbb,scr4,Nrowe,Rect,Rsp)
!
                                         Itinu = Rsp
                                         IF ( Mcbd(5)>=3 ) Itinu = Csp
                                         Iru = 1
                                         Nru = Nrowe
                                         Incru = 1
                                         Nrp = Nrowe
!
!     LOOP OVER EACH SOLUTION STEP
!
                                         DO icol = 1 , ncol
                                         spag_nextblock_1 = 1
                                         SPAG_DispatchLoop_1: DO
                                         SELECT CASE (spag_nextblock_1)
                                         CASE (1)
!
!     GET FREQUENCY OR POLE FROM SOLN ITEM FOR THIS STEP
!
                                         IF ( Rfno>3 ) THEN
!
                                         CALL suread(rz(1),1,n,rc)
                                         IF ( rc/=1 ) GOTO 4
                                         sc = Twophi*rz(1)*(0.0,1.0)
                                         sc2 = sc*sc
                                         ELSE
                                         CALL suread(rz(1),7,n,rc)
                                         IF ( rc/=1 ) GOTO 4
                                         IF ( Mcbd(5)>=3 ) THEN
!
                                         sc = cz(2)
                                         sc2 = sc*sc
                                         ELSE
                                         freq = rz(5)
                                         s2 = -(Twophi*freq)**2
                                         ENDIF
                                         ENDIF
!
!     UNPACK THE NEXT COLUMN
!
                                         CALL unpack(*2,scr5,rz(ivec1))
!
                                         IF ( Mcbd(5)>=3 ) THEN
!
!     CALCULATE ENERGIES FOR COMPLEX VECTORS
!
                                         im = imode - 2
                                         DO i = 1 , Nrowe
                                         im = im + 2
                                         j = i - 1
                                         IF ( rz(im)==0.0 .OR. aimag(sc)>rz(im) ) THEN
!
                                         rz(ivec1+j) = 0.0
                                         rz(ivec2+j) = 0.0
                                         ELSE
                                         wk2 = rz(im)**2
!
                                         dkdc = -sc2*cz(icvec1+j)/(rz(im+1)*wk2**2*(1.0+sc2/wk2))
                                         dksc = cz(icvec1+j)/(rz(im+1)*wk2)
!
                                         rz(ivec2+j) = .5*rz(im+1)*wk2*cabs((2.0*dksc+dkdc)*dkdc)
                                         rz(ivec1+j) = cabs(sc**2/wk2)*rz(ivec2+j)
                                         ENDIF
!
                                         ENDDO
                                         ELSE
!
!     CALCULATE ENERGIES FOR REAL MATRICIES
!
                                         im = imode - 2
                                         DO i = 1 , Nrowe
                                         im = im + 2
                                         j = i - 1
                                         IF ( rz(im)==0.0 .OR. (Twophi*freq)>rz(im) ) THEN
!
                                         rz(ivec1+j) = 0.0
                                         rz(ivec2+j) = 0.0
                                         ELSE
                                         wk2 = rz(im)**2
!
                                         dkd = -s2*rz(ivec1+j)/(rz(im+1)*wk2**2*(1.0+s2/wk2))
                                         dks = rz(ivec1+j)/(rz(im+1)*wk2)
!
                                         rz(ivec2+j) = .5*rz(im+1)*wk2*abs((2.0*dks+dkd)*dkd)
                                         rz(ivec1+j) = abs(s2/wk2)*rz(ivec2+j)
                                         ENDIF
!
                                         ENDDO
                                         ENDIF
                                         spag_nextblock_1 = 2
                                         CYCLE SPAG_DispatchLoop_1
 2                                       DO i = 1 , Nrowe
                                         j = i - 1
                                         rz(ivec1+j) = 0.0
                                         rz(ivec2+j) = 0.0
                                         ENDDO
                                         spag_nextblock_1 = 2
                                         CASE (2)
!
!     PACK OUT THE KENETIC AND POTENTIAL ENERGIES
!
                                         CALL pack(rz(ivec1),scr3,Mcba)
                                         CALL pack(rz(ivec2),scr4,Mcbb)
                                         EXIT SPAG_DispatchLoop_1
                                         END SELECT
                                         ENDDO SPAG_DispatchLoop_1
!
                                         ENDDO
!
                                         CALL close(scr5,Rew)
                                         CALL close(scr3,Rew)
                                         CALL close(scr4,Rew)
                                         CALL wrttrl(Mcba)
                                         CALL wrttrl(Mcbb)
!
!     NORMAL RETURN
!
                                         RETURN
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
 4                            CALL smsg(rc+4,item,Rss)
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
      ENDIF
   ENDIF
   WRITE (Nout,99002) Uwm , Rss
!
!     FORMAT STATEMENTS
!
99002 FORMAT (A25,' 6371, CALCULATIONS FOR EXCLUDED MODE ENERGIES FOR',' SUBSTRUCTURE ',2A4,' ABORTED.')
   Noexcl = .TRUE.
   CALL close(scr3,Rew)
   CALL close(scr4,Rew)
   CALL close(scr5,Rew)
END SUBROUTINE rcovem
