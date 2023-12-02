!*==rcovem.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovem(Noexcl,Nrowe)
   USE c_blank
   USE c_condas
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_parmeg
   USE c_rcovcm
   USE c_rcovcr
   USE c_saddx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
   lcorez = korsz(z)
!
!     FROM THE LAST GROUP ON LAMS CREATE A PARTITIONING VECTOR TO
!     DIFFERENTIATE THE INCLUDED AND EXCLUDED MODES
!
   Nrowe = 0
   item = lams
   CALL sfetch(rss,lams,1,rc)
   IF ( rc/=1 ) THEN
!
!     ERRORS
!
      CALL smsg(rc-2,item,rss)
   ELSE
      n = 2
      CALL sjump(n)
      IF ( n<0 ) THEN
         CALL smsg(7,item,rss)
      ELSE
         i = 0
         SPAG_Loop_1_1: DO
!
            CALL suread(icode,1,n,rc)
            IF ( rc==1 ) THEN
               i = i + 1
               IF ( i>buf1 ) THEN
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
               IF ( qa+pa/=0 ) THEN
                  itinp = rsp
                  itoutp = rsp
                  irp = 1
                  nrp = i
                  incrp = 1
                  CALL makmcb(mcba,scr8,nrp,rect,rsp)
                  CALL gopen(scr8,z(buf1),wrtrew)
                  CALL pack(rz(1),scr8,mcba)
                  CALL close(scr8,rew)
                  CALL wrttrl(mcba)
!
!     PARTITION THE EIGENVECTOR TO GET THE EXCLUDED MODES OUT
!
                  item = phis
                  CALL mtrxi(scr7,rss,phis,0,rc)
                  IF ( rc/=1 ) THEN
                     CALL smsg(rc-2,item,rss)
                  ELSE
                     rule = 0
                     mcbp(1) = scr7
                     CALL rdtrl(mcbp)
                     IF ( mcbp(2)/=nrp ) THEN
                        WRITE (nout,99001) uwm , rss
99001                   FORMAT (A25,' 6372, THE PHIS AND LAMS ITEMS ARE INCONSISTANT FOR',' SUBSTRUCTURE ',2A4)
                     ELSE
                        CALL makmcb(mcbp11,scr6,mcbp(3),rect,mcbp(5))
                        mcbp11(2) = Nrowe
                        mcbp21(1) = 0
                        mcbp12(1) = 0
                        mcbp22(1) = 0
!
!     SETUP NULL COLUMN PARTITONING VECTOR
!
                        CALL makmcb(mcbb,0,mcbp(3),rect,rsp)
                        mcbb(2) = 1
                        mrgz = lcorez
                        CALL sofcls
!
                        CALL partn(mcba,mcbb,z(1))
!
                        CALL wrttrl(mcbp11)
!
!     IF BOTH LOADS AND SINGLE POINT CONSTRAINT FORCES EXIST, ADD
!     THEM TOGETHER
!
                        irh = qa + pa
                        IF ( qa/=0 .AND. pa/=0 ) THEN
                           nomat = 2
                           typa = 1
                           alpha = 1.0
                           mcbaa(1) = qa
                           CALL rdtrl(mcbaa)
                           typb = 1
                           beta = 1.0
                           mcbbb(1) = pa
                           CALL rdtrl(mcbbb)
                           CALL makmcb(mcbxx,scr7,mcbaa(3),rect,mcbaa(5))
                           mcbxx(2) = mcbaa(2)
!
                           CALL sadd(z(1),z(1))
!
                           CALL wrttrl(mcbxx)
                           irh = scr7
                        ENDIF
!
!     MULTIPLY   PK = QK(T)*(PA + QA)
!
                        DO i = 1 , 7
                           mcba(i) = mcbp11(i)
                        ENDDO
                        mcbb(i) = irh
                        CALL rdtrl(mcbb)
                        mcbc(1) = 0
                        mpyz = lcorez
                        tflag = 1
                        signab = 1
                        signc = 1
                        mprec = 0
                        mscr = scr8
                        CALL makmcb(mcbd,scr5,Nrowe,rect,mcba(5))
!
                        CALL mpyad(z(1),z(1),z(1))
!
!     READ MODAL MASS AND TWOPHI*FREQUENCY FOR EACH OF THE EXCLUDED
!     MODES MODES FROM LAMS
!     IF MODE WAS EXCLUDED BECAUSE OF NON-PARTICIPATION, SET ITS
!     FREQUENCY TO ZERO
!
                        CALL sofopn(z(sof1),z(sof2),z(sof3))
                        item = lams
                        CALL sfetch(rss,lams,1,rc)
                        IF ( rc/=1 ) THEN
                           CALL smsg(rc-2,item,rss)
                        ELSE
                           n = 1
                           CALL sjump(n)
                           IF ( n<=0 ) THEN
                              CALL smsg(7,item,rss)
                           ELSE
                              imode = 8
                              CALL suread(z(imode),-1,n,rc)
                              IF ( rc==2 ) THEN
                                 nmode = imode + n - 1
                                 IF ( nmode>buf3 ) THEN
                                    CALL mesage(8,0,name)
                                    EXIT SPAG_Loop_1_1
                                 ELSE
                                    icode = nmode + 1
                                    CALL suread(z(icode),-1,n,rc)
                                    IF ( rc==2 .OR. rc==3 ) THEN
                                       ncode = icode + n - 1
                                       IF ( ncode>buf3 ) THEN
                                         CALL mesage(8,0,name)
                                         EXIT SPAG_Loop_1_1
                                       ELSE
!
                                         i1 = imode - 7
                                         i2 = imode - 2
                                         DO i = icode , ncode
                                         i1 = i1 + 7
                                         IF ( z(i)/=1 ) THEN
                                         i2 = i2 + 2
                                         rz(i2) = rz(i1+3)
                                         IF ( z(i)==2 .OR. rz(i2)<=0.001 ) rz(i2) = 0.0
                                         rz(i2+1) = rz(i1+5)
                                         ENDIF
                                         ENDDO
                                         nmode = i2 + 1
!
!     POSITION SOLN ITEM TO SOLUTION DATA
!
                                         item = soln
                                         CALL sfetch(rss,soln,1,rc)
                                         IF ( rc/=1 ) THEN
                                         CALL smsg(rc-2,item,rss)
                                         EXIT SPAG_Loop_1_1
                                         ELSE
                                         n = 1
                                         CALL sjump(n)
                                         IF ( n<0 ) THEN
                                         CALL smsg(7,item,rss)
                                         EXIT SPAG_Loop_1_1
                                         ELSE
!
!     SET UP TO LOOP OVER COLUMNS
!
                                         ncol = mcbd(2)
                                         nword = 1
                                         IF ( mcbd(5)>=3 ) nword = 2
                                         ivec1 = (nmode/2)*2 + 3
                                         icvec1 = ivec1/2 + 1
                                         ivec2 = ivec1 + (Nrowe*nword/2)*2 + 1
                                         IF ( ivec2+Nrowe>buf3 ) THEN
                                         CALL mesage(8,0,name)
                                         EXIT SPAG_Loop_1_1
                                         ELSE
!
                                         CALL gopen(scr5,z(buf1),rdrew)
                                         CALL gopen(scr3,z(buf2),wrtrew)
                                         CALL gopen(scr4,z(buf3),wrtrew)
                                         CALL makmcb(mcba,scr3,Nrowe,rect,rsp)
                                         CALL makmcb(mcbb,scr4,Nrowe,rect,rsp)
!
                                         itinu = rsp
                                         IF ( mcbd(5)>=3 ) itinu = csp
                                         iru = 1
                                         nru = Nrowe
                                         incru = 1
                                         nrp = Nrowe
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
                                         IF ( rfno>3 ) THEN
!
                                         CALL suread(rz(1),1,n,rc)
                                         IF ( rc/=1 ) GOTO 4
                                         sc = twophi*rz(1)*(0.0,1.0)
                                         sc2 = sc*sc
                                         ELSE
                                         CALL suread(rz(1),7,n,rc)
                                         IF ( rc/=1 ) GOTO 4
                                         IF ( mcbd(5)>=3 ) THEN
!
                                         sc = cz(2)
                                         sc2 = sc*sc
                                         ELSE
                                         freq = rz(5)
                                         s2 = -(twophi*freq)**2
                                         ENDIF
                                         ENDIF
!
!     UNPACK THE NEXT COLUMN
!
                                         CALL unpack(*2,scr5,rz(ivec1))
!
                                         IF ( mcbd(5)>=3 ) THEN
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
                                         IF ( rz(im)==0.0 .OR. (twophi*freq)>rz(im) ) THEN
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
                                         CALL pack(rz(ivec1),scr3,mcba)
                                         CALL pack(rz(ivec2),scr4,mcbb)
                                         EXIT SPAG_DispatchLoop_1
                                         END SELECT
                                         ENDDO SPAG_DispatchLoop_1
!
                                         ENDDO
!
                                         CALL close(scr5,rew)
                                         CALL close(scr3,rew)
                                         CALL close(scr4,rew)
                                         CALL wrttrl(mcba)
                                         CALL wrttrl(mcbb)
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
 4                            CALL smsg(rc+4,item,rss)
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
   WRITE (nout,99002) uwm , rss
!
!     FORMAT STATEMENTS
!
99002 FORMAT (A25,' 6371, CALCULATIONS FOR EXCLUDED MODE ENERGIES FOR',' SUBSTRUCTURE ',2A4,' ABORTED.')
   Noexcl = .TRUE.
   CALL close(scr3,rew)
   CALL close(scr4,rew)
   CALL close(scr5,rew)
END SUBROUTINE rcovem
