!*==rcove.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcove
   USE c_blank
   USE c_names
   USE c_output
   USE c_rcovcm
   USE c_rcovcr
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , casess , cmask , eqss , ib , lams , mmask , scr3 , scr4 , scr6 , scr7 , soln
   INTEGER , DIMENSION(1) :: buf
   INTEGER , DIMENSION(2) , SAVE :: casecc , name , temode , timode , trigid
   LOGICAL :: credu , mredu , noexcl
   INTEGER :: file , grid , i , icode , icol , idit , iexc , iinc , ikflag , imdi , imode , ip , ipflag , iset , isets , isil ,     &
            & item , ivec1 , ivec2 , ivec3 , ivec4 , lcc , lseq , lset , lskip , mode , n , ncol , next , nmode , nmodei , nrigid , &
            & nrowe , nrowi , rc
   REAL :: freq , keng , peng , perk , perkt , perp , perpt , tkeng , tpeng
   INTEGER , DIMENSION(2) :: higher , names , type
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(3) :: z
   EXTERNAL andf , close , fdsub , fmdi , fndnxl , fread , fwdrec , gopen , korsz , mesage , page1 , rcovem , rcovim , rdtrl ,      &
          & read , setfnd , sfetch , sjump , smsg , sofcls , sofopn , suread , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS SUBROUTINE PRINTS THE ENERGIES ON THE MODAL COORDINATES
!     IN A SUBSTRUCTURE THAT WAS MODAL REDUCED.  IT WILL ALSO PRINT
!     THE ENERGIES ON THOSE MODES EXCLUDED FROM THE REDUCTION
!     PROCESSING.
!
   !>>>>EQUIVALENCE (Buf(1),Rz(1))
   !>>>>EQUIVALENCE (Rz(1),Z(1))
   DATA casecc/4HCASE , 4HCC  /
   DATA eqss , lams , soln/4HEQSS , 4HLAMS , 4HSOLN/
   DATA casess , scr3 , scr4 , scr6 , scr7/101 , 303 , 304 , 306 , 307/
   DATA trigid/4HINER , 4HTIAL/
   DATA timode/4HIN-M , 4HODE /
   DATA temode/4HEX-M , 4HODE /
   DATA ib/1/
   DATA mmask/201326592/
   DATA cmask/67108864/
   DATA blank/4H    /
   DATA name/4HRCOV , 4HE   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     IF THIS IS A STATICS SOLUTION NO ENERGY CALCULATIONS CAN BE MADE
!
         IF ( rfno<=2 ) RETURN
!
!     INITIALIZE
!
         sof1 = korsz(z) - sysbuf + 1
         sof2 = sof1 - sysbuf - 1
         sof3 = sof2 - sysbuf
         buf1 = sof3 - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         lcore = buf4 - 1
         IF ( lcore<=0 ) THEN
            n = 8
            CALL mesage(n,file,name)
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     GET THE NAME OF THE HIGHER LEVEL SUBSTRUCTURE.  IF NONE EXISTS
!     THEN RETURN.
!
            CALL sofopn(z(sof1),z(sof2),z(sof3))
            names(1) = rss(1)
            names(2) = rss(2)
            CALL fndnxl(rss,higher)
            rc = 4
            IF ( higher(1)==blank ) THEN
!
!     ERRORS
!
               CALL smsg(rc-2,item,names)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( higher(1)==rss(1) .AND. higher(2)==rss(2) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     CHECK IF THE HIGHER LEVEL SUBSTRUCTURE WAS MODAL REDUCED.
!     IF NOT THEN WE HAVE NOTHING TO DO
!
               names(1) = higher(1)
               names(2) = higher(2)
               rc = 4
               CALL fdsub(higher,idit)
               IF ( idit<0 ) THEN
                  CALL smsg(rc-2,item,names)
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  CALL fmdi(idit,imdi)
                  mredu = .FALSE.
                  credu = .FALSE.
                  IF ( andf(buf(imdi+ib),mmask)/=0 ) mredu = .TRUE.
                  IF ( andf(buf(imdi+ib),cmask)/=0 ) credu = .TRUE.
                  IF ( .NOT.mredu ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
!     READ THE MODAL GROUP OF THE EQSS TO DETERMINE IF THERE ARE ANY
!     RIGID BODY DOF PRESENT.  ALSO GET THE SIL NUMBER OF THE FIRST
!     MODAL CORDINATE.
!
                  item = eqss
                  CALL sfetch(higher,eqss,1,rc)
                  IF ( rc/=1 ) THEN
                     CALL smsg(rc-2,item,names)
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     CALL suread(z(1),3,n,rc)
                     IF ( rc/=1 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     n = z(3)
                     CALL sjump(n)
                     IF ( n<0 ) THEN
                        CALL smsg(7,item,names)
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
!
                        nrigid = 0
                        DO
                           CALL suread(z(1),3,n,rc)
                           IF ( rc/=1 ) THEN
                              spag_nextblock_1 = 4
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( nrigid==0 ) ip = z(2)
                           IF ( z(1)<100 ) THEN
                              nrigid = nrigid + 1
!
                           ELSEIF ( 2*ip>sof3 ) THEN
                              n = 8
                              CALL mesage(n,file,name)
                              spag_nextblock_1 = 5
                              CYCLE SPAG_DispatchLoop_1
                           ELSE
                              n = 1
                              CALL sjump(n)
                              IF ( n<0 ) THEN
                                 CALL smsg(7,item,names)
                                 spag_nextblock_1 = 5
                                 CYCLE SPAG_DispatchLoop_1
                              ELSE
!
                                 CALL suread(z(1),2*ip,n,rc)
                                 IF ( rc/=1 ) THEN
                                    spag_nextblock_1 = 4
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 i = 2*(ip-1) + 1
                                 isil = z(i)
!
!     CALCULATE THE ENERGIES ON THE EXCLUDED MODES
!
                                 noexcl = .TRUE.
                                 nrowe = 0
                                 IF ( .NOT.(credu .OR. rfno<3 .OR. rfno>8) ) THEN
                                    noexcl = .FALSE.
                                    CALL rcovem(noexcl,nrowe)
                                 ENDIF
!
!     CALCULATE THE ENERGIES ON THE INCLUDED MODE AND THE TOTAL
!     ENERGIES ON EACH VECTOR
!
                                 CALL rcovim(higher)
                                 IF ( iopt<0 ) THEN
                                    spag_nextblock_1 = 5
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 mcb(1) = scr6
                                 CALL rdtrl(mcb)
                                 ncol = mcb(2)
                                 nrowi = mcb(3)
                                 nmodei = nrowi - isil + 1
!
!     READ THE MODE DATA FROM LAMS AND SAVE THE MODE NUMBER AND
!     THE FREQUENCY FOR EACH MODE.
!
                                 names(1) = rss(1)
                                 names(2) = rss(2)
                                 item = lams
                                 CALL sfetch(rss,lams,1,rc)
                                 IF ( rc/=1 ) THEN
                                    CALL smsg(rc-2,item,names)
                                    spag_nextblock_1 = 5
                                    CYCLE SPAG_DispatchLoop_1
                                 ELSE
                                    n = 1
                                    CALL sjump(n)
                                    IF ( n<0 ) THEN
                                       CALL smsg(7,item,names)
                                       spag_nextblock_1 = 5
                                       CYCLE SPAG_DispatchLoop_1
                                    ELSE
                                       imode = 1
                                       IF ( nrigid/=0 ) THEN
                                         n = 3*nrigid
                                         imode = imode + n
                                         DO i = 1 , n , 3
                                         z(i) = 0
                                         z(i+1) = 0
                                         z(i+2) = (i-1)/3 + 1
                                         ENDDO
                                       ENDIF
                                       nmode = 3*(nmodei-1+nrowe)
                                       IF ( nmode>lcore ) THEN
                                         n = 8
                                         CALL mesage(n,file,name)
                                         spag_nextblock_1 = 5
                                         CYCLE SPAG_DispatchLoop_1
                                       ELSE
!
                                         DO i = imode , nmode , 3
                                         CALL suread(z(i),7,n,rc)
                                         IF ( rc/=1 ) THEN
                                         spag_nextblock_1 = 4
                                         CYCLE SPAG_DispatchLoop_1
                                         ENDIF
                                         z(i+1) = z(i+4)
                                         z(i+2) = 0
                                         ENDDO
!
!     READ THE LAST GROUP OF LAMS AND GENERATE GRID NUMBERS FOR THE
!     INCLUDED MODES.
!
                                         n = 1
                                         CALL sjump(n)
                                         IF ( n<0 ) THEN
                                         CALL smsg(7,item,names)
                                         spag_nextblock_1 = 5
                                         CYCLE SPAG_DispatchLoop_1
                                         ELSE
                                         iinc = 100
                                         DO i = imode , nmode , 3
                                         CALL suread(icode,1,n,rc)
                                         IF ( rc/=1 ) THEN
                                         spag_nextblock_1 = 4
                                         CYCLE SPAG_DispatchLoop_1
                                         ENDIF
                                         IF ( icode<=1 ) THEN
                                         iinc = iinc + 1
                                         z(i+2) = iinc
                                         ENDIF
                                         ENDDO
!
!     POSITION THE SOLN ITEM TO THE FREQUENCY OR TIME DATA
!
                                         item = soln
                                         CALL sfetch(rss,soln,1,rc)
                                         IF ( rc/=1 ) THEN
                                         CALL smsg(rc-2,item,names)
                                         spag_nextblock_1 = 5
                                         CYCLE SPAG_DispatchLoop_1
                                         ELSE
                                         n = 1
                                         CALL sjump(n)
                                         IF ( n<0 ) THEN
                                         CALL smsg(7,item,names)
                                         spag_nextblock_1 = 5
                                         CYCLE SPAG_DispatchLoop_1
                                         ELSE
!
!     ALLOCATE INCORE ARRAYS FOR THE ENERGY VECTORS
!
                                         ivec1 = nmode + 1
                                         ivec2 = ivec1 + nmodei
                                         ivec3 = ivec2 + nmodei
                                         ivec4 = ivec3 + nrowe
                                         isets = ivec4 + nrowe
                                         IF ( isets>lcore ) THEN
                                         n = 8
                                         CALL mesage(n,file,name)
                                         spag_nextblock_1 = 5
                                         CYCLE SPAG_DispatchLoop_1
                                         ELSE
!
!     READ CASESS AND GET THE TITLE AND ANY SET INFORMATION
!
                                         file = casess
                                         CALL gopen(casess,z(buf1),rdrew)
                                         DO
                                         CALL fread(casess,z(ivec1),2,1)
                                         IF ( z(ivec1)==casecc(1) .AND. z(ivec1+1)==casecc(2) ) THEN
!
                                         CALL fread(casess,0,-38,0)
                                         CALL fread(casess,ititle(1),96,0)
!
                                         IF ( energy<=0 ) THEN
                                         spag_nextblock_1 = 2
                                         CYCLE SPAG_DispatchLoop_1
                                         ENDIF
                                         CALL fread(casess,0,-31,0)
                                         CALL fread(casess,lcc,1,0)
                                         lskip = 167 - lcc
                                         CALL fread(casess,0,lskip,0)
                                         CALL read(*40,*20,casess,lseq,1,0,i)
                                         IF ( lseq>0 ) CALL fread(casess,0,lseq,0)
                                         DO
!
                                         CALL read(*40,*20,casess,iset,1,0,i)
                                         CALL fread(casess,lset,1,0)
                                         IF ( iset/=energy ) THEN
                                         CALL fread(casess,0,-lset,0)
                                         ELSEIF ( isets+lset>lcore ) THEN
                                         n = 8
                                         CALL mesage(n,file,name)
                                         spag_nextblock_1 = 5
                                         CYCLE SPAG_DispatchLoop_1
                                         ELSE
                                         CALL fread(casess,z(isets),lset,0)
                                         spag_nextblock_1 = 2
                                         CYCLE SPAG_DispatchLoop_1
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
 20      WRITE (nout,99001) uwm , energy
99001    FORMAT (A25,' 6365, REQUESTED OUTPUT SET ID',I6,' IS NOT ','DECLARED IN CASE CONTROL. ALL OUTPUT WILL BE PRODUCED')
         energy = -1
         spag_nextblock_1 = 2
      CASE (2)
!
         CALL close(casess,rew)
!
!     LOOP OVER EACH COLUMN AND PRINT THE KINETIC AND POTENTIAL
!     ENERGIES FOR EACH MODAL COORDINATE IF REQUESTED
!
         next = 1
         CALL gopen(scr6,z(buf1),rdrew)
         CALL gopen(scr7,z(buf2),rdrew)
         IF ( .NOT.(noexcl) ) THEN
            CALL gopen(scr3,z(buf3),rdrew)
            CALL gopen(scr4,z(buf4),rdrew)
         ENDIF
!
         itinu = rsp
         incru = 1
!
         DO icol = 1 , ncol
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
!     SET FLAGS FOR NULL COLUMNS
!
                  ikflag = 0
                  ipflag = 0
!
!     GET THE FREQUENCY OR TIME FOR THIS VECTOR
!
                  IF ( rfno>3 ) THEN
!
!     DYNAMICS SOLUTION
!
                     CALL suread(step,1,n,rc)
                     IF ( rc/=1 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ELSE
!
!     NORMAL MODES SOLUTION
!
                     CALL suread(z(ivec1),7,n,rc)
                     IF ( rc/=1 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     step = rz(ivec1+4)
                  ENDIF
!
!     SEE IF THIS COLUMN IS REQUESTED
!
                  IF ( energy>0 ) CALL setfnd(*30,z(isets),lset,icol,next)
!
                  IF ( step<range(1) .OR. step>range(2) ) GOTO 30
!
!     UNPACK THE KINETIC AND POTENTIAL ENERGIES ON INCLUDED MODES
!
                  iru = isil
                  nru = nrowi
                  CALL unpack(*22,scr6,rz(ivec1))
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 22               DO i = 1 , nmodei
                     rz(ivec1+i-1) = 0.0
                  ENDDO
                  rz(ivec1+nmodei-1) = 1.0
                  ikflag = 1
                  spag_nextblock_2 = 2
               CASE (2)
!
                  CALL unpack(*24,scr7,rz(ivec2))
                  spag_nextblock_2 = 3
                  CYCLE SPAG_DispatchLoop_2
 24               DO i = 1 , nmodei
                     rz(ivec2+i-1) = 0.0
                  ENDDO
                  rz(ivec2+nmodei-1) = 1.0
                  ipflag = 1
                  spag_nextblock_2 = 3
               CASE (3)
!
!     UNPACK THE KINETIC AND POTENTIAL ENERGIES ON EXLUDED MODES
!
                  IF ( .NOT.(noexcl) ) THEN
                     iru = 1
                     nru = nrowe
                     CALL unpack(*26,scr3,rz(ivec3))
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
 26               DO i = 1 , nrowe
                     rz(ivec3+i-1) = 0.0
                  ENDDO
                  spag_nextblock_2 = 4
               CASE (4)
!
                  IF ( .NOT.(noexcl) ) THEN
                     CALL unpack(*28,scr4,rz(ivec4))
                     spag_nextblock_2 = 5
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
 28               DO i = 1 , nrowe
                     rz(ivec4+i-1) = 0.0
                  ENDDO
                  spag_nextblock_2 = 5
               CASE (5)
!
!     INITILIZE FOR THE OUTPUT
!
                  nlines = nlpp
!
!     GET TOTAL ENERGIES
!
                  tkeng = rz(ivec1+nmodei-1)
                  tpeng = rz(ivec2+nmodei-1)
                  perkt = 1.0
                  perpt = 1.0
!
!     LOOP OVER EACH MODAL COORDINATE
!
                  iinc = 0
                  iexc = 0
!
                  DO i = 1 , nmode , 3
                     spag_nextblock_3 = 1
                     SPAG_DispatchLoop_3: DO
                        SELECT CASE (spag_nextblock_3)
                        CASE (1)
!
                           mode = z(i)
                           freq = rz(i+1)
                           grid = z(i+2)
!
!     GET ENERGIES FORM THE PROPER VECTOR
!
                           IF ( .NOT.(noexcl) ) THEN
                              IF ( grid==0 ) THEN
!
                                 keng = rz(ivec3+iexc)
                                 peng = rz(ivec4+iexc)
                                 iexc = iexc + 1
                                 type(1) = temode(1)
                                 type(2) = temode(2)
                                 spag_nextblock_3 = 2
                                 CYCLE SPAG_DispatchLoop_3
                              ENDIF
                           ENDIF
                           keng = rz(ivec1+iinc)
                           peng = rz(ivec2+iinc)
                           iinc = iinc + 1
                           type(1) = timode(1)
                           type(2) = timode(2)
!
                           IF ( mode==0 ) THEN
                              type(1) = trigid(1)
                              type(2) = trigid(2)
                           ENDIF
                           spag_nextblock_3 = 2
                        CASE (2)
!
!     CALCULATE THE ENERGY PERCENTAGES
!
                           perk = keng/tkeng
                           IF ( perk>=100.0 ) perk = 99.9999
                           perp = peng/tpeng
                           IF ( perp>=100.0 ) perp = 99.9999
                           IF ( grid==0 ) THEN
                              perkt = perkt + perk
                              perpt = perpt + perp
                           ENDIF
!
!     PRINT A LINE OF OUTPUT
!
                           nlines = nlines + 1
                           IF ( nlines>nlpp ) THEN
                              CALL page1
                              WRITE (nout,99002) rss
!
!     FORMAT STATEMENTS
!
99002                         FORMAT (//39X,43HMODAL COORDINATE ENERGIES FOR SUBSTRUCTURE ,2A4)
                              IF ( rfno==9 ) WRITE (nout,99003) step
99003                         FORMAT (//12X,7HTIME = ,1P,E13.6)
                              IF ( rfno/=9 ) WRITE (nout,99004) step
99004                         FORMAT (//12X,12HFREQUENCY = ,1P,E13.6)
                              WRITE (nout,99005)
99005                         FORMAT (//12X,4HGRID,6X,4HTYPE,6X,4HMODE,7X,9HFREQUENCY,10X,7HKINETIC,8X,8HKE/TOTAL,6X,9HPOTENTIAL,7X,&
                                     &8HPE/TOTAL,/)
                              nlines = 0
                           ENDIF
!
                           WRITE (nout,99006) grid , type , mode , freq , keng , perk , peng , perp
99006                      FORMAT (1H ,8X,I8,5X,2A4,2X,I5,5X,1P,E13.6,2(5X,1P,E13.6,5X,0P,F7.4))
                           EXIT SPAG_DispatchLoop_3
                        END SELECT
                     ENDDO SPAG_DispatchLoop_3
!
                  ENDDO
!
!     PRINT THE TOTAL KINETIC AND POTENTIAL ENERGIES FOR THIS COLUMN
!
                  IF ( perkt>=100.0 ) perkt = 99.9999
                  IF ( perpt>=100.0 ) perpt = 99.9999
                  IF ( ikflag/=0 ) THEN
                     tkeng = 0.0
                     perkt = 0.0
                  ENDIF
                  IF ( ipflag/=0 ) THEN
                     tpeng = 0.0
                     perpt = 0.0
                  ENDIF
                  WRITE (nout,99007) tkeng , perkt , tpeng , perpt
99007             FORMAT (1H ,55X,2(4X,14H--------------,4X,8H--------),/12X,28HTOTAL ENERGY FOR THIS VECTOR,15X,                   &
                        & 2(5X,1P,E13.6,5X,0P,F7.4))
                  CYCLE
!
!     THIS VECTOR IS NOT TO BE PRINTED SO SKIP IT
!
 30               CALL fwdrec(*40,scr6)
                  CALL fwdrec(*40,scr7)
                  IF ( .NOT.(noexcl) ) THEN
                     CALL fwdrec(*40,scr3)
                     CALL fwdrec(*40,scr4)
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
!     CLOSE FILES
!
         CALL close(scr6,rew)
         CALL close(scr7,rew)
         IF ( .NOT.(noexcl) ) THEN
            CALL close(scr3,rew)
            CALL close(scr4,rew)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     NORMAL RETURN
!
         CALL sofcls
         RETURN
      CASE (4)
         CALL smsg(rc+4,item,names)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      n = 2
         CALL mesage(n,file,name)
         spag_nextblock_1 = 5
      CASE (5)
         CALL sofcls
         WRITE (nout,99008) uwm , rss
99008    FORMAT (A25,' 6371, MODAL REDUCTION ENERGY CALCULATIONS FOR ','SUBSTRUCTURE ',2A4,' ABORTED.')
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rcove
