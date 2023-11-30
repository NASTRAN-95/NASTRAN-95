
SUBROUTINE rcove
   IMPLICIT NONE
   INTEGER Buf(1) , Buf1 , Buf2 , Buf3 , Buf4 , Dry , Energy , Fss(2) , Icore , Incru , Iopt , Ireq , Iru , Itinu , Ititle(96) ,    &
         & Lbasic , Lcore , Loop , Lreq , Lui , Mrecvr , Neigv , Nlines , Nlpp , Norew , Nosort , Nout , Nru , Rfno , Rsp , Rss(2) ,&
         & Sof1 , Sof2 , Sof3 , Sysbuf , Ua , Z(3)
   REAL Cdp , Csp , Diag , Dum1(6) , Dum2(2) , Eofnrw , Pa , Pthres , Qa , Qthres , Range(2) , Rd , Rdp , Rdrew , Rect , Rew ,      &
      & Rz(1) , Square , Step , Uimpro , Uinms(2,5) , Uthres , Wrt , Wrtrew
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Square , Rect , Diag
   COMMON /output/ Ititle
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /system/ Sysbuf , Nout , Dum1 , Nlpp , Dum2 , Nlines
   COMMON /unpakx/ Itinu , Iru , Nru , Incru
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Rz
   INTEGER andf , korsz
   INTEGER blank , casecc(2) , casess , cmask , eqss , file , grid , higher(2) , i , ib , icode , icol , idit , iexc , iinc ,       &
         & ikflag , imdi , imode , ip , ipflag , iset , isets , isil , item , ivec1 , ivec2 , ivec3 , ivec4 , lams , lcc , lseq ,   &
         & lset , lskip , mcb(7) , mmask , mode , n , name(2) , names(2) , ncol , next , nmode , nmodei , nrigid , nrowe , nrowi ,  &
         & rc , scr3 , scr4 , scr6 , scr7 , soln , temode(2) , timode(2) , trigid(2) , type(2)
   LOGICAL credu , mredu , noexcl
   REAL freq , keng , peng , perk , perkt , perp , perpt , tkeng , tpeng
   EXTERNAL andf
!
!     THIS SUBROUTINE PRINTS THE ENERGIES ON THE MODAL COORDINATES
!     IN A SUBSTRUCTURE THAT WAS MODAL REDUCED.  IT WILL ALSO PRINT
!     THE ENERGIES ON THOSE MODES EXCLUDED FROM THE REDUCTION
!     PROCESSING.
!
   EQUIVALENCE (Buf(1),Rz(1))
   EQUIVALENCE (Rz(1),Z(1))
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
!
!     IF THIS IS A STATICS SOLUTION NO ENERGY CALCULATIONS CAN BE MADE
!
   IF ( Rfno<=2 ) RETURN
!
!     INITIALIZE
!
   Sof1 = korsz(Z) - Sysbuf + 1
   Sof2 = Sof1 - Sysbuf - 1
   Sof3 = Sof2 - Sysbuf
   Buf1 = Sof3 - Sysbuf
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Buf4 = Buf3 - Sysbuf
   Lcore = Buf4 - 1
   IF ( Lcore<=0 ) THEN
      n = 8
      CALL mesage(n,file,name)
      GOTO 1000
   ELSE
!
!     GET THE NAME OF THE HIGHER LEVEL SUBSTRUCTURE.  IF NONE EXISTS
!     THEN RETURN.
!
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      names(1) = Rss(1)
      names(2) = Rss(2)
      CALL fndnxl(Rss,higher)
      rc = 4
      IF ( higher(1)==blank ) THEN
!
!     ERRORS
!
         CALL smsg(rc-2,item,names)
         GOTO 1000
      ELSE
         IF ( higher(1)==Rss(1) .AND. higher(2)==Rss(2) ) GOTO 700
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
            GOTO 1000
         ELSE
            CALL fmdi(idit,imdi)
            mredu = .FALSE.
            credu = .FALSE.
            IF ( andf(Buf(imdi+ib),mmask)/=0 ) mredu = .TRUE.
            IF ( andf(Buf(imdi+ib),cmask)/=0 ) credu = .TRUE.
            IF ( .NOT.mredu ) GOTO 700
!
!     READ THE MODAL GROUP OF THE EQSS TO DETERMINE IF THERE ARE ANY
!     RIGID BODY DOF PRESENT.  ALSO GET THE SIL NUMBER OF THE FIRST
!     MODAL CORDINATE.
!
            item = eqss
            CALL sfetch(higher,eqss,1,rc)
            IF ( rc/=1 ) THEN
               CALL smsg(rc-2,item,names)
               GOTO 1000
            ELSE
               CALL suread(Z(1),3,n,rc)
               IF ( rc/=1 ) GOTO 800
               n = Z(3)
               CALL sjump(n)
               IF ( n<0 ) THEN
                  CALL smsg(7,item,names)
                  GOTO 1000
               ELSE
!
                  nrigid = 0
                  DO
                     CALL suread(Z(1),3,n,rc)
                     IF ( rc/=1 ) GOTO 800
                     IF ( nrigid==0 ) ip = Z(2)
                     IF ( Z(1)<100 ) THEN
                        nrigid = nrigid + 1
!
                     ELSEIF ( 2*ip>Sof3 ) THEN
                        n = 8
                        CALL mesage(n,file,name)
                        GOTO 1000
                     ELSE
                        n = 1
                        CALL sjump(n)
                        IF ( n<0 ) THEN
                           CALL smsg(7,item,names)
                           GOTO 1000
                        ELSE
!
                           CALL suread(Z(1),2*ip,n,rc)
                           IF ( rc/=1 ) GOTO 800
                           i = 2*(ip-1) + 1
                           isil = Z(i)
!
!     CALCULATE THE ENERGIES ON THE EXCLUDED MODES
!
                           noexcl = .TRUE.
                           nrowe = 0
                           IF ( .NOT.(credu .OR. Rfno<3 .OR. Rfno>8) ) THEN
                              noexcl = .FALSE.
                              CALL rcovem(noexcl,nrowe)
                           ENDIF
!
!     CALCULATE THE ENERGIES ON THE INCLUDED MODE AND THE TOTAL
!     ENERGIES ON EACH VECTOR
!
                           CALL rcovim(higher)
                           IF ( Iopt<0 ) GOTO 1000
                           mcb(1) = scr6
                           CALL rdtrl(mcb)
                           ncol = mcb(2)
                           nrowi = mcb(3)
                           nmodei = nrowi - isil + 1
!
!     READ THE MODE DATA FROM LAMS AND SAVE THE MODE NUMBER AND
!     THE FREQUENCY FOR EACH MODE.
!
                           names(1) = Rss(1)
                           names(2) = Rss(2)
                           item = lams
                           CALL sfetch(Rss,lams,1,rc)
                           IF ( rc/=1 ) THEN
                              CALL smsg(rc-2,item,names)
                              GOTO 1000
                           ELSE
                              n = 1
                              CALL sjump(n)
                              IF ( n<0 ) THEN
                                 CALL smsg(7,item,names)
                                 GOTO 1000
                              ELSE
                                 imode = 1
                                 IF ( nrigid/=0 ) THEN
                                    n = 3*nrigid
                                    imode = imode + n
                                    DO i = 1 , n , 3
                                       Z(i) = 0
                                       Z(i+1) = 0
                                       Z(i+2) = (i-1)/3 + 1
                                    ENDDO
                                 ENDIF
                                 nmode = 3*(nmodei-1+nrowe)
                                 IF ( nmode>Lcore ) THEN
                                    n = 8
                                    CALL mesage(n,file,name)
                                    GOTO 1000
                                 ELSE
!
                                    DO i = imode , nmode , 3
                                       CALL suread(Z(i),7,n,rc)
                                       IF ( rc/=1 ) GOTO 800
                                       Z(i+1) = Z(i+4)
                                       Z(i+2) = 0
                                    ENDDO
!
!     READ THE LAST GROUP OF LAMS AND GENERATE GRID NUMBERS FOR THE
!     INCLUDED MODES.
!
                                    n = 1
                                    CALL sjump(n)
                                    IF ( n<0 ) THEN
                                       CALL smsg(7,item,names)
                                       GOTO 1000
                                    ELSE
                                       iinc = 100
                                       DO i = imode , nmode , 3
                                         CALL suread(icode,1,n,rc)
                                         IF ( rc/=1 ) GOTO 800
                                         IF ( icode<=1 ) THEN
                                         iinc = iinc + 1
                                         Z(i+2) = iinc
                                         ENDIF
                                       ENDDO
!
!     POSITION THE SOLN ITEM TO THE FREQUENCY OR TIME DATA
!
                                       item = soln
                                       CALL sfetch(Rss,soln,1,rc)
                                       IF ( rc/=1 ) THEN
                                         CALL smsg(rc-2,item,names)
                                         GOTO 1000
                                       ELSE
                                         n = 1
                                         CALL sjump(n)
                                         IF ( n<0 ) THEN
                                         CALL smsg(7,item,names)
                                         GOTO 1000
                                         ELSE
!
!     ALLOCATE INCORE ARRAYS FOR THE ENERGY VECTORS
!
                                         ivec1 = nmode + 1
                                         ivec2 = ivec1 + nmodei
                                         ivec3 = ivec2 + nmodei
                                         ivec4 = ivec3 + nrowe
                                         isets = ivec4 + nrowe
                                         IF ( isets>Lcore ) THEN
                                         n = 8
                                         CALL mesage(n,file,name)
                                         GOTO 1000
                                         ELSE
!
!     READ CASESS AND GET THE TITLE AND ANY SET INFORMATION
!
                                         file = casess
                                         CALL gopen(casess,Z(Buf1),Rdrew)
                                         DO
                                         CALL fread(casess,Z(ivec1),2,1)
                                         IF ( Z(ivec1)==casecc(1) .AND. Z(ivec1+1)==casecc(2) ) THEN
!
                                         CALL fread(casess,0,-38,0)
                                         CALL fread(casess,Ititle(1),96,0)
!
                                         IF ( Energy<=0 ) GOTO 200
                                         CALL fread(casess,0,-31,0)
                                         CALL fread(casess,lcc,1,0)
                                         lskip = 167 - lcc
                                         CALL fread(casess,0,lskip,0)
                                         CALL read(*900,*100,casess,lseq,1,0,i)
                                         IF ( lseq>0 ) CALL fread(casess,0,lseq,0)
                                         DO
!
                                         CALL read(*900,*100,casess,iset,1,0,i)
                                         CALL fread(casess,lset,1,0)
                                         IF ( iset/=Energy ) THEN
                                         CALL fread(casess,0,-lset,0)
                                         ELSEIF ( isets+lset>Lcore ) THEN
                                         n = 8
                                         CALL mesage(n,file,name)
                                         GOTO 1000
                                         ELSE
                                         CALL fread(casess,Z(isets),lset,0)
                                         GOTO 200
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
 100  WRITE (Nout,99001) Uwm , Energy
99001 FORMAT (A25,' 6365, REQUESTED OUTPUT SET ID',I6,' IS NOT ','DECLARED IN CASE CONTROL. ALL OUTPUT WILL BE PRODUCED')
   Energy = -1
!
 200  CALL close(casess,Rew)
!
!     LOOP OVER EACH COLUMN AND PRINT THE KINETIC AND POTENTIAL
!     ENERGIES FOR EACH MODAL COORDINATE IF REQUESTED
!
   next = 1
   CALL gopen(scr6,Z(Buf1),Rdrew)
   CALL gopen(scr7,Z(Buf2),Rdrew)
   IF ( .NOT.(noexcl) ) THEN
      CALL gopen(scr3,Z(Buf3),Rdrew)
      CALL gopen(scr4,Z(Buf4),Rdrew)
   ENDIF
!
   Itinu = Rsp
   Incru = 1
!
   DO icol = 1 , ncol
!
!     SET FLAGS FOR NULL COLUMNS
!
      ikflag = 0
      ipflag = 0
!
!     GET THE FREQUENCY OR TIME FOR THIS VECTOR
!
      IF ( Rfno>3 ) THEN
!
!     DYNAMICS SOLUTION
!
         CALL suread(Step,1,n,rc)
         IF ( rc/=1 ) GOTO 800
      ELSE
!
!     NORMAL MODES SOLUTION
!
         CALL suread(Z(ivec1),7,n,rc)
         IF ( rc/=1 ) GOTO 800
         Step = Rz(ivec1+4)
      ENDIF
!
!     SEE IF THIS COLUMN IS REQUESTED
!
      IF ( Energy>0 ) CALL setfnd(*650,Z(isets),lset,icol,next)
!
      IF ( Step<Range(1) .OR. Step>Range(2) ) GOTO 650
!
!     UNPACK THE KINETIC AND POTENTIAL ENERGIES ON INCLUDED MODES
!
      Iru = isil
      Nru = nrowi
      CALL unpack(*250,scr6,Rz(ivec1))
      GOTO 300
 250  DO i = 1 , nmodei
         Rz(ivec1+i-1) = 0.0
      ENDDO
      Rz(ivec1+nmodei-1) = 1.0
      ikflag = 1
!
 300  CALL unpack(*350,scr7,Rz(ivec2))
      GOTO 400
 350  DO i = 1 , nmodei
         Rz(ivec2+i-1) = 0.0
      ENDDO
      Rz(ivec2+nmodei-1) = 1.0
      ipflag = 1
!
!     UNPACK THE KINETIC AND POTENTIAL ENERGIES ON EXLUDED MODES
!
 400  IF ( .NOT.(noexcl) ) THEN
         Iru = 1
         Nru = nrowe
         CALL unpack(*450,scr3,Rz(ivec3))
         GOTO 500
      ENDIF
 450  DO i = 1 , nrowe
         Rz(ivec3+i-1) = 0.0
      ENDDO
!
 500  IF ( .NOT.(noexcl) ) THEN
         CALL unpack(*550,scr4,Rz(ivec4))
         GOTO 600
      ENDIF
 550  DO i = 1 , nrowe
         Rz(ivec4+i-1) = 0.0
      ENDDO
!
!     INITILIZE FOR THE OUTPUT
!
 600  Nlines = Nlpp
!
!     GET TOTAL ENERGIES
!
      tkeng = Rz(ivec1+nmodei-1)
      tpeng = Rz(ivec2+nmodei-1)
      perkt = 1.0
      perpt = 1.0
!
!     LOOP OVER EACH MODAL COORDINATE
!
      iinc = 0
      iexc = 0
!
      DO i = 1 , nmode , 3
!
         mode = Z(i)
         freq = Rz(i+1)
         grid = Z(i+2)
!
!     GET ENERGIES FORM THE PROPER VECTOR
!
         IF ( .NOT.(noexcl) ) THEN
            IF ( grid==0 ) THEN
!
               keng = Rz(ivec3+iexc)
               peng = Rz(ivec4+iexc)
               iexc = iexc + 1
               type(1) = temode(1)
               type(2) = temode(2)
               GOTO 620
            ENDIF
         ENDIF
         keng = Rz(ivec1+iinc)
         peng = Rz(ivec2+iinc)
         iinc = iinc + 1
         type(1) = timode(1)
         type(2) = timode(2)
!
         IF ( mode==0 ) THEN
            type(1) = trigid(1)
            type(2) = trigid(2)
         ENDIF
!
!     CALCULATE THE ENERGY PERCENTAGES
!
 620     perk = keng/tkeng
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
         Nlines = Nlines + 1
         IF ( Nlines>Nlpp ) THEN
            CALL page1
            WRITE (Nout,99002) Rss
!
!     FORMAT STATEMENTS
!
99002       FORMAT (//39X,43HMODAL COORDINATE ENERGIES FOR SUBSTRUCTURE ,2A4)
            IF ( Rfno==9 ) WRITE (Nout,99003) Step
99003       FORMAT (//12X,7HTIME = ,1P,E13.6)
            IF ( Rfno/=9 ) WRITE (Nout,99004) Step
99004       FORMAT (//12X,12HFREQUENCY = ,1P,E13.6)
            WRITE (Nout,99005)
99005       FORMAT (//12X,4HGRID,6X,4HTYPE,6X,4HMODE,7X,9HFREQUENCY,10X,7HKINETIC,8X,8HKE/TOTAL,6X,9HPOTENTIAL,7X,8HPE/TOTAL,/)
            Nlines = 0
         ENDIF
!
         WRITE (Nout,99006) grid , type , mode , freq , keng , perk , peng , perp
99006    FORMAT (1H ,8X,I8,5X,2A4,2X,I5,5X,1P,E13.6,2(5X,1P,E13.6,5X,0P,F7.4))
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
      WRITE (Nout,99007) tkeng , perkt , tpeng , perpt
99007 FORMAT (1H ,55X,2(4X,14H--------------,4X,8H--------),/12X,28HTOTAL ENERGY FOR THIS VECTOR,15X,2(5X,1P,E13.6,5X,0P,F7.4))
      CYCLE
!
!     THIS VECTOR IS NOT TO BE PRINTED SO SKIP IT
!
 650  CALL fwdrec(*900,scr6)
      CALL fwdrec(*900,scr7)
      IF ( .NOT.(noexcl) ) THEN
         CALL fwdrec(*900,scr3)
         CALL fwdrec(*900,scr4)
      ENDIF
!
   ENDDO
!
!     CLOSE FILES
!
   CALL close(scr6,Rew)
   CALL close(scr7,Rew)
   IF ( .NOT.(noexcl) ) THEN
      CALL close(scr3,Rew)
      CALL close(scr4,Rew)
   ENDIF
!
!     NORMAL RETURN
!
 700  CALL sofcls
   RETURN
 800  CALL smsg(rc+4,item,names)
   GOTO 1000
 900  n = 2
   CALL mesage(n,file,name)
 1000 CALL sofcls
   WRITE (Nout,99008) Uwm , Rss
99008 FORMAT (A25,' 6371, MODAL REDUCTION ENERGY CALCULATIONS FOR ','SUBSTRUCTURE ',2A4,' ABORTED.')
   RETURN
END SUBROUTINE rcove
