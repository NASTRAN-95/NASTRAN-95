!*==rcovb.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovb
USE C_BLANK
USE C_MPYADX
USE C_NAMES
USE C_RCOVCM
USE C_RCOVCR
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , eqss , gmask , horg , ib , mmask , pove , rmask , schk , scr1 , scr2 , scr3 , scr5 , ugv , uvec
   INTEGER , DIMENSION(1) :: buf , iz
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: file , i , idit , idpcor , imdi , ipove , item , j , jlvl , lastss , lcorez , n , nrow , pao , rc , ub
   INTEGER , DIMENSION(7) :: mcbtrl
   LOGICAL :: modal
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , SAVE :: scr6 , scr7 , srd , swrt
   INTEGER , DIMENSION(2) :: ssnm
   INTEGER , DIMENSION(5) , SAVE :: ui
   EXTERNAL andf , fdsub , fmdi , fndnxl , korsz , makmcb , mesage , mpyad , mtrxi , mtrxo , rcovls , rcovui , rcovuo , rdtrl ,     &
          & sfetch , smsg , sofcls , sofopn , sofsiz , softrl , tmtogo , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     RCOVB PERFORMS THE BACK-SUBSTITUTIONS TO OBTAIN THE G-SET
!     DISPLACEMENTS OF A SUBSTRUCTURE WHOSE LEVEL IS LOWER THAN OR
!     EQUAL TO THAT OF THE FINAL SOLUTION STRUCTURE (FSS).
!     FOR EACH SUBSTRUCTURE WHOSE DISPLACEMENTS ARE RECOVERED,
!     AN SOLN ITEM IS CREATED BY EDITING THE SOLN ITEM OF THE FSS.
!
!     INTEGER          SCR6       ,SCR7       ,SRD        ,SWRT
   !>>>>EQUIVALENCE (Buf(1),Z(1))
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1))
   DATA name/4HRCOV , 4HB   /
   DATA ugv , scr1 , scr2 , scr3 , scr5/106 , 301 , 302 , 303 , 305/
   DATA ui/204 , 205 , 206 , 207 , 208/
   DATA uvec , pove , horg , eqss/4HUVEC , 4HPOVE , 4HHORG , 4HEQSS/
   DATA ib , schk/1 , 3/
   DATA scr6 , scr7 , srd , swrt/306 , 307 , 1 , 2/
   DATA rmask/469762048/
   DATA gmask/268435456/
   DATA mmask/134217728/
   DATA blank/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         lcorez = korsz(Z) - Lreq
         Sof1 = lcorez - Sysbuf + 1
         Sof2 = Sof1 - Sysbuf - 1
         Sof3 = Sof2 - Sysbuf
         Buf1 = Sof3 - Sysbuf
         Buf2 = Buf1 - Sysbuf
         Buf3 = Buf2 - Sysbuf
         Buf4 = Buf3 - Sysbuf
         Lcore = Buf4 - 1
         IF ( Lcore<=0 ) THEN
            n = 8
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
            Ua = 0
            pao = 0
            Tflag = 0
            Signab = 1
            Signc = 1
            Mprec = 0
            Scrm = scr5
!
!     FIND OUT HOW MANY UI FILES THERE ARE AND WHICH ONES
!
            DO i = 1 , 5
               iz(1) = ui(i)
               CALL rdtrl(iz)
               IF ( iz(1)<0 ) Uinms(1,i) = 0
            ENDDO
!
!     IF UINMS(1,I) = 0         THEN  FILE UI(I) IS PURGED
!     IF UINMS(1,I) = BLANK     THEN  FILE UI(I) IS AVAILABLE AND NOT
!                                     IN USE
!     IF UINMS(1,I) = OTHER     THEN  FILE UI(I) CONTAINS UGV FOR
!                                     SUBSTRUCTURE -OTHER-
!
            ssnm(1) = Ssnm1(1)
            ssnm(2) = Ssnm1(2)
!
!     IF SSNM IS THE FINAL SOLUTION STRUCTURE (FSS), NO RECOVERY IS
!     NECESSARY.
!
            IF ( ssnm(1)/=Fss(1) .OR. ssnm(2)/=Fss(2) ) THEN
!
!     SEARCH THE SOF FOR A DISPLACEMENT MATRIX OF SSNM OR A HIGHER
!     LEVEL SUBSTRUCTURE FROM WHICH THE REQUESTED DISPLACEMENTS CAN BE
!     RECOVERED
!
               jlvl = 1
               SPAG_Loop_1_1: DO
                  CALL softrl(ssnm,uvec,mcbtrl)
                  rc = mcbtrl(1)
                  IF ( rc==1 ) EXIT SPAG_Loop_1_1
                  IF ( rc==2 .AND. Dry<0 ) EXIT SPAG_Loop_1_1
                  IF ( rc==3 ) THEN
!
!     NO UVEC AT THIS LEVEL.  SAVE SSNM IN A STACK AT TOP OF OPEN CORE
!     AND SEARCH FOR UVEC OF THE NEXT HIGHER LEVEL
!
                     lastss = 2*jlvl - 1
                     iz(lastss) = ssnm(1)
                     iz(lastss+1) = ssnm(2)
                     jlvl = jlvl + 1
                     CALL fndnxl(Z(lastss),ssnm)
                     IF ( ssnm(1)==blank ) THEN
                        WRITE (Nout,99001) Uwm , iz(lastss) , iz(lastss+1)
!
!     FORMAT STATEMENTS
!
99001                   FORMAT (A25,' 6306, ATTEMPT TO RECOVER DISPLACEMENTS FOR NON-','EXISTANT SUBSTRUCTURE ',2A4)
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ELSEIF ( ssnm(1)==iz(lastss) .AND. ssnm(2)==iz(lastss+1) ) THEN
                        WRITE (Nout,99002) Uwm , Ssnm1 , ssnm
99002                   FORMAT (A25,' 6308, NO SOLUTION AVAILABLE FROM WHICH DISPLACE','MENTS FOR SUBSTRUCTURE ',2A4,/32X,          &
                               &'CAN BE RECOVERED.  ','HIGHEST LEVEL SUBSTRUCTURE FOUND WAS ',2A4)
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
!
!     IF SSNM IS NOT THE FSS, LOOK FOR UVEC ON THE SOF.  IF DRY RUN,
!     EXIT.  IF IT IS THE FSS, SET UA=UGV.  IF UGV IS NOT PURGED GO TO
!     BEGIN BACK-SUBSTITUTION.  OTHERWISE, GIVE IT THE SAME TREATMENT
!     AS IF IT WERE NOT THE FSS.
!
                     ELSEIF ( ssnm(1)==Fss(1) .AND. ssnm(2)==Fss(2) ) THEN
                        IF ( Dry<0 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        Ua = ugv
                        mcbtrl(1) = Ua
                        CALL rdtrl(mcbtrl)
                        IF ( mcbtrl(1)>0 ) GOTO 10
                     ENDIF
                  ELSE
                     IF ( rc==5 ) CALL smsg(3,uvec,ssnm)
                     IF ( rc==4 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     WRITE (Nout,99003) Uwm , Ssnm1 , ssnm
99003                FORMAT (A25,' 6307, WHILE ATTEMPTING TO RECOVER DISPLACEMENTS ','FOR SUBSTRUCTURE ',2A4,1H,,/32X,              &
                            &'THE DISPLACEMENTS FOR ','SUBSTRUCTURE ',2A4,' WERE FOUND TO EXIST IN DRY RUN ','FORM ONLY.')
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_1_1
!
!     FOUND A UVEC ON SOF FOR THIS LEVEL.  SEE IF IT HAS ALREADY BEEN
!     PUT ON A UI FILE.  (IF DRY RUN, EXIT)
!
               IF ( Dry<0 .OR. jlvl==1 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , 5
                  Ua = ui(i)
                  IF ( ssnm(1)==Uinms(1,i) .AND. ssnm(2)==Uinms(2,i) ) GOTO 10
               ENDDO
!
!     DATA BLANK /4H    /
!
!     IT DOES NOT RESIDE ON ANY UI FILE.  FIND A UI FILE TO USE.
!
               j = 0
               DO i = 1 , 5
                  IF ( Uinms(1,i)/=0 ) THEN
                     j = j + 1
                     IF ( Uinms(1,i)==blank ) GOTO 5
                  ENDIF
               ENDDO
!
!     ALL UI FILES SEEM TO BE IN USE.  DO ANY REALLY EXIST
!
               IF ( j==0 ) THEN
!
!     ALL UI FILES ARE PURGED.  USE SCR1 INSTEAD
!
                  Ua = scr1
!
!     COPY UVEC FROM SOF TO UA
!
                  CALL mtrxi(Ua,ssnm,uvec,0,rc)
                  GOTO 10
               ELSE
!
!     AT LEAST ONE EXISTS.  RE-USE THE ONE WITH OLDEST DATA
!
                  i = Lui + 1
                  IF ( i>5 ) i = 1
                  j = i
                  DO WHILE ( Uinms(1,i)==0 )
!
!     NO FILE THERE.  TRY NEXT ONE.
!
                     i = i + 1
                     IF ( i>5 ) i = 1
                     IF ( i==j ) THEN
                        Ua = scr1
                        CALL mtrxi(Ua,ssnm,uvec,0,rc)
                        GOTO 10
                     ENDIF
                  ENDDO
               ENDIF
!
!     FOUND A UI FILE TO USE
!
 5             Lui = i
               Ua = ui(i)
               Uinms(1,i) = ssnm(1)
               Uinms(2,i) = ssnm(2)
               CALL mtrxi(Ua,ssnm,uvec,0,rc)
            ELSE
               Ua = ugv
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 10         SPAG_Loop_1_2: DO
!
!     TOP OF BACK-SUBSTITUTION LOOP
!
               ub = Ua
               Uaomcb(1) = 0
               Icore = lastss + 2
               idpcor = Icore/2 + 1
!
!     CHECK IF THE EQSS ITEM IS THERE FOR THIS SUBSTRUCTURE
!
               CALL sfetch(Z(lastss),eqss,schk,rc)
               IF ( rc/=1 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     COMPUTE TIME TO RECOVER THIS LEVEL AND CHECK TIME-TO-GO
!
!     (A DETAILED TIME CHECK SHOULD BE CODED LATER.  FOR THE PRESENT,
!     JUST CHECK TO SEE IF TIME HAS RUN OUT NOW.)
!
               CALL tmtogo(i)
               IF ( i<=0 ) THEN
!
!     ERROR PROCESSING
!
                  WRITE (Nout,99004) Sfm , iz(lastss) , iz(lastss+1) , ssnm , Ssnm1
99004             FORMAT (A25,' 6309, INSUFFICIENT TIME REMAINING TO RECOVER DIS','PLACEMENTS OF SUBSTRUCTURE ',2A4,/32X,           &
                         &'FROM THOSE OF ','SUBSTRUCTURE ',2A4,'.  (PROCESSING USER RECOVER REQUEST',/32X,'FOR SUBSTRUCTURE ',2A4,  &
                         &1H))
                  n = -37
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSE
!
!     CHECK REMAINING SPACE ON SOF.  FIRST CALCULATE HOW MUCH SPACE
!     THE RECOVERED DISPLACEMENT MATRIX WILL TAKE (ASSUMING IT IS FULL).
!
                  mcbtrl(1) = ub
                  CALL rdtrl(mcbtrl)
                  i = mcbtrl(2)
!
!     NO. OF COLUMNS IN DISPLACEMENT MATRIX IN I
!
                  CALL softrl(Z(lastss),horg,mcbtrl)
                  rc = mcbtrl(1)
                  item = horg
                  IF ( rc>1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  nrow = mcbtrl(3)
                  j = i*nrow
!
!     NOW CHECK SPACE
!
                  IF ( sofsiz(i)<j ) THEN
                     WRITE (Nout,99005) Swm , iz(lastss) , iz(lastss+1) , ssnm , Ssnm1
99005                FORMAT (A27,' 6310, INSUFFICIENT SPACE ON SOF TO RECOVER DIS','PLACEMENTS OF SUBSTRUCTURE ',2A4,/32X,          &
                            &' FROM THOSE OF ','SUBSTRUCTURE ',2A4,' WHILE PROCESSING USER RECOVER ','REQUEST',/32X,                &
                            &'FOR SUBSTRUCTURE ',2A4)
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
!
!     CREATE THE SOLUTION ITEM FOR THE RECOVERED SUBSTRUCTURE.
!
                     CALL rcovls(Z(lastss))
                     IF ( Iopt<0 ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     FIND A UI FILE FOR DISPLACEMENTS
!
                     j = 0
                     DO i = 1 , 5
                        IF ( Uinms(1,i)/=0 ) THEN
                           j = j + 1
                           IF ( Uinms(1,i)==blank ) GOTO 12
                        ENDIF
                     ENDDO
!
!     NO UNUSED UI FILES ARE AVAILABLE.  IF TWO OR MORE UI FILES ARE
!     NOT PURGED, USE THE ONE WITH OLDEST DATA.  OTHERWISE, USE SCR2.
!     MAKE SURE WE DON T ASSIGN THE SAME FILE AS THE HIGHER
!     LEVEL DISPLACEMENTS ARE ON (UB)
!
                     IF ( j<2 ) THEN
                        Ua = scr2
                        GOTO 14
                     ELSE
                        i = Lui + 1
                        IF ( i>5 ) i = 1
                        j = i
                        DO WHILE ( Uinms(1,i)==0 .OR. ui(i)==ub )
                           i = i + 1
                           IF ( i>5 ) i = 1
                           IF ( i==j ) THEN
                              Ua = scr2
                              GOTO 14
                           ENDIF
                        ENDDO
                     ENDIF
!
!     FOUND A UI FILE
!
 12                  Lui = i
                     Ua = ui(i)
                     Uinms(1,i) = iz(lastss)
                     Uinms(2,i) = iz(lastss+1)
!
!     IF THE RECOVERED SUBSTRUCTURE WAS NOT REDUCED GENERATE THE
!     DISPLACEMENTS DIRECTLY.
!     IF THE SUBSTRUCTURE WAS REDUCED AND THE UIMPROVED FLAG IS SET
!     AND THIS IS A NON-STATICS RUN GENERATE THE IMPROVED DISPLACEMENTS.
!     IF THE SUBSTRUCTURE WAS IN A GUYAN REDUCTION AND THIS IS A
!     STATICS RUN GENERATE THE LOADS ON THE OMMITED POINTS.
!
!     INCLUDE THE CHECK ON THE POVE ITEM ALSO TO BE COMPATABLE WITH
!     PREVIOUS SOFS WITH NO TYPE BITS
!
 14                  CALL softrl(Z(lastss),pove,mcbtrl)
                     ipove = mcbtrl(1)
                     CALL fdsub(ssnm,idit)
                     rc = 4
                     IF ( idit<0 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     CALL fmdi(idit,imdi)
                     modal = .FALSE.
                     IF ( andf(buf(imdi+ib),mmask)/=0 ) modal = .TRUE.
                     IF ( andf(buf(imdi+ib),rmask)/=0 .AND. Uimpro/=0 .AND. Rfno>2 ) THEN
!
!     IF THE USER REQUESTED AN IMPROVED VECTOR AND THIS IS A NONSTATICS
!     RUN THEN GENERATE IT.
!
                        CALL rcovui(ub,Z(lastss),modal)
                        IF ( Iopt<0 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        GOTO 18
                     ELSE
                        IF ( andf(buf(imdi+ib),gmask)==0 .OR. Rfno>2 ) THEN
                           IF ( andf(buf(imdi+ib),rmask)/=0 .OR. ipove/=1 .OR. Rfno>2 ) GOTO 16
                        ENDIF
!
!     GENERATE THE LOADS ON THE OMITED POINTS FOR REDUCED SUBSTRUCTURES
!     IF THIS IS A STATICS RUN
!
                        CALL rcovuo(0,Uaomcb(1),Z(lastss))
                        IF ( Iopt<0 ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
!
!     MULIPLY AND ADD TO GET DISPLACEMENTS OF LOWER-LIVEL SUBSTRUCTURE.
!
!     COPY H OR G TRANSFORMATION MATRIX TO SCR3
!
                        CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
                     ENDIF
 16                  item = horg
                     CALL mtrxi(scr3,Z(lastss),horg,0,rc)
                     IF ( rc/=1 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
!     SETUP FOR MPYAD
!
                     CALL sofcls
                     Hmcb(1) = scr3
                     Ubmcb(1) = ub
                     CALL rdtrl(Hmcb)
                     CALL rdtrl(Ubmcb)
                     IF ( Uaomcb(1)/=0 ) CALL rdtrl(Uaomcb)
                     CALL makmcb(Uamcb,Ua,Hmcb(3),Rect,Ubmcb(5))
                     Mpyz = lcorez - Icore - 7
                     CALL mpyad(dz(idpcor),dz(idpcor),dz(idpcor))
                     CALL wrttrl(Uamcb)
!
!     COPY RECOVERED DISPLACEMENTS TO SOF
!
 18                  CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
                     CALL mtrxo(Ua,Z(lastss),uvec,0,rc)
!
!     END OF BACK-SUBSTITUTION LOOP
!     CLOSE AND REOPEN THE SOF TO GET ANY CONTROL BLOCKS WRITTEN TO
!     FILE
!
                     CALL sofcls
                     CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
                     ssnm(1) = iz(lastss)
                     ssnm(2) = iz(lastss+1)
                     lastss = lastss - 2
                     jlvl = jlvl - 1
                     WRITE (Nout,99006) Uim , jlvl , ssnm
99006                FORMAT (A29,' 6312, LEVEL',I4,' DISPLACEMENTS FOR SUBSTRUCTURE ',2A4,/36X,                                     &
                            &'HAVE BEEN RECOVERED AND SAVED ON THE SOF.')
                     IF ( jlvl<=1 ) EXIT SPAG_Loop_1_2
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     NORMAL COMPLETION OF MODULE EXECUTION
!
         DO i = 1 , 5
            IF ( Uinms(1,i)==0 ) Uinms(1,i) = blank
         ENDDO
         CALL sofcls
         RETURN
      CASE (3)
         IF ( rc==2 ) rc = 3
         CALL smsg(rc-2,item,Z(lastss))
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (Nout,99007) Swm , Ssnm1
99007    FORMAT (A25,' 6317, RECOVER OF DISPLACEMENTS FOR SUBSTRUCTURE ',2A4,' ABORTED.')
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         CALL sofcls
         CALL mesage(n,file,name)
         spag_nextblock_1 = 6
      CASE (6)
         Iopt = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rcovb
