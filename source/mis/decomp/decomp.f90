!*==decomp.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE decomp(Ix,X,Dx) !HIDESTARS (*,Ix,X,Dx)
   USE c_dcompx
   USE c_names
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zntpkx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ix
   REAL , DIMENSION(1) :: X
   REAL(REAL64) , DIMENSION(1) :: Dx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bbar1 , bbbar , bbbar1 , bufa , cbcnt , ccount , count , end , flag , forma , i , i1 , i1sp , i2 , i3 , i3sp , i4 ,   &
            & i4sp , i5 , i6sp , i7sp , ibbar2 , ibufl , icrq , imhere , in1 , in2 , in3 , in4 , intch , intchn , ioff , ipak ,     &
            & iterm , itrn , jpos , jposl , jtrn , k , kk , kkk , lcol , ll , lll , llll , ncol , outbuf , scrflg , sr1buf ,        &
            & sr2buf , sr2fl , sr3buf , sr3fl , typea , typel
   REAL(REAL64) :: da , dtrn , dz , max
   INTEGER , SAVE :: ibegn , iend
   INTEGER , DIMENSION(4) :: itran
   INTEGER , DIMENSION(5) , SAVE :: parm
   EXTERNAL bldpk , bldpkn , close , conmsg , dloop , finwrt , fname , fwdrec , genvec , gopen , intpk , mesage , onetwo , open ,   &
          & page2 , read , transp , write , xloop , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DECOMP WILL DECOMPOSE A REAL UNSYMETRIC MATRIX INTO A UNIT LOWER
!     TRIANGULAR MATRIX AND AN UPPER TRIANGULAR MATRIX,USING PARTIAL
!     PIVOTING WITHIN THE LOWER BAND
!
!     DEFINITION OF INPUT PARAMETERS
!
!     FILEA    =  MATRIX CONTROL BLOCK FOR THE INPUT  MATRIX A
!     FILEL    =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX L
!     FILEU    =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX U
!     SR1FIL   =  SCRATCH FILE
!     SR2FIL   =  SCRATCH FILE
!     SR3FIL   =  SCRATCH FILE
!     NX       =  NUMBER OF CELLS OF CORE AVAILABLE AT IX
!     DET      =  CELL WHERE THE DETERMINATE OF A WILL BE STORED
!     POWER    =  SCALE FACTOR TO BE APPLIED TO THE DETERMINATE
!                 (DETERMINATE = DET*10**POWER)
!     MINDIA   =  CELL WHERE THE VALUE OF THE MINIMUM DIAGONAL WILL BE
!                 SAVED
!     IX       =  BLOCK OF CORE AVAILABLE AS WORKING STORAGE TO DECOMP
!     X        =  SAME BLOCK AS IX, BUT TYPED REAL
!     DX       =  SAME BLOCK AS IX, BUT TYPED DOUBLE PRECISION
!
   !>>>>EQUIVALENCE (Da,A(1)) , (Dz,Z(1)) , (Forma,Filea(4)) , (Typea,Filea(5)) , (Ncol,Filea(3)) , (Typel,Filel(5))
   !>>>>EQUIVALENCE (itran(1),itrn) , (itran(2),jtrn) , (itran(3),dtrn)
   DATA parm(3) , parm(4)/4HDECO , 4HMP  /
   DATA ibegn/4HBEGN/ , iend/4HEND /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     AT LAST, THE START OF THE PROGRAM
!
         IF ( (forma/=sqr .AND. forma/=sym) .OR. typea>rdp ) THEN
!
!     ERROR EXITS
!
            parm(1) = -7
            CALL mesage(parm(1),parm(2),parm(3))
            RETURN
         ELSE
!
!     BUFFER ALLOCATION
!
            bufa = nx - sysbuf
            ibufl = bufa - sysbuf
            outbuf = ibufl - sysbuf
            sr1buf = outbuf - sysbuf
            sr2buf = sr1buf - sysbuf
            sr3buf = sr2buf - sysbuf
            icrq = -sr3buf
            IF ( icrq>0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            det = 1.D0
            power = 0
            mindia = 1.D+25
            iterm = 0
            IF ( filea(1)<0 ) iterm = 1
            filea(1) = iabs(filea(1))
!
!     WRITE THE HEADER RECORD ON THE OUTPUT TAPES AND INITIALIZE THE
!     TRAILER RECORDS.
!
            CALL gopen(filel,Ix(ibufl),wrtrew)
            parm(2) = sr2fil
            CALL open(*60,sr2fil,Ix(outbuf),wrtrew)
            CALL fname(fileu(1),X(1))
            CALL write(sr2fil,X(1),2,1)
            filel(3) = ncol
            filel(4) = 4
            filel(2) = 0
            filel(6) = 0
            filel(7) = 0
            fileu(2) = 0
            fileu(3) = ncol
            fileu(4) = 5
            fileu(6) = 0
            fileu(7) = 0
            filea(5) = 2
            IF ( ncol>2 ) THEN
               IF ( b<=0 .OR. bbar<=0 ) THEN
                  imhere = 10
                  CALL genvec(*120,Ix(bufa),filea(1),nx,Ix(1),ncol,b,bbar,c,cbar,r,1)
               ENDIF
               bbar1 = bbar + 1
               bbbar = min0(b+bbar,ncol)
               bbbar1 = bbbar - 1
               scrflg = 0
               IF ( r<bbbar1 ) scrflg = 1
               IF ( scrflg/=0 ) THEN
                  icrq = (bbbar1-r)*2*bbar
                  CALL page2(3)
                  WRITE (nout,99001) uim , icrq
99001             FORMAT (A29,' 2177, SPILL WILL OCCUR IN UNSYMMETRIC DECOMPOSITION',/,I10,                                         &
                         &' ADDITIONAL MEMORY WORDS NEEDED TO STAY IN CORE.')
               ENDIF
!
!     INITIALIZE POINTERS TO SPECIFIC AREAS OF CORE
!
               i1 = 1
               i1sp = (i1+bbar*r)*2 - 1
               ipak = i1 + bbar*r + bbbar/2 + 1
               i2 = ipak
               i3sp = (i2+min0(ncol,bbbar+bbar))*2 - 1
               i3 = i2 + min0(ncol,bbbar+bbar) + c
               i4sp = i3sp + (bbar+2)*c*2
               i4 = i3 + bbar1*c + cbar
               i5 = i4 + bbbar*cbar
               i6sp = (i5+c*cbar)*2 - 1
               i7sp = i6sp + cbar
               end = i7sp + c
               parm(5) = ibegn
               CALL conmsg(parm(3),3,0)
!
!     DEFINITION OF KEY PROGRAM PARAMETERS
!
!     I1     =  POINTER TO AREA WHERE COMPLETED COLUMNS OF L ARE STORED
!     I1SP   =  POINTER TO AREA WHERE THE PERMUTATION INDEXES ARE STORED
!     IPAK   =  POINTER TO AREA WHERE COLUMNS WILL BE PACKED FROM
!     I2     =  POINTER TO AREA WHERE THE NEXT COLUMN OF A IS STORED
!     I3     =  POINTER TO AREA WHERE ACTIVE COLUMNS ARE STORED
!     I4     =  POINTER TO AREA WHERE ACTIVE ROWS ARE STORED
!     I5     =  POINTER TO AREA WHERE INTERACTION ELEMENTS ARE STORED
!     I6SP   =  POINTER TO AREA WHERE SEQUENCED ACTIVE ROW INDICES
!               ARE STORED
!     I7SP   =  POINTER TO AREA WHERE SEQUENCED ACTIVE COLUMN INDICES
!               ARE STORED
!     B      =  UPPER HALF-BAND
!     BBAR   =  LOWER HALF-BAND
!     C      =  NUMBER OF ACTIVE COLUMNS
!     CBAR   =  NUMBER OF ACTIVE ROWS
!     R      =  NUMBER OF COLUMNS OF L THAT CAN BE STORED IN CORE
!     JPOS   =  CURRENT PIVOTAL COLUMN INDEX
!     JPOSL  =  NEXT COLUMN OF L TO BE WRITTEN OUT
!     LCOL   =  NUMBER OF COLUMNS OF L CURRENTLY STORED IN CORE OR ON
!               SCRATCH FILES
!     CCOUNT =  CURRENT NUMBER OF ACTIVE COLUMNS
!     CBCNT  =  CURRENT NUMBER OF ACTIVE ROWS
!     ITRN   =  ROW INDEX OF NEXT ACTIVE COLUMN ELEMENT
!     JTRN   =  COLUMN INDEX  OF NEXT ACTIVE COLUMN ELEMENT
!     IOFF   =  ROW POSITION OF THE FIRST ELEMENT IN AREA II
!     ITERM  =  IF NONZERO, TERMINATE BEFORE THE RE-WRITE
!     NCOL   =  SIZE OF THE INPUT MATRIX
!     BBBAR  =  B + BBAR
!     BBAR1  =  BBAR + 1
!     BBBAR1 =  B+BBAR - 1
!     SCRFLG =  NONZERO MEANS SPILL
!
!     ****************************************************************
!     RE-WRITE THE UPPER TRIANGLE OF ACTIVE ELEMENTS IN THE TRANSPOSED
!     ORDER
!     ****************************************************************
!
               parm(2) = filea(1)
               CALL open(*60,filea(1),Ix(bufa),rdrew)
               ccount = 0
               IF ( c/=0 ) CALL transp(Ix(1),X(1),nx,filea(1),b,sr1fil)
!
!     ZERO CORE
!
               DO i = 1 , end
                  X(i) = 0.
               ENDDO
               IF ( c==0 ) GOTO 20
!
!     ****************************************************************
!     OPEN THE FILE CONTAINING THE TRANSPOSED ACTIVE ELEMENTS AND READ I
!     THE FIRST BBAR + 1 ROWS
!     ****************************************************************
!
               parm(2) = sr1fil
               CALL open(*60,sr1fil,Ix(sr1buf),rd)
               k = 0
               SPAG_Loop_1_1: DO
                  CALL read(*80,*100,sr1fil,itran(1),4,0,flag)
                  IF ( itrn>0 ) THEN
                     DO WHILE ( itrn>k+1 )
                        k = k + 1
                        IF ( k<bbar1 ) THEN
                        ELSEIF ( k==bbar1 ) THEN
!
!     SET INDEXES IN AREA VII TO POINT TO THE ACTIVE COLUMNS IN SEQUENCE
!
                           ASSIGN 20 TO kk
                           EXIT SPAG_Loop_1_1
                        ELSE
                           parm(1) = -25
                           CALL mesage(parm(1),parm(2),parm(3))
                           RETURN
                        ENDIF
                     ENDDO
!
!     DETERMINE IF COLUMN IS ALREADY ACTIVE
!
                     IF ( jtrn>bbbar ) THEN
                        kk = 0
                        SPAG_Loop_2_2: DO
                           in1 = i3sp + kk
                           IF ( Ix(in1)==jtrn ) THEN
!
!     ADD IN ACTIVE ELEMENT TO EXISTING COLUMN
!
                              in1 = i3 + kk*bbar1 + k
                              Dx(in1) = dtrn
                              EXIT SPAG_Loop_2_2
                           ELSE
                              kk = kk + 1
                              IF ( kk<c ) THEN
                              ELSEIF ( kk==c ) THEN
!
!     CREATE NEW ACTIVE COLUMN
!
                                 ccount = ccount + 1
                                 kk = 0
                                 DO
                                    in1 = i3sp + kk
                                    IF ( Ix(in1)==0 ) THEN
                                       Ix(in1) = jtrn
                                       in1 = in1 + c
                                       Ix(in1) = k + 1
                                       in1 = i3 + kk*bbar1 + k
                                       Dx(in1) = dtrn
                                       EXIT SPAG_Loop_2_2
                                    ELSE
                                       kk = kk + 1
                                       IF ( kk>=c ) THEN
                                         parm(1) = -25
                                         CALL mesage(parm(1),parm(2),parm(3))
                                         RETURN
                                       ENDIF
                                    ENDIF
                                 ENDDO
                              ELSE
                                 parm(1) = -25
                                 CALL mesage(parm(1),parm(2),parm(3))
                                 RETURN
                              ENDIF
                           ENDIF
                        ENDDO SPAG_Loop_2_2
                     ENDIF
                  ELSE
                     CALL close(sr1fil,rew)
                     ASSIGN 20 TO kk
                     EXIT SPAG_Loop_1_1
                  ENDIF
               ENDDO SPAG_Loop_1_1
            ELSE
               imhere = 9
               CALL onetwo(*120,Ix(1),X(1),Dx(1),iterm)
!
!     CALL GENVEC TO PICK B,BBAR,C,CBAR, AND R
!
               RETURN
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         in1 = i7sp
         k = 0
         DO
            in2 = i3sp + k
            IF ( Ix(in2)<0 ) THEN
               parm(1) = -25
               CALL mesage(parm(1),parm(2),parm(3))
               RETURN
            ELSEIF ( Ix(in2)/=0 ) THEN
               IF ( in1/=i7sp ) THEN
                  kkk = 0
                  SPAG_Loop_2_3: DO
                     in3 = in1 - kkk
                     IF ( in3>i7sp ) THEN
                        in4 = i3sp + Ix(in3-1)
                        IF ( Ix(in2)<Ix(in4) ) THEN
                           Ix(in3) = Ix(in3-1)
                           kkk = kkk + 1
                        ELSEIF ( Ix(in2)==Ix(in4) ) THEN
                           parm(1) = -25
                           CALL mesage(parm(1),parm(2),parm(3))
                           RETURN
                        ELSE
                           Ix(in3) = k
                           EXIT SPAG_Loop_2_3
                        ENDIF
                     ELSE
                        Ix(in3) = k
                        EXIT SPAG_Loop_2_3
                     ENDIF
                  ENDDO SPAG_Loop_2_3
               ELSE
                  Ix(in1) = k
               ENDIF
               in1 = in1 + 1
            ENDIF
            k = k + 1
            IF ( k<c ) THEN
            ELSEIF ( k==c ) THEN
               GOTO kk
            ELSE
               parm(1) = -25
               CALL mesage(parm(1),parm(2),parm(3))
               RETURN
            ENDIF
         ENDDO
!
!     INITIALIZE
!
 20      sr2fl = fileu(1)
         sr3fl = sr3fil
         jpos = 1
         parm(2) = filea(1)
         CALL fwdrec(*80,filea(1))
         lcol = 0
         cbcnt = 0
         jposl = 0
         spag_nextblock_1 = 3
      CASE (3)
         IF ( jpos>ncol ) THEN
!
!     FINISH WRITING OUT THE COMPLETED COLUMNS OF L
!
            CALL close(sr1fil,rew)
            CALL close(filel,norew)
            CALL close(sr2fil,norew)
            parm(5) = iend
            CALL conmsg(parm(3),3,0)
            CALL finwrt(iterm,scrflg,sr2fl,jposl,i1sp,bbar,i1,cbcnt,ipak,r,bbbar1,bbbar,i6sp,i4,i4sp,Ix,Dx,X,lcol)
            fileu(7) = bbbar
            RETURN
         ELSE
!****************************************************************
!     READ NEXT COLUMN OF A INTO AREA II
!****************************************************************
            ioff = max0(1,jpos-bbbar1)
            count = cbcnt
            imhere = 275
            CALL intpk(*120,filea(1),0,rdp,0)
            k = 1
            IF ( jpos>bbbar ) k = jpos - b + 1
            DO WHILE ( eol==0 )
               CALL zntpki
               IF ( ii>=k ) THEN
                  k = jpos + bbar
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            spag_nextblock_1 = 5
         ENDIF
      CASE (4)
         IF ( ii>k ) THEN
!
!     TAKE CARE OF ACTIVE ELEMENTS BELOW THE BAND
!
            kk = 0
            SPAG_Loop_1_4: DO
               in1 = i4sp + kk
               IF ( Ix(in1)/=ii ) THEN
                  kk = kk + 1
                  IF ( kk<cbar ) THEN
                  ELSEIF ( kk==cbar ) THEN
!
!     CREATE NEW ACTIVE ROW
!
                     kk = 0
                     DO
                        in1 = i4sp + kk
                        IF ( Ix(in1)==0 ) THEN
                           Ix(in1) = ii
                           in1 = in1 + cbar
                           Ix(in1) = jpos
                           in1 = i4 + (kk+1)*bbbar - 1
                           Dx(in1) = da
                           cbcnt = cbcnt + 1
                           EXIT SPAG_Loop_1_4
                        ELSE
                           kk = kk + 1
                           IF ( kk>=cbar ) THEN
                              parm(1) = -25
                              CALL mesage(parm(1),parm(2),parm(3))
                              RETURN
                           ENDIF
                        ENDIF
                     ENDDO
                  ELSE
                     parm(1) = -25
                     CALL mesage(parm(1),parm(2),parm(3))
                     RETURN
                  ENDIF
               ELSE
!
!     ADD IN ACTIVE ELEMENT TO EXISTING ROW
!
                  in1 = i4 + (kk+1)*bbbar - 1
                  Dx(in1) = da
                  EXIT SPAG_Loop_1_4
               ENDIF
            ENDDO SPAG_Loop_1_4
         ELSE
!
!     READ ELEMENTS WITHIN THE BAND INTO AREA II
!
            in1 = i2 - ioff + ii
            Dx(in1) = da
         ENDIF
         IF ( eol==0 ) THEN
            CALL zntpki
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     ARRANGE ACTIVE ROW INDEXES IN SEQUENCE AND STORE THEM IN AREA VI
!
         IF ( count/=cbcnt ) THEN
            in1 = i6sp
            k = 0
            SPAG_Loop_1_6: DO
               in2 = i4sp + k
               IF ( Ix(in2)<0 ) THEN
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ELSEIF ( Ix(in2)/=0 ) THEN
                  IF ( in1/=i6sp ) THEN
                     kk = 0
                     SPAG_Loop_2_5: DO
                        in3 = in1 - kk
                        IF ( in3>i6sp ) THEN
                           in4 = i4sp + Ix(in3-1)
                           IF ( Ix(in2)<Ix(in4) ) THEN
                              Ix(in3) = Ix(in3-1)
                              kk = kk + 1
                           ELSEIF ( Ix(in2)==Ix(in4) ) THEN
                              parm(1) = -25
                              CALL mesage(parm(1),parm(2),parm(3))
                              RETURN
                           ELSE
                              Ix(in3) = k
                              EXIT SPAG_Loop_2_5
                           ENDIF
                        ELSE
                           Ix(in3) = k
                           EXIT SPAG_Loop_2_5
                        ENDIF
                     ENDDO SPAG_Loop_2_5
                  ELSE
                     Ix(in1) = k
                  ENDIF
                  in1 = in1 + 1
               ENDIF
               k = k + 1
               IF ( k<cbar ) THEN
               ELSEIF ( k==cbar ) THEN
                  EXIT SPAG_Loop_1_6
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_6
         ENDIF
!
!     TEST FOR POSSIBLE MERGING BETWEEN AN INACTIVE-ACTIVE COLUMN AND
!     THE CURRENT PIVOTAL COLUMN
!
         IF ( ccount==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         in1 = Ix(i7sp) + i3sp
         IF ( Ix(in1)<jpos ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            RETURN
         ELSEIF ( Ix(in1)==jpos ) THEN
!
!     MERGE ACTIVE COLUMN AND CURRENT PIVOTAL COLUMN AND ZERO THAT
!     ACTIVE COLUMN IN AREA III
!
            Ix(in1) = 0
            in1 = in1 + c
            Ix(in1) = 0
            in1 = i3 + Ix(i7sp)*bbar1
            ccount = ccount - 1
            kk = 0
            SPAG_Loop_1_7: DO
               in2 = in1 + kk
               in3 = i2 + kk
               Dx(in3) = Dx(in3) + Dx(in2)
               Dx(in2) = 0.D0
               kk = kk + 1
               IF ( kk<bbar1 ) THEN
               ELSEIF ( kk==bbar1 ) THEN
!
!     MERGE INTERACTION ELEMENTS
!
                  IF ( cbcnt==0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  in1 = i5 + Ix(i7sp)*cbar
                  k = 0
                  EXIT SPAG_Loop_1_7
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_7
         ELSE
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_8: DO
            in2 = i4sp + k
            IF ( Ix(in2)/=0 ) THEN
               in3 = in1 + k
               IF ( Dx(in3)/=0.D0 ) THEN
                  IF ( Ix(in2)>jpos+bbar ) THEN
!
!     STORE ELEMENT IN THE ACTIVE ROW
!
                     in2 = i4 + (k+1)*bbbar - 1
                     Dx(in2) = Dx(in2) - Dx(in3)
                     Dx(in3) = 0.D0
                  ELSE
!
!     STORE ELEMENT WITHIN THE LOWER BAND
!
                     in2 = i2 + Ix(in2) - ioff
                     Dx(in2) = Dx(in2) - Dx(in3)
                  ENDIF
                  Dx(in3) = 0.D0
               ENDIF
            ENDIF
            k = k + 1
            IF ( k>=cbar ) THEN
               IF ( k/=cbar ) THEN
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
               EXIT SPAG_Loop_1_8
            ENDIF
         ENDDO SPAG_Loop_1_8
         spag_nextblock_1 = 6
      CASE (6)
!
!     MOVE THE POINTERS IN AREA VII UP ONE
!
         in1 = i7sp + ccount - 1
         DO i = i7sp , in1
            Ix(i) = Ix(i+1)
         ENDDO
         Ix(in1+1) = 0
         spag_nextblock_1 = 7
      CASE (7)
         IF ( lcol==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ****************************************************************
!     OPERATE ON THE CURRENT COLUMN OF A BY ALL PREVIOUS COLUMNS OF L,
!     MAKING NOTED INTERCHANGES AS YOU GO
!     ****************************************************************
!
         IF ( scrflg/=0 ) THEN
            IF ( lcol>=(r-1) ) THEN
               IF ( lcol/=(r-1) ) THEN
                  parm(2) = sr2fl
                  CALL open(*60,sr2fl,Ix(sr2buf),rd)
               ENDIF
               parm(2) = sr3fl
               CALL open(*60,sr3fl,Ix(sr3buf),wrtrew)
            ENDIF
         ENDIF
         ll = 0
         lll = 0
         llll = 0
         spag_nextblock_1 = 8
      CASE (8)
!
!     PICK UP INTERCHANGE INDEX FOR COLUMN JPOSL + LL + 1
!
         in1 = i1sp + ll
         intchn = Ix(in1)
         in2 = i2 + ll
         IF ( intchn/=0 ) THEN
!
!     PERFORM ROW INTERCHANGE
!
            in1 = in2 + intchn
            da = Dx(in1)
            Dx(in1) = Dx(in2)
            Dx(in2) = da
         ENDIF
!
!     COMPUTE THE CONTRIBUTION FROM THAT COLUMN
!
         end = min0(bbar1,ncol-(jposl+ll))
         end = end - 1
         IF ( Dx(in2)/=0 ) THEN
            in1 = i1 + lll*bbar
            CALL dloop(Dx(in2+1),Dx(in1),-Dx(in2),end)
            IF ( cbcnt/=0 ) THEN
!
!     TEST TO SEE IF AN INACTIVE-ACTIVE ROW CONTRIBUTION SHOULD BE
!     ADDED IN
!
               kkk = 0
               SPAG_Loop_1_9: DO
                  in3 = i6sp + kkk
                  in1 = Ix(in3) + i4sp
                  IF ( Ix(in1)>jpos+bbar ) EXIT SPAG_Loop_1_9
                  kk = in1 + cbar
                  IF ( Ix(kk)<=jposl+ll+1 ) THEN
                     IF ( Ix(in1)-jposl-bbar1>ll ) THEN
!
!     ADD IN EFFECT OF THE INACTIVE-ACTIVE ROW
!
                        in4 = i2 + Ix(in1) - ioff
                        k = jposl + bbbar - jpos + ll + i4 + Ix(in3)*bbbar
                        Dx(in4) = Dx(in4) - Dx(k)*Dx(in2)
                     ENDIF
                  ENDIF
                  kkk = kkk + 1
                  IF ( kkk>=cbcnt ) EXIT SPAG_Loop_1_9
               ENDDO SPAG_Loop_1_9
            ENDIF
         ENDIF
         ll = ll + 1
         lll = lll + 1
         IF ( ll==lcol ) THEN
!
!     COMPUTE ELEMENTS FOR THE ACTIVE ROWS
!
            IF ( cbcnt==0 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = 0
         ELSE
            IF ( ll-r+1<0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ll-r+1==0 ) THEN
               IF ( r==bbbar1 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               in1 = i1 + ll*bbar
            ELSE
               in1 = i1 + (lll-1)*bbar
               IF ( ll/=r .OR. lcol/=bbbar1 ) CALL write(sr3fl,Dx(in1),2*bbar,0)
               lll = lll - 1
            ENDIF
            icrq = in1 + bbar*2 - 1 - sr3buf
            IF ( icrq>0 ) THEN
               spag_nextblock_1 = 23
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ibbar2 = bbar*2
            CALL read(*80,*100,sr2fl,Dx(in1),ibbar2,0,flag)
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         in1 = i4sp + k
         IF ( Ix(in1)>jpos+bbar ) THEN
            in1 = in1 + cbar
            IF ( Ix(in1)/=jpos ) THEN
               kkk = max0(0,bbbar-jpos+Ix(in1)-1)
               in2 = i4 + k*bbbar - 1
               in3 = i2 + kkk - 1 - max0(0,bbbar-jpos)
               in1 = in2 + bbbar
               in2 = in2 + kkk
               SPAG_Loop_1_10: DO
                  in2 = in2 + 1
                  kkk = kkk + 1
                  in3 = in3 + 1
                  Dx(in1) = Dx(in1) - Dx(in2)*Dx(in3)
                  IF ( kkk<bbbar1 ) THEN
                  ELSEIF ( kkk==bbbar1 ) THEN
                     EXIT SPAG_Loop_1_10
                  ELSE
                     parm(1) = -25
                     CALL mesage(parm(1),parm(2),parm(3))
                     RETURN
                  ENDIF
               ENDDO SPAG_Loop_1_10
            ENDIF
         ENDIF
         k = k + 1
         IF ( k<cbar ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k/=cbar ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            RETURN
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
!
!     SEARCH THE LOWER BAND FOR THE MAXIMUM ELEMENT AND INTERCHANGE
!     ROWS TO BRING IT TO THE DIAGONAL
!
         k = 1
         in1 = i2 + jpos - ioff
         max = dabs(Dx(in1))
         end = min0(bbar1,ncol-jpos+1)
         intchn = 0
         IF ( end/=1 ) THEN
            SPAG_Loop_1_11: DO
               in2 = in1 + k
               IF ( dabs(Dx(in2))>max ) THEN
                  max = dabs(Dx(in2))
                  intchn = k
               ENDIF
               k = k + 1
               IF ( k<end ) THEN
               ELSEIF ( k==end ) THEN
                  EXIT SPAG_Loop_1_11
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_11
         ENDIF
!
         IF ( intchn/=0 ) THEN
!
!     INTERCHANGE ROWS IN AREA II
!
            det = -det
!
            max = Dx(in1)
            in2 = in1 + intchn
            Dx(in1) = Dx(in2)
            Dx(in2) = max
!
!     STORE THE PERMUTATION INDEX
!
            in2 = i1sp + lcol
            Ix(in2) = intchn
         ENDIF
!
!     DIVIDE THE LOWER BAND BY THE DIAGONAL ELEMENT
!
         imhere = 870
         IF ( Dx(in1)==0.D0 ) GOTO 120
         max = 1.D0/Dx(in1)
         mindia = dmin1(dabs(Dx(in1)),mindia)
         DO WHILE ( dabs(det)>10.D0 )
            det = det/10.D0
            power = power + 1
         ENDDO
         DO WHILE ( dabs(det)<.1D0 )
            det = det*10.D0
            power = power - 1
         ENDDO
         det = det*Dx(in1)
         k = 1
         end = min0(bbar1,ncol-jpos+1)
         IF ( end/=1 ) THEN
            SPAG_Loop_1_12: DO
               in2 = in1 + k
               Dx(in2) = Dx(in2)*max
               k = k + 1
               IF ( k<end ) THEN
               ELSEIF ( k==end ) THEN
                  EXIT SPAG_Loop_1_12
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_12
         ENDIF
         IF ( cbcnt/=0 ) THEN
!
!     DIVIDE THE ACTIVE ROWS BY THE DIAGONAL
!
            k = 0
            in1 = i4 + bbbar1
            SPAG_Loop_1_13: DO
               Dx(in1) = Dx(in1)*max
               in1 = in1 + bbbar
               k = k + 1
               IF ( k<cbar ) THEN
               ELSEIF ( k==cbar ) THEN
                  EXIT SPAG_Loop_1_13
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_13
         ENDIF
!
!     INTERCHANGE ACTIVE COLUMNS AND ADD IN EFFECT OF THE COLUMN OF L
!     ABOUT TO BE WRITTEN OUT
!
         IF ( ccount==0 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jpos<bbbar ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         intch = Ix(i1sp)
         k = 0
         spag_nextblock_1 = 11
      CASE (11)
         in1 = i3sp + k
         IF ( intch/=0 ) THEN
            in1 = i3 + k*bbar1
            in2 = in1 + intch
            da = Dx(in1)
            Dx(in1) = Dx(in2)
            Dx(in2) = da
         ENDIF
         kk = 1
         in2 = i1 - 1
         in1 = i3 + k*bbar1
         IF ( Dx(in1)/=0.D0 ) THEN
            SPAG_Loop_1_14: DO
               in3 = in1 + kk
               in4 = in2 + kk
               Dx(in3) = Dx(in3) - Dx(in1)*Dx(in4)
               kk = kk + 1
               IF ( kk<bbar1 ) THEN
               ELSEIF ( kk==bbar1 ) THEN
                  EXIT SPAG_Loop_1_14
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_14
         ENDIF
         k = k + 1
         IF ( k<c ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k/=c ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            RETURN
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     WRITE OUT THE NEXT COLUMN OF U AND THE ROW OF ACTIVE ELEMENTS
!
         parm(2) = sr2fil
         CALL bldpk(rdp,typel,sr2fil,0,0)
         in1 = i2
         jj = ioff
         imhere = 1030
         SPAG_Loop_1_15: DO
            dz = Dx(in1)
            IF ( dz/=0 ) CALL zblpki
            in1 = in1 + 1
            jj = jj + 1
            IF ( jj>jpos ) THEN
               IF ( Dx(in1-1)==0 ) GOTO 120
!
!     PACK ACTIVE COLUMN ELEMENTS ALSO
!
               IF ( ccount==0 ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( jpos<bbbar ) THEN
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = 0
               EXIT SPAG_Loop_1_15
            ENDIF
         ENDDO SPAG_Loop_1_15
         SPAG_Loop_1_16: DO
            in1 = i7sp + k
            in2 = Ix(in1) + i3sp
            in3 = i3 + Ix(in1)*bbar1
            dz = Dx(in3)
            IF ( dz/=0.D0 ) THEN
               jj = Ix(in2)
               CALL zblpki
            ENDIF
            k = k + 1
            IF ( k<ccount ) THEN
            ELSEIF ( k==ccount ) THEN
               EXIT SPAG_Loop_1_16
            ELSE
               parm(1) = -25
               CALL mesage(parm(1),parm(2),parm(3))
               RETURN
            ENDIF
         ENDDO SPAG_Loop_1_16
         spag_nextblock_1 = 13
      CASE (13)
         CALL bldpkn(sr2fil,0,fileu)
!
!     COMPUTE ACTIVE ROW-COLUMN INTERACTION
!
         IF ( ccount==0 .OR. cbcnt==0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jpos<bbbar ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = 0
         spag_nextblock_1 = 14
      CASE (14)
         in1 = i3 + k*bbar1
         IF ( Dx(in1)/=0.D0 ) THEN
            kk = 0
            SPAG_Loop_1_17: DO
               in2 = i4sp + kk
               in2 = i4 + kk*bbbar
               IF ( Dx(in2)/=0.D0 ) THEN
                  in3 = i5 + k*cbar + kk
                  Dx(in3) = Dx(in3) + Dx(in2)*Dx(in1)
               ENDIF
               kk = kk + 1
               IF ( kk<cbar ) THEN
               ELSEIF ( kk==cbar ) THEN
                  EXIT SPAG_Loop_1_17
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_17
         ENDIF
         k = k + 1
         IF ( k<c ) THEN
            spag_nextblock_1 = 14
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k/=c ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            RETURN
         ENDIF
         spag_nextblock_1 = 15
      CASE (15)
!
!     MOVE ELEMENTS IN AREA III UP ONE CELL
!
         IF ( ccount==0 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( jpos<bbbar ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = 0
         spag_nextblock_1 = 16
      CASE (16)
         in1 = i3sp + k
         IF ( Ix(in1)/=0 ) THEN
            kk = 0
            in1 = i3 + k*(bbar1)
            SPAG_Loop_1_18: DO
               in2 = in1 + kk
               Dx(in2) = Dx(in2+1)
               kk = kk + 1
               IF ( kk<bbar ) THEN
               ELSEIF ( kk==bbar ) THEN
                  Dx(in2+1) = 0.D0
                  EXIT SPAG_Loop_1_18
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_18
         ENDIF
         k = k + 1
         IF ( k<c ) THEN
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k/=c ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            RETURN
         ENDIF
         spag_nextblock_1 = 17
      CASE (17)
!
!     DETERMINE IF A COLUMN OF L CAN BE WRITTEN OUT
!
         IF ( lcol<bbbar1 ) THEN
            spag_nextblock_1 = 19
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     OUTPUT A COLUMN OF L
!
         parm(2) = filel(1)
         jposl = jposl + 1
         CALL bldpk(rdp,typel,filel(1),0,0)
!
!     STORE THE PERMUTATION INDEX AS THE DIAGONAL ELEMENT
!
         jj = jposl
         dz = Ix(i1sp)
         CALL zblpki
         k = 0
         SPAG_Loop_1_19: DO
            jj = jposl + k + 1
            in2 = i1 + k
            dz = Dx(in2)
            IF ( dz/=0 ) CALL zblpki
            k = k + 1
            IF ( k<bbar ) THEN
            ELSEIF ( k==bbar ) THEN
!
!     PACK ACTIVE ROW ELEMENTS ALSO
!
               IF ( cbcnt==0 ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               k = 0
               EXIT SPAG_Loop_1_19
            ELSE
               parm(1) = -25
               CALL mesage(parm(1),parm(2),parm(3))
               RETURN
            ENDIF
         ENDDO SPAG_Loop_1_19
         SPAG_Loop_1_20: DO
            in1 = i6sp + k
            in2 = i4 + Ix(in1)*bbbar
            in1 = Ix(in1) + i4sp
            jj = Ix(in1)
            dz = Dx(in2)
            IF ( dz/=0.D0 ) CALL zblpki
            k = k + 1
            IF ( k<cbcnt ) THEN
            ELSEIF ( k==cbcnt ) THEN
               EXIT SPAG_Loop_1_20
            ELSE
               parm(1) = -25
               CALL mesage(parm(1),parm(2),parm(3))
               RETURN
            ENDIF
         ENDDO SPAG_Loop_1_20
         spag_nextblock_1 = 18
      CASE (18)
         CALL bldpkn(filel,0,filel)
!
!     MOVE PERMUTATION INDICES OVER ONE ELEMENT
!
         end = i1sp + lcol
         DO i = i1sp , end
            Ix(i) = Ix(i+1)
         ENDDO
!
!     MOVE ELEMENTS IN AREA I OVER ONE COLUMN
!
         k = 0
         IF ( scrflg/=0 ) THEN
            CALL close(sr2fl,rew)
            IF ( r<=2 ) THEN
               icrq = i1 + bbar*2 - 1 - sr3buf
               IF ( icrq>0 ) THEN
                  spag_nextblock_1 = 23
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL open(*60,sr2fl,Ix(sr2buf),rd)
               ibbar2 = 2*bbar
               CALL read(*80,*100,sr2fl,Dx(i1),ibbar2,0,flag)
               lcol = lcol - 1
               spag_nextblock_1 = 19
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         SPAG_Loop_1_21: DO
            in1 = i1 + k*bbar
            in2 = in1 + bbar
            CALL xloop(Dx(in1),Dx(in2),bbar)
            k = k + 1
            IF ( k-r+2<0 ) THEN
            ELSEIF ( k-r+2==0 ) THEN
               IF ( r<bbbar1 ) THEN
                  icrq = in2 + bbar*2 - 1 - sr3buf
                  IF ( icrq>0 ) THEN
                     spag_nextblock_1 = 23
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL open(*60,sr2fl,Ix(sr2buf),rd)
                  ibbar2 = bbar*2
                  CALL read(*80,*100,sr2fl,Dx(in2),ibbar2,0,flag)
                  lcol = lcol - 1
                  EXIT SPAG_Loop_1_21
               ELSEIF ( r/=bbbar1 ) THEN
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ELSE
               lcol = lcol - 1
               EXIT SPAG_Loop_1_21
            ENDIF
         ENDDO SPAG_Loop_1_21
         spag_nextblock_1 = 19
      CASE (19)
!
!     STORE CURRENT COLUMN OF L
!
         IF ( cbcnt==0 ) THEN
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     MOVE ELEMENTS IN AREA IV UP ONE CELL
!
         k = 0
         spag_nextblock_1 = 20
      CASE (20)
         in1 = i4sp + k
         IF ( Ix(in1)/=0 ) THEN
            kk = 0
            in1 = i4 + k*bbbar
            SPAG_Loop_1_22: DO
               in2 = in1 + kk
               Dx(in2) = Dx(in2+1)
               kk = kk + 1
               IF ( kk<bbbar1 ) THEN
               ELSEIF ( kk==bbbar1 ) THEN
                  Dx(in2+1) = 0.D0
                  EXIT SPAG_Loop_1_22
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_22
         ENDIF
         k = k + 1
         IF ( k<cbar ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k/=cbar ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            RETURN
         ENDIF
         spag_nextblock_1 = 21
      CASE (21)
         IF ( scrflg/=0 ) THEN
!
!     STORE COLUMN ON THE SCRATCH FILE
!
            IF ( lcol-r+1>=0 ) THEN
               IF ( lcol-r+1/=0 ) THEN
                  in1 = i1 + (lll-1)*bbar
                  CALL write(sr3fl,Dx(in1),bbar*2,0)
               ENDIF
               in1 = i2 + jpos - ioff + 1
               CALL write(sr3fl,Dx(in1),bbar*2,0)
!
!     CLOSE SCRATCH FILES AND SWITCH THE POINTERS TO THEM
!
               CALL close(sr3fl,rew)
               CALL close(sr2fl,rew)
               in1 = sr2fl
               sr2fl = sr3fl
               sr3fl = in1
               spag_nextblock_1 = 22
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     STORE COLUMN IN CORE
!
         in1 = i1 + lcol*bbar
         end = min0(bbar,ncol-jpos)
         IF ( end/=0 ) THEN
            k = 0
            in3 = i2 + jpos - ioff + 1
            SPAG_Loop_1_23: DO
               in2 = in1 + k
               in4 = in3 + k
               Dx(in2) = Dx(in4)
               k = k + 1
               IF ( k<end ) THEN
               ELSEIF ( k==end ) THEN
                  EXIT SPAG_Loop_1_23
               ELSE
                  parm(1) = -25
                  CALL mesage(parm(1),parm(2),parm(3))
                  RETURN
               ENDIF
            ENDDO SPAG_Loop_1_23
         ENDIF
         spag_nextblock_1 = 22
      CASE (22)
         lcol = lcol + 1
         IF ( c/=0 ) THEN
            IF ( jpos>=bbbar ) THEN
!
!     READ IN THE NEXT ROW OF ACTIVE COLUMN ELEMENTS
!
               count = ccount
               IF ( itrn>=0 ) THEN
                  SPAG_Loop_1_25: DO WHILE ( itrn<=jpos-b+2 )
!
!     TEST TO SEE IF COLUMN IS ALREADY ACTIVE
!
                     k = 0
                     SPAG_Loop_2_24: DO
                        in1 = i3sp + k
                        IF ( Ix(in1)==jtrn ) THEN
!
!     STORE ELEMENT IN EXISTING COLUMN
!
                           in1 = i3 + (k+1)*bbar1 - 1
                           Dx(in1) = Dx(in1) + dtrn
                           EXIT SPAG_Loop_2_24
                        ELSE
                           k = k + 1
                           IF ( k<c ) THEN
                           ELSEIF ( k==c ) THEN
!
!     CREATE A NEW ACTIVE COLUMN
!
                              k = 0
                              DO
                                 in1 = i3sp + k
                                 IF ( Ix(in1)==0 ) THEN
                                    Ix(in1) = jtrn
                                    in1 = in1 + c
                                    Ix(in1) = itrn
                                    in1 = i3 + (k+1)*bbar1 - 1
                                    Dx(in1) = dtrn
                                    ccount = ccount + 1
                                    EXIT SPAG_Loop_2_24
                                 ELSE
                                    k = k + 1
                                    IF ( k>=c ) THEN
                                       parm(1) = -25
                                       CALL mesage(parm(1),parm(2),parm(3))
                                       RETURN
                                    ENDIF
                                 ENDIF
                              ENDDO
                           ELSE
                              parm(1) = -25
                              CALL mesage(parm(1),parm(2),parm(3))
                              RETURN
                           ENDIF
                        ENDIF
                     ENDDO SPAG_Loop_2_24
                     CALL read(*80,*100,sr1fil,itran,4,0,flag)
                     IF ( itrn<=0 ) THEN
                        CALL close(sr1fil,rew)
                        EXIT SPAG_Loop_1_25
                     ENDIF
                  ENDDO SPAG_Loop_1_25
                  IF ( ccount/=count ) THEN
!
!     RE-ARRANGE INDEXES IN SEQUENTIAL ORDER
!
                     ASSIGN 40 TO kk
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
 40      jpos = jpos + 1
!
!     ZERO AREA II
!
         end = i2 + min0(jpos-ioff+bbar-1,ncol-1)
         DO i = i2 , end
            Dx(i) = 0.D0
         ENDDO
!
!      TEST TO SEE IF ROW INTERACTION ELEMENTS WILL MERGE INTO AREA III
!
         IF ( cbcnt/=0 ) THEN
            IF ( ccount/=0 ) THEN
               IF ( jpos-1<bbbar ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               in1 = i4sp
               k = 0
               SPAG_Loop_1_26: DO
                  in2 = in1 + k
                  IF ( Ix(in2)==jpos-b+1 ) THEN
                     in1 = i5 + k
                     in2 = i3 + bbar
                     k = 0
                     DO
                        Dx(in2) = Dx(in2) - Dx(in1)
                        Dx(in1) = 0.D0
                        in2 = in2 + bbar1
                        in1 = in1 + cbar
                        k = k + 1
                        IF ( k>=c ) EXIT SPAG_Loop_1_26
                     ENDDO
                  ELSE
                     k = k + 1
                     IF ( k>=cbar ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_26
            ENDIF
!
!      TEST TO SEE IF ACTIVE ROW HAS BEEN ELIMINATED
!
            in1 = Ix(i6sp) + i4sp
            IF ( Ix(in1)-jposl==bbar1 ) THEN
!
!     ELIMINATE THE ACTIVE ROW
!
               Ix(in1) = 0
               in1 = in1 + cbar
               Ix(in1) = 0
               cbcnt = cbcnt - 1
!
!     MOVE INDEXES IN AREA VI UP ONE
!
               in1 = i6sp + cbcnt - 1
               DO i = i6sp , in1
                  Ix(i) = Ix(i+1)
               ENDDO
               Ix(in1+1) = 0
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (23)
         parm(1) = -8
         parm(2) = icrq
         CALL mesage(parm(1),parm(2),parm(3))
         RETURN
 60      parm(1) = -1
         CALL mesage(parm(1),parm(2),parm(3))
         RETURN
 80      parm(1) = -2
         CALL mesage(parm(1),parm(2),parm(3))
         RETURN
 100     parm(1) = -3
         CALL mesage(parm(1),parm(2),parm(3))
         RETURN
!
!     SINGULAR MATRIX - CLOSE ALL FILES AND RETURN TO USER
!
 120     CALL close(filea(1),rew)
         CALL close(filel(1),rew)
         CALL close(fileu(1),rew)
         CALL close(sr1fil,rew)
         CALL close(sr2fil,rew)
         CALL close(sr3fil,rew)
         WRITE (nout,99002) imhere
99002    FORMAT (/60X,'DECOMP/IMHERE@',I5)
!WKBA 4/95 SPR94018
         fileu(7) = bbbar
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE decomp
