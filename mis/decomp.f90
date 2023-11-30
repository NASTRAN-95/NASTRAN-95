
SUBROUTINE decomp(*,Ix,X,Dx)
   IMPLICIT NONE
   REAL A(4) , Cdp , Csp , Diag , Eofnrw , Rd , Rdrew , Rect , Rew , Row , Rsp , Uprtri , Wrt , Wrtrew , Z(4)
   INTEGER B , Bbar , C , Cbar , Eol , Filea(7) , Filel(7) , Fileu(7) , Forma , Ident , Ii , Incrx , Incry , Itype1 , Itype2 ,      &
         & Itypex , Ixy , Iy , Jj , Jxy , Jy , Lowtri , Ncol , Norew , Nout , Nx , Power , R , Rdp , Sqr , Sr1fil , Sr2fil ,        &
         & Sr3fil , Sym , Sysbuf , Typea , Typel
   DOUBLE PRECISION Da , Det , Dz , Mindia
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /dcompx/ Filea , Filel , Fileu , Sr1fil , Sr2fil , Sr3fil , Det , Power , Nx , Mindia , B , Bbar , C , Cbar , R
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Ident
   COMMON /packx / Itype1 , Itype2 , Iy , Jy , Incry
   COMMON /system/ Sysbuf , Nout
   COMMON /unpakx/ Itypex , Ixy , Jxy , Incrx
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zblpkx/ Z , Jj
   COMMON /zntpkx/ A , Ii , Eol
   DOUBLE PRECISION Dx(1)
   INTEGER Ix(1)
   REAL X(1)
   INTEGER bbar1 , bbbar , bbbar1 , bufa , cbcnt , ccount , count , end , flag , i , i1 , i1sp , i2 , i3 , i3sp , i4 , i4sp , i5 ,  &
         & i6sp , i7sp , ibbar2 , ibegn , ibufl , icrq , iend , imhere , in1 , in2 , in3 , in4 , intch , intchn , ioff , ipak ,     &
         & iterm , itran(4) , itrn , jpos , jposl , jtrn , k , kk , kkk , lcol , ll , lll , llll , outbuf , parm(5) , scrflg ,      &
         & sr1buf , sr2buf , sr2fl , sr3buf , sr3fl
   DOUBLE PRECISION dtrn , max
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
!
!     AT LAST, THE START OF THE PROGRAM
!
   IF ( (Forma/=Sqr .AND. Forma/=Sym) .OR. Typea>Rdp ) THEN
!
!     ERROR EXITS
!
      parm(1) = -7
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ELSE
!
!     BUFFER ALLOCATION
!
      bufa = Nx - Sysbuf
      ibufl = bufa - Sysbuf
      outbuf = ibufl - Sysbuf
      sr1buf = outbuf - Sysbuf
      sr2buf = sr1buf - Sysbuf
      sr3buf = sr2buf - Sysbuf
      icrq = -sr3buf
      IF ( icrq>0 ) GOTO 3100
      Det = 1.D0
      Power = 0
      Mindia = 1.D+25
      iterm = 0
      IF ( Filea(1)<0 ) iterm = 1
      Filea(1) = iabs(Filea(1))
!
!     WRITE THE HEADER RECORD ON THE OUTPUT TAPES AND INITIALIZE THE
!     TRAILER RECORDS.
!
      CALL gopen(Filel,Ix(ibufl),Wrtrew)
      parm(2) = Sr2fil
      CALL open(*3200,Sr2fil,Ix(outbuf),Wrtrew)
      CALL fname(Fileu(1),X(1))
      CALL write(Sr2fil,X(1),2,1)
      Filel(3) = Ncol
      Filel(4) = 4
      Filel(2) = 0
      Filel(6) = 0
      Filel(7) = 0
      Fileu(2) = 0
      Fileu(3) = Ncol
      Fileu(4) = 5
      Fileu(6) = 0
      Fileu(7) = 0
      Filea(5) = 2
      IF ( Ncol>2 ) THEN
         IF ( B<=0 .OR. Bbar<=0 ) THEN
            imhere = 10
            CALL genvec(*3500,Ix(bufa),Filea(1),Nx,Ix(1),Ncol,B,Bbar,C,Cbar,R,1)
         ENDIF
         bbar1 = Bbar + 1
         bbbar = min0(B+Bbar,Ncol)
         bbbar1 = bbbar - 1
         scrflg = 0
         IF ( R<bbbar1 ) scrflg = 1
         IF ( scrflg/=0 ) THEN
            icrq = (bbbar1-R)*2*Bbar
            CALL page2(3)
            WRITE (Nout,99001) Uim , icrq
99001       FORMAT (A29,' 2177, SPILL WILL OCCUR IN UNSYMMETRIC DECOMPOSITION',/,I10,                                               &
                   &' ADDITIONAL MEMORY WORDS NEEDED TO STAY IN CORE.')
         ENDIF
!
!     INITIALIZE POINTERS TO SPECIFIC AREAS OF CORE
!
         i1 = 1
         i1sp = (i1+Bbar*R)*2 - 1
         ipak = i1 + Bbar*R + bbbar/2 + 1
         i2 = ipak
         i3sp = (i2+min0(Ncol,bbbar+Bbar))*2 - 1
         i3 = i2 + min0(Ncol,bbbar+Bbar) + C
         i4sp = i3sp + (Bbar+2)*C*2
         i4 = i3 + bbar1*C + Cbar
         i5 = i4 + bbbar*Cbar
         i6sp = (i5+C*Cbar)*2 - 1
         i7sp = i6sp + Cbar
         end = i7sp + C
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
         parm(2) = Filea(1)
         CALL open(*3200,Filea(1),Ix(bufa),Rdrew)
         ccount = 0
         IF ( C/=0 ) CALL transp(Ix(1),X(1),Nx,Filea(1),B,Sr1fil)
!
!     ZERO CORE
!
         DO i = 1 , end
            X(i) = 0.
         ENDDO
         IF ( C==0 ) GOTO 200
!
!     ****************************************************************
!     OPEN THE FILE CONTAINING THE TRANSPOSED ACTIVE ELEMENTS AND READ I
!     THE FIRST BBAR + 1 ROWS
!     ****************************************************************
!
         parm(2) = Sr1fil
         CALL open(*3200,Sr1fil,Ix(sr1buf),Rd)
         k = 0
         DO
            CALL read(*3300,*3400,Sr1fil,itran(1),4,0,flag)
            IF ( itrn>0 ) THEN
               DO WHILE ( itrn>k+1 )
                  k = k + 1
                  IF ( k<bbar1 ) THEN
                  ELSEIF ( k==bbar1 ) THEN
!
!     SET INDEXES IN AREA VII TO POINT TO THE ACTIVE COLUMNS IN SEQUENCE
!
                     ASSIGN 200 TO kk
                     GOTO 100
                  ELSE
                     parm(1) = -25
                     CALL mesage(parm(1),parm(2),parm(3))
                     GOTO 99999
                  ENDIF
               ENDDO
!
!     DETERMINE IF COLUMN IS ALREADY ACTIVE
!
               IF ( jtrn>bbbar ) THEN
                  kk = 0
                  DO
                     in1 = i3sp + kk
                     IF ( Ix(in1)==jtrn ) THEN
!
!     ADD IN ACTIVE ELEMENT TO EXISTING COLUMN
!
                        in1 = i3 + kk*bbar1 + k
                        Dx(in1) = dtrn
                        EXIT
                     ELSE
                        kk = kk + 1
                        IF ( kk<C ) THEN
                        ELSEIF ( kk==C ) THEN
!
!     CREATE NEW ACTIVE COLUMN
!
                           ccount = ccount + 1
                           kk = 0
                           DO
                              in1 = i3sp + kk
                              IF ( Ix(in1)==0 ) THEN
                                 Ix(in1) = jtrn
                                 in1 = in1 + C
                                 Ix(in1) = k + 1
                                 in1 = i3 + kk*bbar1 + k
                                 Dx(in1) = dtrn
                                 GOTO 20
                              ELSE
                                 kk = kk + 1
                                 IF ( kk>=C ) THEN
                                    parm(1) = -25
                                    CALL mesage(parm(1),parm(2),parm(3))
                                    GOTO 99999
                                 ENDIF
                              ENDIF
                           ENDDO
                        ELSE
                           parm(1) = -25
                           CALL mesage(parm(1),parm(2),parm(3))
                           GOTO 99999
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ELSE
               CALL close(Sr1fil,Rew)
               ASSIGN 200 TO kk
               EXIT
            ENDIF
 20      ENDDO
      ELSE
         imhere = 9
         CALL onetwo(*3500,Ix(1),X(1),Dx(1),iterm)
!
!     CALL GENVEC TO PICK B,BBAR,C,CBAR, AND R
!
         RETURN
      ENDIF
   ENDIF
 100  in1 = i7sp
   k = 0
   DO
      in2 = i3sp + k
      IF ( Ix(in2)<0 ) THEN
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ELSEIF ( Ix(in2)/=0 ) THEN
         IF ( in1/=i7sp ) THEN
            kkk = 0
            DO
               in3 = in1 - kkk
               IF ( in3>i7sp ) THEN
                  in4 = i3sp + Ix(in3-1)
                  IF ( Ix(in2)<Ix(in4) ) THEN
                     Ix(in3) = Ix(in3-1)
                     kkk = kkk + 1
                  ELSEIF ( Ix(in2)==Ix(in4) ) THEN
                     parm(1) = -25
                     CALL mesage(parm(1),parm(2),parm(3))
                     GOTO 99999
                  ELSE
                     Ix(in3) = k
                     EXIT
                  ENDIF
               ELSE
                  Ix(in3) = k
                  EXIT
               ENDIF
            ENDDO
         ELSE
            Ix(in1) = k
         ENDIF
         in1 = in1 + 1
      ENDIF
      k = k + 1
      IF ( k<C ) THEN
      ELSEIF ( k==C ) THEN
         GOTO kk
      ELSE
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ENDIF
   ENDDO
!
!     INITIALIZE
!
 200  sr2fl = Fileu(1)
   sr3fl = Sr3fil
   jpos = 1
   parm(2) = Filea(1)
   CALL fwdrec(*3300,Filea(1))
   lcol = 0
   cbcnt = 0
   jposl = 0
 300  IF ( jpos>Ncol ) THEN
!
!     FINISH WRITING OUT THE COMPLETED COLUMNS OF L
!
      CALL close(Sr1fil,Rew)
      CALL close(Filel,Norew)
      CALL close(Sr2fil,Norew)
      parm(5) = iend
      CALL conmsg(parm(3),3,0)
      CALL finwrt(iterm,scrflg,sr2fl,jposl,i1sp,Bbar,i1,cbcnt,ipak,R,bbbar1,bbbar,i6sp,i4,i4sp,Ix,Dx,X,lcol)
      Fileu(7) = bbbar
      RETURN
   ELSE
!****************************************************************
!     READ NEXT COLUMN OF A INTO AREA II
!****************************************************************
      ioff = max0(1,jpos-bbbar1)
      count = cbcnt
      imhere = 275
      CALL intpk(*3500,Filea(1),0,Rdp,0)
      k = 1
      IF ( jpos>bbbar ) k = jpos - B + 1
      DO WHILE ( Eol==0 )
         CALL zntpki
         IF ( Ii>=k ) THEN
            k = jpos + Bbar
            GOTO 400
         ENDIF
      ENDDO
      GOTO 600
   ENDIF
 400  IF ( Ii>k ) THEN
!
!     TAKE CARE OF ACTIVE ELEMENTS BELOW THE BAND
!
      kk = 0
      DO
         in1 = i4sp + kk
         IF ( Ix(in1)/=Ii ) THEN
            kk = kk + 1
            IF ( kk<Cbar ) THEN
            ELSEIF ( kk==Cbar ) THEN
!
!     CREATE NEW ACTIVE ROW
!
               kk = 0
               DO
                  in1 = i4sp + kk
                  IF ( Ix(in1)==0 ) THEN
                     Ix(in1) = Ii
                     in1 = in1 + Cbar
                     Ix(in1) = jpos
                     in1 = i4 + (kk+1)*bbbar - 1
                     Dx(in1) = Da
                     cbcnt = cbcnt + 1
                     GOTO 500
                  ELSE
                     kk = kk + 1
                     IF ( kk>=Cbar ) THEN
                        parm(1) = -25
                        CALL mesage(parm(1),parm(2),parm(3))
                        GOTO 99999
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               parm(1) = -25
               CALL mesage(parm(1),parm(2),parm(3))
               GOTO 99999
            ENDIF
         ELSE
!
!     ADD IN ACTIVE ELEMENT TO EXISTING ROW
!
            in1 = i4 + (kk+1)*bbbar - 1
            Dx(in1) = Da
            EXIT
         ENDIF
      ENDDO
   ELSE
!
!     READ ELEMENTS WITHIN THE BAND INTO AREA II
!
      in1 = i2 - ioff + Ii
      Dx(in1) = Da
   ENDIF
 500  IF ( Eol==0 ) THEN
      CALL zntpki
      GOTO 400
   ENDIF
!
!     ARRANGE ACTIVE ROW INDEXES IN SEQUENCE AND STORE THEM IN AREA VI
!
 600  IF ( count==cbcnt ) GOTO 700
   in1 = i6sp
   k = 0
   DO
      in2 = i4sp + k
      IF ( Ix(in2)<0 ) THEN
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ELSEIF ( Ix(in2)/=0 ) THEN
         IF ( in1/=i6sp ) THEN
            kk = 0
            DO
               in3 = in1 - kk
               IF ( in3>i6sp ) THEN
                  in4 = i4sp + Ix(in3-1)
                  IF ( Ix(in2)<Ix(in4) ) THEN
                     Ix(in3) = Ix(in3-1)
                     kk = kk + 1
                  ELSEIF ( Ix(in2)==Ix(in4) ) THEN
                     parm(1) = -25
                     CALL mesage(parm(1),parm(2),parm(3))
                     GOTO 99999
                  ELSE
                     Ix(in3) = k
                     EXIT
                  ENDIF
               ELSE
                  Ix(in3) = k
                  EXIT
               ENDIF
            ENDDO
         ELSE
            Ix(in1) = k
         ENDIF
         in1 = in1 + 1
      ENDIF
      k = k + 1
      IF ( k<Cbar ) THEN
      ELSEIF ( k==Cbar ) THEN
         EXIT
      ELSE
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ENDIF
   ENDDO
!
!     TEST FOR POSSIBLE MERGING BETWEEN AN INACTIVE-ACTIVE COLUMN AND
!     THE CURRENT PIVOTAL COLUMN
!
 700  IF ( ccount==0 ) GOTO 1000
   in1 = Ix(i7sp) + i3sp
   IF ( Ix(in1)<jpos ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ELSEIF ( Ix(in1)==jpos ) THEN
!
!     MERGE ACTIVE COLUMN AND CURRENT PIVOTAL COLUMN AND ZERO THAT
!     ACTIVE COLUMN IN AREA III
!
      Ix(in1) = 0
      in1 = in1 + C
      Ix(in1) = 0
      in1 = i3 + Ix(i7sp)*bbar1
      ccount = ccount - 1
      kk = 0
      DO
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
            IF ( cbcnt==0 ) GOTO 900
            in1 = i5 + Ix(i7sp)*Cbar
            k = 0
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ELSE
      GOTO 1000
   ENDIF
 800  in2 = i4sp + k
   IF ( Ix(in2)/=0 ) THEN
      in3 = in1 + k
      IF ( Dx(in3)/=0.D0 ) THEN
         IF ( Ix(in2)>jpos+Bbar ) THEN
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
   IF ( k<Cbar ) GOTO 800
   IF ( k/=Cbar ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     MOVE THE POINTERS IN AREA VII UP ONE
!
 900  in1 = i7sp + ccount - 1
   DO i = i7sp , in1
      Ix(i) = Ix(i+1)
   ENDDO
   Ix(in1+1) = 0
 1000 IF ( lcol==0 ) GOTO 1400
!
!     ****************************************************************
!     OPERATE ON THE CURRENT COLUMN OF A BY ALL PREVIOUS COLUMNS OF L,
!     MAKING NOTED INTERCHANGES AS YOU GO
!     ****************************************************************
!
   IF ( scrflg/=0 ) THEN
      IF ( lcol<(R-1) ) GOTO 1100
      IF ( lcol/=(R-1) ) THEN
         parm(2) = sr2fl
         CALL open(*3200,sr2fl,Ix(sr2buf),Rd)
      ENDIF
      parm(2) = sr3fl
      CALL open(*3200,sr3fl,Ix(sr3buf),Wrtrew)
   ENDIF
 1100 ll = 0
   lll = 0
   llll = 0
!
!     PICK UP INTERCHANGE INDEX FOR COLUMN JPOSL + LL + 1
!
 1200 in1 = i1sp + ll
   intchn = Ix(in1)
   in2 = i2 + ll
   IF ( intchn/=0 ) THEN
!
!     PERFORM ROW INTERCHANGE
!
      in1 = in2 + intchn
      Da = Dx(in1)
      Dx(in1) = Dx(in2)
      Dx(in2) = Da
   ENDIF
!
!     COMPUTE THE CONTRIBUTION FROM THAT COLUMN
!
   end = min0(bbar1,Ncol-(jposl+ll))
   end = end - 1
   IF ( Dx(in2)/=0 ) THEN
      in1 = i1 + lll*Bbar
      CALL dloop(Dx(in2+1),Dx(in1),-Dx(in2),end)
      IF ( cbcnt/=0 ) THEN
!
!     TEST TO SEE IF AN INACTIVE-ACTIVE ROW CONTRIBUTION SHOULD BE
!     ADDED IN
!
         kkk = 0
         DO
            in3 = i6sp + kkk
            in1 = Ix(in3) + i4sp
            IF ( Ix(in1)>jpos+Bbar ) EXIT
            kk = in1 + Cbar
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
            IF ( kkk>=cbcnt ) EXIT
         ENDDO
      ENDIF
   ENDIF
   ll = ll + 1
   lll = lll + 1
   IF ( ll==lcol ) THEN
!
!     COMPUTE ELEMENTS FOR THE ACTIVE ROWS
!
      IF ( cbcnt==0 ) GOTO 1400
      k = 0
   ELSE
      IF ( ll-R+1<0 ) GOTO 1200
      IF ( ll-R+1==0 ) THEN
         IF ( R==bbbar1 ) GOTO 1200
         in1 = i1 + ll*Bbar
      ELSE
         in1 = i1 + (lll-1)*Bbar
         IF ( ll/=R .OR. lcol/=bbbar1 ) CALL write(sr3fl,Dx(in1),2*Bbar,0)
         lll = lll - 1
      ENDIF
      icrq = in1 + Bbar*2 - 1 - sr3buf
      IF ( icrq>0 ) GOTO 3100
      ibbar2 = Bbar*2
      CALL read(*3300,*3400,sr2fl,Dx(in1),ibbar2,0,flag)
      GOTO 1200
   ENDIF
 1300 in1 = i4sp + k
   IF ( Ix(in1)>jpos+Bbar ) THEN
      in1 = in1 + Cbar
      IF ( Ix(in1)/=jpos ) THEN
         kkk = max0(0,bbbar-jpos+Ix(in1)-1)
         in2 = i4 + k*bbbar - 1
         in3 = i2 + kkk - 1 - max0(0,bbbar-jpos)
         in1 = in2 + bbbar
         in2 = in2 + kkk
         DO
            in2 = in2 + 1
            kkk = kkk + 1
            in3 = in3 + 1
            Dx(in1) = Dx(in1) - Dx(in2)*Dx(in3)
            IF ( kkk<bbbar1 ) THEN
            ELSEIF ( kkk==bbbar1 ) THEN
               EXIT
            ELSE
               parm(1) = -25
               CALL mesage(parm(1),parm(2),parm(3))
               GOTO 99999
            ENDIF
         ENDDO
      ENDIF
   ENDIF
   k = k + 1
   IF ( k<Cbar ) GOTO 1300
   IF ( k/=Cbar ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     SEARCH THE LOWER BAND FOR THE MAXIMUM ELEMENT AND INTERCHANGE
!     ROWS TO BRING IT TO THE DIAGONAL
!
 1400 k = 1
   in1 = i2 + jpos - ioff
   max = dabs(Dx(in1))
   end = min0(bbar1,Ncol-jpos+1)
   intchn = 0
   IF ( end==1 ) GOTO 1500
   DO
      in2 = in1 + k
      IF ( dabs(Dx(in2))>max ) THEN
         max = dabs(Dx(in2))
         intchn = k
      ENDIF
      k = k + 1
      IF ( k<end ) THEN
      ELSEIF ( k==end ) THEN
         EXIT
      ELSE
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ENDIF
   ENDDO
!
 1500 IF ( intchn/=0 ) THEN
!
!     INTERCHANGE ROWS IN AREA II
!
      Det = -Det
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
   IF ( Dx(in1)==0.D0 ) GOTO 3500
   max = 1.D0/Dx(in1)
   Mindia = dmin1(dabs(Dx(in1)),Mindia)
   DO WHILE ( dabs(Det)>10.D0 )
      Det = Det/10.D0
      Power = Power + 1
   ENDDO
   DO WHILE ( dabs(Det)<.1D0 )
      Det = Det*10.D0
      Power = Power - 1
   ENDDO
   Det = Det*Dx(in1)
   k = 1
   end = min0(bbar1,Ncol-jpos+1)
   IF ( end/=1 ) THEN
      DO
         in2 = in1 + k
         Dx(in2) = Dx(in2)*max
         k = k + 1
         IF ( k<end ) THEN
         ELSEIF ( k==end ) THEN
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
   IF ( cbcnt/=0 ) THEN
!
!     DIVIDE THE ACTIVE ROWS BY THE DIAGONAL
!
      k = 0
      in1 = i4 + bbbar1
      DO
         Dx(in1) = Dx(in1)*max
         in1 = in1 + bbbar
         k = k + 1
         IF ( k<Cbar ) THEN
         ELSEIF ( k==Cbar ) THEN
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
!
!     INTERCHANGE ACTIVE COLUMNS AND ADD IN EFFECT OF THE COLUMN OF L
!     ABOUT TO BE WRITTEN OUT
!
   IF ( ccount==0 ) GOTO 1700
   IF ( jpos<bbbar ) GOTO 1700
   intch = Ix(i1sp)
   k = 0
 1600 in1 = i3sp + k
   IF ( intch/=0 ) THEN
      in1 = i3 + k*bbar1
      in2 = in1 + intch
      Da = Dx(in1)
      Dx(in1) = Dx(in2)
      Dx(in2) = Da
   ENDIF
   kk = 1
   in2 = i1 - 1
   in1 = i3 + k*bbar1
   IF ( Dx(in1)/=0.D0 ) THEN
      DO
         in3 = in1 + kk
         in4 = in2 + kk
         Dx(in3) = Dx(in3) - Dx(in1)*Dx(in4)
         kk = kk + 1
         IF ( kk<bbar1 ) THEN
         ELSEIF ( kk==bbar1 ) THEN
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
   k = k + 1
   IF ( k<C ) GOTO 1600
   IF ( k/=C ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     WRITE OUT THE NEXT COLUMN OF U AND THE ROW OF ACTIVE ELEMENTS
!
 1700 parm(2) = Sr2fil
   CALL bldpk(Rdp,Typel,Sr2fil,0,0)
   in1 = i2
   Jj = ioff
   imhere = 1030
   DO
      Dz = Dx(in1)
      IF ( Dz/=0 ) CALL zblpki
      in1 = in1 + 1
      Jj = Jj + 1
      IF ( Jj>jpos ) THEN
         IF ( Dx(in1-1)==0 ) GOTO 3500
!
!     PACK ACTIVE COLUMN ELEMENTS ALSO
!
         IF ( ccount==0 ) GOTO 1800
         IF ( jpos<bbbar ) GOTO 1800
         k = 0
         EXIT
      ENDIF
   ENDDO
   DO
      in1 = i7sp + k
      in2 = Ix(in1) + i3sp
      in3 = i3 + Ix(in1)*bbar1
      Dz = Dx(in3)
      IF ( Dz/=0.D0 ) THEN
         Jj = Ix(in2)
         CALL zblpki
      ENDIF
      k = k + 1
      IF ( k<ccount ) THEN
      ELSEIF ( k==ccount ) THEN
         EXIT
      ELSE
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ENDIF
   ENDDO
 1800 CALL bldpkn(Sr2fil,0,Fileu)
!
!     COMPUTE ACTIVE ROW-COLUMN INTERACTION
!
   IF ( ccount==0 .OR. cbcnt==0 ) GOTO 2100
   IF ( jpos<bbbar ) GOTO 2100
   k = 0
 1900 in1 = i3 + k*bbar1
   IF ( Dx(in1)==0.D0 ) GOTO 2000
   kk = 0
   DO
      in2 = i4sp + kk
      in2 = i4 + kk*bbbar
      IF ( Dx(in2)/=0.D0 ) THEN
         in3 = i5 + k*Cbar + kk
         Dx(in3) = Dx(in3) + Dx(in2)*Dx(in1)
      ENDIF
      kk = kk + 1
      IF ( kk<Cbar ) THEN
      ELSEIF ( kk==Cbar ) THEN
         EXIT
      ELSE
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ENDIF
   ENDDO
 2000 k = k + 1
   IF ( k<C ) GOTO 1900
   IF ( k/=C ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     MOVE ELEMENTS IN AREA III UP ONE CELL
!
 2100 IF ( ccount==0 ) GOTO 2300
   IF ( jpos<bbbar ) GOTO 2300
   k = 0
 2200 in1 = i3sp + k
   IF ( Ix(in1)/=0 ) THEN
      kk = 0
      in1 = i3 + k*(bbar1)
      DO
         in2 = in1 + kk
         Dx(in2) = Dx(in2+1)
         kk = kk + 1
         IF ( kk<Bbar ) THEN
         ELSEIF ( kk==Bbar ) THEN
            Dx(in2+1) = 0.D0
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
   k = k + 1
   IF ( k<C ) GOTO 2200
   IF ( k/=C ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     DETERMINE IF A COLUMN OF L CAN BE WRITTEN OUT
!
 2300 IF ( lcol<bbbar1 ) GOTO 2500
!
!     OUTPUT A COLUMN OF L
!
   parm(2) = Filel(1)
   jposl = jposl + 1
   CALL bldpk(Rdp,Typel,Filel(1),0,0)
!
!     STORE THE PERMUTATION INDEX AS THE DIAGONAL ELEMENT
!
   Jj = jposl
   Dz = Ix(i1sp)
   CALL zblpki
   k = 0
   DO
      Jj = jposl + k + 1
      in2 = i1 + k
      Dz = Dx(in2)
      IF ( Dz/=0 ) CALL zblpki
      k = k + 1
      IF ( k<Bbar ) THEN
      ELSEIF ( k==Bbar ) THEN
!
!     PACK ACTIVE ROW ELEMENTS ALSO
!
         IF ( cbcnt==0 ) GOTO 2400
         k = 0
         EXIT
      ELSE
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ENDIF
   ENDDO
   DO
      in1 = i6sp + k
      in2 = i4 + Ix(in1)*bbbar
      in1 = Ix(in1) + i4sp
      Jj = Ix(in1)
      Dz = Dx(in2)
      IF ( Dz/=0.D0 ) CALL zblpki
      k = k + 1
      IF ( k<cbcnt ) THEN
      ELSEIF ( k==cbcnt ) THEN
         EXIT
      ELSE
         parm(1) = -25
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 99999
      ENDIF
   ENDDO
 2400 CALL bldpkn(Filel,0,Filel)
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
      CALL close(sr2fl,Rew)
      IF ( R<=2 ) THEN
         icrq = i1 + Bbar*2 - 1 - sr3buf
         IF ( icrq>0 ) GOTO 3100
         CALL open(*3200,sr2fl,Ix(sr2buf),Rd)
         ibbar2 = 2*Bbar
         CALL read(*3300,*3400,sr2fl,Dx(i1),ibbar2,0,flag)
         lcol = lcol - 1
         GOTO 2500
      ENDIF
   ENDIF
   DO
      in1 = i1 + k*Bbar
      in2 = in1 + Bbar
      CALL xloop(Dx(in1),Dx(in2),Bbar)
      k = k + 1
      IF ( k-R+2<0 ) THEN
      ELSEIF ( k-R+2==0 ) THEN
         IF ( R<bbbar1 ) THEN
            icrq = in2 + Bbar*2 - 1 - sr3buf
            IF ( icrq>0 ) GOTO 3100
            CALL open(*3200,sr2fl,Ix(sr2buf),Rd)
            ibbar2 = Bbar*2
            CALL read(*3300,*3400,sr2fl,Dx(in2),ibbar2,0,flag)
            lcol = lcol - 1
            EXIT
         ELSEIF ( R/=bbbar1 ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ELSE
         lcol = lcol - 1
         EXIT
      ENDIF
   ENDDO
!
!     STORE CURRENT COLUMN OF L
!
 2500 IF ( cbcnt==0 ) GOTO 2700
!
!     MOVE ELEMENTS IN AREA IV UP ONE CELL
!
   k = 0
 2600 in1 = i4sp + k
   IF ( Ix(in1)/=0 ) THEN
      kk = 0
      in1 = i4 + k*bbbar
      DO
         in2 = in1 + kk
         Dx(in2) = Dx(in2+1)
         kk = kk + 1
         IF ( kk<bbbar1 ) THEN
         ELSEIF ( kk==bbbar1 ) THEN
            Dx(in2+1) = 0.D0
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
   k = k + 1
   IF ( k<Cbar ) GOTO 2600
   IF ( k/=Cbar ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
 2700 IF ( scrflg/=0 ) THEN
!
!     STORE COLUMN ON THE SCRATCH FILE
!
      IF ( lcol-R+1<0 ) GOTO 2800
      IF ( lcol-R+1/=0 ) THEN
         in1 = i1 + (lll-1)*Bbar
         CALL write(sr3fl,Dx(in1),Bbar*2,0)
      ENDIF
      in1 = i2 + jpos - ioff + 1
      CALL write(sr3fl,Dx(in1),Bbar*2,0)
!
!     CLOSE SCRATCH FILES AND SWITCH THE POINTERS TO THEM
!
      CALL close(sr3fl,Rew)
      CALL close(sr2fl,Rew)
      in1 = sr2fl
      sr2fl = sr3fl
      sr3fl = in1
      GOTO 2900
   ENDIF
!
!     STORE COLUMN IN CORE
!
 2800 in1 = i1 + lcol*Bbar
   end = min0(Bbar,Ncol-jpos)
   IF ( end/=0 ) THEN
      k = 0
      in3 = i2 + jpos - ioff + 1
      DO
         in2 = in1 + k
         in4 = in3 + k
         Dx(in2) = Dx(in4)
         k = k + 1
         IF ( k<end ) THEN
         ELSEIF ( k==end ) THEN
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
 2900 lcol = lcol + 1
   IF ( C/=0 ) THEN
      IF ( jpos>=bbbar ) THEN
!
!     READ IN THE NEXT ROW OF ACTIVE COLUMN ELEMENTS
!
         count = ccount
         IF ( itrn>=0 ) THEN
            DO WHILE ( itrn<=jpos-B+2 )
!
!     TEST TO SEE IF COLUMN IS ALREADY ACTIVE
!
               k = 0
               DO
                  in1 = i3sp + k
                  IF ( Ix(in1)==jtrn ) THEN
!
!     STORE ELEMENT IN EXISTING COLUMN
!
                     in1 = i3 + (k+1)*bbar1 - 1
                     Dx(in1) = Dx(in1) + dtrn
                     EXIT
                  ELSE
                     k = k + 1
                     IF ( k<C ) THEN
                     ELSEIF ( k==C ) THEN
!
!     CREATE A NEW ACTIVE COLUMN
!
                        k = 0
                        DO
                           in1 = i3sp + k
                           IF ( Ix(in1)==0 ) THEN
                              Ix(in1) = jtrn
                              in1 = in1 + C
                              Ix(in1) = itrn
                              in1 = i3 + (k+1)*bbar1 - 1
                              Dx(in1) = dtrn
                              ccount = ccount + 1
                              GOTO 2905
                           ELSE
                              k = k + 1
                              IF ( k>=C ) THEN
                                 parm(1) = -25
                                 CALL mesage(parm(1),parm(2),parm(3))
                                 GOTO 99999
                              ENDIF
                           ENDIF
                        ENDDO
                     ELSE
                        parm(1) = -25
                        CALL mesage(parm(1),parm(2),parm(3))
                        GOTO 99999
                     ENDIF
                  ENDIF
               ENDDO
 2905          CALL read(*3300,*3400,Sr1fil,itran,4,0,flag)
               IF ( itrn<=0 ) THEN
                  CALL close(Sr1fil,Rew)
                  EXIT
               ENDIF
            ENDDO
            IF ( ccount/=count ) THEN
!
!     RE-ARRANGE INDEXES IN SEQUENTIAL ORDER
!
               ASSIGN 3000 TO kk
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 3000 jpos = jpos + 1
!
!     ZERO AREA II
!
   end = i2 + min0(jpos-ioff+Bbar-1,Ncol-1)
   DO i = i2 , end
      Dx(i) = 0.D0
   ENDDO
!
!      TEST TO SEE IF ROW INTERACTION ELEMENTS WILL MERGE INTO AREA III
!
   IF ( cbcnt/=0 ) THEN
      IF ( ccount/=0 ) THEN
         IF ( jpos-1<bbbar ) GOTO 300
         in1 = i4sp
         k = 0
         DO
            in2 = in1 + k
            IF ( Ix(in2)==jpos-B+1 ) THEN
               in1 = i5 + k
               in2 = i3 + Bbar
               k = 0
               DO
                  Dx(in2) = Dx(in2) - Dx(in1)
                  Dx(in1) = 0.D0
                  in2 = in2 + bbar1
                  in1 = in1 + Cbar
                  k = k + 1
                  IF ( k>=C ) GOTO 3050
               ENDDO
            ELSE
               k = k + 1
               IF ( k>=Cbar ) GOTO 300
            ENDIF
         ENDDO
      ENDIF
!
!      TEST TO SEE IF ACTIVE ROW HAS BEEN ELIMINATED
!
 3050 in1 = Ix(i6sp) + i4sp
      IF ( Ix(in1)-jposl==bbar1 ) THEN
!
!     ELIMINATE THE ACTIVE ROW
!
         Ix(in1) = 0
         in1 = in1 + Cbar
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
   GOTO 300
 3100 parm(1) = -8
   parm(2) = icrq
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 3200 parm(1) = -1
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 3300 parm(1) = -2
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 3400 parm(1) = -3
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
!
!     SINGULAR MATRIX - CLOSE ALL FILES AND RETURN TO USER
!
 3500 CALL close(Filea(1),Rew)
   CALL close(Filel(1),Rew)
   CALL close(Fileu(1),Rew)
   CALL close(Sr1fil,Rew)
   CALL close(Sr2fil,Rew)
   CALL close(Sr3fil,Rew)
   WRITE (Nout,99002) imhere
99002 FORMAT (/60X,'DECOMP/IMHERE@',I5)
!WKBA 4/95 SPR94018
   Fileu(7) = bbbar
   RETURN 1
99999 RETURN
END SUBROUTINE decomp