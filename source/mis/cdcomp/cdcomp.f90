!*==cdcomp.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cdcomp(Ix,X,Dx) !HIDESTARS (*,Ix,X,Dx)
   IMPLICIT NONE
   USE C_CDCMPX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZBLPKX
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ix
   REAL , DIMENSION(1) :: X
   REAL*8 , DIMENSION(1) :: Dx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bbar1 , bbbar , bbbar1 , bufa , cbcnt , ccount , count , end , flag , forma , i , i1 , i1sp , i2 , i3 , i3sp , i4 ,   &
            & i4sp , i5 , i6sp , i7sp , ibbar4 , ibufl , icrq , in1 , in2 , in3 , in4 , intch , intchn , ioff , ipak , iterm ,      &
            & itrn , jpos , jposl , jtrn , k , kk , kkk , lcol , ll , lll , llll , ncol , outbuf , scrflg , sr1buf , sr2buf ,       &
            & sr2fl , sr3buf , sr3fl , typea , typel
   REAL*8 , DIMENSION(2) :: da , dtrn , dz , max
   REAL*8 :: dx1 , dx2
   INTEGER , SAVE :: ibegn , iend
   INTEGER , DIMENSION(6) :: itran
   REAL*8 , SAVE :: limit
   INTEGER , DIMENSION(5) , SAVE :: parm
!
! End of declarations rewritten by SPAG
!
!
!     CDCOMP WILL DECOMPOSE A COMPLEX UNSYMETRIC MATRIX INTO A UNIT
!     LOWER TRIANGULAR MATRIX AND AN UPPER TRIANGULAR MATRIX,USING
!     PARTIAL PIVOTING WITHIN THE LOWER BAND
!
!     IMPORTANT - CALLER MUST FIRST INITIALIZE B AND/OR BBAR IN /CDCMPX/
!
!     DEFINITION OF INPUT PARAMETERS
!
!     FILEA  =  MATRIX CONTROL BLOCK FOR THE  INPUT MATRIX A
!     FILEL  =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX L
!     FILEU  =  MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX U
!     SR1FIL =  SCRATCH FILE
!     SR2FIL =  SCRATCH FILE
!     SR3FIL =  SCRATCH FILE
!     NX     =  NUMBER OF CELLS OF CORE AVAILABLE AT IX
!     DET    =  CELL WHERE THE DETERMINATE OF A WILL BE STORED
!     POWER  =  SCALE FACTOR TO BE APPLIED TO THE DETERMINATE
!               ( DETERMINATE = DET*10**POWER )
!     MINDIA =  CELL WHERE THE VALUE OF THE MINIMUM DIAGONAL WILL BE
!               STORED
!     IX     =  BLOCK OF CORE AVAILABLE AS WORKING STORAGE TO DECOMP
!     X      =  SAME BLOCK AS IX, BUT TYPED REAL
!     DX     =  SAME BLOCK AS IX, BUT TYPED DOUBLE PRECISION
!
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR
   !>>>>EQUIVALENCE (Da(1),A(1)) , (Dz(1),Z(1)) , (Forma,Filea(4)) , (Typea,Filea(5)) , (Ncol,Filea(3)) , (Typel,Filel(5))
   !>>>>EQUIVALENCE (itran(1),itrn) , (itran(2),jtrn) , (itran(3),dtrn(1))
   DATA parm(3) , parm(4)/4HCDCO , 4HMP  /
   DATA ibegn/4HBEGN/ , iend/4HEND /
   DATA limit/1.0D-38/
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
   IF ( icrq>0 ) GOTO 2800
   Det(1) = 1.D0
   Det(2) = 0.D0
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
   CALL open(*2500,Sr2fil,Ix(outbuf),Wrtrew)
   CALL fname(Fileu(1),X(1))
   CALL write(Sr2fil,X(1),2,1)
   Filel(2) = 0
   Filel(3) = ncol
   Filel(4) = 4
   Filel(6) = 0
   Filel(7) = 0
   Fileu(2) = 0
   Fileu(3) = ncol
   Fileu(4) = 5
   Fileu(6) = 0
   Fileu(7) = 0
   IF ( ncol>2 ) THEN
!
!     CALL GENVEC TO PICK B, BBAR, C, CBAR, AND R
!
      IF ( B<=0 .OR. Bbar<=0 ) CALL genvec(*2900,Ix(bufa),Filea(1),Nx,Ix(1),ncol,B,Bbar,C,Cbar,R,2)
      bbar1 = Bbar + 1
      bbbar = min0(B+Bbar,ncol)
      bbbar1 = bbbar - 1
      scrflg = 0
      IF ( R<bbbar1 ) scrflg = 1
      IF ( scrflg/=0 ) THEN
         icrq = (bbbar1-R)*4*Bbar
         CALL page2(2)
         WRITE (Nout,99001) Uim , icrq
99001    FORMAT (A29,' 2177, SPILL WILL OCCUR IN COMPLEX UNSYMMETRIC ','DECOMPOSITION.',/I10,                                       &
                &' ADDITIONAL WORDS NEEDED TO STAY IN CORE.')
      ENDIF
!
!     INITIALIZE POINTERS TO SPECIFIC AREAS OF CORE
!
      i1 = 1
      ipak = i1 + 2*Bbar*R + bbbar/2 + 1
      i1sp = Bbar*R*4 + 1
      i2 = ipak
      i3sp = (i2+2*min0(ncol,bbbar+Bbar))*2 - 1
      i3 = i2 + 2*min0(ncol,bbbar+Bbar) + C
      i4sp = i3sp + (Bbar+2)*C*4 - 2*C
      i4 = i3 + 2*bbar1*C + Cbar
      i5 = i4 + 2*bbbar*Cbar
      i6sp = (i5+2*C*Cbar)*2 - 1
      i7sp = i6sp + Cbar
      parm(5) = ibegn
      CALL conmsg(parm(3),3,0)
      end = i7sp + C
!
!     DEFINITION OF KEY PROGRAM PARAMETERS
!
!     I1     =  POINTER TO AREA WHERE COMPLETED COLUMNS OF L ARE STORE
!     I1SP   =  POINTER TO AREA WHERE THE PERMUTATION INDEXES ARE STOR
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
!     BBBAR1 =  B + BBAR - 1
!     SCRFLG =  NONZERO MEANS SPILL
!
!     RE-WRITE THE UPPER TRIANGLE OF ACTIVE ELEMENTS IN THE TRANSPOSED
!     ORDER
!
      parm(2) = Filea(1)
      CALL open(*2500,Filea(1),Ix(bufa),Rdrew)
      ccount = 0
      IF ( C/=0 ) CALL ctrnsp(Ix(1),X(1),Nx,Filea(1),B,Sr1fil)
!
!     ZERO CORE
!
      DO i = 1 , end
         X(i) = 0.
      ENDDO
      IF ( C==0 ) GOTO 200
!
!     OPEN THE FILE CONTAINING THE TRANSPOSED ACTIVE ELEMENTS AND READ I
!     THE FIRST BBAR + 1 ROWS
!
      parm(2) = Sr1fil
      CALL open(*2500,Sr1fil,Ix(sr1buf),Rd)
      k = 0
      DO
         CALL read(*2600,*2700,Sr1fil,itran(1),6,0,flag)
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
!
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
                     in1 = i3 + 2*kk*bbar1 + k + k
                     Dx(in1) = dtrn(1)
                     Dx(in1+1) = dtrn(2)
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
                              in1 = i3 + 2*kk*bbar1 + k + k
                              Dx(in1) = dtrn(1)
                              Dx(in1+1) = dtrn(2)
                              GOTO 50
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
 50   ENDDO
   ELSE
      CALL com12(*2900,Ix(1),X(1),Dx(1),iterm)
      parm(5) = iend
      CALL conmsg(parm(3),3,0)
      RETURN
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
   CALL fwdrec(*2600,Filea(1))
   lcol = 0
   cbcnt = 0
   jposl = 0
 300  IF ( jpos>ncol ) THEN
!
!     FINISH WRITING OUT THE COMPLETED COLUMNS OF L
!
      CALL close(Sr1fil,Rew)
      CALL close(Filel,Norew)
      CALL close(Sr2fil,Norew)
      CALL comfin(iterm,scrflg,sr2fl,jposl,i1sp,Bbar,i1,cbcnt,ipak,R,bbbar1,bbbar,i6sp,i4,i4sp,Ix,Dx,X,lcol)
      parm(5) = iend
      CALL conmsg(parm(3),3,0)
      Fileu(7) = bbbar
      RETURN
   ELSE
!
!     READ NEXT COLUMN OF A INTO AREA II
!
      ioff = max0(1,jpos-bbbar1)
      count = cbcnt
      CALL intpk(*2900,Filea(1),0,Cdp,0)
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
                     in1 = i4 + (kk+1)*bbbar*2 - 2
                     Dx(in1) = da(1)
                     Dx(in1+1) = da(2)
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
            in1 = i4 + 2*(kk+1)*bbbar - 2
            Dx(in1) = da(1)
            Dx(in1+1) = da(2)
            EXIT
         ENDIF
      ENDDO
   ELSE
!
!     READ ELEMENTS WITHIN THE BAND INTO AREA II
!
      in1 = i2 + 2*(Ii-ioff)
      Dx(in1) = da(1)
      Dx(in1+1) = da(2)
   ENDIF
 500  IF ( Eol==0 ) THEN
      CALL zntpki
      GOTO 400
   ENDIF
!
!     ARRANGE ACTIVE ROW INDEXES IN SEQUENCE AND STORE THEM IN AREA VI
!
 600  IF ( count/=cbcnt ) THEN
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
   ENDIF
!
!     TEST FOR POSSIBLE MERGING BETWEEN AN INACTIVE-ACTIVE COLUMN AND
!     THE CURRENT PIVOTAL COLUMN
!
   IF ( ccount==0 ) GOTO 800
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
      in1 = i3 + Ix(i7sp)*bbar1*2
      ccount = ccount - 1
      kk = 0
      DO
         in2 = in1 + kk + kk
         in3 = i2 + kk + kk
         Dx(in3) = Dx(in3) + Dx(in2)
         Dx(in3+1) = Dx(in3+1) + Dx(in2+1)
         Dx(in2) = 0.D0
         Dx(in2+1) = 0.D0
         kk = kk + 1
         IF ( kk<bbar1 ) THEN
         ELSEIF ( kk==bbar1 ) THEN
!
!     MERGE INTERACTION ELEMENTS
!
            IF ( cbcnt==0 ) GOTO 700
            in1 = i5 + 2*Ix(i7sp)*Cbar
            k = 0
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ELSE
      GOTO 800
   ENDIF
   DO
      in2 = i4sp + k
      IF ( Ix(in2)/=0 ) THEN
         in3 = in1 + 2*k
         IF ( Dx(in3)/=0.D0 .OR. Dx(in3+1)/=0.D0 ) THEN
            IF ( Ix(in2)>jpos+Bbar ) THEN
!
!     STORE ELEMENT IN THE ACTIVE ROW
!
               in2 = i4 + 2*(k+1)*bbbar - 2
               Dx(in2+1) = Dx(in2+1) - Dx(in3+1)
               Dx(in3+1) = 0.D0
               Dx(in2) = Dx(in2) - Dx(in3)
               Dx(in3) = 0.D0
            ELSE
!
!     STORE ELEMENT WITHIN THE LOWER BAND
!
               in2 = i2 + 2*(Ix(in2)-ioff)
               Dx(in2) = Dx(in2) - Dx(in3)
               Dx(in2+1) = Dx(in2+1) - Dx(in3+1)
            ENDIF
            Dx(in3) = 0.D0
            Dx(in3+1) = 0.D0
         ENDIF
      ENDIF
      k = k + 1
      IF ( k>=Cbar ) THEN
         IF ( k/=Cbar ) THEN
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
         EXIT
      ENDIF
   ENDDO
!
!     MOVE THE POINTERS IN AREA VII UP ONE
!
 700  in1 = i7sp + ccount - 1
   DO i = i7sp , in1
      Ix(i) = Ix(i+1)
   ENDDO
   Ix(in1+1) = 0
 800  IF ( lcol==0 ) GOTO 1100
!
!     OPERATE ON THE CURRENT COLUMN OF A BY ALL PREVIOUS COLUMNS OF L,
!     MAKING NOTED INTERCHANGES AS YOU GO
!
   IF ( scrflg/=0 ) THEN
      IF ( lcol>=(R-1) ) THEN
         IF ( lcol/=(R-1) ) THEN
            parm(2) = sr2fl
            CALL open(*2500,sr2fl,Ix(sr2buf),Rd)
         ENDIF
         parm(2) = sr3fl
         CALL open(*2500,sr3fl,Ix(sr3buf),Wrtrew)
      ENDIF
   ENDIF
   ll = 0
   lll = 0
   llll = 0
!
!     PICK UP INTERCHANGE INDEX FOR COLUMN JPOSL + LL + 1
!
 900  in1 = i1sp + ll
   intchn = Ix(in1)
   in2 = i2 + ll + ll
   IF ( intchn/=0 ) THEN
!
!     PERFORM ROW INTERCHANGE
!
      in1 = in2 + 2*intchn
      da(1) = Dx(in1)
      Dx(in1) = Dx(in2)
      Dx(in2) = da(1)
      da(1) = Dx(in1+1)
      Dx(in1+1) = Dx(in2+1)
      Dx(in2+1) = da(1)
   ENDIF
!
!     COMPUTE THE CONTRIBUTION FROM THAT COLUMN
!
   end = min0(bbar1,ncol-(jposl+ll))
   IF ( Dx(in2)/=0.D0 .OR. Dx(in2+1)/=0.D0 ) THEN
      in1 = i1 + 2*lll*Bbar
      CALL cloop(Dx(in2+2),Dx(in1),Dx(in2),end-1)
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
                  in4 = i2 + 2*(Ix(in1)-ioff)
                  k = i4 + 2*(jposl+bbbar-jpos+ll+Ix(in3)*bbbar)
                  Dx(in4) = Dx(in4) - Dx(k)*Dx(in2) + Dx(k+1)*Dx(in2+1)
                  Dx(in4+1) = Dx(in4+1) - Dx(in2+1)*Dx(k) - Dx(in2)*Dx(k+1)
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
      IF ( cbcnt==0 ) GOTO 1100
      k = 0
   ELSE
      IF ( ll-R+1<0 ) GOTO 900
      IF ( ll-R+1==0 ) THEN
         IF ( R==bbbar1 ) GOTO 900
         in1 = i1 + 2*ll*Bbar
      ELSE
         in1 = i1 + (lll-1)*Bbar*2
         IF ( ll/=R .OR. lcol/=bbbar1 ) CALL write(sr3fl,Dx(in1),4*Bbar,0)
         lll = lll - 1
      ENDIF
      icrq = in1 + Bbar*4 - 1 - sr3buf
      IF ( icrq>0 ) GOTO 2800
      ibbar4 = Bbar*4
      CALL read(*2600,*2700,sr2fl,Dx(in1),ibbar4,0,flag)
      GOTO 900
   ENDIF
 1000 in1 = i4sp + k
   IF ( Ix(in1)>jpos+Bbar ) THEN
      in1 = in1 + Cbar
      IF ( Ix(in1)/=jpos ) THEN
         kkk = max0(0,bbbar-jpos+Ix(in1)-1)
         in2 = i4 + 2*k*bbbar - 2
         in3 = i2 + 2*(kkk-1-max0(0,bbbar-jpos))
         in1 = in2 + 2*bbbar
         in2 = in2 + 2*kkk
         DO
            in2 = in2 + 2
            kkk = kkk + 1
            in3 = in3 + 2
            Dx(in1) = Dx(in1) - Dx(in2)*Dx(in3) + Dx(in2+1)*Dx(in3+1)
            Dx(in1+1) = Dx(in1+1) - Dx(in2+1)*Dx(in3) - Dx(in2)*Dx(in3+1)
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
   IF ( k<Cbar ) GOTO 1000
   IF ( k/=Cbar ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     SEARCH THE LOWER BAND FOR THE MAXIMUM ELEMENT AND INTERCHANGE
!     ROWS TO BRING IT TO THE DIAGONAL
!
 1100 k = 1
   in1 = i2 + (jpos-ioff)*2
   dx1 = 0.D0
   dx2 = 0.D0
   IF ( dabs(Dx(in1))>limit ) dx1 = Dx(in1)**2
   IF ( dabs(Dx(in1+1))>limit ) dx2 = Dx(in1+1)**2
   max(1) = dx1 + dx2
   intchn = 0
   end = min0(bbar1,ncol-jpos+1)
   IF ( end/=1 ) THEN
      DO
         in2 = in1 + k + k
         dx1 = 0.D0
         dx2 = 0.D0
         IF ( dabs(Dx(in2))>limit ) dx1 = Dx(in2)**2
         IF ( dabs(Dx(in2+1))>limit ) dx2 = Dx(in2+1)**2
         dx2 = dx2 + dx1
         IF ( dx2>max(1) ) THEN
            max(1) = dx2
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
   ENDIF
!
   IF ( intchn/=0 ) THEN
!
!     INTERCHANGE ROWS IN AREA II
!
      Det(1) = -Det(1)
      Det(2) = -Det(2)
!
      max(1) = Dx(in1)
      in2 = in1 + 2*intchn
      Dx(in1) = Dx(in2)
      Dx(in2) = max(1)
      max(1) = Dx(in1+1)
      Dx(in1+1) = Dx(in2+1)
      Dx(in2+1) = max(1)
!
!     STORE THE PERMUTATION INDEX
!
      in2 = i1sp + lcol
      Ix(in2) = intchn
   ENDIF
!
!     DIVIDE THE LOWER BAND BY THE DIAGONAL ELEMENT
!
   dx1 = 0.D0
   dx2 = 0.D0
   IF ( dabs(Dx(in1))>limit ) dx1 = Dx(in1)**2
   IF ( dabs(Dx(in1+1))>limit ) dx2 = Dx(in1+1)**2
   da(1) = dx1 + dx2
   IF ( da(1)==0.D0 ) GOTO 2900
   max(1) = Dx(in1)/da(1)
   max(2) = -Dx(in1+1)/da(1)
   Mindia = dmin1(dsqrt(da(1)),Mindia)
   da(1) = dmax1(dabs(Det(1)),dabs(Det(2)))
   DO WHILE ( da(1)>10.D0 )
      Det(1) = Det(1)*.1D0
      Det(2) = Det(2)*.1D0
      da(1) = da(1)*.1D0
      Power = Power + 1
   ENDDO
   DO WHILE ( da(1)<.1D0 )
      Det(1) = Det(1)*10.D0
      Det(2) = Det(2)*10.D0
      da(1) = da(1)*10.D0
      Power = Power - 1
   ENDDO
   da(1) = Det(1)*Dx(in1) - Det(2)*Dx(in1+1)
   Det(2) = Det(2)*Dx(in1) + Det(1)*Dx(in1+1)
   Det(1) = da(1)
   k = 1
   end = min0(bbar1,ncol-jpos+1)
   IF ( end/=1 ) THEN
      DO
         in2 = in1 + k + k
         da(1) = Dx(in2)*max(1) - Dx(in2+1)*max(2)
         Dx(in2+1) = Dx(in2)*max(2) + Dx(in2+1)*max(1)
         Dx(in2) = da(1)
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
      in1 = i4 + 2*bbbar1
      DO
         da(1) = Dx(in1)*max(1) - Dx(in1+1)*max(2)
         Dx(in1+1) = Dx(in1)*max(2) + Dx(in1+1)*max(1)
         Dx(in1) = da(1)
         in1 = in1 + 2*bbbar
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
!     INTERCHANGE ACTIVE COLUMNS AND ADD IN EFFECT OF THE CURRENT COLUMN
!
   IF ( ccount==0 ) GOTO 1300
   IF ( jpos<bbbar ) GOTO 1300
   intch = Ix(i1sp)
   k = 0
 1200 in1 = i3sp + k
   IF ( intch/=0 ) THEN
      in1 = i3 + 2*k*bbar1
      in2 = in1 + intch + intch
      da(1) = Dx(in1)
      Dx(in1) = Dx(in2)
      Dx(in2) = da(1)
      da(1) = Dx(in1+1)
      Dx(in1+1) = Dx(in2+1)
      Dx(in2+1) = da(1)
   ENDIF
   kk = 1
   in2 = i1 - 2
   in1 = i3 + 2*k*bbar1
   IF ( Dx(in1)/=0.D0 .OR. Dx(in1+1)/=0.D0 ) THEN
      DO
         in3 = in1 + kk + kk
         in4 = in2 + kk + kk
         Dx(in3) = Dx(in3) - Dx(in1)*Dx(in4) + Dx(in1+1)*Dx(in4+1)
         Dx(in3+1) = Dx(in3+1) - Dx(in1)*Dx(in4+1) - Dx(in1+1)*Dx(in4)
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
   IF ( k<C ) GOTO 1200
   IF ( k/=C ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     WRITE OUT THE NEXT COLUMN OF U AND THE ROW OF ACTIVE ELEMENTS
!
 1300 parm(2) = Sr2fil
   CALL bldpk(Cdp,typel,Sr2fil,0,0)
   in1 = i2
   Jj = ioff
   DO
      dz(1) = Dx(in1)
      dz(2) = Dx(in1+1)
      IF ( dz(1)/=0.D0 .OR. dz(2)/=0.D0 ) CALL zblpki
      in1 = in1 + 2
      Jj = Jj + 1
      IF ( Jj>jpos ) THEN
         IF ( Dx(in1-2)==0.D0 .AND. Dx(in1-1)==0.D0 ) GOTO 2900
!
!     PACK ACTIVE COLUMN ELEMENTS ALSO
!
         IF ( ccount==0 ) GOTO 1400
         IF ( jpos<bbbar ) GOTO 1400
         k = 0
         EXIT
      ENDIF
   ENDDO
   DO
      in1 = i7sp + k
      in2 = Ix(in1) + i3sp
      in3 = i3 + 2*(Ix(in1)*bbar1)
      dz(1) = Dx(in3)
      dz(2) = Dx(in3+1)
      IF ( dz(1)/=0.D0 .OR. dz(2)/=0.D0 ) THEN
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
 1400 CALL bldpkn(Sr2fil,0,Fileu)
!
!     COMPUTE ACTIVE ROW-COLUMN INTERACTION
!
   IF ( ccount==0 .OR. cbcnt==0 ) GOTO 1600
   IF ( jpos<bbbar ) GOTO 1600
   k = 0
 1500 in1 = i3 + 2*k*bbar1
   IF ( Dx(in1)/=0.D0 .OR. Dx(in1+1)/=0.D0 ) THEN
      kk = 0
      DO
         in2 = i4 + 2*kk*bbbar
         IF ( Dx(in2)/=0.D0 .OR. Dx(in2+1)/=0.D0 ) THEN
            in3 = i5 + 2*(k*Cbar+kk)
            Dx(in3) = Dx(in3) + Dx(in2)*Dx(in1) - Dx(in2+1)*Dx(in1+1)
            Dx(in3+1) = Dx(in3+1) + Dx(in2)*Dx(in1+1) + Dx(in2+1)*Dx(in1)
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
   ENDIF
   k = k + 1
   IF ( k<C ) GOTO 1500
   IF ( k/=C ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!     MOVE ELEMENTS IN AREA III UP ONE CELL
!
 1600 IF ( ccount==0 ) GOTO 1800
   IF ( jpos<bbbar ) GOTO 1800
   k = 0
 1700 in1 = i3sp + k
   IF ( Ix(in1)/=0 ) THEN
      kk = 0
      in1 = i3 + 2*k*bbar1
      DO
         in2 = in1 + kk + kk
         Dx(in2) = Dx(in2+2)
         Dx(in2+1) = Dx(in2+3)
         kk = kk + 1
         IF ( kk<Bbar ) THEN
         ELSEIF ( kk==Bbar ) THEN
            Dx(in2+2) = 0.D0
            Dx(in2+3) = 0.D0
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
   k = k + 1
   IF ( k<C ) GOTO 1700
   IF ( k/=C ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
!
!
!     DETERMINE IF A COLUMN OF L CAN BE WRITTEN OUT
!
 1800 IF ( lcol<bbbar1 ) GOTO 2000
!
!     OUTPUT A COLUMN OF L
!
   parm(2) = Filel(1)
   jposl = jposl + 1
   CALL bldpk(Cdp,typel,Filel(1),0,0)
!
!     STORE THE PERMUTATION INDEX AS THE DIAGONAL ELEMENT
!
   Jj = jposl
   dz(1) = Ix(i1sp)
   dz(2) = 0.D0
   CALL zblpki
   k = 0
   DO
      Jj = jposl + k + 1
      in2 = i1 + k + k
      dz(1) = Dx(in2)
      dz(2) = Dx(in2+1)
      IF ( dz(1)/=0.D0 .OR. dz(2)/=0.D0 ) CALL zblpki
      k = k + 1
      IF ( k<Bbar ) THEN
      ELSEIF ( k==Bbar ) THEN
!
!     PACK ACTIVE ROW ELEMENTS ALSO
!
         IF ( cbcnt==0 ) GOTO 1900
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
      in2 = i4 + (Ix(in1)*bbbar)*2
      in1 = Ix(in1) + i4sp
      Jj = Ix(in1)
      dz(1) = Dx(in2)
      dz(2) = Dx(in2+1)
      IF ( dz(1)/=0.D0 .OR. dz(2)/=0.D0 ) CALL zblpki
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
 1900 CALL bldpkn(Filel,0,Filel)
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
      CALL open(*2500,sr2fl,Ix(sr2buf),Rd)
      IF ( R<=2 ) THEN
         icrq = i1 + Bbar*4 - 1 - sr3buf
         IF ( icrq>0 ) GOTO 2800
         ibbar4 = Bbar*4
         CALL read(*2600,*2700,sr2fl,Dx(i1),ibbar4,0,flag)
         lcol = lcol - 1
         GOTO 2000
      ENDIF
   ENDIF
   DO
      in1 = i1 + k*Bbar*2
      in2 = in1 + Bbar + Bbar
      CALL cxloop(Dx(in1),Dx(in2),Bbar)
      k = k + 1
      IF ( k-R+2<0 ) THEN
      ELSEIF ( k-R+2==0 ) THEN
         IF ( R<bbbar1 ) THEN
            icrq = in2 + Bbar*4 - 1 - sr3buf
            IF ( icrq>0 ) GOTO 2800
            ibbar4 = Bbar*4
            CALL read(*2600,*2700,sr2fl,Dx(in2),ibbar4,0,flag)
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
 2000 IF ( cbcnt==0 ) GOTO 2200
!
!     MOVE ELEMENTS IN AREA IV UP ONE CELL
!
   k = 0
 2100 in1 = i4sp + k
   IF ( Ix(in1)/=0 ) THEN
      kk = 0
      in1 = i4 + 2*k*bbbar
      DO
         in2 = in1 + kk + kk
         Dx(in2) = Dx(in2+2)
         Dx(in2+1) = Dx(in2+3)
         kk = kk + 1
         IF ( kk<bbbar1 ) THEN
         ELSEIF ( kk==bbbar1 ) THEN
            Dx(in2+2) = 0.D0
            Dx(in2+3) = 0.D0
            EXIT
         ELSE
            parm(1) = -25
            CALL mesage(parm(1),parm(2),parm(3))
            GOTO 99999
         ENDIF
      ENDDO
   ENDIF
   k = k + 1
   IF ( k<Cbar ) GOTO 2100
   IF ( k/=Cbar ) THEN
      parm(1) = -25
      CALL mesage(parm(1),parm(2),parm(3))
      GOTO 99999
   ENDIF
 2200 IF ( scrflg/=0 ) THEN
!
!     STORE COLUMN ON THE SCRATCH FILE
!
      IF ( lcol-R+1>=0 ) THEN
         IF ( lcol-R+1/=0 ) THEN
            in1 = i1 + (lll-1)*Bbar*2
            CALL write(sr3fl,Dx(in1),Bbar*4,0)
         ENDIF
         in1 = i2 + 2*(jpos-ioff+1)
         CALL write(sr3fl,Dx(in1),Bbar*4,0)
!
!     CLOSE SCRATCH FILES AND SWITCH THE POINTERS TO THEM
!
         CALL close(sr3fl,Rew)
         CALL close(sr2fl,Rew)
         in1 = sr2fl
         sr2fl = sr3fl
         sr3fl = in1
         GOTO 2300
      ENDIF
   ENDIF
!
!     STORE COLUMN IN CORE
!
   in1 = i1 + 2*lcol*Bbar
   end = min0(Bbar,ncol-jpos)
   IF ( end/=0 ) THEN
      k = 0
      in3 = i2 + 2*(jpos-ioff+1)
      DO
         in2 = in1 + k + k
         in4 = in3 + k + k
         Dx(in2) = Dx(in4)
         Dx(in2+1) = Dx(in4+1)
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
 2300 lcol = lcol + 1
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
                     in1 = i3 + 2*(k+1)*bbar1 - 2
                     Dx(in1) = Dx(in1) + dtrn(1)
                     Dx(in1+1) = Dx(in1+1) + dtrn(2)
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
                              in1 = i3 + 2*(k+1)*bbar1 - 2
                              Dx(in1) = dtrn(1)
                              Dx(in1+1) = dtrn(2)
                              ccount = ccount + 1
                              GOTO 2305
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
 2305          CALL read(*2600,*2700,Sr1fil,itran(1),6,0,flag)
               IF ( itrn<=0 ) THEN
                  CALL close(Sr1fil,Rew)
                  EXIT
               ENDIF
            ENDDO
            IF ( ccount/=count ) THEN
!
!     RE-ARRANGE INDEXES IN SEQUENTIAL ORDER
!
               ASSIGN 2400 TO kk
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 2400 jpos = jpos + 1
!
!     ZERO AREA II
!
   end = i2 + 2*min0(jpos-ioff+Bbar-1,ncol-1) + 1
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
               in1 = i5 + k + k
               in2 = i3 + Bbar + Bbar
               k = 0
               DO
                  Dx(in2) = Dx(in2) - Dx(in1)
                  Dx(in2+1) = Dx(in2+1) - Dx(in1+1)
                  Dx(in1) = 0.D0
                  Dx(in1+1) = 0.D0
                  in2 = in2 + bbar1 + bbar1
                  in1 = in1 + Cbar + Cbar
                  k = k + 1
                  IF ( k>=C ) GOTO 2450
               ENDDO
            ELSE
               k = k + 1
               IF ( k>=Cbar ) GOTO 300
            ENDIF
         ENDDO
      ENDIF
!
!     TEST TO SEE IF A ACTIVE ROW HAS BEEN ELIMINATED
!
 2450 in1 = Ix(i6sp) + i4sp
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
!
!     ERROR EXITS
!
 2500 parm(1) = -1
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 2600 parm(1) = -2
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 2700 parm(1) = -3
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 2800 parm(1) = -8
   parm(2) = icrq
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
!
!     SINGULAR MATRIX - CLOSE ALL FILES AND RETURN TO USER
!
 2900 CALL close(Filea(1),Rew)
   CALL close(Filel(1),Rew)
   CALL close(Fileu(1),Rew)
   CALL close(Sr1fil,Rew)
   CALL close(Sr2fil,Rew)
   CALL close(Sr3fil,Rew)
!WKBR SPR94018 4/95      FILEU(2) = BBBAR
   Fileu(7) = bbbar
   RETURN 1
99999 END SUBROUTINE cdcomp
