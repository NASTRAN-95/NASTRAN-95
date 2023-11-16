
SUBROUTINE trnsps(Z,Iz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Clsrew , Rc(2) , Rd , Rdrew , Wrt , Wrtrew
   INTEGER Dgflag , Filea(7) , Fileat(7) , Ia(2) , Iat(2) , Iforat , Iforma , Incr , Incr1 , Iotyp , Iotyp1 , Iotypa , Ip , Iscr ,  &
         & Itypa , Itypat , Iu , Iwords(4) , Jp , Ju , Lcore , Namea , Nameat , Ncola , Ncolat , Nout , Nrowa , Nrowat , Nscr ,     &
         & Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Dgflag
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /packx / Iotyp , Iotypa , Ip , Jp , Incr
   COMMON /system/ Sysbuf , Nout
   COMMON /trnspx/ Namea , Ncola , Nrowa , Iforma , Itypa , Ia , Nameat , Ncolat , Nrowat , Iforat , Itypat , Iat , Lcore , Nscr ,  &
                 & Iscr
   COMMON /type  / Rc , Iwords
   COMMON /unpakx/ Iotyp1 , Iu , Ju , Incr1
   COMMON /xmssg / Ufm , Uwm , Uim
!
! Dummy argument declarations
!
   INTEGER Iz(2)
   REAL Z(6)
!
! Local variable declarations
!
   REAL a(2) , t1 , t2
   INTEGER base , file , i , i2 , i3 , ibuf , ibuf1 , iend , iend1 , ii , iikb , imhere , irat , isum , j , jj , jjke , k , kb ,    &
         & kbe , ke , kx , last , ll , lx , mm , n1 , n2 , nam(2) , ncp7 , ncpp , npas , nrec , ntype , nwd , nwd1 , nwds , nz
   DOUBLE PRECISION da
   LOGICAL debug
!
! End of declarations
!
!
!     MATRIX TRANSPOSE ROUTINE REPLACING NASTRAN ORIGINAL TRNSP, WHICH
!     IS AT LEAST 2 TO 4 TIMES SLOWER (COMPARISON DONE ON VAX), AND
!     USING UP TO 8 SCRATCH FILES
!
!     WITH BOTH IN-CORE AND OUT-OF-CORE LOGICS
!     (USE TRANSP FOR IN-CORE MATRIX TRANSPOSE)
!
!     IF DGFLAG = -123457890 (SET BY DTRANP), AND INPUT IS A UPPER OR
!     LOWER TRIANGULAR MATRIX, THE DIAGONAL ELEMENTS ARE REPLACED BY
!     UNITY (1.0)
!
!     CALLER MUST SUPPLY A SCRATCH FILE ISCR, IF MATRIX TO BE TRANSPOSED
!     IS SQUARE, RECTANGULAR, LOWER, AND UPPER TRIAGULAR (FORM 1,2,4,5).
!
!     THIS ROUTINE SETS UP THE OUTPUT MATRIX TRAILER WORDS IN NAMEAT
!     (FILEAT) BUT IT DOES NOT CALL WRTTRL TO WRITE THEM OUT
!
!     WRITTEN BY G.CHAN/UNISYS  12/91
!
   EQUIVALENCE (Filea(1),Namea) , (Fileat(1),Nameat) , (a(1),da)
   DATA nam/4HTRNS , 4HPS  / , debug/.FALSE./
!
   CALL sswtch(19,i)
   IF ( i==1 ) debug = .TRUE.
   last = 1
   ntype = Iotypa
   IF ( ntype==3 ) ntype = 2
   ibuf1 = Lcore - Sysbuf
   ibuf = ibuf1 - Sysbuf
   nz = ibuf - 1
   imhere = 10
   IF ( nz<=0 ) GOTO 900
   nrec = 0
   file = Namea
   IF ( Iforma>2 .OR. Ncola==1 ) CALL open(*700,Namea,Z(ibuf1),Rdrew)
   DO i = 2 , 7
      Fileat(i) = Filea(i)
   ENDDO
   IF ( debug ) WRITE (Nout,99001) Fileat
99001 FORMAT (' TRNSPS/@5 BEFORE TRANSPOSE, TRAIL-AT =',7I8)
   IF ( Iforma==3 ) GOTO 600
   IF ( Iforma==4 .OR. Iforma==5 ) THEN
!
!     UPPER OR LOWER TRIANGULAR MATRICES
!     ==================================
!
!     TRANSPOSE OF UPPER TRIANGULAR MATRIX IS THE LOWER TRIANG. MATRIX
!     AND VISE VERSA
!
!     (IS THIS HOW THE UPPER OR LOWER TRIANGULAR MATRIX WRITTEN? <==?
!
!     NO! IT IS NOT. WE STOP TRNSP SENDING THESE MATRICES OVER HERE.
!     BESIDE, THE LOGIC OF WRITING THE MATRIX BACKWARD HERE IS NOT
!     CORRECT. WE HAVE NOT ACCOMPLISHED THE TRANSPOSE OF THE ORIGINAL
!     MATRIX YET. ALSO, WE SHOULD WRITE THE TRANSPOSE MATRIX OUT BY
!     STRINGS, OR PACK THE MATRIX OUT)
!
      imhere = 600
      n1 = -37
      IF ( n1==-37 ) GOTO 1000
      CALL gopen(Iscr,Z(ibuf),Wrtrew)
      CALL skprec(Namea,Ncola)
      nwd = Iwords(Itypa)
      irat = 3
      iend = (ibuf-1-nwd*Ncola)/irat
      iend1 = iend + 1
      isum = 0
      DO i = 1 , Ncola
         Iu = 0
         CALL unpack(*1000,Namea,Z(3))
         Iz(1) = Iu
         Iz(2) = Ju
         ll = (Ju-Iu+1)*nwd + 2
         isum = isum + ll
         IF ( isum>iend ) THEN
            nrec = nrec + 1
            CALL write(Iscr,0,0,1)
            isum = ll
         ENDIF
         IF ( Dgflag==-123457890 ) THEN
            IF ( Iforma/=5 ) THEN
               IF ( Itypa==2 ) GOTO 10
               IF ( Itypa==3 ) THEN
                  Z(4) = 0.0
               ELSEIF ( Itypa==4 ) THEN
                  Z(5) = 0.0
                  Z(6) = 0.0
                  GOTO 10
               ENDIF
               Z(3) = 1.0
            ELSEIF ( Itypa==2 ) THEN
               da = 1.0D+0
               Z(Ju*2+1) = a(1)
               Z(Ju*2+2) = a(2)
            ELSEIF ( Itypa==3 ) THEN
               Z(Ju*2+1) = 1.0
               Z(Ju*2+2) = 0.0
            ELSEIF ( Itypa==4 ) THEN
               j = Ju*4 - 3
               da = 1.0D+0
               Z(j+1) = a(1)
               Z(j+2) = a(2)
               Z(j+3) = 0.0
               Z(j+4) = 0.0
            ELSE
               Z(Ju+2) = 1.0
            ENDIF
            GOTO 20
 10         da = 1.0D+0
            Z(3) = a(1)
            Z(4) = a(2)
         ENDIF
 20      CALL write(Iscr,Z(1),ll,0)
         CALL bckrec(Namea)
         CALL bckrec(Namea)
      ENDDO
      nrec = nrec + 1
      CALL write(Iscr,0,0,1)
      CALL close(Namea,Clsrew)
      CALL close(Iscr,Clsrew)
      Itypat = Itypa
      IF ( Iforma==4 ) Iforat = 5
      IF ( Iforma==5 ) Iforat = 4
      Iat(1) = Ia(1)
      Iat(2) = Ia(2)
      Dgflag = 0
      Filea(4) = nrec*10
      Filea(6) = isum
   ELSEIF ( Iforma==6 ) THEN
!
!     SYMMETRIC MATRIX
!     ================
!
      IF ( Ncola==Nrowa ) GOTO 600
      CALL fname(Namea,a)
      WRITE (Nout,99002) Uwm , a , Ncola , Nrowa
99002 FORMAT (A25,' FROM TRNSP, ',2A4,' MATRIX (',I7,4H BY ,I7,') IS NOT SYMMETRIC NOR SQUARE ',/5X,                                &
             &'IT WILL BE TREATED AS RECTANGULAR')
      CALL close(Namea,Clsrew)
   ELSEIF ( Iforma==7 ) THEN
!
!     ROW VECTOR (IFORMA=7, 1xN)
!     ==========================
!
!     A ROW VECTOR IS A ROW OF MATRIX ELEMENTS STORED IN COLUMN FORMAT
!     WITH TRAILER 1xN (NOT Nx1). THEREFORE THE TRANSPOSE OF ROW VECTOR
!     (IFORMA=7) IS A COLUMN VECTOR, WHICH IS RECTANG. (IFORAT=2).
!     THE TRAILER REMAINS UNCHANGED
!
      IF ( Ncola/=1 ) GOTO 1200
      Iforat = 2
      GOTO 600
   ELSEIF ( Iforma==8 ) THEN
!
!     IDENTITY MATRIX
!     ===============
!     SIMILAR TO DIAGONAL MATRIX, INDENTITY MATRIX (IFORMA = 8) IS ALSO
!     IN ONE-COLUMN MATRIX FORM
!
!     ALSO, THE IDENTITY MATRIX MAY EXIST ONLY IN THE MATRIX TRAILER.
!     IT DOES NOT PHYSICALLY EXIST.
!
!
      CALL read(*99999,*99999,Namea,Z(1),1,1,j)
      CALL bckrec(Namea)
      GOTO 600
   ENDIF
!
!     SQUARE AND RECTANGULAR MATRICES
!     ===============================
!
   IF ( Ncola==1 ) THEN
!
!     ONE-COLUMN (1xN) RECTANGUALR MATRIX
!     ===================================
!     TRANSPOSE IS A ROW VECTOR, FORM=7. THE TRAILER REMAINS 1xN.
!
      IF ( Ncola/=1 ) GOTO 1200
      Iforat = 8
      GOTO 600
   ELSE
      Nrowat = Ncola
      Ncolat = 0
      Iat(1) = 0
      Iat(2) = 0
      Ip = 1
      Jp = Nrowat
      Incr = 1
      nwd = Iwords(Itypa)
      nwd1 = nwd - 1
      nwds = Ncola*nwd
      IF ( nrec==0 ) THEN
         irat = min0(max0((Lcore/100000+4)*Ncola/Nrowa,3),10)
         iend = (ibuf1-1-nwds)/irat
         iend = max0(iend,5000)
         iend1 = iend + 1
         CALL unpscr(Filea,Iscr,Z,ibuf1,ibuf,iend,0,1)
         nrec = Filea(4)/10
      ENDIF
      file = Iscr
      CALL open(*700,Iscr,Z(ibuf1),Rdrew)
      j = Filea(6) - iend*irat
      IF ( j>0 ) THEN
!
!     ENTIRE FILEA CAN NOT FIT INTO CORE
!
!     OPEN CORE ALLOCATION -             N1    N2              NZ
!                                        /     /  <-- IEND --> /
!     +----------------------------------+-----+---------------+---+---+
!      /          OPEN CORE               /     /                GINO
!     I1                                 I2    I3               BUFFERS
!
!      Z(I1)... Z(N1) FOR TRANSPOSED OUTPUT MATRIX NAMEAT
!     IZ(I2)...IZ(N2) IS A (3 x NREC) TABLE, (MIN, MAX, COLUMN COUNTER)
!               CONTROLLING DATA TRANSFER FROM SCRATCH FILE ISCR.
!      Z(I3)... Z(NZ) FOR INPUT MATRIX NAMEA COMING FROM ISCR
!
!     NOTE - THE RATIO OF (N1-I1)/(NZ-I3), WHICH IS IRAT, IS A FUNCTION
!            OF OPEN CORE SIZE, AND THE MATRIX COLUMN AND ROW SIZES.
!            IRAT IS LIMITED TO 10:1
!     NCPP = NO. OF COULMNS PER PASS, OF THE TRANSPOSE MATRIX NAMEAT
!
!     THE TERMS 'ROW' AND 'COLUMN' ARE LOOSELY DEFINED IN COMMENT LINES
!
         n2 = nz - iend
         i3 = n2 + 1
         n1 = n2 - 3*nrec
         i2 = n1 + 1
         ncpp = n1/nwds
         ncp7 = ncpp*7
         npas = (Ncola+ncpp-1)/ncpp
         IF ( .NOT.(.NOT.debug .AND. j>3*nz) ) THEN
            WRITE (Nout,99003) Uim , npas , j
99003       FORMAT (A29,', MATRIX TRANSPOSE WAS PROCESSED BY THE NEW TRNSP ','OUT-OF-CORE METHOD WITH',I5,' NO. OF PASSES',/5X,     &
                   &'(FOR MAXIMUM EFFECIENCY, THE IN-CORE METHOD COULD BE ','ACTIVATED WITH',I9,' ADDITIONAL OPEN CORE WORDS)')
            WRITE (Nout,99004) n1 , iend , irat , ncpp , npas , nrec
99004       FORMAT (/5X,'OPEN CORE -',I9,' WORDS USED FOR TRANSPOSE OUTPUT ','MATRIX, AND',I8,' WORDS FOR INPUT MATRIX (',I2,       &
                   &'/1 RATIO)',/5X,'NO. OF COLUMNS PER PASS =',I5,',  NO. OF PASSES =',I6,',  INPUT MATRIX REWRITTEN IN',I4,       &
                   &' RECORDS')
         ENDIF
         file = Nameat
         CALL open(*700,Nameat,Z(ibuf),Wrtrew)
         CALL fname(Nameat,a(1))
         CALL write(Nameat,a(1),2,1)
         DO mm = i2 , n2 , 3
            Iz(mm) = Nrowa
            Iz(mm+1) = 0
         ENDDO
         CALL tmtogo(t1)
!
!     OUTER KB-KE LOOP
!
!     MAP DATA INTO TRANSPOSE OUTPUT MATRIX SPACE, Z(I1)...Z(N1), BY
!     PASSES. EACH PASS RANGES FROM KB THRU KE COLUMNS
!
         file = Iscr
         ke = 0
      ELSE
!
!     ENTIRE FILEA (FROM ISCR FILE) FITS INTO CORE
!
         IF ( debug ) WRITE (Nout,99005) Uim
99005    FORMAT (A29,', MATRIX TRANSPOSE WAS PORCESSED BY THE NEW TRNSP ','IN-CORE METHOD')
         CALL fwdrec(*800,Iscr)
         ll = nwds + 1
         DO i = 1 , nrec
            CALL read(*800,*30,Iscr,Z(ll),iend1,1,k)
            imhere = 60
            GOTO 900
 30         ll = ll + k
         ENDDO
         CALL close(Iscr,Clsrew)
!
         file = Nameat
         CALL open(*700,Nameat,Z(ibuf1),Wrtrew)
         CALL fname(Nameat,a(1))
         CALL write(Nameat,a(1),2,1)
!
         DO k = 1 , Nrowa
            DO j = 1 , nwds
               Z(j) = 0.0
            ENDDO
            base = nwds + 2
            IF ( nwd<2 ) THEN
               DO i = 1 , Ncola
                  ii = Iz(base-1)
                  jj = Iz(base)
                  IF ( k>=ii .AND. k<=jj ) THEN
                     kx = k - ii + base
                     Z(i) = Z(kx+1)
                  ENDIF
                  base = base + jj - ii + 3
               ENDDO
            ELSEIF ( nwd==2 ) THEN
               DO i = 1 , Ncola
                  ii = Iz(base-1)
                  jj = Iz(base)
                  IF ( k>=ii .AND. k<=jj ) THEN
                     kx = (k-ii)*2 + base
                     lx = (i-1)*2
                     Z(lx+1) = Z(kx+1)
                     Z(lx+2) = Z(kx+2)
                  ENDIF
                  base = base + (jj-ii+2)*2
               ENDDO
            ELSE
               DO i = 1 , Ncola
                  ii = Iz(base-1)
                  jj = Iz(base)
                  IF ( k>=ii .AND. k<=jj ) THEN
                     kx = (k-ii)*nwd + base
                     lx = (i-1)*nwd
                     DO j = 1 , nwd
                        Z(j+lx) = Z(j+kx)
                     ENDDO
                  ENDIF
                  base = base + (jj-ii+1)*nwd + 2
               ENDDO
            ENDIF
            CALL pack(Z(1),Nameat,Nameat)
         ENDDO
         GOTO 500
      ENDIF
   ENDIF
 100  kb = ke + 1
   ke = ke + ncpp
   IF ( ke>Nrowa ) ke = Nrowa
   IF ( ke==ncp7 ) THEN
      IF ( debug ) WRITE (Nout,99006) (Iz(j),j=i2,n2)
99006 FORMAT ('  IZ(I2...N2) =',18I6,/,(15X,18I6))
      CALL tmtogo(t2)
      t1 = (t1-t2)*0.143
      t1 = t1*float(npas)
      IF ( t1>t2 ) GOTO 1300
   ENDIF
   CALL rewind(Iscr)
   CALL fwdrec(*800,Iscr)
   kbe = (ke-kb+1)*nwds
   DO j = 1 , kbe
      Z(j) = 0.0
   ENDDO
   mm = n1 - 3
   ll = 0
   base = 2
!
!     MIDDLE I-LOOP
!
!     LOAD DATA FROM ISCR/NAMEA INTO Z(I3)...Z(NZ) WHEN NEEDED.
!     AND RUN THRU EACH ROW OF MATRIX NAMEA IN THIS LOOP
!
   i = 0
 200  DO
      i = i + 1
      IF ( i>Ncola ) THEN
!
!     END OF MIDDLE I-LOOP
!
!     PACK THE KB THRU KE COLUMNS OF THE TRANSPOSE MATRIX NAMEAT OUT
!
         DO j = 1 , kbe , nwds
            CALL pack(Z(j),Nameat,Nameat)
         ENDDO
!
         IF ( ke<Nrowa ) GOTO 100
         CALL close(Iscr,1)
         GOTO 500
      ELSE
         IF ( base<ll ) GOTO 400
         mm = mm + 3
         IF ( kb/=1 ) THEN
!
!     IF NOT FIRST PASS, CHECK KB AND KE AGAINST MIN/MAX TABLE IN IZ(I2)
!     THRU IZ(N2). IF THEY ARE OUTSIDE RANGE, SKIP NEXT DATA RECORD FROM
!     ISCR FILE AND UPDATE COLUMN COUNTER I
!
            IF ( kb>Iz(mm+2) .OR. ke<Iz(mm+1) ) THEN
               CALL fwdrec(*800,Iscr)
               i = Iz(mm+3)
               CYCLE
            ENDIF
         ENDIF
         CALL read(*800,*300,Iscr,Z(i3),iend1,1,ll)
         imhere = 160
         GOTO 900
      ENDIF
   ENDDO
 300  ll = n2 + ll
   base = n2 + 2
 400  ii = Iz(base-1)
   jj = Iz(base)
   IF ( kb<=1 ) THEN
!
!     DURING FIRST PASS, SAVE MIN-II, MAX-JJ, AND COLUMN I IN IZ(MM)
!     TABLE. MM RUNS FROM I2 THRU N2.
!
      IF ( ii<Iz(mm+1) ) Iz(mm+1) = ii
      IF ( jj>Iz(mm+2) ) Iz(mm+2) = jj
      Iz(mm+3) = i
   ENDIF
!
   iikb = max0(ii,kb)
   jjke = min0(jj,ke)
   IF ( jjke>=iikb ) THEN
!
!     INNER K-LOOP
!
!     RUN THRU THE IIKB-JJKE ELEMENTS FOR EACH ROW OF MATRIX NAMEA,
!
!     KK = (IIKB-KB)*NWDS
!     LX = (I-1)*NWD + KK + 1
!     KK = BASE -  II*NWD + 1
!
      lx = (i-1)*nwd + (iikb-kb)*nwds + 1
      kx = (iikb-ii)*nwd + base + 1
      IF ( nwd<2 ) THEN
         DO k = iikb , jjke
            Z(lx) = Z(kx)
            kx = kx + 1
            lx = lx + nwds
         ENDDO
      ELSEIF ( nwd==2 ) THEN
         DO k = iikb , jjke
            Z(lx) = Z(kx)
            Z(lx+1) = Z(kx+1)
            kx = kx + 2
            lx = lx + nwds
         ENDDO
      ELSE
         DO k = iikb , jjke
            Z(lx) = Z(kx)
            Z(lx+1) = Z(kx+1)
            Z(lx+2) = Z(kx+2)
            Z(lx+3) = Z(kx+3)
            kx = kx + 4
            lx = lx + nwds
         ENDDO
      ENDIF
   ENDIF
!
!     END OF INNER K-LOOP
!
!     ADJUST BASE FOR ANOTHER ROW OF MATRIX NAMEA
!
   base = base + (jj-ii+1)*nwd + 2
   GOTO 200
!
!     END OF OUTTER KB-KE LOOP, AND
!     END OF SQUARE AND RECTANGULAR MATRIX TRNASPOSE
!
!     OPEN AND CLOSE SCRATCH FILE AGAIN TO PHYSICALLY DELETE THE FILE.
!     MATRIX TRAILER WILL BE WRITTEN OUT BY DTRANP
!
 500  CALL close(Nameat,Clsrew)
   CALL gopen(Iscr,Z(ibuf1),Wrtrew)
   CALL close(Iscr,Clsrew)
   GOTO 99999
 600  file = Nameat
   CALL open(*700,Nameat,Z(ibuf),Wrtrew)
   CALL cpyfil(Namea,Nameat,Z(1),nz,k)
   CALL close(Nameat,Clsrew)
   CALL close(Namea,Clsrew)
   IF ( debug ) WRITE (Nout,99007) Fileat
99007 FORMAT (' TRNSPS/@525 AFTER TRANSPOSE, TRAIL-AT =',7I8)
!
!     DIAGONAL MATRIX
!     ===============
!     DIAGONAL MATRIX (IFORMA=3) IS A ONE-COLUMN MATRIX. (1xN)
!
!     THE MATRIX AT RIGHT IS SQUARE (IFORMA=1),      1.  0.  0.
!     OR RECTANGULAR (IFORMA=2), AND IS NOT          0.  2.  0.
!     DIAGONAL (IFORMA=3) IN NASTRAN TERMINOLOGY     0.  0.  1.
!
   GOTO 99999
!
!     ERROR MESSAGES
!
 700  IF ( Iforma==8 ) GOTO 99999
   n1 = -1
   GOTO 1100
 800  n1 = -2
   GOTO 1100
 900  n1 = -8
 1000 WRITE (Nout,99008) imhere
99008 FORMAT (/5X,'IMHERE =',I5)
 1100 CALL mesage(n1,file,nam)
 1200 CALL fname(Namea,a)
   WRITE (Nout,99009) Ufm , a , Iforma , Ncola , Nrowa
99009 FORMAT (A23,' FROM TRNSPS, INPUT MATRIX ',2A4,' IS NOT SUITABLE ','FOR MATRIX TRANSPOSE.',/5X,'FORM, COLUMN, ROW =',3I6)
   CALL mesage(-37,Namea,nam)
 1300 WRITE (Nout,99010) Ufm , t1
99010 FORMAT (A23,', INSUFFICIENT TIME REMAINING FOR MATRIX TRANSPOSE',/5X,'ESTIMATED TIME NEEDED (FOR TRANSPOSE ALONE) =',I9,      &
             &' CPU SECONDS')
   CALL mesage(-37,0,nam)
!
99999 RETURN
END SUBROUTINE trnsps
