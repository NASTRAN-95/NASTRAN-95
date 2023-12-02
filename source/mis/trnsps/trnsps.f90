!*==trnsps.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trnsps(Z,Iz)
   USE c_blank
   USE c_names
   USE c_packx
   USE c_system
   USE c_trnspx
   USE c_type
   USE c_unpakx
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: Z
   INTEGER , DIMENSION(2) :: Iz
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: a
   INTEGER :: base , file , i , i2 , i3 , ibuf , ibuf1 , iend , iend1 , ii , iikb , imhere , irat , isum , j , jj , jjke , k , kb , &
            & kbe , ke , kx , last , ll , lx , mm , n1 , n2 , ncp7 , ncpp , npas , nrec , ntype , nwd , nwd1 , nwds , nz
   REAL(REAL64) :: da
   LOGICAL , SAVE :: debug
   INTEGER , DIMENSION(7) :: filea , fileat
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL :: t1 , t2
   EXTERNAL bckrec , close , cpyfil , fname , fwdrec , gopen , mesage , open , pack , read , rewind , skprec , sswtch , tmtogo ,    &
          & unpack , unpscr , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   !>>>>EQUIVALENCE (Filea(1),Namea) , (Fileat(1),Nameat) , (a(1),da)
   DATA nam/4HTRNS , 4HPS  / , debug/.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL sswtch(19,i)
         IF ( i==1 ) debug = .TRUE.
         last = 1
         ntype = iotypa
         IF ( ntype==3 ) ntype = 2
         ibuf1 = lcore - sysbuf
         ibuf = ibuf1 - sysbuf
         nz = ibuf - 1
         imhere = 10
         IF ( nz<=0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nrec = 0
         file = namea
         IF ( iforma>2 .OR. ncola==1 ) CALL open(*40,namea,Z(ibuf1),rdrew)
         DO i = 2 , 7
            fileat(i) = filea(i)
         ENDDO
         IF ( debug ) WRITE (nout,99001) fileat
99001    FORMAT (' TRNSPS/@5 BEFORE TRANSPOSE, TRAIL-AT =',7I8)
         IF ( iforma==3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iforma==4 .OR. iforma==5 ) THEN
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
            IF ( n1==-37 ) GOTO 80
            CALL gopen(iscr,Z(ibuf),wrtrew)
            CALL skprec(namea,ncola)
            nwd = iwords(itypa)
            irat = 3
            iend = (ibuf-1-nwd*ncola)/irat
            iend1 = iend + 1
            isum = 0
            DO i = 1 , ncola
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     iu = 0
                     CALL unpack(*80,namea,Z(3))
                     Iz(1) = iu
                     Iz(2) = ju
                     ll = (ju-iu+1)*nwd + 2
                     isum = isum + ll
                     IF ( isum>iend ) THEN
                        nrec = nrec + 1
                        CALL write(iscr,0,0,1)
                        isum = ll
                     ENDIF
                     IF ( dgflag==-123457890 ) THEN
                        IF ( iforma/=5 ) THEN
                           IF ( itypa==2 ) GOTO 2
                           IF ( itypa==3 ) THEN
                              Z(4) = 0.0
                           ELSEIF ( itypa==4 ) THEN
                              Z(5) = 0.0
                              Z(6) = 0.0
                              GOTO 2
                           ENDIF
                           Z(3) = 1.0
                        ELSEIF ( itypa==2 ) THEN
                           da = 1.0D+0
                           Z(ju*2+1) = a(1)
                           Z(ju*2+2) = a(2)
                        ELSEIF ( itypa==3 ) THEN
                           Z(ju*2+1) = 1.0
                           Z(ju*2+2) = 0.0
                        ELSEIF ( itypa==4 ) THEN
                           j = ju*4 - 3
                           da = 1.0D+0
                           Z(j+1) = a(1)
                           Z(j+2) = a(2)
                           Z(j+3) = 0.0
                           Z(j+4) = 0.0
                        ELSE
                           Z(ju+2) = 1.0
                        ENDIF
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
 2                      da = 1.0D+0
                        Z(3) = a(1)
                        Z(4) = a(2)
                     ENDIF
                     spag_nextblock_2 = 2
                  CASE (2)
                     CALL write(iscr,Z(1),ll,0)
                     CALL bckrec(namea)
                     CALL bckrec(namea)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            nrec = nrec + 1
            CALL write(iscr,0,0,1)
            CALL close(namea,clsrew)
            CALL close(iscr,clsrew)
            itypat = itypa
            IF ( iforma==4 ) iforat = 5
            IF ( iforma==5 ) iforat = 4
            iat(1) = ia(1)
            iat(2) = ia(2)
            dgflag = 0
            filea(4) = nrec*10
            filea(6) = isum
         ELSEIF ( iforma==6 ) THEN
!
!     SYMMETRIC MATRIX
!     ================
!
            IF ( ncola==nrowa ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL fname(namea,a)
            WRITE (nout,99002) uwm , a , ncola , nrowa
99002       FORMAT (A25,' FROM TRNSP, ',2A4,' MATRIX (',I7,4H BY ,I7,') IS NOT SYMMETRIC NOR SQUARE ',/5X,                          &
                   &'IT WILL BE TREATED AS RECTANGULAR')
            CALL close(namea,clsrew)
         ELSEIF ( iforma==7 ) THEN
!
!     ROW VECTOR (IFORMA=7, 1xN)
!     ==========================
!
!     A ROW VECTOR IS A ROW OF MATRIX ELEMENTS STORED IN COLUMN FORMAT
!     WITH TRAILER 1xN (NOT Nx1). THEREFORE THE TRANSPOSE OF ROW VECTOR
!     (IFORMA=7) IS A COLUMN VECTOR, WHICH IS RECTANG. (IFORAT=2).
!     THE TRAILER REMAINS UNCHANGED
!
            IF ( ncola/=1 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iforat = 2
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( iforma==8 ) THEN
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
            CALL read(*99999,*99999,namea,Z(1),1,1,j)
            CALL bckrec(namea)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SQUARE AND RECTANGULAR MATRICES
!     ===============================
!
         IF ( ncola==1 ) THEN
!
!     ONE-COLUMN (1xN) RECTANGUALR MATRIX
!     ===================================
!     TRANSPOSE IS A ROW VECTOR, FORM=7. THE TRAILER REMAINS 1xN.
!
            IF ( ncola/=1 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            iforat = 8
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSE
            nrowat = ncola
            ncolat = 0
            iat(1) = 0
            iat(2) = 0
            ip = 1
            jp = nrowat
            incr = 1
            nwd = iwords(itypa)
            nwd1 = nwd - 1
            nwds = ncola*nwd
            IF ( nrec==0 ) THEN
               irat = min0(max0((lcore/100000+4)*ncola/nrowa,3),10)
               iend = (ibuf1-1-nwds)/irat
               iend = max0(iend,5000)
               iend1 = iend + 1
               CALL unpscr(filea,iscr,Z,ibuf1,ibuf,iend,0,1)
               nrec = filea(4)/10
            ENDIF
            file = iscr
            CALL open(*40,iscr,Z(ibuf1),rdrew)
            j = filea(6) - iend*irat
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
               npas = (ncola+ncpp-1)/ncpp
               IF ( .NOT.(.NOT.debug .AND. j>3*nz) ) THEN
                  WRITE (nout,99003) uim , npas , j
99003             FORMAT (A29,', MATRIX TRANSPOSE WAS PROCESSED BY THE NEW TRNSP ','OUT-OF-CORE METHOD WITH',I5,' NO. OF PASSES',   &
                        & /5X,'(FOR MAXIMUM EFFECIENCY, THE IN-CORE METHOD COULD BE ','ACTIVATED WITH',I9,                          &
                         &' ADDITIONAL OPEN CORE WORDS)')
                  WRITE (nout,99004) n1 , iend , irat , ncpp , npas , nrec
99004             FORMAT (/5X,'OPEN CORE -',I9,' WORDS USED FOR TRANSPOSE OUTPUT ','MATRIX, AND',I8,' WORDS FOR INPUT MATRIX (',I2, &
                         &'/1 RATIO)',/5X,'NO. OF COLUMNS PER PASS =',I5,',  NO. OF PASSES =',I6,',  INPUT MATRIX REWRITTEN IN',I4, &
                         &' RECORDS')
               ENDIF
               file = nameat
               CALL open(*40,nameat,Z(ibuf),wrtrew)
               CALL fname(nameat,a(1))
               CALL write(nameat,a(1),2,1)
               DO mm = i2 , n2 , 3
                  Iz(mm) = nrowa
                  Iz(mm+1) = 0
               ENDDO
               CALL tmtogo(t1)
!
!     OUTER KB-KE LOOP
!
!     MAP DATA INTO TRANSPOSE OUTPUT MATRIX SPACE, Z(I1)...Z(N1), BY
!     PASSES. EACH PASS RANGES FROM KB THRU KE COLUMNS
!
               file = iscr
               ke = 0
            ELSE
!
!     ENTIRE FILEA (FROM ISCR FILE) FITS INTO CORE
!
               IF ( debug ) WRITE (nout,99005) uim
99005          FORMAT (A29,', MATRIX TRANSPOSE WAS PORCESSED BY THE NEW TRNSP ','IN-CORE METHOD')
               CALL fwdrec(*60,iscr)
               ll = nwds + 1
               DO i = 1 , nrec
                  CALL read(*60,*4,iscr,Z(ll),iend1,1,k)
                  imhere = 60
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
 4                ll = ll + k
               ENDDO
               CALL close(iscr,clsrew)
!
               file = nameat
               CALL open(*40,nameat,Z(ibuf1),wrtrew)
               CALL fname(nameat,a(1))
               CALL write(nameat,a(1),2,1)
!
               DO k = 1 , nrowa
                  DO j = 1 , nwds
                     Z(j) = 0.0
                  ENDDO
                  base = nwds + 2
                  IF ( nwd<2 ) THEN
                     DO i = 1 , ncola
                        ii = Iz(base-1)
                        jj = Iz(base)
                        IF ( k>=ii .AND. k<=jj ) THEN
                           kx = k - ii + base
                           Z(i) = Z(kx+1)
                        ENDIF
                        base = base + jj - ii + 3
                     ENDDO
                  ELSEIF ( nwd==2 ) THEN
                     DO i = 1 , ncola
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
                     DO i = 1 , ncola
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
                  CALL pack(Z(1),nameat,nameat)
               ENDDO
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         kb = ke + 1
         ke = ke + ncpp
         IF ( ke>nrowa ) ke = nrowa
         IF ( ke==ncp7 ) THEN
            IF ( debug ) WRITE (nout,99006) (Iz(j),j=i2,n2)
99006       FORMAT ('  IZ(I2...N2) =',18I6,/,(15X,18I6))
            CALL tmtogo(t2)
            t1 = (t1-t2)*0.143
            t1 = t1*float(npas)
            IF ( t1>t2 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         CALL rewind(iscr)
         CALL fwdrec(*60,iscr)
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
         spag_nextblock_1 = 3
      CASE (3)
         DO
            i = i + 1
            IF ( i>ncola ) THEN
!
!     END OF MIDDLE I-LOOP
!
!     PACK THE KB THRU KE COLUMNS OF THE TRANSPOSE MATRIX NAMEAT OUT
!
               DO j = 1 , kbe , nwds
                  CALL pack(Z(j),nameat,nameat)
               ENDDO
!
               IF ( ke<nrowa ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL close(iscr,1)
               spag_nextblock_1 = 5
            ELSE
               IF ( base<ll ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               mm = mm + 3
               IF ( kb/=1 ) THEN
!
!     IF NOT FIRST PASS, CHECK KB AND KE AGAINST MIN/MAX TABLE IN IZ(I2)
!     THRU IZ(N2). IF THEY ARE OUTSIDE RANGE, SKIP NEXT DATA RECORD FROM
!     ISCR FILE AND UPDATE COLUMN COUNTER I
!
                  IF ( kb>Iz(mm+2) .OR. ke<Iz(mm+1) ) THEN
                     CALL fwdrec(*60,iscr)
                     i = Iz(mm+3)
                     CYCLE
                  ENDIF
               ENDIF
               CALL read(*60,*20,iscr,Z(i3),iend1,1,ll)
               imhere = 160
               spag_nextblock_1 = 7
            ENDIF
            CYCLE SPAG_DispatchLoop_1
         ENDDO
 20      ll = n2 + ll
         base = n2 + 2
         spag_nextblock_1 = 4
      CASE (4)
         ii = Iz(base-1)
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
         spag_nextblock_1 = 3
      CASE (5)
!
!     END OF OUTTER KB-KE LOOP, AND
!     END OF SQUARE AND RECTANGULAR MATRIX TRNASPOSE
!
!     OPEN AND CLOSE SCRATCH FILE AGAIN TO PHYSICALLY DELETE THE FILE.
!     MATRIX TRAILER WILL BE WRITTEN OUT BY DTRANP
!
         CALL close(nameat,clsrew)
         CALL gopen(iscr,Z(ibuf1),wrtrew)
         CALL close(iscr,clsrew)
         RETURN
      CASE (6)
         file = nameat
         CALL open(*40,nameat,Z(ibuf),wrtrew)
         CALL cpyfil(namea,nameat,Z(1),nz,k)
         CALL close(nameat,clsrew)
         CALL close(namea,clsrew)
         IF ( debug ) WRITE (nout,99007) fileat
99007    FORMAT (' TRNSPS/@525 AFTER TRANSPOSE, TRAIL-AT =',7I8)
!
!     DIAGONAL MATRIX
!     ===============
!     DIAGONAL MATRIX (IFORMA=3) IS A ONE-COLUMN MATRIX. (1xN)
!
!     THE MATRIX AT RIGHT IS SQUARE (IFORMA=1),      1.  0.  0.
!     OR RECTANGULAR (IFORMA=2), AND IS NOT          0.  2.  0.
!     DIAGONAL (IFORMA=3) IN NASTRAN TERMINOLOGY     0.  0.  1.
!
         RETURN
!
!     ERROR MESSAGES
!
 40      IF ( iforma==8 ) RETURN
         n1 = -1
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 60      n1 = -2
         spag_nextblock_1 = 8
      CASE (7)
         n1 = -8
 80      WRITE (nout,99008) imhere
99008    FORMAT (/5X,'IMHERE =',I5)
         spag_nextblock_1 = 8
      CASE (8)
         CALL mesage(n1,file,nam)
         spag_nextblock_1 = 9
      CASE (9)
         CALL fname(namea,a)
         WRITE (nout,99009) ufm , a , iforma , ncola , nrowa
99009    FORMAT (A23,' FROM TRNSPS, INPUT MATRIX ',2A4,' IS NOT SUITABLE ','FOR MATRIX TRANSPOSE.',/5X,'FORM, COLUMN, ROW =',3I6)
         CALL mesage(-37,namea,nam)
         spag_nextblock_1 = 10
      CASE (10)
         WRITE (nout,99010) ufm , t1
99010    FORMAT (A23,', INSUFFICIENT TIME REMAINING FOR MATRIX TRANSPOSE',/5X,'ESTIMATED TIME NEEDED (FOR TRANSPOSE ALONE) =',I9,   &
                &' CPU SECONDS')
         CALL mesage(-37,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99999 END SUBROUTINE trnsps
