
SUBROUTINE mpy4t(Iz,Z,Dz)
!
!     INNER LOOP FOR MPYAD, METHOD 4 WITH TRANSPOSE
!
!          T
!         A * B + C = D
!
!     THIS ROUTINE IS CALLED ONLY BY MPYAD WHEN METHOD 2 TRANSPOSE,
!     MPY2T, IS SELECTED, AND DIAG 41 IS NOT TURNED ON BY USER.
!
!     MPY4T IS ABOUT 5 TIMES FASTER THAN MPY2T AS TESTED ON VAX
!
!     THERE IS A PICTORIAL DISCRIPTION ABOUT MPY4T IN MPYAD SUBROUTINE
!
!     THIS MACHINE INDEPENDENT ROUTINE CAN ACTUALLY BE INCORPORATED
!     INTO MPYQ, WHICH IS PRESENTLY A .MDS ROUTINE
!
!     IF MATRIX A, OR B, OR BOTH,  IS COMPLEX, MATRIX D IS COMPLEX.
!     MATRIX D CAN NOT BE COMPLEX, IF BOTH MATRICES A AND B ARE REAL.
!
!
!     WRITTEN BY G.CHAN/UNISYS   1/92
!
   IMPLICIT NONE
   INTEGER Acore , All , Apoint , Arow , Arow1 , Arown , Bcol , Crow , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Firstl , Ihalf , &
         & Ii , Jbb , Jhalf , Jj , Jump , Ll , Lll , Mach , Na(3) , Nbx(3) , Nwda , Nwds(4) , Prc(2) , Prca , Prcd , Rc(4) , Rca ,  &
         & Rcb , Rcd , Typ
   COMMON /machin/ Mach , Ihalf , Jhalf
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed
   COMMON /mpyadz/ Rcb , Rcd , Ll , Lll , Jbb , Nbx , Arow , Arow1 , Arown , Acore , Apoint , Bcol , Crow , Firstl , Na , Nwda
   COMMON /mpyqt4/ Rca , Prca , All , Jump , Prcd
   COMMON /type  / Prc , Nwds , Rc
   COMMON /unpakx/ Typ , Ii , Jj
   DOUBLE PRECISION Dz(1)
   INTEGER Iz(1)
   REAL Z(1)
   INTEGER andf , rshift
   DOUBLE PRECISION dsumi , dsumr , dzero
   INTEGER init , ipoint , j , ja , jb , je , kb , l , last , nam(2) , nbr , nbrstr
   REAL sumi , sumr
   EQUIVALENCE (dsumr,sumr) , (dsumi,sumi)
   DATA nam/4HMPY4 , 1HT/ , dzero/0.0D+0/
!
!*****
!     ANDF(I,J)   = IAND(I,J)
!     RSHIFT(I,J) = ISHFT(I,-J)
!     WHERE         ISHFT(I,-J) IS RIGHT-SHIFT I BY J BITS, ZERO FILL
!     AND           ISHFT IS SYSTEM ROUTINE
!
! UNIX:
!     REMOVE ABOVE 2 ON-LINE FUNCTIONS IF IAND AND ISHFT SYSTEM
!     FUNCTIONS ARE NOT AVAILABLE. ANDF AND RSHIFT ARE ALREADY ENTRY
!     POINTS IN SUBROUTINE MAPFNS.
!*****
!
!     METHOD 4T TRANSPOSE CASE
!
!     ARRAY Z(JBB) THRU Z(ACORE-1) HOLDS THE CURRENT COLUMN OF MATRIX B
!     ARRAY Z(1) THRU Z(JBB-1) IS A WORKING COLUMN SPACE FOR MATRIX D
!
!     ON EACH ROW OF A, WE WANT TO MULTIPLY
!
!        A(ROW,J)*B(J,COL) + C(ROW,COL) = D(ROW,COL)
!
!     NOTICE B(J,COL) RUNS FROM B(II,COL) THRU B(JJ,COL) WITHOUT
!     SKIPPING,
!     WHILE A(ROW,J) RUNS IN MULTIPLE STRING SEGMENTS ALONG J.
!     ALSO THE BEGINING OF J IN A(ROW,J) AND THE BEGINING OF J IN
!     B(J,COL) MOST LIKELY START DIFFERNTLY
!
!     NOW, ON EACH ROW, WE START FROM FIRST STRING. SKIP THIS STRING
!     IF IT IS NOT WITHIN B(II,) AND B(JJ,) RANGE. (ALSO, WE HAVE
!     SAVED PREVIOUSLY THE LAST TERM OF THE LAST STRING, AND THEREFORE
!     IF THE WHOLE ROW OF A(,J) WITH ITS STRINGS IS NOT WITHIN II,JJ
!     RANGE OF COLUMN B, WE SKIP THE WHOLE ROW-AND-COLUMN COMPUTATION.)
!     IF IT IS WITHIN THE RANGE, WE NEED TO SYNCHRONIZE THE J INDEX FOR
!     BOTH A(ROW,J) AND B(J,COL), THEN MULTIPLY, AND SUM ON AN ELEMENT
!     OF MATRIX D. THEN MOVE ON TO THE NEXT STRING, AND DO THE SAME.
!     REPEAT THIS PROCESS UNTIL J IS EXHAUST EITHER ON A(ROW,J) OR ON
!     B(J,COL).
!     WHEN ALL ROWS OF MATRIX A CURRENTLY IN CORE HAVE PASSED THRU, WE
!     HAVE ONE COLUMN OF MATRIX D DONE, FROM AROW1 THRU AROWN.
!
!     SINCE TRANSPOSE OF MATRIX A IS WHAT WE WANT, THE TERM 'ROW' IS
!     ACTUALLY 'COLUMN' WHEN THE DATA WAS MOVED INTO Z SPACE IN MPYAD
!
!     RCA,RCB    = 1, MATRIX A,B  IS REAL, = 2 MATRIX A,B IS COMPLEX
!     PRCA       = 1, MATRIX A IS IN S.P., = 2 MATRIX A IS IN D.P.
!     PRCD       = 0, MATRIX D IS IN S.P., = 1 MATRIX A IS IN D.P.
!     NWDA       = NUMBER OF WORDS PER ELEMENT OF MATRIX A
!     JBB        = POINTER TO FIRST WORD OF COLUMN B
!     II,JJ      = FIRST TO LAST NON-ZERO TERMS IN CURRENT COLUMN OF B
!     ALL        = 1,2,3,4 ALL MATRICES ARE OF THE SAME TYPE - S.P.,
!                  D.P., C.S.P., OR C.D.P. RESPECTIVELY
!                = 5, MATRICES ARE OF MIXED TYPES
!     JUMP       = BRANCHING INDEX TO MIXED TYPE MATRICES COMPUTATION.
!
!     APOINT     = POINTER TO STRING CONTROL WORD
!                = 0, CURRENT ROW OF A IS EXHAULTED
!     IZ(APOINT) = LEFT HALF OF WORD IS NBR, RIGHT HALF IS NBRSTR
!     NBR        = NO. OF WORDS   IN THIS STRING
!     NBRSTR     = NO. OF STRINGS IN THIS ROW A
!     INIT       = COLUMN POSITION OF 1ST STRING WORD
!     IF (INIT .GT. JJ) = 1ST STRING WORD IS BEYOND LAST WORD IN COLN B
!     IF (INIT+NBR .LT. II) = LAST STRING WORD IS BEFORE 1ST WORD IN
!                  COLUMN OF B
!     JB,JE      = BEGINNING AND ENDING J-INDEX FOR COLUMN A AND ROW B
!     IPOINT     = THE JB WORD POSITION IN ROW A
!     JA         = POINTER TO ROW A ELEMENT
!     KB         = POINTER TO COLUMN B ELEMENT
!     LAST       = POSITION OF LAST NON-ZERO COLUMN TERM IN ROW OF A
!
!
!     WE START FROM FIRST ROW AROW1, AND WILL RUN THRU TO LAST ROW AROWN
!
   Arow = Arow1
   l = Firstl
 100  Apoint = Iz(l)
   IF ( Apoint==0 ) GOTO 600
   last = rshift(Iz(l-1),Ihalf)
   init = andf(Iz(Apoint),Jhalf)
   IF ( init>Jj .OR. last<Ii ) GOTO 600
   nbrstr = andf(Iz(l-1),Jhalf)
 200  nbr = rshift(Iz(Apoint),Ihalf)
   IF ( init>Jj ) GOTO 600
   IF ( init+nbr>=Ii ) THEN
      jb = max0(init,Ii)
      je = min0(init+nbr-1,Jj)
      IF ( jb<=je ) THEN
         ipoint = Apoint + (jb-init+1)*Prca
         ja = (ipoint-1)/Prca + 1
         kb = (jb-Ii)*Rcb + Jbb
         dsumr = dzero
         IF ( All==2 ) THEN
!
            DO j = jb , je
               dsumr = dsumr + Dz(ja)*Dz(kb)
               ja = ja + Rca
               kb = kb + Rcb
            ENDDO
            Dz(Arow) = Dz(Arow) + dsumr
         ELSEIF ( All==3 ) THEN
!
            sumi = 0.0
            DO j = jb , je
               sumr = sumr + Z(ja)*Z(kb) - Z(ja+1)*Z(kb+1)
               sumi = sumi + Z(ja)*Z(kb+1) + Z(ja+1)*Z(kb)
               ja = ja + Rca
               kb = kb + Rcb
            ENDDO
            Z(Arow) = Z(Arow) + sumr
            Z(Arow+1) = Z(Arow+1) + sumi
         ELSEIF ( All==4 ) THEN
!
            dsumi = dzero
            DO j = jb , je
               dsumr = dsumr + Dz(ja)*Dz(kb) - Dz(ja+1)*Dz(kb+1)
               dsumi = dsumi + Dz(ja)*Dz(kb+1) + Dz(ja+1)*Dz(kb)
               ja = ja + Rca
               kb = kb + Rcb
            ENDDO
            Dz(Arow) = Dz(Arow) + dsumr
            Dz(Arow+1) = Dz(Arow+1) + dsumi
         ELSEIF ( All==5 ) THEN
!
!
            IF ( Jump==2 ) THEN
!
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja))*Dz(kb)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd<=0 ) THEN
                  Z(Arow) = Z(Arow) + sngl(dsumr)
               ELSE
!
                  Dz(Arow) = Dz(Arow) + dsumr
               ENDIF
            ELSEIF ( Jump==3 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja)*Z(kb))
                  dsumi = dsumi + dble(Z(ja)*Z(kb+1))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==4 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja))*Dz(kb)
                  dsumi = dsumi + dble(Z(ja))*Dz(kb+1)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==5 ) THEN
!
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*dble(Z(kb))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd<=0 ) THEN
                  Z(Arow) = Z(Arow) + sngl(dsumr)
               ELSE
                  Dz(Arow) = Dz(Arow) + dsumr
               ENDIF
            ELSEIF ( Jump==6 ) THEN
!
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*Dz(kb)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               Z(Arow) = Z(Arow) + sngl(dsumr)
            ELSEIF ( Jump==7 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*dble(Dz(kb))
                  dsumi = dsumi + Dz(ja)*dble(Dz(kb+1))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==8 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*Dz(kb)
                  dsumi = dsumi + Dz(ja)*Dz(kb+1)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==9 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja)*Z(kb))
                  dsumi = dsumi + dble(Z(ja+1)*Z(kb))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==10 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja))*Dz(kb)
                  dsumi = dsumi + dble(Z(ja+1))*Dz(kb)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==11 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja)*Z(kb)) - dble(Z(ja+1)*Z(kb+1))
                  dsumi = dsumi + dble(Z(ja)*Z(kb+1)) + dble(Z(ja+1)*Z(kb))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               GOTO 300
            ELSEIF ( Jump==12 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja))*Dz(kb)
                  dsumi = dsumi + dble(Z(ja+1))*Dz(kb)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==13 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*dble(Z(kb))
                  dsumi = dsumi + Dz(ja+1)*dble(Z(kb))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==14 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*Dz(kb)
                  dsumi = dsumi + Dz(ja+1)*Dz(kb)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==15 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*dble(Z(kb)) - Dz(ja+1)*dble(Z(kb+1))
                  dsumi = dsumi + Dz(ja)*dble(Z(kb+1)) + Dz(ja+1)*dble(Z(kb))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               IF ( Prcd>0 ) GOTO 300
               GOTO 400
            ELSEIF ( Jump==16 ) THEN
!
               dsumi = dzero
               DO j = jb , je
                  dsumr = dsumr + Dz(ja)*Dz(kb) - Dz(ja+1)*Dz(kb+1)
                  dsumi = dsumi + Dz(ja)*Dz(kb+1) + Dz(ja+1)*Dz(kb)
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               GOTO 400
            ELSE
!
!                      +--------------- MATRIX  B -----------------+
!        MATRIX          REAL        REAL       COMPLEX     COMPLEX
!          A            SINGLE      DOUBLE      SINGLE      DOUBLE
!     ---------------  ----------  ---------  ----------  ----------
!     REAL SINGLE         130         150         170         190
!     REAL DOUBLE         210         230         250         270
!     COMPLEX SINGLE      290         310         330         350
!     COMPLEX DOUBLE      370         390         410         430
!
!
               DO j = jb , je
                  dsumr = dsumr + dble(Z(ja)*Z(kb))
                  ja = ja + Rca
                  kb = kb + Rcb
               ENDDO
               Dz(Arow) = Dz(Arow) + dsumr
            ENDIF
         ELSEIF ( All==6 ) THEN
!
            CALL mesage(-37,0,nam)
            GOTO 99999
         ELSE
!
            DO j = jb , je
               sumr = sumr + Z(ja)*Z(kb)
!
!     DON'T BE SUPRISED TO SEE SOME Z(JA) ARE ZEROS
!     (VAX PACKING ROUTINE ALLOWS UP TO 3 ZEROS BETWEEN STRINGS)
!
               ja = ja + Rca
               kb = kb + Rcb
            ENDDO
            Z(Arow) = Z(Arow) + sumr
         ENDIF
      ENDIF
   ENDIF
   GOTO 500
 300  Dz(Arow) = Dz(Arow) + dsumr
   Dz(Arow+1) = Dz(Arow+1) + dsumi
   GOTO 500
 400  Z(Arow) = Z(Arow) + sngl(dsumr)
   Z(Arow+1) = Z(Arow+1) + sngl(dsumi)
!
!
!     END OF STRING DATA. IF THIS IS NOT THE LAST STRING OF CURRENT
!     ROW OF A, RETURN FOR NEXT STRING
!
 500  nbrstr = nbrstr - 1
   Apoint = Apoint + nbr*Nwda + Prca
   IF ( nbrstr>0 ) THEN
      init = andf(Iz(Apoint),Jhalf)
      GOTO 200
   ENDIF
!
!     END OF A ROW OF MATRIX A.
!     RETURN FOR NEXT ROW IF THIS IS NOT THE LAST ROW IN OPEN CORE.
!     IF IT IS THE LAST ROW, RETURN TO CALLER FOR PACKING OUT THE
!     CURRENT COLUMN OF MATRIX D (IN C ARRAY)
!
 600  l = l - 2
   Arow = Arow + 1
   IF ( Arow<=Arown ) GOTO 100
   RETURN
99999 RETURN
END SUBROUTINE mpy4t
