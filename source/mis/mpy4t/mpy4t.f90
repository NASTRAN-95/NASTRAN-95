!*==mpy4t.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE c_machin
   USE c_mpyadx
   USE c_mpyadz
   USE c_mpyqt4
   USE c_type
   USE c_unpakx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Iz
   REAL , DIMENSION(1) :: Z
   REAL(REAL64) , DIMENSION(1) :: Dz
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dsumi , dsumr
   REAL(REAL64) , SAVE :: dzero
   INTEGER :: init , ipoint , j , ja , jb , je , kb , l , last , nbr , nbrstr
   INTEGER , DIMENSION(2) , SAVE :: nam
   REAL :: sumi , sumr
   EXTERNAL andf , mesage , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (dsumr,sumr) , (dsumi,sumi)
   DATA nam/4HMPY4 , 1HT/ , dzero/0.0D+0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         arow = arow1
         l = firstl
         spag_nextblock_1 = 2
      CASE (2)
         apoint = Iz(l)
         IF ( apoint==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         last = rshift(Iz(l-1),ihalf)
         init = andf(Iz(apoint),jhalf)
         IF ( init>jj .OR. last<ii ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nbrstr = andf(Iz(l-1),jhalf)
         spag_nextblock_1 = 3
      CASE (3)
         nbr = rshift(Iz(apoint),ihalf)
         IF ( init>jj ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( init+nbr>=ii ) THEN
            jb = max0(init,ii)
            je = min0(init+nbr-1,jj)
            IF ( jb<=je ) THEN
               ipoint = apoint + (jb-init+1)*prca
               ja = (ipoint-1)/prca + 1
               kb = (jb-ii)*rcb + jbb
               dsumr = dzero
               IF ( all==2 ) THEN
!
                  DO j = jb , je
                     dsumr = dsumr + Dz(ja)*Dz(kb)
                     ja = ja + rca
                     kb = kb + rcb
                  ENDDO
                  Dz(arow) = Dz(arow) + dsumr
               ELSEIF ( all==3 ) THEN
!
                  sumi = 0.0
                  DO j = jb , je
                     sumr = sumr + Z(ja)*Z(kb) - Z(ja+1)*Z(kb+1)
                     sumi = sumi + Z(ja)*Z(kb+1) + Z(ja+1)*Z(kb)
                     ja = ja + rca
                     kb = kb + rcb
                  ENDDO
                  Z(arow) = Z(arow) + sumr
                  Z(arow+1) = Z(arow+1) + sumi
               ELSEIF ( all==4 ) THEN
!
                  dsumi = dzero
                  DO j = jb , je
                     dsumr = dsumr + Dz(ja)*Dz(kb) - Dz(ja+1)*Dz(kb+1)
                     dsumi = dsumi + Dz(ja)*Dz(kb+1) + Dz(ja+1)*Dz(kb)
                     ja = ja + rca
                     kb = kb + rcb
                  ENDDO
                  Dz(arow) = Dz(arow) + dsumr
                  Dz(arow+1) = Dz(arow+1) + dsumi
               ELSEIF ( all==5 ) THEN
!
!
                  IF ( jump==2 ) THEN
!
                     DO j = jb , je
                        dsumr = dsumr + dble(Z(ja))*Dz(kb)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        Z(arow) = Z(arow) + sngl(dsumr)
                     ELSE
!
                        Dz(arow) = Dz(arow) + dsumr
                     ENDIF
                  ELSEIF ( jump==3 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + dble(Z(ja)*Z(kb))
                        dsumi = dsumi + dble(Z(ja)*Z(kb+1))
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==4 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + dble(Z(ja))*Dz(kb)
                        dsumi = dsumi + dble(Z(ja))*Dz(kb+1)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==5 ) THEN
!
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*dble(Z(kb))
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        Z(arow) = Z(arow) + sngl(dsumr)
                     ELSE
                        Dz(arow) = Dz(arow) + dsumr
                     ENDIF
                  ELSEIF ( jump==6 ) THEN
!
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*Dz(kb)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     Z(arow) = Z(arow) + sngl(dsumr)
                  ELSEIF ( jump==7 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*dble(Dz(kb))
                        dsumi = dsumi + Dz(ja)*dble(Dz(kb+1))
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==8 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*Dz(kb)
                        dsumi = dsumi + Dz(ja)*Dz(kb+1)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==9 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + dble(Z(ja)*Z(kb))
                        dsumi = dsumi + dble(Z(ja+1)*Z(kb))
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==10 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + dble(Z(ja))*Dz(kb)
                        dsumi = dsumi + dble(Z(ja+1))*Dz(kb)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==11 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + dble(Z(ja)*Z(kb)) - dble(Z(ja+1)*Z(kb+1))
                        dsumi = dsumi + dble(Z(ja)*Z(kb+1)) + dble(Z(ja+1)*Z(kb))
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==12 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + dble(Z(ja))*Dz(kb)
                        dsumi = dsumi + dble(Z(ja+1))*Dz(kb)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==13 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*dble(Z(kb))
                        dsumi = dsumi + Dz(ja+1)*dble(Z(kb))
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==14 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*Dz(kb)
                        dsumi = dsumi + Dz(ja+1)*Dz(kb)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==15 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*dble(Z(kb)) - Dz(ja+1)*dble(Z(kb+1))
                        dsumi = dsumi + Dz(ja)*dble(Z(kb+1)) + Dz(ja+1)*dble(Z(kb))
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     IF ( prcd<=0 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( jump==16 ) THEN
!
                     dsumi = dzero
                     DO j = jb , je
                        dsumr = dsumr + Dz(ja)*Dz(kb) - Dz(ja+1)*Dz(kb+1)
                        dsumi = dsumi + Dz(ja)*Dz(kb+1) + Dz(ja+1)*Dz(kb)
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
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
                        ja = ja + rca
                        kb = kb + rcb
                     ENDDO
                     Dz(arow) = Dz(arow) + dsumr
                  ENDIF
               ELSEIF ( all==6 ) THEN
!
                  CALL mesage(-37,0,nam)
                  RETURN
               ELSE
!
                  DO j = jb , je
                     sumr = sumr + Z(ja)*Z(kb)
!
!     DON'T BE SUPRISED TO SEE SOME Z(JA) ARE ZEROS
!     (VAX PACKING ROUTINE ALLOWS UP TO 3 ZEROS BETWEEN STRINGS)
!
                     ja = ja + rca
                     kb = kb + rcb
                  ENDDO
                  Z(arow) = Z(arow) + sumr
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 6
      CASE (4)
         Dz(arow) = Dz(arow) + dsumr
         Dz(arow+1) = Dz(arow+1) + dsumi
         spag_nextblock_1 = 6
      CASE (5)
         Z(arow) = Z(arow) + sngl(dsumr)
         Z(arow+1) = Z(arow+1) + sngl(dsumi)
         spag_nextblock_1 = 6
      CASE (6)
!
!
!     END OF STRING DATA. IF THIS IS NOT THE LAST STRING OF CURRENT
!     ROW OF A, RETURN FOR NEXT STRING
!
         nbrstr = nbrstr - 1
         apoint = apoint + nbr*nwda + prca
         IF ( nbrstr>0 ) THEN
            init = andf(Iz(apoint),jhalf)
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     END OF A ROW OF MATRIX A.
!     RETURN FOR NEXT ROW IF THIS IS NOT THE LAST ROW IN OPEN CORE.
!     IF IT IS THE LAST ROW, RETURN TO CALLER FOR PACKING OUT THE
!     CURRENT COLUMN OF MATRIX D (IN C ARRAY)
!
         l = l - 2
         arow = arow + 1
         IF ( arow<=arown ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mpy4t
