!*==trd1d.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trd1d
   USE c_machin
   USE c_packx
   USE c_system
   USE c_trdd1
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: aa , ab , alpha , alphb , eta , etb , fab , fabsq , fx , fy , h , tavga , tavgb , x , xh , xk , y
   LOGICAL :: dec
   INTEGER :: file , i , ialg , ibuf1 , icards , icrq , iflag , in1 , in2 , in3 , ip1 , ipx , itabl , itid1 , itid2 , izl , j , k , &
            & kk , l , m , mm , n , ncards , nn , ntabl , numtb , nxx
   INTEGER , DIMENSION(13) , SAVE :: itlist
   INTEGER , DIMENSION(1) :: iz
   INTEGER , SAVE :: kount
   INTEGER , DIMENSION(2) , SAVE :: name , nmtd
   EXTERNAL close , fwdrec , mesage , numtyp , open , pretab , read , sswtch , tab
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THIS ROUTINE COMPUTES NON-LINEAR LOADS FOR TRANSIENT ANALYSIS
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA itlist/4 , 1105 , 11 , 1 , 1205 , 12 , 2 , 1305 , 13 , 3 , 1405 , 14 , 4/
   DATA name/4HNLFT , 4HTRDD/
   DATA nmtd/4HTRD1 , 4HD   /
   DATA kount/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     IDENTIFICATION OF VARIABLES
!
!     NLFT    NON-LINEAR FUNCTION TABLE
!     PNL     NON-LINEAR FORCES --MATRIX
!     DIT     DIRECT INPUT TABLES
!     NLFTP   NON-LINEAR FUNCTION SET SELECTION
!     NOUT    OUT PUT  EVERY NOUT TIME STEPS( PLUS 1 AND NSTEP)
!     ICOUNT  CURRENT INTERATION COUNTER
!     ILOOP   LOOP ON NUMBER OF TIME STEP CHANGES
!     MODAL   LESS THAN ZERO IMPLIES THIS IS A DIRECT FORMULATION
!     LCORE   AMOUNT OF CORE FOR TRD1D
!     ICORE   POINTER TO FIRST CELL OF OPEN CORE
!     IU      POINTER TO LATEST DISPLACEMENT VECTOR
!     IU1     POINTER TO DISPLACEMENT VECTOR -- ONE TIME STEP BACK
!     IP      POINTER TO LOAD VECTOR
!     NMODES  NUMBER OF MODES IN PROBLEM
!     NSTEP   NUMBER OF TIME STEPS
!     ITLIST  LIST OF CARD TYPES FOR DYNAMIC TABLES
!     NROW    SIZE OF SOLUTION SET
!     IBUF1   POINTER TO BUFFER
!     NCARDS  NUMBER OF LOAD CARDS IN SELECTED SET
!     ICARDS  POINTER TO FIRST CARD
!     NTABL   NUMBER OF TABLES
!     ITABL   POINTER TO FIRST TABLE
!     IPNL    MATRIX CONTROL BLOCK FOR PNL
!
!     DESCRIPTION OF TYPES OF NON-LINEAR LOADING
!
!     TYPE    DESCRIPTION
!     ----    -----------
!
!       1     DISPLACEMENT-DEPENDENT NOLIN1 LOAD
!       2     DISPLACEMENT-DEPENDENT/DISPLACEMENT-DEPENDENT NOLIN2 LOAD
!       3     DISPLACEMENT-DEPENDENT NOLIN3 LOAD
!       4     DISPLACEMENT-DEPENDENT NOLIN4 LOAD
!       5     VELOCITY-DEPENDENT NOLIN1 LOAD
!       6     VELOCITY-DEPENDENT/DISPLACEMENT-DEPENDENT NOLIN2 LOAD
!       7     VELOCITY-DEPENDENT NOLIN3 LOAD
!       8     VELOCITY-DEPENDENT NOLIN4 LOAD
!       9     VELOCITY-DEPENDENT/VELOCITY-DEPENDENT NOLIN2 LOAD
!      10     DISPLACEMENT-DEPENDENT/VELOCITY-DEPENDENT NOLIN2 LOAD
!      11     TEMPERATURE-DEPENDENT CONVECTION NON-LINEAR LOAD (FTUBE)
!      12     TEMPERATURE-DEPENDENT EMISSIVITIES-ABSORPTIVITIES, NOLIN5
!      13     DISPLACEMENT-DEPENDENT/VELOCITY-DEPENDENT NOLIN6 LOAD
!      14     VELOCITY-DEPENDENT/DISPLACEMENT-DEPENDENT NOLIN6 LOAD
!
!     DETERMINE ENTRY NUMBER
!
         dec = mach==5 .OR. mach==6 .OR. mach==21
         ipx = ip
!
         IF ( (iloop==1 .AND. icount>1) .OR. (iloop>1 .AND. icount>0) ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ifrst/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FIRST TIME FOR TIME STEP
!
         CALL sswtch(10,ialg)
         ibuf1 = lcore + icore - sysbuf
         file = nlft
         lcore = lcore - sysbuf - 1
         icrq = -lcore
         IF ( lcore>0 ) THEN
            CALL open(*60,nlft,iz(ibuf1),0)
!
!     FIND SELECTED SET ID
!
            CALL read(*80,*20,nlft,iz(icore+1),lcore,0,iflag)
            icrq = lcore
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 20      DO i = 3 , iflag
            k = i + icore
            IF ( iz(k)==nlftp ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL mesage(-31,nlftp,name)
         spag_nextblock_1 = 2
      CASE (2)
!
!     FOUND SET ID -- POSITION TO RECORD IN NLFT
!
         k = i - 3
         IF ( k/=0 ) THEN
            DO i = 1 , k
               CALL fwdrec(*80,nlft)
            ENDDO
         ENDIF
!
!     BRING IN  8 WORDS PER CARD
!     FORMAT =    TYPE,SILD,SILE,A,SILD,SILE,A OR SILD,SILE
!     CONVERT TO  TYPE,ROWP,ROWP,A,ROWP OR A
!     COUNT NUMBER OF CARDS
!
         ncards = 0
         icards = icore + 1
         k = icards
         DO
            icrq = 8 - lcore
            IF ( icrq>0 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL read(*80,*40,nlft,iz(k),8,0,iflag)
            IF ( modal>=0 ) THEN
!
!     MODAL FORM -- CONVERT SILE TO ROW POSITIONS AND STORE IN SILD
!
               IF ( iz(k+2)==0 ) THEN
!
!     LOADED POINT  NOT E-POINT IN MODAL FORMULATION
!
                  CALL mesage(-44,nlftp,iz(k))
                  RETURN
               ELSE
                  iz(k+1) = iz(k+2) + nmodes
                  IF ( iz(k+5)==0 ) THEN
                     CALL mesage(-44,nlftp,iz(k))
                     RETURN
                  ELSE
                     iz(k+4) = iz(k+5) + nmodes
                     IF ( iz(k)==2 .OR. iz(k)==6 .OR. iz(k)==9 .OR. iz(k)==10 ) THEN
                        IF ( iz(k+7)==0 ) THEN
                           CALL mesage(-44,nlftp,iz(k))
                           RETURN
                        ELSE
                           iz(k+6) = iz(k+7) + nmodes
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
!
!     MOVE UP
!
            iz(k+2) = iz(k+4)
            iz(k+4) = iz(k+6)
            k = k + 5
            lcore = lcore - 5
            ncards = ncards + 1
         ENDDO
!
!     END OF RECORD-- DONE
!
 40      CALL close(nlft,1)
!
!     EXTRACT LIST OF  UNIQUE TABLES FROM CARD TYPES 1,5,11 THRU 14
!
         l = icards
         ntabl = 0
         itabl = k
         numtb = 1
         DO i = 1 , ncards
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  izl = iz(l)
                  IF ( izl/=1 .AND. izl/=5 .AND. (izl<11 .OR. izl>14) ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( izl/=11 .AND. izl/=12 ) THEN
                     itid1 = iz(l+4)
                     numtb = 1
                  ELSE
                     izl = iz(l+4)
                     IF ( iz(l)/=11 ) THEN
!
!     NOLIN5 CARD
!
                        nxx = numtyp(iz(l+3))
                        IF ( dec .AND. iz(l+3)>16000 .AND. iz(l+3)<=99999999 ) nxx = 1
                        IF ( nxx==1 ) THEN
                           itid1 = iz(l+3)
                           nxx = numtyp(izl)
                           IF ( dec .AND. izl>16000 .AND. izl<=99999999 ) nxx = 1
                           IF ( nxx/=1 ) THEN
                              numtb = 1
                           ELSE
                              itid2 = iz(l+4)
                              numtb = 2
                           ENDIF
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
!
!     NFTUBE CARD
!
                     nxx = numtyp(izl)
                     IF ( dec .AND. izl>16000 .AND. izl<=99999999 ) nxx = 1
                     IF ( nxx/=1 ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     itid1 = iz(l+4)
                     numtb = 1
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
!
!     FIND OUT IF UNIQUE TABLE
!
                  IF ( ntabl/=0 ) THEN
                     DO m = 1 , ntabl
                        k = itabl + m
                        IF ( iz(k)==itid1 ) THEN
                           spag_nextblock_2 = 3
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                  ENDIF
!
!     NEW TABLE
!
                  ntabl = ntabl + 1
                  k = itabl + ntabl
                  iz(k) = itid1
                  spag_nextblock_2 = 3
               CASE (3)
                  IF ( numtb==1 ) THEN
                     l = l + 5
                  ELSE
                     numtb = 1
                     itid1 = itid2
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
!
         iz(itabl) = ntabl
         lcore = lcore - ntabl - 1
         icrq = -lcore
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ntabl/=0 ) THEN
!
!     INITIALIZE TABLES
!
            k = itabl + ntabl + 1
            CALL pretab(dit,iz(k),iz(k),iz(ibuf1),lcore,l,iz(itabl),itlist)
            lcore = lcore - l
            IF ( ialg/=0 ) THEN
               in1 = k + l - 1
               in2 = in1 + nrow
               in3 = in2 + nrow
               lcore = lcore - 3*nrow
               icrq = -lcore
               IF ( lcore<0 ) THEN
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     ZERO LOAD VECTORS
!
               DO i = 1 , nrow
                  k = in1 + i
                  z(k) = 0.0
                  k = in2 + i
                  z(k) = 0.0
                  k = in3 + i
                  z(k) = 0.0
               ENDDO
            ENDIF
         ENDIF
         RETURN
      CASE (3)
!
!     COMPUTE LOADS
!
         k = icards + ncards*5 - 1
         IF ( ialg/=0 ) THEN
            ipx = in1
            DO i = 1 , nrow
               l = in1 + i
               z(l) = 0.0
            ENDDO
         ENDIF
!
!     LOOP THRU EACH LOAD CARD OR COLLECTION (NOLIN5, NOLIN6)
!
         h = 1.0/deltat
         i = icards
         spag_nextblock_1 = 4
      CASE (4)
         fx = 0.0
         fy = 1.0
         m = iu + iz(i+2)
         mm = iu + iz(i+4)
         n = iu1 + iz(i+2)
         nn = iu1 + iz(i+4)
         x = z(m)
         y = (x-z(n))*h
         l = iz(i)
!     L  =     1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14
         IF ( l/=2 ) THEN
            IF ( l==3 ) THEN
!
!     NOLIN  3
!
               IF ( x>0.0 ) fx = x**z(i+4)
            ELSEIF ( l==4 ) THEN
!
!     NOLIN 4
!
               IF ( x<0.0 ) fx = -abs(x)**z(i+4)
            ELSE
               IF ( l==5 ) THEN
                  x = y
               ELSEIF ( l==6 ) THEN
                  x = y
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( l==7 ) THEN
                  x = y
                  IF ( x>0.0 ) fx = x**z(i+4)
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( l==8 ) THEN
                  x = y
                  IF ( x<0.0 ) fx = -abs(x)**z(i+4)
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( l==9 ) THEN
                  x = y
                  fx = x*(z(mm)-z(nn))*h
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( l==10 ) THEN
                  fx = x*(z(mm)-z(nn))*h
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( l==11 ) THEN
!
!     NFTUBE.  LOOKUP VDOT IF NEEDED
!
                  fx = z(i+4)
                  izl = iz(i+4)
                  nxx = numtyp(izl)
                  IF ( dec .AND. izl>16000 .AND. izl<=99999999 ) nxx = 1
                  IF ( nxx==1 ) CALL tab(iz(i+4),tim,fx)
                  IF ( fx>=0.0 ) m = iu + iz(i+1)
                  fx = fx*z(m)
                  l = ipx + iz(i+2)
                  z(l) = z(l) + fx*z(i+3)
                  fy = -1.0
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( l==12 ) THEN
!
!     NOLIN5
!
!     A. COMPUTE SURFACE AVERAGE TEMPERATURES
!
                  mm = 0
                  nn = 0
                  tavga = 0.0
                  tavgb = 0.0
                  j = 1
                  DO l = 1 , 4
                     IF ( l==3 ) j = 6
                     m = iz(i+j)
                     IF ( m/=0 ) THEN
                        m = iu + m
                        tavga = tavga + z(m)
                        mm = mm + 1
                     ENDIF
                     m = iz(i+j+10)
                     IF ( m/=0 ) THEN
                        m = iu + m
                        tavgb = tavgb + z(m)
                        nn = nn + 1
                     ENDIF
                     j = j + 1
                  ENDDO
                  tavga = tavga/float(mm)
                  tavgb = tavgb/float(nn)
                  aa = z(i+3)
                  ab = z(i+4)
                  fab = z(i+8)
                  fabsq = fab*fab
                  eta = z(i+13)
                  etb = z(i+14)
                  nxx = numtyp(iz(i+13))
                  IF ( dec .AND. iz(i+13)>16000 .AND. iz(i+13)<=99999999 ) nxx = 1
                  IF ( nxx==1 ) CALL tab(iz(i+13),tavga,eta)
                  nxx = numtyp(iz(i+14))
                  IF ( dec .AND. iz(i+14)>16000 .AND. iz(i+14)<=99999999 ) nxx = 1
                  IF ( nxx==1 ) CALL tab(iz(i+14),tavgb,etb)
                  alpha = z(i+18)
                  alphb = z(i+19)
                  nxx = numtyp(iz(i+18))
                  IF ( dec .AND. iz(i+18)>16000 .AND. iz(i+18)<=99999999 ) nxx = 1
                  IF ( nxx==1 ) CALL tab(iz(i+18),tavga,alpha)
                  nxx = numtyp(iz(i+19))
                  IF ( dec .AND. iz(i+19)>16000 .AND. iz(i+19)<=99999999 ) nxx = 1
                  IF ( nxx==1 ) CALL tab(iz(i+19),tavgb,alphb)
                  alpha = alpha - 1.0
                  alphb = alphb - 1.0
!
!     B. COMPUTE DENOMINATOR
!
                  xh = sigma*eta*(tavga+tabs)**4
                  xk = sigma*etb*(tavgb+tabs)**4
                  fx = alpha*fab*xk - aa*xh + fab*xk - (alphb*fabsq*xh)/ab
                  fy = alphb*fab*xh - ab*xk + fab*xh - (alpha*fabsq*xk)/aa
                  fab = 1.0 - (alpha*alphb/aa)*(fabsq/ab)
                  fx = fx/(fab*float(mm))
                  fy = fy/(fab*float(nn))
!
!     C. APPLY FORCES ON AREAS A AND  B
!
                  j = 1
                  DO l = 1 , 4
                     IF ( l==3 ) j = 6
                     m = iz(i+j)
                     IF ( m/=0 ) THEN
                        m = ipx + m
                        z(m) = z(m) + fx
                     ENDIF
                     m = iz(i+j+10)
                     IF ( m/=0 ) THEN
                        m = ipx + m
                        z(m) = z(m) + fy
                     ENDIF
                     j = j + 1
                  ENDDO
                  i = i + 20
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( l==13 ) THEN
!
!     NOLIN 6
!
                  x = y
                  fy = x*abs(x)
                  x = z(m)
               ELSEIF ( l==14 ) THEN
                  y = z(mm)
                  fy = y*abs(y)
               ENDIF
!
!     NOLIN 1
!
               CALL tab(iz(i+4),x,fx)
            ENDIF
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     NOLIN 2
!
         y = z(mm)
         fx = x*y
         spag_nextblock_1 = 6
      CASE (6)
!
!     FINISH APPLYING SCALE FACTOR AND ADD
!
         l = ipx + iz(i+1)
         z(l) = z(l) + fx*fy*z(i+3)
         IF ( abs(z(l))<1.0E-36 ) z(l) = 0.0
         IF ( abs(z(l))>=1.0E+36 ) THEN
            kount = kount + 1
            IF ( kount==1 .OR. kount==4 ) WRITE (iout,99001)
99001       FORMAT (/1X,28(4H****),/)
            IF ( kount<=3 ) WRITE (iout,99002) uwm , z(l)
99002       FORMAT (A25,' 3309, UNUSUALLY LARGE VALUE COMPUTED FOR NONLINEAR',' FORCING FUNCTION',5X,E15.5)
         ENDIF
         i = i + 5
         spag_nextblock_1 = 7
      CASE (7)
         IF ( i<k ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF LOAD LOOP
!
!
!     DONE
!
         IF ( ialg/=0 ) THEN
            DO i = 1 , nrow
!
!     SUM OVER LAST THREE LOADS
!
               l = ip + i
               k = in1 + i
               m = in2 + i
               kk = in3 + i
               z(l) = z(l) + (z(k)+z(m)+z(kk))/3.0
            ENDDO
!
!     SWITCH POINTERS
!
            k = in1
            in1 = in2
            in2 = in3
            in3 = k
         ENDIF
         RETURN
!
!     ERROR MESSAGES
!
 60      WRITE (iout,99003) ufm
99003    FORMAT (A23,', NON-LINEAR FORCING LOAD (NLFT) WAS NOT GENERATED ','PREVIOUSLY')
         ip1 = -37
         spag_nextblock_1 = 8
      CASE (8)
         CALL mesage(ip1,file,nmtd)
         RETURN
 80      ip1 = -2
         spag_nextblock_1 = 8
      CASE (9)
         ip1 = -8
         file = icrq
         spag_nextblock_1 = 8
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trd1d
