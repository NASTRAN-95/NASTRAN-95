
SUBROUTINE shlsts(Elid,Pid,Tlam,Epsumi,Epscmi)
   IMPLICIT NONE
   REAL Degrad , Dum1(30) , Dum2(100) , Dum3(31) , Dum4(163) , Forsul(37) , Pi , Raddeg , Stres(69) , Strin(69) , Twopi , Z(1)
   INTEGER Edest , Fdest , Ipcmp , Ipcmp1 , Ipcmp2 , Iz(1) , Ksdrde(200) , Npcmp , Npcmp1 , Npcmp2 , Oef1l , Oes1al , Oes1l , Sdest
   LOGICAL Forreq , Grids , Gridss , Layer , Layers , Stnreq , Strcur , Stsreq , Vonms , Vonmss
   COMMON /condas/ Pi , Twopi , Raddeg , Degrad
   COMMON /outreq/ Stsreq , Stnreq , Forreq , Strcur , Grids , Vonms , Layer , Gridss , Vonmss , Layers
   COMMON /sdr2c1/ Ipcmp , Npcmp , Ipcmp1 , Npcmp1 , Ipcmp2 , Npcmp2
   COMMON /sdr2de/ Ksdrde
   COMMON /sdr2x2/ Dum1 , Oes1l , Oef1l
   COMMON /sdr2x7/ Dum2 , Stres , Dum3 , Forsul , Dum4 , Strin
   COMMON /zzzzzz/ Z
   INTEGER Elid , Pid
   REAL Tlam
   REAL Epscmi(6,1) , Epsumi(6,1)
   REAL c , c2 , ei(2) , epslcf(3) , epslcm(3) , epslr(3) , epsluf(3) , epslum(3) , ernar(2) , ernshr(2) , ernsrr(2) , fb(2) ,      &
      & fbmax , fbond , fbondr , fimax , fimaxr , findex , findxr , fpmax , gg(9) , s , s2 , sb , stresl(3) , strslr(3) , theta ,   &
      & ti , trans(9) , trnar(2) , trnshr(2) , trnsrr(2) , ultstn(6) , v(2) , zbar(2) , zk , zk1 , zref , zsubi
   INTEGER elemid , fthr , half , icontr , iflag1 , iflag2 , iflag3 , igi , ihalf , ip , ipc11 , ipc21 , ipoint , ir , istr ,       &
         & itype , k , kk , lamopt , ll , lpc11 , lpcomp , lyrid , mem , nlay , nlayer , nstrqt , pcomp , pcomp1 , pcomp2 , pidloc ,&
         & souti , strinf , sym , symmem
   LOGICAL force , nonmem , pcmp , pcmp1 , pcmp2 , strain , stress , symlay , trnflx
!
!     TO PERFORM LAYER STRAIN, STRESS AND FORCE CALCULATIONS FOR THE
!     2-D SHELL ELEMENTS.
!     ONLY THE ELEMENT CENTER VALUES ARE CONSIDERED
!
!     INPUT :
!           ELID   - ELEMENT ID
!           PID    - COMPOSITE PROPERTY ID
!           TLAM   - AVERAGE ELEMENT THICKNESS
!           EPSUMI - UNCORRECTED STRAINS IN MATERIAL COORD. SYSTEM
!           EPSCMI - CORRECTED STRAINS IN MATERIAL COORD. SYSTEM
!          /CONDAS/- TRIGONOMETRIC CONSTATNTS
!          /OUTREQ/- OUTPUT REQUEST LOGICAL FLAGS
!
!     OUTPUT:
!           OUTPUT DATA ARE WRITTEN DIRECTLY TO EACH APPROPRIATE OUTPUT
!           FILE - OEF1L, OES1L/OES1AL
!
!
!     LAYER STRESS/STRAIN OUTPUT BLOCK FOR EACH CTRIA3 ELEMENT
!
!         1.    10*ELEMENT ID + DEVICE CODE
!         2.    NLAYER - NUMBER OF OUTPUT LAYERS
!         3.    TYPE OF FAILURE THEORY SELECTED
!
!         4.    LAYER ID
!        5-7.   LAYER STRESSES/STRAINS
!         8.    LAYER FAILURE INDEX, FI
!         9.    IFLAG1 = 1 IF FI.GE.0.999
!                      = 0 OTHERWISE
!       10-11.  INTERLAMINAR SHEAR STRESSES/STRAINS
!        12.    SHEAR BONDING INDEX, FB
!        13.    IFLAG2 = 1 IF FB.GE.0.999
!                      = 0 OTHERWISE
!         :
!         :     REPEAT 4-13 NLAYER TIMES FOR EACH LAYER
!
!       LAST-1. MAXIMUM FAILURE INDEX OF LAMINATE, FIMAX
!        LAST.  IFLAG3 = 1 IF FIMAX.GE.0.999
!                      = 0 OTHERWISE
!
!
!     FORCE OUTPUT BLOCK
!
!         1.    10*ELEMENT ID + DEVICE CODE
!        2-9.   FORCE RESULTANTS:
!                 MEMBRANE        BENDING     TRANSVERSE
!               -- FORCES --    - MOMENTS -  SHEAR FROCES
!               FX,  FY, FXY,   MX, MY, MXY,    VX, VY
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Sdest,Ksdrde(26)) , (Fdest,Ksdrde(33)) , (Edest,Ksdrde(148)) , (Oes1l,Oes1al)
   DATA symmem , mem , sym , pcomp , pcomp1 , pcomp2 , strinf/3 , 2 , 1 , 0 , 1 , 2 , 5/
!
!     INITIALIZE
!
   zref = -Tlam/2.0
   findex = 0.0
   fbond = 0.0
   fpmax = 0.0
   fbmax = 0.0
   fimax = 0.0
!
   DO ll = 1 , 2
      ernar(ll) = 0.0
      trnar(ll) = 0.0
      ernshr(ll) = 0.0
      trnshr(ll) = 0.0
   ENDDO
!
   force = Forreq .AND. Layer
   stress = Stsreq .AND. Layer
   strain = Stnreq .AND. Layers
!
   itype = -1
   lpcomp = Ipcmp + Npcmp + Npcmp1 + Npcmp2
   pcmp = Npcmp>0
   pcmp1 = Npcmp1>0
   pcmp2 = Npcmp2>0
!
   IF ( force ) THEN
!
!     WRITE FORCE RESULTANTS TO OEF1L IF REQUESTED
!
      elemid = 10*Elid + Fdest
      CALL write(Oef1l,elemid,1,0)
      CALL write(Oef1l,Forsul(3),8,0)
   ENDIF
!
!     FORCE REQUEST HAS BEEN PROCESSED. IF NO MORE REQUESTS WE ARE DONE.
!     IF NOT, PREPARE FOR OTHER REQUESTS.
!     ISSUE ERROR IF PCOMPI DATA HAS NOT BEEN READ INTO CORE.
!
   IF ( .NOT.(stress .OR. strain) ) GOTO 99999
!
!     START WRITING STRESS/STRAIN OUTPUT TO OES1L/OES1AL
!     (NOTE - OES1L AND OES1AL ARE SAME FILE IN COSMIC/NASTRAN)
!
!     1.  10*ELEMENT ID + DEVICE CODE
!
   IF ( lpcomp==Ipcmp ) THEN
!
!
!     ERROR MESSAGE
!
!     NO PCOMP, PCOMP1, PCOMP2 FOUND
!
      CALL mesage(-30,223,Elid)
      GOTO 99999
   ELSE
      elemid = 10*Elid + Sdest
      IF ( strain ) elemid = 10*Elid + Edest
      CALL write(Oes1l,elemid,1,0)
!
!     DETERMINE  IF INTERLAMINAR SHEAR STRESS CALCULATIONS ARE REQUIRED
!     BY CHECKING THE TRANSVERSE SHEAR STRESS RESULTANTS QX AND QY
!
      v(1) = Forsul(9)
      v(2) = Forsul(10)
      trnflx = v(1)/=0.0 .OR. v(2)/=0.0
!
!     LOCATE PID BY PERFORMING A SEQUENTIAL SEARCH OF THE PCOMPI DATA
!     BLOCK WHICH IS IN CORE.
!
!     SEARCH FOR PID IN PCOMP DATA
!
      IF ( pcmp ) THEN
         ip = Ipcmp
         IF ( Iz(ip)==Pid ) GOTO 200
         ipc11 = Ipcmp1 - 1
         DO ip = Ipcmp , ipc11
            IF ( Iz(ip)==-1 .AND. ip<ipc11 ) THEN
               IF ( Iz(ip+1)==Pid ) GOTO 100
            ENDIF
         ENDDO
      ENDIF
!
!     SEARCH FOR PID IN PCOMP1 DATA
!
      IF ( pcmp1 ) THEN
         ip = Ipcmp1
         IF ( Iz(ip)==Pid ) GOTO 400
         ipc21 = Ipcmp2 - 1
         DO ip = Ipcmp1 , ipc21
            IF ( Iz(ip)==-1 .AND. ip<ipc21 ) THEN
               IF ( Iz(ip+1)==Pid ) GOTO 300
            ENDIF
         ENDDO
      ENDIF
!
!     SEARCH FOR PID IN PCOMP2 DATA
!
      IF ( .NOT.pcmp2 ) THEN
         CALL mesage(-30,223,Elid)
         GOTO 99999
      ELSE
         ip = Ipcmp2
         IF ( Iz(ip)==Pid ) GOTO 600
         lpc11 = lpcomp - 1
         DO ip = Ipcmp2 , lpc11
            IF ( Iz(ip)==-1 .AND. ip<lpc11 ) THEN
               IF ( Iz(ip+1)==Pid ) GOTO 500
            ENDIF
         ENDDO
!
!     PID WAS NOT LOCATED; ISSUE ERROR
!
         CALL mesage(-30,223,Elid)
         GOTO 99999
      ENDIF
   ENDIF
!
!     PID WAS LOCATED; DETERMINE TYPE
!
!     FOR PCOMP BULK DATA DETERMINE HOW MANY LAYERS HAVE THE STRESS/
!     STRAIN OUTPUT REQUEST (SOUTI).
!     FOR PCOMP1 OR PCOMP2 BULK DATA ENTRIES LAYER STRESSES/STRAINS ARE
!     OUTPUT FOR ALL LAYERS.
!
 100  ip = ip + 1
 200  itype = pcomp
   pidloc = ip
   nlay = Iz(pidloc+1)
   nlayer = nlay
   nstrqt = 0
   DO k = 1 , nlay
      IF ( Iz(pidloc+8+4*k)==1 ) nstrqt = nstrqt + 1
   ENDDO
   nlayer = nstrqt
   ipoint = pidloc + 8 + 4*nlay
   icontr = ipoint + 27*nlay
   GOTO 700
!
 300  ip = ip + 1
 400  itype = pcomp1
   pidloc = ip
   nlay = Iz(pidloc+1)
   nlayer = nlay
   ipoint = pidloc + 8 + nlay
   icontr = ipoint + 25 + 2*nlay
   GOTO 700
!
 500  ip = ip + 1
 600  itype = pcomp2
   pidloc = ip
   nlay = Iz(pidloc+1)
   nlayer = nlay
   ipoint = pidloc + 8 + 2*nlay
   icontr = ipoint + 25 + 2*nlay
!
!     DETERMINE GENERAL COMPOSITE PROPERTY VALUES
!
!     LAMOPT - LAMINATION GENERATION OPTION
!            = ALL    = 0 (ALL PLYS SPECIFIED, DEFAULT)
!            = SYM    = 1 (SYMMETRIC)
!            = MEM    = 2 (MEMBRANE ONLY)
!            = SYMMEM = 3 (SYMMETRIC-MEMBRANE)
!
!     FTHR   - FAILURE THEORY
!            = 1    HILL
!            = 2    HOFFMAN
!            = 3    TSAI-WU
!            = 4    MAX-STRESS
!            = 5    MAX-STRAIN
!
!     SB     - SHEAR BONDING STRENGTH
!
 700  lamopt = Iz(pidloc+8)
   fthr = Iz(pidloc+5)
   sb = Z(pidloc+4)
   ei(1) = Z(icontr+1)
   ei(2) = Z(icontr+2)
   zbar(1) = Z(icontr+3)
   zbar(2) = Z(icontr+4)
!
   nonmem = lamopt/=mem .AND. lamopt/=symmem
   symlay = lamopt==sym .OR. lamopt==symmem
   IF ( symlay ) nlayer = 2*nlayer
   IF ( nlayer/=0 ) THEN
!
!     CONTINUE TO WRITE LAYER-INDEPENDENT DATA TO OES1L/OES1AL
!
!     2.  NLAYER - NUMBER OF LAYERS FOR LAMINATE
!     3.  TYPE OF FAILURE THEORY SELECTED
!
      CALL write(Oes1l,nlayer,1,0)
      CALL write(Oes1l,fthr,1,0)
!
!     START THE LOOP OVER LAYERS
!
      zk = zref
      half = 1
      IF ( symlay ) half = 2
!
      DO ihalf = 1 , half
         DO kk = 1 , nlay
            k = kk
            IF ( ihalf==2 ) k = nlay + 1 - kk
!
!     OBTAIN LAYER K INFORMATION
!     - THE BOUNDARIES
!     - THE DISTANCE FROM THE REFERENCE SURFACE TO THE MIDDLE OF LAYER
!     - LAYER THICKNESS
!     - STRESS OUTPUT REQUEST (SOUTI) FOR PCOMP BULK DATA
!       (NOT SUPPORTED FOR PCOMP1 OR PCOMP2 BULK DATA)
!
            zk1 = zk
            IF ( itype==pcomp ) zk = zk1 + Z(pidloc+6+4*k)
            IF ( itype==pcomp1 ) zk = zk1 + Z(pidloc+7)
            IF ( itype==pcomp2 ) zk = zk1 + Z(pidloc+7+2*k)
            zsubi = (zk+zk1)/2.0
            ti = zk - zk1
            souti = 1
            IF ( itype==pcomp ) souti = Iz(pidloc+8+4*k)
!
!     LAYER MATERIAL PROPERTIES
!
            DO igi = 1 , 9
               gg(igi) = Z(ipoint+igi)
            ENDDO
!
!     LAYER ULTIMATE STRENGTHS
!
            DO ir = 1 , 6
               ultstn(ir) = Z(ipoint+16+ir)
            ENDDO
!
!     LAYER ORIENTATION
!
            IF ( itype==pcomp ) theta = Z(pidloc+7+4*k)
            IF ( itype==pcomp1 ) theta = Z(pidloc+8+k)
            IF ( itype==pcomp2 ) theta = Z(pidloc+8+2*k)
            theta = theta*Degrad
!
!     BUILD THE STRAIN TENSOR TRANSFORMATION TO TRANSFORM
!     LAYER STRAINS FROM MATERIAL TO FIBER DIRECTION.
!
            c = cos(theta)
            c2 = c*c
            s = sin(theta)
            s2 = s*s
!
            trans(1) = c2
            trans(2) = s2
            trans(3) = c*s
            trans(4) = s2
            trans(5) = c2
            trans(6) = -c*s
            trans(7) = -2.0*c*s
            trans(8) = 2.0*c*s
            trans(9) = c2 - s2
!
!     CALCULATE THE CORRECTED AND UNCORRECTED STRAIN VECTORS AT ZSUBI
!     IN THE MATERIAL COORD. SYSTEM, THENTRANSFORM STRAINS FROM MATERIAL
!     TO FIBER COORD. SYSTEM AND CALCULATE THE LAYER STRESS VECTOR IN
!     THE FIBER COORD. SYSTEM
!
            DO ir = 1 , 3
               epslcm(ir) = Epscmi(ir,1) - zsubi*Epscmi(ir+3,1)
               epslum(ir) = Epsumi(ir,1) - zsubi*Epsumi(ir+3,1)
            ENDDO
!
            CALL gmmats(trans(1),3,3,0,epslcm(1),3,1,0,epslcf(1))
            CALL gmmats(trans(1),3,3,0,epslum(1),3,1,0,epsluf(1))
            CALL gmmats(gg(1),3,3,0,epslcf,3,1,0,stresl(1))
!
            IF ( fthr>0 ) THEN
!
!     COMPUTE FAILURE INDEX FOR THIS LAYER AND THE MAXIMUM FAILURE INDEX
!
               IF ( fthr==strinf ) CALL failrs(fthr,ultstn,epsluf,findex)
               IF ( fthr/=strinf ) CALL failrs(fthr,ultstn,stresl,findex)
               IF ( abs(findex)>=abs(fpmax) ) fpmax = findex
            ENDIF
!
            IF ( .NOT.(.NOT.trnflx .OR. .NOT.nonmem) ) THEN
!
!     CALCULATE INTERLAMINAR SHEAR STRESSES AND STRAINS
!
               IF ( itype==pcomp ) icontr = ipoint + 25
               IF ( itype==pcomp1 ) icontr = ipoint + 23 + 2*k
               IF ( itype==pcomp2 ) icontr = ipoint + 23 + 2*k
               DO ir = 1 , 2
                  ernar(ir) = ernar(ir) + ti*(zbar(ir)-zsubi)
                  trnar(ir) = trnar(ir) + ti*(zbar(ir)-zsubi)*Z(icontr+ir)
               ENDDO
!
               DO ir = 1 , 2
                  trnshr(ir) = v(ir)*trnar(ir)/ei(ir)
                  ernshr(ir) = v(ir)*ernar(ir)/ei(ir)
               ENDDO
!
               IF ( sb>0.0 ) THEN
!
!     CALCULATE SHEAR BONDING FAILURE INDEX, FB, AND THE MAX SHEAR
!     BONDING INDEX, FBMAX.
!
                  DO ir = 1 , 2
                     fb(ir) = abs(trnshr(ir))/sb
                  ENDDO
!
                  fbond = fb(1)
                  IF ( fb(2)>fb(1) ) fbond = fb(2)
                  IF ( fbond>=fbmax ) fbmax = fbond
               ENDIF
            ENDIF
!
            IF ( souti/=0 ) THEN
!
!     CONTINUE TO WRITE LAYER-DEPENDENT DATA TO OES1L AND OES1AL
!
!       4.   LAYER ID, LYRID
!     5,6,7. LAYER STRESSES/STRAINS
!       8.   LAYER FAILURE INDEX, FINDXR
!       9.   IFLAG1 (=1 IF FINDXR.GE.0.999, DEFAULT=0)
!     10,11. INTERLAMINAR SHEAR STRESSES/STRAINS
!      12.   SHEAR BONDING FAILURE INDEX, FBONDR
!      13.   IFLAG2 (=1 IF FBONDR.GE.0.999, DEFAULT=0)
!       :    REPEAT 4-13 FOR NUMBER OF LAYER WITH LAYER STRESS/STRAIN
!       :    REQUEST
!
!
               lyrid = k
               IF ( ihalf==2 ) lyrid = nlay + kk
!
               findxr = findex
               iflag1 = 0
               IF ( abs(findex)>=0.999 ) iflag1 = 1
!
               fbondr = fbond
               iflag2 = 0
               IF ( abs(fbond)>=0.999 ) iflag2 = 1
!
               IF ( stress ) THEN
                  DO istr = 1 , 3
                     strslr(istr) = stresl(istr)
                  ENDDO
                  trnsrr(1) = trnshr(1)
                  trnsrr(2) = trnshr(2)
                  CALL write(Oes1l,lyrid,1,0)
                  CALL write(Oes1l,strslr(1),3,0)
                  CALL write(Oes1l,findxr,1,0)
                  CALL write(Oes1l,iflag1,1,0)
                  CALL write(Oes1l,trnsrr(1),2,0)
                  CALL write(Oes1l,fbondr,1,0)
                  CALL write(Oes1l,iflag2,1,0)
               ENDIF
!
               IF ( strain ) THEN
                  DO istr = 1 , 3
                     epslr(istr) = epsluf(istr)
                  ENDDO
                  ernsrr(1) = ernshr(1)
                  ernsrr(2) = ernshr(2)
                  CALL write(Oes1al,lyrid,1,0)
                  CALL write(Oes1al,epslr(1),3,0)
                  CALL write(Oes1al,findxr,1,0)
                  CALL write(Oes1al,iflag1,1,0)
                  CALL write(Oes1al,ernsrr(1),2,0)
                  CALL write(Oes1al,fbondr,1,0)
                  CALL write(Oes1al,iflag2,1,0)
               ENDIF
            ENDIF
!
!     UPDATE IPOINT FOR PCOMP BULK DATA ENTRY
!
            IF ( itype==pcomp ) THEN
               IF ( ihalf==1 .AND. k/=nlay ) ipoint = ipoint + 27
               IF ( ihalf==2 ) ipoint = ipoint - 27
            ENDIF
         ENDDO
      ENDDO
!
!     END OF LOOP OVER LAYERS
!
      IF ( fthr>0 ) THEN
!
!     DETERMINE THE MAXIMUM FAILURE INDEX
!
         fimax = fpmax
         IF ( fbmax>abs(fpmax) ) fimax = fbmax
      ENDIF
!
!     CONTINUE TO OUTPUT THE MAXIMUM FAILURE INDEX TO OES1L/OES1AL
!
!     LAST-1.  MAXIMUM FAILURE INDEX OF LIMIATE, FIMAXR
!      LAST.   IFLAG3 (=1 IF FIMAXR.GE.0.999, DEFAULT=0)
!
      fimaxr = fimax
      iflag3 = 0
      IF ( abs(fimax)>=0.999 ) iflag3 = 1
!
      CALL write(Oes1l,fimaxr,1,0)
      CALL write(Oes1l,iflag3,1,0)
   ENDIF
!
99999 RETURN
END SUBROUTINE shlsts