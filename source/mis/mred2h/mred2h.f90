!*==mred2h.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2h
!
!     THIS SUBROUTINE CREATES THE REDUCED SUBSTRUCTURE NEW TABLE ITEMS
!     FOR THE MRED2 MODULE.
!
!     INPUT DATA
!     GINO  - EQST   - TEMPORARY SUBSTRUCTURE EQUIVALENCE TABLE FOR
!                      SUBSTRUCTURE BEING REDUCED
!
!     OUTPUT DATA
!     SOF   - EQSS   - SUBSTRUCTURE EQUIVALENCE TABLE FOR REDUCED
!                      SUBSTRUCTURE
!             BGSS   - BASIC GRID POINT DEFINITION TABLE FOR REDUCED
!                      SUBSTRUCTURE
!             LODS   - LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!             LOAP   - APPENDED LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!             PLTS   - PLOT SET DATA FOR REDUCED SUBSTRUCTURE
!             CSTM   - COORDINATE SYSTEM TRANSFORMATION DATA FOR REDUCED
!                      SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT - DRY    - MODULE OPERATION FLAG
!             POPT   - LOAD OPTION FLAG
!             GBUF1  - GINO BUFFER
!             INFILE - INPUT FILE NUMBERS
!             KORLEN - LENGTH OF OPEN CORE
!             KORBGN - BEGINNING ADDRESS OF OPEN CORE
!             OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!             NEWNAM - NAME OF REDUCED SUBSTRUCTURE
!             FREBDY - FREEBODY OPR
!             FREBDY - FREEBODY OPTIONS FLAG
!             IO     - OUTPUT OPTIONS FLAG
!             MODPTS - NUMBER OF MODAL POINTS
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: eqst , i , icode , ifile , imsg , index1 , ipid , iprb , ips , item , itest , itype , iwds , j , k , kore , lamloc ,  &
            & loc , locbgs , loceqs , locnew , locold , lstsil , mode , moduse , namloc , nbgss , ncsubs , newcs , newpts , ngrp ,  &
            & nofreq , nwds , nwdsd , nwdsrd
   INTEGER , SAVE :: farind , izero , loap , lods , papp , sofeog
   INTEGER , DIMENSION(3) , SAVE :: itmlst
   INTEGER , DIMENSION(2) :: itmnam
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , DIMENSION(32) :: lstbit
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   REAL , SAVE :: zero
   EXTERNAL andf , close , cmiwrt , decode , gopen , mesage , page1 , rdtrl , read , rshift , sfetch , sjump , smsg , smsg1 ,       &
          & sofcls , sort , suread , suwrt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Eqst,Infile(4)) , (Rz(1),Z(1))
   DATA modnam/4HMRED , 4H2H  /
   DATA papp , lods , loap/4HPAPP , 4HLODS , 4HLOAP/
   DATA farind , izero , zero/6 , 0 , 0.0/
   DATA itmlst/4HEQSS , 4HBGSS , 4HLAMS/
   DATA sofeog/4H$EOG/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK FOR LOADS PROCESSING ONLY
!
         IF ( .NOT.(Ponly) ) THEN
!
!     PROCESS EQSS, BGSS DATA
!
            IF ( Dry==-2 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            itrlr(1) = eqst
            CALL rdtrl(itrlr)
            itmnam(1) = Newnam(1)
            itmnam(2) = Newnam(2)
            ifile = eqst
            IF ( itrlr(1)<0 ) THEN
!
!     PROCESS SYSTEM FATAL ERRORS
!
               imsg = -1
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL gopen(eqst,Z(Gbuf1),0)
               itest = 3
               item = itmlst(1)
               CALL sfetch(Newnam,4HEQSS,2,itest)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               newpts = Modpts
               IF ( Frebdy ) newpts = newpts + farind
!
!     PROCESS EQSS GROUP 0 DATA
!
               IF ( Korbgn+itrlr(2)+2>=Korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*20,*40,eqst,Z(Korbgn),itrlr(2),1,nwdsrd)
               ncsubs = Z(Korbgn+2)
               Z(Korbgn+2) = Z(Korbgn+2) + 1
               Z(Korbgn+3) = Z(Korbgn+3) + newpts
               newcs = itrlr(2)
               Z(Korbgn+newcs) = Newnam(1)
               Z(Korbgn+newcs+1) = Newnam(2)
               newcs = itrlr(2) + 2
               CALL suwrt(Z(Korbgn),newcs,2)
!
!     PROCESS REMAINING EQSS GROUPS
!
               nwds = Korlen - Korbgn
               DO i = 1 , ncsubs
                  CALL read(*20,*2,eqst,Z(Korbgn),nwds,1,nwdsrd)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
 2                IF ( Korbgn+1+nwdsrd>=Korlen ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL suwrt(Z(Korbgn),nwdsrd,2)
               ENDDO
!
!     PROCESS MODAL AND FREE-BODY POINTS
!
               IF ( Korbgn+3*newpts>=Korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , newpts
                  kore = 3*(i-1)
                  IF ( .NOT.Frebdy ) THEN
                     Z(Korbgn+kore) = 100 + i
                  ELSEIF ( i>farind ) THEN
                     Z(Korbgn+kore) = 100 + i - farind
                  ELSE
                     Z(Korbgn+kore) = i
                  ENDIF
                  Z(Korbgn+kore+1) = itrlr(4)/2 + i
                  Z(Korbgn+kore+2) = 1
               ENDDO
               nwdsrd = 3*newpts
               CALL suwrt(Z(Korbgn),nwdsrd,2)
!
!     PROCESS EQSS SIL DATA
!
               IF ( Korbgn+itrlr(4)+2*newpts>=Korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*20,*40,eqst,Z(Korbgn),itrlr(4),1,nwdsrd)
               nwdsrd = itrlr(4) - 1
               icode = Z(Korbgn+nwdsrd)
               CALL decode(icode,lstbit,nwdsd)
               lstsil = Z(Korbgn+nwdsrd-1) + nwdsd - 1
               DO i = 1 , newpts
                  kore = itrlr(4) + 2*(i-1)
                  Z(Korbgn+kore) = lstsil + i
                  Z(Korbgn+kore+1) = 1
               ENDDO
               nwdsrd = itrlr(4) + 2*newpts
               CALL suwrt(Z(Korbgn),nwdsrd,2)
               CALL suwrt(Z(Korbgn),0,3)
!
!     PROCESS BGSS DATA
!
               IF ( Korbgn+itrlr(5)+4*newpts>=Korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               item = itmlst(2)
               itest = 3
               CALL sfetch(Newnam,4HBGSS,2,itest)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*20,*40,eqst,Z(Korbgn),3,1,nwdsrd)
               Z(Korbgn) = Newnam(1)
               Z(Korbgn+1) = Newnam(2)
               Z(Korbgn+2) = Z(Korbgn+2) + newpts
               locbgs = Korbgn
               CALL suwrt(Z(Korbgn),3,2)
               CALL read(*20,*40,eqst,Z(Korbgn),itrlr(5),1,nwdsrd)
               DO i = 1 , newpts
                  kore = itrlr(5) + 4*(i-1)
                  Z(Korbgn+kore) = -1
                  rz(Korbgn+kore+1) = 0.0
                  rz(Korbgn+kore+2) = 0.0
                  rz(Korbgn+kore+3) = 0.0
               ENDDO
               nwdsrd = itrlr(5) + 4*newpts
               CALL suwrt(Z(Korbgn),nwdsrd,2)
               CALL suwrt(Z(Korbgn),0,3)
               Korbgn = Korbgn + itrlr(5)
            ENDIF
         ENDIF
!
!     PROCESS LODS, LOAP ITEM
!
         item = lods
         IF ( Popt==papp ) item = loap
         itest = 3
         CALL sfetch(Oldnam,item,1,itest)
         IF ( itest/=3 ) THEN
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            IF ( (Korbgn+nwdsrd)>=Korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Z(Korbgn) = Newnam(1)
            Z(Korbgn+1) = Newnam(2)
            Z(Korbgn+3) = Z(Korbgn+3) + 1
            Z(Korbgn+nwdsrd) = Newnam(1)
            Z(Korbgn+nwdsrd+1) = Newnam(2)
            Z(Korbgn+nwdsrd+2) = sofeog
            iwds = nwdsrd + 3
            CALL suread(Z(Korbgn+iwds),-2,nwdsrd,itest)
            IF ( Korbgn+iwds+nwdsrd+2>=Korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Z(Korbgn+iwds+nwdsrd) = 0
            Z(Korbgn+iwds+nwdsrd+1) = sofeog
            iwds = iwds + nwdsrd + 2
            itest = 3
            CALL sfetch(Newnam,item,2,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suwrt(Z(Korbgn),iwds,3)
            IF ( Ponly ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     PROCESS PLTS ITEM
!
         CALL sfetch(Oldnam,4HPLTS,1,itest)
         IF ( itest/=3 ) THEN
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            Z(Korbgn) = Newnam(1)
            Z(Korbgn+1) = Newnam(2)
            itest = 3
            CALL sfetch(Newnam,4HPLTS,2,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            itest = 2
            CALL suwrt(Z(Korbgn),nwdsrd,itest)
            itest = 3
            CALL suwrt(Z(Korbgn),0,itest)
         ENDIF
!
!     PROCESS CSTM ITEM
!
         CALL sfetch(Oldnam,4HCSTM,1,itest)
         IF ( itest/=3 ) THEN
            CALL suread(Z(Korbgn),-2,nwdsrd,itest)
            IF ( Korbgn+2*nwdsrd>=Korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Z(Korbgn) = Newnam(1)
            Z(Korbgn+1) = Newnam(2)
            kore = nwdsrd - 4
            CALL sort(0,0,14,1,Z(Korbgn+3),kore)
            kore = kore/14
            IF ( Korbgn+2*nwdsrd+kore>=Korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = 1 , kore
               Z(Korbgn+nwdsrd+i-1) = 0
            ENDDO
            nbgss = itrlr(5)/4
            DO i = 1 , nbgss
               k = 4*(i-1)
               IF ( Z(locbgs+k)>0 ) THEN
                  SPAG_Loop_2_1: DO j = 1 , kore
                     loc = 14*(j-1)
                     IF ( Z(Korbgn+3+loc)==Z(locbgs+k) ) THEN
                        Z(Korbgn+nwdsrd+j-1) = 1
                        EXIT SPAG_Loop_2_1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ENDDO
            locnew = 0
            DO i = 1 , kore
               IF ( Z(Korbgn+nwdsrd+i-1)/=0 ) THEN
                  locold = 14*(i-1)
                  DO j = 1 , 14
                     Z(Korbgn+nwdsrd+kore+locnew+j-1) = Z(Korbgn+3+locold+j-1)
                  ENDDO
                  locnew = locnew + 14
               ENDIF
            ENDDO
            IF ( locnew/=0 ) THEN
               itest = 3
               CALL sfetch(Newnam,4HCSTM,2,itest)
               CALL suwrt(Newnam,2,2)
               CALL suwrt(Z(Korbgn+nwdsrd+kore),locnew,2)
               CALL suwrt(Z(Korbgn),0,3)
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     OUTPUT EQSS ITEM
!
         CALL close(eqst,1)
         IF ( andf(rshift(Io,4),1)==1 ) THEN
            CALL sfetch(Newnam,4HEQSS,1,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(Z(Korbgn),4,nwdsrd,itest)
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            loc = Korbgn + nwdsrd
            ncsubs = ncsubs + 1
            DO i = 1 , ncsubs
               CALL suread(Z(loc),-1,nwdsrd,itest)
               namloc = Korbgn + 2*(i-1)
               CALL cmiwrt(1,Newnam,Z(namloc),loc,nwdsrd,Z,Z)
            ENDDO
            CALL suread(Z(loc),-1,nwdsrd,itest)
            IF ( (loc+nwdsrd)>=Korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL cmiwrt(8,Newnam,0,loc,nwdsrd,Z,Z)
         ENDIF
!
!     OUTPUT BGSS ITEM
!
         IF ( andf(rshift(Io,5),1)==1 ) THEN
            CALL sfetch(Newnam,4HBGSS,1,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ngrp = 1
            CALL sjump(ngrp)
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            CALL cmiwrt(2,Newnam,Newnam,Korbgn,nwdsrd,Z,Z)
         ENDIF
!
!     OUTPUT CSTM ITEM
!
         IF ( andf(rshift(Io,6),1)==1 ) THEN
            CALL sfetch(Newnam,4HCSTM,1,itest)
            IF ( itest/=3 ) THEN
               ngrp = 1
               CALL sjump(ngrp)
               CALL suread(Z(Korbgn),-1,nwdsrd,itest)
               CALL cmiwrt(3,Newnam,Newnam,Korbgn,nwdsrd,Z,Z)
            ENDIF
         ENDIF
!
!     OUTPUT PLTS ITEM
!
         IF ( andf(rshift(Io,7),1)==1 ) THEN
            CALL sfetch(Newnam,4HPLTS,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(Z(Korbgn),3,nwdsrd,itest)
               CALL suread(Z(Korbgn),-1,nwdsrd,itest)
               CALL cmiwrt(4,Newnam,Newnam,Korbgn,nwdsrd,Z,Z)
            ENDIF
         ENDIF
!
!     OUTPUT LODS ITEM
!
         IF ( andf(rshift(Io,8),1)==1 ) THEN
            CALL sfetch(Newnam,item,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(Z(Korbgn),4,nwdsrd,itest)
               CALL suread(Z(Korbgn),-1,nwdsrd,itest)
               loc = Korbgn + nwdsrd
               itype = 5
               IF ( item==loap ) itype = 7
               DO i = 1 , ncsubs
                  namloc = Korbgn + 2*(i-1)
                  CALL suread(Z(loc),-1,nwdsrd,itest)
                  CALL cmiwrt(itype,Newnam,Z(namloc),loc,nwdsrd,Z,Z)
                  itype = 6
               ENDDO
            ENDIF
         ENDIF
!
!     OUTPUT MODAL DOF SUMMARY
!
         IF ( andf(rshift(Io,9),1)/=1 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         item = itmlst(3)
         itmnam(1) = Oldnam(1)
         itmnam(2) = Oldnam(2)
         CALL sfetch(Oldnam,item,1,itest)
         IF ( itest/=1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL suread(Z(Korbgn),-1,nwdsrd,itest)
         CALL page1
         WRITE (Iprntr,99001) Newnam
         Line = Line + 11
         nofreq = Z(Korbgn+3)
         lamloc = Korbgn
         moduse = lamloc + 7*nofreq + 1
         CALL suread(Z(Korbgn),-2,nwdsrd,itest)
         IF ( (Korbgn+nwdsrd)>=Korlen ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Usrmod<=1 ) THEN
            item = itmlst(1)
            itmnam(1) = Newnam(1)
            itmnam(2) = Newnam(2)
            CALL sfetch(Newnam,item,1,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Korbgn = Korbgn + moduse + nofreq
            IF ( Korbgn>=Korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            DO i = 1 , ncsubs
               CALL suread(Z(Korbgn),-1,nwdsrd,itest)
               IF ( Korbgn+nwdsrd>=Korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            iprb = 0
            IF ( Frebdy ) iprb = Z(Korbgn+1) - 1
            nwdsrd = nwdsrd/3
            loceqs = Korbgn
            SPAG_Loop_1_2: DO i = 1 , nwdsrd
               j = 1 + 3*(i-1)
               ipid = Z(loceqs+j)
               IF ( Z(loceqs+j-1)>100 ) EXIT SPAG_Loop_1_2
            ENDDO SPAG_Loop_1_2
            Korbgn = Korbgn + 3*nwdsrd
            ipid = 2*ipid
            IF ( Korbgn+ipid>=Korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(Z(Korbgn),ipid,nwdsrd,itest)
            ips = Z(Korbgn+ipid-2)
            IF ( Frebdy ) THEN
               DO i = 1 , farind
                  j = 3*(i-1)
                  k = 2*((i-1)+iprb)
                  WRITE (Iprntr,99002) izero , zero , izero , Z(loceqs+j) , Z(Korbgn+k)
               ENDDO
            ENDIF
         ENDIF
         index1 = -3
         IF ( Frebdy ) index1 = 3*farind - 3
         DO i = 1 , nofreq
            IF ( Line>Nlpp ) THEN
               CALL page1
               WRITE (Iprntr,99001) Newnam
               Line = Line + 11
            ENDIF
            IF ( (Z(moduse+i-1)>1) .OR. (Usrmod>1) ) THEN
               mode = 7*(i-1)
               WRITE (Iprntr,99002) Z(lamloc+mode) , rz(lamloc+mode+4) , Z(moduse+i-1)
            ELSE
               index1 = index1 + 3
               mode = 7*(i-1)
               WRITE (Iprntr,99002) Z(lamloc+mode) , rz(lamloc+mode+4) , Z(moduse+i-1) , Z(loceqs+index1) , ips
               ips = ips + 1
            ENDIF
            Line = Line + 1
         ENDDO
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 20      imsg = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -3
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         imsg = -8
         ifile = 0
         spag_nextblock_1 = 4
      CASE (4)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==4 ) THEN
            imsg = -2
         ELSEIF ( itest==5 .OR. itest==6 ) THEN
            imsg = -3
         ELSE
            CALL smsg1(-9,item,itmnam,modnam)
            Dry = -2
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL smsg(imsg,item,itmnam)
         spag_nextblock_1 = 6
      CASE (6)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99001 FORMAT (1H0,36X,43HMODAL DOF SUMMARY FOR REDUCED SUBSTRUCTURE ,2A4,//30X,36HUSAGE CODES ARE 0 - RIGID BODY POINT,/46X,        &
             &25H1 - INCLUDED IN MODAL SET,/46X,20H2 - EXCLUDED FROM MO,36HDAL SET BECAUSE OF NON-PARTICIPATION,/46X,10H3 - EXCLUD, &
             &42HED FROM MODAL SET BECAUSE OF RANGE OR NMAX,//40X,4HMODE,22X,15HUSAGE      GRID,/39X,6HNUMBER,8X,6HCYCLES,8X,       &
             &26HCODE    POINT ID       SIL,/)
99002 FORMAT (39X,I5,5X,1P,E13.6,6X,I1,6X,I8,4X,I6)
!
END SUBROUTINE mred2h
