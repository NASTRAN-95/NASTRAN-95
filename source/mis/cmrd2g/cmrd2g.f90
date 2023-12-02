!*==cmrd2g.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2g
   USE c_blank
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: eqst , i , icode , ifile , imsg , index1 , ipid , ips , item , itest , itype , iwds , j , k , kore , lamloc , loc ,   &
            & locbgs , loceqs , locnew , locold , lstsil , mode , moduse , namloc , nbgss , ncsubs , newcs , newpts , ngrp ,        &
            & nofreq , nwds , nwdsd , nwdsrd
   INTEGER , DIMENSION(3) , SAVE :: itmlst
   INTEGER , DIMENSION(2) :: itmnam
   INTEGER , DIMENSION(7) :: itrlr
   INTEGER , SAVE :: loap , lods , nhcstm , nhplts , sofeog
   INTEGER , DIMENSION(32) :: lstbit
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , SAVE :: papp
   REAL , DIMENSION(1) :: rz
   EXTERNAL andf , close , cmiwrt , decode , gopen , mesage , page1 , rdtrl , read , rshift , sfetch , sjump , smsg , sofcls ,      &
          & sort , suread , suwrt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE CREATES THE REDUCED SUBSTRUCTURE NEW TABLE ITEMS
!     FOR THE CMRD2 MODULE.
!
!     INPUT DATA
!     GINO  - EQST  - TEMPORARY SUBSTRUCTURE EQUIVALENCE TABLE FOR
!                     SUBSTRUCTURE BEING REDUCED
!
!     OUTPUT DATA
!     SOF  - EQSS   - SUBSTRUCTURE EQUIVALENCE TABLE FOR REDUCED
!                     SUBSTRUCTURE
!            BGSS   - BASIC GRID POINT DEFINITION TABLE FOR REDUCED
!                     SUBSTRUCTURE
!            LODS   - LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!            LOAP   - APPENDED LOAD SET DATA FOR REDUCED SUBSTRUCTURE
!            PLTS   - PLOT SET DATA FOR REDUCED SUBSTRUCTURE
!            CSTM   - COORDINATE SYSTEM TRANSFORMATION DATA FOR REDUCED
!                     SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT- DRY    - MODULE OPERATION FLAG
!            POPT   - LOADS OPERATION FLAG
!            GBUF1  - GINO BUFFER
!            INFILE - INPUT FILE NUMBERS
!            KORLEN - LENGTH OF OPEN CORE
!            KORBGN - BEGINNING ADDRESS OF OPEN CORE
!            OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!            NEWNAM - NAME OF REDUCED SUBSTRUCTURE
!            FREBDY - FREEBODY OPR
!            FREBDY - FREEBODY OPTIONS FLAG
!            IO     - OUTPUT OPTIONS FLAG
!            MODPTS - NUMBER OF MODAL POINTS
!
   !>>>>EQUIVALENCE (Eqst,Infile(5)) , (Rz(1),Z(1))
   DATA modnam/4HCMRD , 4H2G  /
   DATA papp , lods , loap/4HPAPP , 4HLODS , 4HLOAP/
   DATA itmlst/4HEQSS , 4HBGSS , 4HLAMS/
   DATA sofeog/4H$EOG/ , nhplts , nhcstm/4HPLTS , 4HCSTM/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CHECK FOR LOADS ONLY
!
         IF ( .NOT.(ponly) ) THEN
!
!     PROCESS EQSS, BGSS DATA
!
            IF ( dry==-2 ) RETURN
            itrlr(1) = eqst
            CALL rdtrl(itrlr)
            ifile = eqst
            IF ( itrlr(1)<0 ) THEN
!
!     PROCESS SYSTEM FATAL ERRORS
!
               imsg = -1
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL gopen(eqst,z(gbuf1),0)
               itest = 3
               item = itmlst(1)
               itmnam(1) = newnam(1)
               itmnam(2) = newnam(2)
               CALL sfetch(newnam,item,2,itest)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               newpts = modpts
!
!     PROCESS EQSS GROUP 0 DATA
!
               IF ( korbgn+itrlr(2)+2>=korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*20,*40,eqst,z(korbgn),itrlr(2),1,nwdsrd)
               ncsubs = z(korbgn+2)
               z(korbgn+2) = z(korbgn+2) + 1
               z(korbgn+3) = z(korbgn+3) + newpts
               newcs = itrlr(2)
               z(korbgn+newcs) = newnam(1)
               z(korbgn+newcs+1) = newnam(2)
               newcs = itrlr(2) + 2
               CALL suwrt(z(korbgn),newcs,2)
!
!     PROCESS REMAINING EQSS GROUPS
!
               nwds = korlen - korbgn
               DO i = 1 , ncsubs
                  CALL read(*20,*2,eqst,z(korbgn),nwds,1,nwdsrd)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
 2                IF ( korbgn+1+nwdsrd>=korlen ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL suwrt(z(korbgn),nwdsrd,2)
               ENDDO
!
!     PROCESS MODAL POINTS
!
               IF ( korbgn+3*newpts>=korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , newpts
                  kore = 3*(i-1)
                  z(korbgn+kore) = 100 + i
                  z(korbgn+kore+1) = itrlr(4)/2 + i
                  z(korbgn+kore+2) = 1
               ENDDO
               nwdsrd = 3*newpts
               CALL suwrt(z(korbgn),nwdsrd,2)
!
!     PROCESS EQSS SIL DATA
!
               IF ( korbgn+itrlr(4)+2*newpts>=korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*20,*40,eqst,z(korbgn),itrlr(4),1,nwdsrd)
               nwdsrd = itrlr(4) - 1
               icode = z(korbgn+nwdsrd)
               CALL decode(icode,lstbit,nwdsd)
               lstsil = z(korbgn+nwdsrd-1) + nwdsd - 1
               DO i = 1 , newpts
                  kore = itrlr(4) + 2*(i-1)
                  z(korbgn+kore) = lstsil + i
                  z(korbgn+kore+1) = 1
               ENDDO
               nwdsrd = itrlr(4) + 2*newpts
               CALL suwrt(z(korbgn),nwdsrd,2)
               CALL suwrt(z(korbgn),0,3)
!
!     PROCESS BGSS DATA
!
               IF ( korbgn+itrlr(5)+4*newpts>=korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               item = itmlst(2)
               itest = 3
               CALL sfetch(newnam,item,2,itest)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*20,*40,eqst,z(korbgn),3,1,nwdsrd)
               z(korbgn) = newnam(1)
               z(korbgn+1) = newnam(2)
               z(korbgn+2) = z(korbgn+2) + newpts
               locbgs = korbgn
               CALL suwrt(z(korbgn),3,2)
               CALL read(*20,*40,eqst,z(korbgn),itrlr(5),1,nwdsrd)
               DO i = 1 , newpts
                  kore = itrlr(5) + 4*(i-1)
                  z(korbgn+kore) = -1
                  rz(korbgn+kore+1) = 0.0
                  rz(korbgn+kore+2) = 0.0
                  rz(korbgn+kore+3) = 0.0
               ENDDO
               nwdsrd = itrlr(5) + 4*newpts
               CALL suwrt(z(korbgn),nwdsrd,2)
               CALL suwrt(z(korbgn),0,3)
               korbgn = korbgn + itrlr(5)
            ENDIF
         ENDIF
!
!     PROCESS LODS, LOAP ITEM
!
         item = lods
         IF ( popt==papp ) item = loap
         itest = 3
         CALL sfetch(oldnam,item,1,itest)
         IF ( itest/=3 ) THEN
            CALL suread(z(korbgn),-1,nwdsrd,itest)
            IF ( korbgn+nwdsrd>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            z(korbgn) = newnam(1)
            z(korbgn+1) = newnam(2)
            z(korbgn+3) = z(korbgn+3) + 1
            z(korbgn+nwdsrd) = newnam(1)
            z(korbgn+nwdsrd+1) = newnam(2)
            z(korbgn+nwdsrd+2) = sofeog
            iwds = nwdsrd + 3
            CALL suread(z(korbgn+iwds),-2,nwdsrd,itest)
            IF ( korbgn+iwds+nwdsrd+2>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            z(korbgn+iwds+nwdsrd) = 0
            z(korbgn+iwds+nwdsrd+1) = sofeog
            iwds = iwds + nwdsrd + 2
            itest = 3
            CALL sfetch(newnam,item,2,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suwrt(z(korbgn),iwds,3)
            IF ( ponly ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     PROCESS PLTS ITEM
!
         CALL sfetch(oldnam,nhplts,1,itest)
         IF ( itest/=3 ) THEN
            CALL suread(z(korbgn),-1,nwdsrd,itest)
            z(korbgn) = newnam(1)
            z(korbgn+1) = newnam(2)
            itest = 3
            CALL sfetch(newnam,nhplts,2,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suwrt(z(korbgn),nwdsrd,itest)
         ENDIF
!
!     PROCESS CSTM ITEM
!
         CALL sfetch(oldnam,nhcstm,1,itest)
         IF ( itest/=3 ) THEN
            CALL suread(z(korbgn),-2,nwdsrd,itest)
            IF ( korbgn+2*nwdsrd>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            z(korbgn) = newnam(1)
            z(korbgn+1) = newnam(2)
            kore = nwdsrd - 4
            CALL sort(0,0,14,1,z(korbgn+3),kore)
            kore = kore/14
            IF ( korbgn+2*nwdsrd+kore>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            DO i = 1 , kore
               z(korbgn+nwdsrd+i-1) = 0
            ENDDO
            nbgss = itrlr(5)/4
            DO i = 1 , nbgss
               k = 4*(i-1)
               IF ( z(locbgs+k)>0 ) THEN
                  SPAG_Loop_2_1: DO j = 1 , kore
                     loc = 14*(j-1)
                     IF ( z(korbgn+3+loc)==z(locbgs+k) ) THEN
                        z(korbgn+nwdsrd+j-1) = 1
                        EXIT SPAG_Loop_2_1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ENDDO
            locnew = 0
            DO i = 1 , kore
               IF ( z(korbgn+nwdsrd+i-1)/=0 ) THEN
                  locold = 14*(i-1)
                  DO j = 1 , 14
                     z(korbgn+nwdsrd+kore+locnew+j-1) = z(korbgn+3+locold+j-1)
                  ENDDO
                  locnew = locnew + 14
               ENDIF
            ENDDO
            IF ( locnew/=0 ) THEN
               itest = 3
               CALL sfetch(newnam,nhcstm,2,itest)
               CALL suwrt(newnam,2,2)
               CALL suwrt(z(korbgn+nwdsrd+kore),locnew,2)
               CALL suwrt(z(korbgn),0,3)
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     OUTPUT EQSS ITEM
!
         CALL close(eqst,1)
         IF ( andf(rshift(io,4),1)==1 ) THEN
            CALL sfetch(newnam,itmlst(1),1,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(z(korbgn),4,nwdsrd,itest)
            CALL suread(z(korbgn),-1,nwdsrd,itest)
            loc = korbgn + nwdsrd
            ncsubs = ncsubs + 1
            DO i = 1 , ncsubs
               CALL suread(z(loc),-1,nwdsrd,itest)
               namloc = korbgn + 2*(i-1)
               CALL cmiwrt(1,newnam,z(namloc),loc,nwdsrd,z,z)
            ENDDO
            CALL suread(z(loc),-1,nwdsrd,itest)
            IF ( loc+nwdsrd>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL cmiwrt(8,newnam,0,loc,nwdsrd,z,z)
         ENDIF
!
!     OUTPUT BGSS ITEM
!
         IF ( andf(rshift(io,5),1)==1 ) THEN
            CALL sfetch(newnam,itmlst(2),1,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ngrp = 1
            CALL sjump(ngrp)
            CALL suread(z(korbgn),-1,nwdsrd,itest)
            CALL cmiwrt(2,newnam,newnam,korbgn,nwdsrd,z,z)
         ENDIF
!
!     OUTPUT CSTM ITEM
!
         IF ( andf(rshift(io,6),1)==1 ) THEN
            CALL sfetch(newnam,nhcstm,1,itest)
            IF ( itest/=3 ) THEN
               ngrp = 1
               CALL sjump(ngrp)
               CALL suread(z(korbgn),-1,nwdsrd,itest)
               CALL cmiwrt(3,newnam,newnam,korbgn,nwdsrd,z,z)
            ENDIF
         ENDIF
!
!     OUTPUT PLTS ITEM
!
         IF ( andf(rshift(io,7),1)==1 ) THEN
            CALL sfetch(newnam,nhplts,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(z(korbgn),3,nwdsrd,itest)
               CALL suread(z(korbgn),-1,nwdsrd,itest)
               CALL cmiwrt(4,newnam,newnam,korbgn,nwdsrd,z,z)
            ENDIF
         ENDIF
!
!     OUTPUT LODS ITEM
!
         IF ( andf(rshift(io,8),1)==1 ) THEN
            CALL sfetch(newnam,lods,1,itest)
            IF ( itest/=3 ) THEN
               CALL suread(z(korbgn),4,nwdsrd,itest)
               CALL suread(z(korbgn),-1,nwdsrd,itest)
               loc = korbgn + nwdsrd
               itype = 5
               IF ( item==loap ) itype = 7
               DO i = 1 , ncsubs
                  namloc = korbgn + 2*(i-1)
                  CALL suread(z(loc),-1,nwdsrd,itest)
                  CALL cmiwrt(itype,newnam,z(namloc),loc,nwdsrd,z,z)
                  itype = 6
               ENDDO
            ENDIF
         ENDIF
!
!     OUTPUT MODAL DOF SUMMARY
!
         IF ( andf(rshift(io,9),1)==1 ) THEN
            item = itmlst(3)
            itmnam(1) = oldnam(1)
            itmnam(2) = oldnam(2)
            CALL sfetch(oldnam,item,1,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(z(korbgn),-1,nwdsrd,itest)
            CALL page1
            WRITE (iprntr,99002) newnam
            line = line + 10
            nofreq = z(korbgn+3)
            lamloc = korbgn
            moduse = lamloc + 7*nofreq + 1
            CALL suread(z(korbgn),-2,nwdsrd,itest)
            IF ( korbgn+nwdsrd>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            item = itmlst(1)
            itmnam(1) = newnam(1)
            itmnam(2) = newnam(2)
            CALL sfetch(newnam,item,1,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            korbgn = korbgn + moduse + nofreq
            IF ( korbgn>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(z(korbgn),-1,nwdsrd,itest)
            DO i = 1 , ncsubs
               CALL suread(z(korbgn),-1,nwdsrd,itest)
               IF ( korbgn+nwdsrd>=korlen ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            loceqs = korbgn
            ipid = 2*z(korbgn+1)
            korbgn = korbgn + nwdsrd
            IF ( korbgn+ipid>=korlen ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL suread(z(korbgn),ipid,nwdsrd,itest)
            ips = z(korbgn+ipid-2)
            index1 = -3
            DO i = 1 , nofreq
               IF ( line>nlpp ) THEN
                  CALL page1
                  WRITE (iprntr,99002) newnam
                  line = line + 10
               ENDIF
               IF ( z(moduse+i-1)>1 ) THEN
                  mode = 7*(i-1)
                  WRITE (iprntr,99003) z(lamloc+mode) , rz(lamloc+mode+4) , z(moduse+i-1)
               ELSE
                  index1 = index1 + 3
                  mode = 7*(i-1)
                  WRITE (iprntr,99003) z(lamloc+mode) , rz(lamloc+mode+4) , z(moduse+i-1) , z(loceqs+index1) , ips
                  ips = ips + 1
               ENDIF
               line = line + 1
            ENDDO
         ENDIF
         RETURN
 20      imsg = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -3
         spag_nextblock_1 = 4
      CASE (3)
         imsg = -8
         ifile = 0
         spag_nextblock_1 = 4
      CASE (4)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
      CASE (5)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==4 ) THEN
!
            imsg = -2
         ELSEIF ( itest==5 .OR. itest==6 ) THEN
            imsg = -3
         ELSE
            WRITE (iprntr,99001) ufm , modnam , item , itmnam
!
99001       FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
            dry = -2
            RETURN
         ENDIF
         CALL smsg(imsg,item,itmnam)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (//36X,43HMODAL DOF SUMMARY FOR REDUCES SUBSTRUCTURE ,2A4,//30X,41HUSAGE CODES ARE 1 - INCLUDED IN MODAL SET,/46X,     &
             &50H2 - EXCLUDED FROM MODAL SET BECAUSE OF NON-PARTICI,6HPATION,/46X,41H3 - EXCLUDED FROM MODAL SET BECAUSE OF RA,     &
             &11HNGE OR NMAX,//40X,4HMODE,22X,15HUSAGE      GRID,/39X,6HNUMBER,8X,6HCYCLES,8X,26HCODE    POINT ID       SIL,/)
99003 FORMAT (39X,I5,5X,1P,E13.6,6X,I1,6X,I8,4X,I6)
!
END SUBROUTINE cmrd2g
