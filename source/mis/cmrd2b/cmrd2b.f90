!*==cmrd2b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmrd2b(Kode)
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kode
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifile , imsg , item , itest , lamamr , lams , lamwds , nwds , nwdsrd , phisl , phissl , phissr
   INTEGER , DIMENSION(3) , SAVE :: itmlst
   INTEGER , DIMENSION(2) , SAVE :: modnam
   INTEGER , SAVE :: rgdfmt
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , delete , fwdrec , gopen , mesage , mtrxi , mtrxo , read , sfetch , smsg , sofcls , suread , suwrt , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES THE OLDMODES OPTION FLAG FOR THE CMRED2
!     MODULE.
!
!     INPUT  DATA
!     GINO - LAMAMR - EIGENVALUE TABLE FOR SUBSTRUCTURE BEING REDUCED
!            PHISSR - RIGHT HAND EIGENVECTOR MATRIX FOR SUBSTRUCTURE
!                     BEING REDUCED
!            PHISSL - LEFT HAND EIGENVECTOR MATRIX FOR SUBSTRUCTURE
!                     BEING REDUCED
!     SOF  - LAMS   - EIGENVALUE TABLE FOR ORIGINAL SUBSTRUCTURE
!            PHIS   - RIGHT HAND EIGENVECTOR TABLE FOR ORIGINAL
!                     SUBSTRUCTURE
!            PHIL   - LEFT HAND EIGENVECTOR TABLE FOR ORIGINAL
!                     SUBSTRUCTURE
!
!     OUTPUT DATA
!     GINO - LAMAMR - EIGENVALUE TABLE FOR SUBSTRUCTURE BEING REDUCED
!            PHISS  - EIGENVECTOR MATRIX FOR SUBSTRUCTURE BEING REDUCED
!     SOF  - LAMS   - EIGENVALUE TABLE FOR ORIGINAL SUBSTRUCTURE
!            PHIS   - EIGENVECTOR MATRIX FOR ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT- GBUF   - GINO BUFFER
!            INFILE - INPUT FILE NUMBERS
!            ISCR   - SCRATCH FILE NUMBERS
!            KORBGN - BEGINNING ADDRESS OF OPEN CORE
!            OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!            MODES  - OLDMODES OPTION FLAG
!            NFOUND - NUMBER OF MODAL POINTS USED
!            LAMAAP - BEGINNING ADDRESS OF LAMS RECORD TO BE APPENDED
!            MODLEN - LENGTH OF MODE USE ARRAY
!     OTHERS-LAMAMR - LAMAMR INPUT FILE NUMBER
!            PHIS   - PHIS INPUT FILE NUMBER
!            LAMS   - LAMS INPUT FILE NUMBER
!            PHISS  - PHISS INPUT FILE NUMBER
!
   !>>>>EQUIVALENCE (Rz(1),Z(1)) , (Lamamr,Infile(2)) , (Phissr,Infile(3)) , (Phissl,Infile(4)) , (Lams,Iscr(5)) , (Phisl,Iscr(6))
   DATA modnam/4HCMRD , 4H2B  /
   DATA itmlst/4HPHIS , 4HPHIL , 4HLAMS/
   DATA rgdfmt/3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST OPERATION FLAG
!
         IF ( Dry==-2 ) RETURN
         IF ( Kode==3 ) THEN
!
!     STORE LAMAMR (TABLE) AS LAMS ON SOF
!
            IF ( .NOT.(Modes) ) THEN
               item = itmlst(3)
               CALL delete(Oldnam,item,itest)
               IF ( itest==2 .OR. itest>3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ifile = lamamr
               CALL gopen(lamamr,Z(Gbuf1),0)
               CALL fwdrec(*40,lamamr)
               itest = 3
               CALL sfetch(Oldnam,itmlst(3),2,itest)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO i = 1 , 2
                  Z(Korbgn+i-1) = Oldnam(i)
               ENDDO
               Z(Korbgn+2) = rgdfmt
               Z(Korbgn+3) = Modlen
               CALL suwrt(Z(Korbgn),4,2)
               lamwds = Modlen - 1
               rz(Korbgn+6) = 0.0
               DO i = 1 , lamwds
                  CALL read(*20,*40,lamamr,Z(Korbgn),6,0,nwds)
                  CALL suwrt(Z(Korbgn),7,1)
               ENDDO
               CALL read(*20,*40,lamamr,Z(Korbgn),6,0,nwds)
               CALL close(lamamr,1)
               CALL suwrt(Z(Korbgn),7,2)
               CALL suwrt(Z(Lamaap),Modlen,2)
               CALL suwrt(Z(Lamaap),0,3)
            ENDIF
            RETURN
!
!     TEST OLDMODES OPTION FLAG
!
         ELSEIF ( Modes ) THEN
!
!     READ SOF PHI(S,L) ONTO GINO PHI(S,L) SCRATCH FILES
!
            item = itmlst(Kode)
            CALL mtrxi(phisl,Oldnam,item,0,itest)
            IF ( itest/=1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     READ SOF LAMS ONTO GINO LAMS SCRATCH FILE
!
            CALL sfetch(Oldnam,itmlst(3),1,itest)
            item = itmlst(3)
            IF ( itest>1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL gopen(lams,Z(Gbuf1),1)
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            CALL write(lams,Z(Korbgn),nwdsrd,1)
            CALL suread(Z(Korbgn),-1,nwdsrd,itest)
            CALL write(lams,Z(Korbgn),nwdsrd,1)
            CALL close(lams,1)
!
!     SWITCH FILE NUMBERS
!
            IF ( Kode==1 ) phissr = phisl
            IF ( Kode==2 ) phissl = phisl
            lamamr = lams
            RETURN
         ELSE
!
!     STORE GINO PHISS(R,L) AS PHI(S,L) ON SOF
!
            ifile = phissr
            IF ( Kode==2 ) ifile = phissl
            item = itmlst(Kode)
            CALL mtrxo(ifile,Oldnam,item,0,itest)
            IF ( itest/=3 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            RETURN
         ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 20      imsg = -2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -3
         spag_nextblock_1 = 2
      CASE (2)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
      CASE (3)
!
!     PROCESS MODULE FATAL ERRORS
!
         IF ( itest==2 ) THEN
!
            WRITE (Iprntr,99001) Ufm , modnam , item , Oldnam
99001       FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
            Dry = -2
            RETURN
         ELSEIF ( itest==3 ) THEN
            imsg = -1
         ELSEIF ( itest==4 ) THEN
            imsg = -2
         ELSEIF ( itest==5 ) THEN
            imsg = -3
         ELSEIF ( itest==6 ) THEN
!
            WRITE (Iprntr,99002) Ufm , modnam , item , Oldnam
99002       FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,             &
                   &', IS PURBED.')
            Dry = -2
            RETURN
         ELSE
            WRITE (Iprntr,99003) Ufm , modnam , item , Oldnam
!
99003       FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
            Dry = -2
            RETURN
         ENDIF
         CALL smsg(imsg,item,Oldnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE cmrd2b
