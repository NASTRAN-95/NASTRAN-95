!*==mred2c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2c(Kode)
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kode
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifile , imsg , ismg , item , itest , lamamr , lams , lamwds , nwds , nwdsrd , phis , phiss
   INTEGER , DIMENSION(2) , SAVE :: itmlst , modnam
   INTEGER , SAVE :: rgdfmt
   EXTERNAL close , delete , fwdrec , gopen , mesage , mtrxi , mtrxo , read , sfetch , smsg , smsg1 , sofcls , suread , suwrt ,     &
          & write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES THE OLDMODES OPTION FLAG FOR THE MRED2
!     MODULE.
!
!     INPUT DATA
!     GINO - LAMAMR   - EIGENVALUE TABLE  FOR SUBSTRUCTURE BEING REDUCED
!            PHISS    - EIGENVCTOR MATRIX FOR SUBSTRUCTURE BEING REDUCED
!     SOF  - LAMS     - EIGENVALUE  TABLE FOR ORIGINAL SUBSTRUCTURE
!            PHIS     - EIGENVCTOR  TABLE FOR ORIGINAL SUBSTRUCTURE
!
!     OUTPUT DATA
!     GINO - LAMAMR   - EIGENVALUE TABLE  FOR SUBSTRUCTURE BEING REDUCED
!            PHISS    - EIGENVCTOR MATRIX FOR SUBSTRUCTURE BEING REDUCED
!     SOF  - LAMS     - EIGENVALUE TABLE  FOR ORIGINAL SUBSTRUCTURE
!            PHIS     - EIGENVCTOR MATRIX FOR ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT  - GBUF   - GINO BUFFER
!              INFILE - INPUT FILE NUMBERS
!              ISCR   - SCRATCH FILE NUMBERS
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
!              MODES  - OLDMODES OPTION FLAG
!              LAMAAP - BEGINNING ADDRESS OF LAMS RECORD TO BE APPENDED
!              NFOUND - NUMBER OF MODAL POINTS USED
!              MODLEN - LENGTH OF MODE USE ARRAY
!     OTHERS - LAMAMR - LAMAMR INPUT FILE NUMBER
!              PHIS   - PHIS INPUT FILE NUMBER
!              LAMS   - LAMS INPUT FILE NUMBER
!              PHISS  - PHISS INPUT FILE NUMBER
!
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Phis,Infile(3)) , (Lams,Iscr(5)) , (Phiss,Iscr(6))
   DATA modnam/4HMRED , 4H2C  /
   DATA itmlst/4HPHIS , 4HLAMS/
   DATA rgdfmt/3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST OPERATION FLAG
!
         IF ( Dry/=-2 ) THEN
            IF ( Kode>1 ) THEN
!
!     STORE LAMAMR (TABLE) AS LAMS ON SOF
!
               IF ( .NOT.(Modes) ) THEN
                  item = itmlst(2)
                  CALL delete(Oldnam,item,itest)
                  IF ( itest==2 .OR. itest>3 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ifile = lamamr
                  CALL gopen(lamamr,Z(Gbuf1),0)
                  CALL fwdrec(*40,lamamr)
                  itest = 3
                  CALL sfetch(Oldnam,itmlst(2),2,itest)
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
                  IF ( lamwds>=1 ) THEN
                     DO i = 1 , lamwds
                        CALL read(*20,*40,lamamr,Z(Korbgn),7,0,nwds)
                        CALL suwrt(Z(Korbgn),7,1)
                     ENDDO
                  ENDIF
                  CALL read(*20,*40,lamamr,Z(Korbgn),7,0,nwds)
                  CALL close(lamamr,1)
                  CALL suwrt(Z(Korbgn),7,2)
                  IF ( Kode==3 ) THEN
                     DO i = 1 , Modlen
                        Z(Korbgn+i-1) = 1
                     ENDDO
                     CALL suwrt(Z(Korbgn),Modlen,2)
                     CALL suwrt(Z(Korbgn),0,3)
                  ELSE
                     CALL suwrt(Z(Lamaap),Modlen,2)
                     CALL suwrt(Z(Lamaap),0,3)
                  ENDIF
               ENDIF
!
!     TEST OLDMODES OPTION FLAG
!
            ELSEIF ( Modes ) THEN
!
!     READ SOF PHIS ONTO GINO PHIS SCRATCH FILE
!
               CALL mtrxi(phiss,Oldnam,itmlst(1),0,itest)
               item = itmlst(1)
               IF ( itest/=1 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     READ SOF LAMS ONTO GINO LAMAMR SCRATCH FILE
!
               CALL sfetch(Oldnam,itmlst(2),1,itest)
               item = itmlst(2)
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
               phis = phiss
               lamamr = lams
            ELSE
!
!     STORE GINO PHIS AS PHIS ON SOF
!
               ifile = phis
               CALL mtrxo(phis,Oldnam,itmlst(1),0,itest)
               item = itmlst(1)
               IF ( itest/=3 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         RETURN
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
            ismg = -11
         ELSEIF ( itest==3 ) THEN
            imsg = -1
            CALL smsg(imsg,item,Oldnam)
            RETURN
         ELSEIF ( itest==4 ) THEN
            imsg = -2
            CALL smsg(imsg,item,Oldnam)
            RETURN
         ELSEIF ( itest==5 ) THEN
            imsg = -3
            CALL smsg(imsg,item,Oldnam)
            RETURN
         ELSEIF ( itest==6 ) THEN
            imsg = -10
         ELSE
            imsg = -9
         ENDIF
         Dry = -2
         CALL smsg1(imsg,item,Oldnam,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mred2c
