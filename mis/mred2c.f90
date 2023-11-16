
SUBROUTINE mred2c(Kode)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dry , Gbuf1 , Idum1 , Idum2(5) , Idum3(6) , Idum4 , Idum5(9) , Idum6 , Idum7 , Infile(12) , Iprntr , Iscr(10) , Korbgn , &
         & Korlen , Lamaap , Lamamr , Lams , Modlen , Nfound , Oldnam(2) , Phis , Phiss , Z(1)
   LOGICAL Modes
   COMMON /blank / Idum1 , Dry , Idum7 , Gbuf1 , Idum2 , Infile , Idum3 , Iscr , Korlen , Korbgn , Oldnam , Idum5 , Modes , Idum6 , &
                 & Lamaap , Nfound , Modlen
   COMMON /system/ Idum4 , Iprntr
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Kode
!
! Local variable declarations
!
   INTEGER i , ifile , imsg , ismg , item , itest , itmlst(2) , lamwds , modnam(2) , nwds , nwdsrd , rgdfmt
!
! End of declarations
!
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
   EQUIVALENCE (Lamamr,Infile(2)) , (Phis,Infile(3)) , (Lams,Iscr(5)) , (Phiss,Iscr(6))
   DATA modnam/4HMRED , 4H2C  /
   DATA itmlst/4HPHIS , 4HLAMS/
   DATA rgdfmt/3/
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
            IF ( itest==2 .OR. itest>3 ) GOTO 400
            ifile = Lamamr
            CALL gopen(Lamamr,Z(Gbuf1),0)
            CALL fwdrec(*200,Lamamr)
            itest = 3
            CALL sfetch(Oldnam,itmlst(2),2,itest)
            IF ( itest/=3 ) GOTO 400
            DO i = 1 , 2
               Z(Korbgn+i-1) = Oldnam(i)
            ENDDO
            Z(Korbgn+2) = rgdfmt
            Z(Korbgn+3) = Modlen
            CALL suwrt(Z(Korbgn),4,2)
            lamwds = Modlen - 1
            IF ( lamwds>=1 ) THEN
               DO i = 1 , lamwds
                  CALL read(*100,*200,Lamamr,Z(Korbgn),7,0,nwds)
                  CALL suwrt(Z(Korbgn),7,1)
               ENDDO
            ENDIF
            CALL read(*100,*200,Lamamr,Z(Korbgn),7,0,nwds)
            CALL close(Lamamr,1)
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
         CALL mtrxi(Phiss,Oldnam,itmlst(1),0,itest)
         item = itmlst(1)
         IF ( itest/=1 ) GOTO 400
!
!     READ SOF LAMS ONTO GINO LAMAMR SCRATCH FILE
!
         CALL sfetch(Oldnam,itmlst(2),1,itest)
         item = itmlst(2)
         IF ( itest>1 ) GOTO 400
         CALL gopen(Lams,Z(Gbuf1),1)
         CALL suread(Z(Korbgn),-1,nwdsrd,itest)
         CALL write(Lams,Z(Korbgn),nwdsrd,1)
         CALL suread(Z(Korbgn),-1,nwdsrd,itest)
         CALL write(Lams,Z(Korbgn),nwdsrd,1)
         CALL close(Lams,1)
!
!     SWITCH FILE NUMBERS
!
         Phis = Phiss
         Lamamr = Lams
      ELSE
!
!     STORE GINO PHIS AS PHIS ON SOF
!
         ifile = Phis
         CALL mtrxo(Phis,Oldnam,itmlst(1),0,itest)
         item = itmlst(1)
         IF ( itest/=3 ) GOTO 400
      ENDIF
   ENDIF
   GOTO 99999
!
!     PROCESS SYSTEM FATAL ERRORS
!
 100  imsg = -2
   GOTO 300
 200  imsg = -3
 300  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   GOTO 99999
!
!     PROCESS MODULE FATAL ERRORS
!
 400  IF ( itest==2 ) THEN
      ismg = -11
   ELSEIF ( itest==3 ) THEN
      imsg = -1
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==6 ) THEN
      imsg = -10
   ELSE
      imsg = -9
   ENDIF
   Dry = -2
   CALL smsg1(imsg,item,Oldnam,modnam)
99999 RETURN
END SUBROUTINE mred2c
