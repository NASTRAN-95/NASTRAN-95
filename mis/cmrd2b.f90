
SUBROUTINE cmrd2b(Kode)
   IMPLICIT NONE
   INTEGER Dry , Gbuf1 , Idum1 , Idum2(5) , Idum3(6) , Idum4 , Idum5(7) , Idum6 , Idum7 , Infile(11) , Iprntr , Iscr(11) , Korbgn , &
         & Korlen , Lamaap , Lamamr , Lams , Modlen , Nfound , Oldnam(2) , Phisl , Phissl , Phissr , Z(1)
   LOGICAL Modes
   REAL Rz(1)
   CHARACTER*23 Ufm
   COMMON /blank / Idum1 , Dry , Idum7 , Gbuf1 , Idum2 , Infile , Idum3 , Iscr , Korlen , Korbgn , Oldnam , Idum5 , Modes , Idum6 , &
                 & Lamaap , Nfound , Modlen
   COMMON /system/ Idum4 , Iprntr
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Kode
   INTEGER i , ifile , imsg , item , itest , itmlst(3) , lamwds , modnam(2) , nwds , nwdsrd , rgdfmt
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
   EQUIVALENCE (Rz(1),Z(1)) , (Lamamr,Infile(2)) , (Phissr,Infile(3)) , (Phissl,Infile(4)) , (Lams,Iscr(5)) , (Phisl,Iscr(6))
   DATA modnam/4HCMRD , 4H2B  /
   DATA itmlst/4HPHIS , 4HPHIL , 4HLAMS/
   DATA rgdfmt/3/
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
         IF ( itest==2 .OR. itest>3 ) GOTO 400
         ifile = Lamamr
         CALL gopen(Lamamr,Z(Gbuf1),0)
         CALL fwdrec(*200,Lamamr)
         itest = 3
         CALL sfetch(Oldnam,itmlst(3),2,itest)
         IF ( itest/=3 ) GOTO 400
         DO i = 1 , 2
            Z(Korbgn+i-1) = Oldnam(i)
         ENDDO
         Z(Korbgn+2) = rgdfmt
         Z(Korbgn+3) = Modlen
         CALL suwrt(Z(Korbgn),4,2)
         lamwds = Modlen - 1
         Rz(Korbgn+6) = 0.0
         DO i = 1 , lamwds
            CALL read(*100,*200,Lamamr,Z(Korbgn),6,0,nwds)
            CALL suwrt(Z(Korbgn),7,1)
         ENDDO
         CALL read(*100,*200,Lamamr,Z(Korbgn),6,0,nwds)
         CALL close(Lamamr,1)
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
      CALL mtrxi(Phisl,Oldnam,item,0,itest)
      IF ( itest/=1 ) GOTO 400
!
!     READ SOF LAMS ONTO GINO LAMS SCRATCH FILE
!
      CALL sfetch(Oldnam,itmlst(3),1,itest)
      item = itmlst(3)
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
      IF ( Kode==1 ) Phissr = Phisl
      IF ( Kode==2 ) Phissl = Phisl
      Lamamr = Lams
      RETURN
   ELSE
!
!     STORE GINO PHISS(R,L) AS PHI(S,L) ON SOF
!
      ifile = Phissr
      IF ( Kode==2 ) ifile = Phissl
      item = itmlst(Kode)
      CALL mtrxo(ifile,Oldnam,item,0,itest)
      IF ( itest/=3 ) GOTO 400
      RETURN
   ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 100  imsg = -2
   GOTO 300
 200  imsg = -3
 300  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   RETURN
!
!     PROCESS MODULE FATAL ERRORS
!
 400  IF ( itest==2 ) THEN
!
      WRITE (Iprntr,99001) Ufm , modnam , item , Oldnam
99001 FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
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
99002 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURBED.')
      Dry = -2
      RETURN
   ELSE
      WRITE (Iprntr,99003) Ufm , modnam , item , Oldnam
!
99003 FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
      Dry = -2
      RETURN
   ENDIF
   CALL smsg(imsg,item,Oldnam)
   RETURN
!
END SUBROUTINE cmrd2b
