!*==amg.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE amg
   IMPLICIT NONE
   USE C_AMGBUG
   USE C_AMGMN
   USE C_AMGP2
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: acpt , aero , ajjl , skj , w1jk , w2jk
   INTEGER :: buf1 , buf2 , buf3 , i , icore , ifile , j , method , n , n1 , nmk , nms
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
!     THIS IS THE MAIN DRIVER FOR AEROELASTIC MATRIX GENERATION
!
!     NOTES ON NEW METHOD IMPLIMENTATION
!     1. ACPT FILE WILL BE POSITIONED READY TO READ AN INPUT RECORD
!        LEAVE FILE READY TO READ NEXT RECORD.
!
!     2. ALWAYS PACK OUT A COLUMN (REALY A ROW) OF NJ LENGTH
!        OUTPUT FILE, PACKX, AND TRAILER(MCB) WILL BE SET UP
!
!     3. YOUR ROW POSITION WILL START AT NROW + 1
!
!     4. ALWAYS BUMP NROW BY THE NUMBER OF ROWS WHICH EXIST IN
!        YOUR INPUT RECORD
!
!     5. COMPUTATIONS FOR AJJK MATRIX WILL HAVE 3 BUFFERS OF CORE USED
!        COMPUTATIONS FOR OTHER MATRICES WILL HAVE 4 BUFFERS USED
!
   DATA name/4HAMG  , 4H    /
   DATA aero/101/ , acpt/102/ , ajjl/201/ , skj/202/ , w1jk/203/ , w2jk/204/
!
   Debug = .FALSE.
   CALL sswtch(20,j)
   IF ( j==1 ) Debug = .TRUE.
!
!     USE IZ TO COMPUTE BUFFERS
!
   icore = korsz(Iz)
   ifile = 4*Sysbuf + 3*Nj
   IF ( icore>ifile ) THEN
!
!     OPEN INPUT STRUCTURAL DATA
!
      icore = icore - Sysbuf
      CALL gopen(acpt,Iz(icore+1),0)
!
!     OPEN AND SKIP HEADER ON AERO
!
      ifile = aero
      buf1 = icore - Sysbuf
      CALL gopen(aero,Iz(buf1+1),0)
!
!     READ 3 INPUT WORDS INTO COMMON
!
      CALL read(*900,*900,aero,Nd,3,1,n)
!
!     OPEN OUTPUT FILE FOR AJJL MATRIX, SET UP TRAILER AND WRITE HEADER
!
      buf2 = buf1 - Sysbuf
      ifile = ajjl
      CALL open(*800,ajjl,Iz(buf2+1),1)
      CALL fname(ajjl,Mcb)
      CALL write(ajjl,Mcb,2,0)
      CALL write(ajjl,Nj,1,0)
      CALL write(ajjl,Nk,1,0)
      buf3 = buf2 - Sysbuf
      CALL gopen(skj,Iz(buf3+1),1)
      ifile = aero
      CALL read(*800,*100,aero,Iz,buf3,0,n)
   ENDIF
   nms = -8
   CALL mesage(nms,ifile,name)
   GOTO 99999
 100  nmk = n/2
   CALL rewind(aero)
   CALL fwdrec(*900,aero)
   CALL fwdrec(*900,aero)
   CALL write(ajjl,nmk,1,0)
   CALL write(ajjl,Iz,n,0)
   ifile = acpt
   Iz(1) = 0
   n1 = 2
   DO
      CALL read(*200,*200,acpt,method,1,0,n)
      Iz(1) = Iz(1) + 1
      Iz(n1) = method
      IF ( method==2 ) THEN
!
!     DOUBLET LATTICE WITH BODIES
!
         CALL read(*800,*900,acpt,Mcb,2,1,n)
         Iz(n1+1) = Mcb(1)
         Iz(n1+2) = Mcb(2)
      ELSEIF ( method==3 .OR. method==4 .OR. method==5 ) THEN
!
!     MACH BOX  STRIP THEORY  PISTON THEORY
!
         CALL read(*800,*900,acpt,Mcb,1,1,n)
         Iz(n1+1) = Mcb(1)
         Iz(n1+2) = Mcb(1)
      ELSEIF ( method==6 ) THEN
!
!     COMPRESSOR BLADE METHOD
!
         CALL read(*800,*900,acpt,Mcb,5,1,n)
!
!     NUMBER OF COLUMNS ADDED IS NJ = NK = (NSTNS*NLINES) FOR THE BLADE
!
         Iz(n1+1) = Mcb(4)*Mcb(5)
         Iz(n1+2) = Iz(n1+1)
      ELSEIF ( method==7 ) THEN
!
!     SWEPT TURBOPROP BLADE METHOD
!
         CALL read(*800,*900,acpt,Mcb,5,1,n)
!
!     NUMBER OF COLUMNS ADDED IS NJ = NK = (2*NSTNS*NLINES) FOR THE PROP
!
         Iz(n1+1) = 2*Mcb(4)*Mcb(5)
         Iz(n1+2) = Iz(n1+1)
      ELSE
!
!     DOUBLET LATTICE METHOD
!
         CALL read(*800,*900,acpt,Mcb,3,1,n)
!
!     NUMBER OF COLUMNS ADDED EQUAL NUMBER OF BOXES
!
         Iz(n1+1) = Mcb(3)
         Iz(n1+2) = Mcb(3)
      ENDIF
      n1 = n1 + 3
   ENDDO
 200  CALL rewind(acpt)
   CALL write(ajjl,Iz,n1-1,1)
   Mcb(1) = ajjl
   Mcb(2) = 0
   Mcb(3) = Nj
   Mcb(4) = 2
   Mcb(5) = 3
   Mcb(6) = 0
   Mcb(7) = 0
   Incr = 1
   Tskj(1) = skj
   Tskj(2) = 0
   Tskj(3) = Nk
   Tskj(4) = 2
   Tskj(5) = 3
   Tskj(6) = 0
   Tskj(7) = 0
   ifile = acpt
!
!     READ MACH NUMBER AND REDUCED FREQUENCY AND LOOP UNTIL COMPLETED
!
 300  CALL read(*500,*500,aero,Fmach,2,0,n)
!
!     NUMBER OF ROWS ADDED BY EACH RECORD ON ACPT
!
   Nrow = 0
   Isk = 1
   Nsk = 0
!
!     SKIP HEADER
!
   CALL fwdrec(*900,acpt)
   DO
!
!     READ A RECORD AND LOOP BY METHOD UNTIL EOF
!     NSK IS BUMPED BY DRIVERS = COLUMNS BUILT  ISK = NEXT COLUMN
!
      CALL read(*400,*400,acpt,method,1,0,n)
      IF ( method==2 ) THEN
!
!     DOUBLET LATTICE WITH BODIES
!
         CALL dlamby(acpt,ajjl,skj)
      ELSEIF ( method==3 ) THEN
!
!     MACH BOX
!
         CALL mbamg(acpt,ajjl,skj)
      ELSEIF ( method==4 ) THEN
!
!     STRIP THEORY
!
         CALL stpda(acpt,ajjl,skj)
      ELSEIF ( method==5 ) THEN
!
!     PISTON THEORY
!
         CALL pstamg(acpt,ajjl,skj)
      ELSEIF ( method==6 ) THEN
!
!     COMPRESSOR BLADE METHOD
!
         CALL amgb1(acpt,ajjl,skj)
      ELSEIF ( method==7 ) THEN
!
!     SWEPT TURBOPROP BLADE METHOD
!
         CALL amgt1(acpt,ajjl,skj)
      ELSE
!
!     DOUBLET LATTICE METHOD
!
         CALL dlamg(acpt,ajjl,skj)
      ENDIF
      IF ( Nsk>Nk ) THEN
!
!     ERROR MESSAGES
!
!     NROW IN RECORDS DID NOT MATCH NJ PARAMETER
!
         Nrow = Nsk
         Nj = Nk
         GOTO 700
      ELSEIF ( Nrow>Nj ) THEN
         GOTO 700
      ENDIF
   ENDDO
 400  CALL rewind(acpt)
   GOTO 300
 500  CALL close(aero,1)
   CALL close(ajjl,1)
   CALL close(skj,1)
   CALL wrttrl(Tskj)
   CALL wrttrl(Mcb)
!
!     COMPUTE W1JK - W2JK
!
!
!     OPEN OUTPUT FILES
!
   CALL fwdrec(*900,acpt)
   ifile = w1jk
   CALL gopen(w1jk,Iz(buf1+1),1)
   ifile = w2jk
   CALL gopen(w2jk,Iz(buf2+1),1)
   ifile = acpt
!
!     SET UP PACKX AND TRAILERS
!
   Incr = 1
   Iti = 1
   Ito = 1
!
!     II AND NN ARE BUMPED BY METHOD DRIVERS
!
   Ii = 1
   DO i = 2 , 7
      Tw1jk(i) = 0
      Tw2jk(i) = 0
   ENDDO
   Tw1jk(1) = w1jk
   Tw2jk(1) = w2jk
   Tw1jk(3) = Nk
   Tw1jk(4) = 2
   Tw1jk(5) = 1
   Tw2jk(3) = Nk
   Tw2jk(4) = 2
   Tw2jk(5) = 1
   DO
!
!     READ A RECORD AND LOOP ON METHOD UNTIL EOR
!
      CALL read(*600,*600,acpt,method,1,0,n)
      IF ( method==2 ) THEN
!
!     DOUBLET LATTICE WITH BODIES
!
         CALL dlbpt2(acpt,w1jk,w2jk)
      ELSEIF ( method==3 .OR. method==4 .OR. method==5 ) THEN
!
!     STRIP THEORY     PISTON THEORY
!     MACH BOX
!
         CALL stppt2(acpt,w1jk,w2jk)
      ELSEIF ( method==6 ) THEN
!
!     COMPRESSOR BLADE METHOD
!
         CALL amgb2(acpt,w1jk,w2jk)
      ELSEIF ( method==7 ) THEN
!
!     SWEPT TURBOPROP BLADE METHOD
!
         CALL amgt2(acpt,w1jk,w2jk)
      ELSE
!
!     DOUBLET LATTICE METHOD
!
         CALL dlpt2(acpt,w1jk,w2jk)
      ENDIF
      IF ( Nn>Nk ) THEN
         Nrow = Nn
         Nj = Nk
         GOTO 700
      ENDIF
   ENDDO
!
!     DONE
!
 600  CALL close(acpt,1)
   CALL close(w1jk,1)
   CALL close(w2jk,1)
   CALL wrttrl(Tw1jk)
   CALL wrttrl(Tw2jk)
   RETURN
 700  WRITE (Iout,99001) Sfm , Nrow , Nj
99001 FORMAT (A25,' 2264, NUMBER OF ROWS COMPUTED (',I4,') WAS GREATER',' THAN SIZE REQUESTED FOR OUTPUT MATRIX (',I4,2H).)
   CALL mesage(-61,n,name)
 800  nms = -1
   CALL mesage(nms,ifile,name)
   GOTO 99999
 900  nms = -2
   CALL mesage(nms,ifile,name)
99999 END SUBROUTINE amg
