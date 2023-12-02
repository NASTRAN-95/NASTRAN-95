!*==amg.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE amg
   IMPLICIT NONE
   USE c_amgbug
   USE c_amgmn
   USE c_amgp2
   USE c_blank
   USE c_packx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
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
! Local variable declarations rewritten by SPAG
!
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
   debug = .FALSE.
   CALL sswtch(20,j)
   IF ( j==1 ) debug = .TRUE.
!
!     USE IZ TO COMPUTE BUFFERS
!
   icore = korsz(iz)
   ifile = 4*sysbuf + 3*nj
   IF ( icore>ifile ) THEN
!
!     OPEN INPUT STRUCTURAL DATA
!
      icore = icore - sysbuf
      CALL gopen(acpt,iz(icore+1),0)
!
!     OPEN AND SKIP HEADER ON AERO
!
      ifile = aero
      buf1 = icore - sysbuf
      CALL gopen(aero,iz(buf1+1),0)
!
!     READ 3 INPUT WORDS INTO COMMON
!
      CALL read(*900,*900,aero,nd,3,1,n)
!
!     OPEN OUTPUT FILE FOR AJJL MATRIX, SET UP TRAILER AND WRITE HEADER
!
      buf2 = buf1 - sysbuf
      ifile = ajjl
      CALL open(*800,ajjl,iz(buf2+1),1)
      CALL fname(ajjl,mcb)
      CALL write(ajjl,mcb,2,0)
      CALL write(ajjl,nj,1,0)
      CALL write(ajjl,nk,1,0)
      buf3 = buf2 - sysbuf
      CALL gopen(skj,iz(buf3+1),1)
      ifile = aero
      CALL read(*800,*100,aero,iz,buf3,0,n)
   ENDIF
   nms = -8
   CALL mesage(nms,ifile,name)
   GOTO 99999
 100  nmk = n/2
   CALL rewind(aero)
   CALL fwdrec(*900,aero)
   CALL fwdrec(*900,aero)
   CALL write(ajjl,nmk,1,0)
   CALL write(ajjl,iz,n,0)
   ifile = acpt
   iz(1) = 0
   n1 = 2
   DO
      CALL read(*200,*200,acpt,method,1,0,n)
      iz(1) = iz(1) + 1
      iz(n1) = method
      IF ( method==2 ) THEN
!
!     DOUBLET LATTICE WITH BODIES
!
         CALL read(*800,*900,acpt,mcb,2,1,n)
         iz(n1+1) = mcb(1)
         iz(n1+2) = mcb(2)
      ELSEIF ( method==3 .OR. method==4 .OR. method==5 ) THEN
!
!     MACH BOX  STRIP THEORY  PISTON THEORY
!
         CALL read(*800,*900,acpt,mcb,1,1,n)
         iz(n1+1) = mcb(1)
         iz(n1+2) = mcb(1)
      ELSEIF ( method==6 ) THEN
!
!     COMPRESSOR BLADE METHOD
!
         CALL read(*800,*900,acpt,mcb,5,1,n)
!
!     NUMBER OF COLUMNS ADDED IS NJ = NK = (NSTNS*NLINES) FOR THE BLADE
!
         iz(n1+1) = mcb(4)*mcb(5)
         iz(n1+2) = iz(n1+1)
      ELSEIF ( method==7 ) THEN
!
!     SWEPT TURBOPROP BLADE METHOD
!
         CALL read(*800,*900,acpt,mcb,5,1,n)
!
!     NUMBER OF COLUMNS ADDED IS NJ = NK = (2*NSTNS*NLINES) FOR THE PROP
!
         iz(n1+1) = 2*mcb(4)*mcb(5)
         iz(n1+2) = iz(n1+1)
      ELSE
!
!     DOUBLET LATTICE METHOD
!
         CALL read(*800,*900,acpt,mcb,3,1,n)
!
!     NUMBER OF COLUMNS ADDED EQUAL NUMBER OF BOXES
!
         iz(n1+1) = mcb(3)
         iz(n1+2) = mcb(3)
      ENDIF
      n1 = n1 + 3
   ENDDO
 200  CALL rewind(acpt)
   CALL write(ajjl,iz,n1-1,1)
   mcb(1) = ajjl
   mcb(2) = 0
   mcb(3) = nj
   mcb(4) = 2
   mcb(5) = 3
   mcb(6) = 0
   mcb(7) = 0
   incr = 1
   tskj(1) = skj
   tskj(2) = 0
   tskj(3) = nk
   tskj(4) = 2
   tskj(5) = 3
   tskj(6) = 0
   tskj(7) = 0
   ifile = acpt
!
!     READ MACH NUMBER AND REDUCED FREQUENCY AND LOOP UNTIL COMPLETED
!
 300  CALL read(*500,*500,aero,fmach,2,0,n)
!
!     NUMBER OF ROWS ADDED BY EACH RECORD ON ACPT
!
   nrow = 0
   isk = 1
   nsk = 0
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
      IF ( nsk>nk ) THEN
!
!     ERROR MESSAGES
!
!     NROW IN RECORDS DID NOT MATCH NJ PARAMETER
!
         nrow = nsk
         nj = nk
         GOTO 700
      ELSEIF ( nrow>nj ) THEN
         GOTO 700
      ENDIF
   ENDDO
 400  CALL rewind(acpt)
   GOTO 300
 500  CALL close(aero,1)
   CALL close(ajjl,1)
   CALL close(skj,1)
   CALL wrttrl(tskj)
   CALL wrttrl(mcb)
!
!     COMPUTE W1JK - W2JK
!
!
!     OPEN OUTPUT FILES
!
   CALL fwdrec(*900,acpt)
   ifile = w1jk
   CALL gopen(w1jk,iz(buf1+1),1)
   ifile = w2jk
   CALL gopen(w2jk,iz(buf2+1),1)
   ifile = acpt
!
!     SET UP PACKX AND TRAILERS
!
   incr = 1
   iti = 1
   ito = 1
!
!     II AND NN ARE BUMPED BY METHOD DRIVERS
!
   ii = 1
   DO i = 2 , 7
      tw1jk(i) = 0
      tw2jk(i) = 0
   ENDDO
   tw1jk(1) = w1jk
   tw2jk(1) = w2jk
   tw1jk(3) = nk
   tw1jk(4) = 2
   tw1jk(5) = 1
   tw2jk(3) = nk
   tw2jk(4) = 2
   tw2jk(5) = 1
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
      IF ( nn>nk ) THEN
         nrow = nn
         nj = nk
         GOTO 700
      ENDIF
   ENDDO
!
!     DONE
!
 600  CALL close(acpt,1)
   CALL close(w1jk,1)
   CALL close(w2jk,1)
   CALL wrttrl(tw1jk)
   CALL wrttrl(tw2jk)
   RETURN
 700  WRITE (iout,99001) sfm , nrow , nj
99001 FORMAT (A25,' 2264, NUMBER OF ROWS COMPUTED (',I4,') WAS GREATER',' THAN SIZE REQUESTED FOR OUTPUT MATRIX (',I4,2H).)
   CALL mesage(-61,n,name)
 800  nms = -1
   CALL mesage(nms,ifile,name)
   GOTO 99999
 900  nms = -2
   CALL mesage(nms,ifile,name)
99999 END SUBROUTINE amg
