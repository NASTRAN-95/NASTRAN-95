
SUBROUTINE invfbs(Dx,Dy,Iobuf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Cdp , Csp , Diag , Eofnrw , Rc(2) , Rd , Rdrew , Rect , Rew , Row , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew
   DOUBLE PRECISION Da
   INTEGER Eol , Filel(7) , Fileu(7) , Ibuf , Identy , Idummy(27) , Ii , Iopen , Lowtri , Mach , Norew , Nout , Nrow , Nwds(4) , Rdp
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /infbsx/ Filel , Fileu
   COMMON /machin/ Mach
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Identy
   COMMON /system/ Ibuf , Nout
   COMMON /trdxx / Idummy , Iopen
   COMMON /type  / Rc , Nwds
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zntpkx/ A , Ii , Eol
!
! Dummy argument declarations
!
   DOUBLE PRECISION Dx(1) , Dy(1)
   INTEGER Iobuf(1)
!
! Local variable declarations
!
   DOUBLE PRECISION djj , dtemp , dyj , epsi
   INTEGER i , ifile , ifw , ijj(2) , in1 , in2 , ioff , j , ji , jj , jx , k , ntms , nwd , parm(4) , typear
   REAL x1 , x2
!
! End of declarations
!
!
!     DOUBLE PRECISION VERSION
!
!     INVFBS IS A SPECIAL FORWARD-BACKWARD SUBSTITUTION ROUTINE FOR
!     INVPWR. IT OPERATES ON CONJUNCTION WITH SDCOMP.
!     THE ARITHMETIC PRECISION IS THAT OF THE INPUT FILE
!
!     FILEL    =  MATRIX CONTROL BLOCK FOR THE LOWER TRIANGLE L
!     FILEU    =  MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE U
!     DX       =  THE LOAD VECTOR B
!     DY       =  THE SOLUTION VECTOR X
!     IOBUF    =  THE INPUT BUFFER
!
!     COMMENT FROM G.CHAN/UNISYS, 6/89
!     IF LOAD IS SUDDENLY INCREADED TO A LARGE VALUE, THE VAX MACHINE
!     MAY BLOW ITS TOP (ARITHMETIC FAULT, FLOATING OVERFLOW) BECAUSE
!     VAX DOUBLE PRECISION REAL NUMBERS ARE LIMITED TO 10**38, SAME
!     LIMIT AS THE SINGLE PRECISION REAL NUMBERS. OTHER MACHINES ALLOW
!     MUCH LARGER LIMITS FOR DOUBLE PRECISION NUMBERS.
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR
   EQUIVALENCE (A(1),Da) , (Filel(3),Nrow) , (djj,ijj(1))
   DATA epsi/1.0D-24/
   DATA parm(3) , parm(4)/4HINVF , 4HBS  /
!
!
!     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
!
   DO i = 1 , Nrow
      Dy(i) = Dx(i)
   ENDDO
   typear = Rdp
!
!     OPEN FILE FOR THE LOWER TRIANGLE
!     IOPEN WAS SET TO -20 BY STEP2
!
   parm(2) = Filel(1)
   IF ( Iopen==-20 ) CALL fwdrec(*1100,Filel(1))
   IF ( Filel(7)<0 ) THEN
!
!
!     NEW METHOD
!     FILEL HAS BEEN RE-WRITTEN FORWARD FIRST THAN BACKWARD BY UNPSCR
!     IN INVP3)
!
!     THE LOAD VECTOR DX WILL BE DESTROYED IN THIS NEW METHOD
!
!     FORWARD SWEEP DIRECTLY ON SOLUTION VECTOR DY
!
      ifile = -Filel(7)
      parm(2) = ifile
      nwd = Nwds(Filel(5))
      IF ( Filel(4)/=2 ) THEN
         WRITE (Nout,99001) Filel(4)
99001    FORMAT ('0*** FILEL MATRIX IN WRONG FORMAT. UNPSCR FLAG =',I3)
         parm(1) = -37
         CALL mesage(parm(1),parm(2),parm(3))
      ELSE
         ifw = +1
         CALL rewind(ifile)
         CALL skprec(ifile,1)
         CALL read(*1100,*1200,ifile,Dx,2,0,i)
         ntms = 0
         DO j = 1 , Nrow
            djj = Dx(ntms+1)
            Ii = ijj(1)
            jj = ijj(2)
            IF ( Ii/=j ) GOTO 1300
            ntms = jj - Ii + 1
            ji = ntms*nwd + 2
            CALL read(*1100,*1200,ifile,Dx,ji,0,i)
            IF ( ntms>1 ) THEN
               dyj = Dy(j)
               IF ( dabs(dyj)>=epsi ) THEN
                  DO i = 2 , ntms
                     Ii = Ii + 1
                     Dy(Ii) = Dy(Ii) + Dx(i)*dyj
                  ENDDO
               ENDIF
            ENDIF
            Dy(j) = Dy(j)/Dx(1)
         ENDDO
!
!     BACKWARD SUBSTITUTION OMIT DIAGONAL
!
         ifw = -1
         IF ( Nrow/=1 ) THEN
            j = Nrow
            DO jx = 1 , Nrow
               djj = Dx(ntms+1)
               Ii = ijj(1)
               jj = ijj(2)
               IF ( Ii/=j ) GOTO 1300
               ntms = jj - Ii + 1
               ji = ntms*nwd + 2
               CALL read(*1100,*1200,ifile,Dx,ji,0,i)
               IF ( ntms>1 ) THEN
                  DO i = 2 , ntms
                     Ii = Ii + 1
                     Dy(j) = Dy(j) + Dx(i)*Dy(Ii)
                  ENDDO
               ENDIF
               j = j - 1
            ENDDO
         ENDIF
      ENDIF
      GOTO 99999
   ELSE
!
!     NASTRAN ORIGINAL CODE
!
!     BEGIN FORWARD PASS
!
      j = 1
      CALL intpk(*400,Filel(1),0,typear,0)
   ENDIF
 100  DO WHILE ( Eol==0 )
      CALL zntpki
      IF ( j<Ii ) GOTO 300
      IF ( j==Ii ) THEN
!
!     PERFORM THE REQUIRED ROW INTERCHANGE
!
         in1 = j + ifix(sngl(Da))
         dtemp = Dy(j)
         Dy(j) = Dy(in1)
         Dy(in1) = dtemp
         GOTO 200
      ENDIF
   ENDDO
   GOTO 1000
 200  IF ( Eol/=0 ) GOTO 400
   CALL zntpki
 300  IF ( Mach/=5 .OR. (dabs(Da)<1.D+19 .AND. dabs(Dy(j))<1.D+19) ) THEN
      Dy(Ii) = Dy(Ii) - Dy(j)*Da
      GOTO 200
   ELSE
      x1 = alog10(abs(sngl(Da)))
      x2 = alog10(abs(sngl(Dy(j))))
      IF ( x1+x2>38. ) GOTO 900
      Dy(Ii) = Dy(Ii) - Dy(j)*Da
      GOTO 200
   ENDIF
 400  j = j + 1
   IF ( j<Nrow ) THEN
      CALL intpk(*400,Filel(1),0,typear,0)
      GOTO 100
   ELSE
      CALL rewind(Filel(1))
      IF ( Iopen/=-20 ) CALL skprec(Filel,1)
!
!     BEGIN BACKWARD PASS
!
      ioff = Fileu(7) - 1
      parm(2) = Fileu(1)
      IF ( Iopen==-20 ) CALL fwdrec(*1100,Fileu(1))
      j = Nrow
   ENDIF
 500  CALL intpk(*1000,Fileu(1),0,typear,0)
   IF ( Eol/=0 ) GOTO 1000
 600  CALL zntpki
   i = Nrow - Ii + 1
   IF ( i/=j ) GOTO 800
!
!     DIVIDE BY THE DIAGONAL
!
   Dy(i) = Dy(i)/Da
!
!     SUBTRACT OFF REMAINING TERMS
!
 700  IF ( i>j ) GOTO 600
   IF ( Eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 500
      CALL rewind(Fileu)
      IF ( Iopen==-20 ) RETURN
      CALL skprec(Fileu,1)
      GOTO 99999
   ELSE
      CALL zntpki
      i = Nrow - Ii + 1
   ENDIF
 800  in1 = i
   in2 = j
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff
      in2 = k
   ENDIF
   IF ( Mach/=5 .OR. (dabs(Da)<1.D+19 .AND. dabs(Dy(in2))<1.D+19) ) THEN
      Dy(in1) = Dy(in1) - Dy(in2)*Da
      GOTO 700
   ELSE
      x1 = alog10(abs(sngl(Da)))
      x2 = alog10(abs(sngl(Dy(in2))))
      IF ( x1+x2<=38. ) THEN
         Dy(in1) = Dy(in1) - Dy(in2)*Da
         GOTO 700
      ENDIF
   ENDIF
!
 900  WRITE (Nout,99002) Sfm , parm(1) , parm(2)
99002 FORMAT (A25,' FROM ',2A4,'- SOLUTION VECTOR VALUE OVERFLOWS,',/5X,'POSSIBLY DUE TO SUDDEN INCREASE OF LARGE LOAD VECTOR OR ', &
             &'OTHER INPUT CONDITION')
   parm(1) = -37
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 1000 parm(1) = -5
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
!
!     ERROR
!
 1100 parm(1) = -2
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 1200 parm(1) = -3
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 99999
 1300 WRITE (Nout,99003) ifw , Ii , j
99003 FORMAT ('   ERROR IN INVFBS.   IFW),II,J =',I3,1H),2I6)
   parm(1) = -37
   CALL mesage(parm(1),parm(2),parm(3))
!
99999 END SUBROUTINE invfbs
