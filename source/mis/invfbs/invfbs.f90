!*==invfbs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE invfbs(Dx,Dy,Iobuf)
   IMPLICIT NONE
   USE c_infbsx
   USE c_machin
   USE c_names
   USE c_system
   USE c_trdxx
   USE c_type
   USE c_xmssg
   USE c_zntpkx
!
! Dummy argument declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: Dx
   REAL*8 , DIMENSION(1) :: Dy
   INTEGER , DIMENSION(1) :: Iobuf
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: da , djj , dtemp , dyj
   REAL*8 , SAVE :: epsi
   INTEGER :: i , ifile , ifw , in1 , in2 , ioff , j , ji , jj , jx , k , nrow , ntms , nwd , typear
   INTEGER , DIMENSION(2) :: ijj
   INTEGER , DIMENSION(4) , SAVE :: parm
   REAL :: x1 , x2
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (A(1),Da) , (Filel(3),Nrow) , (djj,ijj(1))
   DATA epsi/1.0D-24/
   DATA parm(3) , parm(4)/4HINVF , 4HBS  /
!
!
!     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
!
   DO i = 1 , nrow
      Dy(i) = Dx(i)
   ENDDO
   typear = rdp
!
!     OPEN FILE FOR THE LOWER TRIANGLE
!     IOPEN WAS SET TO -20 BY STEP2
!
   parm(2) = filel(1)
   IF ( iopen==-20 ) CALL fwdrec(*1100,filel(1))
   IF ( filel(7)<0 ) THEN
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
      ifile = -filel(7)
      parm(2) = ifile
      nwd = nwds(filel(5))
      IF ( filel(4)/=2 ) THEN
         WRITE (nout,99001) filel(4)
99001    FORMAT ('0*** FILEL MATRIX IN WRONG FORMAT. UNPSCR FLAG =',I3)
         parm(1) = -37
         CALL mesage(parm(1),parm(2),parm(3))
      ELSE
         ifw = +1
         CALL rewind(ifile)
         CALL skprec(ifile,1)
         CALL read(*1100,*1200,ifile,Dx,2,0,i)
         ntms = 0
         DO j = 1 , nrow
            djj = Dx(ntms+1)
            ii = ijj(1)
            jj = ijj(2)
            IF ( ii/=j ) GOTO 1300
            ntms = jj - ii + 1
            ji = ntms*nwd + 2
            CALL read(*1100,*1200,ifile,Dx,ji,0,i)
            IF ( ntms>1 ) THEN
               dyj = Dy(j)
               IF ( dabs(dyj)>=epsi ) THEN
                  DO i = 2 , ntms
                     ii = ii + 1
                     Dy(ii) = Dy(ii) + Dx(i)*dyj
                  ENDDO
               ENDIF
            ENDIF
            Dy(j) = Dy(j)/Dx(1)
         ENDDO
!
!     BACKWARD SUBSTITUTION OMIT DIAGONAL
!
         ifw = -1
         IF ( nrow/=1 ) THEN
            j = nrow
            DO jx = 1 , nrow
               djj = Dx(ntms+1)
               ii = ijj(1)
               jj = ijj(2)
               IF ( ii/=j ) GOTO 1300
               ntms = jj - ii + 1
               ji = ntms*nwd + 2
               CALL read(*1100,*1200,ifile,Dx,ji,0,i)
               IF ( ntms>1 ) THEN
                  DO i = 2 , ntms
                     ii = ii + 1
                     Dy(j) = Dy(j) + Dx(i)*Dy(ii)
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
      CALL intpk(*400,filel(1),0,typear,0)
   ENDIF
 100  DO WHILE ( eol==0 )
      CALL zntpki
      IF ( j<ii ) GOTO 300
      IF ( j==ii ) THEN
!
!     PERFORM THE REQUIRED ROW INTERCHANGE
!
         in1 = j + ifix(sngl(da))
         dtemp = Dy(j)
         Dy(j) = Dy(in1)
         Dy(in1) = dtemp
         GOTO 200
      ENDIF
   ENDDO
   GOTO 1000
 200  IF ( eol/=0 ) GOTO 400
   CALL zntpki
 300  IF ( mach/=5 .OR. (dabs(da)<1.D+19 .AND. dabs(Dy(j))<1.D+19) ) THEN
      Dy(ii) = Dy(ii) - Dy(j)*da
      GOTO 200
   ELSE
      x1 = alog10(abs(sngl(da)))
      x2 = alog10(abs(sngl(Dy(j))))
      IF ( x1+x2>38. ) GOTO 900
      Dy(ii) = Dy(ii) - Dy(j)*da
      GOTO 200
   ENDIF
 400  j = j + 1
   IF ( j<nrow ) THEN
      CALL intpk(*400,filel(1),0,typear,0)
      GOTO 100
   ELSE
      CALL rewind(filel(1))
      IF ( iopen/=-20 ) CALL skprec(filel,1)
!
!     BEGIN BACKWARD PASS
!
      ioff = fileu(7) - 1
      parm(2) = fileu(1)
      IF ( iopen==-20 ) CALL fwdrec(*1100,fileu(1))
      j = nrow
   ENDIF
 500  CALL intpk(*1000,fileu(1),0,typear,0)
   IF ( eol/=0 ) GOTO 1000
 600  CALL zntpki
   i = nrow - ii + 1
   IF ( i/=j ) GOTO 800
!
!     DIVIDE BY THE DIAGONAL
!
   Dy(i) = Dy(i)/da
!
!     SUBTRACT OFF REMAINING TERMS
!
 700  IF ( i>j ) GOTO 600
   IF ( eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 500
      CALL rewind(fileu)
      IF ( iopen==-20 ) RETURN
      CALL skprec(fileu,1)
      GOTO 99999
   ELSE
      CALL zntpki
      i = nrow - ii + 1
   ENDIF
 800  in1 = i
   in2 = j
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff
      in2 = k
   ENDIF
   IF ( mach/=5 .OR. (dabs(da)<1.D+19 .AND. dabs(Dy(in2))<1.D+19) ) THEN
      Dy(in1) = Dy(in1) - Dy(in2)*da
      GOTO 700
   ELSE
      x1 = alog10(abs(sngl(da)))
      x2 = alog10(abs(sngl(Dy(in2))))
      IF ( x1+x2<=38. ) THEN
         Dy(in1) = Dy(in1) - Dy(in2)*da
         GOTO 700
      ENDIF
   ENDIF
!
 900  WRITE (nout,99002) sfm , parm(1) , parm(2)
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
 1300 WRITE (nout,99003) ifw , ii , j
99003 FORMAT ('   ERROR IN INVFBS.   IFW),II,J =',I3,1H),2I6)
   parm(1) = -37
   CALL mesage(parm(1),parm(2),parm(3))
!
99999 END SUBROUTINE invfbs
