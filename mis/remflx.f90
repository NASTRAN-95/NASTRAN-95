
SUBROUTINE remflx(Ngrids)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Ditfil , Ibuf , Ii , Iihmat , Incr , Inflag , Iout , Iz(1) , Jncr , Jout , Last , Lcore , Matid , Mptfil , Ne(1) ,&
         & Nelems , Nn , Nnhmat
   REAL Costh , Dum(10) , Eltemp , Sinth , Stress , Xmat(6) , Z(1)
   LOGICAL Remfl
   CHARACTER*23 Ufm
   COMMON /biot  / Dum , Buf1 , Remfl , Lcore
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /hmatdd/ Iihmat , Nnhmat , Mptfil , Ditfil
   COMMON /hmtout/ Xmat
   COMMON /matin / Matid , Inflag , Eltemp , Stress , Sinth , Costh
   COMMON /system/ Ibuf , Iout
   COMMON /unpakx/ Jout , Ii , Nn , Jncr
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ngrids
!
! Local variable declarations
!
   REAL angle , c , cs , csq , den , det , ecpt(200) , g(3,3) , rem(3) , s , ssq , temp , x2
   INTEGER buf2 , buf3 , dit , eltype , estwds , file , hest , i , ict , idx , iflag , igrids , ihc , imid , ipoint(32) , ipt ,     &
         & irem , isil , ising , isub , itemp , ith , iwork(3,3) , j , jcount , jel , kount , mcb(7) , mpt , n , n3 , nam(2) ,      &
         & ncol , ncount , necpt(200) , nextz , nhit , nrow , pointr(6,19) , remfld , scr1
   LOGICAL hitone
!
! End of declarations
!
!
!     CHECK FOR REMFLUX IN MAGNETIC FIELD PROBLEMS WHEN COMPUTING
!     PROLATE SPHEROIDAL COEFFICIENTS
!
   EQUIVALENCE (Z(1),Iz(1)) , (ecpt(1),necpt(1))
   DATA remfld , hest , mpt , dit , scr1/107 , 108 , 109 , 110 , 301/
   DATA nam/4HREMF , 4HLX  /
!
!                   TYPE  ISIL   MID   ITH NGRIDS ITEMP
!
   DATA pointr/1 , 2 , 4 , 0 , 2 , 17 , 3 , 2 , 4 , 0 , 2 , 16 , 6 , 2 , 6 , 5 , 3 , 27 , 9 , 2 , 6 , 5 , 3 , 21 , 10 , 2 , 4 , 0 , &
      & 2 , 17 , 16 , 2 , 7 , 6 , 4 , 26 , 17 , 2 , 6 , 5 , 3 , 21 , 18 , 2 , 7 , 6 , 4 , 26 , 19 , 2 , 7 , 6 , 4 , 32 , 34 , 2 ,   &
      & 16 , 0 , 2 , 42 , 36 , 2 , 6 , 5 , 3 , 19 , 37 , 2 , 7 , 6 , 4 , 24 , 39 , 3 , 2 , 0 , 4 , 23 , 40 , 3 , 2 , 0 , 6 , 33 ,   &
      & 41 , 3 , 2 , 0 , 8 , 43 , 42 , 3 , 2 , 0 , 8 , 43 , 65 , 2 , 10 , 0 , 8 , 48 , 66 , 2 , 22 , 0 , 20 , 108 , 67 , 2 , 34 ,   &
      & 0 , 32 , 168/
!
   Remfl = .FALSE.
   mcb(1) = remfld
   CALL rdtrl(mcb)
!
!     CHECK FOR ANY REMFLUX
!
   IF ( mcb(6)==0 ) RETURN
!
!     TOO BAD
!
   Remfl = .TRUE.
   ncol = mcb(2)
   nrow = mcb(3)
   ncount = nrow/3
!
!     BRING IN MATERIALS SINCE H=B/MU
!
   Iihmat = Ngrids
   Nnhmat = Lcore
   Mptfil = mpt
   Ditfil = dit
   CALL prehma(Z)
   nextz = Nnhmat + 1
!
   buf2 = Buf1 - Ibuf
   buf3 = buf2 - Ibuf
!
!     SET UP POINTERS
!     IHC = START OF RESULTS HC = B/MU
!     IREM= REMFL COLUMN
!     ICT = COUNTER FOR NUMBER OF ELEMENTS AT EACH PROLATE GRID (FOR
!     AVERAGING
!
   ihc = nextz
   irem = ihc + 3*Ngrids
   ict = irem + nrow
   IF ( buf3<ict+Ngrids ) THEN
      n = -8
      file = 0
      CALL mesage(n,file,nam)
      CALL mesage(-61,0,0)
      GOTO 99999
   ELSE
!
      CALL gopen(scr1,Z(Buf1),1)
      CALL gopen(remfld,Z(buf2),0)
      CALL gopen(hest,Z(buf3),0)
!
      Ii = 1
      Nn = nrow
      Jncr = 1
      Jout = 1
      jcount = 0
   ENDIF
!
 100  DO i = 1 , Ngrids
      Iz(ict+i) = 0
   ENDDO
   n3 = 3*Ngrids
   DO i = 1 , n3
      Z(ihc+i) = 0.
   ENDDO
!
!     UNPACK A COULMN OF REMFLD
!
   jcount = jcount + 1
   CALL unpack(*200,remfld,Z(irem+1))
!
!     SINCE THE ELEMENT DATA DO NOT CHANGE WITH REMFLD COLIMN, THIS IS
!     NOT NECESSARILY THE BEST KIND OF LOOPING. BUT OTHER WAYS WOULD
!     NEED MORE CORE AND IF THERE IS MORE THAN ONE REMFLUX CASE, IT
!     WOULD BE A SURPRISE
!
   file = hest
   kount = 0
   GOTO 300
!
!     ZERO COLUMN
!
 200  DO i = 1 , n3
      Z(ihc+i) = 0.
   ENDDO
   GOTO 800
 300  CALL read(*700,*1000,hest,eltype,1,0,iflag)
   idx = (eltype-1)*Incr
   estwds = Ne(idx+12)
!
!     PICK UP ELEMENT TYPE INFO
!
   DO i = 1 , 19
      jel = i
      IF ( eltype<pointr(1,i) ) EXIT
      IF ( eltype==pointr(1,i) ) GOTO 400
   ENDDO
!
   WRITE (Iout,99001) Ufm
99001 FORMAT (A23,', ILLEGAL ELEMENT TYPE IN REMFLX')
   CALL mesage(-61,0,0)
   GOTO 99999
!
 400  isil = pointr(2,jel)
   imid = pointr(3,jel)
   ith = pointr(4,jel)
   igrids = pointr(5,jel)
   itemp = pointr(6,jel)
 500  DO
!
      CALL read(*900,*300,hest,ecpt,estwds,0,iflag)
!
!     PICK UP REMFLUX FOR THIS ELEMENT
!
      kount = kount + 1
      nhit = 0
      hitone = .FALSE.
      DO i = 1 , igrids
         ipoint(i) = 0
      ENDDO
      DO i = 1 , Ngrids
         DO j = 1 , igrids
            ipt = necpt(isil+j-1)
            IF ( ipt==Iz(i) ) GOTO 520
         ENDDO
         CYCLE
!
!     MATCH
!
 520     hitone = .TRUE.
         nhit = nhit + 1
         Iz(ict+i) = Iz(ict+i) + 1
         ipoint(j) = i
         IF ( nhit==igrids ) GOTO 600
      ENDDO
      IF ( hitone ) EXIT
   ENDDO
!
 600  isub = irem + 3*(kount-1)
   rem(1) = Z(isub+1)
   rem(2) = Z(isub+2)
   rem(3) = Z(isub+3)
!
!     PICK UP MATERIALS
!
   Matid = necpt(imid)
   Eltemp = ecpt(itemp)
   Inflag = 3
   Sinth = 0.
   Costh = 0.
   CALL hmat(necpt(1))
   g(1,1) = Xmat(1)
   g(1,2) = Xmat(2)
   g(1,3) = Xmat(3)
   g(2,1) = Xmat(2)
   g(2,2) = Xmat(4)
   g(2,3) = Xmat(5)
   g(3,1) = Xmat(3)
   g(3,2) = Xmat(5)
   g(3,3) = Xmat(6)
!
!     FOR COMMENTS ON MATERIALS SEE EM2D
!
   IF ( ith/=0 ) THEN
      angle = ecpt(ith)*0.017453293
      IF ( Xmat(3)==0. .AND. Xmat(5)==0. ) THEN
         IF ( abs(angle)>.0001 ) THEN
            s = sin(angle)
            c = cos(angle)
            csq = c*c
            ssq = s*s
            cs = c*s
            x2 = 2.*cs*Xmat(2)
            g(1,1) = csq*Xmat(1) - x2 + ssq*Xmat(4)
            g(1,2) = cs*(Xmat(1)-Xmat(4)) + (csq-ssq)*Xmat(2)
            g(2,2) = ssq*Xmat(1) + x2 + csq*Xmat(4)
            g(2,1) = g(1,2)
            g(3,3) = Xmat(6)
            g(1,3) = 0.
            g(2,3) = 0.
            g(3,1) = 0.
            g(3,2) = 0.
!
!     SINCE MAT5 INFO FOR TRAPRG,TRIARG IS GIVEN IN X-Y ORDER,
!     INETRCHANGE YA AND Z
!
            temp = g(2,2)
            g(2,2) = g(3,3)
            g(3,3) = temp
            temp = g(1,2)
            g(1,2) = g(1,3)
            g(1,3) = temp
            g(2,1) = g(1,2)
            g(3,1) = g(1,3)
         ENDIF
      ENDIF
   ENDIF
!
!     SOLVE MU*H = B
!
   CALL invers(3,g,3,rem,1,det,ising,iwork)
   IF ( ising==2 ) THEN
      WRITE (Iout,99002) Ufm , Matid
99002 FORMAT (A23,', MATERIAL',I9,' IS SINGULAR IN REMFLX')
      CALL mesage(-61,0,0)
      GOTO 99999
   ELSE
!
!     REM NOW HAS HC- CHECK POINTER LIST TO SEE WHICH GRIDS ARE ON THE
!     SPHEROID AND ADD REMFLUX T THOSE ALREADY ACCUMULATED
!
      DO i = 1 , igrids
         IF ( ipoint(i)/=0 ) THEN
            isub = ihc + 3*(ipoint(i)-1)
            DO j = 1 , 3
               Z(isub+j) = Z(isub+j) + rem(j)
            ENDDO
         ENDIF
      ENDDO
!
!     GO BACK FOR ANOTHER ELEMEENT
!
      GOTO 500
   ENDIF
!
!     DONE WITH ALL TYPES-AVERAGE THE RESULTS BY NUMBER OF ELEMENTS AT
!     EACH
!
 700  DO i = 1 , Ngrids
      den = float(Iz(ict+i))
      IF ( den/=0. ) THEN
         isub = 3*(i-1) + ihc
         DO j = 1 , 3
            Z(isub+j) = Z(isub+j)/den
         ENDDO
      ENDIF
   ENDDO
!
!     WRITE RESULTS TO SCR1
!
 800  CALL write(scr1,Z(ihc+1),3*Ngrids,1)
!
!     GO BACK FOR ANOTHER REMFLD RECORD
!
   IF ( jcount==ncol ) THEN
!
!     DONE
!
      CALL close(scr1,1)
      mcb(1) = scr1
      mcb(2) = ncol
      mcb(3) = 3*Ngrids
      DO i = 4 , 7
         mcb(i) = 0
      ENDDO
      CALL wrttrl(mcb)
      CALL close(hest,1)
      CALL close(remfld,1)
      RETURN
   ELSE
      CALL rewind(hest)
      CALL fwdrec(*900,hest)
      GOTO 100
   ENDIF
!
 900  n = -2
   CALL mesage(n,file,nam)
   CALL mesage(-61,0,0)
   GOTO 99999
 1000 n = -3
   CALL mesage(n,file,nam)
   CALL mesage(-61,0,0)
99999 END SUBROUTINE remflx
