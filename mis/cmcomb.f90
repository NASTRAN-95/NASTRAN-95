
SUBROUTINE cmcomb(Nps,Nent,Ndof,Ic)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Buf2 , Buf3 , Idry , Istep , Junk(2) , Junk1(2) , Lcore , Scconn , Scmcon , Score , Scr2 , Z(1)
   REAL Scr1
   COMMON /blank / Istep , Idry
   COMMON /cmb001/ Scr1 , Scr2 , Junk , Scconn , Scmcon
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Junk1 , Score , Lcore
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ndof , Nent , Nps
   INTEGER Ic(Nent,Nps,Ndof)
!
! Local variable declarations
!
   INTEGER aaa(2) , ce(9) , ceid , comset , i , ibeg , icomp , ient , iersub , iertab(2000) , ifile , ifin , iinc , imsg , io(10) , &
         & irow , isub , itomny , j , jj , k , kk , krow(6) , list(32) , nce , ncomp , nersub , nloop , nmcon , nnn , npss , nrec , &
         & nwd , nword , saconn , savce
   LOGICAL match
   INTEGER orf
   EXTERNAL orf
!
! End of declarations
!
!
!     THIS SUBROUTINE COMBINES CONNECTION ENTRIES THAT HAVE BEEN SPECIFI
!     ON SEVERAL CONCT OR CONCT1 CARDS.
!
   DATA aaa/4HCMCO , 4HMB  /
!
!     CE IS THE CONNECTION ENTRY
!     KROW(I) IS THE NO. OF ROWS IN THE ITH DOF MATRIX
!
   iersub = 0
   itomny = 0
   ifile = Scconn
   CALL open(*1000,Scconn,Z(Buf1),0)
   ifile = Scmcon
   CALL open(*1000,Scmcon,Z(Buf2),0)
   nrec = -1
   npss = Nps - 1
   nword = Nps + 1
   ient = 0
   DO i = 1 , 6
      krow(i) = 0
   ENDDO
   savce = 0
 100  CALL read(*1100,*400,Scmcon,ceid,1,0,nnn)
   nrec = ceid - savce - 1
   savce = ceid
!
!     GO FIND ENTRY NO. CEID
!
   ifile = Scconn
   IF ( nrec/=0 ) THEN
      DO i = 1 , nrec
         CALL fwdrec(*1200,Scconn)
      ENDDO
   ENDIF
!
!     READ IN CONNECTION ENTRY
!
   CALL read(*1100,*200,Scconn,ce,10,1,nnn)
!
!     FIND WHICH DOF ARE PRESENT IN CONNECTION ENTRY
!
 200  CALL decode(ce(1),list,ncomp)
   DO i = 1 , ncomp
      icomp = list(i) + 1
      IF ( krow(icomp)==0 ) THEN
         nloop = 0
      ELSE
!
!     FIND FIRST NON-ZERO ENTRY IN CURRENT CE
!
         DO j = 1 , npss
            IF ( ce(j+2)/=0 ) THEN
               isub = j
               EXIT
            ENDIF
         ENDDO
!
!     NOW HAVE FOUND FIRST NON-ZERO, SEARCH FOR POSSIBLE
!     MATCHING ENTRIES IN MATRIX
!
         nloop = krow(icomp)
         DO j = 1 , nloop
            match = .FALSE.
            nersub = 0
            DO jj = isub , npss
               IF ( Ic(j,jj,icomp)/=0 .AND. ce(jj+2)/=0 ) THEN
                  IF ( Ic(j,jj,icomp)/=ce(jj+2) ) THEN
                     IF ( iersub+nersub>2000 ) itomny = 1
                     IF ( iersub+nersub<=2000 ) THEN
                        iertab(iersub+nersub+1) = icomp
                        iertab(iersub+nersub+2) = jj
                        iertab(iersub+nersub+3) = Ic(j,jj,icomp)
                        iertab(iersub+nersub+4) = ce(jj+2)
                        nersub = nersub + 4
                     ENDIF
                  ELSE
                     match = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
            IF ( match ) iersub = iersub + nersub
            IF ( match ) THEN
               DO jj = isub , npss
                  IF ( ce(jj+2)==0 .OR. Ic(j,jj,icomp)==0 ) Ic(j,jj,icomp) = Ic(j,jj,icomp) + ce(jj+2)
               ENDDO
               Ic(j,npss+1,icomp) = orf(Ic(j,npss+1,icomp),ce(2))
               GOTO 300
            ENDIF
         ENDDO
      ENDIF
      DO jj = 1 , npss
         Ic(nloop+1,jj,icomp) = ce(jj+2)
      ENDDO
      Ic(nloop+1,npss+1,icomp) = ce(2)
      krow(icomp) = krow(icomp) + 1
 300  ENDDO
   GOTO 100
 400  IF ( iersub==0 ) THEN
!
      CALL close(Scconn,1)
      ifile = Scr2
      CALL open(*1000,Scr2,Z(Buf3),1)
      DO k = 1 , Ndof
         irow = krow(k)
         IF ( irow>0 ) THEN
            DO i = 1 , irow
               io(1) = k
               io(2) = Ic(i,Nps,k)
               DO j = 1 , npss
                  io(j+2) = Ic(i,j,k)
               ENDDO
               CALL write(Scr2,io(1),Nps+1,0)
            ENDDO
         ENDIF
      ENDDO
      CALL write(Scr2,io(1),0,1)
      CALL close(Scr2,1)
      CALL open(*1000,Scr2,Z(Buf3),0)
      CALL read(*1100,*500,Scr2,Z(Score),Lcore,1,nwd)
      imsg = -8
      CALL mesage(imsg,ifile,aaa)
      GOTO 99999
   ELSE
!
!     GENERATE ERROR TABLE AND TERMINATE
!
      CALL close(Scconn,1)
      CALL close(Scmcon,1)
      CALL cmtrce(iertab,iersub,itomny)
      Idry = -2
      RETURN
   ENDIF
 500  CALL sort(0,0,Nps+1,2,Z(Score),nwd)
   CALL close(Scr2,1)
   CALL open(*1000,Scr2,Z(Buf3),1)
   ifin = Score + nwd - 1
   iinc = Nps + 1
   DO i = Score , ifin , iinc
      IF ( Z(i)/=0 ) THEN
         comset = Z(i)
         ibeg = i + iinc
         DO j = ibeg , ifin , iinc
            IF ( Z(j)/=0 ) THEN
               IF ( Z(j+1)>Z(i+1) ) EXIT
               DO k = 1 , npss
                  IF ( Z(i+k+1)/=Z(j+k+1) ) GOTO 520
               ENDDO
               comset = 10*comset + Z(j)
               Z(j) = 0
            ENDIF
 520     ENDDO
         CALL encode(comset)
         io(1) = comset
         DO kk = 1 , Nps
            io(1+kk) = Z(i+kk)
         ENDDO
         CALL write(Scr2,io,Nps+1,1)
      ENDIF
   ENDDO
   CALL rewind(Scmcon)
   ifile = Scmcon
   CALL read(*1100,*600,Scmcon,Z(Score),Lcore,1,nmcon)
 600  nce = 0
   saconn = Scconn
   CALL open(*1000,Scconn,Z(Buf1),0)
 700  CALL read(*900,*800,Scconn,ce,10,1,nnn)
 800  nce = nce + 1
   DO i = 1 , nmcon
      IF ( nce==Z(Score+i-1) ) GOTO 700
   ENDDO
   CALL write(Scr2,ce,Nps+1,1)
   GOTO 700
 900  CALL close(Scmcon,1)
   CALL close(Scconn,1)
   CALL close(Scr2,1)
   Scconn = Scr2
   Scr2 = saconn
   RETURN
!
 1000 imsg = -1
   CALL mesage(imsg,ifile,aaa)
   GOTO 99999
 1100 imsg = -2
   CALL mesage(imsg,ifile,aaa)
   GOTO 99999
 1200 imsg = -3
   CALL mesage(imsg,ifile,aaa)
99999 END SUBROUTINE cmcomb
