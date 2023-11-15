
SUBROUTINE cmcont
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Combo(7,5) , Iauto , Idry , Ierr , Inam(2) , Inpt , Lcore , Npsub , Outt , Scconn , Scmcon , &
         & Score , Scr1 , Scr2 , Scsfil
   REAL Buf5 , Casecc , Conset , Geom4 , Scbdat , Sctoc , Step , Toler , Z(1)
   CHARACTER*23 Ufm
   COMMON /blank / Step , Idry
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub
   COMMON /cmbfnd/ Inam , Ierr
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER aaa(2) , dof(6) , i , iclen , icomp , icor , ifile , igrid , ii(9) , ii2 , ilen(100) , imsg , io(9) , ip(6) , isave ,    &
         & istrt(100) , j , jj , mfile , ncsub , nip , nnn , nwd , ofile
   INTEGER andf , lshift , rshift
   LOGICAL odd
   EXTERNAL andf , lshift , rshift
!
! End of declarations
!
!
!     THIS ROUTINE DEFINES THE CONNECTION ENTRIES IN TERMS OF IP
!     NUMBERS.
!
   DATA aaa/4HCMCO , 4HNT  /
!
   icor = Score
   iclen = Lcore
   mfile = Scsfil
   CALL open(*200,Scsfil,Z(Buf3),0)
   ofile = Scr2
   ifile = Scr1
   nwd = 2 + Npsub
   odd = .FALSE.
!
   DO i = 1 , Npsub
      odd = .NOT.odd
      ncsub = Combo(i,5)
!
!     READ IN EQSS FOR ITH PSEUDO-STRUCTURE
!
      mfile = ifile
      CALL open(*200,ifile,Z(Buf1),0)
      mfile = ofile
      CALL open(*200,ofile,Z(Buf2),1)
!
!     MOVE TO FIRST COMPONENT EQSS
!
      DO j = 1 , ncsub
         mfile = Scsfil
         CALL read(*300,*20,Scsfil,Z(Score),Lcore,1,nnn)
         GOTO 400
 20      istrt(j) = Score
         ilen(j) = nnn
         Score = Score + nnn
         Lcore = Lcore - nnn
      ENDDO
      CALL skpfil(Scsfil,1)
!
!     CONNECTION ENTRIES IN TERMS OF GRID POINT ID ARE ON SCR1
!     IN THE FORM...
!        C/CC/G1/G2/G3/G4/G5/G6/G7
!
!     READ CONNECTION ENTRY..
!
      mfile = ifile
 50   CALL read(*150,*100,ifile,ii,10,1,nnn)
 100  icomp = ii(2+i)/1000000
      igrid = ii(2+i) - 1000000*icomp
      IF ( igrid==0 ) THEN
         CALL write(ofile,ii,nwd,1)
      ELSE
!
!     THE ABOVE RETRIEVED THE ORIGINAL GRID PT. NO., NOW FIND OUT
!     IF IT HAS SEVERAL IP NO.
!
         IF ( ilen(icomp)/=0 ) THEN
            CALL gridip(igrid,istrt(icomp),ilen(icomp),ip,dof,nip,Z,nnn)
            IF ( Ierr/=1 ) THEN
               DO j = 1 , nip
                  ii2 = rshift(dof(j),26)
                  ii2 = lshift(ii2,26)
                  dof(j) = dof(j) - ii2
                  io(1) = andf(ii(1),dof(j))
                  IF ( io(1)/=0 ) THEN
                     io(2) = ii(2)
                     DO jj = 1 , nwd
                        io(2+jj) = ii(2+jj)
                     ENDDO
                     io(2+i) = ip(j)
                     CALL write(ofile,io,nwd,1)
                  ENDIF
               ENDDO
               GOTO 50
            ENDIF
         ENDIF
         Idry = -2
         WRITE (Outt,99001) Ufm , igrid , Combo(i,1) , Combo(i,2) , icomp
99001    FORMAT (A23,' 6535, A MANUAL CONNECTION SPECIFIES GRID ID ',I8,' OF PSEUDOSTRUCTURE ',2A4,/30X,                            &
                &'COMPONENT STRUCTURE,I4,22H WHICH DOES NOT EXIST.')
      ENDIF
      GOTO 50
 150  CALL close(ifile,1)
      IF ( i==Npsub ) CALL close(ofile,2)
      IF ( i<Npsub ) CALL close(ofile,1)
      isave = ifile
      ifile = ofile
      ofile = isave
   ENDDO
   Scconn = Scr1
   IF ( odd ) Scconn = Scr2
   IF ( Scconn==Scr1 ) Scr1 = 305
   IF ( Scconn==Scr2 ) Scr2 = 305
   Score = icor
   Lcore = iclen
   CALL close(Scsfil,1)
   RETURN
!
 200  imsg = -1
   GOTO 500
 300  imsg = -2
   GOTO 500
 400  imsg = -8
 500  CALL mesage(imsg,mfile,aaa)
END SUBROUTINE cmcont
