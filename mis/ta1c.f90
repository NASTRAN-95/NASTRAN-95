
SUBROUTINE ta1c
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bgpdt , Buf1 , Buf2 , Buf3 , Clsrew , Cstm , Ecpt , Ect , Ept , Est , Gei , Genl , Gpct , Gpect , Gptt , Idgenl , Iud ,  &
         & Iui , Iz , Luset , Mpt , Mptx , Nbpw , Nfile(6) , Nogenl , Nogo , Nosimp , Nosup , Nsil , Nud , Nui , Rd , Rdrew , Scr1 ,&
         & Scr2 , Scr3 , Scr4 , Sil , Sysbuf , Wrt , Wrtrew , Z(1)
   REAL Comps , Dum38(38) , Eptx , Pcomps
   COMMON /blank / Luset , Nosimp , Nosup , Nogenl , Genl , Comps
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew
   COMMON /setup / Nfile
   COMMON /system/ Sysbuf , Dum38 , Nbpw
   COMMON /ta1com/ Nsil , Ect , Ept , Bgpdt , Sil , Gptt , Cstm , Mpt , Est , Gei , Gpect , Ecpt , Gpct , Mptx , Pcomps , Eptx ,    &
                 & Scr1 , Scr2 , Scr3 , Scr4
   COMMON /tac1ax/ Buf1 , Buf2 , Buf3 , Iui , Nui , Iud , Nud , Iz , Nogo , Idgenl
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER buf(10) , file , flag , genel(2) , half , i , ijk , intcol , introw , j , jfile , k , kcol , koz , krow , m , n , nam(2) &
         & , nbrud , nbrui , ncore , nocore , nwds , nwdud , nwdui , nwdz , silno
   INTEGER korsz
!
! End of declarations
!
!
!     TA1C READS GENERAL ELEMENTS FROM THE ECT AND BUILDS THE GEI.
!     FOR EACH GENERAL ELEMENT, THE UI AND UD LISTS ARE CONVERTED TO
!     SIL NOS. AND SORTED ON SIL NO. THE ELEMENTS OF THE Z AND S
!     MATRICES ARE WRITTEN IN INTERNAL SORT (I.E., ROW AND COL NOS
!     CORRESPOND TO POSITION IN THE SORTED UI AND UD LISTS.
!
!
   DATA genel/4301 , 43/ , nam/4HTA1C , 4H    /
   DATA half/65536/
!
!     ADD MORE BITS TO HALF IF MACHINE WORD IS LARGER THAN 32
!
   IF ( Nbpw>=36 ) half = 4*half
   IF ( Nbpw>36 ) half = 4*half
!
!     SET BUFFER POINTERS, ETC.
!
   Buf1 = korsz(Z) - Sysbuf - 2
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Nogo = 0
   Nogenl = 0
!
!     READ THE SIL INTO CORE
!
   file = Sil
   CALL open(*700,Sil,Z(Buf1),Rdrew)
   CALL fwdrec(*800,Sil)
   CALL read(*800,*100,Sil,Z,Buf2,1,Nsil)
   CALL mesage(-8,0,nam)
 100  CALL close(Sil,Clsrew)
!
!     OPEN THE GEI. WRITE HEADER RECORD.
!
   file = Gei
   CALL open(*700,Gei,Z(Buf2),Wrtrew)
   CALL fname(Gei,buf)
   CALL write(Gei,buf,2,1)
!
!     OPEN THE ECT. READ ELEMENT ID.
!
   file = Ect
   CALL preloc(*700,Z(Buf1),Ect)
   CALL locate(*1100,Z(Buf1),genel,flag)
 200  CALL read(*800,*600,Ect,buf,1,0,flag)
   Idgenl = buf(1)
   Nogenl = Nogenl + 1
!
!     READ THE UI LIST. STORE POSITION IN UI LIST, SIL NO.,
!     INTERNAL GRID NO., AND COMPONENT CODE.
!
   Iui = Nsil + 1
   i = Iui
   j = 1
   DO
      CALL read(*800,*900,Ect,Z(i+2),2,0,flag)
      IF ( Z(i+2)==-1 ) THEN
         Nui = i - 4
         nbrui = j - 1
         nwdui = 4*nbrui
!
!     READ THE UD LIST (IF PRESENT). STORE POSITION IN UD LIST, SIL NO.,
!     INTERNAL GRID NO., AND COMPONENT CODE.
!
         Iud = i
         j = 1
         DO
            CALL read(*800,*900,Ect,Z(i+2),2,0,flag)
            IF ( Z(i+2)==-1 ) THEN
               Nud = i - 4
               nbrud = j - 1
               nwdud = 4*nbrud
               Iz = i
!
!     SORT UI AND UD LISTS ON SIL NO.
!     STORE INTERNAL POSITION IN UI AND UD LISTS.
!     WRITE ELEMENT ID, NO. OF UI-S, NO. OF UD-S.
!     WRITE SIL NOS. FOR UI LIST AND SIL NOS. FOR UD LIST.
!
               CALL sorti(0,0,4,2,Z(Iui),nwdui)
               buf(2) = nbrui
               buf(3) = nbrud
               CALL write(Gei,buf,3,0)
               k = 1
               DO i = Iui , Nui , 4
                  silno = Z(i+1)
                  Z(i+1) = k
                  CALL write(Gei,silno,1,0)
                  k = k + 1
               ENDDO
               IF ( nbrud/=0 ) THEN
                  CALL sorti(0,0,4,2,Z(Iud),nwdud)
                  k = 1
                  DO i = Iud , Nud , 4
                     silno = Z(i+1)
                     Z(i+1) = k
                     CALL write(Gei,silno,1,0)
                     k = k + 1
                  ENDDO
               ENDIF
!
!     SORT UI LIST ON EXTERNAL POSITION.
!
               CALL sorti(0,0,4,1,Z(Iui),nwdui)
!
!     DETERMINE IF CORE WILL HOLD THE FULL Z OR K MATRIX
!
               ncore = Buf2 - Iz
               nwdz = nbrui**2
               nocore = 0
               IF ( nwdz>ncore ) nocore = 1
!
!     READ INDICATOR OF INPUT OF Z OR K MATRIX
!
               CALL read(*800,*900,Ect,ijk,1,0,flag)
               CALL write(Gei,ijk,1,0)
               koz = 0
               IF ( ijk==2 ) koz = 1
!
!     READ THE ELEMENTS OF THE Z OR K MATRIX.
!     CONVERT FROM EXTERNAL ROW AND COL NOS. TO INTERNAL ROW AND COL
!     NOS.  IF CORE WILL HOLD Z OR K, STORE THE ELEMENTS IN CORE
!     OTHERWISE, WRITE CODED ROW/COL NOS AND ELEMENTS ON SCRATCH FILE.
!
               IF ( nocore/=0 ) CALL open(*700,Scr4,Z(Buf3),Wrtrew)
               DO i = Iui , Nui , 4
                  introw = Z(i+1)
                  krow = Iz + (introw-1)*nbrui - 1
                  DO j = i , Nui , 4
                     intcol = Z(j+1)
                     kcol = Iz + (intcol-1)*nbrui - 1
                     CALL read(*800,*900,Ect,buf(3),1,0,flag)
                     IF ( nocore/=0 ) THEN
                        m = 3
                        buf(1) = intcol
                        buf(2) = introw
                        IF ( introw/=intcol ) THEN
                           buf(4) = introw
                           buf(5) = intcol
                           buf(6) = buf(3)
                           m = 6
                        ENDIF
                        CALL write(Scr4,buf,m,0)
                     ELSE
                        k = krow + intcol
                        Z(k) = buf(3)
                        k = kcol + introw
                        Z(k) = buf(3)
                     ENDIF
                  ENDDO
               ENDDO
               IF ( nocore/=0 ) CALL close(Scr4,Clsrew)
!
!     IF Z OR K MATRIX IS IN CORE,WRITE IT OUT
!     OTHERWISE,SORT THE MATRIX AND THEN WRITE IT.
!
               IF ( nocore==0 ) THEN
                  CALL write(Gei,Z(Iz),nwdz,0)
                  GOTO 400
               ELSE
                  CALL open(*700,Scr4,Z(Buf3),Rdrew)
                  Nfile(1) = Scr1
                  Nfile(2) = Scr2
                  Nfile(3) = Scr3
                  CALL sorti(Scr4,0,3,2,Z(Iz),ncore-Sysbuf)
                  CALL close(Scr4,Clsrew)
                  IF ( Nfile(6)==Nfile(1) ) Nfile(1) = Scr4
                  IF ( Nfile(6)==Nfile(2) ) Nfile(2) = Scr4
                  IF ( Nfile(6)==Nfile(3) ) Nfile(3) = Scr4
                  jfile = Nfile(6)
                  CALL open(*700,jfile,Z(Buf3),Rdrew)
                  CALL sorti(jfile,0,3,-1,Z(Iz),ncore-Sysbuf)
                  CALL close(jfile,Clsrew)
                  CALL open(*700,Nfile(6),Z(Buf3),Rdrew)
                  DO
                     CALL read(*800,*300,Nfile(6),buf,3,0,flag)
                     CALL write(Gei,buf(3),1,0)
                  ENDDO
               ENDIF
            ELSE
               Z(i) = j
               k = Z(i+2)
               Z(i+1) = Z(k)
               IF ( Z(i+3)/=0 ) Z(i+1) = Z(i+1) + Z(i+3) - 1
               i = i + 4
               j = j + 1
            ENDIF
         ENDDO
      ELSE
         Z(i) = j
         k = Z(i+2)
         Z(i+1) = Z(k)
         IF ( Z(i+3)/=0 ) Z(i+1) = Z(i+1) + Z(i+3) - 1
         i = i + 4
         j = j + 1
      ENDIF
   ENDDO
 300  CALL close(Nfile(6),Clsrew)
!
!     READ FLAG WORD FOR S MATRIX.
!     IF S MATRIX NOT PRESENT, BUT UD IS PRESENT,
!     EXECUTE TA1CA TO COMPUTE AND WRITE S MATRIX.
!     IF S MATRIX AND UD BOTH NOT PRESENT, CLOSE GEI RECORD AND LOOP
!     BACK
!
 400  CALL read(*800,*900,Ect,buf,1,0,flag)
   IF ( buf(1)/=0 ) THEN
!
!     S MATRIX IS PRESENT.
!     DETERMINE IF CORE WILL HOLD THE FULL S MATRIX
!
      nwds = nbrud*nbrui
      CALL sorti(0,0,4,1,Z(Iud),nwdud)
      nocore = 0
      IF ( nwds>ncore ) nocore = 1
!
!     READ THE ELEMENTS OF THE S MATRIX.
!     CONVERT FROM EXTERNAL ROW AND COL NOS TO INTERNAL ROW AND COL NOS.
!     IF CORE WILL HOLD S, STORE THE ELEMENTS IN CORE.
!     OTHERWISE, WRITE CODED ROW/COL NOS AND ELEMENTS ON SCRATCH FILE.
!
      IF ( nocore/=0 ) CALL open(*700,Scr4,Z(Buf3),Wrtrew)
      DO i = Iui , Nui , 4
         introw = Z(i+1)
         krow = Iz + (introw-1)*nbrud - 1
         DO j = Iud , Nud , 4
            intcol = Z(j+1)
            k = krow + intcol
            CALL read(*800,*900,Ect,buf(3),1,0,flag)
            IF ( nocore/=0 ) THEN
               buf(1) = introw
               buf(2) = intcol
               CALL write(Scr4,buf,3,1)
            ELSE
               Z(k) = buf(3)
            ENDIF
         ENDDO
      ENDDO
      IF ( nocore/=0 ) CALL close(Scr4,Clsrew)
!
!     IF S MATRIX IS IN CORE, WRITE IT OUT.
!     OTHERWISE, SORT THE MATRIX AND THEN WRITE IT.
!
      IF ( nocore==0 ) THEN
         CALL write(Gei,Z(Iz),nwds,0)
      ELSE
         CALL open(*700,Scr4,Z(Buf3),Rdrew)
         Nfile(1) = Scr1
         Nfile(2) = Scr2
         Nfile(3) = Scr3
         CALL sorti(Scr4,0,3,2,Z(Iz),ncore-Sysbuf)
         CALL close(Scr4,Clsrew)
         IF ( Nfile(6)==Nfile(1) ) Nfile(1) = Scr4
         IF ( Nfile(6)==Nfile(2) ) Nfile(2) = Scr4
         IF ( Nfile(6)==Nfile(3) ) Nfile(3) = Scr4
         jfile = Nfile(6)
         CALL open(*700,jfile,Z(Buf3),Rdrew)
         CALL sorti(jfile,0,3,-1,Z(Iz),ncore-Sysbuf)
         CALL close(jfile,Clsrew)
         CALL open(*700,Nfile(6),Z(Buf3),Rdrew)
         DO
            CALL read(*800,*500,Nfile(6),buf,3,0,file)
            CALL write(Gei,buf(3),1,0)
         ENDDO
      ENDIF
   ELSE
      IF ( nbrud/=0 ) THEN
         CALL sorti(0,0,4,2,Z(Iui),nwdui)
         CALL ta1ca(koz)
      ENDIF
      CALL write(Gei,0,0,1)
      GOTO 200
   ENDIF
 500  CALL write(Gei,0,0,1)
   GOTO 200
!
!     HERE WHEN NO MORE GENERAL ELEMENTS
!
 600  CALL close(Ect,Clsrew)
   CALL close(Gei,Clsrew)
   buf(1) = Gei
   buf(2) = Nogenl
   CALL wrttrl(buf)
   IF ( Nogo/=0 ) CALL mesage(-61,0,nam)
   RETURN
!
!     FATAL ERRORS
!
 700  n = -1
   GOTO 1000
 800  n = -2
   GOTO 1000
 900  n = -3
 1000 CALL mesage(n,file,nam)
 1100 CALL mesage(-30,63,buf)
END SUBROUTINE ta1c
