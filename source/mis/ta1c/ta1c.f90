!*==ta1c.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ta1c
   USE c_blank
   USE c_names
   USE c_setup
   USE c_system
   USE c_ta1com
   USE c_tac1ax
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(10) :: buf
   INTEGER :: file , flag , i , ijk , intcol , introw , j , jfile , k , kcol , koz , krow , m , n , nbrud , nbrui , ncore , nocore ,&
            & nwds , nwdud , nwdui , nwdz , silno
   INTEGER , DIMENSION(2) , SAVE :: genel , nam
   INTEGER , SAVE :: half
   EXTERNAL close , fname , fwdrec , korsz , locate , mesage , open , preloc , read , sorti , ta1ca , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ADD MORE BITS TO HALF IF MACHINE WORD IS LARGER THAN 32
!
         IF ( nbpw>=36 ) half = 4*half
         IF ( nbpw>36 ) half = 4*half
!
!     SET BUFFER POINTERS, ETC.
!
         buf1 = korsz(z) - sysbuf - 2
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         nogo = 0
         nogenl = 0
!
!     READ THE SIL INTO CORE
!
         file = sil
         CALL open(*100,sil,z(buf1),rdrew)
         CALL fwdrec(*120,sil)
         CALL read(*120,*20,sil,z,buf2,1,nsil)
         CALL mesage(-8,0,nam)
 20      CALL close(sil,clsrew)
!
!     OPEN THE GEI. WRITE HEADER RECORD.
!
         file = gei
         CALL open(*100,gei,z(buf2),wrtrew)
         CALL fname(gei,buf)
         CALL write(gei,buf,2,1)
!
!     OPEN THE ECT. READ ELEMENT ID.
!
         file = ect
         CALL preloc(*100,z(buf1),ect)
         CALL locate(*160,z(buf1),genel,flag)
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*120,*80,ect,buf,1,0,flag)
         idgenl = buf(1)
         nogenl = nogenl + 1
!
!     READ THE UI LIST. STORE POSITION IN UI LIST, SIL NO.,
!     INTERNAL GRID NO., AND COMPONENT CODE.
!
         iui = nsil + 1
         i = iui
         j = 1
         DO
            CALL read(*120,*140,ect,z(i+2),2,0,flag)
            IF ( z(i+2)==-1 ) THEN
               nui = i - 4
               nbrui = j - 1
               nwdui = 4*nbrui
!
!     READ THE UD LIST (IF PRESENT). STORE POSITION IN UD LIST, SIL NO.,
!     INTERNAL GRID NO., AND COMPONENT CODE.
!
               iud = i
               j = 1
               DO
                  CALL read(*120,*140,ect,z(i+2),2,0,flag)
                  IF ( z(i+2)==-1 ) THEN
                     nud = i - 4
                     nbrud = j - 1
                     nwdud = 4*nbrud
                     iz = i
!
!     SORT UI AND UD LISTS ON SIL NO.
!     STORE INTERNAL POSITION IN UI AND UD LISTS.
!     WRITE ELEMENT ID, NO. OF UI-S, NO. OF UD-S.
!     WRITE SIL NOS. FOR UI LIST AND SIL NOS. FOR UD LIST.
!
                     CALL sorti(0,0,4,2,z(iui),nwdui)
                     buf(2) = nbrui
                     buf(3) = nbrud
                     CALL write(gei,buf,3,0)
                     k = 1
                     DO i = iui , nui , 4
                        silno = z(i+1)
                        z(i+1) = k
                        CALL write(gei,silno,1,0)
                        k = k + 1
                     ENDDO
                     IF ( nbrud/=0 ) THEN
                        CALL sorti(0,0,4,2,z(iud),nwdud)
                        k = 1
                        DO i = iud , nud , 4
                           silno = z(i+1)
                           z(i+1) = k
                           CALL write(gei,silno,1,0)
                           k = k + 1
                        ENDDO
                     ENDIF
!
!     SORT UI LIST ON EXTERNAL POSITION.
!
                     CALL sorti(0,0,4,1,z(iui),nwdui)
!
!     DETERMINE IF CORE WILL HOLD THE FULL Z OR K MATRIX
!
                     ncore = buf2 - iz
                     nwdz = nbrui**2
                     nocore = 0
                     IF ( nwdz>ncore ) nocore = 1
!
!     READ INDICATOR OF INPUT OF Z OR K MATRIX
!
                     CALL read(*120,*140,ect,ijk,1,0,flag)
                     CALL write(gei,ijk,1,0)
                     koz = 0
                     IF ( ijk==2 ) koz = 1
!
!     READ THE ELEMENTS OF THE Z OR K MATRIX.
!     CONVERT FROM EXTERNAL ROW AND COL NOS. TO INTERNAL ROW AND COL
!     NOS.  IF CORE WILL HOLD Z OR K, STORE THE ELEMENTS IN CORE
!     OTHERWISE, WRITE CODED ROW/COL NOS AND ELEMENTS ON SCRATCH FILE.
!
                     IF ( nocore/=0 ) CALL open(*100,scr4,z(buf3),wrtrew)
                     DO i = iui , nui , 4
                        introw = z(i+1)
                        krow = iz + (introw-1)*nbrui - 1
                        DO j = i , nui , 4
                           intcol = z(j+1)
                           kcol = iz + (intcol-1)*nbrui - 1
                           CALL read(*120,*140,ect,buf(3),1,0,flag)
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
                              CALL write(scr4,buf,m,0)
                           ELSE
                              k = krow + intcol
                              z(k) = buf(3)
                              k = kcol + introw
                              z(k) = buf(3)
                           ENDIF
                        ENDDO
                     ENDDO
                     IF ( nocore/=0 ) CALL close(scr4,clsrew)
!
!     IF Z OR K MATRIX IS IN CORE,WRITE IT OUT
!     OTHERWISE,SORT THE MATRIX AND THEN WRITE IT.
!
                     IF ( nocore==0 ) THEN
                        CALL write(gei,z(iz),nwdz,0)
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        CALL open(*100,scr4,z(buf3),rdrew)
                        nfile(1) = scr1
                        nfile(2) = scr2
                        nfile(3) = scr3
                        CALL sorti(scr4,0,3,2,z(iz),ncore-sysbuf)
                        CALL close(scr4,clsrew)
                        IF ( nfile(6)==nfile(1) ) nfile(1) = scr4
                        IF ( nfile(6)==nfile(2) ) nfile(2) = scr4
                        IF ( nfile(6)==nfile(3) ) nfile(3) = scr4
                        jfile = nfile(6)
                        CALL open(*100,jfile,z(buf3),rdrew)
                        CALL sorti(jfile,0,3,-1,z(iz),ncore-sysbuf)
                        CALL close(jfile,clsrew)
                        CALL open(*100,nfile(6),z(buf3),rdrew)
                        DO
                           CALL read(*120,*40,nfile(6),buf,3,0,flag)
                           CALL write(gei,buf(3),1,0)
                        ENDDO
                     ENDIF
                  ELSE
                     z(i) = j
                     k = z(i+2)
                     z(i+1) = z(k)
                     IF ( z(i+3)/=0 ) z(i+1) = z(i+1) + z(i+3) - 1
                     i = i + 4
                     j = j + 1
                  ENDIF
               ENDDO
            ELSE
               z(i) = j
               k = z(i+2)
               z(i+1) = z(k)
               IF ( z(i+3)/=0 ) z(i+1) = z(i+1) + z(i+3) - 1
               i = i + 4
               j = j + 1
            ENDIF
         ENDDO
 40      CALL close(nfile(6),clsrew)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ FLAG WORD FOR S MATRIX.
!     IF S MATRIX NOT PRESENT, BUT UD IS PRESENT,
!     EXECUTE TA1CA TO COMPUTE AND WRITE S MATRIX.
!     IF S MATRIX AND UD BOTH NOT PRESENT, CLOSE GEI RECORD AND LOOP
!     BACK
!
         CALL read(*120,*140,ect,buf,1,0,flag)
         IF ( buf(1)/=0 ) THEN
!
!     S MATRIX IS PRESENT.
!     DETERMINE IF CORE WILL HOLD THE FULL S MATRIX
!
            nwds = nbrud*nbrui
            CALL sorti(0,0,4,1,z(iud),nwdud)
            nocore = 0
            IF ( nwds>ncore ) nocore = 1
!
!     READ THE ELEMENTS OF THE S MATRIX.
!     CONVERT FROM EXTERNAL ROW AND COL NOS TO INTERNAL ROW AND COL NOS.
!     IF CORE WILL HOLD S, STORE THE ELEMENTS IN CORE.
!     OTHERWISE, WRITE CODED ROW/COL NOS AND ELEMENTS ON SCRATCH FILE.
!
            IF ( nocore/=0 ) CALL open(*100,scr4,z(buf3),wrtrew)
            DO i = iui , nui , 4
               introw = z(i+1)
               krow = iz + (introw-1)*nbrud - 1
               DO j = iud , nud , 4
                  intcol = z(j+1)
                  k = krow + intcol
                  CALL read(*120,*140,ect,buf(3),1,0,flag)
                  IF ( nocore/=0 ) THEN
                     buf(1) = introw
                     buf(2) = intcol
                     CALL write(scr4,buf,3,1)
                  ELSE
                     z(k) = buf(3)
                  ENDIF
               ENDDO
            ENDDO
            IF ( nocore/=0 ) CALL close(scr4,clsrew)
!
!     IF S MATRIX IS IN CORE, WRITE IT OUT.
!     OTHERWISE, SORT THE MATRIX AND THEN WRITE IT.
!
            IF ( nocore==0 ) THEN
               CALL write(gei,z(iz),nwds,0)
            ELSE
               CALL open(*100,scr4,z(buf3),rdrew)
               nfile(1) = scr1
               nfile(2) = scr2
               nfile(3) = scr3
               CALL sorti(scr4,0,3,2,z(iz),ncore-sysbuf)
               CALL close(scr4,clsrew)
               IF ( nfile(6)==nfile(1) ) nfile(1) = scr4
               IF ( nfile(6)==nfile(2) ) nfile(2) = scr4
               IF ( nfile(6)==nfile(3) ) nfile(3) = scr4
               jfile = nfile(6)
               CALL open(*100,jfile,z(buf3),rdrew)
               CALL sorti(jfile,0,3,-1,z(iz),ncore-sysbuf)
               CALL close(jfile,clsrew)
               CALL open(*100,nfile(6),z(buf3),rdrew)
               DO
                  CALL read(*120,*60,nfile(6),buf,3,0,file)
                  CALL write(gei,buf(3),1,0)
               ENDDO
            ENDIF
         ELSE
            IF ( nbrud/=0 ) THEN
               CALL sorti(0,0,4,2,z(iui),nwdui)
               CALL ta1ca(koz)
            ENDIF
            CALL write(gei,0,0,1)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      CALL write(gei,0,0,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     HERE WHEN NO MORE GENERAL ELEMENTS
!
 80      CALL close(ect,clsrew)
         CALL close(gei,clsrew)
         buf(1) = gei
         buf(2) = nogenl
         CALL wrttrl(buf)
         IF ( nogo/=0 ) CALL mesage(-61,0,nam)
         RETURN
!
!     FATAL ERRORS
!
 100     n = -1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 120     n = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 140     n = -3
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(n,file,nam)
 160     CALL mesage(-30,63,buf)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ta1c
