!*==cmcomb.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmcomb(Nps,Nent,Ndof,Ic)
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nps
   INTEGER :: Nent
   INTEGER :: Ndof
   INTEGER , DIMENSION(Nent,Nps,Ndof) :: Ic
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER , DIMENSION(9) :: ce
   INTEGER :: ceid , comset , i , ibeg , icomp , ient , iersub , ifile , ifin , iinc , imsg , irow , isub , itomny , j , jj , k ,   &
            & kk , nce , ncomp , nersub , nloop , nmcon , nnn , npss , nrec , nwd , nword , saconn , savce
   INTEGER , DIMENSION(2000) :: iertab
   INTEGER , DIMENSION(10) :: io
   INTEGER , DIMENSION(6) :: krow
   INTEGER , DIMENSION(32) :: list
   LOGICAL :: match
   EXTERNAL close , cmtrce , decode , encode , fwdrec , mesage , open , orf , read , rewind , sort , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE COMBINES CONNECTION ENTRIES THAT HAVE BEEN SPECIFI
!     ON SEVERAL CONCT OR CONCT1 CARDS.
!
   DATA aaa/4HCMCO , 4HMB  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     CE IS THE CONNECTION ENTRY
!     KROW(I) IS THE NO. OF ROWS IN THE ITH DOF MATRIX
!
         iersub = 0
         itomny = 0
         ifile = scconn
         CALL open(*140,scconn,z(buf1),0)
         ifile = scmcon
         CALL open(*140,scmcon,z(buf2),0)
         nrec = -1
         npss = Nps - 1
         nword = Nps + 1
         ient = 0
         DO i = 1 , 6
            krow(i) = 0
         ENDDO
         savce = 0
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*160,*40,scmcon,ceid,1,0,nnn)
         nrec = ceid - savce - 1
         savce = ceid
!
!     GO FIND ENTRY NO. CEID
!
         ifile = scconn
         IF ( nrec/=0 ) THEN
            DO i = 1 , nrec
               CALL fwdrec(*180,scconn)
            ENDDO
         ENDIF
!
!     READ IN CONNECTION ENTRY
!
         CALL read(*160,*20,scconn,ce,10,1,nnn)
!
!     FIND WHICH DOF ARE PRESENT IN CONNECTION ENTRY
!
 20      CALL decode(ce(1),list,ncomp)
         SPAG_Loop_1_2: DO i = 1 , ncomp
            icomp = list(i) + 1
            IF ( krow(icomp)==0 ) THEN
               nloop = 0
            ELSE
!
!     FIND FIRST NON-ZERO ENTRY IN CURRENT CE
!
               SPAG_Loop_2_1: DO j = 1 , npss
                  IF ( ce(j+2)/=0 ) THEN
                     isub = j
                     EXIT SPAG_Loop_2_1
                  ENDIF
               ENDDO SPAG_Loop_2_1
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
                     CYCLE SPAG_Loop_1_2
                  ENDIF
               ENDDO
            ENDIF
            DO jj = 1 , npss
               Ic(nloop+1,jj,icomp) = ce(jj+2)
            ENDDO
            Ic(nloop+1,npss+1,icomp) = ce(2)
            krow(icomp) = krow(icomp) + 1
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      IF ( iersub==0 ) THEN
!
            CALL close(scconn,1)
            ifile = scr2
            CALL open(*140,scr2,z(buf3),1)
            DO k = 1 , Ndof
               irow = krow(k)
               IF ( irow>0 ) THEN
                  DO i = 1 , irow
                     io(1) = k
                     io(2) = Ic(i,Nps,k)
                     DO j = 1 , npss
                        io(j+2) = Ic(i,j,k)
                     ENDDO
                     CALL write(scr2,io(1),Nps+1,0)
                  ENDDO
               ENDIF
            ENDDO
            CALL write(scr2,io(1),0,1)
            CALL close(scr2,1)
            CALL open(*140,scr2,z(buf3),0)
            CALL read(*160,*60,scr2,z(score),lcore,1,nwd)
            imsg = -8
            CALL mesage(imsg,ifile,aaa)
            RETURN
         ELSE
!
!     GENERATE ERROR TABLE AND TERMINATE
!
            CALL close(scconn,1)
            CALL close(scmcon,1)
            CALL cmtrce(iertab,iersub,itomny)
            idry = -2
            RETURN
         ENDIF
 60      CALL sort(0,0,Nps+1,2,z(score),nwd)
         CALL close(scr2,1)
         CALL open(*140,scr2,z(buf3),1)
         ifin = score + nwd - 1
         iinc = Nps + 1
         DO i = score , ifin , iinc
            IF ( z(i)/=0 ) THEN
               comset = z(i)
               ibeg = i + iinc
               SPAG_Loop_2_3: DO j = ibeg , ifin , iinc
                  IF ( z(j)/=0 ) THEN
                     IF ( z(j+1)>z(i+1) ) EXIT SPAG_Loop_2_3
                     DO k = 1 , npss
                        IF ( z(i+k+1)/=z(j+k+1) ) CYCLE SPAG_Loop_2_3
                     ENDDO
                     comset = 10*comset + z(j)
                     z(j) = 0
                  ENDIF
               ENDDO SPAG_Loop_2_3
               CALL encode(comset)
               io(1) = comset
               DO kk = 1 , Nps
                  io(1+kk) = z(i+kk)
               ENDDO
               CALL write(scr2,io,Nps+1,1)
            ENDIF
         ENDDO
         CALL rewind(scmcon)
         ifile = scmcon
         CALL read(*160,*80,scmcon,z(score),lcore,1,nmcon)
 80      nce = 0
         saconn = scconn
         CALL open(*140,scconn,z(buf1),0)
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*120,*100,scconn,ce,10,1,nnn)
 100     nce = nce + 1
         DO i = 1 , nmcon
            IF ( nce==z(score+i-1) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         CALL write(scr2,ce,Nps+1,1)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 120     CALL close(scmcon,1)
         CALL close(scconn,1)
         CALL close(scr2,1)
         scconn = scr2
         scr2 = saconn
         RETURN
!
 140     imsg = -1
         CALL mesage(imsg,ifile,aaa)
         RETURN
 160     imsg = -2
         CALL mesage(imsg,ifile,aaa)
         RETURN
 180     imsg = -3
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmcomb
