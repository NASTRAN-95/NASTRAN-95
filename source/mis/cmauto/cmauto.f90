!*==cmauto.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmauto
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_output
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: a , b
   INTEGER , DIMENSION(2) , SAVE :: aaa
   REAL :: asej , bsej , xsej
   LOGICAL :: back , found , print
   INTEGER , DIMENSION(9) :: ce
   INTEGER :: i , ic1 , ic2 , idir , ifile , imsg , isavl , isavs , j , jj , k , kdh , kk , m1 , m2 , mm , ncsub , ni , nipi ,      &
            & nipj , nlin , nout , np2 , snk , spk , st , sts , svkk
   INTEGER , DIMENSION(2) :: ibits , jbits
   INTEGER , SAVE :: iblnk
   INTEGER , DIMENSION(12) , SAVE :: ihd
   INTEGER , DIMENSION(14) :: name
   INTEGER , DIMENSION(8) :: nsil , nwd , snext , ssil
   REAL , DIMENSION(1) :: rz
   EXTERNAL andf , bitpat , close , fwdrec , mesage , open , page , read , rshift , skpfil , sortf , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES THE AUTOMATIC CONNECTION OF
!     SUBSTRUCTURES IN THE COMB1 MODULE
!
   !>>>>EQUIVALENCE (Z(1),Rz(1))
   DATA aaa/4HCMAU , 2HTO/ , iblnk/4H    /
   DATA ihd/4H SUM , 4HMARY , 4H OF  , 4H AUT , 4HOMAT , 4HICAL , 4HLY G , 4HENER , 4HATED , 4H CON , 4HNECT , 4HIONS/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nlin = 1000
         found = .FALSE.
         print = .FALSE.
         IF ( andf(rshift(iprint,10),1)==1 ) print = .TRUE.
         np2 = 2*npsub
         DO i = 1 , np2 , 2
            j = i/2 + 1
            name(i) = combo(j,1)
            name(i+1) = combo(j,2)
         ENDDO
         DO i = 1 , 96
            ihead(i) = iblnk
         ENDDO
         j = 1
         DO i = 75 , 86
            ihead(i) = ihd(j)
            j = j + 1
         ENDDO
         isavs = score
         isavl = lcore
         ifile = scconn
         CALL open(*20,scconn,z(buf2),3)
         IF ( iauto ) THEN
!
            ifile = scsfil
            CALL open(*20,scsfil,z(buf1),0)
            ssil(1) = score
            nout = npsub + 2
            idir = isort + 1
            DO i = 1 , npsub
               sts = ssil(i)
               ncsub = combo(i,5)
               DO j = 1 , ncsub
                  CALL fwdrec(*40,scsfil)
               ENDDO
!
!     READ SIL,C FOR THE I-TH PSEUDOSTRUCTURE
!
               CALL read(*40,*5,scsfil,z(sts),lcore,1,nsil(i))
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
 5             lcore = lcore - nsil(i)
               snext(i) = score + nsil(i)
               score = score + nsil(i)
               st = snext(i)
!
!     READ BGSS FOR THE I-TH PSEUDOSTRUCTURE
!
               CALL read(*40,*10,scsfil,z(st),lcore,1,nwd(i))
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
 10            snext(i+1) = snext(i) + nwd(i)
               ssil(i+1) = snext(i) + nwd(i)
               score = score + nwd(i)
               CALL skpfil(scsfil,1)
               lcore = lcore - nwd(i)
               ni = nwd(i) + st
!
!     WRITE THE IP NUMBER OVER THE CID IN THE BGSS
!     WILL BE USED AFTER SORTING
!
               DO j = st , ni , 4
                  jj = (j-st+4)/4
                  IF ( z(j)+1/=0 ) THEN
                     z(j) = jj
                  ELSE
                     z(j) = -jj
                  ENDIF
               ENDDO
            ENDDO
!
!     SORT EACH BGSS IN THE SPECIFIED COORDINATE DIRECTION
!
            DO i = 1 , npsub
               st = snext(i)
               CALL sortf(0,0,4,idir,rz(st),nwd(i))
            ENDDO
            i = 1
         ELSE
            CALL close(scconn,1)
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         k = 0
         kk = 0
         back = .FALSE.
         svkk = 0
         ic1 = ssil(i)
         nipi = nwd(i)/4
         j = i + 1
         IF ( restct(i,j)/=1 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         ic2 = ssil(j)
         nipj = nwd(j)/4
         spag_nextblock_1 = 4
      CASE (4)
         spk = snext(i) + k + 1
         IF ( z(spk-1)<0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         a(1) = rz(spk)
         a(2) = rz(spk+1)
         a(3) = rz(spk+2)
         spag_nextblock_1 = 5
      CASE (5)
         snk = snext(j) + kk + 1
         IF ( z(snk-1)<0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         b(1) = rz(snk)
         b(2) = rz(snk+1)
         b(3) = rz(snk+2)
         IF ( a(isort)<b(isort)-toler ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( b(isort)<a(isort)-toler ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( .NOT.(back) ) THEN
            back = .TRUE.
            svkk = kk
         ENDIF
         asej = a(isort)
         bsej = b(isort)
         xsej = asej - bsej
         DO mm = 1 , 3
            IF ( mm/=isort ) THEN
               asej = a(mm)
               bsej = b(mm)
               xsej = a(mm) - b(mm)
               IF ( abs(xsej)>toler ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
!
!     GENERATE THE NEW CONNECTION ENTRY
!
         DO kdh = 1 , 9
            ce(kdh) = 0
         ENDDO
         ce(2) = 2**(i-1) + 2**(j-1)
         ce(2+i) = iabs(z(spk-1))
         ce(2+j) = iabs(z(snk-1))
         m1 = iabs(z(spk-1))
         m2 = iabs(z(snk-1))
         ce(1) = andf(z(ic1+2*m1-1),z(ic2+2*m2-1))
         found = .TRUE.
!
!     WRITE THE CONNECTION ENTRY ON SCCONN
!
         IF ( ce(1)/=0 ) CALL write(scconn,ce,nout,1)
         IF ( .NOT.print ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ce(1)==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nlin<nlpp ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         nlin = 0
         CALL page
         WRITE (outt,99001) (name(kdh),kdh=1,np2)
99001    FORMAT (/14X,22HCONNECTED   CONNECTION,29X,22HPSEUDOSTRUCTURE  NAMES,/17X,3HDOF,9X,4HCODE,3X,7(3X,2A4)//)
         nlin = nlin + 10
         spag_nextblock_1 = 7
      CASE (7)
         CALL bitpat(ce(1),ibits)
         CALL bitpat(ce(2),jbits)
         nlin = nlin + 1
         IF ( nlin>nlpp ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (outt,99002) ibits(1) , ibits(2) , jbits(1) , jbits(2) , (ce(kdh+2),kdh=1,npsub)
99002    FORMAT (16X,A4,A2,5X,A4,A3,2X,7(3X,I8))
         spag_nextblock_1 = 10
      CASE (8)
         kk = svkk
         back = .FALSE.
         spag_nextblock_1 = 9
      CASE (9)
         k = k + 4
         IF ( k/4<nipi ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = 0
         kk = 0
         svkk = 0
         back = .FALSE.
         spag_nextblock_1 = 11
      CASE (10)
         kk = kk + 4
         IF ( kk/4<nipj ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (11)
         j = j + 1
         IF ( j<=npsub ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = i + 1
         j = i
         IF ( i<npsub ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (outt,99003)
99003    FORMAT (//40X,'NOTE - GRID POINTS IN PSEUDOSTRUCTURE INTERNAL',' GRID NUMBERS')
         CALL close(scconn,1)
         CALL close(scsfil,1)
         score = isavs
         lcore = isavl
         IF ( found .OR. tdat(1) .OR. tdat(2) ) RETURN
!
         WRITE (outt,99004) ufm
99004    FORMAT (A23,' 6531, NO CONNECTIONS HAVE BEEN FOUND DURING ','AUTOMATIC CONNECTION PROCEDURE.')
         idry = -2
         RETURN
!
 20      imsg = -1
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 13
      CASE (12)
         imsg = -8
         spag_nextblock_1 = 13
      CASE (13)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmauto
