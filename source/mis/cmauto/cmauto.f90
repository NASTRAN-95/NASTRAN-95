!*==cmauto.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cmauto
   IMPLICIT NONE
   USE C_BLANK
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
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
         IF ( andf(rshift(Iprint,10),1)==1 ) print = .TRUE.
         np2 = 2*Npsub
         DO i = 1 , np2 , 2
            j = i/2 + 1
            name(i) = Combo(j,1)
            name(i+1) = Combo(j,2)
         ENDDO
         DO i = 1 , 96
            Ihead(i) = iblnk
         ENDDO
         j = 1
         DO i = 75 , 86
            Ihead(i) = ihd(j)
            j = j + 1
         ENDDO
         isavs = Score
         isavl = Lcore
         ifile = Scconn
         CALL open(*20,Scconn,Z(Buf2),3)
         IF ( Iauto ) THEN
!
            ifile = Scsfil
            CALL open(*20,Scsfil,Z(Buf1),0)
            ssil(1) = Score
            nout = Npsub + 2
            idir = Isort + 1
            DO i = 1 , Npsub
               sts = ssil(i)
               ncsub = Combo(i,5)
               DO j = 1 , ncsub
                  CALL fwdrec(*40,Scsfil)
               ENDDO
!
!     READ SIL,C FOR THE I-TH PSEUDOSTRUCTURE
!
               CALL read(*40,*5,Scsfil,Z(sts),Lcore,1,nsil(i))
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
 5             Lcore = Lcore - nsil(i)
               snext(i) = Score + nsil(i)
               Score = Score + nsil(i)
               st = snext(i)
!
!     READ BGSS FOR THE I-TH PSEUDOSTRUCTURE
!
               CALL read(*40,*10,Scsfil,Z(st),Lcore,1,nwd(i))
               spag_nextblock_1 = 12
               CYCLE SPAG_DispatchLoop_1
 10            snext(i+1) = snext(i) + nwd(i)
               ssil(i+1) = snext(i) + nwd(i)
               Score = Score + nwd(i)
               CALL skpfil(Scsfil,1)
               Lcore = Lcore - nwd(i)
               ni = nwd(i) + st
!
!     WRITE THE IP NUMBER OVER THE CID IN THE BGSS
!     WILL BE USED AFTER SORTING
!
               DO j = st , ni , 4
                  jj = (j-st+4)/4
                  IF ( Z(j)+1/=0 ) THEN
                     Z(j) = jj
                  ELSE
                     Z(j) = -jj
                  ENDIF
               ENDDO
            ENDDO
!
!     SORT EACH BGSS IN THE SPECIFIED COORDINATE DIRECTION
!
            DO i = 1 , Npsub
               st = snext(i)
               CALL sortf(0,0,4,idir,rz(st),nwd(i))
            ENDDO
            i = 1
         ELSE
            CALL close(Scconn,1)
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
         IF ( Restct(i,j)/=1 ) THEN
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
         IF ( Z(spk-1)<0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         a(1) = rz(spk)
         a(2) = rz(spk+1)
         a(3) = rz(spk+2)
         spag_nextblock_1 = 5
      CASE (5)
         snk = snext(j) + kk + 1
         IF ( Z(snk-1)<0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         b(1) = rz(snk)
         b(2) = rz(snk+1)
         b(3) = rz(snk+2)
         IF ( a(Isort)<b(Isort)-Toler ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( b(Isort)<a(Isort)-Toler ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( .NOT.(back) ) THEN
            back = .TRUE.
            svkk = kk
         ENDIF
         asej = a(Isort)
         bsej = b(Isort)
         xsej = asej - bsej
         DO mm = 1 , 3
            IF ( mm/=Isort ) THEN
               asej = a(mm)
               bsej = b(mm)
               xsej = a(mm) - b(mm)
               IF ( abs(xsej)>Toler ) THEN
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
         ce(2+i) = iabs(Z(spk-1))
         ce(2+j) = iabs(Z(snk-1))
         m1 = iabs(Z(spk-1))
         m2 = iabs(Z(snk-1))
         ce(1) = andf(Z(ic1+2*m1-1),Z(ic2+2*m2-1))
         found = .TRUE.
!
!     WRITE THE CONNECTION ENTRY ON SCCONN
!
         IF ( ce(1)/=0 ) CALL write(Scconn,ce,nout,1)
         IF ( .NOT.print ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ce(1)==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nlin<Nlpp ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         nlin = 0
         CALL page
         WRITE (Outt,99001) (name(kdh),kdh=1,np2)
99001    FORMAT (/14X,22HCONNECTED   CONNECTION,29X,22HPSEUDOSTRUCTURE  NAMES,/17X,3HDOF,9X,4HCODE,3X,7(3X,2A4)//)
         nlin = nlin + 10
         spag_nextblock_1 = 7
      CASE (7)
         CALL bitpat(ce(1),ibits)
         CALL bitpat(ce(2),jbits)
         nlin = nlin + 1
         IF ( nlin>Nlpp ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (Outt,99002) ibits(1) , ibits(2) , jbits(1) , jbits(2) , (ce(kdh+2),kdh=1,Npsub)
99002    FORMAT (16X,A4,A2,5X,A4,A3,2X,7(3X,I8))
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
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
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         kk = kk + 4
         IF ( kk/4<nipj ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
         j = j + 1
         IF ( j<=Npsub ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = i + 1
         j = i
         IF ( i<Npsub ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         WRITE (Outt,99003)
99003    FORMAT (//40X,'NOTE - GRID POINTS IN PSEUDOSTRUCTURE INTERNAL',' GRID NUMBERS')
         CALL close(Scconn,1)
         CALL close(Scsfil,1)
         Score = isavs
         Lcore = isavl
         IF ( found .OR. Tdat(1) .OR. Tdat(2) ) RETURN
!
         WRITE (Outt,99004) Ufm
99004    FORMAT (A23,' 6531, NO CONNECTIONS HAVE BEEN FOUND DURING ','AUTOMATIC CONNECTION PROCEDURE.')
         Idry = -2
         RETURN
!
 20      imsg = -1
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
 40      imsg = -2
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (12)
         imsg = -8
         spag_nextblock_1 = 13
      CASE (13)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cmauto
