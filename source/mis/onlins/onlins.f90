!*==onlins.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE onlins(Lx) !HIDESTARS (*,Lx)
!
!     ON-LINE SCAN ROUTINE, CALLED ONLY BY SCAN
!
!     WRITTEN FEBY G.CHAN/SPERRY,  FEB. 1986
!
   USE c_blank
   USE c_ifp1a
   USE c_machin
   USE c_system
   USE c_xscanx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: all , blank , debug1 , debug2 , debug3 , equal , i0 , lu , stop
   INTEGER :: bgn , end , i , i81 , ib , iscan , j , jj , jmp , jumph , kk , l , lsem , nout , nwpc1 , nz , nzz , subid
   INTEGER , DIMENSION(20) :: card
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(2) :: r
   REAL , DIMENSION(30) :: z
   EXTERNAL a82int , andf , complf , fwdrec , ifp1c , ifp1h , korsz , lshift , mesage , orf , read , rewind , rshift , xrcard ,     &
          & xread
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!            /ZZIFP1/ IS THE OPEN CORE FOR SCAN
   !>>>>EQUIVALENCE (Iz(1),Lcse(1))
   !>>>>EQUIVALENCE (Imax,Amax) , (Imin,Amin) , (Idupl,Ibeg) , (Inc,Iend) , (Card(1),Core(1)) , (Iz(1),Z(1))
   DATA blank , equal , stop , all , name/4H     , 4H=    , 4HSTOP , 4HALL  , 4HONLI , 4HNS  /
   DATA lu , debug1 , debug2 , debug3 , i0/1 , 4HDEBU , 4HG ON , 4HG OF , 0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE /IFP1A/
!
         scr1 = 301
         casecc = 101
         is = 0
         nwpc = 20
         ncpw = 4
         nmodes = 0
         icc = 0
         isub = 1
         iblnk = blank
         iequal = equal
         ieor = complf(0)
         ieor = rshift(ieor,1)
!
!     SET INTERACTIVE FLAG TO POSITIVE, A SIGNAL TO SCAN, TOTAPE, IFP1C
!
         intra = iabs(intra)
         IF ( intra==0 ) intra = 1
!
         icomp = Lx
         nwpc1 = nwpc + 1
         nout = outtap
         WRITE (nout,99001)
99001    FORMAT (///1X,'*** SCAN INTERACTIVE INPUT ***')
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ CASECC FILE AND SAVE DATA IN LCSE, ONE SUBCASE AT A TIME
!     SAVE SET DATA IN CORE BEGIN AT CORE(BGN)
!
         lcse(166) = 200
         lcse(199) = 0
         lcse(200) = 0
         nz = korsz(core(1)) - 3*ibuf - 1
         nz = min0(nz,lcore)
         iscan = 0
         nset = 0
         i81 = nwpc1
         subid = -1
         Lx = 0
         IF ( icomp==-2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NO QUESTION ASKED IF SORT2 DATA TYPE IS USED.
!
         Lx = 1
 20      WRITE (nout,99002)
99002    FORMAT (//,' ENTER SUBCASE ID (DEFAULT=FIRST SUBCASE)')
         READ (in,99003) r
99003    FORMAT (2A4)
         CALL a82int(*20,r,8,subid,i)
         IF ( subid==0 ) subid = -1
         IF ( intra>10 ) WRITE (lu,99004) subid
99004    FORMAT (///3X,'SUBCASE ID',I8)
         spag_nextblock_1 = 3
      CASE (3)
         jj = 1
         CALL rewind(casecc)
         CALL fwdrec(*80,casecc)
         DO
            jj = jj + 1
            CALL read(*80,*80,casecc,lcse(jj),1,0,i)
            IF ( subid==-1 ) subid = lcse(jj)
            IF ( lcse(jj)==subid ) THEN
               lcse(1) = lcse(jj)
               CALL read(*80,*100,casecc,lcse(2),199,0,i)
               lencc = lcse(166)
               lsem = lcse(lencc)
               nset = lcse(lencc-1)
               IF ( lsem>0 ) CALL read(*80,*100,casecc,core(i81),lsem,0,i)
               i81 = i81 + lsem
               bgn = i81
               end = i81
               DO
                  CALL read(*40,*40,casecc,core(i81),2,0,i)
                  jmp = core(i81+1)
                  core(i81+2) = jj
                  i81 = i81 + 3
                  CALL read(*80,*100,casecc,core(i81),jmp,0,i)
                  nset = nset + 1
                  i81 = i81 + jmp
               ENDDO
            ELSE
               CALL fwdrec(*80,casecc)
            ENDIF
         ENDDO
 40      SPAG_Loop_1_2: DO
!
!     SET CARD
!
            WRITE (nout,99005)
99005       FORMAT (//,' ENTER A BLANK, OR A SET CARD (SEE USER MANUAL P. ','2.3-44)',/,' E.G.  SET 101 = 1, 5 THRU 20')
            SPAG_Loop_2_1: DO
               core(i81) = ieor
               nogo = 0
               CALL xread(*40,card)
               IF ( card(1)==blank .AND. card(2)==blank ) THEN
!
!     SET DATA - FROM CORE(BGN) THRU CORE(END)
!
                  end = i81 - 1
                  nzz = nz - i81
                  EXIT SPAG_Loop_2_1
               ELSE
                  WRITE (lu,99014) card
                  IF ( card(1)/=debug1 ) THEN
                     ib = i81
                     nzz = nz - i81
                     CALL xrcard(core(i81),nzz,card(1))
                     IF ( core(i81+8)/=all ) THEN
                        icc = 1
                        CALL ifp1c(i81,nzz)
!
!     CONTINUATION CARDS FOR SET ARE READ IN BY IFP1C
!
                        IF ( nogo/=0 ) THEN
                           i81 = ib
                           CYCLE SPAG_Loop_1_2
                        ENDIF
                     ELSE
                        core(i81) = core(i81+4)
                        core(i81+1) = 1
                        core(i81+2) = jj
                        core(i81+3) = -1
                        i81 = i81 + 4
                     ENDIF
                     nset = nset + 1
                     WRITE (nout,99006) core(ib)
99006                FORMAT (/,' THIS NEW SET',I6,' IS DEFINED FOR LOCAL USE ONLY',//,' ENTER A BLANK, OR ANOTHER SET CARD')
                     kk = 55
                     IF ( debug ) WRITE (6,99013) kk , i81
                  ELSE
                     j = lshift(1,20)
                     IF ( card(2)==debug2 ) swtch1 = orf(j,swtch1)
                     j = complf(j)
                     IF ( card(2)==debug3 ) swtch1 = andf(j,swtch1)
                     debug = .FALSE.
                     IF ( card(2)==debug2 ) debug = .TRUE.
                     CYCLE SPAG_Loop_1_2
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_2_1
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
 60      SPAG_Loop_1_4: DO
!
!     SCAN CARD
!
            WRITE (nout,99007)
99007       FORMAT (//,' ENTER A BLANK, OR A SCAN CARD (SEE USER MANUAL P.2.3-41A',/,                                               &
                   &'  E.G. SCAN (STRESS,CBAR,AXIAL,SA/MAX) = 15, SET 102',/,'       SCAN (FORCE,3,ROD,2) = +2000.,-1500.',/,       &
                   &'       SCAN (HELP)')
            SPAG_Loop_2_3: DO
!
               jumph = 0
               CALL xread(*60,card)
               IF ( card(1)==stop .AND. card(2)==blank ) THEN
                  RETURN 1
               ELSEIF ( card(1)==blank .AND. card(2)==blank ) THEN
!
!     MOVE SET AND SCAN DATA TO THE END OF CASECC ARRAY IN /ZZIFP1/
!     THEN, MOVE THE ENTIRE CASECC DATA (SET AND SCAN INCLUDED) TO
!     THE END OF THE OPEN CORE. FINALLY, MOVE THE SAME DATA BLOCK
!     TO THE BEGINNING OF THE OPEN CORE SPACE IN /ZZSCAN/ FOR SCAN
!     OPERATION
!
                  l = lencc
                  IF ( i81>nwpc1 ) THEN
                     j = bgn + 2
                     i81 = i81 - 1
                     DO i = nwpc1 , i81
                        IF ( i/=j ) THEN
                           l = l + 1
                           lcse(l) = core(i)
                        ELSE
                           j = j + core(j-1) + 3
                        ENDIF
                     ENDDO
                     j = lcore
                     DO i = 1 , l
                        lcse(j) = lcse(i)
                        j = j - 1
                     ENDDO
                     IF ( i>j ) CALL mesage(+8,0,name)
                     j = lcore
                     DO i = 1 , l
                        z(i) = lcse(j)
                        j = j - 1
                     ENDDO
                     IF ( debug ) WRITE (6,99008) (z(i),i=1,l)
99008                FORMAT (//,' Z(1...200+) =',(/4X,10I7))
                  ENDIF
                  IF ( Lx>0 ) Lx = l
!
                  IF ( iscan/=20000000 ) THEN
                     IF ( z(25)==0 ) THEN
!
                        WRITE (nout,99009)
99009                   FORMAT (//,' STRESS OUTPUT FILE NOT AVAILABLE FOR SCAN',//)
                        CYCLE
                     ELSE
!
!     STRESS SCAN
!
                        z(24) = -1
                        z(25) = 1
                        z(26) = 1
                     ENDIF
                  ENDIF
                  IF ( iscan/=20000000 ) EXIT SPAG_Loop_2_3
                  IF ( z(28)==0 ) THEN
                     WRITE (nout,99010)
99010                FORMAT (//,' FORCE  OUTPUT FILE NOT AVAILABLE FOR SCAN',//)
                  ELSE
!
!     FORCE SCAN
!
                     z(27) = -1
                     z(28) = 1
                     z(29) = 1
                     EXIT SPAG_Loop_2_3
                  ENDIF
               ELSE
                  WRITE (lu,99014) card
                  ib = i81
                  CALL xrcard(core(i81),nzz,card(1))
                  CALL ifp1h(i81,nzz,jumph)
                  IF ( nogo==0 ) THEN
                     IF ( jumph==0 ) THEN
!
                        j = core(ib)
                        IF ( iscan==0 ) iscan = j
                        IF ( iscan==j ) iscan = 30000000
                        WRITE (nout,99011)
99011                   FORMAT (/,' ENTER A BLANK, OR ANOTHER SCAN CARD')
                        kk = 87
                        IF ( debug ) WRITE (6,99013) kk , i81
                        CYCLE
                     ELSE
                        CALL ifp1h(0,0,2)
                     ENDIF
                  ENDIF
                  i81 = ib
                  IF ( nogo/=0 ) CYCLE SPAG_Loop_1_4
               ENDIF
            ENDDO SPAG_Loop_2_3
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
         spag_nextblock_1 = 4
      CASE (4)
         IF ( intra>10 ) outtap = lu
         RETURN
!
 80      jj = jj - 1
         WRITE (nout,99012) subid , (z(i),i=1,jj)
99012    FORMAT (//,' SUBCASE',I5,' NOT FOUND',//,' EXISTING SUBCASES ARE -',(/5X,10I7))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 100     CALL mesage(+2,casecc,name)
         spag_nextblock_1 = 4
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99013 FORMAT ('   ONLINS/',I2,4X,'I81 =',I7)
99014 FORMAT (20A4)
END SUBROUTINE onlins
