!*==onlins.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE onlins(Lx) !HIDESTARS (*,Lx)
!
!     ON-LINE SCAN ROUTINE, CALLED ONLY BY SCAN
!
!     WRITTEN FEBY G.CHAN/SPERRY,  FEB. 1986
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_IFP1A
   USE C_MACHIN
   USE C_SYSTEM
   USE C_XSCANX
   USE C_ZZZZZZ
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
         Scr1 = 301
         Casecc = 101
         Is = 0
         Nwpc = 20
         Ncpw = 4
         Nmodes = 0
         Icc = 0
         Isub = 1
         Iblnk = blank
         Iequal = equal
         Ieor = complf(0)
         Ieor = rshift(Ieor,1)
!
!     SET INTERACTIVE FLAG TO POSITIVE, A SIGNAL TO SCAN, TOTAPE, IFP1C
!
         Intra = iabs(Intra)
         IF ( Intra==0 ) Intra = 1
!
         Icomp = Lx
         nwpc1 = Nwpc + 1
         nout = Outtap
         WRITE (nout,99001)
99001    FORMAT (///1X,'*** SCAN INTERACTIVE INPUT ***')
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ CASECC FILE AND SAVE DATA IN LCSE, ONE SUBCASE AT A TIME
!     SAVE SET DATA IN CORE BEGIN AT CORE(BGN)
!
         Lcse(166) = 200
         Lcse(199) = 0
         Lcse(200) = 0
         nz = korsz(Core(1)) - 3*Ibuf - 1
         nz = min0(nz,Lcore)
         iscan = 0
         Nset = 0
         i81 = nwpc1
         subid = -1
         Lx = 0
         IF ( Icomp==-2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     NO QUESTION ASKED IF SORT2 DATA TYPE IS USED.
!
         Lx = 1
 20      WRITE (nout,99002)
99002    FORMAT (//,' ENTER SUBCASE ID (DEFAULT=FIRST SUBCASE)')
         READ (In,99003) r
99003    FORMAT (2A4)
         CALL a82int(*20,r,8,subid,i)
         IF ( subid==0 ) subid = -1
         IF ( Intra>10 ) WRITE (lu,99004) subid
99004    FORMAT (///3X,'SUBCASE ID',I8)
         spag_nextblock_1 = 3
      CASE (3)
         jj = 1
         CALL rewind(Casecc)
         CALL fwdrec(*80,Casecc)
         DO
            jj = jj + 1
            CALL read(*80,*80,Casecc,Lcse(jj),1,0,i)
            IF ( subid==-1 ) subid = Lcse(jj)
            IF ( Lcse(jj)==subid ) THEN
               Lcse(1) = Lcse(jj)
               CALL read(*80,*100,Casecc,Lcse(2),199,0,i)
               Lencc = Lcse(166)
               lsem = Lcse(Lencc)
               Nset = Lcse(Lencc-1)
               IF ( lsem>0 ) CALL read(*80,*100,Casecc,Core(i81),lsem,0,i)
               i81 = i81 + lsem
               bgn = i81
               end = i81
               DO
                  CALL read(*40,*40,Casecc,Core(i81),2,0,i)
                  jmp = Core(i81+1)
                  Core(i81+2) = jj
                  i81 = i81 + 3
                  CALL read(*80,*100,Casecc,Core(i81),jmp,0,i)
                  Nset = Nset + 1
                  i81 = i81 + jmp
               ENDDO
            ELSE
               CALL fwdrec(*80,Casecc)
            ENDIF
         ENDDO
 40      SPAG_Loop_1_2: DO
!
!     SET CARD
!
            WRITE (nout,99005)
99005       FORMAT (//,' ENTER A BLANK, OR A SET CARD (SEE USER MANUAL P. ','2.3-44)',/,' E.G.  SET 101 = 1, 5 THRU 20')
            SPAG_Loop_2_1: DO
               Core(i81) = Ieor
               Nogo = 0
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
                     CALL xrcard(Core(i81),nzz,card(1))
                     IF ( Core(i81+8)/=all ) THEN
                        Icc = 1
                        CALL ifp1c(i81,nzz)
!
!     CONTINUATION CARDS FOR SET ARE READ IN BY IFP1C
!
                        IF ( Nogo/=0 ) THEN
                           i81 = ib
                           CYCLE SPAG_Loop_1_2
                        ENDIF
                     ELSE
                        Core(i81) = Core(i81+4)
                        Core(i81+1) = 1
                        Core(i81+2) = jj
                        Core(i81+3) = -1
                        i81 = i81 + 4
                     ENDIF
                     Nset = Nset + 1
                     WRITE (nout,99006) Core(ib)
99006                FORMAT (/,' THIS NEW SET',I6,' IS DEFINED FOR LOCAL USE ONLY',//,' ENTER A BLANK, OR ANOTHER SET CARD')
                     kk = 55
                     IF ( Debug ) WRITE (6,99013) kk , i81
                  ELSE
                     j = lshift(1,20)
                     IF ( card(2)==debug2 ) Swtch1 = orf(j,Swtch1)
                     j = complf(j)
                     IF ( card(2)==debug3 ) Swtch1 = andf(j,Swtch1)
                     Debug = .FALSE.
                     IF ( card(2)==debug2 ) Debug = .TRUE.
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
                  l = Lencc
                  IF ( i81>nwpc1 ) THEN
                     j = bgn + 2
                     i81 = i81 - 1
                     DO i = nwpc1 , i81
                        IF ( i/=j ) THEN
                           l = l + 1
                           Lcse(l) = Core(i)
                        ELSE
                           j = j + Core(j-1) + 3
                        ENDIF
                     ENDDO
                     j = Lcore
                     DO i = 1 , l
                        Lcse(j) = Lcse(i)
                        j = j - 1
                     ENDDO
                     IF ( i>j ) CALL mesage(+8,0,name)
                     j = Lcore
                     DO i = 1 , l
                        z(i) = Lcse(j)
                        j = j - 1
                     ENDDO
                     IF ( Debug ) WRITE (6,99008) (z(i),i=1,l)
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
                  CALL xrcard(Core(i81),nzz,card(1))
                  CALL ifp1h(i81,nzz,jumph)
                  IF ( Nogo==0 ) THEN
                     IF ( jumph==0 ) THEN
!
                        j = Core(ib)
                        IF ( iscan==0 ) iscan = j
                        IF ( iscan==j ) iscan = 30000000
                        WRITE (nout,99011)
99011                   FORMAT (/,' ENTER A BLANK, OR ANOTHER SCAN CARD')
                        kk = 87
                        IF ( Debug ) WRITE (6,99013) kk , i81
                        CYCLE
                     ELSE
                        CALL ifp1h(0,0,2)
                     ENDIF
                  ENDIF
                  i81 = ib
                  IF ( Nogo/=0 ) CYCLE SPAG_Loop_1_4
               ENDIF
            ENDDO SPAG_Loop_2_3
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
         spag_nextblock_1 = 4
      CASE (4)
         IF ( Intra>10 ) Outtap = lu
         RETURN
!
 80      jj = jj - 1
         WRITE (nout,99012) subid , (z(i),i=1,jj)
99012    FORMAT (//,' SUBCASE',I5,' NOT FOUND',//,' EXISTING SUBCASES ARE -',(/5X,10I7))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 100     CALL mesage(+2,Casecc,name)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99013 FORMAT ('   ONLINS/',I2,4X,'I81 =',I7)
99014 FORMAT (20A4)
END SUBROUTINE onlins
