
SUBROUTINE onlins(*,Lx)
!
!     ON-LINE SCAN ROUTINE, CALLED ONLY BY SCAN
!
!     WRITTEN FEBY G.CHAN/SPERRY,  FEB. 1986
!
   IMPLICIT NONE
   INTEGER Amax , Amin , Card(20) , Casecc , Core(1) , Dum(74) , Dumm(2) , Dummy(3) , Ibeg , Iblnk , Ibuf , Icc , Icomp , Icompx ,  &
         & Idum(4) , Idupl , Iel , Ielt(2) , Iend , Ieor , Iequal , Imax , Imin , In , Inc , Intra , Iopt , Is , Iset , Isort ,     &
         & Isub , Iz(1) , Jdum(6) , Lbeg , Lcore , Lcse(400) , Lencc , Lend , Mach , Ncpw , Nmodes , Nogo , Nset , Ntop , Nwpc ,    &
         & Outtap , Scr1 , Skip(2) , Swtch1
   LOGICAL Debug
   REAL Z(30)
   COMMON /blank / Ielt , Icomp , Ntop , Amax , Amin , Ibeg , Iend , Icompx
   COMMON /ifp1a / Scr1 , Casecc , Is , Nwpc , Ncpw , Nmodes , Icc , Nset , Dummy , Isub , Lencc , Iblnk , Iequal , Ieor
   COMMON /machin/ Mach
   COMMON /system/ Ibuf , Outtap , Nogo , In , Dum , Swtch1 , Jdum , Intra
   COMMON /xscanx/ Skip , Lcore , Lbeg , Lend , Dumm , Iel , Iopt , Iset , Isort , Idum , Debug
   COMMON /zzzzzz/ Lcse , Core
   INTEGER Lx
   INTEGER all , bgn , blank , debug1 , debug2 , debug3 , end , equal , i , i0 , i81 , ib , iscan , j , jj , jmp , jumph , kk , l , &
         & lsem , lu , name(2) , nout , nwpc1 , nz , nzz , stop , subid
   INTEGER andf , complf , korsz , lshift , orf , rshift
   REAL r(2)
   EXTERNAL andf , complf , lshift , orf , rshift
!
!            /ZZIFP1/ IS THE OPEN CORE FOR SCAN
   EQUIVALENCE (Iz(1),Lcse(1))
   EQUIVALENCE (Imax,Amax) , (Imin,Amin) , (Idupl,Ibeg) , (Inc,Iend) , (Card(1),Core(1)) , (Iz(1),Z(1))
   DATA blank , equal , stop , all , name/4H     , 4H=    , 4HSTOP , 4HALL  , 4HONLI , 4HNS  /
   DATA lu , debug1 , debug2 , debug3 , i0/1 , 4HDEBU , 4HG ON , 4HG OF , 0/
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
99001 FORMAT (///1X,'*** SCAN INTERACTIVE INPUT ***')
!
!     READ CASECC FILE AND SAVE DATA IN LCSE, ONE SUBCASE AT A TIME
!     SAVE SET DATA IN CORE BEGIN AT CORE(BGN)
!
 100  Lcse(166) = 200
   Lcse(199) = 0
   Lcse(200) = 0
   nz = korsz(Core(1)) - 3*Ibuf - 1
   nz = min0(nz,Lcore)
   iscan = 0
   Nset = 0
   i81 = nwpc1
   subid = -1
   Lx = 0
   IF ( Icomp==-2 ) GOTO 300
!
!     NO QUESTION ASKED IF SORT2 DATA TYPE IS USED.
!
   Lx = 1
 200  WRITE (nout,99002)
99002 FORMAT (//,' ENTER SUBCASE ID (DEFAULT=FIRST SUBCASE)')
   READ (In,99003) r
99003 FORMAT (2A4)
   CALL a82int(*200,r,8,subid,i)
   IF ( subid==0 ) subid = -1
   IF ( Intra>10 ) WRITE (lu,99004) subid
99004 FORMAT (///3X,'SUBCASE ID',I8)
 300  jj = 1
   CALL rewind(Casecc)
   CALL fwdrec(*700,Casecc)
   DO
      jj = jj + 1
      CALL read(*700,*700,Casecc,Lcse(jj),1,0,i)
      IF ( subid==-1 ) subid = Lcse(jj)
      IF ( Lcse(jj)==subid ) THEN
         Lcse(1) = Lcse(jj)
         CALL read(*700,*800,Casecc,Lcse(2),199,0,i)
         Lencc = Lcse(166)
         lsem = Lcse(Lencc)
         Nset = Lcse(Lencc-1)
         IF ( lsem>0 ) CALL read(*700,*800,Casecc,Core(i81),lsem,0,i)
         i81 = i81 + lsem
         bgn = i81
         end = i81
         DO
            CALL read(*400,*400,Casecc,Core(i81),2,0,i)
            jmp = Core(i81+1)
            Core(i81+2) = jj
            i81 = i81 + 3
            CALL read(*700,*800,Casecc,Core(i81),jmp,0,i)
            Nset = Nset + 1
            i81 = i81 + jmp
         ENDDO
      ELSE
         CALL fwdrec(*700,Casecc)
      ENDIF
   ENDDO
!
!     SET CARD
!
 400  WRITE (nout,99005)
99005 FORMAT (//,' ENTER A BLANK, OR A SET CARD (SEE USER MANUAL P. ','2.3-44)',/,' E.G.  SET 101 = 1, 5 THRU 20')
   DO
      Core(i81) = Ieor
      Nogo = 0
      CALL xread(*400,Card)
      IF ( Card(1)==blank .AND. Card(2)==blank ) THEN
!
!     SET DATA - FROM CORE(BGN) THRU CORE(END)
!
         end = i81 - 1
         nzz = nz - i81
         EXIT
      ELSE
         WRITE (lu,99014) Card
         IF ( Card(1)/=debug1 ) THEN
            ib = i81
            nzz = nz - i81
            CALL xrcard(Core(i81),nzz,Card(1))
            IF ( Core(i81+8)/=all ) THEN
               Icc = 1
               CALL ifp1c(i81,nzz)
!
!     CONTINUATION CARDS FOR SET ARE READ IN BY IFP1C
!
               IF ( Nogo/=0 ) THEN
                  i81 = ib
                  GOTO 400
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
99006       FORMAT (/,' THIS NEW SET',I6,' IS DEFINED FOR LOCAL USE ONLY',//,' ENTER A BLANK, OR ANOTHER SET CARD')
            kk = 55
            IF ( Debug ) WRITE (6,99013) kk , i81
         ELSE
            j = lshift(1,20)
            IF ( Card(2)==debug2 ) Swtch1 = orf(j,Swtch1)
            j = complf(j)
            IF ( Card(2)==debug3 ) Swtch1 = andf(j,Swtch1)
            Debug = .FALSE.
            IF ( Card(2)==debug2 ) Debug = .TRUE.
            GOTO 400
         ENDIF
      ENDIF
   ENDDO
!
!     SCAN CARD
!
 500  WRITE (nout,99007)
99007 FORMAT (//,' ENTER A BLANK, OR A SCAN CARD (SEE USER MANUAL P.2.3-41A',/,                                                     &
             &'  E.G. SCAN (STRESS,CBAR,AXIAL,SA/MAX) = 15, SET 102',/,'       SCAN (FORCE,3,ROD,2) = +2000.,-1500.',/,             &
             &'       SCAN (HELP)')
   DO
!
      jumph = 0
      CALL xread(*500,Card)
      IF ( Card(1)==stop .AND. Card(2)==blank ) THEN
         RETURN 1
      ELSEIF ( Card(1)==blank .AND. Card(2)==blank ) THEN
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
               Z(i) = Lcse(j)
               j = j - 1
            ENDDO
            IF ( Debug ) WRITE (6,99008) (Z(i),i=1,l)
99008       FORMAT (//,' Z(1...200+) =',(/4X,10I7))
         ENDIF
         IF ( Lx>0 ) Lx = l
!
         IF ( iscan/=20000000 ) THEN
            IF ( Z(25)==0 ) THEN
!
               WRITE (nout,99009)
99009          FORMAT (//,' STRESS OUTPUT FILE NOT AVAILABLE FOR SCAN',//)
               CYCLE
            ELSE
!
!     STRESS SCAN
!
               Z(24) = -1
               Z(25) = 1
               Z(26) = 1
            ENDIF
         ENDIF
         IF ( iscan/=20000000 ) EXIT
         IF ( Z(28)==0 ) THEN
            WRITE (nout,99010)
99010       FORMAT (//,' FORCE  OUTPUT FILE NOT AVAILABLE FOR SCAN',//)
         ELSE
!
!     FORCE SCAN
!
            Z(27) = -1
            Z(28) = 1
            Z(29) = 1
            EXIT
         ENDIF
      ELSE
         WRITE (lu,99014) Card
         ib = i81
         CALL xrcard(Core(i81),nzz,Card(1))
         CALL ifp1h(i81,nzz,jumph)
         IF ( Nogo==0 ) THEN
            IF ( jumph==0 ) THEN
!
               j = Core(ib)
               IF ( iscan==0 ) iscan = j
               IF ( iscan==j ) iscan = 30000000
               WRITE (nout,99011)
99011          FORMAT (/,' ENTER A BLANK, OR ANOTHER SCAN CARD')
               kk = 87
               IF ( Debug ) WRITE (6,99013) kk , i81
               CYCLE
            ELSE
               CALL ifp1h(0,0,2)
            ENDIF
         ENDIF
         i81 = ib
         IF ( Nogo/=0 ) GOTO 500
      ENDIF
   ENDDO
 600  IF ( Intra>10 ) Outtap = lu
   RETURN
!
 700  jj = jj - 1
   WRITE (nout,99012) subid , (Z(i),i=1,jj)
99012 FORMAT (//,' SUBCASE',I5,' NOT FOUND',//,' EXISTING SUBCASES ARE -',(/5X,10I7))
   GOTO 100
!
 800  CALL mesage(+2,Casecc,name)
   GOTO 600
99013 FORMAT ('   ONLINS/',I2,4X,'I81 =',I7)
99014 FORMAT (20A4)
END SUBROUTINE onlins
