!*==pla42.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla42
USE C_BLANK
USE C_CONDAS
USE C_PLA42C
USE C_PLA42D
USE C_PLA42E
USE C_PLA42S
USE C_PLA4ES
USE C_PLA4UV
USE C_SYSTEM
USE C_ZBLPKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , c , d , dmt , fj , t , word
   INTEGER :: buffr1 , buffr2 , buffr3 , buffr4 , file , i , i1 , i2 , i3 , ibeg , ifile , ifirst , iflag , ilast , imat , inc ,    &
            & ipr , itemp , itype , izmax , j , jj , jjj , jlast , kkk , left , leftt , lim , lincor , low , m , matcr , nn ,       &
            & nwdsrd , setno
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(7) :: ecptot , mcbkgg
   INTEGER , DIMENSION(2) :: inpvt
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name , planos
   INTEGER , DIMENSION(40) , SAVE :: nwdsp2
   REAL , DIMENSION(4) :: p
   REAL , DIMENSION(16) :: tubsav
   EXTERNAL bckrec , bldpk , bldpkn , close , fread , gopen , korsz , locate , makmcb , mesage , open , pkbar , pkqad1 , pkqad2 ,   &
          & pkqdm , pkrod , pktri1 , pktri2 , pktrm , preloc , premat , pretrd , pretrs , read , skprec , write , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE PROCESSES THE SCRATCH DATA BLOCK ECPTS, WHICH IS THE
!     ECPTNL DATA BLOCK APPENDED WITH THE PROPER DISPLACEMENT VECTOR
!     COMPONENTS, AND CREATES THE STIFFNESS MATRIX KGGNL AND THE UPDATED
!     ECPTNL, ECPTNL1.  ECPTNL1, NAMED ECPTO IN THIS ROUTINE, DOES NOT
!     CONTAIN DISPLACEMENT VECTOR COMPONENTS.
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1)) , (p(1),ip(1))
   DATA name/4HPLA4 , 4H2   / , planos/1103 , 11/
   DATA nwdsp2/20 , 0 , 19 , 0 , 0 , 33 , 0 , 0 , 27 , 20 , 5*0 , 32 , 27 , 32 , 38 , 0 , 13*0 , 45 , 6*0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         DO i = 1 , 40
            Iovrly(i) = 1
         ENDDO
!
!     DETERMINE SIZE OF VARIABLE CORE AND SET UP BUFFERS
!
         izmax = korsz(Z)
         buffr1 = izmax - Sysbuf
         buffr2 = buffr1 - Sysbuf
         buffr3 = buffr2 - Sysbuf
         buffr4 = buffr3 - Sysbuf
         leftt = buffr4 - 1
         Ipass = Placnt - 1
         ipr = Iprec
!
!     READ THE CSTM INTO CORE
!
         file = Cstm
         Ncstm = 0
         Icstm = 0
         CALL open(*40,Cstm,Z(buffr1),Inrw)
         CALL skprec(Cstm,1)
         CALL read(*140,*20,Cstm,Z(Icstm+1),leftt,Eor,Ncstm)
         CALL mesage(-8,0,name)
 20      leftt = leftt - Ncstm
!
!     PRETRD SETS UP SUBSEQUENT CALLS TO TRANSD
!
         CALL pretrd(Z(Icstm+1),Ncstm)
         CALL pretrs(Z(Icstm+1),Ncstm)
         CALL close(Cstm,Clsrw)
 40      imat = Ncstm
!
!     SEARCH THE MPT FOR THE PLAFACT CARDS.
!
         file = Mpt
         CALL preloc(*120,Z(buffr1-3),Mpt)
         CALL locate(*160,Z(buffr1-3),planos,iflag)
         SPAG_Loop_1_1: DO
!
!     FIND THE CORRECT PLA SET NO.
!
            CALL fread(Mpt,setno,1,0)
            IF ( setno==Plsetn ) THEN
!
!     SKIP THE PROPER NO. OF WORDS ON THE PLFACT CARD SO THAT GAMMA AND
!     GAMMAS (GAMMA STAR) WILL BE CORRECTLY COMPUTED.
!
               IF ( Placnt>4 ) CALL fread(Mpt,0,-(Placnt-4),0)
               nwdsrd = 4
               IF ( Placnt<4 ) nwdsrd = Placnt
               CALL fread(Mpt,p,nwdsrd,0)
               IF ( Placnt<3 ) THEN
                  Gammas = 0.0
                  Plfact(1) = p(2) - p(1)
                  Gamma = Plfact(1)/p(1)
               ELSEIF ( Placnt==3 ) THEN
                  word = p(2) - p(1)
                  Plfact(1) = p(3) - p(2)
                  Gammas = word/p(1)
                  Gamma = Plfact(1)/word
               ELSE
                  word = p(3) - p(2)
                  Plfact(1) = p(4) - p(3)
                  Gammas = word/(p(2)-p(1))
                  Gamma = Plfact(1)/word
               ENDIF
               Plfact(2) = 0.0
               CALL close(Mpt,Clsrw)
!
!     CALL PREMAT TO READ MPT AND DIT INTO CORE.  NOTE NEGATIVE FILE NO.
!     FOR DIT TO TRIGGER PLA FLAG IN SUBROUTINE PREMAT.
!
               CALL premat(Z(imat+1),Z(imat+1),Z(buffr1),leftt,matcr,Mpt,-Dit)
               leftt = leftt - matcr
               Igpct = Ncstm + matcr
!
!     OPEN KGGNL, ECPTO, ECPTS, AND GPCT
!
               ifile = Kggnl
               CALL gopen(Kggnl,Z(buffr1),1)
               CALL makmcb(mcbkgg,Kggnl,0,6,ipr)
               CALL gopen(Ecpto,Z(buffr2),1)
               CALL makmcb(ecptot,Ecpto,0,0,0)
               CALL gopen(Ecpts,Z(buffr3),0)
               CALL gopen(Gpct,Z(buffr4),0)
               EXIT SPAG_Loop_1_1
            ELSE
               SPAG_Loop_2_2: DO
                  CALL fread(Mpt,nn,1,0)
                  IF ( nn==(-1) ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ THE FIRST TWO WORDS OF NEXT GPCT RECORD INTO INPVT(1).
!     INPVT(1) IS THE PIVOT POINT.  INPVT(1) .GT. 0 IMPLIES THE PIVOT
!     POINT IS A GRID POINT.  INPVT(1) .LT. 0 IMPLIES THE PIVOT POINT
!     IS A SCALAR POINT.  INPVT(2) IS THE NUMBER OF WORDS IN THE
!     REMAINDER OF THIS RECORD OF THE GPCT.
!
         file = Gpct
         CALL read(*100,*80,Gpct,inpvt(1),2,Neor,iflag)
         Ngpct = inpvt(2)
         CALL fread(Gpct,iz(Igpct+1),Ngpct,1)
         IF ( inpvt(1)<0 ) GOTO 80
!
!     FROWIC IS THE FIRST ROW IN CORE. (1 .LE. FROWIC .LE. 6)
!
         Frowic = 1
!
!     DECREMENT THE AMOUNT OF CORE REMAINING.
!
         left = leftt - 2*Ngpct
         IF ( left<=0 ) CALL mesage(-8,0,name)
         Ipoint = Igpct + Ngpct
         Npoint = Ngpct
         I6x6k = Ipoint + Npoint
         I6x6k = (I6x6k-1)/2 + 2
!
!     CONSTRUCT THE POINTER TABLE, WHICH WILL ENABLE SUBROUTINE PLA4B TO
!     INSERT THE 6 X 6 MATRICES INTO KGGNL.
!
         iz(Ipoint+1) = 1
         i1 = 1
         i = Igpct
         j = Ipoint + 1
         SPAG_Loop_1_3: DO
            i1 = i1 + 1
            IF ( i1>Ngpct ) EXIT SPAG_Loop_1_3
            i = i + 1
            j = j + 1
            inc = 6
            IF ( iz(i)<0 ) inc = 1
            iz(j) = iz(j-1) + inc
         ENDDO SPAG_Loop_1_3
         spag_nextblock_1 = 3
      CASE (3)
!
!     JMAX = NO. OF COLUMNS OF KGGNL THAT WILL BE GENERATED WITH THE
!     CURRENT GRID POINT.
!
         inc = 5
         ilast = Igpct + Ngpct
         jlast = Ipoint + Npoint
         IF ( iz(ilast)<0 ) inc = 0
         Jmax = iz(jlast) + inc
!
!     IF 2*6*JMAX .LT. LEFT, THERE ARE NO SPILL LOGIC PROBLEMS FOR KGGNL
!     SINCE THE WHOLE DOUBLE PRECISION SUBMATRIX OF ORDER 6 X JMAX CAN
!     FIT IN CORE.
!
         itemp = 6*Jmax
         IF ( 2*itemp<left ) THEN
            Nrowsc = 6
!
!     LROWIC IS THE LAST ROW IN CORE. (1 .LE. LROWIC .LE. 6)
!
            Lrowic = Frowic + Nrowsc - 1
         ELSE
            name(2) = inpvt(1)
            CALL mesage(30,85,name)
            Nrowsc = 3
            DO WHILE ( 2*Nrowsc*Jmax>=left )
               Nrowsc = Nrowsc - 1
               IF ( Nrowsc==0 ) CALL mesage(-8,0,name)
            ENDDO
            Lrowic = Frowic + Nrowsc - 1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     ZERO OUT THE KGGD SUBMATRIX IN CORE.
!
         low = I6x6k + 1
         lim = I6x6k + Jmax*Nrowsc
         DO i = low , lim
            dz(i) = 0.0D0
         ENDDO
!
!     INITIALIZE THE LINK VECTOR TO -1.
!
         DO i = 1 , Nlinks
            Link(i) = -1
         ENDDO
!
!     TURN FIRST PASS INDICATOR ON.
!
         ifirst = 1
!
!     READ THE 1ST WORD OF THE ECPT RECORD, THE PIVOT POINT, INTO NPVT.
!     IF NPVT .LT. 0, THE REMAINDER OF THE ECPT RECORD IS NULL SO THAT
!     1 OR 6 NULL COLUMNS MUST BE GENERATED
!
         file = Ecpts
         CALL fread(Ecpts,Npvt,1,0)
         IF ( Npvt<0 ) GOTO 80
!
!     WRITE PIVOT POINT ON ECPTNL1 (ECPTO)
!
         CALL write(Ecpto,Npvt,1,Neor)
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_4: DO
!
!     READ THE NEXT ELEMENT TYPE INTO THE CELL ITYPE.
!
            CALL read(*140,*60,Ecpts,itype,1,Neor,iflag)
!
!     READ THE ECPT ENTRY FOR THE CURRENT TYPE INTO THE ECPT ARRAY. THE
!     NUMBER OF WORDS TO BE READ WILL BE NWORDS(ITYPE).
!
            IF ( Nwords(itype)<=0 ) CALL mesage(-30,61,name)
            CALL fread(Ecpts,Ecpt,Nwords(itype),0)
            itemp = Iovrly(itype)
!
!     IF THIS IS THE 1ST ELEMENT READ ON THE CURRENT PASS OF THE ECPT
!     CHECK TO SEE IF THIS ELEMENT IS IN A LINK THAT HAS ALREADY BEEN
!     PROCESSED.
!
            IF ( ifirst/=1 ) THEN
!
!     THIS IS NOT THE FIRST PASS.  IF ITYPE(TH) ELEMENT ROUTINE IS IN
!     CORE, PROCESS IT.
!
               IF ( itemp==lincor ) EXIT SPAG_Loop_1_4
!
!     THE ITYPE(TH) ELEMENT ROUTINE IS NOT IN CORE.  IF THIS ELEMENT
!     ROUTINE IS IN A LINK THAT ALREADY HAS BEEN PROCESSED READ THE NEXT
!     ELEMENT.
!
!
!     SET A TO BE PROCESSED LATER FLAG FOR THE LINK IN WHICH THE ELEMENT
!     RESIDES
!
               IF ( Link(itemp)/=1 ) Link(itemp) = 0
!
!     SINCE THIS IS THE FIRST ELEMENT TYPE TO BE PROCESSED ON THIS PASS
!     OF THE ECPT RECORD, A CHECK MUST BE MADE TO SEE IF THIS ELEMENT
!     IS IN A LINK THAT HAS ALREADY BEEN PROCESSED.  IF IT IS SUCH AN
!     ELEMENT, WE KEEP IFIRST = 1 AND READ THE NEXT ELEMENT.
!
            ELSEIF ( Link(itemp)/=1 ) THEN
!
!     SET THE CURRENT LINK IN CORE = ITEMP AND IFIRST = 0
!
               lincor = itemp
               ifirst = 0
               EXIT SPAG_Loop_1_4
            ENDIF
         ENDDO SPAG_Loop_1_4
!
!     CALL THE PROPER ELEMENT ROUTINE.
!
!                     ROD      BEAM      TUBE     SHEAR     TWIST
!                       1         2         3         4         5
!                   TRIA1     TRBSC     TRPLT     TRMEM    CONROD
!                       6         7         8         9        10
!                   ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
!                      11        12        13        14        15
!                   QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
!                      16        17        18        19        20
!                   DAMP2     DAMP3     DAMP4      VISC     MASS1
!                      21        22        23        24        25
!                   MASS2     MASS3     MASS4     CONM1     CONM2
!                      26        27        28        29        30
!                  PLOTEL     REACT     QUAD3       BAR      CONE
!                      31        32        33        34        35
!                   TRIARG    TRAPRG    CTORDRG    CORE      CAP
!                      36        37        38        39        40
         IF ( itype==2 .OR. itype==4 .OR. itype==5 .OR. itype==7 .OR. itype==8 .OR. itype==11 .OR. itype==12 .OR. itype==13 .OR.    &
            & itype==14 .OR. itype==15 .OR. itype==20 .OR. itype==21 .OR. itype==22 .OR. itype==23 .OR. itype==24 .OR.              &
            & itype==25 .OR. itype==26 .OR. itype==27 .OR. itype==28 .OR. itype==29 .OR. itype==30 .OR. itype==31 .OR.              &
            & itype==32 .OR. itype==33 .OR. itype==35 .OR. itype==36 .OR. itype==37 .OR. itype==38 .OR. itype==39 .OR. itype==40 )  &
            & THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype==3 ) THEN
!
!     THIS IS A TUBE ELEMENT.  REARRANGE THE ECPT FOR THE TUBE SO THAT
!     IT IS IDENTICAL TO THE ONE FOR THE ROD.
!
!     SAVE THE ECPT ENTRY FOR THE TUBE EXCEPT FOR THE 3 WORDS WHICH WILL
!     BE UPDATED BY THE PKROD ROUTINE AND THE TRANSLATIONAL COMPONENTS
!     OF THE DISPLACEMENTS VECTORS.
!
            DO i = 1 , 16
               tubsav(i) = Ecpt(i)
            ENDDO
!
!     COMPUTE AREA, TORSIONAL INERTIA TERM AND STRESS COEFFICIENT.
!
            d = Ecpt(5)
            t = Ecpt(6)
            dmt = d - t
            a = dmt*t*Pi
            fj = .25*a*(dmt**2+t**2)
            c = d/2.0
!
!     MOVE THE END OF THE ECPT ARRAY DOWN ONE SLOT SO THAT ENTRIES 7
!     THROUGH  25 WILL BE MOVED TO POSITIONS 8 THROUGH 26.
!
            m = 26
            DO i = 1 , 19
               Ecpt(m) = Ecpt(m-1)
               m = m - 1
            ENDDO
            Ecpt(5) = a
            Ecpt(6) = fj
            Ecpt(7) = c
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( itype==6 ) THEN
!
!     TRIA1
!
            CALL pktri1
         ELSEIF ( itype==9 ) THEN
!
!     TRMEM
!
            CALL pktrm
         ELSEIF ( itype==16 ) THEN
!
!     QDMEM
!
            CALL pkqdm
         ELSEIF ( itype==17 ) THEN
!
!     TRIA2
!
            CALL pktri2
         ELSEIF ( itype==18 ) THEN
!
!     QUAD2
!
            CALL pkqad2
         ELSEIF ( itype==19 ) THEN
!
!     QUAD1
!
            CALL pkqad1
         ELSEIF ( itype==34 ) THEN
!
!     BAR
!
            CALL pkbar
         ELSE
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
!     ROD, CONROD
!
         CALL pkrod
!
!     IF THE ELEMENT IS A TUBE, RESTORE THE SAVED ECPTNL ENTRY AND STORE
!     THE UPDATED VARIABLES IN PROPER SLOTS.
!
         IF ( itype==3 ) THEN
            DO i = 1 , 16
               Ecpt(i) = tubsav(i)
            ENDDO
            Ecpt(17) = Ecpt(18)
            Ecpt(18) = Ecpt(19)
            Ecpt(19) = Ecpt(20)
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
!
!     WRITE ELEMENT TYPE AND UPDATED ECPT ENTRY ONTO ECPTNL1 (ECPTO)
!
         CALL write(Ecpto,itype,1,Neor)
         CALL write(Ecpto,Ecpt,nwdsp2(itype),Neor)
         ecptot(2) = ecptot(2) + 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     AT STATEMENT NO. 500 WE HAVE HIT AN EOR ON THE ECPT FILE.  SEARCH
!     THE LINK VECTOR TO DETERMINE IF THERE ARE LINKS TO BE PROCESSED.
!
 60      Link(lincor) = 1
         DO i = 1 , Nlinks
            IF ( Link(i)==0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         IF ( Nogo==1 ) CALL mesage(-61,0,0)
!
!     AT THIS POINT BLDPK THE NUMBER OF ROWS IN CORE ONTO THE KGGNL FILE
!
         i1 = 0
         SPAG_Loop_1_5: DO
            i2 = 0
            ibeg = I6x6k + i1*Jmax
            CALL bldpk(2,ipr,ifile,0,0)
            DO
               i2 = i2 + 1
               IF ( i2>Ngpct ) THEN
                  CALL bldpkn(ifile,0,mcbkgg)
                  i1 = i1 + 1
                  IF ( i1<Nrowsc ) CYCLE SPAG_Loop_1_5
!
!     WRITE AN EOR ON ECPTO
!
                  CALL write(Ecpto,0,0,Eor)
!
!     TEST TO SEE IF THE LAST ROW IN CORE, LROWIC, = THE TOTAL NO. OF
!     ROWS TO BE COMPUTED = 6.  IF IT IS, WE ARE DONE.  IF NOT, THE
!     ECPTS MUST BE BACKSPACED.
!
                  IF ( Lrowic==6 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL bckrec(Ecpts)
                  Frowic = Frowic + Nrowsc
                  Lrowic = Lrowic + Nrowsc
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  jj = Igpct + i2
                  Index = iabs(iz(jj)) - 1
                  lim = 6
                  IF ( iz(jj)<0 ) lim = 1
                  jjj = Ipoint + i2
                  kkk = ibeg + iz(jjj) - 1
                  i3 = 0
                  SPAG_Loop_3_6: DO
                     i3 = i3 + 1
                     IF ( i3>lim ) EXIT SPAG_Loop_3_6
                     Index = Index + 1
                     kkk = kkk + 1
                     Dpword = dz(kkk)
                     IF ( Dpword/=0.0D0 ) CALL zblpki
                  ENDDO SPAG_Loop_3_6
               ENDIF
            ENDDO
            GOTO 80
         ENDDO SPAG_Loop_1_5
         spag_nextblock_1 = 8
      CASE (8)
!
!     SINCE AT LEAST ONE LINK HAS NOT BEEN PROCESSED THE ECPT FILE MUST
!     BE BACKSPACED.
!
         CALL bckrec(Ecpts)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      IF ( Nogo==1 ) CALL mesage(-61,0,0)
!
!     HERE WE HAVE A PIVOT POINT WITH NO ELEMENTS CONNECTED, SO THAT
!     NULL COLUMNS MUST BE OUTPUT ON THE KGGD FILE.
!
         file = Ecpts
         lim = 6
         IF ( inpvt(1)<0 ) lim = 1
         DO i = 1 , lim
            CALL bldpk(2,ipr,ifile,0,0)
            CALL bldpkn(Kggnl,0,mcbkgg)
         ENDDO
         CALL skprec(Ecpts,1)
!
!     WRITE PIVOT POINT ON ECPTO
!
         CALL write(Ecpto,Npvt,1,Eor)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     CHECK NOGO FLAG. IF NOGO = 1, TERMINATE EXECUTION
!
 100     IF ( Nogo==1 ) CALL mesage(-61,0,0)
!
!     WRAP UP BEFORE RETURN
!
         CALL close(Ecpts,Clsrw)
         CALL close(Ecpto,Clsrw)
         CALL close(Gpct,Clsrw)
         CALL close(Kggnl,Clsrw)
         mcbkgg(3) = mcbkgg(2)
         CALL wrttrl(mcbkgg)
         CALL wrttrl(ecptot)
         RETURN
!
!     ERROR RETURNS
!
 120     CALL mesage(-1,file,name)
 140     CALL mesage(-2,file,name)
 160     CALL mesage(-4,file,name)
         spag_nextblock_1 = 9
      CASE (9)
         CALL mesage(-30,92,itype)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pla42
