!*==sdcmm.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcmm(Z,Mset,Msze,Matrix,Uset,Gpl,Sil,Subnam)
   IMPLICIT NONE
   USE C_NAMES
   USE C_SDCQ
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Z
   INTEGER :: Mset
   INTEGER :: Msze
   INTEGER :: Matrix
   INTEGER :: Uset
   INTEGER :: Gpl
   INTEGER :: Sil
   INTEGER , DIMENSION(2) :: Subnam
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf2 , i , iout , iret , isubst , j , k , l , nbufsz , nstart , nwds
   CHARACTER(4) , DIMENSION(6) :: ctyp
   INTEGER , DIMENSION(14) , SAVE :: err
   INTEGER , DIMENSION(8) , SAVE :: exit
   INTEGER , DIMENSION(4) :: gpid
   INTEGER , SAVE :: iblk
   INTEGER , DIMENSION(3) :: in
   INTEGER , DIMENSION(4) , SAVE :: iner
   INTEGER , DIMENSION(7) :: n
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(6) :: typ
   REAL , DIMENSION(4) :: xgpid
   REAL , DIMENSION(3) :: xin
   EXTERNAL close , fread , mesage , mxcid , mxcids , open , page2
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE WRITES THE EXTERNAL ID AND COMPONENT ID FOR VARIOUS
!     MATRIX ERROR CONDITIONS.
!     SCRATCH1 CONTAINS 3 WORDS/ERROR, EACH MESSAGE BEING 1 RECORD
!         WORD 1 = COLUMN * 10 + ERROR CODE
!         WORD 2 = INPUT DIAGONAL
!         WORD 3 = OUTPUT DIAGONAL
!     SUBROUTINE -MXCID- (NON-SUBSTRUCTURING) IS CALLED TO SUPPLY IDENT.
!     DATA FOR EACH COLUMN.  FOR SUBSTRUCTURING -MXCIDS- IS CALLED - IT
!     RETURNS TWO WORDS/COLUMN PLUS THE BCD NAME OF THE SUBSTRUCTURES AT
!     THE START OF CORE.  IN EITHER CASE, THE 1ST WORD IS 10*ID +
!         COMPONENT.
!     THE SCRATCH FILE IS READ AND THE EXTERNAL ID INDEXED DIRECTLY.E
!     NOTE - THAT EACH COLUMN MAY GENERATE MORE THAN 1 MESSAGE.
!     OPEN CORE IS Z(1) TO Z(BUF-1).  TWO BUFFERS FOLLOW Z(BUF)
!
!WKBNB 8/94
!WKBNE 8/94
!WKBI  8/94
   !>>>>EQUIVALENCE (ctyp,typ) , (xgpid,gpid) , (xin,in)
   !>>>>EQUIVALENCE (Ksystm(1),Nbufsz) , (Ksystm(2),Iout) , (Ksystm(69),Isubst)
   DATA err/4HNULL , 4HCOL. , 4HZERO , 4HDIAG , 4HNEG. , 4HDIAG , 4HSING , 4HTEST , 4HBAD  , 4HCOL. , 4HNON- , 4HCONS , 4HZERO ,    &
       &4HDIAG/
   DATA iner/4HINPU , 2HT  , 4HDECM , 2HP /
   DATA exit/4HCONT , 4HINUE , 4HAT E , 4HND   , 4HAT S , 4HTART , 4HIN D , 4HECMP/
   DATA name/4HSDCM , 2HM /
   DATA iblk/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         buf2 = Buf + nbufsz
         n(1) = 0
         n(2) = 0
         n(3) = 0
         n(4) = 0
         n(5) = 0
         n(6) = 0
         n(7) = 0
         IF ( Buf<=0 ) GOTO 60
!
!     GENERATE EXTERNAL ID
!
         IF ( isubst==0 ) THEN
!
            nstart = 0
            nwds = 1
!
!     2 BUFFERS NEEDED
!
            CALL mxcid(*60,Z,Mset,Msze,nwds,Uset,Gpl,Sil,Buf)
         ELSE
!
!     SUBSTRUCTURING - READ EQSS FILE ON THE SOF
!
!     4 BUFFERS NEEDED
!
            i = Buf - 2*nbufsz
            IF ( i<=3*Msze ) GOTO 60
            nwds = 2
            CALL mxcids(*60,Z,Mset,Msze,nwds,Uset,i,Subnam)
            nstart = i - 1
         ENDIF
!
         CALL open(*100,Filmsg,Z(buf2),Krr0)
         CALL page2(3)
         WRITE (iout,99001) Uwm
99001    FORMAT (A25,' 2377A, MATRIX CONDITIONING ERRORS GIVEN WITH ','EXTERNAL ID',/5X,'GID - C  INPUT-DIAG.   DECOMP-DIAG.',6X,   &
                &'TYPE',17X,'SUBSTRUCTURE')
!
         ASSIGN 40 TO iret
         typ(5) = iblk
         typ(6) = iblk
         IF ( isubst/=0 ) ASSIGN 20 TO iret
         spag_nextblock_1 = 2
      CASE (2)
!
!     LOOP ON MESSAGES - 0 COLUMN IS FLAG TO QUIT
!
         CALL fread(Filmsg,in,3,1)
         IF ( in(1)==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = in(1)/10
         j = in(1) - i*10
         l = nstart + i*nwds
         gpid(1) = Z(l)/10
         gpid(2) = Z(l) - gpid(1)*10
         gpid(3) = in(2)
         gpid(4) = in(3)
         spag_nextblock_1 = 3
      CASE (3)
!
!     INTERNAL FUNCTION
!
         IF ( j<=0 .OR. j>7 ) THEN
!
!     ILLEGAL DATA
!
            CALL mesage(7,Filmsg,name)
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            k = 2*j - 1
            typ(1) = err(k)
            typ(2) = err(k+1)
            k = 1
            IF ( j>1 .AND. j<7 ) k = 3
            typ(3) = iner(k)
            typ(4) = iner(k+1)
            n(j) = n(j) + 1
            CALL page2(2)
            GOTO iret
         ENDIF
!
 20      typ(5) = Z(2*l-1)
         typ(6) = Z(2*l)
!WKBR 8/94      WRITE  (IOUT,40) GPID,TYP
 40      WRITE (iout,99002) gpid(1) , gpid(2) , xgpid(3) , xgpid(4) , ctyp
99002    FORMAT (1H0,I9,2H -,I2,1P,2E14.6,3X,2A5,2H/ ,A4,A2,6HMATRIX,2X,2A4)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     INSUFFICIENT CORE IN -MATCID-
!
 60      CALL page2(3)
         WRITE (iout,99003) Uwm
99003    FORMAT (A25,' 2377B, MATRIX CONDITIONING ERRORS GIVEN WITH ','INTERNAL ID',/,5X,'COLUMN  INPUT DIAG.   DECOMP-DIAG.',6X,   &
                &'TYPE')
!
         CALL open(*100,Filmsg,Z(buf2),Krr0)
         ASSIGN 80 TO iret
         spag_nextblock_1 = 4
      CASE (4)
!
!     LOOP
!
         CALL fread(Filmsg,in,3,1)
         IF ( in(1)==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = in(1)/10
         j = in(1) - i*10
         in(1) = i
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!WKBR 8/94  WRITE  (IOUT,90) IN,TYP
 80      WRITE (iout,99004) in(1) , xin(2) , xin(3) , ctyp
99004    FORMAT (1H0,I8,1P,2E14.6,3X,2A5,2H/ ,A4,A2,6HMATRIX,2X,2A4)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     SCRATCH FILE NOT AVAILABLE
!
 100     CALL mesage(1,Filmsg,name)
         spag_nextblock_1 = 5
      CASE (5)
!
!     ALL DONE, SUMMARIZE
!
         CALL page2(11)
         WRITE (iout,99005) Matrix , Msze , n
99005    FORMAT (1H0,3X,10HFOR MATRIX,I4,6H, SIZE,I8,/I9,13H NULL COLUMNS,/I9,15H ZERO DIAGONALS,/I9,19H NEGATIVE DIAGONALS,/I9,    &
                &31H SINGULARITY TOLERANCE EXCEEDED,/I9,12H BAD COLUMNS,/I9,24H NONCONSERVATIVE COLUMNS,/I9,                        &
                &23H ZERO DIAGONALS (INPUT))
!
!     CHECK FOR EXIT CONDITIONS
!
         i = 2*Noglev + 1
!
!     NOTE - NOGLEV OF 4 ALSO HAS NEGATIVE PARM(1)
!
         IF ( Noglev==4 ) i = 7
         j = i + 1
         WRITE (iout,99006) exit(i) , exit(j)
99006    FORMAT (1H0,3X,13HABORT CODE = ,2A4)
         CALL close(Filmsg,Kcl2)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sdcmm
