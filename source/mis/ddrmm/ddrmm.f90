!*==ddrmm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmm
   USE c_ddrmc1
   USE c_names
   USE c_stdata
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(150) :: buf
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , frqset , i , ibase , ibuf , icol , index , isetx , iwrt , j , k , kk , l ,  &
            & m , nsetx , scrt1 , scrt2 , scrt3 , scrt4 , scrt5 , scrt6 , scrt7
   INTEGER , SAVE :: casecc , eor , iforce , ifrout , ilsym , ispcf , istres , noeor , pp , uv
   REAL :: diff , diff1 , frq
   INTEGER , DIMENSION(3) , SAVE :: dva
   INTEGER , DIMENSION(4) , SAVE :: ifile , ofile
   INTEGER , DIMENSION(15) :: inblk , oublk
   REAL , DIMENSION(146) :: ridrec
   REAL , DIMENSION(1) :: rz
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL close , cpystr , ddrmm1 , ddrmm2 , fname , fwdrec , korsz , mesage , open , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DYNAMIC-DATA-RECOVERY-MATRIX-METHOD
!
!     DMAP SEQUENCES. ONLY SORT2 IS USED
!
!     (TRANSIENT RESPONCE)
!     ====================
!     DDRMM    CASEXX,UHVT,PPT,IPHIP2,IQP2,IES2,IEF2,XYCDB,EST,MPT,DIT/
!              ZUPV2,ZQP2,ZES2,ZEF2, $
!
!     (FREQUENCY RESPONSE)
!     ====================
!     DDRMM    CASEXX,UHVF,PPF,IPHIP1,IQP1,IES1,IEF1,XYCDB,EST,MPT,DIT/
!              ZUPVC1,ZQPC1,ZESC1,ZEFC1, $
!       OR
!     DDRMM    CASEXX,UHVF,PPF,IPHIP2,IQP2,IES2,IEF2,XYCDB,EST,MPT,DIT/
!              ZUPVC2,ZQPC2,ZESC2,ZEFC2, $
!
   !>>>>EQUIVALENCE (Rz(1),Z(1)) , (Rbuf(1),Buf(1))
   !>>>>EQUIVALENCE (Scrt1,Scrt(1)) , (Scrt2,Scrt(2))
   !>>>>EQUIVALENCE (Scrt3,Scrt(3)) , (Scrt4,Scrt(4))
   !>>>>EQUIVALENCE (Scrt5,Scrt(5)) , (Scrt6,Scrt(6))
   !>>>>EQUIVALENCE (Scrt7,Scrt(7)) , (Ridrec(1),Idrec(1))
   !>>>>EQUIVALENCE (Buf1,Buff(1)) , (Buf2,Buff(2))
   !>>>>EQUIVALENCE (Buf3,Buff(3)) , (Buf4,Buff(4))
   !>>>>EQUIVALENCE (Buf5,Buff(5)) , (Buf6,Buff(6))
   DATA ifrout/145/ , dva/20 , 32 , 29/
   DATA istres , iforce , ispcf/23 , 26 , 35/
   DATA ilsym/166/
   DATA subr/4HDDRM , 4HM   /
   DATA eor , noeor/1 , 0/
   DATA casecc , uv , pp/101 , 102 , 103/
   DATA ifile/104 , 105 , 106 , 107/
   DATA ofile/201 , 202 , 203 , 204/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DETERMINE OPEN CORE AVAILABLE AND ALLOCATE BUFFERS.
!
         DO i = 1 , 100
            savdat(i) = 0
         ENDDO
         DO i = 6 , 8
            savdat(i) = 102
            savdat(i+11) = 102
         ENDDO
         savdat(15) = 102
         savdat(76) = 2
         savdat(77) = 10
         DO i = 1 , 7
            scrt(i) = i + 300
         ENDDO
         ncore = korsz(z)
         DO i = 1 , 6
            buff(i) = ncore - sysbuf - 2
            ncore = buff(i) - 1
         ENDDO
!
!     GET FIRST SUBCASE OF CASE CONTROL INTO CORE
!
         ierror = 0
         subcas = 1
         icc = 1
         file = casecc
         CALL open(*160,casecc,z(buf1),rdrew)
         CALL fwdrec(*180,casecc)
         CALL read(*180,*20,casecc,z(icc),ncore-icc,noeor,nwds)
         ierror = 1
         GOTO 220
!
 20      ncc = icc + nwds - 1
         CALL close(casecc,cls)
!
!     READ TRAILER OF SOLUTION DATA BLOCK. IF SOLUTION IS
!     COMPLEX, THEN FREQUENCY RESPONCE IS ASSUMED. IF REAL, THEN
!     TRANSIENT RESPONSE IS ASSUMED.
!
         mcb(1) = uv
         CALL rdtrl(mcb)
         trnsnt = .TRUE.
         IF ( mcb(5)>2 ) trnsnt = .FALSE.
!
!     SET NUMBER OF EIGENVALUES = ROWS IN SOLUTION DATA BLOCK
!
         nlambs = mcb(3)
!
!     SET NUMBER OF SOLUTIONS.(TIME STEPS X 3, OR FREQUENCYS)
!
         nsols = mcb(2)
!
!     OPEN UV AND POSITION OVER HEADER RECORD.
!
         file = uv
         CALL open(*160,uv,z(buf1),rdrew)
         CALL fwdrec(*180,uv)
         CALL close(uv,cls)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ LIST OF FREQUENCYS OR TIME STEPS FROM INPUT LOAD MATRIX
!     HEADER.
!
         ilist = ncc + 1
         file = pp
         CALL open(*160,pp,z(buf1),rdrew)
         ierror = 2
         CALL read(*180,*200,pp,buf(1),-2,noeor,nwds)
         CALL read(*180,*40,pp,z(ilist),ncore-ilist,noeor,entrys)
         GOTO 220
!
 40      nlist = ilist + entrys - 1
         CALL close(pp,clsrew)
!
!     IF FREQUENCY RESPONSE PROBLEM, AND USER HAS SPECIFIED A LIST OF
!     FREQUENCYS TO BE USED AS A GUIDE IN DETERMINING A SUBSET OF
!     SOLUTIONS FOR OUTPUT PURPOSES, AND NOT ALL SOLUTIONS WILL BE
!     OUTPUT, THEN A MODIFIED SOLUTION MATRIX IS NOW FORMED ON
!     SCRATCH-1. THIS WILL ELIMINATE UNNECESSARY MATRIX-MULTIPLIES LATER
!
!     IN ANY EVENT THE NEXT SUBCASE-S SOLUTIONS ARE PLACED ON SCRT1.
!
         uvsol = uv
         IF ( .NOT.(trnsnt) ) THEN
!
!     EXPAND LIST OF FREQS PLACING A FLAG AFTER EACH.
!
            j = nlist
            nlist = nlist + entrys
            ierror = 4
            IF ( nlist>ncore ) GOTO 220
            k = nlist - 1
            DO i = 1 , entrys
               z(k) = z(j)
               z(k+1) = 0
               k = k - 2
               j = j - 1
            ENDDO
!
!     SET FLAGS OF FREQUENCYS TO BE OUTPUT.
!
            index = icc + ifrout - 1
            frqset = z(index)
            IF ( frqset>0 ) THEN
               index = icc + ilsym - 1
               index = z(index) + 1
               SPAG_Loop_1_1: DO
                  isetx = index + 2
                  nsetx = isetx + z(index+1) - 1
                  IF ( z(index)==frqset ) THEN
!
!     COMPARE REQUESTED FREQS WITH ACTUAL FREQS.
!
                     DO i = isetx , nsetx
                        k = 0
                        diff = 1.0E+25
                        frq = rz(i)
                        DO j = ilist , nlist , 2
                           IF ( z(j+1)==0 ) THEN
                              diff1 = abs(rz(j)-frq)
                              IF ( diff1<diff ) THEN
                                 diff = diff1
                                 k = j
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( k/=0 ) z(k+1) = 1
                     ENDDO
                     GOTO 50
                  ELSE
                     index = nsetx + 1
                     IF ( index>=ncc ) THEN
                        frqset = -1
                        EXIT SPAG_Loop_1_1
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_1
            ENDIF
!
!     ALL FREQUENCYS TO BE OUTPUT.
!
            DO i = ilist , nlist , 2
               z(i+1) = 1
            ENDDO
!
 50         file = uv
            ierror = 5
            CALL open(*160,uv,z(buf1),rd)
            file = scrt1
            CALL open(*160,scrt1,z(buf2),wrtrew)
            CALL fname(scrt1,filnam)
            CALL write(scrt1,filnam,2,eor)
            file = uv
!
!     COPY SOLUTION COLUMNS TO BE USED BY NOTEING FREQS MARKED FOR USE.
!
            nsols = 0
            inblk(1) = uv
            oublk(1) = scrt1
            DO i = ilist , nlist , 2
               IF ( z(i+1)/=0 ) THEN
!
!     BLAST COPY THIS SOLUTION.
!
                  icol = (i-ilist)/2 + 1
                  CALL cpystr(inblk,oublk,0,icol)
                  nsols = nsols + 1
               ELSE
                  CALL fwdrec(*180,uv)
               ENDIF
            ENDDO
!
!     RESET -UV- DATA BLOCK DESIGNATOR TO POINT TO SCRT1, AND WRITE
!     A TRAILER.
!
            CALL close(uv,cls)
            CALL close(scrt1,clsrew)
            mcb(1) = uv
            CALL rdtrl(mcb)
            mcb(1) = scrt1
            mcb(2) = nsols
            CALL wrttrl(mcb)
            uvsol = scrt1
!
!     SHRINK UP THE FREQUENCY LIST TO MATCH SOLUTION MATRIX
!
            j = ilist - 1
            DO i = ilist , nlist , 2
               IF ( z(i+1)/=0 ) THEN
                  j = j + 1
                  z(j) = z(i)
               ENDIF
            ENDDO
            nlist = j
         ENDIF
!
!     IF THIS IS A TRANSIENT RESPONSE PROBLEM, THE SOLUTION MATRIX IS
!     NOW PARTITIONED INTO 3 SOLUTION MATRICES FOR DISP, VEL, AND ACCEL.
!
         IF ( trnsnt ) THEN
            file = uv
            ierror = 6
            CALL open(*160,uv,z(buf1),rd)
            mcb(1) = uv
            inblk(1) = uv
            CALL rdtrl(mcb)
            DO i = 1 , 3
               file = scrt(i)
               ibuf = buff(i+1)
               CALL open(*160,file,z(ibuf),wrtrew)
               CALL fname(file,filnam)
               CALL write(file,filnam,2,eor)
               mcb(1) = file
               mcb(2) = nsols/3
               CALL wrttrl(mcb)
            ENDDO
            ierror = 7
            file = uv
            DO i = 1 , nsols , 3
               DO j = 1 , 3
                  oublk(1) = scrt(j)
                  CALL cpystr(inblk,oublk,0,i)
               ENDDO
            ENDDO
            CALL close(uv,clsrew)
            nsols = nsols/3
!
            DO i = 1 , 3
               CALL close(scrt(i),clsrew)
            ENDDO
         ENDIF
!
!     SDR2 FORMED MODAL SOLUTIONS FOR DISPLACEMENTS, SINGLE-POINT-
!     CONSTRAINT-FORCES, ELEMENT STRESSES, AND ELEMENT FORCES MAY BE
!     PRESENT. (ALL WILL BE SORT1-REAL, OR SORT2-REAL)
!
!     IF THIS IS A TRANSIENT PROBLEM, THE SOLUTIONS PRESENT HAVE BEEN
!     PARTITIONED INTO THE DISPLACEMENT, VELOCITY, AND ACCELERATION
!     SUBSETS. ONLY WHEN OPERATING ON THE MODAL DISPLACEMENTS WILL THE
!     VELOCITY AND ACCELERATION SOLUTION SUBSET MATRICES BE USED.
!
         jfile = 1
         spag_nextblock_1 = 3
      CASE (3)
         infile = ifile(jfile)
!
!     CHECK FOR EXISTENCE OF MODAL SOLUTION -INFILE-.
!
         CALL open(*100,infile,z(buf1),rdrew)
         CALL fwdrec(*80,infile)
!
!     INFILE DOES EXIST.SET PARAMETERS FOR PROCESSING
!
!
!     OPEN OFP-FORMAT OUTPUT FILE FOR THIS INFILE.
!
         outfil = ofile(jfile)
         iwrt = wrtrew
         IF ( subcas>1 ) iwrt = wrt
         CALL open(*60,outfil,z(buf4),iwrt)
         IF ( subcas<=1 ) THEN
!
            CALL fname(outfil,filnam)
            CALL write(outfil,filnam,2,eor)
         ENDIF
         CALL close(outfil,cls)
!
!     READ FIRST OFP-ID RECORD AND DETERMINE WHAT THE HELL IS REALLY
!     PRESENT.
!
         ierror = 14
         CALL read(*80,*80,infile,idrec,146,eor,nwds)
!
!     MAJOR ID AND SORT1 OR SORT2 DETERMINATION.
!
         itype1 = idrec(2)/1000
         sort2 = .FALSE.
         IF ( itype1>1 ) sort2 = .TRUE.
         itype1 = idrec(2) - itype1*1000
!
!     BRANCH ON MAJOR ID
!
         IF ( itype1<1 .OR. itype1>7 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         passes = 1
         IF ( itype1==1 .OR. itype1==2 .OR. itype1==6 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( itype1==3 ) THEN
!
!     MODAL SPCF-S ARE ON INFILE.
!
            itemp = icc + ispcf - 1
            nwords = 2
         ELSEIF ( itype1==4 ) THEN
!
!     MODAL FORCES ARE ON INFILE.
!
            itemp = icc + iforce - 1
            nwords = 1
         ELSEIF ( itype1==5 ) THEN
!
!     MODAL STRESSES ARE ON INFILE.
!
            itemp = icc + istres - 1
            nwords = 1
         ELSE
!
!     MODAL DISPLACEMENTS = EIGENVECTORS ARE ON INFILE.
!
            passes = 3
            nwords = 2
         ENDIF
!
!     DETERMINE DISP, VEL, AND ACCEL SET REQUESTS.
!
         ibase = icc + ilsym - 1
         ibase = z(ibase) + 1
         DO i = 1 , passes
            IF ( passes/=1 ) itemp = icc + dva(i) - 1
            sets(1,i) = z(itemp)
            sets(2,i) = z(itemp+1)
            sets(3,i) = iabs(z(itemp+2))
            sets(4,i) = 0
            sets(5,i) = 0
            IF ( sets(1,i)>0 ) THEN
               index = ibase
               SPAG_Loop_2_2: DO
                  isetx = index + 2
                  IF ( z(index)==sets(1,i) ) THEN
                     sets(4,i) = isetx
                     sets(5,i) = z(index+1)
                     EXIT SPAG_Loop_2_2
                  ELSE
                     index = isetx + z(index+1)
                     IF ( index>=ncc ) THEN
                        sets(1,i) = -1
                        EXIT SPAG_Loop_2_2
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDDO
!
!     CALL PROCESSOR TO BUILD DATA-MATRIX ON SCRT5 AND MAPPING-DATA ON
!     SCRT4, AND THEN PERFORM OUTPUT OF RESULTS TO OUTFIL.
!
         IF ( sort2 ) THEN
!
!     SORT2 PROCESSOR
!
            CALL ddrmm2(*160,*180,*200,*220)
         ELSE
!
!     SORT1 PROCESSOR
!
            CALL ddrmm1(*160,*180,*200,*220)
         ENDIF
!
!     WRAP UP PROCESSING FOR THIS INFILE.
!
         mcb(1) = outfil
         mcb(2) = 1
         CALL wrttrl(mcb)
         GOTO 80
 60      WRITE (outpt,99001) uwm , infile
99001    FORMAT (A25,' 2331. (DDRMM-2) OUTPUT DATA BLOCK CORRESPONDING TO',' INPUT MODAL SOLUTION DATA BLOCK',I4,/5X,               &
                &'IS NOT PRESENT.  INPUT DATA BLOCK IGNORED.')
         GOTO 80
      CASE (4)
!
!     ILLEGAL INFILE DATA.
!
         WRITE (outpt,99002) uwm , infile
99002    FORMAT (A25,' 2332.  (DDRMM-4) INVALID INPUT DATA DETECTED IN ','DATA BLOCK',I5,'. PROCESSING STOPPED FOR THIS DATA BLOCK')
 80      CALL close(outfil,clsrew)
         CALL close(infile,clsrew)
         CALL close(scrt4,clsrew)
         CALL close(scrt5,clsrew)
!
!     PROCESS NEXT MODAL SOLUTION INPUT.
!
 100     jfile = jfile + 1
         IF ( jfile<=4 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ALL WORK COMPLETE FOR THIS SUBCASE. IF FREQUENCY RESPONSE PROCESS
!     NEXT SUBCASE.
!
         IF ( trnsnt ) GOTO 140
         file = casecc
         ierror = 471
         CALL open(*160,casecc,z(buf1),rd)
         CALL read(*140,*120,casecc,z(icc),ncore-icc,noeor,nwds)
         GOTO 220
!
 120     ncc = icc + nwds - 1
         subcas = subcas + 1
         CALL close(casecc,cls)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!//// SUBCAS  NUMBER NEEDS TO GET INTO OUTPUT BLOCKS
!
 140     ierror = 511
         CALL close(casecc,clsrew)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     ERRORS FORCING TERMINATION OF THIS MODULE.
!
 160     kk = 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 180     kk = 2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 200     kk = 3
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 220     kk = 8
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(kk,file,subr)
         WRITE (outpt,99003) swm , ierror
99003    FORMAT (A27,' 2333.  (DDRMM-1) MODULE DDRMM TERMINATED WITH ','VARIABLE IERROR =',I10)
!
!     INSURE ALL FILES CLOSED BEFORE RETURNING.
!
         DO l = 100 , 300 , 100
            DO m = 1 , 11
               jfile = m + l
               CALL close(jfile,clsrew)
            ENDDO
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
!
!     INSURE ALL OUT-FILES HAVE AN EOF.
!
         DO l = 201 , 204
            CALL open(*240,l,z(buf1),wrt)
            CALL close(l,clsrew)
 240     ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddrmm
