!*==smcspl.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE smcspl(Mcol,Zi)
   IMPLICIT NONE
   USE I_SMCOMX
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mcol
   INTEGER , DIMENSION(10) :: Zi
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , idir , iend , iend1 , iend2 , ifirst , iidx , ilen , im2 , imidx , imidx1 , index , index1 , index2 , indexp ,    &
            & indext , inext , iprev , ispace1 , ispace2 , iterms , itotal , j , jdir , jlen , jm2 , jmidx1 , jterms , k , kcolp1 , &
            & kpos , length , lidx , mdir , mm2 , mterms , mwords
   INTEGER , DIMENSION(4) :: itemp
!
! End of declarations rewritten by SPAG
!
!
! SMCSPL RETRIEVES COLUMN "MCOL" FROM THE SPILL FILE.
! IF THIS COLUMN IS THE PIVOT COLUMN AND NO SPACE IS AVAILABLE, THEN
! IN-MEMORY DATA WILL BE WRITTEN TO THE SPILL FILE TO MAKE SPACE
! AVAILABLE FOR THE COLUMN DATA.  IF THE COLUMN IS NOT THE PIVOT
! COLUMN, THEN THE DATA IS READ INTO THE SPILL ARRAY IN OPEN CORE.
! WHEN A NEW PIVOT COLUMN IS DETERMINED, AN ANALYSIS IS DONE TO
! FREE UP MEMORY OF COLUMN DATA NO LONGER NEEDED.
!
   mdir = (Mcol-1)*4 + 1
!
! POSITION SPILL FILE TO CORRECT RECORD FOR THIS COLUMN AND READ DATA
!
   CALL filpos(iscr1,Zi(mdir+3))
   CALL read(*500,*600,iscr1,Zi(ispill),4,0,4)
   mm2 = Zi(ispill+1)
   mterms = Zi(ispill+3)
   mwords = mm2 + mterms*ivwrds
!      PRINT *,' SMCSPL,MM2,MTERMS,MWORDS=',MM2,MTERMS,MWORDS
   CALL read(*500,*600,iscr1,Zi(ispill+4),mwords,1,mwords)
!
! CHECK IF WE HAVE ALREADY SCANNED FOR UNNEEDED COLUMNS FOR THIS PIVOT
!
!      PRINT *,' SMCSPL,MEMLCK,KCOL=',MEMLCK,KCOL
   IF ( memlck/=kcol ) THEN
      memlck = kcol
!
! SCAN FOR COLUMNS NO LONGER NEEDED AND ADD THEM TO THE FREE CHAIN
!
      ifirst = 0
      DO i = memcol1 , kcol
         idir = (i-1)*4 + 1
!
! CHECK TO SEE IF THIS COLUMN NEEDED BY ANY SUBSEQUENT COLUMNS TO FOLLOW
!
         IF ( Zi(idir+2)>=kcol ) THEN
            IF ( ifirst==0 ) ifirst = i
!
! DATA NO LONGER NEEDED, IS DATA IN MEMORY IF SO FREE THE SPACE TO THE
! FREE CHAIN
!
         ELSEIF ( Zi(idir)/=0 ) THEN
!
! DATA IS IN MEMORY, RETURN SPACE TO FREE CHAIN
! FIRST, CHECK IF A FREE CHAIN EXISTS
!
            IF ( memfre/=0 ) THEN
!
! FREE CHAIN EXISTS, ADD THIS SPACE TO IT
!
               lidx = memlas
               iidx = Zi(idir)
               memlas = iidx
               Zi(lidx+1) = memlas
               Zi(iidx) = lidx
               Zi(iidx+1) = 0
               Zi(idir) = 0
            ELSE
!
! FREE CHAIN DOES NOT EXISTS, MAKE THIS SPACE THE FREE CHAIN
!
               iidx = Zi(idir)
               memfre = iidx
               memlas = iidx
               Zi(iidx) = 0
               Zi(iidx+1) = 0
               Zi(idir) = 0
            ENDIF
         ENDIF
      ENDDO
      memcol1 = ifirst
   ENDIF
!
! CHECK IF THE FREE CHAIN IS EMPTY
!
   IF ( memfre==0 ) GOTO 200
!
! LOOP THROUGH FREE CHAIN TO FIND BLOCK LARGE ENOUGH FOR DATA
!
   iidx = memfre
   DO WHILE ( Zi(iidx+2)<(mwords+4) )
      iidx = Zi(iidx+1)
!
! FREE CHAIN EXHAUSTED WITHOUT LARGE ENOUGH BLOCK, MUST CREATE SPACE
!
!      PRINT *,' SMCSPL GOING TO 1000 FROM 400'
      IF ( iidx==0 ) GOTO 200
   ENDDO
!
! SPACE FOUND, USE THIS FOR THE COLUMN DATA READ FROM THE SPILL FILE.
! RECONNECT FREE CHAIN WITHOUT THIS SPACE
!
 100  Zi(mdir) = iidx
   iprev = Zi(iidx)
   inext = Zi(iidx+1)
!      PRINT *,' SMCSPL,AFTER 500,IPREV,INEXT=',IPREV,INEXT
   IF ( iprev/=0 ) THEN
      IF ( inext==0 ) THEN
         Zi(iprev+1) = 0
         memlas = iprev
      ELSE
!      PRINT *,' SMCSPL,AFTER 510,INEXT,IPREV=',INEXT,IPREV
         Zi(iprev+1) = inext
         Zi(inext) = iprev
      ENDIF
   ELSEIF ( inext==0 ) THEN
      memfre = 0
   ELSE
      Zi(inext) = 0
      memfre = inext
   ENDIF
!
! MOVE DATA TO IN MEMORY LOCATION
!
   Zi(mdir) = iidx
   Zi(mdir+3) = 0
   Zi(iidx) = Mcol
   Zi(iidx+1) = mm2
   Zi(iidx+3) = mterms
   DO j = 1 , mwords
      Zi(iidx+j+3) = Zi(ispill+j+3)
   ENDDO
   memcoln = Mcol
!      PRINT *,' SMCSPL,A540,IIDX,ZI(1-5=',IIDX,(ZI(IIDX+KB),KB=0,4)
   GOTO 400
!
! NO SPACE FOUND IN MEMORY FOR THIS DATA.
! CHECK IF COLUMN BEING REQUESTED IS THE PIVOT COLUMN
!
!      PRINT *,' SMCSPL,MCOL,KCOL=',MCOL,KCOL
 200  DO WHILE ( Mcol==kcol )
!
! COLUMN REQUESTED IS THE PIVOT COLUMN, FIRST DETERMINE IF THERE
! ARE CONTIGUOUS BLOCKS IN THE FREE CHAIN THAT CAN BE MERGED TOGETHER
!
      IF ( memfre/=0 ) THEN
         index1 = memfre
         indext = memfre
         DO
            index2 = Zi(indext+1)
            IF ( index2==0 ) THEN
!
!  NO BLOCKS CONTIGUOUS WITH THIS BLOCK, GET NEXT BLOCK IN CHAIN
!  AND CHECK FOR CONTIGUOUS BLOCKS WITH IT.
!
               index1 = Zi(index1+1)
!
!  FIRST CHECK THAT THERE IS ANOTHER BLOCK IN THE FREE CHAIN
!
               IF ( index1==0 ) EXIT
               indext = memfre
            ELSE
!
! COMPUTE THE LAST ADDRESS (PLUS 1) OF THIS FREE BLOCK AND COMPARE
! IT WITH THE BEGINNING OF BLOCK REFERENCED BY VARIABLE "INDEX1"
!
               iend = index2 + Zi(index2+2)
               IF ( iend==index1 ) THEN
!
! BLOCK IS CONTIGUOUS, MERGE THIS BLOCK AND THEN GO BACK TO
! TEST THE FREE CHAIN FOR SPACE FOR THE CURRENT PIVOT COLUMN.
!     EACH FREE CHAIN BLOCK HAS THE FOLLOWING FORMAT FOR THE FIRST 3
!     WORDS:
!             (1) = Pointer to previous block in chain
!             (2) = Pointer to next block in chain
!             (3) = Number of words in this block
!         (Note: Blocks are allocated from high memory to low:)
!              Memory Address N
!                     Block k
!                     Block k-1
!                        .
!                     Block 1
!              Memory Address N+M
!
!      PRINT *,' SMCSPL,A1200,INDEX1,INDEX2=',INDEX1,INDEX2
                  Zi(index2+2) = Zi(index1+2) + Zi(index2+2)
!
! RESET NEXT AND PREVIOUS POINTERS OF CHAIN BLOCKS
!
                  indexp = Zi(index1)
                  Zi(index2) = indexp
                  IF ( indexp==0 ) memfre = index2
                  IF ( indexp/=0 ) Zi(indexp+1) = index2
!      PRINT *,' SMCSPL,A1200,MWORDS,ZI(INDEX1+2=',MWORDS,ZI(INDEX1+2)
                  IF ( Zi(index2+2)<(mwords+4) ) GOTO 300
                  iidx = index2
                  GOTO 100
               ELSE
!
! BLOCK IS NOT CONTIGUOUS, GO AND TEST NEXT BLOCK IN CHAIN
!
                  indext = index2
               ENDIF
            ENDIF
         ENDDO
      ENDIF
!
! COLUMN REQUESTED IS THE PIVOT COLUMN, MUST FIND MEMORY TO READ
! THIS DATA INTO.  SEARCH FOR LAST COLUMN IN MEMORY WITH SUFFICIENT
! SPACE AND WRITE THAT COLUMN TO SPILL AND READ THE PIVOT COLUMN DATA
! INTO THE MEMORY THAT BECAME AVAILABLE.
!
      idir = (memcoln-1)*4 + 1
      kcolp1 = kcol + 1
      DO i = memcoln , 1 , -1
         IF ( i/=kcol ) THEN
            idir = (i-1)*4 + 1
!
! CHECK TO SEE IF DATA ALREADY ON SPILL FILE
!
            IF ( Zi(idir)/=0 ) THEN
!
! DATA IS IN MEMORY, CHECK TO SEE IF ENOUGH SPACE
!
               imidx = Zi(idir)
               IF ( Zi(imidx+2)>=(mwords+4) ) THEN
!
! SUFFICIENT SPACE, WRITE THIS COLUMN DATA TO THE SPILL FILE
! TO MAKE ROOM FOR THE PIVOTAL COLUMN DATA TO BE KEPT IN MEMORY.
! SKIP TO END OF FILE, BACKSPACE OVER EOF, CLOSE AND REOPEN FILE
! FOR WRITE WITH APPEND.
!
                  CALL dssend(iscr1)
                  CALL skprec(iscr1,-1)
                  CALL close(iscr1,2)
                  CALL gopen(iscr1,Zi(ibuf2),3)
                  im2 = Zi(imidx+1)
                  iterms = Zi(imidx+3)
                  length = im2 + iterms*ivwrds
                  itemp(1) = i
                  itemp(2) = im2
                  itemp(3) = 0
                  itemp(4) = iterms
                  CALL write(iscr1,itemp,4,0)
                  CALL savpos(iscr1,kpos)
                  CALL write(iscr1,Zi(imidx+4),length,1)
                  CALL close(iscr1,3)
                  CALL gopen(iscr1,Zi(ibuf2),0)
!
! SET DIRECTORY AND MOVE DATA INTO MEMORY LOCATION
!
!      PRINT *,' SMCSPL B1450,IMIDX,ISPILL=',IMIDX,ISPILL
                  Zi(idir) = 0
                  Zi(idir+3) = kpos
                  Zi(mdir) = imidx
                  Zi(mdir+3) = 0
                  Zi(imidx) = Mcol
                  Zi(imidx+1) = mm2
                  Zi(imidx+3) = mterms
!      PRINT *,' SMCSPL,B1450,MCOL,MM2,MTERMS=',MCOL,MM2,MTERMS
                  DO j = 1 , mwords
                     Zi(imidx+j+3) = Zi(ispill+j+3)
                  ENDDO
                  memcoln = Mcol
!      PRINT *,' SMCSPL,A1450,ZI(1-5=',(ZI(IMIDX+KB),KB=0,4)
                  GOTO 400
               ENDIF
            ENDIF
         ENDIF
      ENDDO
!
! NONE OF THE EXISTING IN-MEMORY ALLOCATIONS ARE LARGE ENOUGH.
! THEREFORE, MUST MERGE TWO TOGETHER TO TRY AND MAKE ENOUGH SPACE.
!
      DO i = memcoln , 1 , -1
         IF ( i/=kcol ) THEN
            idir = (i-1)*4 + 1
            IF ( Zi(idir)/=0 ) THEN
               imidx1 = Zi(idir)
               ispace1 = Zi(imidx1+2)
!      PRINT *,' SMCSPL,B1800,IMIDX1,ISPACE1=',IMIDX1,ISPACE1
               iend1 = imidx1 + ispace1
               DO j = memcoln , 1 , -1
                  IF ( j/=kcol ) THEN
                     IF ( j/=i ) THEN
                        jdir = (j-1)*4 + 1
                        IF ( Zi(jdir)/=0 ) THEN
                           jmidx1 = Zi(jdir)
                           ispace2 = Zi(jmidx1+2)
!      PRINT *,' SMCSPL,I1800,JMIDX1,ISPACE2=',JMIDX1,ISPACE2
                           iend2 = jmidx1 + ispace2
                           IF ( iabs(imidx1-iend2)>4 ) THEN
                              IF ( iabs(jmidx1-iend1)>4 ) CYCLE
                           ENDIF
!
! COLUMNS J AND I HAVE CONTIGUOUS MEMORY, CHECK IF COMBINED SPACE IS
! LARGE ENOUGH FOR THIS COLUMN
!
                           itotal = ispace1 + ispace2
!      PRINT *,' SMCSPL,A1700,ISPACE1,ISPACE2,ITOTAL,MWORDS='
!     &,         ISPACE1,ISPACE2,ITOTAL,MWORDS
                           IF ( itotal<(mwords+4) ) EXIT
!
! SPACE IS LARGE ENOUGH, SO WRITE COLUMNS I AND J TO SPILL AND MERGE
! THE TWO AREAS TOGETHER.
! SKIP TO END OF FILE, BACKSPACE OVER EOF, CLOSE AND REOPEN FILE
! FOR WRITE WITH APPEND.
!
                           CALL dssend(iscr1)
                           CALL skprec(iscr1,-1)
                           CALL close(iscr1,2)
                           CALL gopen(iscr1,Zi(ibuf2),3)
!
! WRITE COLUMN I TO SPILL FILE
!
                           im2 = Zi(imidx1+1)
                           iterms = Zi(imidx1+3)
                           ilen = im2 + iterms*ivwrds
                           itemp(1) = i
                           itemp(2) = im2
                           itemp(3) = 0
                           itemp(4) = iterms
!      PRINT *,' SMCSPL WRITING COLUMN I=',I
                           CALL write(iscr1,itemp,4,0)
                           CALL savpos(iscr1,kpos)
                           CALL write(iscr1,Zi(imidx1+4),ilen,1)
!
! RESET DIRECTORY FOR COLUMN I
!
                           Zi(idir) = 0
                           Zi(idir+3) = kpos
!
! WRITE COLUMN J TO THE SPILL FILE
!
                           jm2 = Zi(jmidx1+1)
                           jterms = Zi(jmidx1+3)
                           jlen = 4 + jm2 + jterms*ivwrds
                           itemp(1) = j
                           itemp(2) = jm2
                           itemp(3) = 0
                           itemp(4) = jterms
!      PRINT *,' SMCSPL,WRITING COLUMN J=',J
                           CALL write(iscr1,itemp,4,0)
                           CALL savpos(iscr1,kpos)
                           CALL write(iscr1,Zi(jmidx1+4),jlen,1)
!
! RESET DIRECTORY FOR COLUMN J
!
                           Zi(jdir) = 0
                           Zi(jdir+3) = kpos
                           CALL close(iscr1,3)
                           CALL gopen(iscr1,Zi(ibuf2),0)
                           index = jmidx1
                           IF ( imidx1<jmidx1 ) index = imidx1
!
! MOVE DATA INTO MEMORY LOCATION
!
                           PRINT * , ' B1750,INDEX,ISPILL=' , index , ispill
                           Zi(index) = Mcol
                           Zi(index+1) = mm2
                           Zi(index+2) = itotal
                           Zi(index+3) = mterms
                           Zi(mdir) = index
                           Zi(mdir+3) = 0
                           DO k = 1 , mwords
                              Zi(index+k+3) = Zi(ispill+k+3)
                           ENDDO
                           memcoln = Mcol
                           GOTO 400
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
      WRITE (nout,99001) Ufm , kcol
99001 FORMAT (1X,A23,/,' INSUFFICIENT CORE IN SUBROUTINE SMCSPL FOR',' SYMMETRIC DECOMPOSITION, COLUMN=',I6)
      ierror = 1
      GOTO 700
 300  ENDDO
!
! NO SPACE FOUND AND COLUMN IS NOT THE PIVOTAL COLUMN, USE DATA
! FROM SPILL AREA
!
!      print *,' smcspl is returning, memfre=',memfre
!      ikb = memfre
!      do 9777 kk = 1, 100
!      if ( ikb .eq. 0 ) go to 9778
!      print *,' free block i,1-3=',kk,(zi(ikb+kb),kb=0,2)
!      ikb = zi( ikb+1 )
!9777  continue
!9778  continue
 400  RETURN
 500  WRITE (nout,99002) Ufm , kcol
99002 FORMAT (1X,A23,/,' UNEXPECTED END OF FILE FOR COLUMN ',I4,' IN SUBROUTINE SMCSPL')
   ierror = 3
   GOTO 700
 600  WRITE (nout,99003) Ufm , kcol
99003 FORMAT (1X,A23,/,' UNEXPECTED END OF RECORD FOR COLUMN ',I4,' IN SUBROUTINE SMCSPL')
   ierror = 3
 700  CALL smchlp
   CALL mesage(-61,0,0)
END SUBROUTINE smcspl
