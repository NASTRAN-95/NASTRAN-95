!*==sorti.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sorti(Inpfl,Outfl,Nwds,Keywrd,L,Nx)
   USE c_setup
   USE c_system
   USE c_two
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nwds
   INTEGER :: Inpfl
   INTEGER :: Outfl
   INTEGER :: Keywrd
   INTEGER , DIMENSION(Nwds,2) :: L
   INTEGER :: Nx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bufa , bufb , bufc , dist1 , dist2 , dummy , file , i , if1 , if2 , ii , in1 , in2 , j , jj , k , key2 , keywd ,      &
            & last , len , level , limit , m , n , nflag , nn , nnn , nrec , nz , out , r , scra , scrb , scrc , temp , total ,     &
            & two31
   INTEGER , DIMENSION(2) , SAVE :: subr
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
! End of declarations rewritten by SPAG
!
!
!     WITH ENTRY POINT SORTI2 TO SORT TABLE BY 2 KEY WORDS
!
!     THIS SORTING ROUTINE WAS CALLED SORT BEFORE, AND IS NOW RENAMED
!     SORTI. IT IS CAPABLE FOR IN-CORE SORTING AND FILE SORT.
!
!     THE NEW SUBROUTINE SORT IS A TRUNCATED VERSION OF THIS ROUTINE
!     ONLY FOR IN-CORE SORTING. IT CAN HANDLE INTEGER, REAL, BCD(A4),
!     BCD(A8), BCD(A7), AND 2-KEY SORTINGS.
!
!     (95 PERCENT OF NASTRAN ROUTINES ACTUALLY CALL SORT. THE REMAINING
!       5 PERCENT CALL SORTI)
!
!     IF INPFL AND OUTFL ARE ZERO, CALLING ROUTINE SHOULD CALL SORT
!     FOR EFFICIENCY
!
!     THE OLD SHUTTLE EXCHANGE, WHICH WAS VERY SLOW, IS NOW REPLACED BY
!     A SUPER FAST SORTER, A MODIFIED SHELL SORT.
!
!     THIS MODIFIED VERSION ALSO SORTS TABLE OF ANY LENGTH (PREVIOUSLY N
!     OF WORDS PER ENTRY, NWDS, WAS LIMITED TO 20)
!
   !>>>>EQUIVALENCE (Nfile(1),Scrb) , (Nfile(2),Scrc) , (Nfile(3),Scra)
   DATA subr/4HSORT , 4HI   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         key2 = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY sorti2(Inpfl,Outfl,Nwds,Keywrd,L,Nx)
!     ==========================================
!
         key2 = 2
         spag_nextblock_1 = 2
      CASE (2)
!
!     IF INPFL EQ 0, CORE BLOCK L OF LENGTH NX IS TO BE SORTED
!     IF INPFL NE 0, INPFL IS TO BE SORTED USING BLOCK L
!
         keywd = iabs(Keywrd)
         nnn = Nx
         IF ( nnn<Nwds ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         j = 30
         IF ( nbpw>=60 ) j = 62
         two31 = 2**j
         IF ( Inpfl/=0 ) THEN
            bufa = Nx - sysbuf + 1
!
!     MINIMUM CORE REQUIREMENT = 2 X NUMBER OF WORDS PER ENTRY
!
            nz = bufa - 1
            IF ( nz<Nwds+Nwds ) THEN
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL open(*60,scra,L(bufa,1),1)
            nn = (nz/Nwds)*Nwds
            nnn = nn
            out = scra
            nrec = 0
            CALL read(*180,*20,Inpfl,L,nn,0,nnn)
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     SORT PHASE --
!
         len = nnn/Nwds
         IF ( len*Nwds/=nnn ) THEN
            j = -37
            CALL mesage(j,file,subr)
            RETURN
         ELSE
            m = len
            IF ( Keywrd<0 ) THEN
!
!                     - INTEGER SORT ONLY -
!     IF ORIGINAL ORDER IS TO BE MAINTAINED WHERE DUPLICATE KEYWORDS MAY
!     OCCUR, ADD INDICES TO THE KEYWORDS (GOOD FOR BOTH POSITIVE AND
!     NEGATIVE RANGES, AND BE SURE THAT KEYWORDS ARE NOT OVERFLOWED),
!     SORT THE DATA, AND REMOVE THE INDICES LATER
!
!     IF ANY KEYWORD OVERFLOWS, SWITCH TO SHUTTLE EXCHANGE METHOD
!     LIMIT IS THE MAX VALUE BEFORE INTEGER OVERFLOW
!
               IF ( len>=two(16) .AND. nbpw<=32 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               limit = (two31-len)/len
               DO i = 1 , len
                  j = L(keywd,i)
                  IF ( iabs(j)>limit ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  j = j*len + i
                  k = -1
                  IF ( j<0 ) k = -len
                  L(keywd,i) = j + k
               ENDDO
               IF ( key2/=1 ) THEN
                  DO i = 1 , len
                     j = L(keywd+1,i)
                     IF ( iabs(j)>limit ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     j = j*len + i
                     k = -1
                     IF ( j<0 ) k = -len
                     L(keywd+1,i) = j + k
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     SORT BY
!     MODIFIED SHELL METHOD, A SUPER FAST SORTER
!
         m = m/2
         IF ( m==0 ) THEN
            IF ( Keywrd<0 ) THEN
               DO i = 1 , len
                  L(keywd,i) = L(keywd,i)/len
                  IF ( key2==2 ) L(keywd+1,i) = L(keywd+1,i)/len
               ENDDO
            ENDIF
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
            j = 1
            k = len - m
            i = j
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         SPAG_Loop_1_1: DO
            n = i + m
            IF ( L(keywd,i)<L(keywd,n) ) EXIT SPAG_Loop_1_1
            IF ( L(keywd,i)==L(keywd,n) ) THEN
               IF ( key2==1 ) EXIT SPAG_Loop_1_1
               IF ( L(keywd+1,i)<=L(keywd+1,n) ) EXIT SPAG_Loop_1_1
            ENDIF
            DO r = 1 , Nwds
               temp = L(r,i)
               L(r,i) = L(r,n)
               L(r,n) = temp
            ENDDO
            i = i - m
            IF ( i<1 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         j = j + 1
         IF ( j>k ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i = j
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
!     SORT BY
!     SHUTTLE EXCHANGE METHOD, A SLOW SORTER
!     (THIS WAS NASTRAN ORIGINAL SORTER, MODIFIED FOR 2-D ARRAY
!     OPERATION WITH 20-COLUMN LIMITATION REMOVED)
!
         IF ( i>1 ) THEN
            j = i - 1
            DO i = 1 , j
               L(keywd+1,i) = L(keywd+1,i)/len
            ENDDO
         ENDIF
         i = len
         spag_nextblock_1 = 7
      CASE (7)
         IF ( i>1 ) THEN
            j = i - 1
            DO i = 1 , j
               L(keywd,i) = L(keywd,i)/len
            ENDDO
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
!
         DO ii = 2 , len
            jj = ii - 1
            IF ( L(keywd,ii)<L(keywd,jj) ) THEN
            ELSEIF ( L(keywd,ii)==L(keywd,jj) ) THEN
               IF ( key2==1 ) CYCLE
               IF ( L(keywd+1,ii)>=L(keywd+1,jj) ) CYCLE
            ELSE
               CYCLE
            ENDIF
            SPAG_Loop_2_2: DO
               jj = jj - 1
               IF ( jj<=0 ) EXIT SPAG_Loop_2_2
               IF ( L(keywd,ii)<L(keywd,jj) ) THEN
               ELSEIF ( L(keywd,ii)==L(keywd,jj) ) THEN
                  IF ( key2/=2 ) EXIT SPAG_Loop_2_2
                  IF ( L(keywd+1,ii)>=L(keywd+1,jj) ) EXIT SPAG_Loop_2_2
               ELSE
                  EXIT SPAG_Loop_2_2
               ENDIF
            ENDDO SPAG_Loop_2_2
            jj = jj + 2
            DO i = 1 , Nwds
               temp = L(i,ii)
               m = ii
               DO j = jj , ii
                  L(i,m) = L(i,m-1)
                  m = m - 1
               ENDDO
               L(i,jj-1) = temp
            ENDDO
         ENDDO
         spag_nextblock_1 = 9
      CASE (9)
!
!     IF CORE SORT, SORT IS COMPLETED. IF FILE SORT, WRITE BLOCK ON
!     SCRATCH FILE TO BE MERGED LATER.
!
         IF ( Inpfl==0 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         CALL write(scra,L,nnn,1)
         nrec = nrec + 1
         IF ( nnn/=nn ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL read(*180,*20,Inpfl,L,nn,0,nnn)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      IF ( nnn>0 ) THEN
            IF ( nnn-Nwds>=Nwds ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         CALL close(scra,1)
!
!     IF ONLY ONE RECORD, BYPASS MERGE
!
         IF ( nrec/=1 ) THEN
!
!     COMPUTE OPTIMUM DISTRIBUTION OF SORTED RECORDS ON TWO SCRATCH
!     FILES FOR MERGE PHASE USING FIBONACCI SEQUENCE
!
            level = 0
            dist1 = 1
            dist2 = 0
            total = 1
            SPAG_Loop_1_3: DO
               dummy = total - nrec
               IF ( dummy>=0 ) THEN
                  bufb = bufa - sysbuf
                  bufc = bufb - sysbuf
                  IF ( bufc<1 ) THEN
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  nn = bufb - 1
!
!     COPY N SORTED RECORDS ONTO SECOND SCRATCH FILE
!
                  CALL open(*60,scra,L(bufa,1),0)
                  CALL open(*80,scrb,L(bufb,1),1)
                  n = dist2 - dummy
                  DO i = 1 , n
                     DO
                        CALL read(*200,*22,scra,L,nn,0,nflag)
                        CALL write(scrb,L,nn,0)
                     ENDDO
 22                  CALL write(scrb,L,nflag,1)
                  ENDDO
                  CALL close(scrb,1)
                  CALL close(scra,2)
                  nfile(4) = scrb
                  nfile(5) = scrc
                  k = 4
!
!     MERGE PHASE ---
!     INPUT FILE WITH GREATER NUMBER IF RECORDS = IN1
!     INPUT FILE WITH LESSER  NUMBER OF RECORDS = IN2
!     EACH PASS MERGES ALL RECORDS FROM IN2 WITH LIKE NUMBER OF RECORDS
!     (INCLUDING DUMMY RECORDS) FROM IN1 ONTO OUT. FOR NEXT PASS IN1
!     BECOMES IN2, IN2 BECOMES OUT, AND OUT BECOMES IN1.
!
                  DO i = 1 , level
                     k = k - 1
                     IF ( k==0 ) k = 3
                     in1 = nfile(k)
                     in2 = nfile(k+1)
                     out = nfile(k+2)
                     last = 2
                     CALL open(*100,in1,L(bufa,1),2)
                     CALL open(*120,in2,L(bufb,1),2)
                     CALL open(*140,out,L(bufc,1),1)
                     DO j = 1 , dist2
                        spag_nextblock_2 = 1
                        SPAG_DispatchLoop_2: DO
                           SELECT CASE (spag_nextblock_2)
                           CASE (1)
                              if1 = Nwds
                              if2 = Nwds
                              CALL read(*220,*24,in1,L,Nwds,0,if1)
                              IF ( dummy<=0 ) THEN
                                 CALL read(*240,*26,in2,L(1,2),Nwds,0,if2)
                              ELSE
                                 dummy = dummy - 1
                                 if2 = 0
                                 GOTO 26
                              ENDIF
                              spag_nextblock_2 = 2
                           CASE (2)
                              IF ( L(keywd,1)<L(keywd,2) ) THEN
                              ELSEIF ( L(keywd,1)==L(keywd,2) ) THEN
                                 IF ( key2==2 ) THEN
                                    IF ( L(keywd+1,1)>L(keywd+1,2) ) THEN
                                       spag_nextblock_2 = 4
                                       CYCLE SPAG_DispatchLoop_2
                                    ENDIF
                                 ENDIF
                              ELSE
                                 spag_nextblock_2 = 4
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                              spag_nextblock_2 = 3
                           CASE (3)
                              DO
                                 CALL write(out,L,Nwds,0)
                                 CALL read(*220,*24,in1,L,Nwds,0,if1)
                                 IF ( if2>0 ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                              ENDDO
                              spag_nextblock_2 = 4
                           CASE (4)
                              DO
                                 CALL write(out,L(1,2),Nwds,0)
                                 CALL read(*240,*26,in2,L(1,2),Nwds,0,if2)
                                 IF ( if1>0 ) THEN
                                    spag_nextblock_2 = 2
                                    CYCLE SPAG_DispatchLoop_2
                                 ENDIF
                              ENDDO
 24                           IF ( if2>0 ) THEN
                                 spag_nextblock_2 = 4
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                              spag_nextblock_2 = 5
                              CYCLE SPAG_DispatchLoop_2
 26                           IF ( if1>0 ) THEN
                                 spag_nextblock_2 = 3
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                              spag_nextblock_2 = 5
                           CASE (5)
                              CALL write(out,0,0,1)
                              EXIT SPAG_DispatchLoop_2
                           END SELECT
                        ENDDO SPAG_DispatchLoop_2
                     ENDDO
                     dist2 = dist1 - dist2
                     dist1 = dist1 - dist2
                     IF ( dist2==0 ) last = 1
                     CALL close(in1,last)
                     CALL close(in2,1)
                     CALL close(out,1)
                  ENDDO
                  EXIT SPAG_Loop_1_3
               ELSE
                  dist1 = dist1 + dist2
                  dist2 = dist1 - dist2
                  total = dist1 + dist2
                  level = level + 1
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
!
!     COPY PHASE ---
!     IF OUTPUT FILE IS NOT SPECIFIED, NFILE(6) WILL CONTAIN NAME OF
!     SCRATCH FILE CONTAINING OUTPUT
!
         nfile(6) = out
         IF ( Outfl==0 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL open(*140,out,L(bufa,1),0)
         IF ( Inpfl==Outfl ) THEN
            CALL close(Inpfl,1)
            CALL open(*160,Inpfl,L(bufin,1),1)
         ENDIF
         DO
            CALL read(*260,*40,out,L,nz,0,nflag)
            CALL write(Outfl,L,nz,0)
         ENDDO
 40      CALL write(Outfl,L,nflag,1)
         CALL close(out,1)
         spag_nextblock_1 = 12
      CASE (12)
         RETURN
      CASE (13)
!
!     ERRORS
!
         j = -8
         file = 0
         CALL mesage(j,file,subr)
         RETURN
 60      file = scra
         j = -1
         CALL mesage(j,file,subr)
         RETURN
 80      file = scrb
         j = -1
         CALL mesage(j,file,subr)
         RETURN
 100     file = in1
         j = -1
         CALL mesage(j,file,subr)
         RETURN
 120     file = in2
         j = -1
         CALL mesage(j,file,subr)
         RETURN
 140     file = out
         j = -1
         CALL mesage(j,file,subr)
         RETURN
 160     file = Inpfl
         j = -1
         CALL mesage(j,file,subr)
         RETURN
 180     file = Inpfl
         j = -2
         CALL mesage(j,file,subr)
         RETURN
 200     file = scra
         j = -2
         CALL mesage(j,file,subr)
         RETURN
 220     file = in1
         j = -2
         CALL mesage(j,file,subr)
         RETURN
 240     file = in2
         j = -2
         CALL mesage(j,file,subr)
         RETURN
 260     file = out
         j = -2
         CALL mesage(j,file,subr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE sorti
