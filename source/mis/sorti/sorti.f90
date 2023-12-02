!*==sorti.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sorti(Inpfl,Outfl,Nwds,Keywrd,L,Nx)
   IMPLICIT NONE
   USE C_SETUP
   USE C_SYSTEM
   USE C_TWO
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
!
   key2 = 1
   GOTO 100
!
!
   ENTRY sorti2(Inpfl,Outfl,Nwds,Keywrd,L,Nx)
!     ==========================================
!
   key2 = 2
!
!     IF INPFL EQ 0, CORE BLOCK L OF LENGTH NX IS TO BE SORTED
!     IF INPFL NE 0, INPFL IS TO BE SORTED USING BLOCK L
!
 100  keywd = iabs(Keywrd)
   nnn = Nx
   IF ( nnn<Nwds ) GOTO 1300
   j = 30
   IF ( Nbpw>=60 ) j = 62
   two31 = 2**j
   IF ( Inpfl/=0 ) THEN
      bufa = Nx - Sysbuf + 1
!
!     MINIMUM CORE REQUIREMENT = 2 X NUMBER OF WORDS PER ENTRY
!
      nz = bufa - 1
      IF ( nz<Nwds+Nwds ) GOTO 1400
      CALL open(*1500,scra,L(bufa,1),1)
      nn = (nz/Nwds)*Nwds
      nnn = nn
      out = scra
      nrec = 0
      CALL read(*2100,*1000,Inpfl,L,nn,0,nnn)
   ENDIF
!
!     SORT PHASE --
!
 200  len = nnn/Nwds
   IF ( len*Nwds/=nnn ) THEN
      j = -37
      CALL mesage(j,file,subr)
      GOTO 99999
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
         IF ( len>=Two(16) .AND. Nbpw<=32 ) GOTO 700
         limit = (two31-len)/len
         DO i = 1 , len
            j = L(keywd,i)
            IF ( iabs(j)>limit ) GOTO 600
            j = j*len + i
            k = -1
            IF ( j<0 ) k = -len
            L(keywd,i) = j + k
         ENDDO
         IF ( key2/=1 ) THEN
            DO i = 1 , len
               j = L(keywd+1,i)
               IF ( iabs(j)>limit ) GOTO 500
               j = j*len + i
               k = -1
               IF ( j<0 ) k = -len
               L(keywd+1,i) = j + k
            ENDDO
         ENDIF
      ENDIF
   ENDIF
!
!     SORT BY
!     MODIFIED SHELL METHOD, A SUPER FAST SORTER
!
 300  m = m/2
   IF ( m==0 ) THEN
      IF ( Keywrd<0 ) THEN
         DO i = 1 , len
            L(keywd,i) = L(keywd,i)/len
            IF ( key2==2 ) L(keywd+1,i) = L(keywd+1,i)/len
         ENDDO
      ENDIF
      GOTO 800
   ELSE
      j = 1
      k = len - m
      i = j
   ENDIF
 400  DO
      n = i + m
      IF ( L(keywd,i)<L(keywd,n) ) EXIT
      IF ( L(keywd,i)==L(keywd,n) ) THEN
         IF ( key2==1 ) EXIT
         IF ( L(keywd+1,i)<=L(keywd+1,n) ) EXIT
      ENDIF
      DO r = 1 , Nwds
         temp = L(r,i)
         L(r,i) = L(r,n)
         L(r,n) = temp
      ENDDO
      i = i - m
      IF ( i<1 ) EXIT
   ENDDO
   j = j + 1
   IF ( j>k ) GOTO 300
   i = j
   GOTO 400
!
!     SORT BY
!     SHUTTLE EXCHANGE METHOD, A SLOW SORTER
!     (THIS WAS NASTRAN ORIGINAL SORTER, MODIFIED FOR 2-D ARRAY
!     OPERATION WITH 20-COLUMN LIMITATION REMOVED)
!
 500  IF ( i>1 ) THEN
      j = i - 1
      DO i = 1 , j
         L(keywd+1,i) = L(keywd+1,i)/len
      ENDDO
   ENDIF
   i = len
 600  IF ( i>1 ) THEN
      j = i - 1
      DO i = 1 , j
         L(keywd,i) = L(keywd,i)/len
      ENDDO
   ENDIF
!
 700  DO ii = 2 , len
      jj = ii - 1
      IF ( L(keywd,ii)<L(keywd,jj) ) THEN
      ELSEIF ( L(keywd,ii)==L(keywd,jj) ) THEN
         IF ( key2==1 ) CYCLE
         IF ( L(keywd+1,ii)>=L(keywd+1,jj) ) CYCLE
      ELSE
         CYCLE
      ENDIF
      DO
         jj = jj - 1
         IF ( jj<=0 ) EXIT
         IF ( L(keywd,ii)<L(keywd,jj) ) THEN
         ELSEIF ( L(keywd,ii)==L(keywd,jj) ) THEN
            IF ( key2/=2 ) EXIT
            IF ( L(keywd+1,ii)>=L(keywd+1,jj) ) EXIT
         ELSE
            EXIT
         ENDIF
      ENDDO
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
!
!     IF CORE SORT, SORT IS COMPLETED. IF FILE SORT, WRITE BLOCK ON
!     SCRATCH FILE TO BE MERGED LATER.
!
 800  IF ( Inpfl==0 ) GOTO 1300
 900  CALL write(scra,L,nnn,1)
   nrec = nrec + 1
   IF ( nnn/=nn ) GOTO 1100
   CALL read(*2100,*1000,Inpfl,L,nn,0,nnn)
   GOTO 200
 1000 IF ( nnn>0 ) THEN
      IF ( nnn-Nwds<Nwds ) GOTO 900
      GOTO 200
   ENDIF
 1100 CALL close(scra,1)
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
      DO
         dummy = total - nrec
         IF ( dummy>=0 ) THEN
            bufb = bufa - Sysbuf
            bufc = bufb - Sysbuf
            IF ( bufc<1 ) GOTO 1400
            nn = bufb - 1
!
!     COPY N SORTED RECORDS ONTO SECOND SCRATCH FILE
!
            CALL open(*1500,scra,L(bufa,1),0)
            CALL open(*1600,scrb,L(bufb,1),1)
            n = dist2 - dummy
            DO i = 1 , n
               DO
                  CALL read(*2200,*1105,scra,L,nn,0,nflag)
                  CALL write(scrb,L,nn,0)
               ENDDO
 1105          CALL write(scrb,L,nflag,1)
            ENDDO
            CALL close(scrb,1)
            CALL close(scra,2)
            Nfile(4) = scrb
            Nfile(5) = scrc
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
               in1 = Nfile(k)
               in2 = Nfile(k+1)
               out = Nfile(k+2)
               last = 2
               CALL open(*1700,in1,L(bufa,1),2)
               CALL open(*1800,in2,L(bufb,1),2)
               CALL open(*1900,out,L(bufc,1),1)
               DO j = 1 , dist2
                  if1 = Nwds
                  if2 = Nwds
                  CALL read(*2300,*1112,in1,L,Nwds,0,if1)
                  IF ( dummy<=0 ) THEN
                     CALL read(*2400,*1114,in2,L(1,2),Nwds,0,if2)
                  ELSE
                     dummy = dummy - 1
                     if2 = 0
                     GOTO 1114
                  ENDIF
 1106             IF ( L(keywd,1)<L(keywd,2) ) THEN
                  ELSEIF ( L(keywd,1)==L(keywd,2) ) THEN
                     IF ( key2==2 ) THEN
                        IF ( L(keywd+1,1)>L(keywd+1,2) ) GOTO 1110
                     ENDIF
                  ELSE
                     GOTO 1110
                  ENDIF
 1108             DO
                     CALL write(out,L,Nwds,0)
                     CALL read(*2300,*1112,in1,L,Nwds,0,if1)
                     IF ( if2>0 ) GOTO 1106
                  ENDDO
 1110             DO
                     CALL write(out,L(1,2),Nwds,0)
                     CALL read(*2400,*1114,in2,L(1,2),Nwds,0,if2)
                     IF ( if1>0 ) GOTO 1106
                  ENDDO
 1112             IF ( if2<=0 ) GOTO 1116
                  GOTO 1110
 1114             IF ( if1>0 ) GOTO 1108
 1116             CALL write(out,0,0,1)
               ENDDO
               dist2 = dist1 - dist2
               dist1 = dist1 - dist2
               IF ( dist2==0 ) last = 1
               CALL close(in1,last)
               CALL close(in2,1)
               CALL close(out,1)
            ENDDO
            EXIT
         ELSE
            dist1 = dist1 + dist2
            dist2 = dist1 - dist2
            total = dist1 + dist2
            level = level + 1
         ENDIF
      ENDDO
   ENDIF
!
!     COPY PHASE ---
!     IF OUTPUT FILE IS NOT SPECIFIED, NFILE(6) WILL CONTAIN NAME OF
!     SCRATCH FILE CONTAINING OUTPUT
!
   Nfile(6) = out
   IF ( Outfl==0 ) GOTO 1300
   CALL open(*1900,out,L(bufa,1),0)
   IF ( Inpfl==Outfl ) THEN
      CALL close(Inpfl,1)
      CALL open(*2000,Inpfl,L(Bufin,1),1)
   ENDIF
   DO
      CALL read(*2500,*1200,out,L,nz,0,nflag)
      CALL write(Outfl,L,nz,0)
   ENDDO
 1200 CALL write(Outfl,L,nflag,1)
   CALL close(out,1)
 1300 RETURN
!
!     ERRORS
!
 1400 j = -8
   file = 0
   CALL mesage(j,file,subr)
   GOTO 99999
 1500 file = scra
   j = -1
   CALL mesage(j,file,subr)
   GOTO 99999
 1600 file = scrb
   j = -1
   CALL mesage(j,file,subr)
   GOTO 99999
 1700 file = in1
   j = -1
   CALL mesage(j,file,subr)
   GOTO 99999
 1800 file = in2
   j = -1
   CALL mesage(j,file,subr)
   GOTO 99999
 1900 file = out
   j = -1
   CALL mesage(j,file,subr)
   GOTO 99999
 2000 file = Inpfl
   j = -1
   CALL mesage(j,file,subr)
   GOTO 99999
 2100 file = Inpfl
   j = -2
   CALL mesage(j,file,subr)
   GOTO 99999
 2200 file = scra
   j = -2
   CALL mesage(j,file,subr)
   GOTO 99999
 2300 file = in1
   j = -2
   CALL mesage(j,file,subr)
   GOTO 99999
 2400 file = in2
   j = -2
   CALL mesage(j,file,subr)
   GOTO 99999
 2500 file = out
   j = -2
   CALL mesage(j,file,subr)
99999 END SUBROUTINE sorti
