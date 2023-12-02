!*==aspro.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE aspro(Dmap,Var,Nvar,Obits,Sol)
   IMPLICIT NONE
   USE C_ASDBD
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(18,1) :: Dmap
   INTEGER , DIMENSION(3,200) :: Var
   INTEGER :: Nvar
   INTEGER :: Obits
   INTEGER :: Sol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: alter , ast , blank , oball , slas
   INTEGER , DIMENSION(2,50) :: dbs
   INTEGER :: flag , i , i3 , icd , icol , ikeep , j , key , kopt , l , m , nc , nchar , ndbs , nnew , nold , nxdel , vword
   INTEGER , DIMENSION(2) :: ii
   INTEGER , DIMENSION(4) :: name
   INTEGER , DIMENSION(3,50) :: oct
   INTEGER , DIMENSION(7,200) :: ptbs
   INTEGER , DIMENSION(40) , SAVE :: rfmask
   LOGICAL :: rmv , rmvall
   EXTERNAL andf , pull , push
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS CODE  PERFORMS THE  ROUTINE PROCESSING OF THE  DMAP ALTERS
!     FOR ASDMAP.  KEY TABLES ARE-
!
!      DMAP  -  RAW  18 WORD PER CARD BCD DATA ON INPUT, VARIABLE
!               CHARACTERS ARE ADDED AND  FIELDS AND CARDS ARE DELETED
!               DEPENDING ON USER INPUT IN  VAR(IABLE) AND OPTION FLAGS.
!
!      VAR      CONTROL DATA AND USER INPUT DATA, 3 WORDS, BCD + DATA
!
!      PTBS     POSITIONS-TO-BE-SET TABLE, CONTENTS-PER ENTRY
!
!                    1   CARD NUMBER IN DMAP
!                    2   FIRST CHARACTER OF MODIFIED FIELD
!                    3   FIRST CHARACTER FOR ADDED VARIABLE
!                    4   NUMBER OF VARIABLE CHARACTERS
!                    5   KEY OF VARIABLE TO BE INSERTED
!                    6   MATRIX OPTION FLAG , 1= K, 2=M, 4=P  ETC
!                    7   OUTPUT CONTROL FLAG, AVOIDS SAME DATA BLOCK
!                        OUTPUT FROM TWO MODULES
!
!      OCT      OPTIONAL CARDS TABLE - EACH ENTRY =
!                   DMAP CARD NO. , DELETE BITS ,  KEEP BITS
!
!      OBITS -  BITS ARE ON FOR REQUIRED MATRICES  =  SUM OF NUMBERS
!                   K=1 , M=2 , P=4 , PA=8 , B=16 , K4=32
!
   !>>>>EQUIVALENCE (Ndbs,Sbd(1)) , (Dbs(1,1),Sbd(2))
   DATA alter/4HALTE/ , blank/4H    /
   DATA ast/4H*   / , slas/4H/   /
   DATA rfmask/65536 , 131072 , 262144 , 0 , 0 , 0 , 0 , 524288 , 1048576 , 31*0/
   DATA oball/63/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         rmvall = .TRUE.
         nxdel = 0
         nold = 0
!
!     DELETE CARDS USING OCT TABLE.
!
         IF ( Noct/=0 ) THEN
            m = Ioct - 1
            DO j = 1 , Noct
               DO i = 1 , 3
                  m = m + 1
                  oct(i,j) = Idat(m)
               ENDDO
            ENDDO
            DO i = 1 , Noct
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     icd = oct(1,i)
                     IF ( oct(3,i)/=0 ) THEN
                        IF ( andf(oct(3,i),Obits)==0 ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDIF
                     IF ( andf(oct(2,i),rfmask(Sol))==0 ) CYCLE
                     spag_nextblock_2 = 2
                  CASE (2)
                     Dmap(1,icd) = -1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
         ENDIF
         IF ( Nptbs/=0 ) THEN
            m = Iptbs - 1
            DO j = 1 , Nptbs
               DO i = 1 , 7
                  m = m + 1
                  ptbs(i,j) = Idat(m)
               ENDDO
            ENDDO
            SPAG_Loop_1_2: DO i = 1 , Nptbs
               spag_nextblock_3 = 1
               SPAG_DispatchLoop_3: DO
                  SELECT CASE (spag_nextblock_3)
                  CASE (1)
                     icd = ptbs(1,i)
                     IF ( Dmap(1,icd)==-1 ) CYCLE
                     IF ( icd==0 ) CYCLE
                     rmv = .FALSE.
!
!     CHECK IF  OPTION IS ON
!
                     kopt = ptbs(6,i)
                     IF ( andf(kopt,Obits)==0 ) rmv = .TRUE.
                     IF ( andf(kopt,oball)==0 ) rmv = .FALSE.
                     IF ( andf(kopt,rfmask(Sol))/=0 ) rmv = .TRUE.
                     nchar = ptbs(4,i)
                     nc = 0
                     flag = 0
                     vword = blank
                     icol = ptbs(3,i)
!
!     FIND  VARIABLE  IF  REQUIRED
!
                     IF ( .NOT.(rmv) ) THEN
                        key = ptbs(5,i)
                        i3 = Nvar/3
                        DO j = 1 , i3
                           IF ( Var(1,j)==key ) THEN
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                           IF ( key==j .AND. Var(1,j)==alter ) THEN
                              spag_nextblock_3 = 4
                              CYCLE SPAG_DispatchLoop_3
                           ENDIF
                        ENDDO
!
!     VARIABLE HAS NOT BEEN SET
!
                        vword = blank
                        rmv = .TRUE.
                     ENDIF
                     spag_nextblock_3 = 3
                     CYCLE SPAG_DispatchLoop_3
                  CASE (2)
!
!     VARIABLE  IS FOUND , IT IS IN VAR(2,J) AND/OR VAR(3,J)
!
                     vword = Var(2,j)
                     name(1) = Var(2,j)
                     name(2) = Var(3,j)
!
!     TEST FOR REAL OR INTEGER
!
                     IF ( vword/=0 ) THEN
                        IF ( vword==-1 ) THEN
!
!     WORD IS AN INTEGER
!
                           name(1) = name(2)
                           name(2) = 0
                           flag = 1
                        ELSEIF ( vword==-2 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
!
!     WORD IS REAL (TEMPORARY ERROR)
!
                        ENDIF
                        IF ( ptbs(7,i)==0 ) THEN
                           spag_nextblock_3 = 5
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        nc = ptbs(3,i) - ptbs(2,i)
                        ii(1) = blank
                        ii(2) = blank
                        IF ( nc>0 ) THEN
!
!     CONSTRUCT WHOLE DATA BLOCK NAME
!
                           CALL pull(Dmap(1,icd),ii,ptbs(2,i),nc,0)
                           CALL push(name,ii,nc+1,nchar,flag)
                        ELSE
                           IF ( nc<0 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           ii(1) = name(1)
                           ii(2) = name(2)
                        ENDIF
!
!     CHECK OUTPUT DATA BLOCKS AGAINST PREVIOUS OUTPUTS
!
                        IF ( ndbs/=0 ) THEN
!
                           DO l = 1 , ndbs
                              IF ( ii(1)==dbs(1,l) .AND. ii(2)==dbs(2,l) ) GOTO 2
                           ENDDO
                        ENDIF
                        IF ( ptbs(7,i)>0 ) THEN
!
!     DATA BLOCK IS OUTPUT, REMOVE IF ALLREADY DEFINED.
!
                           rmv = .TRUE.
                           spag_nextblock_3 = 3
                           CYCLE SPAG_DispatchLoop_3
                        ELSE
!
!     VARIABLE IS OK , ADD NAME TO LIST
!
                           ndbs = ndbs + 1
                           dbs(1,ndbs) = ii(1)
                           dbs(2,ndbs) = ii(2)
                           spag_nextblock_3 = 5
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
 2                      IF ( ptbs(7,i)>0 ) THEN
                           spag_nextblock_3 = 5
                           CYCLE SPAG_DispatchLoop_3
                        ENDIF
                        rmv = .TRUE.
                     ENDIF
                     spag_nextblock_3 = 3
                  CASE (3)
!
!     REMOVE WHOLE  NAME HERE  , CHECK FOR PARAMETER
!
                     ii(1) = 0
                     name(1) = blank
                     name(2) = blank
                     name(3) = blank
                     name(4) = blank
                     flag = 0
                     CALL pull(Dmap(1,icd),ii,ptbs(2,i),1,0)
                     IF ( ii(1)/=slas .AND. ii(1)/=ast ) THEN
                        icol = ptbs(2,i)
                        nchar = nchar + ptbs(3,i) - ptbs(2,i)
                     ENDIF
                     spag_nextblock_3 = 5
                     CYCLE SPAG_DispatchLoop_3
                  CASE (4)
!
!     CHECK IF ALTER CARD, OUTPUT AS BCD AND TWO INTEGERS
!
                     Dmap(1,icd) = alter
                     Dmap(2,icd) = Var(2,j)
                     Dmap(3,icd) = Var(3,j)
                     rmvall = .FALSE.
                     nxdel = 0
                     IF ( Var(2,j)==0 ) rmvall = .TRUE.
                     spag_nextblock_3 = 6
                     CYCLE SPAG_DispatchLoop_3
                  CASE (5)
!
!     ADD VARIABLES TO BCD DMAP
!
                     CALL push(name,Dmap(1,icd),icol,nchar,flag)
!
                     IF ( .NOT.rmv ) rmvall = .FALSE.
!
!     IF ALL VARIABLES ARE REMOVED FROM ONE CARD, DELETE THE CARD
!
                     nnew = ptbs(1,i+1)
                     IF ( icd==nnew ) CYCLE
                     SPAG_Loop_2_1: DO
!
!     NEXT  COMMAND GOES TO NEW CARD,  CHECK IF CONTINUATION
!
                        CALL pull(Dmap(1,icd+1),ii,1,4,0)
!
                        IF ( ii(1)/=blank ) EXIT SPAG_Loop_2_1
!
!     CONTINUATION FOUND
!
                        nxdel = nxdel + 1
                        IF ( nnew==icd+1 ) CYCLE SPAG_Loop_1_2
                        icd = icd + 1
                     ENDDO SPAG_Loop_2_1
                     spag_nextblock_3 = 6
                  CASE (6)
!
!     FINISHED WITH  LOGICAL CARD
!
                     IF ( rmvall ) THEN
                        Dmap(1,icd) = -1
                        IF ( nxdel<=0 ) CYCLE
                        DO l = 1 , nxdel
                           j = icd - l
                           Dmap(1,j) = -1
                        ENDDO
                     ENDIF
                     rmvall = .TRUE.
!
!     END OF LOOP ON VARIABLE CHARACTERS
!
                     nxdel = 0
                     EXIT SPAG_DispatchLoop_3
                  END SELECT
               ENDDO SPAG_DispatchLoop_3
!
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     PROCESS CARDS TO BE DELETED FROM SEQUENCE
!
         ikeep = 0
         DO icd = 1 , Nrdm
!
            IF ( Dmap(1,icd)/=-1 ) THEN
!
!     KEEP CARD
!
               ikeep = ikeep + 1
               DO j = 1 , 18
                  Dmap(j,ikeep) = Dmap(j,icd)
               ENDDO
            ENDIF
         ENDDO
         Nrdm = ikeep
         RETURN
      CASE (2)
         WRITE (Iout,99001) Sfm , Dmap(1,icd)
99001    FORMAT (A25,' 6010, ILLEGAL VARIABLE TO BE SET IN DMAP STATEMENT',3X,A4)
!
         Nogo = 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE aspro
