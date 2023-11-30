
SUBROUTINE aspro(Dmap,Var,Nvar,Obits,Sol)
   IMPLICIT NONE
   INTEGER Dbs(2,50) , Idat(1) , Idum1 , Ioct , Iout , Iph , Iptbs , Irdm , Ixtra , Ndbs , Noct , Nogo , Nph , Nptbs , Nrdm , Nxtra
   REAL Sbd(2)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /asdbd / Irdm , Nrdm , Ixtra , Nxtra , Ioct , Noct , Iptbs , Nptbs , Iph , Nph , Idat
   COMMON /system/ Idum1 , Iout , Nogo
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Sbd
   INTEGER Nvar , Obits , Sol
   INTEGER Dmap(18,1) , Var(3,200)
   INTEGER alter , ast , blank , flag , i , i3 , icd , icol , ii(2) , ikeep , j , key , kopt , l , m , name(4) , nc , nchar , nnew ,&
         & nold , nxdel , oball , oct(3,50) , ptbs(7,200) , rfmask(40) , slas , vword
   INTEGER andf
   LOGICAL rmv , rmvall
   EXTERNAL andf
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
         icd = oct(1,i)
         IF ( oct(3,i)/=0 ) THEN
            IF ( andf(oct(3,i),Obits)==0 ) GOTO 20
         ENDIF
         IF ( andf(oct(2,i),rfmask(Sol))==0 ) CYCLE
 20      Dmap(1,icd) = -1
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
      DO i = 1 , Nptbs
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
               IF ( Var(1,j)==key ) GOTO 40
               IF ( key==j .AND. Var(1,j)==alter ) GOTO 80
            ENDDO
!
!     VARIABLE HAS NOT BEEN SET
!
            vword = blank
            rmv = .TRUE.
         ENDIF
         GOTO 60
!
!     VARIABLE  IS FOUND , IT IS IN VAR(2,J) AND/OR VAR(3,J)
!
 40      vword = Var(2,j)
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
               GOTO 200
!
!     WORD IS REAL (TEMPORARY ERROR)
!
            ENDIF
            IF ( ptbs(7,i)==0 ) GOTO 100
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
               IF ( nc<0 ) GOTO 200
               ii(1) = name(1)
               ii(2) = name(2)
            ENDIF
!
!     CHECK OUTPUT DATA BLOCKS AGAINST PREVIOUS OUTPUTS
!
            IF ( Ndbs/=0 ) THEN
!
               DO l = 1 , Ndbs
                  IF ( ii(1)==Dbs(1,l) .AND. ii(2)==Dbs(2,l) ) GOTO 50
               ENDDO
            ENDIF
            IF ( ptbs(7,i)>0 ) THEN
!
!     DATA BLOCK IS OUTPUT, REMOVE IF ALLREADY DEFINED.
!
               rmv = .TRUE.
               GOTO 60
            ELSE
!
!     VARIABLE IS OK , ADD NAME TO LIST
!
               Ndbs = Ndbs + 1
               Dbs(1,Ndbs) = ii(1)
               Dbs(2,Ndbs) = ii(2)
               GOTO 100
            ENDIF
 50         IF ( ptbs(7,i)>0 ) GOTO 100
            rmv = .TRUE.
         ENDIF
!
!     REMOVE WHOLE  NAME HERE  , CHECK FOR PARAMETER
!
 60      ii(1) = 0
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
         GOTO 100
!
!     CHECK IF ALTER CARD, OUTPUT AS BCD AND TWO INTEGERS
!
 80      Dmap(1,icd) = alter
         Dmap(2,icd) = Var(2,j)
         Dmap(3,icd) = Var(3,j)
         rmvall = .FALSE.
         nxdel = 0
         IF ( Var(2,j)==0 ) rmvall = .TRUE.
         GOTO 120
!
!     ADD VARIABLES TO BCD DMAP
!
 100     CALL push(name,Dmap(1,icd),icol,nchar,flag)
!
         IF ( .NOT.rmv ) rmvall = .FALSE.
!
!     IF ALL VARIABLES ARE REMOVED FROM ONE CARD, DELETE THE CARD
!
         nnew = ptbs(1,i+1)
         IF ( icd==nnew ) CYCLE
         DO
!
!     NEXT  COMMAND GOES TO NEW CARD,  CHECK IF CONTINUATION
!
            CALL pull(Dmap(1,icd+1),ii,1,4,0)
!
            IF ( ii(1)/=blank ) EXIT
!
!     CONTINUATION FOUND
!
            nxdel = nxdel + 1
            IF ( nnew==icd+1 ) GOTO 150
            icd = icd + 1
         ENDDO
!
!     FINISHED WITH  LOGICAL CARD
!
 120     IF ( rmvall ) THEN
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
!
 150  ENDDO
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
 200  WRITE (Iout,99001) Sfm , Dmap(1,icd)
99001 FORMAT (A25,' 6010, ILLEGAL VARIABLE TO BE SET IN DMAP STATEMENT',3X,A4)
!
   Nogo = 1
END SUBROUTINE aspro