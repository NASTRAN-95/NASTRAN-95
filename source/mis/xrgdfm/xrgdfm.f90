!*==xrgdfm.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xrgdfm(Newsol,Oldsol,Iapp,Iufile,Iopen,Isize,Iscr,Nogo)
   IMPLICIT NONE
   USE C_ALTRXX
   USE C_PHAS11
   USE C_PHAS25
   USE C_PHAS28
   USE C_PHAS31
   USE C_PHAS37
   USE C_SYSTEM
   USE C_TWO
   USE C_XMDMSK
   USE C_XMSSG
   USE C_XRGDXX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(12) :: Newsol
   INTEGER , DIMENSION(12) :: Oldsol
   INTEGER :: Iapp
   INTEGER , DIMENSION(2) :: Iufile
   INTEGER , DIMENSION(100) :: Iopen
   INTEGER :: Isize
   INTEGER :: Iscr
   INTEGER :: Nogo
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: asters , blank , card , coment , dolacr , dolafl , file , maxsol , nas , rfmt , subset
   INTEGER , DIMENSION(4) , SAVE :: filtyp
   INTEGER :: ibit , ifill , index , isol , isubal , ithrml , iword , k , kb , lu , next , nlpp , oldind , oldnum , optape
   INTEGER , DIMENSION(3) :: idate
   INTEGER , DIMENSION(200) :: ioutbf
   INTEGER , DIMENSION(50) , SAVE :: numsol
   INTEGER , DIMENSION(3) , SAVE :: phase
   INTEGER , DIMENSION(20) , SAVE :: solnum
   INTEGER , DIMENSION(2) , SAVE :: sub
   EXTERNAL close , mesage , orf , rfclse , rfopen , write , xrcard , xrgdcf , xrgdtb , xrgsst , xrgsub
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     XRGDFM READS AND PROCESSES RIGID FORMATS
!
!     WRITTEN BY  RPK CORPORATION; DECEMBER, 1983
!
!     INPUT
!       ARGUMENTS
!         IAPP        =1, FOR DMAP APPROACH; =2, DISPLACEMENT APPRAOCH
!                     =3, HEAT APPROACH    ; =4, AERO APPROACH
!         IOPEN       ARRAY FROM OPEN CORE TO CONTAIN THE MODULE
!                     EXECUTION DECISION TABLE
!         ISIZE       NUMBER OF WORDS AVAILABLE IN THE IOPEN ARRAY
!         IUFILE      NAME OF USER'S FILE CONTAINING THE RIGID FORMAT
!         NEWSOL      ARRAY CONTAINING THE SOLUTION NUMBER FOLLOWED
!                     BY ALL SUBSET NUMBERS GIVEN BY THE USER
!         OLDSOL      SOLUTION ON PREVIOUS RUN IF THIS IS A RESTART
!       OTHER
!       /XRGDXX/
!         IRESTR      RESTART FLAG - NON-ZERO IF RUN IS A RESTART
!         NSUBST      NUMBER OF SUBSETS GIVEN BY THE USER
!         RECORD      ARRAY CONTAINING THE CARD IMAGE IN 20A4 FORMAT
!       /SYSTEM/
!         IDATE       ARRAY CONTAINING MONTH AND YEAR OF NASTRAN LEVEL
!         OPTAPE      UNIT USED FOR THE OUTPUT PRINT FILE
!       /TWO/
!         TWO         ARRAY CONTAINING THE VALUES OF THE POWERS OF 2.
!       /MEDMSK/
!         N1          NUMBER OF WORDS USED FOR THE CARD NAME RESTART
!                     TABLE
!         N2          NUMBER OF WORDS USED FOR THE FILE NAME RESTART
!                     TABLE
!         N3          NUMBER OF WORDS USED FOR THE RIGID FORMAT
!                     CHANGE RESTART TABLE
!
!     OUTPUT
!       ARGUMENTS
!         IOPEN       ARRAY CONTAINING THE MODULE EXECUTION DECISION
!                     TABLE
!       OTHER
!         /MEDMSK/
!           MEDMSK    MODULE EXECUTION DECISION MASK - SET IF SOLUTION
!                     CHANGE OCCURRED ON A RESTART
!         /SYSTEM/
!           ITHRML    SET TO NON-ZERO FOR A HEAT APPROACH
!         /PHAS11/
!           IPAS11    ARRAY FOR SUBSTRUCTURE CONTROLS-SET TO ZERO
!         /PHAS25/
!           IPAS25    SAME AS IPAS11
!         /PHAS28/
!           IPAS28    SAME AS IPAS11
!         /PHAS31/
!           IPAS31    SAME AS IPAS11
!         /PHAS37/
!           IPAS37    SAME AS IPAS11
!         /XRGDXX/
!           IDMAP     DMAP SEQUENCE NUMBER
!           IGNORE    FLAG SET TO IGNORE ANY CONTROL CARDS FOR THE
!                     CURRENT DMAP STATEMENT - IS SET WHEN THE DMAP
!                     STATEMENT IS TO BE DELETED BY THE SUBSET
!           IPHASE    PHASE NUMBER ASSOCIATED WITH THE ****PHS-
!                     CONTROL CARD
!           ITYPE     SET TO 'FILE' OR 'CARD' FOR TYPE OF CONTROL CARD
!           LIMIT     LOWER/UPPER LIMITS ASSOCIATED WITH THE VALUES
!                     OF A PARTICULAR CARD TYPE
!           MEMBER    NAME OF USER'S FILE CONTAINING A RIGID FORMAT
!                     THIS IS A 2-WORD ARRAY IN 2A4 FORMAT
!           NUMENT    NUMBER OF WORDS PER ENTRY IN THE MODULE EXECUTION
!                     DECISION TABLE
!
!
!     LOCAL VARIABLES
!       ASTERS        VARIABLE CONTAINING THE VALUE OF 4H****
!       CARD          VARIABLE CONTAINING THE VALUE OF 4HCARD
!       COMENT        VARIABLE CONTAINING THE VALUE OF 4H$$$$
!       DOLACR        VARIABLE CONTAINING THE VALUE OF 4H$*CA
!       DOLAFL        VARIABLE CONTAINING THE VALUE OF 4H$*FI
!       FILE          VARIABLE CONTAINING THE VALUE OF 4HFILE
!       FILTYP        ARRAY CONTAINING ACRONYMS FOR APPROACH
!       IBIT          BIT NUMBER TO SET IN THE MEDMSK
!       IFILL         VALUE TO BE USED TO INITIALIZE THE MODULE
!                     EXECUTION DECISION TABLE; =0, IF RESTART;
!                     =1, OTHERWISE
!       LU            FORTRAN LOGICAL UNIT NUMBER AS RETURN FROM RFOPEN
!                     =0, IF OPEN IS NOT SUCCESSFUL
!       INDEX         INDEX INTO CURRENT ENTRY OF MODULE EXEC.
!                     DECISION TABLE
!       ISOL          SOLUTION NUMBER
!       IWORD         WORD IN MEDMSK TO BE SET FOR RESTART FLAG
!       NEXT          FLAG INDICATING THAT A NEW DMAP STATEMENT IS
!                     TO BE PROCESSED; =0, IF NEW DMAP STATEMENT;
!                     =1, IF PROCESSING THE SAME DMAP STATEMENT
!       NUMSOL        ARRAY CONTAINING THE RESTART BITS ASSOCIATED
!                     WITH A RIGID FORMAT SWITCH DURING RESTART
!       MAXSOL        MAX. SOLUTION NUMBER
!       PHASE         ARRAY CONTAINING 'PHS1', PHS2', AND 'PHS3'
!       RFMT          VARIABLE CONTAINING THE VALUE 4HRFMT
!       SOLNUM        ARRAY CONTAINING THE ALPHA REPRESENTATIONS OF
!                     THE SOLUTION NUMBERS
!
!     FUNCTIONS
!       1. INITIALIZES SUBSTRUCTURE CONTROLS TO ZERO
!       2. CHECKS FOR USER SUPPLIED RIGID FORMAT
!       3. IF STANDARD RIGID FORMAT, VALIDATES SOLUTION NUMBER,
!          SETS MEDMSK IF A RESTART OCCURRED ON A DIFFERENT
!          RIGID FORMAT
!       4. SETS NUMENT=1 AND IFILL=1 IS NO RESTART - OTHERWISE
!          NUMENT=N1+N2+N3 AND IFILL=0
!       5. CALLS RFOPEN TO OPEN THE RIGID FORMAT
!       6. READS A CARD IMAGE FROM THE RIGID FORMAT FILE -
!          THE DATE AND YEAR OF THE RIGID FORMAT IS VALIDATED AGAINST
!          THAT THE LEVEL OF NASTRAN
!          RE-DEFINE NO. OF LINES PER OUTPUT PAGE IF 4TH WORD IS
!          PRESENT, .GT.20 .AND. .LE.99,  NO DATE CHECK IF THE ORD WROD
!          IS ****
!       7. READS A CARD FROM THE RIGID FORMAT FILE AND DOES THE
!          FOLLOWING DEPENDING ON THE TYPE OF CARD READ:
!          - FOR '$$$$' COMMENT CARDS, NEXT IS RESET
!          - FOR '****SBST' CARDS SUBROUTINE XRGSUB IS CALLED
!          - FOR '****CARD' CARDS SUBROUTINE XRGDCF IS CALLED
!          - FOR '****FILE' CARDS SUBROUTINE XRGDCF IS CALLED
!          - FOR '****RFMT' CARDS SUBROUTINE XRGDCF IS CALLED
!          - FOR '****PHS-' CARDS SUBROUTINE XRGSST IS CALLED
!          - OTHERWISE, THE CARD IS A DMAP AND WRITEN TO SCRATCH 315
!          (NOTE- FOR NON RESTARTS, THE ****CARD,****FILE,****RFMT
!          CARDS ARE BYPASSED.  FOR DMAP STATEMENTS THAT ARE
!          DELETED BY SUBSET CONTROLS, NO CONTROL CARDS ARE
!          PROCESSED EXCEPT FOR ****PHS- CARDS UNTIL THE NEXT
!          DMAP STATEMENT IS ENCOUNTERED)
!       8. WHEN A '$*CA' OR A '$*FI' CARD IS READ, PROCESSING OF
!          DMAP STATEMENTS TERMINATES - IF THE JOB IS NOT A RESTART
!          XRGDFM RETURNS.  OTHERWISE, A CHECK IS MADE TO ENSURE
!          THAT THE CARD NAME TABLE IS GIVEN FIRST FOLLOWED BY
!          THE FILE NAME TABLE.  SUBROUTINE XRGDTB IS CALLED TO
!          PROCESS BOTH TABLES.  AFTER THESE TABLES ARE PROCESSED,
!          XRGDFM RETURNS.
!
!     SUBROUTINES CALLED - RFOPEN,READ,WRITE,XRGSUB,XRGDCF,XRGSST,
!                          XRGDTB,MESAGE,RFCLOS
!
!     COMMENTS FROM G.C./UNISYS - ALL THE MACHINE DEPENDENT DSX* SUB-
!     ROUTINES ARE NO LONGER USED. SEE RFOPEN.  10/1990
!
!     CALLING SUBROUTINE - XCSA
!
!     ERROR MESSAGES 8023,504,8025,8026,8024,8037 MAY BE ISSUED
!
   !>>>>EQUIVALENCE (Ksystm(2),Optape) , (Ksystm(56),Ithrml) , (Ksystm(42),Idate(1)) , (Ksystm(69),Isubal) , (Ksystm(9),Nlpp)
   DATA filtyp/4HDMAP , 4HDISP , 4HHEAT , 4HAERO/
   DATA solnum/1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9 , 2H10 , 2H11 , 2H12 , 2H13 , 2H14 , 2H15 , 2H16 , 2H17 , 2H18 , &
       &2H19 , 2H20/
   DATA card/4HCARD/ , file/4HFILE/
   DATA rfmt/4HRFMT/ , blank/4H    /
   DATA asters/4H****/ , coment/4H$$$$/
   DATA subset/4HSBST/ , dolacr/4H$*CA/
   DATA dolafl/4H$*FI/
   DATA phase/4HPHS1 , 4HPHS2 , 4HPHS3/
   DATA sub/4HXRGD , 4HFM  /
   DATA nas/4HNAS / , maxsol/19/
!
!     IN THE FOLLOWING TABLE, VALUES 187-209 ARE FOR STATICS,
!     210-213 ARE FOR HEAT, AND 214-217 ARE FOR AERO -
!     THIS PROVIDES FOR 31 DIFFERENT VALUES IN TOTAL (1 WORD)
!
   DATA numsol/187 , 188 , 189 , 190 , 191 , 192 , 193 , 194 , 195 , 196 , 197 , 198 , 199 , 200 , 201 , 202 , 203 , 204 , 205 ,    &
      & -1 , -1 , -1 , -1 , 210 , -1 , 211 , -1 , -1 , -1 , -1 , -1 , 212 , -1 , -1 , -1 , -1 , -1 , -1 , 216 , 214 , 215 , 9* - 1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
! WAS:
!     DATA    NUMSOL/
!    1                187, 188, 189, 190, 191, 192, 193, 194,
!    2                195, 196, 197, 198, 199, 200, 201, 202,
!    3                 -1,  -1,  -1,  -1, 207,  -1, 208,  -1,
!    4                 -1,  -1,  -1,  -1, 209,  -1,  -1,  -1,
!    5                 -1,  -1,  -1,  -1,  -1,  -1, 216, 214,
!    6                215, 9*-1 /
!
         Iscrx = Iscr
         Idmap = 0
         DO k = 1 , 8
            Ipas11(k) = 0
         ENDDO
         DO k = 1 , 14
            Ipas25(k) = 0
            Ipas28(k) = 0
         ENDDO
         DO k = 1 , 2
            Ipas31(k) = 0
         ENDDO
         DO k = 1 , 6
            Ipas37(k) = 0
         ENDDO
         IF ( Iufile(1)==0 ) THEN
            isol = Newsol(1)
            IF ( Iapp==1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Iapp==3 ) THEN
               ithrml = 1
               isol = isol - 23
               IF ( isol/=1 .AND. isol/=3 .AND. isol/=9 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( Iapp==4 ) THEN
               isol = isol - 30
               IF ( isol/=9 .AND. isol/=10 .AND. isol/=11 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( isol<1 .OR. isol>maxsol ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Member(1) = filtyp(Iapp)
            Member(2) = solnum(isol)
         ELSE
            Member(1) = Iufile(1)
            Member(2) = Iufile(2)
         ENDIF
!
         oldind = Oldsol(1)
         IF ( oldind/=0 .AND. oldind/=Newsol(1) ) THEN
!
!     MAKE SURE CHECKPOINT TAPE FROM OLDER VERSION IS COMPATIBLE WITH
!     NEW CHANGE MADE IN 1991.
!
            IF ( oldind==21 .OR. oldind==23 .OR. oldind==29 ) THEN
               oldind = oldind + 3
               Oldsol(1) = oldind
               IF ( oldind==Newsol(1) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
            oldnum = numsol(oldind)
            IF ( oldnum>0 ) THEN
               iword = ((oldnum-1)/31) + 1
               ibit = oldnum - 31*(iword-1) + 1
               Medmsk(iword) = orf(Medmsk(iword),Two(ibit))
               WRITE (optape,99001) Oldsol(1) , Newsol(1) , oldnum
99001          FORMAT (51H0*** SWITCHED SOLUTION FOR RESTART - OLD SOLUTION =,I4,16H, NEW SOLUTION =,I4,14H, BIT NUMBER =,I4)
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Irestr/=0 ) THEN
            Nument = N1 + N2 + N3
            ifill = 0
         ELSE
            Nument = 1
            ifill = 1
         ENDIF
         Idmap = 0
         DO kb = 1 , Nument
            Iopen(kb) = ifill
         ENDDO
         index = 1 - Nument
         next = 0
         CALL rfopen(Member,lu)
         Ignore = 0
         IF ( lu==0 ) THEN
            Nogo = 3
            CALL rfclse(lu)
            RETURN
         ELSE
            READ (lu,99008,ERR=20,END=40) Record
!
!     BLANK OUT THE 19TH AND 20TH WORDS AS THEY
!     MAY CONTAIN SEQUENCE INFORMATION
!
            Record(19) = blank
            Record(20) = blank
!
!     ALLOW OPTIONS TO CHANGE NLPP LOCALLY, AND NOT TO CHECK RF DATE.
!     (THE NLPP OPTION HERE IS OBSOLETE. CAN BE EASILY DONE VIA NASINFO
!     FILE - 7/90)
!
            IF ( Record(3)/=asters ) THEN
               IF ( Record(2)/=idate(3) ) THEN
                  WRITE (optape,99002) Ufm , idate(1) , idate(3) , Record(1) , Record(2)
99002             FORMAT (A23,' 8037, NASTRAN IS LEVEL ',2A4,' BUT THE RIGID FORMAT DATA BASE IS LEVEL ',2A4)
                  Nogo = 3
                  CALL rfclse(lu)
                  RETURN
               ENDIF
            ENDIF
            SPAG_Loop_1_1: DO
               READ (lu,99008,ERR=20,END=40) Record
!
!     BLANK OUT THE 19TH AND 20TH WORDS AS THEY
!     MAY CONTAIN SEQUENCE INFORMATION
!
               Record(19) = blank
               Record(20) = blank
               IF ( Record(1)/=coment ) THEN
                  IF ( Record(1)==asters ) THEN
                     IF ( Record(2)/=subset ) THEN
                        IF ( Record(2)/=card ) THEN
                           IF ( Record(2)/=file ) THEN
                              IF ( Record(2)/=rfmt ) THEN
                                 DO k = 1 , 3
                                    IF ( Record(2)==phase(k) ) THEN
                                       Iphase = k
                                       CALL xrgsst(Newsol)
                                       IF ( Ierror/=0 ) Nogo = 3
                                       CYCLE SPAG_Loop_1_1
                                    ENDIF
                                 ENDDO
                                 WRITE (optape,99003) Ufm , Record
99003                            FORMAT (A23,' 8026, THE FOLLOWING CARD HAS AN UNIDENTIFIED ','FUNCTION AFTER ',6H'****',//20X,20A4)
                                 Nogo = 3
                              ELSEIF ( Irestr/=0 .AND. Ignore/=1 ) THEN
                                 Limit(1) = (N1+N2)*31 + 1
                                 Limit(2) = (N1+N2+N3)*31
                                 CALL xrgdcf(Iopen(index))
                                 IF ( Ierror/=0 ) Nogo = 3
                              ENDIF
                           ELSEIF ( Irestr/=0 .AND. Ignore/=1 ) THEN
                              Limit(1) = N1*31 + 1
                              Limit(2) = (N1+N2)*31
                              CALL xrgdcf(Iopen(index))
                              IF ( Ierror/=0 ) Nogo = 3
                           ENDIF
                        ELSEIF ( Irestr/=0 .AND. Ignore/=1 ) THEN
                           Limit(1) = 1
                           Limit(2) = N1*31
                           CALL xrgdcf(Iopen(index))
                           IF ( Ierror/=0 ) Nogo = 3
                        ENDIF
                     ELSEIF ( Nsubst/=0 ) THEN
                        CALL xrgsub(Iopen(index),Newsol(2))
                        IF ( Ierror/=0 ) Nogo = 3
                     ENDIF
                  ELSEIF ( Record(1)==dolacr .OR. Record(1)==dolafl ) THEN
                     CALL write(Iscr,0,0,1)
                     IF ( Newalt/=0 ) THEN
                        CALL write(Altfil,0,0,1)
                        CALL close(Altfil,1)
                     ENDIF
                     CALL write(Iscr,Iopen(1),index+Nument-1,1)
                     IF ( Irestr==0 ) THEN
                        CALL rfclse(lu)
                        RETURN
                     ELSE
                        Itype = card
                        IF ( Record(1)/=dolacr ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        Limit(1) = 1
                        Limit(2) = N1*31
                        CALL xrgdtb(lu)
                        IF ( Ierror/=0 ) Nogo = 3
                        Itype = file
                        IF ( Record(1)/=dolafl ) THEN
                           spag_nextblock_1 = 4
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        Limit(1) = N1*31 + 1
                        Limit(2) = (N1+N2)*31
                        CALL xrgdtb(lu)
                        IF ( Ierror/=0 ) Nogo = 3
                        CALL rfclse(lu)
                        RETURN
                     ENDIF
                  ELSE
                     IF ( next/=1 ) THEN
                        IF ( Newalt/=0 ) THEN
                           CALL xrcard(ioutbf,200,Record)
                           CALL write(Altfil,ioutbf(2),2,0)
                        ENDIF
                        next = 1
                        Idmap = Idmap + 1
                        index = index + Nument
                        DO kb = 1 , Nument
                           Iopen(kb+index-1) = ifill
                        ENDDO
                     ENDIF
                     CALL write(Iscr,Record,18,0)
                     Ignore = 0
                  ENDIF
               ELSEIF ( next/=0 ) THEN
                  next = 0
                  IF ( index>Isize ) THEN
                     CALL mesage(-8,0,sub)
                     CALL rfclse(lu)
                     RETURN
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     ERRORS
!
         WRITE (optape,99004) Ufm , isol , filtyp(Iapp)
99004    FORMAT (A23,' 8023, SOLUTION NUMBER',I4,' IS ILLEGAL FOR APPROACH',A4)
 20      WRITE (optape,99005) Ufm , Member
99005    FORMAT (A23,' 8025, READ ERROR ON FILE ',2A4)
         Nogo = 3
         CALL rfclse(lu)
         RETURN
 40      WRITE (optape,99006) Ufm , Member
99006    FORMAT (A23,' 8025, UNEXPECTED EOF ENCOUNTERED ON FILE ',2A4)
         Nogo = 3
         CALL rfclse(lu)
         RETURN
      CASE (4)
         WRITE (optape,99007) Ufm , Itype , Record
99007    FORMAT (A23,' 8024, EXPECTED A ',3H'$*,A4,1H',' CARD.',' INSTEAD THE FOLLOWING CARD IS READ',//20X,20A4)
         Nogo = 3
         CALL rfclse(lu)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99008 FORMAT (20A4)
END SUBROUTINE xrgdfm
