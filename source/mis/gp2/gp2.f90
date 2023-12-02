!*==gp2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp2
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_setup
   USE c_system
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(34) :: b
   INTEGER :: buf1 , buf2 , buf3 , file , i , igrid , ii , ijk , ijk1 , j , k , khi , klo , kn , l , lx , m , mm , n , n1 , name ,  &
            & ncore , nogeo2 , nogo , nread , ns , nud , nui , nz , ret , ret1
   INTEGER , SAVE :: cbar , cbeam , ect , eqexin , geom2
   REAL :: flag
   INTEGER , DIMENSION(2) , SAVE :: genel , gp2h
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL close , delset , fname , fwdrec , korsz , mesage , open , preloc , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GP2 BUILDS THE ELEMENT CONNECTION TABLE (ECT).
!     STRUCTURAL ELEMENT CONNECTION CARDS ARE ON GEOM2.
!     EACH EXTERNAL GRID PT. NO. IS CONVERTED TO AN INTERNAL INDEX.
!     IN ADDITION, GENERAL ELEMENT CARDS ARE READ AND
!     EXTERNAL GRID NUMBERS ARE CONVERTED TO INTERNAL NUMBERS.
!
!
   !>>>>EQUIVALENCE (geomp,geom2)
!
!     INPUT  DATA FILES
   DATA geom2 , eqexin/101 , 102/
!
!     OUTPUT DATA FILES
   DATA ect/201/
!
!     MISC   DATA
   DATA gp2h/4HGP2  , 4H    / , cbar/4HBAR / , cbeam/4HBEAM/
!
!     GENEL DATA CARDS PROCESSED BY GP2 IN ADDITION TO ELEMENTS.
   DATA genel/4301 , 43/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     PERFORM GENERAL INITIALIZATION
!
         CALL delset
         buf1 = korsz(z) - sysbuf - 2
         buf2 = buf1 - sysbuf
         noect = -1
         buf3 = buf2 - sysbuf
         mcb(1) = geom2
         CALL rdtrl(mcb)
!
!     READ EQEXIN INTO CORE
!
         file = eqexin
         CALL open(*180,eqexin,z(buf1),rdrew)
         CALL fwdrec(*200,eqexin)
         CALL read(*200,*20,eqexin,z,buf2,1,n)
         CALL mesage(-8,0,gp2h)
 20      CALL close(eqexin,clsrew)
         kn = n/2
         n1 = n + 1
!
!     OPEN GEOM2. IF PURGED, RETURN.
!     OTHERWISE, OPEN ECT AND WRITE HEADER RECORD.
!
         nogeo2 = 0
         CALL preloc(*40,z(buf1),geom2)
         nogeo2 = 1
!
         noect = 1
         nogo = 0
         file = ect
         CALL open(*180,ect,z(buf2),wrtrew)
         CALL fname(ect,b)
         CALL write(ect,b,2,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      RETURN
      CASE (2)
         SPAG_Loop_1_1: DO
!
!     READ 3-WORD ID FROM GEOM2. SEARCH ELEMENT TABLE FOR MATCH.
!     IF FOUND, BRANCH TO ELEMENT CODE. IF NOT FOUND, SEARCH GENEL
!     TABLE  FOR MATCH. IF FOUND BRANCH TO APPROPRIATE CODE. IF NOT
!     FOUND, SKIP RECORD AND CONTINUE.
!
            CALL read(*160,*220,geom2,b,3,0,flag)
            DO i = 1 , last , incr
               IF ( elem(i+3)==b(1) ) EXIT SPAG_Loop_1_1
            ENDDO
            IF ( genel(1)==b(1) ) THEN
               k = (i+1)/2
!
!     GENERAL ELEMENTS-- WRITE 3-WORD ID ON ECT. READ ALL GENELS,
!     CONVERT EXTERNAL GRID NOS. TO INTERNAL NOS. AND WRITE THEM ON ECT.
!
               CALL write(ect,b,3,0)
               file = geom2
               l = 2
               ASSIGN 120 TO ret
               ASSIGN 260 TO ret1
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL fwdrec(*160,geom2)
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     WRITE 3-WORD ID ON ECT. READ ALL CARDS FOR ELEMENT AND
!     CONVERT EXTERNAL GRID NOS. TO INTERNAL NOS.  WRITE ENTRIES ON ECT
!     DIRECTLY AFTER CONVERSION.
!
         ASSIGN 60 TO ret
         ASSIGN 240 TO ret1
         CALL write(ect,b,3,0)
         m = elem(i+5)
         lx = elem(i+12)
         mm = lx + elem(i+9)
         name = elem(i)
         ii = n1
         file = geom2
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*200,*100,file,b,m,0,flag)
!
!     CHECK LATER TO SEE IF RESTRICTION APPLIES TO AXIF PROBLEMS
!
         IF ( iaxif==0 ) THEN
            IF ( nbpw<=32 .AND. b(1)>16777215 ) THEN
               nogo = 1
               CALL mesage(30,138,b)
            ENDIF
         ENDIF
!                                  16777215 = 2**24 - 1
         l = lx
         spag_nextblock_1 = 4
      CASE (4)
         IF ( b(l)/=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      l = l + 1
         IF ( l<mm ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( name==cbeam ) THEN
            IF ( b(8)==0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 80 TO ret
            l = 8
            spag_nextblock_1 = 8
         ELSE
            IF ( name/=cbar ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     SPECIAL PROCESSING FOR BAR AND BEAM ELEMENTS
!
            IF ( b(8)==1 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ASSIGN 80 TO ret
            l = 5
            spag_nextblock_1 = 8
         ENDIF
         CYCLE
 80      ASSIGN 60 TO ret
         spag_nextblock_1 = 5
      CASE (5)
!
         CALL write(ect,b,m,0)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     CURRENT ELEMENT IS COMPLETE
!
 100     CALL write(ect,0,0,1)
         spag_nextblock_1 = 2
      CASE (6)
         ijk = 0
         CALL read(*200,*140,geom2,b,1,0,flag)
         CALL write(ect,b,1,0)
         spag_nextblock_1 = 7
      CASE (7)
         CALL read(*200,*220,geom2,b(2),2,0,flag)
         IF ( b(2)/=-1 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nud = b(3)
         IF ( ijk/=0 ) THEN
            CALL write(ect,b(2),2,0)
            CALL read(*200,*220,geom2,ijk1,1,0,flag)
            CALL write(ect,ijk1,1,0)
            ncore = buf2 - n1
            nz = (nui*(nui+1))/2
            nread = 0
            DO
               n = min0(ncore,nz-nread)
               CALL read(*200,*220,geom2,z(n1),n,0,flag)
               CALL write(ect,z(n1),n,0)
               nread = nread + n
               IF ( nread>=nz ) THEN
                  CALL read(*200,*220,geom2,ijk,1,0,flag)
                  CALL write(ect,ijk,1,0)
                  IF ( ijk==0 ) THEN
                     spag_nextblock_1 = 6
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  ns = nui*nud
                  nread = 0
                  DO
                     n = min0(ncore,ns-nread)
                     CALL read(*200,*220,geom2,z(n1),n,0,flag)
                     CALL write(ect,z(n1),n,0)
                     nread = nread + n
                     IF ( nread>=ns ) THEN
                        spag_nextblock_1 = 6
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ELSE
            nui = b(3)
            ijk = 1
         ENDIF
 120     CALL write(ect,b(2),2,0)
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     CALL write(ect,0,0,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     CLOSE FILES, WRITE TRAILER AND RETURN.
!
 160     CALL close(geom2,clsrew)
         CALL close(ect,clsrew)
         mcb(1) = geom2
         CALL rdtrl(mcb)
         mcb(1) = ect
         CALL wrttrl(mcb)
         IF ( nogo/=0 ) CALL mesage(-61,0,0)
         RETURN
      CASE (8)
!
!
!     INTERNAL BINARY SEARCH ROUTINE
!     ==============================
!
         klo = 1
         khi = kn
         igrid = b(l)
         spag_nextblock_1 = 9
      CASE (9)
         k = (klo+khi+1)/2
         DO
            IF ( igrid<z(2*k-1) ) THEN
               khi = k
            ELSEIF ( igrid==z(2*k-1) ) THEN
               b(l) = z(2*k)
               GOTO ret
            ELSE
               klo = k
            ENDIF
            IF ( khi-klo<1 ) THEN
               GOTO ret1
            ELSEIF ( khi-klo==1 ) THEN
               IF ( k==klo ) THEN
                  k = khi
               ELSE
                  k = klo
               ENDIF
               klo = khi
            ELSE
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
!
!     FATAL ERROR MESSAGES
!
 180     j = -1
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 200     j = -2
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 220     j = -3
         spag_nextblock_1 = 10
      CASE (10)
         CALL mesage(j,file,gp2h)
 240     k = 7
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 260     k = 61
         spag_nextblock_1 = 11
      CASE (11)
         b(2) = igrid
         CALL mesage(30,k,b)
         nogo = 1
         GOTO ret
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gp2
