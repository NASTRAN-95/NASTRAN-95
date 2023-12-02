!*==xparam.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xparam
!
!     THE PURPOSE OF XPARAM IS TO GENERATE THE PARAMETER SECTION OF AN
!     OSCAR ENTRY,AND TO GENERATE THE VPS TABLE.
!
!          ... DESCRIPTION OF PROGRAM VARIABLES ...
!     ITMP   = TEMPORARY STORAGE FOR PARAMETER NAME AND VALUE.
!     IPVAL  = HIGHEST PRIORITY NOMINAL VALUE IN ITMP.
!     IPRVOP = PREVIOUS OPERATOR OR OPERAND RECEIVED FROM DMAP.
!     INDEX  = TABLE CONTAINING ROW INDEXES FOR ISYNTX TABLE.
!     ISYNTX = SYNTAX TABLE USED TO PROCESS DMAP PARAMETER LIST.
!     NVSTBL = NOMINAL VALUE SOURCE TABLE.
!     NOSPNT = POINTER TO PARAMETER COUNT IN PARAMETER SECTION OF OSCAR.
!     IOSPNT = POINTER TO NEXT AVAILABLE WORD IN OSCAR.
!     ENDCRD = END OF CARD FLAG
!     MPLLN  = LENGTH(IN WORDS) OF MPL PARAMETER VALUE
!     ITYPE  = TABLE FOR TRANSLATING NUMBER TYPE CODES TO WORD LENGTH.
!     ENDCRD = FLAG INDICATING END OF CARD SENSED.
!
!     RETURN CODES FROM XSCNDM
!
!        1  DELIMITOR
!        2  BCD
!        3  VALUE
!        4  END OF CARD
!        5  ERROR ENCOUNTERED
!
   USE c_autosm
   USE c_system
   USE c_xgpi2
   USE c_xgpi3
   USE c_xgpi4
   USE c_xgpic
   USE c_xgpid
   USE c_xvps
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: and , or
   INTEGER :: endcrd , i , iospnt , iprvop , ipval , isave , j , k , l , m , mplbot , mplln , newtyp , nospnt , ospnt
   INTEGER , SAVE :: iastk , ic , idmap , impl , in , ipvt , is , iv , ival , iy , name , none , nvps
   INTEGER , DIMENSION(2,2) , SAVE :: index
   INTEGER , DIMENSION(4,5) , SAVE :: isyntx
   INTEGER , DIMENSION(7) :: itmp
   INTEGER , DIMENSION(6) , SAVE :: itype
   INTEGER , DIMENSION(4,4) , SAVE :: nvstbl
   INTEGER , DIMENSION(1) :: oscar
   EXTERNAL andf , lshift , orf , rshift , xgpidg , xscndm
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Os(5),Oscar(1))
!
   DATA index/1 , 3 , 2 , 4/ , isyntx/3*1 , 8 , 3*2 , 7 , 3*3 , 5 , 4*4 , 4*6/ , nvstbl/1 , 1 , 3 , 3 , 1 , 1 , 4 , 4 , 1 , 1 , 4 , &
      & 4 , 1 , 2 , 4 , 2/ , itype/1 , 1 , 2 , 2 , 2 , 4/ , ic/4HC   / , iv/4HV   / , iy/4HY   / , in/4HN   / , nvps/4HVPS / ,      &
       &is/4HS   / , iastk/4H*   / , name/1/ , ival/2/ , none/1/ , impl/2/ , idmap/3/ , ipvt/4/
!
!     INITIALIZE
!
   or(i,j) = orf(i,j)
   and(i,j) = andf(i,j)
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         endcrd = 0
         iprvop = islsh
         nospnt = oscar(ospnt) + ospnt
         iospnt = nospnt + 1
         oscar(nospnt) = 0
         mplbot = mpl(mplpnt-7) + mplpnt - 7
         spag_nextblock_1 = 2
      CASE (2)
!
!     GET FIRST/NEXT TYPE AND MODIFY CODES FROM DMAP,CHECK FOR $
!
         newtyp = 0
         isave = 0
         SPAG_Loop_1_1: DO
            CALL xscndm
            IF ( irturn==1 ) THEN
!
!     PROCESS POSSIBLE DELIMITERS - SLASH AND ASTERISK
!
               IF ( dmap(dmppnt+1)==iastk ) THEN
                  CALL xscndm
                  IF ( irturn==1 .OR. irturn==3 ) THEN
                     CALL xgpidg(3,ospnt,oscar(nospnt),0)
                     spag_nextblock_1 = 13
                  ELSEIF ( irturn==4 ) THEN
                     spag_nextblock_1 = 11
                  ELSEIF ( irturn==5 ) THEN
                     spag_nextblock_1 = 16
                  ELSE
                     i = 2
                     j = 2
                     newtyp = 1
                     oscar(nospnt) = oscar(nospnt) + 1
                     EXIT SPAG_Loop_1_1
                  ENDIF
!
!     PROCESS MPL DEFAULTS IF // IS ENCOUNTERED
!
               ELSEIF ( dmap(dmppnt+1)/=islsh ) THEN
!
!     ERROR MESSAGES -
!
!     DMAP CARD FORMAT ERROR
!
                  CALL xgpidg(3,ospnt,oscar(nospnt),0)
                  spag_nextblock_1 = 13
               ELSE
                  i = 2
                  j = 2
                  newtyp = 1
                  oscar(nospnt) = oscar(nospnt) + 1
                  EXIT SPAG_Loop_1_1
               ENDIF
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( irturn==3 ) THEN
!
!     PROCESS ANY INTEGER, REAL, OR COMPLEX CONSTANTS
!
               i = 2
               j = 2
               newtyp = 1
               oscar(nospnt) = oscar(nospnt) + 1
               EXIT SPAG_Loop_1_1
            ELSEIF ( irturn==4 ) THEN
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( irturn==5 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( dmap(dmppnt)/=nblank ) THEN
               oscar(nospnt) = 1 + oscar(nospnt)
               j = dmap(dmppnt)
               IF ( j/=ic .AND. j/=iv .AND. j/=is ) THEN
!
!     PROCESS V,N,NAME PARAMETER TYPES AS /NAME/
!
                  i = 1
                  j = 2
                  newtyp = 1
                  EXIT SPAG_Loop_1_1
               ELSE
                  IF ( j==is ) isave = 1
                  i = 1
                  IF ( j==ic ) i = 2
                  CALL xscndm
                  IF ( irturn==1 .OR. irturn==3 .OR. irturn==4 ) THEN
                     CALL xgpidg(3,ospnt,oscar(nospnt),0)
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( irturn==5 ) THEN
                     spag_nextblock_1 = 16
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     k = dmap(dmppnt)
                     IF ( k/=iy .AND. k/=in ) THEN
                        CALL xgpidg(3,ospnt,oscar(nospnt),0)
                        spag_nextblock_1 = 13
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        j = 1
                        IF ( k==in .OR. k==is ) j = 2
                        EXIT SPAG_Loop_1_1
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     USE I AND J TO OBTAIN ROW INDEX FOR SYNTAX TABLE.
!
         i = index(i,j)
!
!     INITIALIZE IPVAL,AND ITMP WITH MPL DATA
!
         IF ( mplpnt>=mplbot ) THEN
!
!     TOO MANY PARAMETERS IN DMAP PARAMETER LIST.
!
            CALL xgpidg(18,ospnt,0,0)
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO k = 1 , 7
               itmp(k) = 0
            ENDDO
            itmp(3) = iabs(mpl(mplpnt))
!
!     CONVERT PARAMETER TYPE CODE TO WORD LENGTH
!
            k = itmp(3)
            mplln = itype(k)
            ipval = none
            IF ( mpl(mplpnt)>=0 ) THEN
               DO k = 1 , mplln
                  mplpnt = mplpnt + 1
                  itmp(k+3) = mpl(mplpnt)
               ENDDO
               ipval = impl
            ENDIF
            mplpnt = mplpnt + 1
            IF ( newtyp==1 ) THEN
               IF ( irturn==1 ) THEN
!
!     USE DEFAULT MPL VALUE FOR PARAMETER
!
                  IF ( ipval==none ) THEN
                     CALL xgpidg(3,ospnt,oscar(nospnt),0)
                     spag_nextblock_1 = 13
                  ELSE
                     oscar(iospnt) = mplln
                     DO m = 1 , mplln
                        j = iospnt + m
                        oscar(j) = itmp(m+3)
                     ENDDO
                     iospnt = iospnt + mplln + 1
                     spag_nextblock_1 = 2
                  ENDIF
                  CYCLE
               ELSEIF ( irturn==2 ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( irturn==3 ) THEN
                  j = 3
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( irturn==4 ) THEN
                  j = 5
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( irturn==5 ) THEN
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_2: DO
!
!     SCAN DMAP FOR PARAMETER NAME AND VALUE IF ANY, AND CODE DMAP ENTRY
!     FOR USE AS COLUMN INDEX IN SYNTAX TABLE.
!
            CALL xscndm
            IF ( irturn==2 ) EXIT SPAG_Loop_1_2
            IF ( irturn==3 ) THEN
               j = 3
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( irturn==4 ) THEN
               j = 5
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( irturn==5 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( dmap(dmppnt+1)/=iequl .AND. dmap(dmppnt+1)/=islsh .AND. dmap(dmppnt+1)/=iastk ) THEN
               CALL xgpidg(3,ospnt,oscar(nospnt),0)
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( dmap(dmppnt+1)/=iastk ) THEN
               j = 2
               IF ( dmap(dmppnt+1)==islsh ) j = 4
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 4
      CASE (4)
         j = 1
!
!     CHECK FOR BLANK
!
         IF ( dmap(dmppnt)==nblank ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
!
!     BRANCH ON SYNTAX TABLE VALUE
!
         k = isyntx(i,j)
         IF ( k==2 ) THEN
!
!     DMAP ENTRY IS = OPERATOR
!
            IF ( iprvop/=name ) THEN
               CALL xgpidg(3,ospnt,oscar(nospnt),0)
               spag_nextblock_1 = 13
            ELSE
               iprvop = iequl
               spag_nextblock_1 = 3
            ENDIF
            CYCLE
         ELSEIF ( k==3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( k==4 ) THEN
!
!     DMAP ENTRY IS / OPERATOR
!
            IF ( iprvop==iequl ) THEN
               CALL xgpidg(3,ospnt,oscar(nospnt),0)
               spag_nextblock_1 = 13
            ELSE
               iprvop = islsh
               spag_nextblock_1 = 8
            ENDIF
            CYCLE
         ELSEIF ( k==5 ) THEN
!
!     DMAP ENTRY IS BINARY VALUE
!
            IF ( iprvop==islsh ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( k==6 ) THEN
!
!     END OF DMAP INSTRUCTION
!
            IF ( iprvop/=iequl ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL xgpidg(3,ospnt,oscar(nospnt),0)
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( k==7 ) THEN
            CALL xgpidg(3,ospnt,oscar(nospnt),0)
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( k/=8 ) THEN
!
!     NAME FOUND. NAME TO TEMP,UPDATE PREVOP AND SEARCH PVT FOR VALUE.
!
            IF ( iprvop/=iequl ) THEN
               IF ( iprvop/=islsh ) THEN
                  CALL xgpidg(3,ospnt,oscar(nospnt),0)
                  spag_nextblock_1 = 13
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  itmp(1) = dmap(dmppnt)
                  itmp(2) = dmap(dmppnt+1)
                  iprvop = name
!
!     SCAN PVT
                  k = 3
                  DO
                     l = andf(pvt(k+2),nosgn)
                     l = itype(l)
                     IF ( dmap(dmppnt)==pvt(k) .AND. dmap(dmppnt+1)==pvt(k+1) ) THEN
!
!     CHECK LENGTH OF PVT VALUE
!
                        ipval = ipvt
                        pvt(k+2) = orf(pvt(k+2),isgnon)
                        IF ( andf(pvt(k+2),nosgn)/=itmp(3) ) THEN
!
!     PARA CARD ERROR
!
                           CALL xgpidg(5,0,itmp(1),itmp(2))
                        ELSE
!
!     TRANSFER VALUE TO ITMP
!
                           DO m = 1 , l
                              j = k + m + 2
                              itmp(m+3) = pvt(j)
                           ENDDO
                        ENDIF
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        k = k + 3 + l
                        IF ( k>=pvt(2) ) THEN
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
!
!     BCD PARAMETER VALUE FOUND
!
         IF ( itmp(3)/=3 ) THEN
!
!     ILLEGAL DMAP PARAMETER VALUE
!
            CALL xgpidg(6,ospnt,oscar(nospnt),0)
            spag_nextblock_1 = 3
         ELSE
            length = 2
            dmppnt = dmppnt - 1
            dmap(dmppnt) = itmp(3)
            spag_nextblock_1 = 7
         ENDIF
      CASE (6)
         IF ( iprvop/=iequl ) THEN
            CALL xgpidg(3,ospnt,oscar(nospnt),0)
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         iprvop = ival
         IF ( ipval/=ipvt ) THEN
!
!     DMAP VALUE IS HIGHEST PRIORITY
!
            ipval = idmap
            IF ( andf(dmap(dmppnt),nosgn)/=itmp(3) ) THEN
               CALL xgpidg(6,ospnt,oscar(nospnt),0)
            ELSE
!
! TRANSFER DMAP VALUE TO ITMP
!
               DO m = 1 , length
                  j = dmppnt + m
                  itmp(m+3) = dmap(j)
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 3
      CASE (8)
!
!     PARAMETER SCANNED,CHECK CORRECTNESS OF NAME AND VALUE AND
!     PROCESS ITMP ACCORDING TO NVSTBL
!
         IF ( i<4 .AND. itmp(1)==0 ) THEN
!
!     DMAP PARAMETER NAME MISSING
!
            CALL xgpidg(7,ospnt,oscar(nospnt),0)
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            k = nvstbl(i,ipval)
!
            IF ( k==2 ) THEN
!
!     ILLEGAL PARA CARD
!
               CALL xgpidg(8,0,itmp(1),itmp(2))
               IF ( i>2 ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     VARIABLE PARAMETER,VALUE TO VPS,POINTER TO OSCAR
!
               k = 3
            ELSEIF ( k==3 ) THEN
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( k==4 ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ELSE
               k = 3
            ENDIF
            DO WHILE ( itmp(1)/=vps(k) .OR. itmp(2)/=vps(k+1) )
               k = k + and(vps(k+2),maskhi) + 3
               IF ( k>=vps(2) ) THEN
!
!     NAME NOT IN VPS,MAKE NEW ENTRY
!
                  k = vps(2) + 1
                  vps(2) = k + 2 + mplln
                  IF ( vps(2)<=vps(1) ) GOTO 10
!
!     VPS TABLE OVERFLOW
!
                  CALL xgpidg(14,nvps,nblank,dmpcnt)
                  spag_nextblock_1 = 16
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
!
!     PARAMETER IS ALREADY IN VPS - MAKE SURE TYPES AGREE.
!
            l = andf(rshift(vps(k+2),16),15)
            IF ( l/=0 ) THEN
               IF ( l/=andf(itmp(3),15) ) THEN
!
!     INCONSISTENT LENGTH USED FOR VARIABLE PARAMETER.
!
                  CALL xgpidg(15,ospnt,itmp(1),itmp(2))
                  spag_nextblock_1 = 9
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     CHECK VALUE MODIFIED FLAG
!
!
!     VALUE HAS BEEN MODIFIED FOR RESTART - DO NOT CHANGE.
!
            IF ( andf(modflg,vps(k+2))/=0 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     CHECK IF PREVIOUSLY DEFINED
!
            IF ( vps(k+2)<0 ) THEN
!
!     WARNING - PARAMETER ALREADY HAD VALUE ASSIGNED PREVIOUSLY
!
               IF ( ipval==idmap ) CALL xgpidg(-42,ospnt,itmp(1),itmp(2))
               IF ( and(rshift(vps(k+2),16),15)/=and(itmp(3),15) ) CALL xgpidg(15,ospnt,itmp(1),itmp(2))
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     ITMP NAME,LENGTH,FLAG,VALUE TO VPS
!
 10         l = mplln + 3
            DO m = 1 , l
               j = k + m - 1
               vps(j) = itmp(m)
            ENDDO
            vps(k+2) = or(mplln,lshift(itmp(3),16))
            IF ( ipval==idmap ) vps(k+2) = or(vps(k+2),isgnon)
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
!
!     LOCATION OF VALUE IN VPS TO OSCAR
!
         oscar(iospnt) = k + 3
         IF ( isave==1 ) THEN
            nwords = nwords + 1
            savnam(nwords) = k + 3
         ENDIF
         oscar(iospnt) = or(oscar(iospnt),isgnon)
         iospnt = iospnt + 1
         spag_nextblock_1 = 2
      CASE (10)
!
!     CONSTANT PARAMETER,VALUE TO OSCAR
!
         oscar(iospnt) = mplln
         DO m = 1 , mplln
            j = iospnt + m
            oscar(j) = itmp(m+3)
         ENDDO
         iospnt = iospnt + mplln + 1
         spag_nextblock_1 = 2
      CASE (11)
!
!     ALL PARAMETERS ON DMAP CARD PROCESSED,PROCESS ANY REMAINING ON
!     MPL
!
         IF ( mplpnt>=mplbot ) THEN
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         endcrd = 1
         length = iabs(mpl(mplpnt))
         length = itype(length)
         oscar(nospnt) = 1 + oscar(nospnt)
         IF ( mpl(mplpnt)<0 ) THEN
            spag_nextblock_1 = 15
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( mpl(mplpnt)==0 ) THEN
!
!     MPL PARAMETER ERROR
!
            CALL xgpidg(4,ospnt,oscar(nospnt),0)
            spag_nextblock_1 = 13
            CYCLE SPAG_DispatchLoop_1
         ELSE
            oscar(iospnt) = length
            DO m = 1 , length
               j = iospnt + m
               mplpnt = mplpnt + 1
               oscar(j) = mpl(mplpnt)
            ENDDO
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
         mplpnt = mplpnt + 1
         iospnt = iospnt + length + 1
         spag_nextblock_1 = 11
      CASE (13)
!
!     RETURN TO XOSGEN
!
         oscar(ospnt) = iospnt - ospnt
         irturn = 1
         spag_nextblock_1 = 14
      CASE (14)
         RETURN
      CASE (15)
!
!     CONSTANT PARAMETER NOT DEFINED
!
         CALL xgpidg(9,ospnt,oscar(nospnt),0)
         IF ( endcrd==1 ) THEN
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (16)
         nogo = 2
         irturn = 2
         spag_nextblock_1 = 14
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xparam
