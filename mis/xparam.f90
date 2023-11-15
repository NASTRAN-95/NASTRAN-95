
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
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bcdcnt , Bufsz , Core(1) , Dmap(1) , Dmpcnt , Dmppnt , Iallon , Ichar , Icold , Icrdtp , Idmpnt , Iequl , Insert ,       &
         & Irturn , Isavdw , Iseqn , Isgnon , Islsh , Ldmap , Length , Lmpl , Loscar , Maskhi , Masklo , Masks(1) , Modflg ,        &
         & Modidx , Mpl(1) , Mplpnt , Nbegin , Nblank , Nbpc , Nchkpt , Ncond , Ncpw , Ndiag , Ndmap , Nend , Nequiv , Nestm1 ,     &
         & Nestm2 , Newcrd , Nexit , Njump , Nogo , Nosgn , Noutpt , Npurge , Nrept , Nsave , Nsol , Ntime , Nwords , Nwpc ,        &
         & Nxequi , Optape , Os(5) , Osbot , Oscar(1) , Ospnt , Osprc , Pvt(2) , Savnam(100) , Vps(2) , Xxgpid(8)
   COMMON /autosm/ Nwords , Savnam
   COMMON /system/ Bufsz , Optape , Nogo
   COMMON /xgpi2 / Lmpl , Mplpnt , Mpl
   COMMON /xgpi3 / Pvt
   COMMON /xgpi4 / Irturn , Insert , Iseqn , Dmpcnt , Idmpnt , Dmppnt , Bcdcnt , Length , Icrdtp , Ichar , Newcrd , Modidx , Ldmap ,&
                 & Isavdw , Dmap
   COMMON /xgpic / Icold , Islsh , Iequl , Nblank , Nxequi , Ndiag , Nsol , Ndmap , Nestm1 , Nestm2 , Nexit , Nbegin , Nend ,       &
                 & Njump , Ncond , Nrept , Ntime , Nsave , Noutpt , Nchkpt , Npurge , Nequiv , Ncpw , Nbpc , Nwpc , Maskhi ,        &
                 & Masklo , Isgnon , Nosgn , Iallon , Masks
   COMMON /xgpid / Xxgpid , Modflg
   COMMON /xvps  / Vps
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER and , or
   INTEGER andf , lshift , orf , rshift
   INTEGER endcrd , i , iastk , ic , idmap , impl , in , index(2,2) , iospnt , iprvop , ipval , ipvt , is , isave , isyntx(4,5) ,   &
         & itmp(7) , itype(6) , iv , ival , iy , j , k , l , m , mplbot , mplln , name , newtyp , none , nospnt , nvps , nvstbl(4,4)
   EXTERNAL andf , lshift , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (Core(1),Os(1),Loscar) , (Os(2),Osprc) , (Os(3),Osbot) , (Os(4),Ospnt) , (Os(5),Oscar(1))
!
   DATA index/1 , 3 , 2 , 4/ , isyntx/3*1 , 8 , 3*2 , 7 , 3*3 , 5 , 4*4 , 4*6/ , nvstbl/1 , 1 , 3 , 3 , 1 , 1 , 4 , 4 , 1 , 1 , 4 , &
      & 4 , 1 , 2 , 4 , 2/ , itype/1 , 1 , 2 , 2 , 2 , 4/ , ic/4HC   / , iv/4HV   / , iy/4HY   / , in/4HN   / , nvps/4HVPS / ,      &
       &is/4HS   / , iastk/4H*   / , name/1/ , ival/2/ , none/1/ , impl/2/ , idmap/3/ , ipvt/4/
!
!     INITIALIZE
!
   or(i,j) = orf(i,j)
   and(i,j) = andf(i,j)
   endcrd = 0
   iprvop = Islsh
   nospnt = Oscar(Ospnt) + Ospnt
   iospnt = nospnt + 1
   Oscar(nospnt) = 0
   mplbot = Mpl(Mplpnt-7) + Mplpnt - 7
!
!     GET FIRST/NEXT TYPE AND MODIFY CODES FROM DMAP,CHECK FOR $
!
 100  newtyp = 0
   isave = 0
   DO
      CALL xscndm
      IF ( Irturn==1 ) THEN
!
!     PROCESS POSSIBLE DELIMITERS - SLASH AND ASTERISK
!
         IF ( Dmap(Dmppnt+1)==iastk ) THEN
            CALL xscndm
            IF ( Irturn==1 .OR. Irturn==3 ) THEN
               CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
               GOTO 1200
            ELSEIF ( Irturn==4 ) THEN
               GOTO 1000
            ELSEIF ( Irturn==5 ) THEN
               GOTO 1500
            ELSE
               i = 2
               j = 2
               newtyp = 1
               Oscar(nospnt) = Oscar(nospnt) + 1
               EXIT
            ENDIF
!
!     PROCESS MPL DEFAULTS IF // IS ENCOUNTERED
!
         ELSEIF ( Dmap(Dmppnt+1)/=Islsh ) THEN
!
!     ERROR MESSAGES -
!
!     DMAP CARD FORMAT ERROR
!
            CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
            GOTO 1200
         ELSE
            i = 2
            j = 2
            newtyp = 1
            Oscar(nospnt) = Oscar(nospnt) + 1
            EXIT
         ENDIF
      ELSEIF ( Irturn==3 ) THEN
!
!     PROCESS ANY INTEGER, REAL, OR COMPLEX CONSTANTS
!
         i = 2
         j = 2
         newtyp = 1
         Oscar(nospnt) = Oscar(nospnt) + 1
         EXIT
      ELSEIF ( Irturn==4 ) THEN
         GOTO 1000
      ELSEIF ( Irturn==5 ) THEN
         GOTO 1500
      ELSEIF ( Dmap(Dmppnt)/=Nblank ) THEN
         Oscar(nospnt) = 1 + Oscar(nospnt)
         j = Dmap(Dmppnt)
         IF ( j/=ic .AND. j/=iv .AND. j/=is ) THEN
!
!     PROCESS V,N,NAME PARAMETER TYPES AS /NAME/
!
            i = 1
            j = 2
            newtyp = 1
            EXIT
         ELSE
            IF ( j==is ) isave = 1
            i = 1
            IF ( j==ic ) i = 2
            CALL xscndm
            IF ( Irturn==1 .OR. Irturn==3 .OR. Irturn==4 ) THEN
               CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
               GOTO 1200
            ELSEIF ( Irturn==5 ) THEN
               GOTO 1500
            ELSE
               k = Dmap(Dmppnt)
               IF ( k/=iy .AND. k/=in ) THEN
                  CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
                  GOTO 1200
               ELSE
                  j = 1
                  IF ( k==in .OR. k==is ) j = 2
                  EXIT
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
!
!     USE I AND J TO OBTAIN ROW INDEX FOR SYNTAX TABLE.
!
   i = index(i,j)
!
!     INITIALIZE IPVAL,AND ITMP WITH MPL DATA
!
   IF ( Mplpnt>=mplbot ) THEN
!
!     TOO MANY PARAMETERS IN DMAP PARAMETER LIST.
!
      CALL xgpidg(18,Ospnt,0,0)
      GOTO 1200
   ELSE
      DO k = 1 , 7
         itmp(k) = 0
      ENDDO
      itmp(3) = iabs(Mpl(Mplpnt))
!
!     CONVERT PARAMETER TYPE CODE TO WORD LENGTH
!
      k = itmp(3)
      mplln = itype(k)
      ipval = none
      IF ( Mpl(Mplpnt)>=0 ) THEN
         DO k = 1 , mplln
            Mplpnt = Mplpnt + 1
            itmp(k+3) = Mpl(Mplpnt)
         ENDDO
         ipval = impl
      ENDIF
      Mplpnt = Mplpnt + 1
      IF ( newtyp==1 ) THEN
         IF ( Irturn==1 ) THEN
!
!     USE DEFAULT MPL VALUE FOR PARAMETER
!
            IF ( ipval==none ) THEN
               CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
               GOTO 1200
            ELSE
               Oscar(iospnt) = mplln
               DO m = 1 , mplln
                  j = iospnt + m
                  Oscar(j) = itmp(m+3)
               ENDDO
               iospnt = iospnt + mplln + 1
               GOTO 100
            ENDIF
         ELSEIF ( Irturn==2 ) THEN
            GOTO 300
         ELSEIF ( Irturn==3 ) THEN
            j = 3
            GOTO 400
         ELSEIF ( Irturn==4 ) THEN
            j = 5
            GOTO 400
         ELSEIF ( Irturn==5 ) THEN
            GOTO 1500
         ENDIF
      ENDIF
   ENDIF
 200  DO
!
!     SCAN DMAP FOR PARAMETER NAME AND VALUE IF ANY, AND CODE DMAP ENTRY
!     FOR USE AS COLUMN INDEX IN SYNTAX TABLE.
!
      CALL xscndm
      IF ( Irturn==2 ) EXIT
      IF ( Irturn==3 ) THEN
         j = 3
         GOTO 400
      ELSEIF ( Irturn==4 ) THEN
         j = 5
         GOTO 400
      ELSEIF ( Irturn==5 ) THEN
         GOTO 1500
      ELSEIF ( Dmap(Dmppnt+1)/=Iequl .AND. Dmap(Dmppnt+1)/=Islsh .AND. Dmap(Dmppnt+1)/=iastk ) THEN
         CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
         GOTO 1200
      ELSEIF ( Dmap(Dmppnt+1)/=iastk ) THEN
         j = 2
         IF ( Dmap(Dmppnt+1)==Islsh ) j = 4
         GOTO 400
      ENDIF
   ENDDO
 300  j = 1
!
!     CHECK FOR BLANK
!
   IF ( Dmap(Dmppnt)==Nblank ) GOTO 200
!
!     BRANCH ON SYNTAX TABLE VALUE
!
 400  k = isyntx(i,j)
   IF ( k==2 ) THEN
!
!     DMAP ENTRY IS = OPERATOR
!
      IF ( iprvop/=name ) THEN
         CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
         GOTO 1200
      ELSE
         iprvop = Iequl
         GOTO 200
      ENDIF
   ELSEIF ( k==3 ) THEN
      GOTO 500
   ELSEIF ( k==4 ) THEN
!
!     DMAP ENTRY IS / OPERATOR
!
      IF ( iprvop==Iequl ) THEN
         CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
         GOTO 1200
      ELSE
         iprvop = Islsh
         GOTO 700
      ENDIF
   ELSEIF ( k==5 ) THEN
!
!     DMAP ENTRY IS BINARY VALUE
!
      IF ( iprvop/=Islsh ) GOTO 500
      GOTO 600
   ELSEIF ( k==6 ) THEN
!
!     END OF DMAP INSTRUCTION
!
      IF ( iprvop/=Iequl ) GOTO 700
      CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
      GOTO 1200
   ELSEIF ( k==7 ) THEN
      CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
      GOTO 1200
   ELSEIF ( k/=8 ) THEN
!
!     NAME FOUND. NAME TO TEMP,UPDATE PREVOP AND SEARCH PVT FOR VALUE.
!
      IF ( iprvop/=Iequl ) THEN
         IF ( iprvop/=Islsh ) THEN
            CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
            GOTO 1200
         ELSE
            itmp(1) = Dmap(Dmppnt)
            itmp(2) = Dmap(Dmppnt+1)
            iprvop = name
!
!     SCAN PVT
            k = 3
            DO
               l = andf(Pvt(k+2),Nosgn)
               l = itype(l)
               IF ( Dmap(Dmppnt)==Pvt(k) .AND. Dmap(Dmppnt+1)==Pvt(k+1) ) THEN
!
!     CHECK LENGTH OF PVT VALUE
!
                  ipval = ipvt
                  Pvt(k+2) = orf(Pvt(k+2),Isgnon)
                  IF ( andf(Pvt(k+2),Nosgn)/=itmp(3) ) THEN
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
                        itmp(m+3) = Pvt(j)
                     ENDDO
                  ENDIF
                  GOTO 200
               ELSE
                  k = k + 3 + l
                  IF ( k>=Pvt(2) ) GOTO 200
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
      CALL xgpidg(6,Ospnt,Oscar(nospnt),0)
      GOTO 200
   ELSE
      Length = 2
      Dmppnt = Dmppnt - 1
      Dmap(Dmppnt) = itmp(3)
      GOTO 600
   ENDIF
 500  IF ( iprvop/=Iequl ) THEN
      CALL xgpidg(3,Ospnt,Oscar(nospnt),0)
      GOTO 1200
   ENDIF
 600  iprvop = ival
   IF ( ipval/=ipvt ) THEN
!
!     DMAP VALUE IS HIGHEST PRIORITY
!
      ipval = idmap
      IF ( andf(Dmap(Dmppnt),Nosgn)/=itmp(3) ) THEN
         CALL xgpidg(6,Ospnt,Oscar(nospnt),0)
      ELSE
!
! TRANSFER DMAP VALUE TO ITMP
!
         DO m = 1 , Length
            j = Dmppnt + m
            itmp(m+3) = Dmap(j)
         ENDDO
      ENDIF
   ENDIF
   GOTO 200
!
!     PARAMETER SCANNED,CHECK CORRECTNESS OF NAME AND VALUE AND
!     PROCESS ITMP ACCORDING TO NVSTBL
!
 700  IF ( i<4 .AND. itmp(1)==0 ) THEN
!
!     DMAP PARAMETER NAME MISSING
!
      CALL xgpidg(7,Ospnt,Oscar(nospnt),0)
      GOTO 900
   ELSE
      k = nvstbl(i,ipval)
!
      IF ( k==2 ) THEN
!
!     ILLEGAL PARA CARD
!
         CALL xgpidg(8,0,itmp(1),itmp(2))
         IF ( i>2 ) GOTO 900
!
!     VARIABLE PARAMETER,VALUE TO VPS,POINTER TO OSCAR
!
         k = 3
      ELSEIF ( k==3 ) THEN
         GOTO 1400
      ELSEIF ( k==4 ) THEN
         GOTO 900
      ELSE
         k = 3
      ENDIF
      DO WHILE ( itmp(1)/=Vps(k) .OR. itmp(2)/=Vps(k+1) )
         k = k + and(Vps(k+2),Maskhi) + 3
         IF ( k>=Vps(2) ) THEN
!
!     NAME NOT IN VPS,MAKE NEW ENTRY
!
            k = Vps(2) + 1
            Vps(2) = k + 2 + mplln
            IF ( Vps(2)<=Vps(1) ) GOTO 750
!
!     VPS TABLE OVERFLOW
!
            CALL xgpidg(14,nvps,Nblank,Dmpcnt)
            GOTO 1500
         ENDIF
      ENDDO
!
!     PARAMETER IS ALREADY IN VPS - MAKE SURE TYPES AGREE.
!
      l = andf(rshift(Vps(k+2),16),15)
      IF ( l/=0 ) THEN
         IF ( l/=andf(itmp(3),15) ) THEN
!
!     INCONSISTENT LENGTH USED FOR VARIABLE PARAMETER.
!
            CALL xgpidg(15,Ospnt,itmp(1),itmp(2))
            GOTO 800
         ENDIF
      ENDIF
!
!     CHECK VALUE MODIFIED FLAG
!
!
!     VALUE HAS BEEN MODIFIED FOR RESTART - DO NOT CHANGE.
!
      IF ( andf(Modflg,Vps(k+2))/=0 ) GOTO 800
!
!     CHECK IF PREVIOUSLY DEFINED
!
      IF ( Vps(k+2)<0 ) THEN
!
!     WARNING - PARAMETER ALREADY HAD VALUE ASSIGNED PREVIOUSLY
!
         IF ( ipval==idmap ) CALL xgpidg(-42,Ospnt,itmp(1),itmp(2))
         IF ( and(rshift(Vps(k+2),16),15)/=and(itmp(3),15) ) CALL xgpidg(15,Ospnt,itmp(1),itmp(2))
         GOTO 800
      ENDIF
!
!     ITMP NAME,LENGTH,FLAG,VALUE TO VPS
!
 750  l = mplln + 3
      DO m = 1 , l
         j = k + m - 1
         Vps(j) = itmp(m)
      ENDDO
      Vps(k+2) = or(mplln,lshift(itmp(3),16))
      IF ( ipval==idmap ) Vps(k+2) = or(Vps(k+2),Isgnon)
   ENDIF
!
!     LOCATION OF VALUE IN VPS TO OSCAR
!
 800  Oscar(iospnt) = k + 3
   IF ( isave==1 ) THEN
      Nwords = Nwords + 1
      Savnam(Nwords) = k + 3
   ENDIF
   Oscar(iospnt) = or(Oscar(iospnt),Isgnon)
   iospnt = iospnt + 1
   GOTO 100
!
!     CONSTANT PARAMETER,VALUE TO OSCAR
!
 900  Oscar(iospnt) = mplln
   DO m = 1 , mplln
      j = iospnt + m
      Oscar(j) = itmp(m+3)
   ENDDO
   iospnt = iospnt + mplln + 1
   GOTO 100
!
!     ALL PARAMETERS ON DMAP CARD PROCESSED,PROCESS ANY REMAINING ON
!     MPL
!
 1000 IF ( Mplpnt>=mplbot ) GOTO 1200
   endcrd = 1
   Length = iabs(Mpl(Mplpnt))
   Length = itype(Length)
   Oscar(nospnt) = 1 + Oscar(nospnt)
   IF ( Mpl(Mplpnt)<0 ) GOTO 1400
   IF ( Mpl(Mplpnt)==0 ) THEN
!
!     MPL PARAMETER ERROR
!
      CALL xgpidg(4,Ospnt,Oscar(nospnt),0)
      GOTO 1200
   ELSE
      Oscar(iospnt) = Length
      DO m = 1 , Length
         j = iospnt + m
         Mplpnt = Mplpnt + 1
         Oscar(j) = Mpl(Mplpnt)
      ENDDO
   ENDIF
 1100 Mplpnt = Mplpnt + 1
   iospnt = iospnt + Length + 1
   GOTO 1000
!
!     RETURN TO XOSGEN
!
 1200 Oscar(Ospnt) = iospnt - Ospnt
   Irturn = 1
 1300 RETURN
!
!     CONSTANT PARAMETER NOT DEFINED
!
 1400 CALL xgpidg(9,Ospnt,Oscar(nospnt),0)
   IF ( endcrd/=1 ) GOTO 900
   GOTO 1100
 1500 Nogo = 2
   Irturn = 2
   GOTO 1300
END SUBROUTINE xparam
