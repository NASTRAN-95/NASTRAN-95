!*==pretab.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pretab(Ditf,Rz,Inz,Buf,Lcrgvn,Lcused,Tabnol,List)
   USE C_CONDAS
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
   IMPLICIT NONE
   REAL Degra , Pi , Radeg , S4pisq , Twopi , z(1)
   INTEGER Ibuf , Iz(1) , Nout
   CHARACTER*23 Ufm
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Iz
   INTEGER Ditf , Lcrgvn , Lcused , Tabid
   REAL X
   REAL Buf(1) , Rz(1) , Y(2)
   INTEGER Inz(1) , List(1) , Tabnol(1)
   COMPLEX a , b , sum , term
   INTEGER clsrw , dit , i , iary(8) , icheck , icrq , idic , idich , idicl , iflag , igoto , ihop , ii , index , itabl , itable ,  &
         & itype , j , jj , jj3 , jlim , jlow , k , khi , klo , kx , kxx , kxx1 , l , lim , limjj , lnth , lx , mn , name(2) ,      &
         & neor , nn , ntabl , nwds , nwdsrd , offset , sctyp , tabido , tabno , tabtyp
   REAL cp , flag , omega , omegax , p , prop , px(2,2) , rp , tr , up , wsq , x1 , x2 , xi , xip1 , xksq , xlu , xo , xx , yi ,    &
      & yip1 , yo
   INTEGER locfx
   LOGICAL part1
   INTEGER :: spag_nextblock_1
!
!     SUBROUTINE PRETAB READS TABLES INTO OPEN CORE, SETS UP TABLE
!     DICTIONARIES WHICH ARE LATER USED WHEN THE CALLING ROUTINE
!     REQUESTS A FUNCTIONAL VALUE FROM A TABLE VIA A CALL TO THE ENTRY
!     POINT TAB.
!
!     REVISED  7/92, BY G.CHAN/UNISYS.
!     1. NEW REFERENCE TO THE OPEN CORE ARRAY SUCH THAT THE SOURCE CODE
!        IS UP TO ANSI FORTRAN 77 STANDARD
!     2. LOGARITHMIC SCALE ENHANCEMENT
!
!     ARGUMENT LIST -
!
!     DITF     THE GINO NAME OF THE FILE ON WHICH THE TABLES RESIDE.
!     RZ       THE OPEN CORE ARRAY. RZ IS USED AS REAL BY THIS ROUTINE.
!     INZ      SAME ADDRESS AS RZ.  USED AS INTEGER IN THIS ROUTINE.
!     BUF      A BUFFER TO BE USED BY SUBROUTINE PRELOC.
!     LCRGVN   THE LENGTH OF OPEN CORE GIVEN TO PRETAB.
!     LCUSED   THE AMOUNT OF CORE USED BY PRETAB.
!     TABNOL   LIST OF TABLE NUMBERS THAT THE USER WILL BE REFERENCING.
!              TABNOL(1) = N IS THE NUMBER OF TABLES TO BE REFERENCED.
!              TABNOL(2),...,TABNOL(N+1) CONTAIN THE TABLE NUMBERS. NOTE
!              THAT 0 IS AN ADMISSIBLE TABLE NUMBER IN THE TABLE NO.
!              LIST.  TABLE NO. 0 DEFINES A FUNCTION WHICH IS IDENTICAL-
!              LY = 0 FOR ALL VALUES OF THE INDEPENDENT VARIABLE.
!     LIST     ARRAY OF CONTROL WORDS FOR SUBROUTINE LOCATE AND TABLE
!              TYPES.
!              LIST(1) = M IS THE NO. OF TRIPLES WHICH FOLLOW IN LIST.
!              THE FIRST TWO WORDS OF EACH TRIPLE ARE THE SUBROUTINE
!              LOCATE CONTROL WORDS AND THE THIRD WORD IS THE TABLE TYPE
!              = 1,2,3,4, OR 5.
!     LNTH     = 12 WORDS PER TABLE ENTRY
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA clsrw , neor , name , px , lnth/1 , 0 , 4HPRET , 4HAB   , 3. , 2. , 1.339 , 1.0 , 12/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         offset = locfx(Inz(1)) - locfx(Iz(1))
         IF ( offset<0 ) CALL errtrc('PRETAB  ',5)
         dit = Ditf
         idic = 0 + offset
         part1 = .TRUE.
         lim = Tabnol(1)
         icrq = lnth*lim - Lcrgvn
         IF ( icrq>=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SET UP TABLE NUMBERS IN DICTIONARY
!
!     FOR EACH TABLE THE DICTIONARY ENTRY IS AS FOLLOWS -
!
!       LOC.  1      TABLE NUMBER
!       LOC.  2      TABLE TYPE(1,2,3, 4, OR 5)
!       LOC.  3      POINTER TO 1ST  ENTRY IN TABLE.
!       LOC.  4      POINTER TO LAST ENTRY IN TABLE.
!       LOC.  5      *
!       LOC.  6      *
!       LOC.  7      *
!       LOC.  8      * LOCATIONS 5 THRU 11 CONTAIN TABLE PARAMETERS.
!       LOC.  9      *
!       LOC. 10      *
!       LOC. 11      *
!       LOC. 12      SCALE TYPE - LINEAR-LINER(0), LOG-LOG(1), LINEAR-
!                                 LOG(2), LOG-LINEAR(3)
!
         DO i = 1 , lim
            Iz(idic+1) = Tabnol(i+1)
            jlow = idic + 2
            jlim = idic + lnth
            DO j = jlow , jlim
               Iz(j) = 0
            ENDDO
            idic = idic + lnth
         ENDDO
         idicl = 1 + offset
         idich = idic
!
!     READ THE CARDS REFERENCED VIA THE TABNOL AND LIST ARRAY.
!
         itable = idic
         CALL preloc(*120,Buf,dit)
         limjj = Tabnol(1)
         lim = List(1)
         jj = 1
         spag_nextblock_1 = 2
      CASE (2)
         jj3 = 3*jj - 1
         CALL locate(*20,Buf,List(jj3),flag)
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_1: DO
!
!     READ 8 WORDS INTO THE ARRAY IARY
!
            CALL read(*140,*20,dit,iary,8,neor,flag)
            tabno = iary(1)
            sctyp = iary(8)
!
!     DETERMINE IF THIS TABLE NUMBER IS IN THE USER SUPPLIED LIST OF
!     TABLE NUMBERS
!
            DO j = 1 , limjj
               IF ( tabno==iabs(Tabnol(j+1)) ) THEN
                  IF ( tabno/=Tabnol(j+1) ) EXIT SPAG_Loop_1_1
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
!
!     THIS TABLE IS NOT CALLED FOR.  READ THE TABLE SERIALLY UNTIL AN
!     END OF TABLE INDICATOR (TWO MINUS ONES FOR TABLE TYPES 1,2,3 AND
!     ONE MINUS ONE FOR TABLE TYPE 4
!
            nwds = 2
            IF ( List(3*jj+1)==4 ) nwds = 1
            DO
               CALL read(*140,*160,dit,iary(2),nwds,neor,iflag)
               IF ( iary(2)==-1 ) CYCLE SPAG_Loop_1_1
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     THERE ARE TWO DIFFERENT TABLES WITH THE SAME NUMBER -- FATAL ERROR
!
         iary(1) = tabno
         iary(2) = List(3*jj-1)
         CALL mesage(-30,88,iary)
         spag_nextblock_1 = 4
      CASE (4)
!
!     THIS IS A NEW TABLE.  SET TABLE NUMBER NEGATIVE AND DEFINE WORDS
!     2 AND 3 OF THE PROPER DICTIONARY ENTRY.
!
         tabtyp = List(3*jj+1)
         Tabnol(j+1) = -Tabnol(j+1)
         index = lnth*(j-1) + offset
         Iz(index+2) = tabtyp
         Iz(index+3) = itable + 1
!
!     READ THE TABLE INTO CORE.
!
         nwdsrd = 2
         IF ( tabtyp==4 ) nwdsrd = 1
         ii = itable + 1
         DO
            CALL read(*140,*160,dit,z(ii),nwdsrd,neor,flag)
            IF ( Iz(ii)==-1 ) THEN
!
!     STORE THE LAST LOCATION OF THE TABLE IN IZ(INDEX+4)
!
               Iz(index+4) = ii - nwdsrd
!
!     STORE THE PARAMETERS ON THE TABLE CARD IN WORDS 5 THRU 11 OF THE
!     PROPER DICTIONARY ENTRY.
!
               lx = index + 4
               DO k = 2 , 8
                  lx = lx + 1
                  Iz(lx) = iary(k)
               ENDDO
               Iz(lx+1) = sctyp
!
!     STORE THE CORRECT 0TH ADDRESS OF THE NEXT TABLE IN ITABLE
!
               itable = Iz(index+4)
!
!     IF THE TABLE IS A POLYNOMIAL EVALUATE THE END POINTS.
!
               IF ( tabtyp/=4 ) THEN
                  itable = itable + 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  l = index + 1
                  xx = (z(l+6)-z(l+4))/z(l+5)
                  ASSIGN 60 TO igoto
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
               ii = ii + nwdsrd
               icrq = ii - Lcrgvn - offset
               IF ( icrq>=0 ) THEN
                  spag_nextblock_1 = 11
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDDO
!
!     TEST TO SEE IF ALL OF THE REQUESTED TABLES HAVE BEEN FOUND. IF
!     ALL TABLES HAVE NOT BEEN FOUND, GO TO NEXT TRIPLE IN LIST ARRAY
!
 20      IF ( jj<lim ) THEN
            DO i = 1 , limjj
               IF ( Tabnol(i+1)>0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
!
!     SET ALL ENTRIES IN TABNOL BACK TO THEIR ORIGINAL POSITIVE STATUS.
!     IF AN ENTRY IS STILL POSITIVE, THIS IMPLIES THE TABLE WAS NOT
!     FOUND IN THE DIT AND A FATAL ERROR CONDITION EXISTS.
!
         iflag = 0
         DO i = 1 , limjj
            IF ( Tabnol(i+1)<=0 ) THEN
               Tabnol(i+1) = -Tabnol(i+1)
            ELSE
               CALL mesage(30,89,Tabnol(i+1))
               iflag = 1
            ENDIF
         ENDDO
         IF ( iflag/=0 ) CALL mesage(-37,0,name)
!
!     WRAP-UP PRETAB
!
         CALL close(dit,clsrw)
         part1 = .FALSE.
         tabido = -1
         xo = -10.0E+37
         Lcused = itable + 1 - offset
         icheck = 123456789
         RETURN
      CASE (5)
         jj = jj + 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ENTRY TAB COMPUTES THE FUNCTIONAL VALUE Y AT THE ABSCISSA X FOR
!     THE FUNCTION DEFINED BY THE TABLE WHOSE NUMBER IS TABID
!
!
         ENTRY tab(Tabid,X,Y)
!     =====================
!
         IF ( icheck/=123456789 ) CALL errtrc('PRETAB  ',200)
         ASSIGN 40 TO ihop
!
         IF ( Tabid==tabido .AND. X==xo ) THEN
            Y(1) = yo
            RETURN
         ELSE
            tabido = Tabid
            xo = X
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         IF ( Tabid/=0 ) THEN
!
!     SEARCH THE TABLE DICTIONARY TO FIND THE TABLE NUMBER
!
            DO ii = idicl , idich , lnth
               IF ( Tabid==Iz(ii) ) GOTO 30
            ENDDO
!
!     TABID COULD NOT BE FOUND IN THE DICTIONARY - FATAL ERROR
!
            CALL mesage(-30,90,Tabid)
 30         l = ii
            itype = Iz(l+1)
            sctyp = Iz(l+11) + 1
            GOTO ihop
         ELSE
            Y(1) = 0.0
            yo = 0.0
            RETURN
         ENDIF
 40      IF ( itype==2 ) THEN
!
!     TABLE TYPE = 2
!
!     ARGUMENT = (X - X1)
!
            xx = X - z(l+4)
         ELSEIF ( itype==3 ) THEN
!
!     TABLE TYPE = 3
!
!     ARGUMENT = (X - X1)/X2
!
            xx = (X-z(l+4))/z(l+5)
         ELSEIF ( itype==4 ) THEN
!
!     TABLE TYPE = 4
!
!     ARGUMENT = (X - X1)/X2
!
            xx = (X-z(l+4))/z(l+5)
!
!     POLYNOMIAL EVALUATION
!
            IF ( xx<=(z(l+6)-z(l+4))/z(l+5) ) THEN
               prop = z(l+8)
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ELSE
               IF ( xx<(z(l+7)-z(l+4))/z(l+5) ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               prop = z(l+9)
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSEIF ( itype==5 ) THEN
!
!     TABLE TYPE = 5
!
!     TABRNDG CARD FUNTION ONLY
!
!
!     PICK UP TYPE
!
            lx = Iz(l+4)
!
!     P US ONE OVER TERM IN PX TABLE BASED ONL TYPE
!
            p = 1./px(lx,1)
!
!     CONPUTE K SQUARED FROM PX TABLE
!
            xksq = px(lx,2)*px(lx,2)
!
!     RETRIEVE LU (L/U) FROM TABLE PARAMS
!
            xlu = z(l+5)
            xx = 2.*z(l+6)**2*xlu
            xlu = xlu*xlu
            wsq = S4pisq*xo*xo
            tr = xksq*xlu*wsq
            prop = xx*(1.+2.*(p+1.)*tr)/(1.+tr)**(p+1.5)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     TABLE TYPE = 1
!
!     A  RGUMENT = X
!
            xx = X
         ENDIF
!
!     ROUTINE TO PERFORM LINEAR INTERPOLATION FOR FUNCTION IN A TABLE.
!     L POINTS TO THE ENTRY IN THE TABLE DICTIONARY WHICH DEFINES THE
!     TABLE. THE ARGUMENT IS XX. THE FUNCTIONAL VALUE IS STORED IN PROP.
!     EXTRAPOLATION IS MADE IF XX IS OUTSIDE THE LIMITS OF THE TABLE.
!     HENCE THERE ARE NO ERROR RETURNS.
!     HOWEVER, IF FUNCTION OVERFLOWED ON EXTRAPOLATION OUTSIDE TABLE
!     LIMITS, A FATAL MESSAGE IS ISSUED.
!
         itabl = Iz(l+2)
         ntabl = Iz(l+3)
         up = 1.0
         IF ( z(itabl)>z(itabl+2) ) up = -1.0
         kxx1 = itabl
         IF ( (xx-z(itabl))*up>0.0 ) THEN
            kxx1 = ntabl - 2
            IF ( (xx-z(ntabl))*up<0.0 ) THEN
               klo = 1
               khi = (ntabl-itabl)/2 + 1
               SPAG_Loop_1_2: DO
                  kx = (klo+khi+1)/2
                  kxx = (kx-1)*2 + itabl
                  IF ( (xx-z(kxx))*up<0 ) THEN
                     khi = kx
                  ELSEIF ( (xx-z(kxx))*up==0 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     klo = kx
                  ENDIF
                  IF ( khi-klo==1 ) THEN
                     kxx1 = (klo-1)*2 + itabl
                     IF ( kxx==kxx1 ) EXIT SPAG_Loop_1_2
                     IF ( xx/=z(kxx1+2) ) EXIT SPAG_Loop_1_2
                     kxx = kxx1 + 2
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO SPAG_Loop_1_2
            ENDIF
         ENDIF
         IF ( sctyp==1 ) THEN
            prop = (xx-z(kxx1))*(z(kxx1+3)-z(kxx1+1))/(z(kxx1+2)-z(kxx1)) + z(kxx1+1)
            IF ( abs(prop)<1.0E-36 ) prop = 0.0
            IF ( abs(prop)<1.0E+36 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( up>0. .AND. (xx<z(itabl) .OR. xx>z(ntabl)) ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( up<0. .AND. (xx>z(itabl) .OR. xx<z(ntabl)) ) THEN
               spag_nextblock_1 = 10
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( sctyp==3 ) THEN
            CALL smilog(z(kxx1),z(kxx1+1),z(kxx1+2),z(kxx1+3),xx,prop)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( sctyp==4 ) THEN
            CALL logsmi(z(kxx1),z(kxx1+1),z(kxx1+2),z(kxx1+3),xx,prop)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL loglog(z(kxx1),z(kxx1+1),z(kxx1+2),z(kxx1+3),xx,prop)
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      CASE (7)
         IF ( xx==z(kxx-2) ) THEN
            prop = (z(kxx-1)+z(kxx+1))/2.0
         ELSEIF ( xx==z(kxx+2) ) THEN
            prop = (z(kxx+1)+z(kxx+3))/2.0
         ELSE
            prop = z(kxx+1)
         ENDIF
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
      CASE (8)
         nn = Iz(l+3)
         prop = z(nn)
         DO WHILE ( nn>Iz(l+2) )
            prop = prop*xx + z(nn-1)
            nn = nn - 1
         ENDDO
         IF ( part1 ) GOTO igoto
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
 60      z(l+8) = prop
         ASSIGN 80 TO igoto
         xx = (z(l+7)-z(l+4))/z(l+5)
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
 80      z(l+9) = prop
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
!
!     TAB WRAP-UP
!
         Y(1) = prop
         yo = Y(1)
         RETURN
!
!
         ENTRY tab1(Tabid,X,Y)
!     ======================
!
!     ENRTY FOR TABLE TRANSFORM
!
         ASSIGN 100 TO ihop
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     L  POINTS  TO TABLE
!     ITYPE IS THE TABLE TYPE
!
 100     itabl = Iz(l+2)
         ntabl = Iz(l+3)
         omega = Twopi*X
         IF ( itype==2 ) THEN
!
!     TABLED2
!
            x1 = z(l+4)
            x2 = 1.0
         ELSEIF ( itype==3 ) THEN
!
!     TABLED3
!
            x1 = z(l+4)
            x2 = z(l+5)
         ELSEIF ( itype/=4 ) THEN
!
!     TABLED1
!
            x1 = 0.0
            x2 = 1.0
         ENDIF
!
!     TABLED4
!
!
!     EVALUATE SUM
!
         sum = cmplx(0.0,0.0)
         k = itabl
         DO
            yi = z(k+1)
            xi = z(k)
            yip1 = z(k+3)
            xip1 = z(k+2)
            omegax = omega*x2*(xip1-xi)
            CALL ifte2(omegax,rp,cp)
            p = -omega*(x1+x2*xip1)
            a = cmplx(0.,p)
            b = cmplx(rp,cp)
            term = cexp(a)*b*yip1
            p = -omega*(x1+x2*xi)
            a = cmplx(0.,p)
            b = cmplx(rp,-cp)
            term = term + cexp(a)*b*yi
            term = term*(xip1-xi)*.5
            sum = sum + term
            k = k + 2
            IF ( k>=ntabl ) THEN
!
!     FINISH FUNCTION
!
               sum = sum*x2
               Y(1) = real(sum)
               Y(2) = aimag(sum)
               RETURN
            ENDIF
         ENDDO
!
!     FATAL ERROR MESSAGES
!
 120     mn = -1
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 140     mn = -2
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
 160     mn = -3
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (10)
         WRITE (Nout,99001) Ufm , Iz(l)
99001    FORMAT (A23,' 3308, TABLE',I9,' INTERPOLATION ERROR',/5X,'FUNCTION OVERFLOWED WHEN EXTRAPOLATION WAS MADE OUTSIDE ',       &
                &'TABLE GIVEN RANGE.')
         mn = -37
         spag_nextblock_1 = 12
         CYCLE SPAG_DispatchLoop_1
      CASE (11)
         mn = -8
         dit = icrq
         spag_nextblock_1 = 12
      CASE (12)
         CALL mesage(mn,dit,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pretab
