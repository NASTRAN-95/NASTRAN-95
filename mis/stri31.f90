
SUBROUTINE stri31
   IMPLICIT NONE
   LOGICAL Bendng , Mbcoup , Membrn , Norpth , Shrflx
   REAL Bgpdt(4,3) , Cosmat , Drkce(33) , Dummy , Eltemp , Elth , Est(100) , Gpth(3) , Htcp , Kheat(7) , Ph1rst(701) , Relout(300) ,&
      & Sinmat , Tempel , Tsub0 , Type , Z1o , Z2o , Zoff , Zoff1
   INTEGER Elid , Idum(51) , Idumal , Ielout(300) , Igpdt(4,3) , Inflag , Int , Iorder(3) , Itherm , Matid , Nest(39) , Nogo ,      &
         & Nout , Nphi(100) , Pido , Prec , Sil(3) , Silo(3) , Sysbuf
   COMMON /hmtout/ Kheat , Type
   COMMON /matin / Matid , Inflag , Eltemp , Dummy , Sinmat , Cosmat
   COMMON /sdr2x5/ Est , Elid , Silo , Iorder , Tsub0 , Z1o , Z2o , Pido , Idumal , Ph1rst
   COMMON /sdr2x6/ Ielout
   COMMON /system/ Sysbuf , Nout , Nogo , Idum , Prec , Itherm
   COMMON /terms / Membrn , Bendng , Shrflx , Mbcoup , Norpth
   REAL aic(18) , alpha(6) , avgthk , bdum(3) , bmat1(486) , bmatrx(162) , bmtrx(36) , bterms(6) , cente(3) , determ , detjac ,     &
      & dgpth(3) , ecpt(4) , edglen(3) , egnor(4) , egpdt(4,3) , epnorm(4,3) , eps , gi(36) , gpnorm(4,3) , gsube , jog , jok ,     &
      & k11 , k22 , lx , ly , mominr , offset , reali , rho , shpt(3) , tbg(9) , teb(9) , tem(9) , tes(9) , teu(9) , th , thetam ,  &
      & thetas , ts , tsb(9) , tsi , tsm(9) , tss , tub(9) , tum(9) , tus(9) , uem(9) , vem(4) , zz(4)
   INTEGER flags , hunmeg , i , ialf , icount , iec , ierr , ig , ig1 , ig2 , ig3 , index(2,3) , io , iop , ip , ip2 , iph , ipt ,  &
         & ising , istart , ix , j , jg , kpt , ll , mcsid , mid(4) , name(2) , nd2 , nd25 , nd6 , nd7 , nd8 , nd9 , ndof , necpt(4)&
         & , nnod2 , nnode , npart , scsid
   LOGICAL noalfa , sheart , userst
!
!     ROUTINE TO RECOVER CTRIA3 ELEMENT FORCES, STRAINS, AND STRESSES.
!     PHASE 1.
!
!     WAS NAMED T3ST1D/S IN UAI CODE
!
!                 EST  LISTING
!
!        WORD     TYP       DESCRIPTION
!     ----------------------------------------------------------------
!     ECT:
!         1        I   ELEMENT ID, EID
!         2-4      I   SIL LIST, GRIDS 1,2,3
!         5-7      R   MEMBRANE THICKNESSES T, AT GRIDS 1,2,3
!         8        R   MATERIAL PROPERTY ORIENTAION ANGLE, THETA
!               OR I   COORD. SYSTEM ID (SEE TM ON CTRIA3 CARD)
!         9        I   TYPE FLAG FOR WORD 8
!        10        R   GRID OFFSET, ZOFF
!    EPT:
!        11        I   MATERIAL ID FOR MEMBRANE, MID1
!        12        R   ELEMENT THICKNESS,T (MEMBRANE, UNIFORMED)
!        13        I   MATERIAL ID FOR BENDING, MID2
!        14        R   MOMENT OF INERTIA FACTOR, I (BENDING)
!        15        I   MATERIAL ID FOR TRANSVERSE SHEAR, MID3
!        16        R   TRANSV. SHEAR CORRECTION FACTOR, TS/T
!        17        R   NON-STRUCTURAL MASS, NSM
!        18-19     R   STRESS FIBER DISTANCES, Z1,Z2
!        20        I   MATERIAL ID FOR MEMBRANE-BENDING COUPLING, MID4
!        21        R   MATERIAL ANGLE OF ROTATION, THETA
!               OR I   COORD. SYSTEM ID (SEE MCSID ON PSHELL CARD)
!                      (DEFAULT FOR WORD 8)
!        22        I   TYPE FLAG FOR WORD 21 (DEFAULT FOR WORD 9)
!        23        I   INTEGRATION ORDER FLAG
!        24        R   STRESS ANGLE OF RATATION, THETA
!               OR I   COORD. SYSTEM ID (SEE SCSID ON PSHELL CARD)
!        25        I   TYPE FLAG FOR WORD 24
!        26        R   OFFSET, ZOFF1 (DEFAULT FOR WORD 10)
!    BGPDT:
!        27-38   I/R   CID,X,Y,Z  FOR GRIDS 1,2,3
!    ETT:
!        39        I   ELEMENT TEMPERATURE
!
!
!     ****************        RESIDES IN COMMON BLOCK SDR2X5 (AFTER EST)
!     PH1OUT DATA BLOCK       TOTAL NO. OF WORDS = 713
!     ****************
!
!     PH1OUT( 1)    = ELID, ELEMENT ID
!     PH1OUT( 2- 4) = SIL NUMBERS
!     PH1OUT( 5- 7) = ARRAY IORDER
!     PH1OUT( 8)    = TSUB0, REFERENCE TEMP.
!     PH1OUT( 9-10) = Z1 & Z2, FIBER DISTANCES
!     PH1OUT(11)    = ID OF THE ORIGINAL PCOMPI PROPERTY ENTRY
!     PH1OUT(12)    = DUMMY WORD (FOR ALLIGNMENT)
!
!     PH1RST( 1)    = AVGTHK, AVERAGE THICKNESS
!     PH1RST( 2)    = MOMINR, MOMENT OF INER. FACTOR
!     PH1RST( 3-38) = 6X6 MATERIAL PROPERTY MATRIX (NO SHEAR)
!     PH1RST(39-41) = THERMAL EXPANSION COEFFICIENTS FOR MEMBRANE
!     PH1RST(42-44) = THERMAL EXPANSION COEFFICIENTS FOR BENDING
!     PH1RST(45-47) = CORNER NODE THICKNESSES
!     PH1RST(48)    = OFFSET OF ELEMENT FROM GP PLANE
!     PH1RST(49-57) = 3X3 USER-TO-MATERIAL COORD. TRNASF. MATRIX UEM
!     PH1RST(58-66) = 3X3 ELEM-TO-STRESS/STRAIN TRANSF. TENSOR TES
!     PH1RST(67-93) = THREE 3X3 GLOBAL-TO-ELEM COORD. TRANSFORMATION
!                     NODAL MATRICES TEG, ONE FOR EACH NODE
!
!     THE FOLLOWING IS REPEATED FOR EACH EVALUATION POINT (4 TIMES, AT
!     THE CENTER OF THE ELEMENT AND AT 3 STANDARD TRIANGULAR POINTS).
!     THE CHOICE OF THE FINAL STRESS/FORCE OUTPUT POINTS IS MADE AT THE
!     SUBCASE LEVEL (PHASE 2).
!
!              1             ELEMENT THICKNESS AT THIS POINT
!            2 - 5           OUT-OF-PLANE-SHEAR-FORCE/STRAIN MATRIX
!            6 - 8           ELEMENT SHAPE FUNCTION VALUES
!          8+1 - 8+8*NDOF    STRAIN RECOVERY MATRIX
!
!
!     *****************      RESIDES IN COMMON BLOCK SDR2X6
!     IELOUT DATA BLOCK      CONTAINS DATA FOR GPSRN
!     *****************      (TOTAL NO OF WORDS =  77)
!
!              1             ELEMENT ID
!              2             AVERAGE THICKNESS
!
!     THE FOLLOWING IS REPEATED FOR EACH NODE.
!
!         WORD  1            SIL NUMBER
!         WORD  2-10         [TSB] FOR Z1
!         WORD 11-19         [TSB] FOR Z2
!         WORD 20-22         NORMAL VECTOR IN BASIC COORD. SYSTEM
!         WORD 23-25         GRID COORDS   IN BASIC COORD. SYSTEM
!
!
   EQUIVALENCE (Est(1),Nest(1)) , (Est(2),Sil(1)) , (Est(5),Gpth(1)) , (Est(10),Zoff) , (Est(12),Elth) , (Est(23),Int) ,            &
    & (Est(26),Zoff1) , (Est(39),Tempel) , (Est(27),Bgpdt(1,1),Igpdt(1,1))
   EQUIVALENCE (Nphi(1),Elid) , (Nphi(27),Drkce(1)) , (necpt(1),ecpt(1)) , (Ielout(1),Relout(1)) , (Htcp,Kheat(4))
   DATA hunmeg , istart/100000000 , 93/ , eps/1.0E-17/
   DATA name/4HTRIA , 4H3   /
!
!     INITIALIZE
!
   nnode = 3
   mominr = 0.0
   ts = 0.0
   Eltemp = Tempel
   Elid = Nest(1)
   Z1o = Est(18)
   Z2o = Est(19)
   Pido = Nest(11) - hunmeg
   mcsid = Nest(21)
   scsid = Nest(24)
   flags = Nest(25)
   userst = scsid<0 .AND. flags==1
   noalfa = .FALSE.
   sheart = .TRUE.
   offset = Zoff
   IF ( Zoff==0.0 ) offset = Zoff1
!
!     START FILLING IN THE DATA BLOCKS
!
   Ielout(1) = Elid
   DO i = 1 , 3
      Ielout(3+(i-1)*25) = Sil(i)
      DO j = 1 , 3
         Relout(25*i+j-1) = Bgpdt(j+1,i)
      ENDDO
   ENDDO
!
!     SET UP THE ELEMENT FORMULATION
!
   CALL t3sets(ierr,Sil,Igpdt,Elth,Gpth,dgpth,egpdt,gpnorm,epnorm,Iorder,teb,tub,cente,avgthk,lx,ly,edglen,Elid)
   IF ( ierr==0 ) THEN
      CALL gmmats(teb,3,3,0,tub,3,3,1,teu)
      DO i = 1 , 3
         Silo(i) = Sil(i)
      ENDDO
!
!     SET THE NUMBER OF DOF'S
!
      nnod2 = nnode*nnode
      ndof = nnode*6
      npart = ndof*ndof
      nd2 = ndof*2
      nd6 = ndof*6
      nd7 = ndof*7
      nd8 = ndof*8
      nd9 = ndof*9
!
!     PASS THE LOCATION OF THE ELEMENT CENTER FOR TRANSFORMATIONS.
!
      DO iec = 2 , 4
         ecpt(iec) = cente(iec-1)
      ENDDO
!
!     STRESS TRANSFORMATIONS
!
      IF ( userst ) THEN
         Est(24) = 0.0
         Nest(25) = 0
      ENDIF
      CALL shcsgs(*300,Nest(25),Nest(24),Est(24),Nest(25),Nest(24),Est(24),necpt,tub,scsid,thetas,tus)
      CALL gmmats(teu,3,3,0,tus,3,3,0,tes)
!
!     OBTAIN MATERIAL INFORMATION
!
!     SET MATERIAL FLAGS
!     0.83333333 = 5.0/6.0
!
      IF ( Nest(13)/=0 ) mominr = Est(14)
      IF ( Nest(13)/=0 ) ts = Est(16)
      IF ( Est(16)==0.0 ) ts = 0.83333333
      IF ( Nest(13)==0 .AND. Nest(11)>hunmeg ) ts = 0.83333333
!
      mid(1) = Nest(11)
      mid(2) = Nest(13)
      mid(3) = Nest(15)
      mid(4) = Nest(20)
!
      Membrn = mid(1)>0
      Bendng = mid(2)>0 .AND. mominr>0.0
      Shrflx = mid(3)>0
      Mbcoup = mid(4)>0
      Norpth = .FALSE.
!
!     SET UP TRANSFORMATION MATRIX FROM MATERIAL TO ELEMENT COORD. SYSTM
!
      CALL shcsgs(*200,Nest(9),Nest(8),Nest(8),Nest(21),Nest(20),Nest(20),necpt,tub,mcsid,thetam,tum)
!
!     BRANCH ON FORMULATION TYPE, HEAT
!
      IF ( Itherm/=0 ) THEN
!
!
!     BEGINNING OF HEAT FORCE RECOVERY
!
!
!     SET UP FOR THE UNIVERSAL PHASE 2 HEAT RECOVERY
!
         Nphi(22) = 2
         Nphi(23) = nnode
         Nphi(24) = name(1)
         Nphi(25) = name(2)
!
         sheart = .FALSE.
         ipt = 4
         CALL t3bmgs(ierr,sheart,ipt,Iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmatrx)
         IF ( ierr==0 ) THEN
!
            Matid = Nest(11)
            Inflag = 2
            thetas = thetas - thetam
            Sinmat = sin(thetas)
            Cosmat = cos(thetas)
            CALL hmat(Elid)
!
            Drkce(1) = Kheat(1)
            Drkce(2) = Kheat(2)
            Drkce(3) = Kheat(2)
            Drkce(4) = Kheat(3)
!
            tes(3) = tes(4)
            tes(4) = tes(5)
            CALL gmmats(tes,2,2,1,bterms,2,nnode,0,Drkce(10))
!
            GOTO 99999
         ENDIF
      ELSE
!
!     FETCH MATERIAL PROPERTIES
!
         CALL gmmats(teu,3,3,0,tum,3,3,0,tem)
         CALL gmmats(tes,3,3,1,tem,3,3,0,tsm)
         CALL shgmgs(*400,Elid,tsm,mid,ts,noalfa,gi,rho,gsube,Tsub0,egnor,alpha)
!
!     TURN OFF THE COUPLING FLAG WHEN MID4 IS PRESENT WITH ALL
!     CALCULATED ZERO TERMS.
!
         IF ( Mbcoup ) THEN
            DO i = 28 , 36
               IF ( abs(gi(i))>eps ) GOTO 20
            ENDDO
            Mbcoup = .FALSE.
         ENDIF
!
!     CONTINUE FILLING IN THE DATA BLOCKS
!
 20      Ph1rst(1) = avgthk
         Ph1rst(2) = mominr
         Ph1rst(48) = offset
         Relout(2) = avgthk
!
!     PUT NORMALS IN IELOUT, GRID THICKNESS IN PH1OUT
!
         DO i = 1 , nnode
            io = Iorder(i)
            iop = (io-1)*25 + 21
            Relout(iop+1) = gpnorm(2,i)
            Relout(iop+2) = gpnorm(3,i)
            Relout(iop+3) = gpnorm(4,i)
            Ph1rst(44+io) = dgpth(i)
         ENDDO
!
!     CALCULATE [TSB] AND STORE IT IN IELOUT.
!
         CALL gmmats(tes,3,3,1,teb,3,3,0,tsb)
         nd25 = nnode*25
         DO ip2 = 3 , nd25 , 25
            DO ix = 1 , 9
               Relout(ip2+ix) = tsb(ix)
               Relout(ip2+ix+9) = tsb(ix)
            ENDDO
         ENDDO
!
!     STORE ALPHA IN PH1RST(39-44)
!
         DO ialf = 1 , 6
            Ph1rst(38+ialf) = alpha(ialf)
         ENDDO
!
!     STORE UEM IN PH1RST(49-57)
!     STORE TES IN PH1RST(58-66)
!
         CALL shstts(tem,uem,vem)
         DO ll = 1 , 9
            Ph1rst(48+ll) = uem(ll)
            Ph1rst(57+ll) = tes(ll)
         ENDDO
!
!     STORE THE 6X6 [G] IN PH1RST
!
         DO ig = 3 , 38
            Ph1rst(ig) = 0.0
         ENDDO
!
         IF ( Membrn ) THEN
            DO ig = 1 , 3
               ig1 = (ig-1)*6 + 2
               ig2 = (ig-1)*3
               DO jg = 1 , 3
                  Ph1rst(ig1+jg) = gi(ig2+jg)
               ENDDO
            ENDDO
         ENDIF
!
         IF ( Bendng ) THEN
            DO ig = 1 , 3
               ig1 = (ig-1)*6 + 23
               ig2 = (ig-1)*3 + 9
               DO jg = 1 , 3
                  Ph1rst(ig1+jg) = gi(ig2+jg)*mominr
               ENDDO
            ENDDO
!
            IF ( Mbcoup ) THEN
               DO ig = 1 , 3
                  ig3 = (ig-1)*3
                  ig1 = ig3 + 5
                  ig2 = ig3 + 27
                  ig3 = ig3 + 20
                  DO jg = 1 , 3
                     Ph1rst(ig1+jg) = gi(ig2+jg)
                     Ph1rst(ig3+jg) = gi(ig2+jg)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
!
!     CALCULATE [TEG] FOR EACH NODE AND STORE IT IN PH1RST
!
         DO i = 1 , nnode
            ip = 67 + (i-1)*9
            CALL transs(Igpdt(1,i),tbg)
            CALL gmmats(teb,3,3,0,tbg,3,3,0,Ph1rst(ip))
         ENDDO
!
!     GET THE GEOMETRY CORRECTION TERMS
!
         IF ( Bendng ) THEN
            CALL t3gems(ierr,egpdt,Iorder,gi(10),gi(19),lx,ly,edglen,Shrflx,aic,jog,jok,k11,k22)
            IF ( ierr/=0 ) GOTO 100
         ENDIF
!
!     REDUCED INTEGRATION
!
         IF ( Int==0 ) THEN
!
!     DETERMINE THE AVERAGE [B] FOR OUT-OF-PLANE SHEAR
!
            DO ipt = 1 , 3
               kpt = (ipt-1)*nd9 + 1
               CALL t3bmgs(ierr,sheart,ipt,Iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmat1(kpt))
               IF ( ierr/=0 ) GOTO 100
            ENDDO
!
            DO i = 1 , ndof
               bmtrx(i) = bmat1(i+nd6) + bmat1(i+nd6+nd9) + bmat1(i+nd6+2*nd9)
               bmtrx(i+ndof) = bmat1(i+nd7) + bmat1(i+nd7+nd9) + bmat1(i+nd7+2*nd9)
            ENDDO
         ENDIF
!
!     STRAIN/STRESS EVALUATION LOOP
!
!     PRESET THE PH1RST COUNTER TO THE START OF THE REPEATED SECTION
!     WHICH WILL NOW BE FILLED.
!
         icount = istart
!
         DO ipt = 4 , 7
!
            CALL t3bmgs(ierr,sheart,ipt,Iorder,egpdt,dgpth,aic,th,detjac,shpt,bterms,bmatrx)
            IF ( ierr/=0 ) GOTO 100
!
            IF ( Int==0 ) THEN
               DO ix = 1 , ndof
                  bmatrx(ix+nd6) = bmtrx(ix)
                  bmatrx(ix+nd7) = bmtrx(ix+ndof)
               ENDDO
            ENDIF
!
!     FINISH FILLING IN THE DATA BLOCKS
!
!     STORE THICKNESS
!
            Ph1rst(icount+1) = th
!
!     STORE [G3]
!
            IF ( .NOT.Bendng ) THEN
!
               DO ig = 1 , 4
                  Ph1rst(icount+1+ig) = 0.0
               ENDDO
            ELSE
               reali = mominr*th*th*th/12.0
               tsi = ts*th
               tss = 1.0/(2.0*12.0*reali)
!
               zz(1) = (jog/tsi)*gi(22) + tss*jok*k22
               zz(2) = -(jog/tsi)*(gi(20)+gi(21))/2.0
               zz(3) = zz(2)
               zz(4) = (jog/tsi)*gi(19) + tss*jok*k11
!
               CALL invers(2,zz,2,bdum,0,determ,ising,index)
               IF ( ising/=1 ) GOTO 100
!
               DO ig = 1 , 4
                  Ph1rst(icount+1+ig) = zz(ig)
               ENDDO
            ENDIF
!
!     STORE SHAPE FUNCTION VALUES
!
            DO i = 1 , nnode
               Ph1rst(icount+5+i) = shpt(i)
            ENDDO
!
!     STORE THE STRAIN RECOVERY MATRIX
!
            DO iph = 1 , nd8
               Ph1rst(icount+8+iph) = bmatrx(iph)
            ENDDO
!
!     END OF THE EVALUATION LOOP
!
!     INCREMENT THE PH1RST COUNTER
!
            icount = icount + 8 + 8*ndof
!
         ENDDO
         GOTO 99999
      ENDIF
   ENDIF
!
!
!     FATAL ERRORS
!
!     CTRIA3 ELEMENT HAS ILLEGAL GEOMETRY OR CONNECTIONS
!
 100  j = 224
   GOTO 500
!
!     THE X-AXIS OF THE MATERIAL COORDINATE SYSTEM HAS NO PROJECTION
!     ON THE PLANE OF THE CTRIA3 ELEMENT
!
 200  j = 225
   Nest(2) = mcsid
   GOTO 500
!
!     THE X-AXIS OF THE STRESS COORDINATE SYSTEM ID HAS NO PROJECTION
!     ON THE PLANE OF THE CTRIA3 ELEMENT
!
 300  j = 227
   Nest(2) = scsid
   GOTO 500
!
!     ILLEGAL DATA DETECTED ON MATERIAL ID REFERENCED BY CTRIA3 ELEMENT
!     FOR MID3 APPLICATION
!
 400  j = 226
   Nest(2) = mid(3)
!
 500  CALL mesage(30,j,Nest(1))
   Nogo = 1
!
99999 RETURN
END SUBROUTINE stri31
