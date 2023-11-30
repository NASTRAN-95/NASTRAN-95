
SUBROUTINE t3pl4d
   IMPLICIT NONE
   REAL Bgpdt(4,3) , Elth , Est(45) , Gpth(3) , Loadvc(1) , Rpdata(1) , Slt(11)
   INTEGER Elid , Igpdt(4,3) , Ipdata(7) , Islt(1) , Nogo , Nout , Sil(3) , Sysbuf
   COMMON /pindex/ Est , Slt
   COMMON /system/ Sysbuf , Nout , Nogo
   COMMON /zzzzzz/ Loadvc
   DOUBLE PRECISION aic(1) , avgthk , bmatrx(162) , bterms(6) , cente(3) , detjac , dgpth(3) , dpe(3,3) , edglen(3) , egpdt(4,3) ,  &
                  & epnorm(4,3) , gpnorm(4,3) , lx , ly , p , ppp(3) , shp(3) , teb(9) , th , tub(9) , v3t(3) , weight
   INTEGER cid , i , ierr , iorder(3) , ipt , j , jp , ndof , nnode
   LOGICAL constp , normal , sheart
   REAL locate(3) , nv(3) , nvx(3) , pe(3,3) , x
!
!     DOUBLE PRECISION ROUTINE TO PROCESS PLOAD4 PRESSURE DATA AND
!     GENERATE EQUIVALENT NODAL LOADS FOR A TRIA3 ELEMENT.
!
!     WAS NAMED T3PRSD (LOADVC,RPDATA,IPDATA) IN UAI
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
!    DATA IN THE PLOAD4 ENTRY, 11 WORDS IN ISLT ARRAY
!
!       EID - ELEMENT ID, IPDATA(0)=ISLT(1)
!       PPP - CORNER GRID POINT PRESSURES PER UNIT SURFACE AREA,
!             RPDATA (1-4)
!       DUM - DUMMY DATA WORDS, IPDATA (5-6)
!       CID - COORDINATE SYSTEM FOR DEFINITION OF PRESSURE VECTOR,
!             IPDATA(7)
!       NV  - PRESSURE DIRECTION VECTOR, RPDATA(8-10)
!           - IF CID IS BLANK OR ZERO, THE PRESSURE ACTS NORMAL TO THE
!             SURFACE OF THE ELEMENT.
!
!     EQUIVALENT NUMERICAL INTEGRATION POINT LOADS PP(III) ARE OBTAINED
!     VIA BI-LINEAR INTERPOLATION
!
   !>>>>EQUIVALENCE (Slt(1),Islt(1)) , (Est(1),Elid) , (Est(2),Sil(1)) , (Est(5),Gpth(1)) , (Est(12),Elth) ,                             &
!>>>>    & (Est(27),Bgpdt(1,1),Igpdt(1,1)) , (Slt(2),Ipdata(1),Rpdata(1))
!
!     INITIALIZE
!
   weight = 1.0D0/6.0D0
   sheart = .FALSE.
   nnode = 3
   ndof = 3
   DO i = 1 , ndof
      DO j = 1 , nnode
         dpe(i,j) = 0.0D0
      ENDDO
   ENDDO
!
!     GET THE PRESSURE INFORMATION
!
!     EST (45 WORDS) AND SLT (11 WORDS) ARE THE DATA FOR EST AND SLT
!     WHICH ARE READ IN BY EXTERN AND ARE READY TO BE USED
!
!     IF ISLT(1).GT.0, GET THE PLOAD4 DATA FROM THE PROCESSED PLOAD2
!                      INFORMATION IN ARRAY SLT.
!                      (NOT AVAILABLE IN COSMIC/NASTRAN)
!     IF ISLT(1).LT.0, GET THE PLOAD4 DATA FROM THE ORIGINAL PLOAD4
!                      INFORMATION IN ARRAY RPDATA.
!                      (SET TO NEGATIVE BY PLOAD4 SUBROUTINE)
!
   IF ( Islt(1)<0 ) THEN
!
      DO i = 1 , nnode
         ppp(i) = dble(Rpdata(i))
      ENDDO
      constp = ppp(2)==0.0D0 .AND. ppp(3)==0.0D0
      IF ( constp ) p = ppp(1)
      cid = Ipdata(7)
!
!     GET THE DIRECTION VECTOR AND NORMALIZE IT
!
      x = 0.0
      DO i = 1 , nnode
         nv(i) = Rpdata(i+7)
         x = x + nv(i)*nv(i)
      ENDDO
      normal = .TRUE.
      IF ( x>0.0 ) THEN
         normal = .FALSE.
         x = sqrt(x)
         DO i = 1 , nnode
            nv(i) = nv(i)/x
         ENDDO
      ENDIF
   ELSE
      normal = .TRUE.
      constp = .TRUE.
      p = dble(Slt(2))
   ENDIF
!
!     SET UP THE ELEMENT FORMULATION
!
   CALL t3setd(ierr,Sil,Igpdt,Elth,Gpth,dgpth,egpdt,gpnorm,epnorm,iorder,teb,tub,cente,avgthk,lx,ly,edglen,Elid)
   IF ( ierr==0 ) THEN
!
!     START THE LOOP ON INTEGRATION POINTS
!
      DO ipt = 5 , 7
!
         CALL t3bmgd(ierr,sheart,ipt,iorder,egpdt,dgpth,aic,th,detjac,shp,bterms,bmatrx)
         IF ( ierr/=0 ) GOTO 100
!
!     CALCULATE THE PRESSURE AT THIS POINT
!
         IF ( .NOT.(constp) ) THEN
            p = 0.0D0
            DO i = 1 , nnode
               p = p + shp(i)*ppp(i)
            ENDDO
         ENDIF
!
!     SET THE DIRECTION OF PRESSURE AT THIS POINT.
!     THE RESULTING VECTOR MUST BE IN THE BASIC COORD. SYSTEM
!
         IF ( normal ) THEN
            v3t(1) = teb(7)*detjac
            v3t(2) = teb(8)*detjac
            v3t(3) = teb(9)*detjac
!
         ELSEIF ( cid/=0 ) THEN
!
!     FOR NON-ZERO CID, COMPUTE THE LOCATION OF THE INTEGRATION POINT SO
!     THAT WE CAN ROTATE THE USER VECTOR PER CID.  THIS LOCATION IS
!     REQUIRED ONLY IF CID IS CYLINDRICAL OR SPHERICAL.
!
            locate(1) = 0.0
            locate(2) = 0.0
            locate(3) = 0.0
            DO j = 1 , nnode
               locate(1) = locate(1) + Bgpdt(2,j)*shp(j)
               locate(2) = locate(2) + Bgpdt(3,j)*shp(j)
               locate(3) = locate(3) + Bgpdt(4,j)*shp(j)
            ENDDO
!
!     NOW ROTATE THE VECTOR
!
            CALL glbbas(nv(1),nvx(1),locate(1),cid)
            v3t(1) = nvx(1)*detjac
            v3t(2) = nvx(2)*detjac
            v3t(3) = nvx(3)*detjac
         ELSE
            v3t(1) = nv(1)*detjac
            v3t(2) = nv(2)*detjac
            v3t(3) = nv(3)*detjac
         ENDIF
!
!     COMPUTE THE CONTRIBUTION TO THE LOAD MATRIX FROM THIS INTEGRATION
!     POINT AS NT*P*V3T
!
         DO i = 1 , nnode
            DO j = 1 , ndof
               dpe(j,i) = dpe(j,i) + weight*p*shp(i)*v3t(j)
               pe(j,i) = dpe(j,i)
            ENDDO
         ENDDO
!
      ENDDO
!
!     END OF NUMERICAL INTEGRATION LOOP
!     ADD ELEMENT LOAD TO OVERALL LOAD.
!
      DO j = 1 , nnode
         IF ( Igpdt(1,j)/=0 ) CALL basglb(pe(1,j),pe(1,j),Bgpdt(2,j),Igpdt(1,j))
         jp = Sil(j) - 1
         DO i = 1 , ndof
            Loadvc(jp+i) = Loadvc(jp+i) + pe(i,j)
         ENDDO
      ENDDO
      GOTO 99999
   ENDIF
!
!     FATAL ERROR
!
 100  Islt(1) = iabs(Islt(1))
   CALL mesage(30,224,Islt(1))
   Nogo = 1
!
99999 RETURN
END SUBROUTINE t3pl4d