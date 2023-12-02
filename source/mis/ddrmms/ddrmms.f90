!*==ddrmms.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmms(Buf,Eltype,Buf4,Buf6)
   USE c_matin
   USE c_matout
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(16) :: Buf
   INTEGER :: Eltype
   INTEGER :: Buf4
   INTEGER :: Buf6
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(100) :: bufa
   REAL :: cprim , delta , fint1 , temp
   INTEGER , SAVE :: dit , est , int1 , mpt
   INTEGER , DIMENSION(4) , SAVE :: elm , mtd , tmp , wrd
   INTEGER :: elt , file , i , ielid , ieltmp , imatid , iretrn , itemp , n , n1mat , n2mat , nam , nelt , nwords
   EXTERNAL andf , close , fread , fwdrec , mat , mesage , open , premat
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (int1,fint1) , (Ieltmp,Eltemp)
   DATA int1/1/ , est/109/ , mpt/110/ , dit/111/ , elm/1 , 3 , 10 , 34/ , mtd/4 , 4 , 4 , 16/ , tmp/17 , 16 , 17 , 42/ , wrd/17 ,   &
      & 16 , 17 , 42/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         DO i = 1 , 4
            IF ( elm(i)==Eltype ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
      CASE (2)
         nelt = i
         CALL open(*60,est,z(Buf4),0)
         SPAG_Loop_1_1: DO
            CALL fwdrec(*80,est)
            CALL fread(est,elt,1,0)
            IF ( elt==Eltype ) THEN
               nwords = wrd(nelt)
               CALL fread(est,bufa,nwords,0)
               CALL close(est,1)
               n1mat = Buf4 - Buf6
               CALL premat(z(Buf6),z(Buf6),z(Buf4),n1mat,n2mat,mpt,dit)
               matflg = 1
               itemp = tmp(nelt)
               imatid = mtd(nelt)
               ieltmp = bufa(itemp)
               matid = bufa(imatid)
               ielid = bufa(1)
               CALL mat(ielid)
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         IF ( Eltype>0 ) THEN
            IF ( Eltype<=100 ) THEN
!              ROD       BEAM      TUBE      SHEAR     TWIST
!              TRIA1     TRBSC     TRPLT     TRMEM     CONROD
!              ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
!              QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
!              DAMP2     DAMP3     DAMP4     VISC      MASS1
!              MASS2     MASS3     MASS4     CONM1     CONM2
!              PLOTEL    REACT     QUAD3     BAR       CONE
!              TRIARG    TRAPRG    TORDRG    TETRA     WEDGE
!              HEXA1     HEXA2     FLUID2    FLUID3    FLUID4
!              FLMASS    AXIF2     AXIF3     AXIF4     SLOT3
!              SLOT4     HBDY      DUM1      DUM2      DUM3
!              DUM4      DUM5      DUM6      DUM7      DUM8
!              DUM9      QDMEM1    QDMEM2    QUAD4     IHEX1
!              IHEX2     IHEX3     QUADTS    TRIATS    TRIAAX
!              TRAPAX    AERO1     TRIM6     TRPLT1    TRSHL
!              FHEX1     FHEX2     FTETRA    FWEDGE    IS2D8
!              ELBOW     FTUBE     TRIA3     -----     -----
!              -----     -----     -----     -----     -----
!              -----     -----     -----     -----     -----
!              -----     -----     -----     -----     -----
               IF ( Eltype==2 .OR. Eltype==4 .OR. Eltype==5 .OR. Eltype==11 .OR. Eltype==12 .OR. Eltype==13 .OR. Eltype==14 .OR.    &
                  & Eltype==20 .OR. Eltype==21 .OR. Eltype==22 .OR. Eltype==23 .OR. Eltype==24 .OR. Eltype==25 .OR. Eltype==26 .OR. &
                  & Eltype==27 .OR. Eltype==28 .OR. Eltype==29 .OR. Eltype==30 .OR. Eltype==31 .OR. Eltype==32 .OR. Eltype==33 .OR. &
                  & Eltype==35 .OR. Eltype==36 .OR. Eltype==37 .OR. Eltype==38 .OR. Eltype==43 .OR. Eltype==44 .OR. Eltype==45 .OR. &
                  & Eltype==46 .OR. Eltype==47 .OR. Eltype==48 .OR. Eltype==49 .OR. Eltype==50 .OR. Eltype==51 .OR. Eltype==52 .OR. &
                  & Eltype==53 .OR. Eltype==54 .OR. Eltype==55 .OR. Eltype==56 .OR. Eltype==57 .OR. Eltype==58 .OR. Eltype==59 .OR. &
                  & Eltype==60 .OR. Eltype==61 .OR. Eltype==65 .OR. Eltype==66 .OR. Eltype==67 .OR. Eltype==68 .OR. Eltype==69 .OR. &
                  & Eltype==70 .OR. Eltype==71 .OR. Eltype==72 .OR. Eltype==73 .OR. Eltype==74 .OR. Eltype==75 .OR. Eltype==76 .OR. &
                  & Eltype==77 .OR. Eltype==78 .OR. Eltype==79 .OR. Eltype==82 .OR. Eltype==84 .OR. Eltype==85 .OR. Eltype==86 .OR. &
                  & Eltype==87 .OR. Eltype==88 .OR. Eltype==89 .OR. Eltype==90 .OR. Eltype==91 .OR. Eltype==92 .OR. Eltype==93 .OR. &
                  & Eltype==94 .OR. Eltype==95 .OR. Eltype==96 .OR. Eltype==97 .OR. Eltype==98 .OR. Eltype==99 .OR. Eltype==100 )   &
                  & THEN
               ELSEIF ( Eltype==6 .OR. Eltype==7 .OR. Eltype==8 .OR. Eltype==15 .OR. Eltype==17 .OR. Eltype==18 .OR. Eltype==19 .OR.&
                      & Eltype==64 .OR. Eltype==83 ) THEN
!
!     TRIA1  TRIA2  TRIA3  QUAD1  QUAD2  QUAD4  TRBSC  TRPLT  QDPLT
!
                  i = 2
                  ASSIGN 20 TO iretrn
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Eltype==9 .OR. Eltype==16 .OR. Eltype==62 .OR. Eltype==63 .OR. Eltype==80 ) THEN
!
!     TRMEM  QDMEM  QDMEM1  QDMEM2  IS2D8
!
                  i = 1
                  ASSIGN 40 TO iretrn
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Eltype==34 .OR. Eltype==81 ) THEN
!
!     BAR  ELBOW
!
                  Buf(7) = Buf(6) + amax1(Buf(2),Buf(3),Buf(4),Buf(5))
                  Buf(8) = Buf(6) + amin1(Buf(2),Buf(3),Buf(4),Buf(5))
                  Buf(9) = fint1
                  Buf(14) = Buf(6) + amax1(Buf(10),Buf(11),Buf(12),Buf(13))
                  Buf(15) = Buf(6) + amin1(Buf(10),Buf(11),Buf(12),Buf(13))
                  Buf(16) = fint1
!
!     M. S. IN TENSION
!
                  IF ( sigt>0.0 ) THEN
                     temp = Buf(7)
                     IF ( Buf(7)<Buf(14) ) temp = Buf(14)
                     IF ( temp>0.0 ) Buf(9) = sigt/temp - 1.0
                  ENDIF
!
!     M. S. IN COMPRESSION
!
                  IF ( sigc/=0.0 ) THEN
                     temp = Buf(8)
                     IF ( Buf(8)>Buf(15) ) temp = Buf(15)
                     IF ( temp<0.0 ) THEN
                        cprim = -abs(sigc)
                        Buf(16) = cprim/temp - 1.0
                     ENDIF
                  ENDIF
               ELSEIF ( Eltype==39 .OR. Eltype==40 .OR. Eltype==41 .OR. Eltype==42 ) THEN
!
!     TETRA  WEDGE  HEXA1  HEXA2
!
                  Buf(8) = sqrt(Buf(2)*(Buf(2)-Buf(3)-Buf(4))*2.0+2.0*Buf(3)*(Buf(3)-Buf(4))+2.0*Buf(4)                             &
                         & **2+6.0*(Buf(5)**2+Buf(6)**2+Buf(7)**2))/3.0
               ELSE
!
!     ROD  CONROD  TUBE
!
                  Buf(3) = fint1
                  Buf(5) = fint1
!
!     M. S. IN TENSION OR COMPRESSION
!
                  IF ( Buf(2)>=0.0 ) THEN
                     IF ( sigt>0.0 .AND. Buf(2)/=0.0 ) Buf(3) = sigt/Buf(2) - 1.0
                  ELSEIF ( sigc/=0.0 ) THEN
                     Buf(3) = (-abs(sigc)/Buf(2)) - 1.0
                  ENDIF
!
!     M. S. IN TORSION
!
                  IF ( Buf(4)/=0.0 .AND. sigs>0.0 ) Buf(3) = sigs/abs(Buf(4)) - 1.0
               ENDIF
            ENDIF
         ENDIF
         GOTO 40
 20      i = 10
         ASSIGN 40 TO iretrn
         spag_nextblock_1 = 4
      CASE (4)
!
!     PRINCIPAL STRESS EQUATIONS FOR 2-DIMENSIONAL ELEMENTS
!
         temp = Buf(i+1) - Buf(i+2)
         Buf(i+7) = sqrt((temp/2.0)**2+Buf(i+3)**2)
         delta = (Buf(i+1)+Buf(i+2))/2.0
         Buf(i+5) = delta + Buf(i+7)
         Buf(i+6) = delta - Buf(i+7)
!
         IF ( andf(isys(61),1)>0 ) Buf(i+7) = sqrt(Buf(i+1)**2+Buf(i+2)**2-Buf(i+1)*Buf(i+2)+3.0*Buf(i+3)**2)
!
         delta = 2.0*Buf(i+3)
         IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
!
            Buf(i+4) = 0.0
            GOTO iretrn
         ELSE
            Buf(i+4) = atan2(delta,temp)*28.6478898
            GOTO iretrn
         ENDIF
!
 40      RETURN
!
!     ERROR PROCESSING FOR DDRMMS
!
 60      n = -1
         file = est
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      n = -2
         file = est
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddrmms
