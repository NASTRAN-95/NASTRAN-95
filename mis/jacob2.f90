
SUBROUTINE jacob2(Elid,Shp,Dshp,Gpth,Bgpdt,Gpnorm,Jacob)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Badj
   DOUBLE PRECISION Detj , Hzta , Psitrn(9) , V1(3) , V2(3) , V3(3)
   INTEGER Ibuf , N1 , Nnode , Nogo , Nout
   COMMON /q4dt  / Detj , Hzta , Psitrn , Nnode , Badj , N1
   COMMON /system/ Ibuf , Nout , Nogo
!
! Dummy argument declarations
!
   INTEGER Elid
   REAL Bgpdt(4,1) , Gpnorm(4,1)
   DOUBLE PRECISION Dshp(1) , Gpth(1) , Jacob(3,3) , Shp(1)
!
! Local variable declarations
!
   DOUBLE PRECISION dum(3) , enk(3) , eps , sk(3) , tgrid(3,8) , thick , tk(3) , val
   INTEGER i , index(3,3) , ipoint , ising , j , jtemp , k
!
! End of declarations
!
!
!     THIS ROUTINE WAS CALLED JACOBD BEFORE, AND WAS THE ONLY ROUTINE
!     THAT ENDED WITH 'DB' AND WAS NOT A BLOCK DATA SUBROUTINE.
!
!     THIS SUBROUTINE CALCULATES JACOBIAN AT EACH GIVEN INTEGRATION
!     POINT FOR QUAD4 POTVIN TYPE ELEMENTS.
!
!     DOUBLE PRECISION VERSION
!
!
   EQUIVALENCE (Psitrn(1),V1(1))
   EQUIVALENCE (Psitrn(4),V2(1))
   EQUIVALENCE (Psitrn(7),V3(1))
!
   DATA eps/1.0D-15/
!
!     INITIALIZE BADJ LOGICAL
!
   Badj = .FALSE.
!
!     COMPUTE THE JACOBIAN AT THIS GAUSS POINT,
!     ITS INVERSE AND ITS DETERMINANT.
!
   DO i = 1 , Nnode
      thick = Gpth(i)
      tgrid(1,i) = Bgpdt(2,i) + Hzta*thick*Gpnorm(2,i)
      tgrid(2,i) = Bgpdt(3,i) + Hzta*thick*Gpnorm(3,i)
      tgrid(3,i) = Bgpdt(4,i) + Hzta*thick*Gpnorm(4,i)
   ENDDO
   DO i = 1 , 2
      ipoint = N1*(i-1)
      DO j = 1 , 3
         Jacob(i,j) = 0.0D0
         DO k = 1 , Nnode
            Jacob(i,j) = Jacob(i,j) + Dshp(k+ipoint)*tgrid(j,k)
         ENDDO
      ENDDO
   ENDDO
   DO j = 1 , 3
      Jacob(3,j) = 0.0D0
      DO k = 1 , Nnode
         jtemp = j + 1
         Jacob(3,j) = Jacob(3,j) + 0.5D0*Gpth(k)*Gpnorm(jtemp,k)*Shp(k)
      ENDDO
   ENDDO
!
!     SAVE THE S, T, AND N VECTORS FOR CALCULATING PSI LATER.
!
   DO i = 1 , 3
      IF ( dabs(Jacob(1,i))<=eps ) Jacob(1,i) = 0.0D0
      sk(i) = Jacob(1,i)
      IF ( dabs(Jacob(2,i))<=eps ) Jacob(2,i) = 0.0D0
      tk(i) = Jacob(2,i)
      IF ( dabs(Jacob(3,i))<=eps ) Jacob(3,i) = 0.0D0
      enk(i) = Jacob(3,i)
   ENDDO
!
!     THE INVERSE OF THE JACOBIAN WILL BE STORED IN
!     JACOB AFTER THE SUBROUTINE INVERD HAS EXECUTED.
!
   CALL inverd(3,Jacob,3,dum,0,Detj,ising,index)
   IF ( ising==1 .AND. Detj>0.0D0 ) THEN
      CALL daxb(sk,tk,V3)
      val = dsqrt(V3(1)*V3(1)+V3(2)*V3(2)+V3(3)*V3(3))
      V3(1) = V3(1)/val
      V3(2) = V3(2)/val
      V3(3) = V3(3)/val
!
!     CROSS ELEMENT Y DIRECTION WITH UNIT VECTOR V3 IN ORDER
!     TO BE CONSISTENT WITH THE ELEMENT COORDINATE SYSTEM.
!
!     NOTE - THIS IS IMPORTANT FOR THE DIRECTIONAL REDUCED
!            INTEGRATION CASES.
!
!
!
      V2(1) = 0.0D0
      V2(2) = 1.0D0
      V2(3) = 0.0D0
!
      CALL daxb(V2,V3,V1)
      val = dsqrt(V1(1)*V1(1)+V1(2)*V1(2)+V1(3)*V1(3))
      V1(1) = V1(1)/val
      V1(2) = V1(2)/val
      V1(3) = V1(3)/val
      CALL daxb(V3,V1,V2)
!
!     REMEMBER THAT V1(1) IS EQUIVALENCED TO PSITRN(1), AND SO ON.
!
!     ELIMINATE SMALL NUMBERS
!
      DO i = 1 , 3
         IF ( dabs(V1(i))<=eps ) V1(i) = 0.0D0
         IF ( dabs(V2(i))<=eps ) V2(i) = 0.0D0
         IF ( dabs(V3(i))<=eps ) V3(i) = 0.0D0
      ENDDO
   ELSE
      WRITE (Nout,99001) Elid
!
99001 FORMAT ('0*** USER FATAL ERROR, ELEMENT ID =',I10,'  HAS BAD OR REVERSE GEOMETRY')
      Nogo = 1
      Badj = .TRUE.
   ENDIF
!
   RETURN
END SUBROUTINE jacob2
