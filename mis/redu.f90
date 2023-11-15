
SUBROUTINE redu(Cdata,Nx,Ix,Nas,Ias,Nvar,Var,Ipre,Ier)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ibuf , Iout
   CHARACTER*23 Ufm
   COMMON /system/ Ibuf , Iout
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Ier , Ipre , Nas , Nvar , Nx
   INTEGER Cdata(5) , Ias(2,1) , Ix(3,1) , Var(3,6)
!
! Local variable declarations
!
   INTEGER blank , eqs , i , i2 , j , keys(6) , name , nvx
!
! End of declarations
!
!
   DATA keys/4HNAMA , 4HNAMB , 4HNONA , 4HNONB , 4HPREC , 4HBOUN/
   DATA name/4HNAME/ , eqs/4H=   / , blank/4H    /
!
!     INITIALLIZE
!
   DO i = 1 , 6
      Var(1,i) = keys(i)
   ENDDO
!
   Nvar = 18
   DO i = 1 , 2
      Var(2,i) = blank
      Var(3,i) = blank
   ENDDO
   DO i = 3 , 6
      Var(2,i) = -1
      Var(3,i) = 0
   ENDDO
!
!     DECODE COMMAND
!
   i2 = 4
   IF ( Cdata(5)==eqs ) i2 = 6
   IF ( Cdata(1)*2>=i2 ) THEN
!
      Var(2,1) = Cdata(i2)
      Var(3,1) = Cdata(i2+1)
!
      nvx = 6
!
!     FIND NAME
!
      DO i = 1 , Nx
         IF ( Ix(1,i)==name ) THEN
            Var(2,2) = Ix(2,i)
            Var(3,2) = Ix(3,i)
         ELSEIF ( Ix(1,i)/=keys(6) ) THEN
            nvx = nvx + 1
            DO j = 1 , 3
               Var(j,nvx) = Ix(j,i)
            ENDDO
         ELSE
            Var(2,6) = Ix(2,i)
            Var(3,6) = Ix(3,i)
         ENDIF
      ENDDO
      IF ( Var(2,2)/=blank ) THEN
         IF ( Var(3,6)<=0 ) THEN
            WRITE (Iout,99001) Ufm
99001       FORMAT (A23,' 6615, ILLEGAL BOUNDARY SET IDENTIFICATION NUMBER')
            Ier = 1
            GOTO 99999
         ELSE
            IF ( Ipre<=0 .OR. Ipre>2 ) Ipre = 1
!
            Var(3,5) = Ipre
!
!     FIND STRUCTURE NUMBERS, B MAY NOT PRE-EXIST
!
            IF ( Nas/=0 ) THEN
               DO i = 1 , Nas
                  IF ( Var(2,1)==Ias(1,i) .AND. Var(3,1)==Ias(2,i) ) THEN
                     Var(3,3) = i
                  ELSEIF ( Var(2,2)==Ias(1,i) .AND. Var(3,2)==Ias(2,i) ) THEN
                     GOTO 100
                  ENDIF
               ENDDO
            ENDIF
            Nas = Nas + 1
            Var(3,4) = Nas
            Ias(1,Nas) = Var(2,2)
            Ias(2,Nas) = Var(3,2)
            IF ( Var(3,3)==0 ) THEN
               Nas = Nas + 1
               Var(3,3) = Nas
               Ias(1,Nas) = Var(2,1)
               Ias(2,Nas) = Var(3,1)
            ENDIF
            Ier = 0
            Nvar = nvx*3
            RETURN
         ENDIF
      ENDIF
   ENDIF
!
 100  WRITE (Iout,99002) Ufm
99002 FORMAT (A23,' 6614, ILLEGAL OR NON-EXISTANT STRUCTURE NAME USED ','ABOVE')
   Ier = 1
99999 END SUBROUTINE redu
