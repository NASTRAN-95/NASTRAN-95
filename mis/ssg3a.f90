
SUBROUTINE ssg3a(A,Lll,B,X,Sr1,Sr2,Itr1,Res)
   IMPLICIT NONE
   REAL Core(1)
   DOUBLE PRECISION Dcore(1) , Dx(2)
   INTEGER Filb(7) , Fill(7) , Fillt(7) , Filx(7) , I , Ieol , Ieor , Iepsi , Ik , Incur , Iprec , Ires , Isign , Itb , J ,         &
         & Ksystm(55) , N , Nskip , Nz , Prec , Sysbuf
   COMMON /blank / N , Ires , Nskip , Iepsi
   COMMON /fbsx  / Fill , Fillt , Filb , Filx , Nz , Prec , Isign
   COMMON /system/ Ksystm
   COMMON /unpakx/ Itb , I , J , Incur
   COMMON /zntpkx/ Dx , Ik , Ieol , Ieor
   COMMON /zzzzzz/ Core
   INTEGER A , B , Itr1 , Lll , Res , Sr1 , Sr2 , X
   DOUBLE PRECISION dnom , dnum
   REAL epsi
   INTEGER ipm , l , name(2) , nlen , nload
   INTEGER korsz
!
!     SSG3A SOLVES AX = B USING A = L*LT
!
!     ON OPTION COMPUTES RESIDUAL VECTOR RES = A*X - B
!     AND EPSI= X(T)*RES/B(T)*X
!
   EQUIVALENCE (Core(1),Dcore(1)) , (Ksystm(1),Sysbuf) , (Ksystm(55),Iprec)
   DATA name/4HSSG3 , 4HA   /
!
   Fill(1) = Lll
   CALL rdtrl(Fill)
   IF ( Fill(1)<=0 ) CALL mesage(-1,Lll,name)
   Filb(1) = B
   CALL rdtrl(Filb)
   nload = Filb(2)
   nlen = Filb(3)
   Isign = 1
   Prec = 2
   Nz = korsz(Core)
   DO I = 2 , 7
      Filx(I) = Filb(I)
   ENDDO
   Filx(1) = X
!
!     SAVE DISPLACEMENT VECTOR IN DOUBLE PRECISION
!
   Filx(5) = 1
   IF ( Filb(5)>2 ) Filx(5) = 3
   Filx(5) = Filx(5) + Iprec - 1
   CALL fbs(Core,Core)
   CALL wrttrl(Filx)
   IF ( Itr1>=0 ) THEN
      Fill(1) = Res
      CALL rdtrl(Fill)
      IF ( Fill(1)>0 ) THEN
!
!     COMPUTE RESIDUAL VECTOR
!
         CALL ssg2b(A,X,B,Res,0,2,-2,Sr1)
!
!     COMPUTE EPSI
!
         Nz = Nz - Sysbuf
         CALL gopen(X,Core(Nz+1),0)
         Nz = Nz - Sysbuf
         CALL gopen(Res,Core(Nz+1),0)
         Nz = Nz - Sysbuf
         CALL gopen(B,Core(Nz+1),0)
         IF ( Nz<2*nlen ) THEN
            CALL mesage(-8,0,name)
            GOTO 99999
         ELSE
            Itb = 2
            Incur = 1
            I = 1
            J = nlen
            DO l = 1 , nload
               CALL unpack(*5,X,Core)
               dnum = 0.0D0
               dnom = 0.0D0
               CALL intpk(*10,Res,0,2,0)
               DO WHILE ( Ieol==0 )
                  CALL zntpki
                  dnum = dnum + Dx(1)*Dcore(Ik)
               ENDDO
               CALL intpk(*15,B,0,2,0)
               DO WHILE ( Ieol==0 )
                  CALL zntpki
                  dnom = dnom + Dx(1)*Dcore(Ik)
               ENDDO
               epsi = dnum/dnom
               GOTO 20
 5             CALL fwdrec(*100,Res)
 10            CALL fwdrec(*200,B)
 15            epsi = 0.0
 20            CALL mesage(35,Nskip+l-1,epsi)
               IF ( abs(epsi)>=1.0E-3 ) THEN
                  Iepsi = -1
                  CALL mesage(58,1.0E-3,Nskip+l-1)
               ENDIF
            ENDDO
            CALL close(X,1)
            CALL close(Res,1)
            CALL close(B,1)
         ENDIF
      ENDIF
   ENDIF
   RETURN
 100  DO
      ipm = Res
!
      CALL mesage(-1,ipm,name)
   ENDDO
 200  ipm = B
   CALL mesage(-1,ipm,name)
   GOTO 100
!
99999 RETURN
END SUBROUTINE ssg3a
