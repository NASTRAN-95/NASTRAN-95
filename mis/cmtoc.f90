
SUBROUTINE cmtoc
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Buf1 , Buf2 , Buf3 , Buf4 , Casecc , Conect , Conset , Geom4 , Origin(7,3) , Restct(7,7) , Scbdat , Scconn , Scmcon , Scr1 ,&
      & Scr2 , Scsfil , Toler , Tran
   INTEGER Buf5 , Combo(7,5) , Iauto , Ihdr(96) , Inpt , Iprint , Isort , Ititl(96) , Lcore , Mcon , Npsub , Outt , Score , Sctoc , &
         & Xxx , Z(1)
   LOGICAL Tocopn
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint , Tocopn
   COMMON /output/ Ititl , Ihdr
   COMMON /system/ Xxx
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER aaa(2) , i , ia , ib , ihed(96) , itest , itot , kdh , name(2) , nheqss , nt , nwds
   INTEGER andf , rshift
   LOGICAL print
   EXTERNAL andf , rshift
!
! End of declarations
!
!
!     THIS SUBROUTINE GENERATES A TABLE OF CONTENTS FOR A COMBINE
!     OPERATION. FOR EACH PSEUDO-STRUCTURE IT LISTS THE NAME, NUMBER
!     OF COMPONENTS, AND EACH COMPONENT BASIC SUBSTRUCTURE NAME.
!     THIS DATA IS THEN WRITTEN ON SCRATCH FILE SCTOC.
!
   DATA ihed/7*4H     , 4HP S  , 4HE U  , 4HD O  , 4HS T  , 4HR U  , 4HC T  , 4HU R  , 4HE    , 4HT A  , 4HB L  , 4HE    , 4HO F  , &
       &4H  C  , 4HO N  , 4HT E  , 4HN T  , 4HS    , 15*4H     , 4H PSE , 4HUDO- , 4H     , 4H   N , 4HO. O , 4HF    , 26*2H   ,    &
       &4HSTRU , 4HCTUR , 4HE    , 4H COM , 4HPONE , 4HNTS  , 4H   - , 4H---- , 4H---- , 4H- CO , 4HMPON , 4HENT  , 4HNAME ,        &
      & 4HS -- , 4H---- , 4H---- , 4H-    , 8*4H    /
   DATA aaa/4HCMTO , 4HC   /
   DATA nheqss/4HEQSS/
!
   print = .FALSE.
   IF ( andf(rshift(Iprint,1),1)==1 ) print = .TRUE.
   Tocopn = .TRUE.
   itot = 0
   DO i = 1 , 96
      Ihdr(i) = ihed(i)
   ENDDO
   IF ( print ) CALL page
   CALL open(*100,Sctoc,Z(Buf5),1)
   DO i = 1 , Npsub
      name(1) = Combo(i,1)
      name(2) = Combo(i,2)
      CALL sfetch(name,nheqss,1,itest)
      CALL suread(Z(Score),-1,nwds,itest)
      Z(Score) = name(1)
      Z(Score+1) = name(2)
      CALL write(Sctoc,Z(Score),3,0)
      itot = itot + 3
      ia = Score
      ib = Score + 2
      IF ( print ) WRITE (Outt,99001) (Z(kdh),kdh=ia,ib)
99001 FORMAT (34X,2A4,6X,I4)
      Combo(i,5) = Z(Score+2)
      nwds = nwds - 4
      ia = Score + 4
      ib = ia + nwds - 1
      nt = (ib-ia+1)/8
      IF ( nt==0 ) nt = 1
      IF ( print ) CALL page2(nt)
      IF ( print ) WRITE (Outt,99002) (Z(kdh),kdh=ia,ib)
99002 FORMAT (1H+,57X,2X,2A4,2X,2A4,2X,2A4,2X,2A4,/(58X,2X,2A4,2X,2A4,2X,2A4,2X,2A4))
      itot = itot + nwds
      CALL write(Sctoc,Z(Score+4),nwds,1)
   ENDDO
   CALL close(Sctoc,1)
   CALL open(*100,Sctoc,Z(Buf5),0)
!
!     DETERMINE WHETHER TO CLOSE FILE
!
   IF ( itot<=Xxx ) RETURN
   Tocopn = .FALSE.
   CALL close(Sctoc,1)
   RETURN
!
 100  CALL mesage(-1,Sctoc,aaa)
END SUBROUTINE cmtoc
