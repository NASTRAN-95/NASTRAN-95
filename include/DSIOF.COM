!
! PARAMETER definitions
!
   INTEGER MAXPRI , MAXFCB
   PARAMETER (MAXPRI=80,MAXFCB=89)
!
! COMMON variable declarations
!
   CHARACTER*2 Dev(10)
   INTEGER Fcb(17,MAXFCB) , Ibasbf , Iblock , Idatad , Idbadr , Idbbas , Idbdir , Idbfre , Idblen , Idsc , Idsch , Idsct , Idseb ,  &
         & Idsef , Idsn , Idsp , Idsrh , Idsrt , Idssb , Idssd , Idsse , Idssh , Idsst , Idsx , Ieor , Ifilex , Indbas , Indcbp ,   &
         & Indclr , Inddir , Ioblk , Iocode , Ioerr , Iprvop , Iretrn , Irword , Lasnam , Lcw , Lenalc , Lendsp , Lenopc , Lenwpb , &
         & Lhalf , Lwords , Maske1 , Maske2 , Maske3 , Maske4 , Maskh1 , Maskh2 , Maskq1 , Maskq2 , Maskq3 , Maskq4 , Maxalc ,      &
         & Maxblk , Maxdsk , Maxdsn , Mcbmas , Mulq1 , Mulq2 , Mulq3 , Name , Nbfz , Nblock , Nbuff , Nlr , Numcls , Numdev ,       &
         & Numopn , Numrea , Numwri , Nwords , Nwrdel(4)
   CHARACTER*80 Mdsnam(MAXFCB)
   COMMON /dbm   / Idbbas , Idbfre , Idbdir , Indbas , Indclr , Indcbp , Nblock , Lenalc , Iocode , Ifilex , Name , Maxalc ,        &
                 & Maxblk , Maxdsk , Idblen , Idbadr , Ibasbf , Inddir , Numopn , Numcls , Numwri , Numrea , Lenopc
   COMMON /dsdevc/ Numdev , Dev
   COMMON /dsio  / Ieor , Ioerr , Iprvop , Iretrn , Irword , Idatad , Idsn , Lcw , Lwords , Maskh1 , Maskh2 , Maske1 , Maske2 ,     &
                 & Maske3 , Maske4 , Maxdsn , Nwords , Nbuff , Ioblk , Nbfz , Nlr , Maskq1 , Maskq2 , Maskq3 , Maskq4 , Idsx ,      &
                 & Idsp , Idsc , Idsrh , Idsrt , Idssb , Idsse , Idsch , Idsct , Idssh , Idsst , Idssd , Idseb , Idsef , Iblock ,   &
                 & Lasnam , Mcbmas , Mulq1 , Mulq2 , Mulq3 , Lhalf , Lendsp , Lenwpb , Nwrdel
   COMMON /dsname/ Mdsnam
   COMMON /fcb   / Fcb
!
! End of declarations
!
