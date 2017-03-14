program SPI
  use tools
  implicit none
  character(len=256) :: filename
  real, allocatable :: buffer(:,:)
  integer :: nxny, ifile, i
  real :: eps, undef
  double precision, allocatable :: meany(:,:), stdvy(:,:), vals(:,:,:), fcVals(:), SPIval(:)
  real, allocatable :: SPIvalOut(:)
  character(len=70) :: errorMessage
  integer :: numfiles, daycounter,iyear,imon,iday,numOfDays, nyears
  character(len=1),allocatable                                      :: msg(:)

  nyears = 30
  nxny = 12*16
  numfiles = 10993
  eps = 1.0e-20
  undef = -9999.0
  numOfDays = 31
  !filename="eraint_precip_daily_clim.grib"
  allocate(vals(nxny,366,nyears)); vals = 1.0e-20
  allocate(meany(nxny,366)); meany = 1.0e-20
  allocate(stdvy(nxny,366)); stdvy = eps
  daycounter=1
  !do ifile=1,numfiles
  filename="erai_228_19810101_daily_m_1.grib"
  ifile = 1
  do iyear=1981,2010
  do imon=1,12
    !if (imon == 1) then
      !if (iyear == 2011) then
      !  exit
      !endif
      !iyear = iyear + 1
      !write(*,*) "BLAAT 28:"
    !endif
    numOfDays = daysInMonth(imon,iyear)

    do iday=1,numOfDays
   
  
      if (daycounter == 366 .and. iday == 1) then
        if (.not. isLeapYear(iyear-1)) then
          daycounter = 1
        endif
      endif
      if (isLeapYear(iyear-1) .and. iday == 1 .and. daycounter == 367) then
        daycounter = 1
      endif

      write(filename,'(a,i4.4,i2.2,i2.2,a,i1.1,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,".grib"
      
      if (daycounter > 9) then
        write(filename,'(a,i4.4,i2.2,i2.2,a,i2.2,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,".grib"
      endif
      if (daycounter > 99) then
        write(filename,'(a,i4.4,i2.2,i2.2,a,i3.3,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,".grib"
      endif
      write(*,*) filename
    
      call getGribFile(filename,0,buffer,msg)
   !   write(*,*) "BLAAT 60: ", buffer*1000
      vals(:,daycounter,iyear-1981+1) = buffer(:,1)*1000
      meany(:,daycounter) = meany(:,daycounter) + buffer(:,1) / 30
      daycounter = daycounter + 1
      ifile = ifile + 1
    enddo
  enddo
 
  enddo
 ! write(*,*) "BLAAT 68: ", meany(100,:); call flush(6) 
  deallocate(buffer)
 
  do iyear=1,nyears
    stdvy(:,:) = sqrt((vals(:,:,iyear)-meany(:,:)) * (vals(:,:,iyear)-meany(:,:)) / (nyears-1))
  enddo
  where (stdvy < eps) 
    stdvy = eps
  endwhere
  !write(*,*) "BLAAT 74: ", stdvy(100,:)

  !write(*,*) field(5000,:)
 
  allocate(fcVals(nxny)); fcVals = 0.0
  allocate(SPIval(nxny)); SPIval = 0.0
  allocate(SPIvalOut(nxny)); ; SPIvalOut = undef
  daycounter=1
  !do ifile=1,numfiles
  filename="erai_228_19810101_daily_m_1.grib"

  do iyear=2015,2016
  do imon=1,12
    numOfDays = daysInMonth(imon,iyear)

    do iday=1,numOfDays


      if (daycounter == 366 .and. iday == 1) then
        if (.not. isLeapYear(iyear-1)) then
          daycounter = 1
        endif
      endif
      if (isLeapYear(iyear-1) .and. iday == 1 .and. daycounter == 367) then
        daycounter = 1
      endif

      write(filename,'(a,i4.4,i2.2,i2.2,a,i1.1,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,".grib"

      if (daycounter > 9) then
        write(filename,'(a,i4.4,i2.2,i2.2,a,i2.2,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,".grib"
      endif
      if (daycounter > 99) then
        write(filename,'(a,i4.4,i2.2,i2.2,a,i3.3,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,".grib"
      endif

      if (imon >= 9 .or. imon <= 3) then
        write(*,*) filename

        call getGribFile(filename,0,buffer,msg)
        fcVals(:) = buffer(:,1)*1000
        !write(*,*) "BLAAT 111: ", stdvy(:,daycounter); call flush(6)
        !write(*,*) " BLAAT 112: ",(fcVals - meany(:,daycounter)); call flush(6)
        !write(*,*) "BLAAT 112b: ", stdvy(:,daycounter) 
        SPIval = (fcVals - meany(:,daycounter)) / (stdvy(:,daycounter)/sqrt(nyears-1))
        SPIvalOut = real(SPIval,kind=4)

        write(filename,'(a,i4.4,i2.2,i2.2,a,i1.1,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,"_out.grib"

      if (daycounter > 9) then
        write(filename,'(a,i4.4,i2.2,i2.2,a,i2.2,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,"_out.grib"
      endif
      if (daycounter > 99) then
        write(filename,'(a,i4.4,i2.2,i2.2,a,i3.3,a)') trim(filename(1:9)),iyear,imon,iday,"_daily_m_",daycounter,"_out.grib"
      endif

        call writeGribFile(filename,SPIvalOut,msg,2,1)
        write(*,*) "BLAAT 113: ", SPIval; call flush(6)
      endif
        daycounter = daycounter + 1

    enddo
  enddo

  enddo

  





  deallocate(meany)
  deallocate(stdvy)
  deallocate(vals)



  contains

   



    !---------------------------------------------------------------------------------------!
    !   SUBROUTINE getGribFile                                          D.Decremer May 2016 !
    !                                                                                       !
    !   PURPOSE:                                                                            !
    !       reads a GRIB files with any number of fields and returns a 2D array with        !
    !         nxnxy elements in the first dim and the number of msg in the 2nd dim.         !
    !                                                                                       !
    !   INPUT:                                                                              !
    !     filename:    filepath and name of the grib file                                   !
    !  verbose_opt:    verbosity level                                                      !
    !                                                                                       !
    !   OUTPUT:                                                                             !
    !     metaFile:    output structure of type gribFile containing data and metadata       !
    !                     with dimensions (iens,imon,ilev,ivar,idate,imsg)                  !
    !                                                                                       !
    !   EXAMPLE USAGE:                                                                      !
    !     type(gribFile) :: gribData                                                        !
    !     character(len=256) :: filename                                                    !
    !     integer :: imsg                                                                   !
    !     call getGribFile(filename,gribData,2)                                             !
    !     call gribData%printMeta !check what we extracted                                  !
    !     ! do something with your data                                                     !
    !     do imsg=1,gribData%nmsg                                                           !
    !       write(*,*) gribData%data(:,imsg)                                                !
    !     enddo                                                                             !
    !     ! clear up some more memory                                                       !
    !     call gribData%cleanMeta                                                           !
    !                                                                                       !
    !     NOTE:                                                                             !
    !       data is allocated inside this subroutine. The Fortran 2003 standard specifies   !
    !       automatic deallocation of allocatable arrays. It's best to deallocate everything!
    !       after use by using the provided clean functions.                                !
    !---------------------------------------------------------------------------------------!
    subroutine getGribFile(filename,verbose_opt,data,msg)
      use grib_api
      implicit none

      character(len = 256), intent(in)                                  :: filename
      integer, optional, intent(in)                                     :: verbose_opt

!       real, dimension(:,:), allocatable                :: data
      real, dimension(:,:), allocatable, intent(inout)                  :: data

      integer, dimension(:,:), allocatable                              :: metaArray
      integer, dimension(:), allocatable                                :: igrib
      integer                                                           :: ifile, imsg, nmsg, iret
      character(len=12),dimension(:),allocatable                        :: shortNames
      integer                                                           :: nxny, counter, nx, ny
      real,dimension(:),allocatable                                     :: lats,lons
      real                                                              :: missingValue
      character(len=1), dimension(:), allocatable                       :: message
      character(len = 3)                                                :: fileType
      integer(kind=kindOfSize)                                          :: byte_size
      integer                                                           :: verbose, fcmon
      character(len=1),allocatable                                      :: msg(:)


      if (.not. present(verbose_opt)) then
        verbose = 0
      else
        verbose = verbose_opt
      endif

      ! Make sure we can open the file
      call grib_open_file(ifile,filename,'r',iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_open_file:',errorMessage
      if (iret  /=  0) then
        allocate(data(1,1))
        call aborf6
      endif

      call grib_count_in_file(ifile,nmsg)

      ! put the messages in memory
      allocate(igrib(nmsg))
      igrib = -1
      do imsg=1,nmsg
        call grib_new_from_file(ifile,igrib(imsg), iret); call getGribError(iret,errorMessage); !if (verbose >= 3) write(*,*) 'grib_new_from_file:',errorMessage
        if (iret /= 0) then
          write(*,'(a,i6.1,a)') 'ERROR in extractAllGribdata: Message ',imsg,' was not retrieved successfully.'
          call aborf6
        endif
      enddo

      ! we can close the file
      call grib_close_file(ifile,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_close_file:',errorMessage

      ! Now we can process the data in memory

      ! extract the first message which will serve as "template" for creating grib files
      call grib_get(igrib(1),'Ni',nx,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_get Ni:',errorMessage
      call grib_get(igrib(1),'Nj',ny,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_get Nj:',errorMessage
      call grib_get(igrib(1),'numberOfPoints',nxny,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_get numberOfPoints:',errorMessage
      call grib_get(igrib(1),'missingValue',missingValue,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_get missingValue:',errorMessage
      call grib_get_message_size(igrib(1), byte_size,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_get_message_size:',errorMessage
      allocate(message(byte_size), stat=iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'allocate message:',errorMessage
      call grib_copy_message(igrib(1),message,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_copy_message:',errorMessage

      ! allocate the data that will go in the gribFile type
      allocate(lats(nxny)); lats = undef
      allocate(lons(nxny)); lons = undef
      allocate(metaArray(5,nmsg)); metaArray = -999
      allocate(shortNames(nmsg)); shortNames = '?'
      if (.not.allocated(data)) allocate(data(nxny,nmsg)); 
      data = undef
      do imsg=1,nmsg

        ! grab the data
        call grib_get_data(igrib(imsg),lats,lons,data(:,imsg),iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_get_data:',errorMessage

        ! modify the undefined values
        if (any(mask=abs(data - missingValue)  >  eps)) then
          counter=counter+1
          if (any(mask=abs(data - missingValue)  <=  eps)) then
            where(abs(data - missingValue)  <=  eps)
              data = undef
            end where
          endif
        elseif (all(mask=abs(data - missingValue)  <=  eps)) then
          write(*,*) 'ERROR in gribFun/getGribFile: empty data not placed.'
          call aborf6
        endif
        if (imsg  ==  1) then
          ! extract the first message which will serve as "template" for creating grib files
          call grib_get_message_size(igrib(1),byte_size,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_get_message_size:',errorMessage
          allocate(msg(byte_size),stat=iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'allocate message:',errorMessage
          call grib_copy_message(igrib(1),msg,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_copy_message:',errorMessage
          
        else

        ! we don't need you any more, you grib message!
        call grib_release(igrib(imsg),iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_realease:',errorMessage 
        endif
      enddo



 








     
      return
    end subroutine getGribFile






    !---------------------------------------------------------------------------------------!
    !   SUBROUTINE writeGribFile                                        D.Decremer May 2016 !
    !                                                                                       !
    !   PURPOSE:                                                                            !
    !     create a GRIB file with the contents of a gribFile structure, and using a grib    !
    !     message as template. The grib message comes from a previously extracted grib file !
    !                                                                                       !
    !   INPUT:                                                                              !
    !     filename:  filepath and name of the grib file                                     !
    !     metaFile:   structure of type gribfile which is returned by                       !
    !                   GetGribFile or getGribDataWRequest                                  !
    !  verbose_opt:  verbosity level (0=no verbosity, 1=minimum, 2=basic log, 3=full log)   !
    !  edition_opt:  wished grib version (1 or 2)                                           !
    !    accum_opt:  optional modifier for using with accumulated keys                      !
    !                   (temporary feature, will be removed in future gribfun release)      !
    !                                                                                       !
    !   OUTPUT: none                                                                        !
    !                                                                                       !
    !   EXAMPLE USAGE:                                                                      !
    !     integer :: ie, is, iyr, imsg, idate, nmsg, istat, nmon, nens                      !
    !     type(gribFile) :: gribOut, gribIn                                                 !
    !     character(len=12),allocatable,dimension(:) :: shortNames                          !
    !     integer, allocatable, dimension(:,:) :: metaArray                                 !
    !     real, allocatable, dimension(:,:) :: fields                                       !
    !     ! prepare metadata for a new gribFile object                                      !
    !     nmsg = nens * nmon                                                                !
    !     allocate(metaArray(5,nmsg),stat=istat)                                            !
    !     allocate(shortNames(nmsg),stat=istat)                                             !
    !     fcmonthsArray = makeForecastSteps(11,5,1981)                                         !
    !     idate = makeDateFormat(1981,11,01)                                                !
    !     call paramIds2shortNames([(129,imsg=1,nmsg)],shortNames)                          !
    !     imsg = 1                                                                          !
    !     do is=1,nmon                                                                      !
    !       do ie=1,nens                                                                    !
    !         metaArray(:,imsg) = [ie-1,fcmonthsArray(is),ilev,ipar,idate]                     !
    !         fields(:,imsg) = whateveryoulike                                              !
    !         imsg = imsg + 1                                                               !
    !       enddo                                                                           !
    !     enddo                                                                             !
    !     gribOut = gribFile ( &                                                            !
    !       nmsg, &                                                                         !
    !       gribIn%nx, &                                                                    !
    !       gribIn%ny, &                                                                    !
    !       gribIn%nxny, &                                                                  !
    !       [gribIn%lats], &                                                                !
    !       [gribIn%lons], &                                                                !
    !       -9999.0, &                                                                      !
    !       [gribIn%message], &                                                             !
    !       metaArray, &                                                                    !
    !       shortNames, &                                                                   !
    !       fields &                                                                        !
    !     )                                                                                 !
    !     call writeGribFile(yofile,gribOut,0,2)                                            !
    !     call gribIn%cleanMeta                                                             !
    !     call gribOut%cleanMeta                                                            !
    !                                                                                       !
    !    NOTE:                                                                              !
    !       This routine contains a hack for the origin of the grib file to avoid problems  !
    !         with missing local definition tables. (hack suggested by Shahram Najm)        !
    !       For example, if the data is from lfpw, the key subCentre becomes lfpw, while    !
    !       the key centre becomes ecmf.                                                    !
    !                                                                                       !
    !    Questions to ask Shahram:                                                          !
    !                                                                                       !
    !    1) Which of the following options is most appropriate/clean (keeping grib file exchanges with external colleagues in mind)?
    !      clone grib1 msg, set edition to 2, and set localDefinitionNumber to 15
    !    OR
    !      clone grib1 msg, set edition to 2, and apply/select template 1
    !    OR
    !      clone a sample from /usr/local/apps/grib_api/1.14.5/GNU/4.8.1/share/grib_api/samples, apply/select template 1, and copy over all needed keys from the old grib1 msg onto the new grib2 msg (this is not all too easy because in fortran, there is no way to enquire automatically whether a key is real, double, integer or a string)
    !
    !    2) How can I apply a template? I couldn't find an example on https://software.ecmwf.int/wiki/display/GRIB/Fortran+90.
    !
    !    3) To create a new msg, is it better to use grib_clone or grib_copy_message?
    !                                                                                       !
    !---------------------------------------------------------------------------------------!
    subroutine writeGribFile(filename,data,msg,verbose_opt,edition_opt)
      use grib_api
      implicit none

      character(len = 256),intent(in)                       :: filename
      integer, intent(in),optional                          :: verbose_opt
      integer                                               :: verbose
      integer, optional, intent(in)                         :: edition_opt
      integer                                               :: edition
      real, allocatable, intent(in)                         :: data(:)
      integer                                               :: centre
      integer                                               :: iret
      integer                                               :: igrib, outfile
      integer                                               :: imsg
      character(len=1),allocatable                       :: msg(:)


      if (.not.(present(edition_opt))) then
        edition = 1
      else
        edition = edition_opt
      endif

      if (.not. present(verbose_opt)) then
        verbose = 0
      else
        verbose = verbose_opt
      endif

      if (verbose >= 3) write(*,*)

      ! STEP-1: open output file and load a GRIB message from a sample “GRIB1”
      call grib_open_file(outfile, filename,'w',iret); call getGribError(iret,errorMessage); if (verbose >= 3) then; write(*,*); write(*,*) 'grib_open_file:',errorMessage; write(*,*); endif
      
        call grib_new_from_message(igrib, msg,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_new_from_message:',errorMessage

        ! STEP-2: set the new GRIB message
        if (edition == 1) then
          call grib_set(igrib,'stepType','avg',iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set stepType:',errorMessage ! TODO only for erai avgua, put if statement
          call grib_set(igrib,'localDefinitionNumber',16,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set localDefinitionNumber:',errorMessage
        elseif (edition == 2) then
          call grib_set(igrib,'stepType','avg',iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set stepType:',errorMessage ! TODO only for erai avgua, put if statement
          call grib_set(igrib,'localDefinitionNumber',15,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set localDefinitionNumber:',errorMessage
        endif
        call grib_set(igrib,'edition',edition,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set edition:',errorMessage



        call grib_set(igrib,'bitmapPresent', 1,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set bitmapPresent:',errorMessage
        ! Tells the GRIB-API what the current missing value is.
        ! This value is replaced by the default value in the library, i.e. -9999.0,
        ! when the grib message is written into the output file
        call grib_set(igrib,'missingValue',undef,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set missingValue:',errorMessage

       
     
          call grib_set(igrib,'typeOfLevel','surface',iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set typeOfLevel:',errorMessage
    

        call grib_set(igrib,'values', pack(data, mask=.true.),iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_set values:',errorMessage ! Set values as 1D real array of size nb_values

        ! STEP-4: write modified message to a file
        call grib_write(igrib,outfile,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_write:',errorMessage
        call grib_release(igrib,iret); call getGribError(iret,errorMessage); if (verbose >= 3) write(*,*) 'grib_release:',errorMessage; if (verbose >= 3) write(*,*)
     

      call grib_close_file(outfile,iret); call getGribError(iret,errorMessage); if (verbose >= 3) then; write(*,*); write(*,*) 'grib_close:',errorMessage; write(*,*); endif

      return

    end subroutine writeGribFile






    subroutine paramIds2shortNames(paramIds,shortNames)
      implicit none
      integer,dimension(:),intent(in)            :: paramIds
      character(len=12),dimension(:),intent(out) :: shortNames
      integer                                    :: n,i

      n = size(paramIds,dim=1)
      do i=1,n
        select case (paramIds(i))
          case (31)
            shortNames(i) = 'ci'
          case (34)
            shortNames(i) = 'sst'
          case (129)
            shortNames(i) = 'z'
          case (130)
            shortNames(i) = 't'
          case (131)
            shortNames(i) = 'u'
          case (132)
            shortNames(i) = 'v'
          case (139)
            shortNames(i) = 'stl1'
          case (141)
            shortNames(i) = 'sd'
          case (151)
            shortNames(i) = 'msl'
          case (164)
            shortNames(i) = 'tcc'
          case (165)
            shortNames(i) = '10u'
          case (166)
            shortNames(i) = '10v'
          case (167)
            shortNames(i) = '2t'
          case (170)
            shortNames(i) = 'stl2'
          case (183)
            shortNames(i) = 'stl3'
          case (228)
            shortNames(i) = 'tp'
          case (236)
            shortNames(i) = 'stl4'
          case (172228)
            shortNames(i) = 'tp'
          case (228228)
            shortNames(i) = 'tp'
          case default
            shortNames(i) = '?'
        endselect
      enddo
    end subroutine paramIds2shortNames




    subroutine shortNames2paramIds(shortNames,paramIds)
      implicit none
      character(len=12),dimension(:),intent(in) :: shortNames
      integer,dimension(:),intent(out)          :: paramIds
      integer                                   :: n,i

      n = size(shortNames,dim=1)
      do i=1,n
        select case (shortNames(i))
          case ('ci')
            paramIds(i) = 31
          case ('sst')
            paramIds(i) = 34
          case ('z')
            paramIds(i) = 129
          case ('t')
            paramIds(i) = 130
          case ('u')
            paramIds(i) = 131
          case ('v')
            paramIds(i) = 132
          case ('stl1')
            paramIds(i) = 139
          case ('sd')
            paramIds(i) = 141
          case ('msl')
            paramIds(i) = 151
          case ('tcc')
            paramIds(i) = 164
          case ('10u')
            paramIds(i) = 165
          case ('10v')
            paramIds(i) = 166
          case ('2t')
            paramIds(i) = 167
          case ('stl2')
            paramIds(i) = 170
          case ('stl3')
            paramIds(i) = 183
          case ('tp')
            paramIds(i) = 228
          case ('stl4')
            paramIds(i) = 236
          case default
            paramIds(i) = -1
        endselect
      enddo
    end subroutine shortNames2paramIds




    subroutine getGribError(errcode,errmsg)
      use grib_api
      implicit none

      integer, intent(in) :: errcode
      character(len=70),intent(out) :: errmsg

      if (errcode /= 0) then
        call grib_get_error_string(errcode,errmsg)
      else
        errmsg = "OK"
      endif
!      return
!
!      select case (errcode)
!        case (0)
!          errmsg = 'OK'
!        case (-1)
!          errmsg = 'End of resource reached'
!        case (-2)
!          errmsg = 'Internal error'
!        case (-3)
!          errmsg = 'Passed buffer is too small'
!        case (-4)
!          errmsg = 'Function not yet implemented'
!        case (-5)
!          errmsg = 'Missing 7777 at end of message'
!        case (-6)
!          errmsg = 'Passed array is too small'
!        case (-7)
!          errmsg = 'File not found'
!        case (-8)
!          errmsg = 'Code not found in code table'
!        case (-9)
!          errmsg = 'Array size mismatch'
!        case (-10)
!          errmsg = 'Key/value not found'
!        case (-11)
!          errmsg = 'Input output problem'
!        case (-12)
!          errmsg = 'Message invalid'
!        case (-13)
!          errmsg = 'Decoding invalid'
!        case (-14)
!          errmsg = 'Encoding invalid'
!        case (-15)
!          errmsg = 'Code cannot unpack because of string too small'
!        case (-16)
!          errmsg = 'Problem with calculation of geographic attributes'
!        case (-17)
!          errmsg = 'Out of memory'
!        case (-18)
!          errmsg = 'Value is read only'
!        case (-19)
!          errmsg = 'Invalid argument'
!        case (-20)
!          errmsg = 'Null handle'
!        case (-21)
!          errmsg = 'Invalid section number'
!        case (-22)
!          errmsg = 'Value cannot be missing'
!        case (-23)
!          errmsg = 'Wrong message length'
!        case (-24)
!          errmsg = 'Invalid key type'
!        case (-25)
!          errmsg = 'Unable to set step'
!        case (-26)
!          errmsg = 'Wrong units for step (step must be integer)'
!        case (-27)
!          errmsg = 'Invalid file id'
!        case (-28)
!          errmsg = 'Invalid grib id'
!        case (-29)
!          errmsg = 'Invalid index id'
!        case (-30)
!          errmsg = 'Invalid iterator id'
!        case (-31)
!          errmsg = 'Invalid keys iterator id'
!        case (-32)
!          errmsg = 'Invalid nearest id'
!        case (-33)
!          errmsg = 'Invalid order by'
!        case (-34)
!          errmsg = 'Missing a key from the fieldset'
!        case (-35)
!          errmsg = 'The point is out of the grid area'
!        case (-36)
!          errmsg = 'Concept no match'
!        case (-37)
!          errmsg = 'Definitions files not found'
!        case (-38)
!          errmsg = 'Wrong type while packing'
!        case (-39)
!          errmsg = 'End of resource'
!        case (-40)
!          errmsg = 'Unable to code a field without values'
!        case (-41)
!          errmsg = 'Grid description is wrong or inconsistent'
!        case (-42)
!          errmsg = 'End of index reached'
!        case (-43)
!          errmsg = 'Null index'
!        case (-44)
!          errmsg = 'End of resource reached when reading message'
!        case (-45)
!          errmsg = 'An internal array is too small'
!        case (-46)
!          errmsg = 'Message is too large for the current architecture'
!        case (-47)
!          errmsg = 'Constant field'
!        case (-48)
!          errmsg = 'Switch unable to find a matching case'
!        case (-49)
!          errmsg = 'Underflow'
!        case (-50)
!          errmsg = 'Message malformed'
!        case (-51)
!          errmsg = 'Index is corrupted'
!        case (-52)
!          errmsg = 'Invalid number of bits per value'
!        case (-53)
!          errmsg = 'Edition of two messages is different'
!        case (-54)
!          errmsg = 'Value is different'
!        case (-55)
!          errmsg = 'Invalid key value'
!        case default
!          errmsg = 'Error unknown in gribfun.f90 module'
!      endselect
!
!!      if (errcode /= 0) then
!!        errmsg = ' ERROR in gribfun: ' // errmsg
!!      endif
      errmsg = ' ' // trim(errmsg) // '.'
    end subroutine getGribError





end program SPI
