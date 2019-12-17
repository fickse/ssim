 # run Art

######################################################
# Parameters template

dpar <- list(

    ## Number of cores
    ncores = 20,
    
    ## Output directory -- now must be changed here
    outdir = "out",

    ## Treatment areas
    polygons = 'polys.RData',
    polygon_id_column = 'polyID',

    ##log file
    logfile = paste0('dart_logfile_', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'), '.txt'),

    ## Output Prefix
    prefix = 'ID_',
    
    ## Neighborhood radius (m)
    rad = 3000,

    ## EC buffer (+/- this value determines cutoff for pool of candidates, for each treated pixel) 
    ecBuffer = 3, 

    ## Number of times to expand search radius if no candidates found
    tries = 4,

    ## Target variable name
    varname = 'satvi',
    
    ## Inner buffer radius (shrink treatment area to avoid edge effects)
    innerRad = -15,
    buffer = 90,

    ## Predictions
    n_x = 20, # n columns of tiles
    n_y = 20, # n rows of tiles

    
    #vars to filter by
    filterVars = list(
      soilec = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/ec_0to60cm_100xInt_ucrb.tif",
      soilps = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/UCRB_mPSC_RFE_10plus.tif"#,
#      vegPotential = "/home/steve/data/GIS_ARCHIVE/Landfire/intermediate.grd"
    ),

    # masking variables: 1 = ok, 0 = mask
    maskVars = list(
      refrast =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/refrast.tif",
      roadrast =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/TIGER_2018_ucrb_mask.tif",
      otherpads =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WYCOriv_Nopads.tif",
      oilgas =      "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/oil_gas_buf_ucrb_mask.tif",
      oilgas4corners= "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/fourCorners_oilgas_mask.tif",
      othersites =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/treatment_mask.tif",
      disturbance = '/lustre/projects/ecosystems/sbsc/ucrb/GIS/LANDFIRE/disturbance/allDist2.tif',
      exclosures = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/mfo_exclosures_mask.tif",
      fires= "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/mtbs_mask.tif",
      utblmfires= '/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/utfire_mask.tif',
     # irrigated =  "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WYCOrivnotIrrigated.tif",
     # coal = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/NoCoalCOriv.tif",
     # wind = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/WY_COriv_nowind.tif",
      nlcd = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/NLCDcl.tif",
      nlcdBuf = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/nlcdMask.tif",
      hasPJ = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/MASKS/espHasPJ.tif" 
   ),

    # variables for distance matrix
    topoVars = list(
        ELEVm = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/ELEVm.tif",
        PCURV = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/PCURV.tif",
        TCURV = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/TCURV.tif",
        RELHT1 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT1.tif",
        RELHT32 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT32.tif",
        RELHT128 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELHT128.tif",
        RELMNHT1 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT1.tif",
        RELMNHT32 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT32.tif",
        RELMNHT128 ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/RELMNHT128.tif",
        MRRTF ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/MRRTF.tif",
        MRVBF ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/MRVBF.tif",
        SLOPE ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/SLOPE.tif",
        SOUTHNESS ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/SOUTHNESS.tif",
        EASTNESS ="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/EASTNESS.tif",
        TWI_TOPMODEL="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/TWI_TOPMODEL.tif",
        CAlog_10="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/CAlog_10.tif",
        LFELEMS="/lustre/projects/ecosystems/sbsc/ucrb/GIS/UCRB_Covariates/LFELEMS.tif"
    ),


    ## response variables
    respVars = list(
    # bare = "/lustre/projects/ecosystems/sbsc/ucrb/outputs/bareV1/out/tiles/BareSoilCover_all.vrt"
    # iqr = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/EE/satvi_iqr_MarNov/iqr.vrt",
    # med = "/lustre/projects/ecosystems/sbsc/ucrb/GIS/EE/satvi-median-MarNov/median.vrt",
     ndvi = "shay_ndvi.grd",
     satvi = "shay_satvi.grd",
     savi = 'shay_savi.grd'
    )



)
