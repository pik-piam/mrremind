downloadNIR <- function(subtype=NULL) {

  links <- c("http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/aus-2015-crf-27may.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/aut-2015-crf-5nov15.zip",
              "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/blr-2015-crf-7jul16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/bel-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/bgr-2015-crf-6nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/hrv-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/cyp-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/cze-2015-crf-9nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/dnk-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/fin-2016-crf-15apr16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/fra-2015-crf-4nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/deu-2015-crfr-6nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/grc-2016-crf-23may16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/hun-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/isl-2015-crf-6nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/irl-2015-crf-4nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/ita-2015-crf-3nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/jpn-2015-crf-28dec15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/kaz-2016-crf-8jul16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/lva-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/lie-2015-crf-regen1-14mar16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/ltu-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/lux-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/mlt-2015-crf-6nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/mco-2015-crf-26jul16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/nld-2016-crf-15jun16.zip",
             #"http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/nzl-2015-crf-31jul15_crf_reporter_v5.10.0.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/nor-2015-crf-06jan16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/pol-2015-crf-6nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/prt-2016-crf-27may16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/rou-2016-crf-05aug16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/rus-2015-crf_regen-17feb16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/svk-2016-crf-9sep16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/svn-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/esp-2015-crf-5nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/swe-2016-crf-15jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/che-2015-crf_regen-8apr16.zip",
             "http://unfccc.int/files/national_reports/annex_i_natcom/status_of_submission_of_natcom_under_the_kp/application/zip/tur-2015-crf_regen-12nov15.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/ukr-2015-crf-14jun16.zip",
             "http://unfccc.int/files/national_reports/annex_i_ghg_inventories/national_inventories_submissions/application/zip/gbr-2015-crf-30oct15.zip")


  ### download files
  fnames <- sapply(links, function(x){utils::tail(strsplit(x, split = "/")[[1]], 1)})

  lapply(1:length(links), FUN = function(x){ utils::download.file(links[x], destfile=fnames[x], mode="wb")})

  ###  unzip files
  zipfiles <- list.files(pattern=".zip$")
  lapply(zipfiles, utils::unzip)
  lapply(zipfiles, unlink)

  ### delete unwanted files
  outfiles <- list.files(pattern="^[A-Z]{3}_[0-9]{4}_[0-9]{4}_.*\\.xlsx")
  allfiles <- list.files()
  unlink(allfiles[which(!(allfiles %in% outfiles))])

}
