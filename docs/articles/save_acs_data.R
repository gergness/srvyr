# Downloads data from the ACS website to be used in the srvyr-database
library(readr)
library(dplyr)


# A priori variable types (may not be entirely correct, I have only checked for variables
# used in the Vignette)
{
  var_types <- list()
  var_types$h <- c("character", "integer", "integer", "character", "integer",
                   "character", "integer", "integer", "integer", "character",
                   "integer", "integer", "integer", "integer", "character", "character",
                   "integer", "character", "character", "integer", "character",
                   "character", "integer", "character", "character", "integer",
                   "character", "integer", "integer", "integer", "character", "integer",
                   "character", "integer", "integer", "character", "integer", "integer",
                   "integer", "integer", "integer", "character", "integer", "character",
                   "integer", "integer", "integer", "integer", "integer", "character",
                   "integer", "integer", "character", "character", "integer", "integer",
                   "character", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "character", "character", "integer",
                   "integer", "character", "character", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "character", "integer",
                   "integer", "integer", "character", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", rep("integer", 80))

  names(var_types$h) <- c("rt", "serialno",
                          "division", "puma", "region", "st", "adjhsg", "adjinc", "wgtp",
                          "np", "type", "acr", "ags", "bath", "bdsp", "bld", "bus", "conp",
                          "elep", "fs", "fulp", "gasp", "hfl", "insp", "mhp", "mrgi", "mrgp",
                          "mrgt", "mrgx", "refr", "rmsp", "rntm", "rntp", "rwat", "sink",
                          "smp", "stov", "tel", "ten", "toil", "vacs", "valp", "veh", "watp",
                          "ybl", "fes", "ffincp", "fgrntp", "fhincp", "fincp", "fparc",
                          "fsmocp", "grntp", "grpip", "hhl", "hht", "hincp", "hugcl", "hupac",
                          "hupaoc", "huparc", "kit", "lngi", "multg", "mv", "noc", "npf",
                          "npp", "nr", "nrc", "ocpip", "partner", "plm", "psf", "r18",
                          "r60", "r65", "resmode", "smocp", "smx", "srnt", "sval", "taxp",
                          "wif", "wkexrel", "workstat", "facrp", "fagsp", "fbathp", "fbdsp",
                          "fbldp", "fbusp", "fconp", "felep", "ffsp", "ffulp", "fgasp",
                          "fhflp", "finsp", "fkitp", "fmhp", "fmrgip", "fmrgp", "fmrgtp",
                          "fmrgxp", "fmvp", "fplmp", "frefrp", "frmsp", "frntmp", "frntp",
                          "frwatp", "fsinkp", "fsmp", "fsmxhp", "fsmxsp", "fstovp", "ftaxp",
                          "ftelp", "ftenp", "ftoilp", "fvacsp", "fvalp", "fvehp", "fwatp",
                          "fyblp", paste0("wgtp", 1:80))

  var_types$p <- c("character", "integer", "character", "character",
                   "character", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "character", "character",
                   "character", "character", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "character", "character", "character",
                   "character", "integer", "character", "character", "character",
                   "integer", "character", "character", "character", "character",
                   "integer", "integer", "integer", "integer", "integer", "character",
                   "character", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "character", "character", "character",
                   "character", "integer", "character", "character", "integer",
                   "character", "integer", "integer", "integer", "character", "integer",
                   "character", "character", "character", "character", "character",
                   "character", "integer", "integer", "integer", "integer", "character",
                   "character", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "character", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", "integer", "integer", "integer", "integer", "integer",
                   "integer", rep("integer", 80))

  names(var_types$p) <- c("rt", "serialno", "sporder", "puma",
                          "st", "adjinc", "pwgtp", "agep", "cit", "citwp", "cow", "ddrs",
                          "dear", "deye", "dout", "dphy", "drat", "dratx", "drem", "eng",
                          "fer", "gcl", "gcm", "gcr", "hins1", "hins2", "hins3", "hins4",
                          "hins5", "hins6", "hins7", "intp", "jwmnp", "jwrip", "jwtr",
                          "lanx", "mar", "marhd", "marhm", "marht", "marhw", "marhyp",
                          "mig", "mil", "mlpa", "mlpb", "mlpc", "mlpd", "mlpe", "mlpf",
                          "mlpg", "mlph", "mlpi", "mlpj", "mlpk", "nwab", "nwav", "nwla",
                          "nwlk", "nwre", "oip", "pap", "relp", "retp", "sch", "schg",
                          "schl", "semp", "sex", "ssip", "ssp", "wagp", "wkhp", "wkl",
                          "wkw", "wrk", "yoep", "anc", "anc1p", "anc2p", "decade", "dis",
                          "drivesp", "esp", "esr", "fod1p", "fod2p", "hicov", "hisp", "indp",
                          "jwap", "jwdp", "lanp", "migpuma", "migsp", "msp", "naicsp",
                          "nativity", "nop", "oc", "occp", "paoc", "pernp", "pincp", "pobp",
                          "povpip", "powpuma", "powsp", "privcov", "pubcov", "qtrbir",
                          "rac1p", "rac2p", "rac3p", "racaian", "racasn", "racblk", "racnhpi",
                          "racnum", "racsor", "racwht", "rc", "sciengp", "sciengrlp", "sfn",
                          "sfr", "socp", "vps", "waob", "fagep", "fancp", "fcitp", "fcitwp",
                          "fcowp", "fddrsp", "fdearp", "fdeyep", "fdisp", "fdoutp", "fdphyp",
                          "fdratp", "fdratxp", "fdremp", "fengp", "fesrp", "fferp", "ffodp",
                          "fgclp", "fgcmp", "fgcrp", "fhins1p", "fhins2p", "fhins3c", "fhins3p",
                          "fhins4c", "fhins4p", "fhins5c", "fhins5p", "fhins6p", "fhins7p",
                          "fhisp", "findp", "fintp", "fjwdp", "fjwmnp", "fjwrip", "fjwtrp",
                          "flanp", "flanxp", "fmarhdp", "fmarhmp", "fmarhtp", "fmarhwp",
                          "fmarhyp", "fmarp", "fmigp", "fmigsp", "fmilpp", "fmilsp", "foccp",
                          "foip", "fpap", "fpernp", "fpincp", "fpobp", "fpowsp", "fprivcovp",
                          "fpubcovp", "fracp", "frelp", "fretp", "fschgp", "fschlp", "fschp",
                          "fsemp", "fsexp", "fssip", "fssp", "fwagp", "fwkhp", "fwklp",
                          "fwkwp", "fwrkp", "fyoep", paste0("pwgtp", 1:80))

  var_types_readr <- lapply(var_types, function(x) {
    out <- list(col_integer(), col_character())[match(x, c("integer", "character"))]
    out
  })

  var_types_sql <- lapply(var_types, function(x) {
    out <- lapply(x, function(y) {
      if (y == "character") "1"
      else if (y == "integer") 1L
    })
    as.data.frame(out, stringsAsFactors = FALSE)
  })
}

# Helper functions
# Downloads, unzips and loads acs file, saves an intermediate file at out_file
acs_file_loader <- function(url, data_file, vtypes, out_file = tempfile()) {
  download.file(url, destfile = out_file, quiet = TRUE)
  data_conn <- unz(out_file, data_file) # makes a file connection to an internal file
  read_csv(data_conn, col_types = vtypes, progress = FALSE)
}

# For each state in states argument, downloads the data from the web and uploads it into
# the dataabse db
prepare_acs2011_single <- function(states = c("ak", "hi")) {
  file_types <- c("h", "p")
  file_types_full <- list(h = "household", p = "person")

  out <- list(h = list(), p = list())
  for (iii in seq_along(states)) {
    sss <- states[iii]
    if (iii %% 10 == 1) {
      cat(sprintf("%s", sss))
    } else {
      cat(".")
    }

    for (ttt in file_types) {
      this_data <- acs_file_loader(
        url = sprintf("http://www2.census.gov/acs2011_1yr/pums/csv_%s%s.zip", ttt, sss),
        data_file = sprintf("ss11%s%s.csv", ttt, sss),
        vtypes = var_types_readr[[ttt]]
      )
      names(this_data) <- tolower(names(this_data))
      out[[ttt]][[iii]] <- this_data
    }

  }
  cat("\n")
  out
}

# Merge household and personal datasets
merge_acs_2011_single <- function(acs_h, acs_p, limit_vars = TRUE) {
  acs_h <- bind_rows(acs_h)
  acs_p <- bind_rows(acs_p)

  h_fields <- setdiff(names(acs_h), "rt")
  p_fields <- c(setdiff(names(acs_p), names(acs_h)), "serialno")

  h_prepped <- acs_h %>%
    select(one_of(h_fields))
  p_prepped <- acs_p %>%
    select(one_of(p_fields))

  if (limit_vars) {
    select_statement <- "c(serialno, sporder, dplyr::matches('^pwgtp'), agep, hicov, sex, st)"
  } else {
    select_statement <- "everything()"
  }

  inner_join(h_prepped, p_prepped, by = "serialno") %>%
    select_(select_statement) %>%
    mutate(rt = "M")
}

# acs_status <- prepare_acs2011_single(states = c(tolower(state.abb), "pr"))
acs_data <- prepare_acs2011_single()

# Merge the datasets together
acs_m <- merge_acs_2011_single(acs_data[["h"]], acs_data[["p"]])

save(acs_m, file ="vignettes/acs_m.RData")
