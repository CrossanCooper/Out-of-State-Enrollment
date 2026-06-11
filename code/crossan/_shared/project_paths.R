#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026

## file use: Define portable code, data, output, figure, and table paths for
## Crossan-authored scripts in the active Out-of-State Enrollment codebase.
#
## inputs:
## 1. ADMISSIONS_PROJECT_DATA_ROOT -- optional override for the data/project root
## 2. OOS_ENROLLMENT_CODE_ROOT -- optional override for this code directory
## 3. ADMISSIONS_PROJECT_TOWN_MATCHING_OUTPUT_DIR -- optional town-panel output root
#
## outputs:
## 1. pathHome -- data/project root used by legacy scripts
## 2. pathCode -- active code directory
## 3. pathCrossanCode -- Crossan-authored code directory
## 4. pathData, pathFigures, pathTables -- common project subdirectories
## 5. pathRecruitingOutput -- output directory for town-level recruiting analyses
#=====================================================================

#=====================================================================
# 1 - Path helpers
#=====================================================================

### i. normalize one configured path

## (a) Read an environment variable when present and otherwise use the local default.
normalize_project_path <- function(env_var, default_path, must_work = FALSE) {
  candidate <- Sys.getenv(env_var, unset = default_path)
  normalizePath(path.expand(candidate), mustWork = must_work)
}

## (b) Preserve the trailing slash expected by older paste0(pathHome, ...) code.
add_trailing_slash <- function(path) {
  if (grepl("/$", path)) {
    return(path)
  }
  paste0(path, "/")
}

#=====================================================================
# 2 - Shared paths
#=====================================================================

### i. roots

## (a) Data remain in the original admissions_project folder.
pathHomeRoot <- normalize_project_path(
  "ADMISSIONS_PROJECT_DATA_ROOT",
  "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project",
  must_work = TRUE
)
pathHome <- add_trailing_slash(pathHomeRoot)

## (b) Code lives in the active Out-of-State-Enrollment repository.
pathCode <- normalize_project_path(
  "OOS_ENROLLMENT_CODE_ROOT",
  "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project_personal/Out-of-State-Enrollment/code",
  must_work = TRUE
)

### ii. common project subdirectories

pathCrossanCode <- file.path(pathCode, "crossan")
pathCrossanShared <- file.path(pathCrossanCode, "_shared")
pathCrossanCommencement <- file.path(pathCrossanCode, "commencement")
pathCrossanRecruiting <- file.path(pathCrossanCode, "recruiting")
pathCrossanRevelio <- file.path(pathCrossanCode, "revelio")
pathCrossanPublicData <- file.path(pathCrossanCode, "public_data")
pathTownMatchingCode <- file.path(pathCrossanRecruiting, "town_matching")

pathData <- file.path(pathHomeRoot, "data")
pathFigures <- file.path(pathHomeRoot, "figures")
pathTables <- file.path(pathHomeRoot, "tables")
pathRevelio <- file.path(pathHomeRoot, "revelio_data")
pathGenerated <- file.path(pathHomeRoot, "generated_data")

### iii. recruiting/town-panel outputs

pathRecruitingOutput <- normalize_project_path(
  "ADMISSIONS_PROJECT_TOWN_MATCHING_OUTPUT_DIR",
  file.path(pathHomeRoot, "town_matching", "output"),
  must_work = FALSE
)
