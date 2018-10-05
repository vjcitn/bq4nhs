#' generate a BigQueryConnection to a selected project/dataset
#' @param project character(1) project name in BigQuery
#' @param ds character(1) dataset name
#' @examples
#' nhsBQ()
#' @export
nhsBQ = function (project = "test-project1-205019", ds = "smk_lungca") 
{
    con <- DBI::dbConnect(bigrquery::bigquery(), project = project, 
        dataset = ds)
    con
}

