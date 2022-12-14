% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/databaseClassR6.R
\name{databaseClass}
\alias{databaseClass}
\title{Generic Postgres Database layer R6 class}
\description{
Generic methods to work with postgres, avoiding any SQL in the next
architecture layer.
}
\examples{

## ------------------------------------------------
## Method `databaseClass$replace_value_where`
## ------------------------------------------------

\dontrun{
# set verwijderd = 1 where naam = 'gekozennaam'
replace_value_where("table", 'verwijderd', 'true', 'naam', 'gekozennaam')
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{con}}{Connection object}

\item{\code{schema}}{DB Schema}

\item{\code{dbname}}{DB name}

\item{\code{dbuser}}{DB user}

\item{\code{pool}}{Logical; whether we are connected with the \code{pool} package or not.}

\item{\code{dbtype}}{DB type; either 'postgres' or 'sqlite'}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NA-new}{\code{databaseClass$new()}}
\item \href{#method-NA-connect_to_database}{\code{databaseClass$connect_to_database()}}
\item \href{#method-NA-close}{\code{databaseClass$close()}}
\item \href{#method-NA-list_tables}{\code{databaseClass$list_tables()}}
\item \href{#method-NA-have_column}{\code{databaseClass$have_column()}}
\item \href{#method-NA-make_column}{\code{databaseClass$make_column()}}
\item \href{#method-NA-read_table}{\code{databaseClass$read_table()}}
\item \href{#method-NA-append_data}{\code{databaseClass$append_data()}}
\item \href{#method-NA-query}{\code{databaseClass$query()}}
\item \href{#method-NA-get_query}{\code{databaseClass$get_query()}}
\item \href{#method-NA-execute_query}{\code{databaseClass$execute_query()}}
\item \href{#method-NA-has_value}{\code{databaseClass$has_value()}}
\item \href{#method-NA-replace_value_where}{\code{databaseClass$replace_value_where()}}
\item \href{#method-NA-update_where}{\code{databaseClass$update_where()}}
\item \href{#method-NA-replace_value_where2}{\code{databaseClass$replace_value_where2()}}
\item \href{#method-NA-table_info}{\code{databaseClass$table_info()}}
\item \href{#method-NA-table_columns}{\code{databaseClass$table_columns()}}
\item \href{#method-NA-delete_rows_where}{\code{databaseClass$delete_rows_where()}}
\item \href{#method-NA-to_sql_string}{\code{databaseClass$to_sql_string()}}
\item \href{#method-unknown-clone}{\code{databaseClass$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-new"></a>}}
\if{latex}{\out{\hypertarget{method-NA-new}{}}}
\subsection{Method \code{new()}}{
Make a new R6 object of class \code{database}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$new(
  config_file = "conf/config.yml",
  what,
  schema = NULL,
  pool = TRUE,
  connect_on_init = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{config_file}}{Path to the standard DB connection config file}

\item{\code{what}}{Connect to which database entry in the config file}

\item{\code{schema}}{Name of schema where the data resides}

\item{\code{pool}}{Logical; use \code{\link[=dbPool]{dbPool()}} or not.}

\item{\code{connect_on_init}}{Whether to immediately make a DB connection when
making this object}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-connect_to_database"></a>}}
\if{latex}{\out{\hypertarget{method-NA-connect_to_database}{}}}
\subsection{Method \code{connect_to_database()}}{
Make database connection
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$connect_to_database(
  config_file = NULL,
  schema = NULL,
  what = NULL,
  pool = TRUE,
  sqlite = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{config_file}}{Path to the standard DB connection config file}

\item{\code{schema}}{Name of schema where the data resides}

\item{\code{what}}{Connect to which database entry in the config file}

\item{\code{pool}}{Logical; use \code{\link[=dbPool]{dbPool()}} or not.}

\item{\code{sqlite}}{Name of \code{sqlite} DB file; if used.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-close"></a>}}
\if{latex}{\out{\hypertarget{method-NA-close}{}}}
\subsection{Method \code{close()}}{
Close DB connection.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$close()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-list_tables"></a>}}
\if{latex}{\out{\hypertarget{method-NA-list_tables}{}}}
\subsection{Method \code{list_tables()}}{
List tables
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$list_tables()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-have_column"></a>}}
\if{latex}{\out{\hypertarget{method-NA-have_column}{}}}
\subsection{Method \code{have_column()}}{
Does \code{table} have \code{column}?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$have_column(table, column)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{column}}{Column name}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Logical
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-make_column"></a>}}
\if{latex}{\out{\hypertarget{method-NA-make_column}{}}}
\subsection{Method \code{make_column()}}{
Add a column to a table
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$make_column(table, column, type = "varchar")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{column}}{Column name}

\item{\code{type}}{Column type - watch out, not checked against valid types!}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-read_table"></a>}}
\if{latex}{\out{\hypertarget{method-NA-read_table}{}}}
\subsection{Method \code{read_table()}}{
Read a table from the DB, maybe "lazy"
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$read_table(table, lazy = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Name of table (in the global schema)}

\item{\code{lazy}}{Logical, if FALSE (default), simply downloads the whole table.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
If \code{lazy=TRUE}, returns a lazy connection that can be filtered, selected etc.
using \code{dbplyr}'s \code{filter}, \code{select}, etc. Use if you want the work to be done by the DB,
before downloading the entire table.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-append_data"></a>}}
\if{latex}{\out{\hypertarget{method-NA-append_data}{}}}
\subsection{Method \code{append_data()}}{
Append rows to an existing table
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$append_data(table, data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{data}}{Dataframe with columns that exist in \code{table}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Wrapped in a \code{try}, so will not stop the application if this operation fails.
It will fail when \code{data} has columns that do not exist in \code{table}, or are of
incompatible type.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-query"></a>}}
\if{latex}{\out{\hypertarget{method-NA-query}{}}}
\subsection{Method \code{query()}}{
Run an SQL statement with \code{\link[=dbGetQuery]{dbGetQuery()}}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$query(txt, glue = TRUE, quiet = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{txt}}{SQL query}

\item{\code{glue}}{If TRUE, can \code{glue} the SQL query}

\item{\code{quiet}}{If TRUE, no printing (TODO global!)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Query wrapped in \code{try}, for safety
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_query"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_query}{}}}
\subsection{Method \code{get_query()}}{
Run an SQL statement with \code{\link[=dbGetQuery]{dbGetQuery()}}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$get_query(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Further arguments to \code{self$query}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Query wrapped in \code{try}, for safety
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-execute_query"></a>}}
\if{latex}{\out{\hypertarget{method-NA-execute_query}{}}}
\subsection{Method \code{execute_query()}}{
Run an SQL statement with \code{\link[=dbExecute]{dbExecute()}}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$execute_query(txt, glue = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{txt}}{SQL query ("alter table" type commands)}

\item{\code{glue}}{If TRUE, \code{glue}s the query}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-has_value"></a>}}
\if{latex}{\out{\hypertarget{method-NA-has_value}{}}}
\subsection{Method \code{has_value()}}{
Does \code{column} in \code{table} have this \code{value}?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$has_value(table, column, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{column}}{Column name}

\item{\code{value}}{Value}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-replace_value_where"></a>}}
\if{latex}{\out{\hypertarget{method-NA-replace_value_where}{}}}
\subsection{Method \code{replace_value_where()}}{
Update a value in a table given a single logical condition
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$replace_value_where(
  table,
  col_replace,
  val_replace,
  col_compare,
  val_compare,
  query_only = FALSE,
  quiet = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{col_replace}}{Column name where to update value(s)}

\item{\code{val_replace}}{Value to enter in updated cells}

\item{\code{col_compare}}{Column used to select rows for updating}

\item{\code{val_compare}}{Value in \code{col_compare} used to select rows for updating}

\item{\code{query_only}}{Logical; if TRUE, returns the query. If FALSE, also runs the query.}

\item{\code{quiet}}{If TRUE, no logging}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
# set verwijderd = 1 where naam = 'gekozennaam'
replace_value_where("table", 'verwijderd', 'true', 'naam', 'gekozennaam')
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-update_where"></a>}}
\if{latex}{\out{\hypertarget{method-NA-update_where}{}}}
\subsection{Method \code{update_where()}}{
Synonym for \code{\link[=replace_value_where]{replace_value_where()}}. See its help page.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$update_where(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Further arguments passed to \code{replace_value_where}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-replace_value_where2"></a>}}
\if{latex}{\out{\hypertarget{method-NA-replace_value_where2}{}}}
\subsection{Method \code{replace_value_where2()}}{
Update a value in a table given a two logical AND conditions
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$replace_value_where2(
  table,
  col_replace,
  val_replace,
  col_compare1,
  val_compare1,
  col_compare2,
  val_compare2
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{col_replace}}{Column name where to update value(s)}

\item{\code{val_replace}}{Value to enter in updated cells}

\item{\code{col_compare1}}{Column used to select rows for updating}

\item{\code{val_compare1}}{Value in \code{col_compare} used to select rows for updating}

\item{\code{col_compare2}}{Second column used to select rows for updating}

\item{\code{val_compare2}}{Second value in \code{col_compare} used to select rows for updating}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-table_info"></a>}}
\if{latex}{\out{\hypertarget{method-NA-table_info}{}}}
\subsection{Method \code{table_info()}}{
Get detailed table info (postgres)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$table_info(table)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Queries \code{information_schema.columns}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-table_columns"></a>}}
\if{latex}{\out{\hypertarget{method-NA-table_columns}{}}}
\subsection{Method \code{table_columns()}}{
Get columns of a table
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$table_columns(table, empty_table = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{empty_table}}{If TRUE, returns a table with 0 rows, otherwise
a vector of table column names,}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-delete_rows_where"></a>}}
\if{latex}{\out{\hypertarget{method-NA-delete_rows_where}{}}}
\subsection{Method \code{delete_rows_where()}}{
Delete rows from a table given a single logical condition
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$delete_rows_where(table, col_compare, val_compare)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{Table name}

\item{\code{col_compare}}{Column where to make logical comparison}

\item{\code{val_compare}}{Values in \code{col_compare} where rows will be deleted}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Do not use inside shiny apps or otherwise, only as a tool to clean up tables.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-to_sql_string"></a>}}
\if{latex}{\out{\hypertarget{method-NA-to_sql_string}{}}}
\subsection{Method \code{to_sql_string()}}{
Make a SQL representation of a vector
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$to_sql_string(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{Vector}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-unknown-clone"></a>}}
\if{latex}{\out{\hypertarget{method-unknown-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{databaseClass$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
