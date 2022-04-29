#' Find all matching nodes
#'
#' @param node xml2 node object to search
#' @param leaf_name name of leaf
#' @param leaf_attrs named list of leaf attributes
#' @param node_name name of node
#' @param node_attrs named list of node attributes
#'
#' @return node list with all matching nodes
#' @export
#'
#' @examples
find_matches <- function(node, leaf_name, leaf_attrs=NULL, node_name=NULL, node_attrs=NULL){

  leaf_path <- gcamxmlgen::get_path(leaf_name, names(leaf_attrs), unlist(leaf_attrs[[1]]))

  node_path <- gcamxmlgen::get_path(node_name, names(node_attrs), unlist(node_attrs[[1]]))

  path <- paste0(node_path, leaf_path)

  all_nodes <- xml2::xml_find_all(node, path)
  return(all_nodes)
}

#' Make xpath formatted string
#'
#' @param node_name
#' @param attr_name
#' @param attr_vals
#'
#' @return
#' @export
#'
#' @examples
get_path <- function(node_name, attr_name, attr_vals) {

  # TODO add handling for passing attr_name as a list
  if (length(attr_name)>1) stop("attr_name must be a single value.")

  if (all(sapply(list(node_name, attr_name, attr_vals), is.null))) stop("All three inputs to get_path() cannot be null.")

  if (is.null(node_name)) node_name <- "*"

  attr_path <- get_attr_path(attr_name, attr_vals)
  path <- paste0("//", node_name, attr_path)

  return(path)
}

get_attr_path <- function(attr_name, attr_vals){

  if (is.null(attr_name) & is.null(attr_vals)) attr_path <- ""

  else if (is.null(attr_vals)) attr_path <- paste0("[@",attr_name, "]")

  else {  # both are given or only attr_name is null

    if (is.null(attr_name)) attr_name <- "*"

    attr_path <- NULL
    for (i in 1:length(attr_vals)){
      if (i > 1) attr_path <- paste0(attr_path, " or ")
      attr_path <- paste0(attr_path, "@", attr_name, "='", attr_vals[[i]], "'")
    }
    attr_path <- paste0("[", attr_path, "]")
  }

  return(attr_path)

}

#' Remove odd characters in string
#'
#' @param string_in string to strip down by removing slashes, spaces, parentheses, and others
#' @param sep character to replace odd characters with
#'
#' @return simplified string without unwanted characters
#' @export
#'
#' @examples
#' s <- "AIM/CGE 2.0_ADVANCE_2020_1.5C-2100"
#' strip_string(s)
strip_string <- function(string_in, sep='_'){
  string_out <- gsub("[\\s(),\\+]+", sep, string_in, perl=T)
  string_out <- gsub("[/.]+", "-", string_out, perl=T)
  return(string_out)
}

# TODO generalize to work with any attributes - right now only works with "name"
make_node <- function(tag,name=NULL){

  if (!is.null(name)){
    node_str <- paste0("<",tag," name='", name,"'></",tag,">")
  } else node_str <- paste0("<",tag,"></",tag,">")

  node <- read_xml(node_str)
  return(xml_root(node))
}

make_leaf <- function(value){
  node_str <- paste0("<Value>",value,"</Value>")
  node <- read_xml(node_str)
  node_root <- xml_root(node)
  return(node_root)
}

#' Extract root node only from xml file
#'
#' @param filename name of the file
#'
#' @return xml2 object with only the root node information from the file
#' @export
#'
#' @examples
get_file_stem <- function(filename){
  data <- xml2::read_xml(filename)
  data_root <- xml2::xml_root(data)
  kids <- xml2::xml_children(data_root)
  for (kid in kids){
    xml2::xml_remove(kid)
  }
  return(data_root)
}
