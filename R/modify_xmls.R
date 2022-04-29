#' Set xml2 node values
#'
#' @param xml_tree may be either xml file to be modified or xml2 tree object
#' @param data either a numerical value, or time series data as a tibble or dataframe. If data is tabular, it is assumed to be a time series and must have a column named 'year'
#' @param leaf_name xml name of the specific leaf node you want to change the value of
#' @param leaf_attrs named list with the name specifying the name of the attribute of the leaf you want to change. Value is the value of the attribute.
#' @param node_name
#' @param node_attrs list where the name is the xml node name and the value is the specific xml attribute 'name' within that node where the leaves you want to modify are found (later known as 'tags'). You can specify
#' @param output_filename file where modified xml should be saved
#' @param col_name if data is tabular, this is the column the values should be pulled from
#'
#' @return modified xml2 tree object
#' @export
#' @details Set multiple xml2 nodes to single value or time series data. This function returns the modified xml tree, and will also save an xml file of this tree if output_filename is specified.
#' By passing a tree instead of a file name, the user may use this function to iterate over multiple inputs to modify the same xml document before saving.
#' @examples
#'
set_xml <- function(xml_tree, data, leaf_name, leaf_attrs=NULL, node_name=NULL, node_attrs=NULL, output_filename=NULL, col_name=''){

  # check type of xml_data input: if string, treat as filename and read in. Otherwise proceed
  # allowing either input allows set_xml to be run iteratively with multiple pieces of data
  if (any(class(xml_tree)=="character")) xml_tree <- xml2::read_xml(xml_tree)

  # generate list of nodes corresponding to specified names and attributes
  match_nodes <- gcamxmlgen::find_matches(xml_tree, leaf_name, leaf_attrs, node_name, node_attrs)

  if (length(match_nodes) == 0) {
    stop("No matching nodes found. Double-check your inputs.")
  }

  # if data is a single value
  if (length(data)==1) lapply(match_nodes, xml2::`xml_text<-`, value=as.character(data[[1]]))
  else {
    if (col_name=='') stop("Column name must be specified for tabular data")
    lapply(match_nodes, gcamxmlgen::set_value, data=data, col_name=col_name)
  }

  if (!is.null(output_filename)) xml2::write_xml(xml_tree, output_filename)

  return(xml_tree)
}


#' Set single node value
#'
#' @param node xml2 node object
#' @param data Annual timeseries data in tabular format. year information should be in a column labeled "year"
#' @param col_name The name of the column with the data values
#'
#' @return this function modifies an existing node object and so does not have a return value
#' @details Set value of a single xml2 node object for a given year from tabular data
#' @export
#'
#' @examples
set_value <- function(node, data, col_name){

  year_val <- as.numeric(xml2::xml_attr(node, "year"))
  data_val <- dplyr::filter(data, year == {{year_val}}) %>% dplyr::select({{col_name}})
  xml2::xml_text(node) <- toString(data_val[[1]])

}


#' Add new child nodes to xml
#'
#' @param filename
#' @param child_str
#' @param node_names
#' @param node_attrs
#' @param outfile
#' @param order
#' @param create_parent
#'
#' @return
#' @export
#'
#' @examples
add_children_to_xml <- function(filename, child_str, node_names, node_attrs, outfile, order=0, create_parent=TRUE){

  input_tree <- xml2::read_xml(x = filename)
  input_root <- xml2::xml_root(input_tree)

  child_tree <- xml2::read_xml(child_str)
  child_node <- xml2::xml_root(child_tree)

  # get path for each node_name. paste together with | separator
  full_path <- NULL
  for (i in 1:length(node_names)){
    if (i > 1) full_path <- paste0(full_path, " | ")
    full_path <- paste0(full_path, get_path(node_names[[i]], names(node_attrs)[[i]], unlist(node_attrs[[i]])))
  }

  nodes<-xml2::xml_find_all(input_root, full_path)

  for (i in nodes){
    xml2::xml_add_child(i,child_node,.where=order)
  }

  temp <- xml2::write_xml(input_root,outfile)
}



#' Make a batch file for sets of input files
#'
#' @param input_dir folder where your input files are located
#' @param ensemble_set named list with names corresponding to folders containing sets of input files for GCAM
#' @param outfile name of batch file that will be created
#' @param outer_tag tag for outer set of nodes in batch file. Default is ComponentSet
#' @param inner_tag tag for inner set of nodes in batch file components. Default is FileSet
#' @param sample_file specific batch file to use as a base.
#' @param other_text named list with names matching names of ensemble set argument. This is used for any additional text that should be added after each file in that component set.
#'
#' @return
#' @export
#'
#' @examples
make_batch <- function(input_dir, ensemble_set, outfile, outer_tag="ComponentSet", inner_tag="FileSet", sample_file=system.file("extdata","batch_example.xml",package="gcamxmlgen"), other_text=NULL){

  batch_root <- get_file_stem(sample_file)

  for (i in 1:length(ensemble_set)){

    outer_node <- make_node(outer_tag, name=names(ensemble_set)[[i]])
    sibs <- other_text[[names(ensemble_set)[[i]]]]
    files <- list.files(path=ensemble_set[[i]])

    for (file in files){
      fname <- strsplit(file,".",fixed=TRUE)[[1]][[1]]
      file_node <- make_node(inner_tag,name=fname)
      value_node <- make_leaf(paste0(input_dir,file))
      xml2::xml_add_child(file_node,value_node)

      for (j in 1:length(sibs)){
        print(sibs[[j]])
        sib <- xml2::read_xml(sibs[[j]])
        xml2::xml_add_child(file_node, sib)
      }
      xml2::xml_add_child(outer_node,file_node)
    }
    xml2::xml_add_child(batch_root, outer_node)
  }

  xml2::write_xml(batch_root,file=outfile)

}









