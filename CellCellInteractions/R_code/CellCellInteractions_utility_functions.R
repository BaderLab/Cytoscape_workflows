library(RJSONIO)
library(httr)

# Function takes a table of nodes.  
# All attributes in the node table will get translated into node attributes
#
# specify which column should be used for id, name and shared_name (they will all get the same value)
# Alias, uid,curated_protein_type, tax and celltypes
convert_nodes_tojson <- function(node_set, id_column="Alias"){
  json_nodes <- c()
  for(i in 1:dim(node_set)[1]){
    
    #the all column info - translate all the columns into node attributes
    rest <- c()
    for(j in 1:dim(node_set)[2]){
      rest <-  c(rest,assign(colnames(node_set)[j] , node_set[i,j]))
      
    }
    
    rest <- c(node_set[i,id_column],node_set[i,id_column],node_set[i,id_column],rest)
    rest = list(unlist(rest))
    rest <- lapply(rest,FUN=function(x) {names(x) <-   
                                           c("id","name","shared_name",colnames(node_set))
                                         return(x)
    })
    
    #get current node
    current_node <- list(data =  unlist(rest))

    json_nodes <- c(json_nodes,list(current_node) )
  }
  return(json_nodes)
}

# Function takes a table of interactions.  Interaction table needs to contain columns:
# AliasA, AliasB, and pmids 
convert_edges_tojson <- function(interactions, interA = 'AliasA', interB = 'AliasB', 
                                 inter_type='method'){
  
  json_edges <- c()
  for(i in 1:dim(interactions)[1]){
    
    rest <- c()
    for(j in 1:dim(interactions)[2]){
      rest <-  c(rest,assign(colnames(interactions)[j] , interactions[i,j]))
      
    }
    
    computed_name = paste(interactions[i,interA],interactions[i,inter_type],
                 interactions[i,interB], sep="_")
    
    rest <- c(name = computed_name,shared_name=computed_name,
              source = interactions[i,interA],target=interactions[i,interB],
              rest)
    rest = list(unlist(rest))
    rest <- lapply(rest,FUN=function(x) {names(x) <-   
                                           c("name","shared_name","source","target",
                                             colnames(interactions))
                                         return(x)
    })
    
    current_edge  <- list(data =  unlist(rest))
    #current_edge <- list(
    #  data = list(name=paste(interactions[i,'AliasA'],interactions[i,'method'],
    #                         interactions[i,'AliasB'], sep="_"),
    #              shared_name = paste(interactions[i,'AliasA'],interactions[i,'method'],
    #                                  interactions[i,'AliasB'], sep="_"),
    #              source = interactions[i,'AliasA'],
    #              target = interactions[i,'AliasB'],
    #              pmids =   interactions[i,'pmids']
    #  ))
    json_edges <- c(json_edges,   list(current_edge))
  }
  return(json_edges)
}

cy_unselect_allnodes <- function(base.url,net_suid){
  
  column_name = "selected"
  set.node.column.url <- paste(base.url,"networks",net_suid,"tables/defaultnode/columns",column_name,sep="/")
  
  set_params = list(default="false")
  
  return(response <- PUT(url=set.node.column.url, 
                         query=set_params))
  
}


#
# Given a table that has a column called 'suids'
# method selects the nodes with supplied suids in
#
cy_select_nodes <- function(base.url, nodes, net_suid){
  #go through the set of nodes and create key-value pairs
  
  if(!is.null(nodes) && length(nodes) > 0){
  
    key_value_pairs <- c()
    for(k in 1:dim(nodes)[1]){
      current <- list( id = paste(nodes[k,'SUID']),selected = "true")
      key_value_pairs <- c(key_value_pairs,list(current))
    }
  
    selection <- list( key = "SUID", dataKey="id", data = key_value_pairs)
  
    selection <- toJSON(selection)
  
    get.node.column.url <- paste(base.url,"networks",net_suid,"tables/defaultnode",sep="/")
  
    return(response <- PUT(url=get.node.column.url,  
                         body=selection, encode="json"))
  }
  else{
    return("nothing selected")
  }
}

#
# Get node information from cytoscape
# get the whole node table
#
get_nodetable <- function(base.url, net_suid){
  get.node.column.url <- paste(base.url,"networks",net_suid,"tables/defaultnode/rows/",sep="/")
  
  response <- GET(url=get.node.column.url)
  
  json_file <- fromJSON(rawToChar(response$content))
  json_file <- lapply(json_file, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  nodetable <- do.call("rbind", json_file)
  return(nodetable)
}

cy_move_selected_nodes <- function(base.url, net_suid,x_offset=0,y_offset=0){
  #get the network view suid
  get.viewid.url <- paste(base.url,"networks",net_suid,"views",sep="/")  
  response <- GET(url=get.viewid.url)
  network.viewid <- unname(fromJSON(rawToChar(response$content)))
  
  #get the view node table
  get.viewinfo.url <- paste(base.url,"networks",net_suid,"views",network.viewid,sep="/")  
  response <- GET(url=get.viewinfo.url)
  
  json_file <- fromJSON(rawToChar(response$content))
  
  #go through each node and set the SUID to x and y position
  node_positions <- c()
  
  json_file <- lapply(json_file$elements$nodes, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  for(i in 1:length(json_file)){
    #get the suid, positionx and position y and selected
    current <- json_file[i]
    rowtable <- do.call("rbind", current)
    node_positions <- rbind(node_positions, c( suid = as.numeric(rowtable[,'data.SUID']),
                                               position.x = as.double(rowtable[,'position.x']),
                                               position.y = as.double(rowtable[,'position.y']), 
                                               rowtable[,'data.selected']))
  }
  
  #for the selected nodes, shift the position
  selected_subset <- node_positions[which(node_positions[,'data.selected'] == "TRUE"),1:3]
  class(selected_subset) <- "numeric"
  selected_subset[,'position.y'] <- selected_subset[,'position.y'] + y_offset
  selected_subset[,'position.x'] <- selected_subset[,'position.x'] + x_offset
  
  #create jaon to update the x and y positions
  view_json <- c()
  for(j in 1:dim(selected_subset)[1]){
    param1 = list(visualProperty="NODE_X_LOCATION",
                  value = as.character(selected_subset[j,'position.x']))
    param2 = list(visualProperty="NODE_Y_LOCATION",
                  value = as.character(selected_subset[j,'position.y']))
    current <- list(SUID = as.character(selected_subset[j,'suid']),
                    view=list(param1,param2))
    view_json <- c(view_json, list(current))
  }
  
  view_json <- toJSON(view_json)
  
  #move nodes
  put.viewinfo.url <- paste(base.url,"networks",net_suid,"views",network.viewid,"nodes/",sep="/")
  
  response <- PUT(url=put.viewinfo.url, 
                  body=view_json, encode="json")
  return(response)
  
}


#Apply layout to just the selected nodes 
# Needs to be one of the layours available through cytoscape commands
apply_layout_selected <- function(base.url, selected_layout= "degree-circle"){
  layout.url <- paste(base.url,"commands/layout",selected_layout, sep="/")
  
  layout_params = list(network = "current", nodeList ="selected")
  
  response <- GET(url=layout.url, query =layout_params) 
  return(response)
  
}

#Create visual style for cell cell interaction network
create_cellinteraction_style <- function(style.name, celltypes) {
  # Preepare Defaults
  def.node.border.width <- list(
    visualProperty = "NODE_BORDER_WIDTH",
    value = 0
  )
  
  def.node.transparency <- list(
    visualProperty="NODE_TRANSPARENCY",
    value=230
  )
  
  def.edge.transparency <- list(
    visualProperty="EDGE_TRANSPARENCY",
    value=120
  )
  
  def.edge.width <- list(
    visualProperty="EDGE_WIDTH",
    value=2
  )
  
  def.network.background <- list(
    visualProperty = "NETWORK_BACKGROUND_PAINT",
    value = "white"
  )
  
  defaults <- list(
    def.node.border.width,
    def.edge.width,
    def.node.transparency,
    def.edge.transparency,
    def.network.background
  )
  
  # Mappings
  mappings = list()
  
  colors = c( "rgb(51,0,204)","rgb(0,255,51)", "rgb(204,0,255)","orange","pink","rgb(218,165,32)","rgb(135,206,250)")
  
  celltypes_colors <- c()
  for(i in 1:length(celltypes)){
    current <- list(key = celltypes[i], value = colors[i])
    celltypes_colors = c(celltypes_colors, list(current)) 
  }
  
  # Color mappings
  node.fill.color = list(
    mappingType="discrete",
    mappingColumn="celltypes",
    mappingColumnType="String",
    visualProperty="NODE_FILL_COLOR",
    map=celltypes_colors
  )
  
  #color border orange for significant genes
  border_colors <- c()
  current <- list(key = "significant", value = "orange")
  border_colors = c(border_colors, list(current)) 
  
  # border color
  border.paint = list(
    mappingType="discrete",
    mappingColumn="significant",
    mappingColumnType="String",
    visualProperty="NODE_BORDER_PAINT",
    map=border_colors
  )
  
  #color border orange for significant genes
  border_widths <- c()
  current <- list(key = "significant", value = "10")
  border_widths = c(border_widths, list(current)) 
  
  # border color
  border.widths = list(
    mappingType="discrete",
    mappingColumn="significant",
    mappingColumnType="String",
    visualProperty="NODE_BORDER_WIDTH",
    map=border_widths
  )
  
  prot_shapes <- c()
  current <- list(key = "Ligand", value =  "triangle")
  prot_shapes = c(prot_shapes, list(current)) 
  current <- list(key = "Receptor", value =  "diamond")
  prot_shapes = c(prot_shapes, list(current)) 
  current <- list(key = "Multiple_classifications", value =  "hexagon")
  prot_shapes = c(prot_shapes, list(current))
  
  #node shape mappings
  node.shape = list(
    mappingType="discrete",
    mappingColumn= "protein_type",
    mappingColumnType="String",
    visualProperty="NODE_SHAPE",
    map=prot_shapes
  )
  
  #name mapping
  node.label = list(
    mappingType="passthrough",
    mappingColumn="name",
    mappingColumnType="String",
    visualProperty="NODE_LABEL"
  )
  
  mappings = list(node.label, node.fill.color,node.shape,border.paint,border.widths)
  
  style <- list(title=style.name, defaults = defaults, mappings = mappings)
  return(toJSON(style))
  
}

#Iref has a different way or represening complex membership. Instead of representing them in matrix or spoke instead a fake entity called complexA 
# is created and all genes/proteins that are a part of that complex interact with this fake entity.  This representation would translate in to 
# zero interactions when comparing to our protein set of interest.
# Therefore, translate all irefindex complex interactions into matrix representation.
#
# Given:
# iref_mitab - irefindex expanded mitab representation (from 2013 release) - given that the expanded format is not as stable as compact mitab format
#  if you wish to change to a later release of irefindex (we tried the 2015 release but found that it had significantly less interactions than
#  the 2013 release) it is imporatant that you verify that the expanded mitab fields have not changed. 
# protein_set - vector of genes/proteins to filter the compex interactions by.
compute_complex_interactions_irefindex <- function(iref_mitab,protein_set){
  
  #add the aliases
  #pull out all the hgnc symbols from the interactions in iref
  #extract the HGNC symbols from AliasA and AliasB
  temp1 <-strsplit(iref_mitab[,'aliasA'],'hgnc:')
  AliasA <- unlist(lapply(temp1, function(dat){unlist(strsplit(dat[2],"\\|"))[1]}))
  AliasA<- unlist(lapply(AliasA,as.character))
  
  temp1 <-strsplit(iref_mitab[,'aliasB'],'hgnc:')
  AliasB <- unlist(lapply(temp1, function(dat){unlist(strsplit(dat[2],"\\|"))[1]}))
  AliasB<- unlist(lapply(AliasB,as.character))
  
  #add AliasA and AliasB to the original iref matrix
  iref_mitab <- cbind(AliasA, AliasB, iref_mitab)
  
  #compute edges from complexes
  irefcomplexes  <- iref_mitab[which(iref_mitab['edgetype'] =="C"),]
  
  irefcomplexes <- data.frame(lapply(irefcomplexes, as.character), stringsAsFactors=FALSE)
  
  #some weirdness on windows systems.  uidA column is coming up as x.uidA 
  #if there is a column name x.uidA then change it to uidA
  if(length(which(colnames(irefcomplexes) == "X.uidA")) > 0){
    colnames(irefcomplexes)[which(colnames(irefcomplexes) == "X.uidA")] <- "uidA"
  }
  
  #go through all the unique complexes
  unique_complexes <- unique(irefcomplexes[,'uidA']) 
  
  complex_Data_to_add <- c()
  for(j in 1:length(unique_complexes)){
    complex_rows <- which(irefcomplexes[,'uidA'] == unique_complexes[j])
    subset <- irefcomplexes[complex_rows,];
    
    ids_in_complex <- unique(subset[,'AliasB'])
    FoundInComplex <- which(ids_in_complex %in% protein_set)
    if(length(FoundInComplex) > 1){
      
      for(k in 1:length(FoundInComplex)){
        current_receptor_row <- subset[which(subset[,'AliasB'] == ids_in_complex[FoundInComplex[k]]),][1,]  
        for(l in 1:length(FoundInComplex)){
          if(k != l){
            current_ligand_row <- subset[which(subset[,'AliasB'] == ids_in_complex[FoundInComplex[l]]),][1,]
            #create new row with 56 columns
            new_row <- vector(mode="character", length = 56)
            #receptor is always A
            new_row[c(1,3,5,7,9,14,21,23,25,27,30,37,41,43,45,47,50,53)] <- current_receptor_row[1,c(2,4,6,8,10,15,22,24,26,28,31,38,42,46,44,48,51,54)]
            #ligand is always B
            new_row[c(2,4,6,8,10,15,22,24,26,28,31,38,42,46,44,48,51,54)] <- current_ligand_row[1,c(2,4,6,8,10,15,22,24,26,28,31,38,42,46,44,48,51,54)]
            
            #rest is the same for both so doesn't matter.
            new_row[c(11,12,13,16,17,18,19,20,29,32,33,34,35,36,39,40,49,52,55,56)] <- current_ligand_row[1,c(11,12,13,16,17,18,19,20,29,32,33,34,35,36,39,40,49,52,55,56)]
            complex_Data_to_add <- rbind(complex_Data_to_add , unlist(new_row));
          }        
        }     
      }
    }
  }
  colnames(complex_Data_to_add) <- colnames(iref_mitab)
  return(complex_Data_to_add)
}

# Filter mitab set of interactions by proteins of interest
#
# all_interactions - table of the interactions to filter
# protein_Set - a vector of genes to limit the interaction set by
# pattern - if the fields that we are searching in actualy contains multiple ids then extract the symbols of interest based on this pattern.
# column_alias1 - the column name to find the alias of gene/protein1
# column_alias2 - the column name to find the alias of gene/protein2

filter_mitab_interactions_byproteinset <- function(all_interactions, protein_set,pattern, column_alias1, column_alias2){
  
  #pull out all the hgnc symbols from the interactions in interaction file
  #extract the HGNC symbols from AliasA and AliasB
  #AliasA is the 3rd column
  temp1 <-strsplit(all_interactions[,column_alias1],pattern)
  AliasA <- unlist(lapply(temp1, function(dat){unlist(strsplit(dat[2],"\\|"))[1]}))
  AliasA<- unlist(lapply(AliasA,as.character))
  
  #AliasA is the 4th column
  temp1 <-strsplit(all_interactions[,column_alias2],pattern)
  AliasB <- unlist(lapply(temp1, function(dat){unlist(strsplit(dat[2],"\\|"))[1]}))
  AliasB<- unlist(lapply(AliasB,as.character))
  
  #add AliasA and AliasB to the original iref matrix
  all_interactions <- cbind(AliasA, AliasB, all_interactions)
  
  interaction_set <- all_interactions[which(AliasA %in% protein_set & AliasB %in% protein_set),]
  
  return(interaction_set)
}
