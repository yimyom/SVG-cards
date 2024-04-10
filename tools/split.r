#!/usr/bin/env Rscript

library(xml2)
library(argparser)
library(glue)
library(stringr)

.f = glue::glue

find_base_dimension <- function(svg_doc)
{
    svg_doc |>
    xml_find_first("/*[name()='svg']/*[name()='defs']") |> # get the common objects (in <defs>)
    xml_find_first('./*[name()="g"][@id="base"]') |>
    xml_child()
}

rebuild_card = function(card, all_defs)
{
    doc = xml_new_root(xml_dtd('svg',"-//W3C//DTD SVG 1.1//EN","http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"))
    svg = xml_add_child(doc, 'svg',
        'xmlns'='http://www.w3.org/2000/svg',
        'xmlns:xlink'='http://www.w3.org/1999/xlink', version='1.1')
    
    defs = xml_add_child(svg, 'defs') # create the defs for this card
    new_card = xml_add_child(svg, card)

    inserted_defs = list()
    for(def in xml_find_all(card, './/*[@xlink:href]')) # get the defs needed for this card
    {
        def_id = xml_attr(def, 'xlink:href') # get id of the required def
        if(is.na(def_id))
            def_id = xml_attr(def,'href')
        def_id = sub('#', '', def_id)

        if(!def_id %in% inserted_defs)
        {
            # find its corresponding group in defs
            required_group <- xml_find_first(all_defs, .f("./*[name()='g'][@id='{def_id}']"))
            if(!is.na(xml_type(required_group)))
            {
                xml_add_child(defs, required_group, ) # add this group to the new defs
                xml_ns_strip(defs)
                inserted_defs[[length(inserted_defs)+1]] = def_id
            }
            else
            {
                print(.f('{def_id} is missing'))
            }
        }
    }
    return(svg)
}

clean_up_attr = function(node, node_type, attr)
{
    if(xml_name(node) == node_type)
    {
        print(as.list(xml_attrs(node)))
        attributes = as.list(xml_attrs(node))
        if(attr %in% names(attributes))
            attributes[[attr]] <- NULL
        xml_set_attrs(node, as.vector(attributes))
        print('_____')
        print(as.list(xml_attrs(node)))
    }
    if(xml_name(node)=='use')
    {
        print(as.list(xml_attrs(node)))
        stop()
    }

    for(child in xml_children(node))
    {
        clean_up_attr(child, node_type, attr)
    }
    print('+++++++++++++++++++++++++++++++++++')
}

#            card_base = xml_find_all(card, './/*[@xlink:href="#base"]')
#            card_x = xml_attr(card_base, 'x') |> as.numeric()
#            card_y = xml_attr(card_base, 'y') |> as.numeric()

reposition_card = function(card, card_id)
{
    filename = fs::file_temp(ext='.svg')
    write_xml(card, filename)
    
    output = system2('inkscape', c('--query-all', filename), stdout = TRUE)
    bbox = grep(card_id, output, value=T) |> stringr::str_split(',')
    svg = xml_find_first(card, "//svg")
    xml_set_attr(svg, 'viewBox', paste0(bbox[[1]][2:5], collapse=' '))
    return(svg)
}

main <- function()
{
    # Parse command line arguments
    parser <- arg_parser('Generate SVG cards deck')
    parser <- add_argument(parser, 'file', help='File to split', type='character')
    argv <- parse_args(parser)

    if(is.null(argv$file))
    {
        stop('Please provide the path to the SVG file using the --file option.')
    }
    else
    {
        svg_file <- argv$file
        svg_doc <- read_xml(svg_file) # load svg file
    
        top_group = xml_find_first(svg_doc, "/*[name()='svg']/*[name()='g']") # get the main group with all the cards
        cards = xml_find_all(top_group, "./*[name()='g']") # find all the cards (as <g> groups)
        defs = xml_find_first(svg_doc, "/*[name()='svg']/*[name()='defs']") # get the common objects (in <defs>)

        for(card in cards)
        {
            # rebuild the card as a single SVG file
            new_card = rebuild_card(card, defs)
            card_id <- xml_attr(card, 'id') # get the card's name

            # adjust the viewBox
            new_card = reposition_card(new_card, card_id)
            # write the result
            write_xml(new_card, .f('{card_id}.svg'))
        }
    }
}

if(!interactive())
{
        main()
}
