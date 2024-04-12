#!/usr/bin/env Rscript

library(rsvg, quietly=T) |> suppressPackageStartupMessages()
library(argparser)
library(glue)
library(stringr)
library(fs)
library(jpeg)
library(png)
library(xml2)
library(foreach)
library(doFuture)

plan(multicore)

.f = glue::glue

main <- function()
{
    # Parse command line arguments
    parser <- arg_parser('Generate SVG cards decks in various formats')
    parser <- add_argument(parser, 'dir', help='dir containing all the SVG files to convert', type='character')
    parser <- add_argument(parser, 'dest', help='destination dir', type='character')
    argv <- parse_args(parser)

    if(is.null(argv$dir) || is.null(argv$dest))
    {
        stop('Missing arguments')
    }
    else
    {
        # Find SVG files
        svg_dir = fs::path(argv$dir)
        dest_dir = fs::path(argv$dest)
        files = fs::dir_ls(svg_dir, glob='*.svg')
        files = grep('svg-cards\\.svg', files, value=T, invert=T) 

        # Create output dirs
        formats = c('png','pdf','webp','jpg')
        for(fmt in formats)
        {
            fmt_dest_dir = dest_dir / fmt
            if(!dir_exists(fmt_dest_dir))
                dir_create(fmt_dest_dir)
        }

        # Convert svg to various formats
        foreach(file = files) %doFuture%
        {
            sizes = data.frame(size = c(1,2,4,8), name = c('small','medium','big','huge'))
            for(i in 1:nrow(sizes))
            {
                view_box = read_xml(file) |> xml_find_first('//svg') |> xml_attr('viewBox')
                w = stringr::str_split(view_box, ' ')[[1]][3] |> as.numeric()
                h = stringr::str_split(view_box, ' ')[[1]][4] |> as.numeric()

                w = w*sizes$size[i]
                h = h*sizes$size[i]

                filename = file |> path_file() |> path_ext_remove()

                png_output = dest_dir / 'png' / .f('{filename}_{sizes$name[i]}') |> path_ext_set('png')
                webp_output = dest_dir / 'webp' / .f('{filename}_{sizes$name[i]}') |> path_ext_set('webp')
                jpg_output = dest_dir / 'jpg' / .f('{filename}_{sizes$name[i]}') |> path_ext_set('jpg')
            
                # Do the conversions
                rsvg_png(file, png_output, width=w*i, height=h*i)
                rsvg_webp(file, webp_output, width=w*i, height=h*i)
                print(file);print(png_output); print(sizes[i,])
                print('----------------------')
                print(png_output)
                readPNG(png_output) |> writeJPEG(target=jpg_output, quality=1)
            }
            #pdf_output = dest_dir / 'pdf' / .f('{filename}') |> path_ext_set('pdf')
            #rsvg_pdf(file, pdf_output)
        }
    }
}

if(!interactive())
{
        main()
}
