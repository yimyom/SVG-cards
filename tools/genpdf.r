#!/usr/bin/env Rscript

library(argparser)
library(glue)
library(stringr)
library(fs)
library(foreach) |> suppressPackageStartupMessages()
library(doFuture) |> suppressPackageStartupMessages()

plan(multicore, workers=4)

.f = glue::glue

main <- function(argv=NULL)
{
    # Parse command line arguments
    parser <- arg_parser('Convert SVG cards to PDF format')
    parser <- add_argument(parser, 'dir', help='dir containing all the SVG files to convert', type='character')
    parser <- add_argument(parser, 'dest', help='destination dir', type='character')
    if(is.null(argv))
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

        if(!dir_exists(argv$dest))
            dir_create(argv$dest)

        # Convert svg to various formats
#        foreach(file = files) %dofuture%
        for(file in files)
        {
            filename = file |> path_file() |> path_ext_remove()
            pdf_output = dest_dir / 'pdf' / filename |> path_ext_set('pdf')
            options = c(
                '--batch-process',
                .f("--actions='export-id:{filename};export-id-only;export-filename:{pdf_output};export-do'"),
                file)
            output = system2('inkscape', options, stdout=TRUE)#, stderr=FALSE)
        } #-> dev_null
    }
}

if(!interactive())
{
    main()
}
