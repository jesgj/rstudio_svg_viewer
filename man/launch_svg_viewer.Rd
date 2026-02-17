\name{launch_svg_viewer}
\alias{launch_svg_viewer}
\title{Launch the SVG viewer addin}
\usage{
launch_svg_viewer(file = NULL)
}
\arguments{
  \item{file}{Optional path to an SVG file. If supplied, the picker is skipped.}
}
\value{
Invisibly returns \code{TRUE} when closed with Done and \code{FALSE} when cancelled.
}
\description{
Opens a Shiny gadget that renders an SVG file inside RStudio.
If the active editor document is an SVG, it is used automatically.
Otherwise, a file picker is shown.
}
