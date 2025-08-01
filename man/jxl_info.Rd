\name{jxl_info}
\alias{jxl_info}
\title{Get JPEG XL image information}
\usage{
jxl_info(filename)
}
\arguments{
\item{filename}{Path to the JXL file}
}
\value{
List containing image information with the following components:
\item{width}{Image width in pixels}
\item{height}{Image height in pixels}
\item{channels}{Number of color channels}
\item{alpha}{Logical indicating if alpha channel is present}
\item{bits_per_sample}{Bits per sample}
\item{uses_original_profile}{Logical indicating if original color profile is used}
}
\description{
Extracts basic information from a JPEG XL image file including dimensions,
color channels, and other metadata.
}
\examples{
\dontrun{
# Get information about a JXL file
info <- jxl_info("image.jxl")
print(info)
}
}