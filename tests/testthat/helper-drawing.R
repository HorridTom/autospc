# Draw a plot to a null device.
#
# A bare print() with no device open makes R open the default one, which is
# pdf(), leaving an Rplots.pdf in tests/testthat. Tests that have to draw -
# because what they check only happens at draw time - use this instead.
drawn <- function(plot) {

  pdf(NULL)
  on.exit(dev.off())

  print(plot)

}
