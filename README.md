Gater
================
Andrew Wight & Harvey Cantor
Dana-Farber Research Institute, Boston, MA

<img src="vignettes/gaterlogo.png" style="display: block; margin: auto 0 auto auto;" />

# Abstract

It has been possible for several years now to store, analyze, graph, and
summarize flow cytometry data using free and open source software thanks
to the R statistical programming language and cytometry-based packages
hosted in BioConductor. Unfortunately, despite the obvious benefits of a
free and open source cytometry platform with a large existing support
base and simple integration with thousands of cutting edge packages and
analytical advancements, few cytometerists have adopted R as a complete
flow cytometry analysis solution. We believe that this is because
drawing gates, a central task in flow cytometry analysis, is still
extremely cumbersome in R. Currently, gates must either be automatically
derived by unfamiliar computer models or specified by manually typing
point-by-point x-y coordintates for each gate. In this vignette, we
describe gater, a new package that provides a simple and familiar
graphical interface to R that allows gating by drawing gates on top of
flow plots. We believe this is the missing piece that will allow even a
relative newcomer to R to quickly and confidently perform traditional
cytometry analyses. This vignette works through a simple flow analysis
workflow in detail, beginning with package setup and data import,
compensation, and transformation before displaying how to gate the data
using gater, and then concludes with a simple example of how to produce
professional-looking graphs and summary statistics from these gates.
Some very basic experience with R is assumed, but this vignette is
targeted toward a relative beginner and does not assume familiarity with
other BioConductor packages or any previous attempts to analyze flow
cytometry data in R.


# Introduction

The ability to analyze and manipulate flow cytometry data (in the form
of .FCS files) from within the R statistical programming language is
becoming increasingly relevant. With improved throughput of flow
cytometers through readily available auto-sampling machines and
increased complexity enabled by multi-parameter and spectral analyzers,
there is a growing need for programmatic and open-source solutions that
enable efficient and reproducible cytometry analysis. Moreover, recent
advances in flow cytometry, such as the ability to integrate machine
learning and conventional cytometry to allow a 16-colour cytometer to
analyze hundreds of markers in a single sample (Infinity Flow by Becht
et al., <https://www.biorxiv.org/content/10.1101/2020.06.17.152926v1>),
require familiarity with analyzing cytometry data in R. Finally,
analyzing cytometry data in a coding language enables more transparent
and repeatable science, since it is possible to directly comment on each
step of the analysis strategy and to employ version control software to
track any changes to the analysis over time.

There are currently two general strategies for working with cytometry
data in R. The first is to perform the entire analysis from within the R
coding environment. There are some excellent packages and standards that
allow the import of FCS files, compensation, data transformation,
plotting, and exporting summary statistics. However, manual gating of
flow cytometry data remains cumbersome and difficult to perform
accurately. Of note, there are packages available now that enable
automatic, data-driven gates that avoid these problems, but are
themselves complex to properly prepare and validate the automatic gates.
Moreover, these data-driven gates represent an unfamiliar workflow for
the majority of cytometerists that are accustomed to GUI-based analyses
such as those enabled by FlowJo, Kaluza, and other cytometry analysis
software.

The second strategy is to first perform compensation, transformation,
and gating in FlowJo, and then import the resulting workspace object
into R using the flowWorkspace package. This has the advantage of
allowing cytometerists to work in a familiar way while still enabling
the use of cutting-edge cytometry tools such as Infinity Flow. However,
this approach lacks all of the other benefits that an R-native cytometry
package would allow. In particular, it is dependent on expensive,
closed-source software and does not allow for easy commenting and
version control. Nevertheless, manual gating remains sufficiently
difficult in R as to make this the method used in the Infinity Flow
manuscript.

Gater was developed to fill in this missing ability for manual,
GUI-based gating in R, finally enabling a familiar cytometry analysis
pipeline completely within R. This vignette will demonstrate the gater
function within the context of a complete cytometry analysis pipeline
and is geared toward a researcher who has never used R for flow
cytometry analysis at all.

# Setting up gater

Prior to using gater, it must first be installed on your system. Because
it is in active development and not yet submitted to BioConductor, you
will need to install it directly from GitHub. Installing packages from
GitHub is slightly more complex than installing normal R packages, so if
you’ve never done it before, here are some steps to follow:

1.  Install RStudio (<https://rstudio.com>). Gater uses the Shiny
    package to make interactive gating possible, which tends to work
    best from inside the RStudio IDE. You don’t strictly need to use
    RStudio to make this work, but this vignette is assuming that you
    are, and if you don’t have a reason not to use RStudio, I recommend
    that you do at least while you are working through this vignette.
2.  If you are using a Windows operating system, install Rtools
    (<https://cran.r-project.org/bin/windows/Rtools/>)
3.  Make sure that you have a GitHub account (<http://github.com>)
4.  Install BioConductor dependencies (see code below). Once gater is
    submitted to BioConductor, this step won’t be necessary, but for the
    mean time, the code we will use to install gater from github doesn’t
    know how to automatically install dependencies that are hosted on
    BioConductor, so we need to do it ourselves first. Run the following
    code on your computer before proceeding:

<!-- end list -->

``` r
if(!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

if(!requireNamespace("flowCore", quietly = TRUE)){
  BiocManager::install("flowCore")
}

if(!requireNamespace("flowWorkspace", quietly = TRUE)){
  BiocManager::install("flowWorkspace")
}

if(!requireNamespace("ggcyto", quietly = TRUE)){
  BiocManager::install("ggcyto")
}
```

Once you have the BioConductor dependencies installed, you should be
able to run the following code to download and install gater.

``` r
# install devtools if you don't already have it
if(!requireNamespace("devtools", quietly = TRUE)){
  install.packages("devtools")
}

devtools::install_github("NKInstinct/gater")
```

# Preparing your cytometry data

Before you can start drawing gates on your data, you need to read it
into R and perform a few transformations on it (compensation and
fluorescent channel transforms). If this seems a little complex, don’t
worry—it can be a bit much to wrap your head around the first time you
do it, but once you know the options you would like to use, you can
write them all into a single function and then use that for automatic
data import and transformation in the future. We’ll cover preparing just
such a function at the end of this section in case you aren’t sure how
to do that.

## Assemble a flowSet from seperate .FCS files

The example flow data used in this package comes from the GvHD data
included in flowCore, the base package for all flow cytometry data
analysis in R. The GvHD data that comes with flowCore is already stored
in a flowSet, so for this example, you can find the first three samples
from GvHD bundled with gater as individual .FCS files, which we will
turn into a flowSet here.

``` r
library(gater)
library(flowCore)

path_to_fcs <- system.file("extdata", package = "gater")
```

The `system.file()` command above is needed to get at the .FCS files
that have shipped with gater. However, when you are ready to read your
own flow data into R, you can provide a simple path to the directory you
are interested in. For example, if your target directory is something
called “Flow Data” in your current working directory, you could simply
pass \`path\_to\_fcs \<- “\~/Flow Data/” and that would do the same
thing.

``` r

fs <- read.flowSet(path = path_to_fcs,
                   pattern = ".FCS$",
                   full.names = TRUE)
```

Running this command first finds all of the files at the location
specified by path\_to\_fcs and checks if any of them ends in .FCS
(that’s the “pattern =”.FCS$") part of the code). All of them that
satisfy the pattern are loaded into a flowSet called fs. Note that there
are a lot of customization options here - have a look at the flowCore
vignettes if you want to change something about this default behaviour.

### Working with large flowSets

If you have a lot of flow data, you might not want to load it all into a
flowSet following the above instructions. This is because the whole
flowSet we just created exists in RAM, and if it is extremely large, it
might cause problems depending on how much RAM you have available to
you. Instead, you can use the ncdfFlow package to directly access the
data on your hard drive. This will cause all operations on the data to
be slightly slower, but will not consume an enormous chunk of system
RAM. It also has some benefits for keeping all your data and analyses
together, which we’ll touch on when we talk about saving gated cytometry
data. For those reasons, I tend to prefer this NCDF approach for
cytometry analysis, but it doesn’t really matter which one you use,
especially when just starting out (you can always change your mind
later).

``` r
# Not run for the purposes of the vignette
library(ncdfFlow)
fs <- read.ncdfFlowSet(files = list.files(path = path_to_fcs,
                                          pattern = ".FCS$",
                                          full.names = TRUE))
```

## Convert the flowSet to a GatingSet

FlowSets are excellent containers for holding flow data, but they cannot
store information about gating very well. The flowWorkspace package
introduces a new, similar data structure called a GatingSet that holds
both the flowSet we just made and all gating information about it.
Thankfully, once we have made a flowSet, it is very easy to prepare a
GatingSet.

``` r
library(flowWorkspace)
gs <- GatingSet(fs)
```

## Compensate the data

So far we have created a flowSet and then turned it into a GatingSet.
This GatingSet is a container that can hold many different kinds of
information. Currently, it has the raw expression values recorded off of
the cytometer and experimental metadata. Both of these were contained in
the FCS file, and so were loaded into the flowSet and then brought over
to the GatingSet. Next, we are going to add a compensation matrix to
this container, so that the data are properly compensated. For these
specific example data, the compensation matrix is stored in an external
file which we will import and apply to the GatingSet. There are other
ways to compensate, which we will mention below.

``` r
path_to_comp <- system.file("extdata", "compdata", "compmatrix", package = "flowCore")
comp_matrix <- read.table(path_to_comp, header = TRUE, skip = 2, check.names = FALSE)

comp <- compensation(comp_matrix)

gs <- compensate(gs, comp)
```

### Using acquisition-defined compensation

Although the above example uses an externally-stored compensation
matrix, a common use-case will be that you have acquired flow data for
which you performed instrument-level compensation. In this case, the
compensation matrix is saved directly in the .FCS file upon export, and
you can read that into the comp object instead of loading an external
file.

There are several places in a .FCS file where a compensation matrix can
be stored. My Fortessa X20 stores it in the $SPIL slot of a .FCS, but
other cytometers likely behave differently. To find out where yours is,
you need to first call `spillover()` on one of the samples in your
flowSet (**not the GatingSet\!**), and then look at the output. One of
the slots that gets returned to you will look like a compensation
matrix—take note of which one that is and then store it in a variable
called comp.

Here is an example. Note that since the .FCS files used in this example
do not have an acquisition-defined compensation, trying to run this code
using these data will fail.

``` r
# Not run for the purposes of the vignette

# Find out which slot the compensation data are in.
spillover(some_new_fs[[3]])

# You need to select one of the samples contained in the flowSet. I chose the
# third one here ([[3]]), but that is completely arbitrary. The should all have
# the exact same compensation matrix.

# This command should output a list of several objects. One of them should look
# like a compensation matrix. If we were running this command on data from my
# Fortessa, it would be the first object in the list (the $SPIL slot), but look
# at your data and see which object you want to work with. Once you know which
# object is your compensation matrix, proceed.

comp_matrix <- spillover(some_new_fs[[3]])[[1]]

# Note that I have selected the first object in the list, which corresponds to
# where my acquisition-defined matrices are stored. If yours is in a different
# list object, put that object's number in place of the [[1]] above.

# From here, it is exactly like before:

comp <- compensation(comp_matrix)

some_new_gs <- compensate(some_new_gs, comp)
```

### Creating a new compensation matrix from single-colour controls

It is also possible, using the flowCore package, to automatically create
a compensation matrix from single colour control samples. Exactly how to
do this is beyond the scope of this vignette, so you are encouraged to
look into the flowCore vignettes for further instructions. In general,
however, it is a bit of a pain to do, so if you know you are going to be
using gater for your flow analysis, you are probably better off making
sure you have an acquisition-defined compensation matrix or an external
compensation matrix saved as a .csv before proceeding.

## Transforming the fluorescent data

The final step before analyzing the flow data is to apply a
transformation to the fluorescent channels in the data. There are many
different types of transformation available through flowCore,
flowWorkspace, and others, but for this vignette we will stick with the
biexponential transformation as implemented in FlowJo. To transform the
data, we first create a transformation object, and then apply it to the
GatingSet, much like we did with compensation. To do this, however, we
need to know how many linear channels there are, so that we don’t
accidentally apply a biex transform to them as well.

``` r
colnames(gs)
#> [1] "FSC-H" "SSC-H" "FL1-H" "FL2-H" "FL3-H" "FL2-A" "FL4-H" "Time"
```

In this example dataset, there are three linear channels we don’t want
to transform: “FSC-H”, “SSC-H”, and “Time”. With a small number of
parameters like this, we can simply name them manually, like so:

``` r
channels <- c("FL1-H", "FL2-H", "FL3-H", "FL2-A", "FL4-H")
channels
#> [1] "FL1-H" "FL2-H" "FL3-H" "FL2-A" "FL4-H"
```

However, if there were a lot of channels, or if we wanted to write a
general function to handle any flow data in the future, it would be
better to define this programmatically:

``` r
channels <- colnames(gs)[!stringr::str_detect(colnames(gs), "^FSC-.$|^SSC-.$|^Time$")]
channels
#> [1] "FL1-H" "FL2-H" "FL3-H" "FL2-A" "FL4-H"
```

If you haven’t worked much with strings and regular expressions yet,
check out the stringr package for more information on how that code
works. Essentially, it is storing all of the colnames in our GatingSet
object that do not follow the pattern of being “FSC-” or “SSC-” followed
by a letter (so all of the different variants of forward and side
scatter) or variables that are exactly named “Time” (rather than
variables that contain the word “Time”, which could potentially
accidentally exclude some exotic protein names).

Now that we have a list of all of our non-linear channels, we can
transform them.

``` r
transformation_object <- flowjo_biexp_trans()

transformation_list <- transformerList(channels, transformation_object)

gs <- transform(gs, transformation_list)
```

## Putting it all together—create an import function

As mentioned above, all of that seems like a lot of work just to import
the flow data into R. However, a lot of the complexity of this import
step comes from not knowing exactly how your specific experiment is set
up. Once you know that, you can write all of this into a single function
that holds all of your defaults, and then you can just call that
function to import your data. If we were to do that with the above data
import, it would look something like this:

``` r
import_gs <- function(path, pattern, compensation_matrix){
  fs <- read.flowSet(path = path, pattern = pattern, full.names = TRUE)
  gs <- GatingSet(fs)
  
  comp <- compensation(compensation_matrix)
  gs <- compensate(gs, comp)
  
  channels <- colnames(gs)[!stringr::str_detect(colnames(gs), "^FSC-.$|^SSC-.$|^Time$")]
  transformation_object <- flowjo_biexp_trans()
  transformation_list <- transformerList(channels, transformation_object)
  gs <- transform(gs, transformation_list)
  
  return(gs)
}
```

Now that we have defined this function, we can do all of the above steps
in a couple of lines of code:

``` r
path_to_fcs <- system.file("extdata", package = "gater")
path_to_comp<- system.file("extdata", "compdata", "compmatrix", package = "flowCore")
comp_matrix <- read.table(path_to_comp, header = TRUE, skip = 2, check.names = FALSE)

gs <- import_gs(path = path_to_fcs,
                pattern = ".FCS$",
                compensation_matrix = comp_matrix)
```

This is a pretty basic function, and there’s a lot more we could do to
make it more useful for other experiments with slightly different
conditions, but it’s a good start for now and hopefully demonstrates
that once you have the hang of it, importing cytometry data into R is
neither difficult nor time consuming.

# Interactive gating

Now that we have a compensated and transformed GatingSet object holding
all of our flow cytometry data, it is time to start gating through it.
If you were working on your own data, you would probably be able to jump
right in knowing what parameters are in your dataset, but since this is
an example set, it is helpful to have a quick look at the channel names
contained in the data.

There are two kinds of names for each channel in this GatingSet object:
the detector name (such as “FL1-H”) and, if specified, the marker name
(such as “CD15 FITC”). We can access the first kind of name with
`colnames()` like we did above, and the second kind with
`markernames()`:

``` r
colnames(gs)
#> [1] "FSC-H" "SSC-H" "FL1-H" "FL2-H" "FL3-H" "FL2-A" "FL4-H" "Time"

markernames(gs)
#>        FL1-H        FL2-H        FL3-H        FL4-H 
#>  "CD15 FITC"    "CD45 PE" "CD14 PerCP"   "CD33 APC"
```

Note that not every detector name has a corresponding marker name.
Thankfully, gater can handle either kind of name interchangeably, so
it’s not hard to use whichever is more appropriate for the situation.

## Draw your first gate

As with most cytometry experiments, the first thing we are going to look
at is cells, as defined by their forward and side scatter. Ironically,
because this vignette is non-interactive, you will have to do some of
the legwork here yourself to draw your gates. I will annotate this to
help you follow along, but your best bet is to run this code while
reading to see how it works.

``` r
gs_gate_interactive(gs,
                    filterId = "Leukocytes",
                    dims = list("FSC-H", "SSC-H"))
```

    #> [1] 2

When you first run the gs\_gate\_interactive command, a window like this
should appear

<img src="README_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />
The sidebar on the left lets you decide what kind of gate you want to
draw, and the main window in the middle shows a plot of your data that
you can interact with.

To draw a gate, the first thing you need to do is pick what kind of gate
you want to draw. It is very important that you pick the kind of gate
you want to draw **first**, and then draw it second. Doing it the other
way tends to cause errors.

To select your leukocytes, switch the gating mode over to polygon by
clicking on the polygon radio button

<img src="README_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />
Once you have clicked on the kind of gate you want, you can proceed to
draw it. Polygon gates (like this one) are drawn by clicking multiple
times to trace a polygon. Other gates are drawn differently (Rectangle
and Span gates are drawn by clicking and dragging a rectangle, and
Quadrant gates are drawn by clicking once where the four quadrants
meet). Note that when you draw a Polygon gate, the image doesn’t update
to show your polygon in realtime - you’ll just have to remember where
you put your points (though we’re working on a better implementation of
this now).

Go ahead and draw a polygon gate on your data. Again, remember that you
won’t see your polygon while you draw it.

<img src="README_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

If you think you’ve mis-clicked and want to start over, just hit the
“Clear” button on the top left and then start drawing your polygon
again. Once you are happy with it, click “Done” to close the window and
apply the gate to your whole GatingSet.

### Other notes about gating

If something goes wrong when you are drawing your gates (such as if you
draw a polygon gate when you still have “rectangle gate” selected), the
shiny app can hang. If you exit out of the shiny window without clicking
on “done” first, it’s a good idea to make sure that the shiny app isn’t
still running in the background. Have a look at your R Console window
and see if there is a stop sign in the upper-right corner.

<img src="README_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />
If that button is there, it means that the shiny app is still running
and you should stop it before proceeding. Trying to do anything else in
R while the shiny app runs in the background can cause all kinds of
mysterious errors.

Another thing to note is that when we run `gs_gate_interactive()` on our
data, the resulting gate gets stored in the GatingSet along with
everything else. Sometimes, it can be helpful to have that gate stored
in its own variable within R, such as when you want to apply the same
gate to multiple nodes within the GatingSet, you want to apply it to
different GatingSets, or you just want an easily referenced record of
the gate’s dimensions. To do this, simply assign the output of
`gs_gate_interactive()` to a variable name, such as `gate1 <-
gs_gate_interactive()`. Note that all of these actions can also be
performed on gates within a GatingSet using the flowWorkspace package,
but if you know ahead of time that you are going to want the gate for
multiple purposes, it is easier to assign it to a variable and then go
from there.

## Plot the data with the new gate

Now that we have drawn a leukocytes gate, it is a good idea to have a
look at the plot and see that it looks the way we want it to. There are
a number of ways to plot flow cytometry data in R—my favourite is with
the ggcyto package, which is automatically installed with gater for
visualization purposes. Since we only want to peek at the data right now
to make sure our gate looks right, we can use the easy-but-rigid
`autoplot()` function, like so:

``` r
autoplot(gs[[1]], gate = "Leukocytes")
#> Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

<img src="README_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

The important thing here is that you have a gate drawn on a hex plot
with a percentage in the middle of it—don’t worry if it doesn’t look
exactly like the one here, and don’t worry if the plot doesn’t look the
way you want it to for publication. We’ll cover how to make very nice
plots at the end.

**Common Mistake:** if, when you run autoplot, you don’t see a gate but
you do see a big “0%” sitting roughly where your gate should be, that
means you didn’t switch the window to polygon gate before drawing your
polygon, and the program is trying to make a rectangle gate out of the
very first point you clicked for your polygon (hence an invisibly-small
rectangle with 0% of the events in it). Again, we’re working on making
this mistake harder to make, but for now, just redo the gate by
re-running `gs_gate_interactive()` again. Just make sure you add `regate
= TRUE` to the command so gater knows to delete this Leukocytes gate
before trying to add another Leukocytes gate to root.

## Draw more gates

Now that we have a Leukocytes gate drawn, we can drill down into them
and gate on the other markers in our sample. The next likely gate will
be to take all of the CD45+ cells for further analysis. For illustrative
purposes, let’s gate this one with a 1-D span gate on a histogram.

``` r
gs_gate_interactive(gs,
                    filterId = "CD45",
                    dims = "CD45",
                    subset = "Leukocytes")
```

    #> [1] 3

Note that this time, unlike previously, we specify the dimensions
(`dims`) we want to work with (CD45), and also the subset we want to
look at (the “parent gate”, in this case Leukocytes). Running
`gs_gate_interactive()` with these arguments will draw a histogram
instead of a dot plot, but otherwise the window will look as before.

<img src="README_files/figure-gfm/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />
Switch the gating mode over to “Span”

<img src="README_files/figure-gfm/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

Then draw your gate. For span and rectangle gates, you draw them by
clicking and dragging a rectangle on the plot. In fact, the only
difference between span and rectangle gates is that span only considers
the horizontal dimensions, while rectangle considers both. So for this
gate, you can draw the rectangle as *tall* as you want, since span gates
don’t care about the vertical dimensions of the rectangle. This can be
useful for drawing the gate exactly where you want it, since you can use
the vertical dimensions to help line it up with your histogram peaks.

<img src="README_files/figure-gfm/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

As before, when you are happy with the gate, click Done to close the
window and apply the gate. Unlike with polygon and quadrant gates, you
don’t need to click Clear if you want to adjust the rectangle on your
graph—you can just adjust or redraw it as many times as you like (note:
nothing bad will happen if you click Clear when drawing rectangles, so
don’t worry if you do).

Now, as before, we can have a peek at the gate. This time, however,
we’re going to use the more expanded ggcyto command to start to get
familiar with it. ggcyto uses the same grammar of graphics that ggplot
uses, so if you are already familiar with ggplot, this should be very
straightforward. If not, a full introduction to the grammar of graphics
is beyond the scope of thie vignette, but look at the code below and see
if you can follow what is happening.

``` r
ggcyto(gs[[1]], aes("CD45")) +
  geom_density() +
  geom_gate("CD45") +
  geom_stats()
```

<img src="README_files/figure-gfm/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />
The idea behind the grammar of graphics is that you can draw any graph
by first specifying the data the graph comes from, and then specifying
the kind of image you want to make from those data. In this case, the
first line of code tells ggcyto that we want to make a graph based on
the first sample contained in gs (`gs[[1]]`), and that we want to look
at the CD45 dimension of these data. In the grammar of graphics, this is
called an aesthetic, hence “aes”.

After this line specifying what sort of data we want to look at, the
next three apply types of images to the data. The first,
`geom_density()`, draws a density plot of the CD45 aesthetic in our
data. The second, `geom_gate("CD45")`, adds the gate named “CD45” to our
data. The third, `geom_stats()`, adds all relevant stats (in this case
the percent parent) to any gates drawn on the graph.

Using `ggcyto()` instead of `autoplot()` is a little more cumbersome to
type, but provides much more customization to the resulting graph, and
is well worth learning how to use if you are regularly going to use R
and gater for flow analysis. For that reason, the rest of this vignette
will use `ggcyto()` calls to generate graphs, so we can get more
familiar with what they look like.

## Draw the last quadrant gate

The last gate to add to this plot is a quadrant gate on CD33 and CD15.
As before, we’ll call `gs_gate_interactive()` with the appropriate
arguments, then switch our gating mode to quadrant, and then click
exactly once, where you want the center of the quadrant gate to be. As
with the polygon gate, if you mis-click, you can click on Clear to reset
your selection and then try again.

``` r
gs_gate_interactive(gs[[1]],
                    filterId = "CD33 CD15",
                    dims = list("CD33", "CD15"),
                    subset = "CD45",
                    bins = 64) 
# I set the bins lower because we have fewer cells after drilling down twice
```

    #> [1] 4 5 6 7

As before, we can check the gate by plotting it with ggcyto

``` r
ggcyto(gs[[1]], aes("CD33", "CD15")) +
  geom_hex(bins = 64) +
  geom_gate("CD33 CD15") +
  geom_stats()
```

But wait\! Running this command gives an error: the gate “CD33 CD15”
isn’t found. It’s always a good idea to check your spelling when you
see errors like this, but we can confirm that we did definitely just
make a quadrant gate called “CD33 CD15” and apply it to gs, so it should
be in there like any others.

You may already have a hunch of what’s going on here, but to check for
sure, it’s a good idea to ask gs for the names of all stored gates

``` r
gs_get_pop_paths(gs)
#> [1] "root"                                 "/Leukocytes"                         
#> [3] "/Leukocytes/CD45"                     "/Leukocytes/CD45/CD33 APC-CD15 FITC+"
#> [5] "/Leukocytes/CD45/CD33 APC+CD15 FITC+" "/Leukocytes/CD45/CD33 APC+CD15 FITC-"
#> [7] "/Leukocytes/CD45/CD33 APC-CD15 FITC-"
```

As you can see above, when you draw a quadrant gate, it actually puts
four new gates on the plot (one for each quadrant). Since all four of
these can’t be named “CD33 CD15”, the quadrant gate ignores the filterId
you give it and makes up unique names for the four gates based on the
marker names involved in the plot. So to plot this quadrant gate, we
need to specify all four of these gates.

``` r
ggcyto(gs[[1]], aes("CD33", "CD15")) +
  geom_hex(bins = 64) +
  geom_gate(c("CD33 APC-CD15 FITC+",
              "CD33 APC+CD15 FITC+",
              "CD33 APC+CD15 FITC-",
              "CD33 APC-CD15 FITC-")) +
  geom_stats()
```

<img src="README_files/figure-gfm/unnamed-chunk-27-1.png" style="display: block; margin: auto;" />
If you are comfortable with the stringr package, you can also specify
this a little more efficiently by first selecting all of the population
paths that contain the word “CD33” and then passing this list to
geom\_gate:

``` r
quadgates <- gs_get_pop_paths(gs)[stringr::str_detect(gs_get_pop_paths(gs), "CD33")]

ggcyto(gs[[1]], aes("CD33", "CD15")) +
  geom_hex(bins = 64) +
  geom_gate(quadgates) +
  geom_stats()
```

<img src="README_files/figure-gfm/unnamed-chunk-28-1.png" style="display: block; margin: auto;" />
Again, this isn’t that important, so if stringr is unfamiliar to you,
don’t worry about it yet and come back to this idea when you’re more
comfortable with working with strings.

## Save your GatingSet object

Once you have your gates drawn to your satisfaction, the last thing to
do is to save your GatingSet object so that you don’t need to re-draw
your gates when you come back to them. One detail about the
flowWorkspace package that we haven’t covered yet is that GatingSet
objects are very different from most R objects, and so saving your
GatingSet using normal R proceedures will fail (i.e. if you try to save
it as a .Rds it won’t load properly). If you want to understand what is
going on under the hood, have a look at the flowWorkspace vignette. To
save this object, you need the `save_gs()` function from flowWorkspace.
This saves your GatingSet object as a directory that you can then load
in with `load_gs()`. Although not necessary, I like to end this
directory name with “.gs” to remind myself that the whole directory is
the GatingSet object, so it’s helpful to think of it more like a single
file than a directory.

``` r
save_gs(gs, "GvHD GatingSet.gs")

#Load it back in
gs <- load_gs(file.path("GvHD GatingSet.gs"))
```

One very important note here—the `load_gs()` command is very sensitive
to the way file paths are specified in a system, and in particular fails
when you try to use it in a Windows environment. Wrapping your filepath
with `file.path()` from base R will solve this problem and make it work
across any OS.

It’s also worth mentioning that another benefit of the NCDF style of
flow cytometry data that we mentioned all the way back in data import is
that the NCDF data itself gets saved inside this GatingSet, which means
that the whole thing (data, gates, compensation, etc) are stored in your
GatingSet.gs directory. This is another reason that I like using the
NCDF style of import, but as long as you don’t move the .FCS files that
are making up your GatingSet, you shouldn’t run into any problems if you
load the GatingSet through the conventional workflow (just like in
FlowJo - don’t move your FCS files around after starting to analyze
them\!).

# Data Export—Images and Summary Statistics

Now that we have gated on our samples, the last step is to have a look
at the different flow samples and pick some example plots to show
others, as well as extract statistics like percent of parent populations
and MFI. Again, the flowCore, ggcyto, and flowWorkspace packages are
very feature-rich in this department, so what we will show below is only
the tip of the iceberg. However, this should cover many of the
conventional cytometry use cases and let you do complete, basic
cytometry analysis using only R.

## Plot data nicely

In our above analysis, let’s pretend that the final goal was to show
whether CD33 and CD15 expression changes between the different samples.
Since there’s only three samples in the GatingSet, the best way to do
this is just plot three dotplots showing each sample’s CD33 and CD15
expression, gated with our quadrant gate. Fortunately, we already know
how to do this. Remember how above, each time we plotted something, we
specified that we only wanted the first sample in gs with `gs[[1]]`? All
we need to do is not specify anything and ggcyto will plot all three
graphs.

``` r
quadgates <- gs_get_pop_paths(gs)[stringr::str_detect(gs_get_pop_paths(gs), "CD33")]

ggcyto(gs, aes("CD33", "CD15")) +
  geom_hex(bins = 64) +
  geom_gate(quadgates) +
  geom_stats()
```

<img src="README_files/figure-gfm/unnamed-chunk-30-1.png" style="display: block; margin: auto;" />

These graphs do a good job conveying the information we want to show,
but they look a little primitive compared to graphs that FlowJo
produces, so let’s try to use the huge customization possibilities in
ggcyto to make them a little nicer. We’ll change three things: change
the gates from bright red to a semi-transparent grey, make the stats
overlays slightly more friendly numbers, and increase the bins to make
the dots a little less crude. Have a look at the code below and see if
you can understand what is happening before reading on. 

__NB:__ the ggcyto package insists on "colour" as the only spelling of 
the word. Trying to set the "color" of your gate will result in hours
of fruitless debugging.

``` r
ggcyto(gs, aes("CD33", "CD15")) +
  geom_hex(bins = 128) +
  geom_gate(quadgates, colour = "grey3", alpha = 0.2) +
  geom_stats(digits = 1)
```

<img src="README_files/figure-gfm/unnamed-chunk-31-1.png" style="display: block; margin: auto;" />

That’s starting to look much classier, while still clearly presenting
the data. The last thing I like to do with presentation plots is to
remove the grey in the background. Changing the overall appearance of a
plot is accomplished with the `theme_` family of layers to add to a
ggcyto object. There are some default themes, as well as packages with
other nice ones out there. It’s also possible to get extremely fine
control over theme elements with the `theme()` layer, but that’s an
advanced topic that you should look into the first time you run into a
theme problem you can’t fix with a pre-loaded one.

``` r
ggcyto(gs, aes("CD33", "CD15")) +
  geom_hex(bins = 128) +
  geom_gate(quadgates, colour = "grey3", alpha = 0.2) +
  geom_stats(digits = 1) +
  theme_minimal()
```

<img src="README_files/figure-gfm/unnamed-chunk-32-1.png" style="display: block; margin: auto;" />

## Retrieving summary statistics

In this case, we only care about CD33 and CD15 expression on three
samples, so the graphs above are sufficient to convey all the
information we want. Normally, however, it’s necessary to retrieve some
summary statistics such as percent of parent or MFI from many different
populations and then graph them somehow. flowWorkspace has a number of
functions we can use to access this information. The easiest is
`gs_pop_get_counts_fast()`, which returns a basic table of population
counts without any fuss.

``` r
gs_pop_get_count_fast(gs)
#>           name                           Population           Parent Count
#>  1: s10a01.FCS                          /Leukocytes             root  1320
#>  2: s10a01.FCS                     /Leukocytes/CD45      /Leukocytes   581
#>  3: s10a01.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+ /Leukocytes/CD45    58
#>  4: s10a01.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+ /Leukocytes/CD45    18
#>  5: s10a01.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC- /Leukocytes/CD45    79
#>  6: s10a01.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC- /Leukocytes/CD45   426
#>  7: s10a02.FCS                          /Leukocytes             root  1273
#>  8: s10a02.FCS                     /Leukocytes/CD45      /Leukocytes  1067
#>  9: s10a02.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+ /Leukocytes/CD45    37
#> 10: s10a02.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+ /Leukocytes/CD45     7
#> 11: s10a02.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC- /Leukocytes/CD45    32
#> 12: s10a02.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC- /Leukocytes/CD45   991
#> 13: s10a03.FCS                          /Leukocytes             root  2319
#> 14: s10a03.FCS                     /Leukocytes/CD45      /Leukocytes  2227
#> 15: s10a03.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+ /Leukocytes/CD45    14
#> 16: s10a03.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+ /Leukocytes/CD45     0
#> 17: s10a03.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC- /Leukocytes/CD45     2
#> 18: s10a03.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC- /Leukocytes/CD45  2211
#>     ParentCount
#>  1:        3420
#>  2:        1320
#>  3:         581
#>  4:         581
#>  5:         581
#>  6:         581
#>  7:        3405
#>  8:        1273
#>  9:        1067
#> 10:        1067
#> 11:        1067
#> 12:        1067
#> 13:        3435
#> 14:        2319
#> 15:        2227
#> 16:        2227
#> 17:        2227
#> 18:        2227
```

However, geting anything useful out of that table will require a bit
more data cleaning. Instead, we can use `gs_pop_get_stats()`, which is a
more robust version of `gs_pop_get_counts_fast()`. Here, we can specify
whether we want percent (of parent) or count, or can even specify our
own functions to derive a stat. We can also specify certain gates if we
don’t want to look at all the populations contained within the
GatingSet.

``` r
gs_pop_get_stats(gs, type = "percent")
#>         sample                                  pop      percent
#>  1: s10a01.FCS                                 root 1.0000000000
#>  2: s10a01.FCS                          /Leukocytes 0.3859649123
#>  3: s10a01.FCS                     /Leukocytes/CD45 0.4401515152
#>  4: s10a01.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+ 0.0998278830
#>  5: s10a01.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+ 0.0309810671
#>  6: s10a01.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC- 0.1359724613
#>  7: s10a01.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC- 0.7332185886
#>  8: s10a02.FCS                                 root 1.0000000000
#>  9: s10a02.FCS                          /Leukocytes 0.3738619677
#> 10: s10a02.FCS                     /Leukocytes/CD45 0.8381775334
#> 11: s10a02.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+ 0.0346766635
#> 12: s10a02.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+ 0.0065604499
#> 13: s10a02.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC- 0.0299906279
#> 14: s10a02.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC- 0.9287722587
#> 15: s10a03.FCS                                 root 1.0000000000
#> 16: s10a03.FCS                          /Leukocytes 0.6751091703
#> 17: s10a03.FCS                     /Leukocytes/CD45 0.9603277275
#> 18: s10a03.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+ 0.0062864841
#> 19: s10a03.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+ 0.0000000000
#> 20: s10a03.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC- 0.0008980692
#> 21: s10a03.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC- 0.9928154468
#>         sample                                  pop      percent

gs_pop_get_stats(gs, type = "percent", nodes = "CD45")
#>        sample  pop   percent
#> 1: s10a01.FCS CD45 0.4401515
#> 2: s10a02.FCS CD45 0.8381775
#> 3: s10a03.FCS CD45 0.9603277
```

Specifying our own function is how we get MFI from this command,
although the resulting table is a little different in that it returns
the MFI for each channel for each population:

``` r
gs_pop_get_stats(gs, type = pop.MFI)
#>         sample                                  pop FSC-Height SSC-Height CD15 FITC
#>  1: s10a01.FCS                                 root      197.0      145.5  517.4555
#>  2: s10a01.FCS                          /Leukocytes      346.0      106.0  480.1491
#>  3: s10a01.FCS                     /Leukocytes/CD45      331.0       87.0  499.3827
#>  4: s10a01.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+      428.0      232.0 1377.8764
#>  5: s10a01.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+      390.5      129.0 1259.4863
#>  6: s10a01.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC-      336.0       71.0  456.1987
#>  7: s10a01.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC-      320.0       84.0  497.4192
#>  8: s10a02.FCS                                 root      155.0       73.0  464.5641
#>  9: s10a02.FCS                          /Leukocytes      311.0       88.0  475.5759
#> 10: s10a02.FCS                     /Leukocytes/CD45      322.0       88.0  476.7680
#> 11: s10a02.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+      477.0      291.0 1218.3777
#> 12: s10a02.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+      639.0      369.0 1153.6484
#> 13: s10a02.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC-      385.5      113.5  463.6175
#> 14: s10a02.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC-      315.0       85.0  475.9509
#> 15: s10a03.FCS                                 root      256.0       91.0  459.5044
#> 16: s10a03.FCS                          /Leukocytes      307.0       98.0  456.9625
#> 17: s10a03.FCS                     /Leukocytes/CD45      305.0       98.0  456.5277
#> 18: s10a03.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC+      358.5      111.5 1321.7644
#> 19: s10a03.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC+        NaN        NaN       NaN
#> 20: s10a03.FCS /Leukocytes/CD45/CD33 APC+CD15 FITC-      425.0      193.5  468.7266
#> 21: s10a03.FCS /Leukocytes/CD45/CD33 APC-CD15 FITC-      305.0       98.0  456.4223
#>         sample                                  pop FSC-Height SSC-Height CD15 FITC
#>       CD45 PE CD14 PerCP  CD33 APC Time (51.20 sec.)
#>  1:  593.7854  410.67198  495.0520             270.0
#>  2: 1015.5518  377.97733  524.0503             272.0
#>  3: 1749.9963   62.67339  583.5952             264.0
#>  4: 1788.1024  103.03740  623.2792             281.5
#>  5: 1818.2706  -24.10231 1416.1251             277.0
#>  6: 1743.1720   60.99744 1226.6180             269.0
#>  7: 1747.6771   62.33017  560.8105             261.0
#>  8:  681.5362  436.36536  478.8656             378.0
#>  9: 1652.6788  109.78046  552.9479             367.0
#> 10: 1706.7782   73.31882  568.2742             367.0
#> 11: 1923.8468 -117.46257  750.1991             328.0
#> 12: 2135.8716 -493.54919  923.8947             609.0
#> 13: 1736.9251   43.08282  931.7885             372.0
#> 14: 1695.8188   80.76798  562.0908             367.0
#> 15: 1794.2864  -15.21270  578.0396             373.0
#> 16: 1882.1064 -105.20982  601.4911             382.0
#> 17: 1889.7155 -113.13597  603.1785             382.0
#> 18: 1861.1179 -118.04110  601.9433             358.0
#> 19:       NaN        NaN       NaN               NaN
#> 20: 2094.7684 -400.34341 1131.9677             100.5
#> 21: 1889.7155 -113.05835  603.1652             382.0
#>       CD45 PE CD14 PerCP  CD33 APC Time (51.20 sec.)
```

## Where to go from here

As with any tabular data in R, you can save any of these results tables
as .csv files by storing them in a variable and then calling
`write.csv()` around it:

``` r
results <- gs_pop_get_stats(gs, type = "percent")

writs.csv(results, "results.csv")
```

This would let you analyze these data in any software you like, just
like you already do with your analysis software of choice. However, R is
a powerhouse of analysis and graphing capabilities, and so the best
thing to do with these data is to analyze and display them in R. This is
a huge subject that goes far beyond the scope of this vignette, but if
you’ve never tried R’s graphing and data carpentry capabilities before,
I highly recommend Wickham and Grolemund’s R for Data Science
(<https://r4ds.had.co.nz/>), which will give you an excellent
introduction to all of these subjects and let you quickly plot graphs
from these data without needing any external software.

# Acknowledgements

The authors thank Dr. Meromit Singer and the HMS computing core for 
help getting our lab started in applying bioinformatics methods. The 
original code for gater was based on the `choose_cells()` function from 
Monocle3 by the Trapnell lab 
(<https://cole-trapnell-lab.github.io/monocle3/>). Andrew Wight was 
funded by an AAI Intersect fellowship for cross-training immunologists 
in computational biology.
