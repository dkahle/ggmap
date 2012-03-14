\name{route}
\alias{route}
\title{Grab a route from Google}
\usage{
  route(from, to,
    mode = c("driving", "walking", "bicycling"),
    output = c("simple", "all"), alternatives = FALSE,
    messaging = FALSE, sensor = TRUE,
    override_limit = FALSE)
}
\arguments{
  \item{from}{name of origin addresses in a data frame
  (vector accepted)}

  \item{to}{name of destination addresses in a data frame
  (vector accepted)}

  \item{output}{amount of output}

  \item{mode}{driving, bicycling, or walking}

  \item{alternatives}{should more than one route be
  provided?}

  \item{messaging}{turn messaging on/off}

  \item{sensor}{whether or not the geocoding request comes
  from a device with a location sensor}

  \item{override_limit}{override the current query count
  (.GoogleRouteQueryCount)}
}
\value{
  a data frame (output='simple') or all of the geocoded
  information (output='all')
}
\description{
  Grab a route from Google
}
\examples{
\dontrun{

from <- 'houson, texas'
to <- 'waco, texas'
route_df <- route(from, to)
qmap('college station, texas', zoom = 8) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
    colour = 'red', size = 1.5, data = route_df
  )

qmap('college station, texas', zoom = 6) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
    colour = 'red', size = 1.5, data = route_df
  )

theme_set(theme_bw())
route_df <- route(from, to, alternatives = TRUE)
p <- qplot(route, minutes, data = route_df, geom = 'bar',
    stat = 'identity', fill = factor(leg)) +
  scale_fill_discrete(guide = 'none') +
  labs(x = 'Route', y = 'Time (Minutes)', fill = 'Leg') +
  opts(
    title = 'Route Time by Leg',
    plot.background = theme_rect(fill = 'green'),
    axis.text.x = theme_text(colour = 'white'),
    axis.title.x = theme_text(colour = 'white'),
    axis.text.y = theme_text(colour = 'white'),
    axis.title.y = theme_text(colour = 'white', angle = 90),
    plot.title = theme_text(colour = 'white')
  )

options('device')$device(width = 7.56, height = 6.84)
qmap('college station, texas', zoom = 8, maptype = 'satellite', fullpage = FALSE) +
  geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
    alpha = 3/4, size = 1.75, data = route_df
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Routes') +
  opts(title = 'Approximate Routes from Houston to Waco') +
  ggmap:::annotation_custom(ggplotGrob(p),
    xmin = -96.5, xmax = -94.5, ymin = 30.35, ymax = 32.2)

routeQueryCheck()



}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{https://developers.google.com/maps/documentation/directions/}
}

