\name{route}
\alias{route}
\title{Grab a route from Google}
\usage{
  route(from, to,
    mode = c("driving", "walking", "bicycling"),
    structure = c("legs", "route"),
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

  \item{structure}{structure of output, see examples}

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
route_df <- route(from, to, structure = 'route')
qmap('college station, texas', zoom = 8) +
  geom_path(
    aes(x = lon, y = lat),  colour = 'red', size = 1.5,
    data = route_df, lineend = 'round'
  )

qmap('college station, texas', zoom = 6) +
  geom_path(
    aes(x = lon, y = lat), colour = 'red', size = 1.5,
    data = route_df, lineend = 'round'
  )

theme_set(theme_bw())
legs_df <- route(from, to, alternatives = TRUE)
p <- qplot(route, minutes, data = legs_df, geom = 'bar',
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

route_df <- legs2route(legs_df)
options('device')$device(width = 7.56, height = 6.84)
qmap('college station, texas', zoom = 8, maptype = 'hybrid', fullpage = FALSE) +
  geom_path(
    aes(x = lon, y = lat, colour = route),
    alpha = 3/4, size = 1.75, data = route_df, lineend='round'
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Routes') +
  opts(title = 'Approximate Routes from Houston to Waco') +
  ggmap:::annotation_custom(ggplotGrob(p),
    xmin = -96.5, xmax = -94.5, ymin = 30.35, ymax = 32.2)

routeQueryCheck()




(legs_df <- route(
  'marrs mclean science, baylor university',
  '220 south 3rd street, waco, tx 76701', # ninfa's
  alternatives = TRUE))

options('device')$device(width = 11.65, height = 4.17)
qmap('424 clay avenue, waco, tx', zoom = 16, maprange = TRUE, maptype = 'hybrid',
    base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
  geom_leg(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
    alpha = 3/4, size = 2, data = legs_df
  ) +
  scale_x_continuous(breaks = pretty(c(-97.1325,-97.119),4), lim = c(-97.1325,-97.119)) +
  facet_wrap(~ route) + theme_bw() +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Routes')
}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{https://developers.google.com/maps/documentation/directions/},
  \code{\link{legs2route}}, \code{\link{routeQueryCheck}},
  \code{\link{geom_leg}}
}

