type t

// Construct a new WHATWG URL
@new external make: string => t = "URL"
@new external makeWithBase: (string, string) => t = "URL"

// Common URL properties
@get external href: t => string = "href"
@get external protocol: t => string = "protocol"
@get external host: t => string = "host"         // may include ":port"
@get external hostname: t => string = "hostname" // without port
@get external port: t => string = "port"
@get external pathname: t => string = "pathname"
@get external search: t => string = "search"

// String conversion
@send external toString: t => string = "toString"
