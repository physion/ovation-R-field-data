InitRovationOSX()


context("MakeDate")
year <- 2013
day_of_year <- 302
tzone <- "America/Chicago"
expect_that(MakeDate(year, day_of_year, tzone=tzone)$getYear(), equals(year))
expect_that(MakeDate(year, day_of_year, tzone=tzone)$getDayOfYear(), equals(day_of_year))
expect_that(MakeDate(year, day_of_year, tzone=tzone)$getZone()$getID(), equals(tzone))
expect_true(MakeDate(year, day_of_year, tzone=tzone)$equals(
            MakeDate(year, day_of_year, tzone=tzone)$toDateMidnight()$toDateTime()))

context("Day ends")
d <- new(J("org.joda.time.DateTime"))
ends <- DayEnds(d)
midnight <- d$toDateMidnight()$toDateTime();
expect_true(ends$start$equals(midnight))
expect_true(ends$end$equals(midnight$plusDays(as.integer(1))$minusSeconds(as.integer(1))))

context("Import CSV")