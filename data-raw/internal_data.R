fix_names <- list("elev" = c("elevation" = "elev",
                             "z" = "elev",
                             "northing" = "y",
                             "latitude" = "y",
                             "lat" = "y",
                             "easting" = "x",
                             "longitude" = "x",
                             "lon" = "x",
                             "long" = "x"))
match_names <- list("elev" = c("x", "y", "elev"),
                    "arule" = c("sortorder", "file_in", "attr_in", "class_out",
                                "model_no", "b", "b_low", "b_hi", "b1", "b2", "d"),
                    "crule" = c("f_name", "fuzattr", "attrwt", "facet_no",
                                "f_code"))

usethis::use_data(fix_names, match_names, internal = TRUE, overwrite = TRUE)

