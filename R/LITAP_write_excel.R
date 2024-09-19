
create_excel <- function(file, meta, x1, x2, x3, x4, x5, x6) {

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "SiteSum")

  wb <- xl_add_meta(wb, meta, start = c(2, 1))
  wb <- xl_add_x1(wb, x1, start = c(13, 1), name = "Table 2. Topographic derivatives of each slope segment on the representative hillslope")
  wb <- xl_add_x2(wb, x2, start = c(25, 1), name = "Table 3. Area-weighted average values of selected topographic derivatives for each slope segment")
  wb <- xl_add_x3(wb, x3, start = c(13, 10), name = "Table 4. Statistics of selected topographic derivatives (area-weighted)")
  wb <- xl_add_x4(wb, x4, start = c(25, 10), name = "Table 5. Indexes, parameters and ratios characterizing different aspects of the landscape topography")
  wb <- xl_add_x5(wb, x5, start = c(40, 1), name = "Table 6. Location (X) and relief (Z) of points (at the top of the slope and the end of each segment) along the modal hillslopes")
  #wb <- xl_add_x6(wb, x6, start = c(39, 10), name = "Slope by Z")

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# Add Meta
xl_add_meta <- function(wb, meta, start = c(2, 1)) {
  r <- start[1]
  c <- start[2]
  c2 <- 14

  m <- tidyr::pivot_longer(meta, cols = dplyr::everything(),
                           values_transform = as.character)

  add_table_name(wb, "Run Information", r = r, c = c)
  openxlsx::writeData(wb, 1, m[1:4, ], startCol = c, startRow = r+1, colNames = FALSE)

  add_table_name(wb, "Table 1. Metadata for terrain analysis", r = r+6, c = c)
  openxlsx::writeData(wb, 1, meta[, 5:ncol(meta)], startCol = c, startRow = r+7)
  add_table_style(wb, r+7, r+8, c, c2)
  wb
}

xl_add_x1 <- function(wb, x, start = c(11, 1), name) {
  r <- start[1] + 1
  r2 <- start[1] + nrow(x) + 1
  c <- start[2]
  c2 <- start[2] + ncol(x) - 1

  add_table_name(wb, name, r - 1, c)

  openxlsx::writeData(wb, 1, x, startCol = c, startRow = r)
  openxlsx::deleteData(wb, 1, cols = c, rows = r)

  # Add rounding to the cell formats
  openxlsx::addStyle(wb, 1, d0(), rows = (r+1):(r+6), cols = (c+1):c2, gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d1(), rows = c(r+3, r+7), cols = (c+1):c2, gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d2(), rows = r+2, cols = (c+1):c2, gridExpand = TRUE)
  add_table_style(wb, r, r2, c, c2)
  wb
}

xl_add_x2 <- function(wb, x, start = c(21, 1), name) {
  r <- start[1] + 1
  r2 <- start[1] + nrow(x) + 1
  c <- start[2]
  c2 <- start[2] + ncol(x) - 1

  add_table_name(wb, name, r - 1, c)

  openxlsx::writeData(wb, 1, x, startCol = c, startRow = r)
  openxlsx::deleteData(wb, 1, cols = c, rows = r)

  # Add rounding to the cell formats
  openxlsx::addStyle(wb, 1, d0(), rows = r+2, cols = (c+1):c2, gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d1(), rows = c(r+1, (r+3):r2), cols = (c+1):c2, gridExpand = TRUE)
  add_table_style(wb, r, r2, c, c2)
  wb
}

xl_add_x3 <- function(wb, x, start = c(30, 1), name) {
  r <- start[1] + 1
  r2 <- start[1] + nrow(x) + 1
  c <- start[2]
  c2 <- start[2] + ncol(x) - 1

  add_table_name(wb, name, r - 1, c)

  openxlsx::writeData(wb, 1, x, startCol = c, startRow = r)

  # Add rounding to the cell formats
  openxlsx::addStyle(wb, 1, d0(), rows = r+c(5, 7), cols = (c+2):c2, gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d1(), rows = r+c(4,6,8), cols = (c+2):c2, gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d2(), rows = r+c(1:3, 9), cols = (c+2):c2, gridExpand = TRUE)

  add_table_style(wb, r, r2, c, c2, c_labels = 1:2)
  wb
}

xl_add_x4 <- function(wb, x, start = c(42, 1), name) {
  r <- start[1] + 1
  r2 <- start[1] + nrow(x) + 1
  c <- start[2]
  c2 <- start[2] + ncol(x) - 1

  add_table_name(wb, name, r - 1, c)

  openxlsx::writeData(wb, 1, x, startCol = c, startRow = r)

  # Add rounding to the cell formats
  openxlsx::addStyle(wb, 1, d0(), rows = r+c(4, 8:9), cols = (c+2):c2, gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d1(), rows = r+c(1:3, 5:7), cols = (c+2):c2, gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d2(), rows = r+c(10:12), cols = (c+2):c2, gridExpand = TRUE)
  add_table_style(wb, r, r2, c, c2, c_labels = 1:2)
  wb
}


xl_add_x5 <- function(wb, x, start = c(55, 1), name) {
  r <- start[1] + 1
  r2 <- start[1] + nrow(x) + 1
  c <- start[2]
  c2 <- start[2] + ncol(x) - 1

  add_table_name(wb, name, r - 1, c)

  openxlsx::writeData(wb, 1, x, startCol = c, startRow = r)
  openxlsx::deleteData(wb, 1, cols = c, rows = r)

  # Add rounding to the cell formats
  openxlsx::addStyle(wb, 1, d0(), rows = r:r2, cols = c:(c+6), gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, d2(), rows = r:r2, cols = (c+7):c2, gridExpand = TRUE)
  add_table_style(wb, r, r2, c, c2)
  wb
}

xl_add_x6 <- function(wb, x, start = c(70, 1), name) {
  r <- start[1] + 1
  r2 <- start[1] + nrow(x) + 1
  c <- start[2]
  c2 <- start[2] + ncol(x) - 1

  add_table_name(wb, name, r - 1, c)

  openxlsx::writeData(wb, 1, x, startCol = c, startRow = r)
  openxlsx::deleteData(wb, 1, cols = c, rows = r)

  # Add rounding to the cell formats
  openxlsx::addStyle(wb, 1, d2(), rows = r:r2, cols = c:c2, gridExpand = TRUE)
  add_table_style(wb, r, r2, c, c2)
  wb
}

d2 <- function() openxlsx::createStyle(numFmt = "0.00")
d1 <- function() openxlsx::createStyle(numFmt = "0.0")
d0 <- function() openxlsx::createStyle(numFmt = "0")

add_table_style <- function(wb, r, r2, c, c2, c_labels = 1) {
  h <- openxlsx::createStyle(border = c("top", "bottom"),
                        borderStyle = c("thick", "thin"),
                        halign = "center")
  b <- openxlsx::createStyle(halign = "center")
  f <- openxlsx::createStyle(border = "bottom", borderStyle = "thick")
  l <- openxlsx::createStyle(halign = "left")

  openxlsx::addStyle(wb, 1, h, r, c:c2, stack = TRUE)
  openxlsx::addStyle(wb, 1, b, r:r2, c:c2, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, 1, f, r2, c:c2, stack = TRUE)
  openxlsx::addStyle(wb, 1, l, r:r2, c+c_labels-1, gridExpand = TRUE, stack = TRUE)
}

add_table_name <- function(wb, text, r, c) {
  t_name <- openxlsx::createStyle(textDecoration = "bold")

  openxlsx::writeData(wb, 1, text, startCol = c, startRow = r)
  openxlsx::addStyle(wb, 1, rows = r, cols = c, style = t_name)
}
