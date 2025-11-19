library(openxlsx2)

d <- data.frame(
  ID = c("BASD-55498","FINA-97846","DESI-48765","SHEW-11687","WILJ-10348"),
  Status = c("Approved","Pending","Denied","Abandoned","Approved"),
  Amount = c(12.4,0,-5,-5.4,10)
  )

#write_xlsx(x = d, file = "openxlsx2_example1.xlsx")

# Get cell style from existing workbook
#table1 <- wb_load(file = "openxlsx2_styles.xlsx", sheet = "table1") |>
#  wb_get_cell_style(dims = "B2:E7")

# Set colors
border_color <- wb_color(hex = "#DADCE0")
header_fill <- wb_color(hex = "#E6EAFB")
row1_fill <- wb_color(hex = "#F7F9FC")

wb <- wb_workbook() |> 
  wb_add_worksheet(sheet = "s1", grid_lines = FALSE) |> 
  wb_add_data(x = d, start_col = 2, start_row = 2) |> 
  #wb_set_cell_style(dims = "A1:D6", style = table1) |> 
  wb_add_border( # add the outer and inner border
    dims = "B2:D7",
    top_border = "thin", top_color = border_color,
    bottom_border = "thin", bottom_color = border_color,
    left_border = "thin", left_color = border_color,
    right_border = "thin", right_color = border_color,
    inner_hgrid = "thin", inner_hcolor = border_color,
    inner_vgrid = "thin", inner_vcolor = border_color
  ) |>
  wb_add_fill(dims = "B2:D2", color = header_fill) |>  
  wb_add_font(dims = "B2:D7", name = "Calibri") |>
  wb_add_font(dims = "B2:D2", bold = TRUE) |>
  wb_add_fill(dims = "B2:D7", color = row1_fill, every_nth_row = 2) |>
  wb_add_numfmt(dims = "D3:D7", numfmt = "0.0 ;[Red](0.0);- ") |> 
  wb_add_cell_style(dims = "B2:C7", horizontal = "center") |>  
  wb_save(file = "openxlsx2_example1.xlsx")
