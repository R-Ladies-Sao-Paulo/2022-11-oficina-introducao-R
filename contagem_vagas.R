url <- "https://docs.google.com/spreadsheets/d/1QwbvoqcXxNm3sxSinJrosThbK6w5YppATaqaAhO1crg/edit?resourcekey#gid=918245694"


forms <- googlesheets4::read_sheet(url)


forms |> 
  dplyr::count(categoria, sort = TRUE)

forms |> 
  dplyr::count() |> 
  dplyr::mutate(porc = scales::percent(n/120))
