#===============================================================================
# Creating the hex sticker
# 
# Duncan Gates
# 2024-02-29
#===============================================================================


library(magick)
library(hexSticker)

plain_index <- magick::image_read("inst/sticker/plain-index.png") |>
  magick::image_scale(200) 

lightning <- magick::image_read("inst/sticker/quizzy_deck-removebg.png") |>
  magick::image_scale(200)

flash_index <- magick::image_read("inst/sticker/quizzy-index.png")


sticker(
  flash_index,
  s_x = 1, 
  s_y = 1.08, 
  s_width = 1.4, 
  s_height = 1.4,
  package = "flashcaRd",
  p_family = "Fira Sans",
  p_fontface = "bold",
  p_color = "black",
  p_x = 1, 
  p_y = 0.7, 
  p_size = 10,
  h_fill = "#0382c9",
  h_color = "black"
)


magick::image_read("flashcaRd.png")
