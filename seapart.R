library(ggplot2) # görselleştirme için
library(dplyr) # tibble oluşturmak için

# Bu fonksiyon ilk önce iki kullanıcı arasında bir boşluk kalacak şekilde tesadüfi olarak koltuk numarası belirler. Bu tip koltukların tamamı dolduktan sonra kalan koltuklar arasından yine tesadüfi olarak koltuk numarası belirler.

seapart = function(n = 10, users = 20 * n * 0.5, seed = 321) {
  set.seed(seed)

  block1 = c(1:(n * 5)) # ilk beşlik blok
  block2 = c((5 * n + 1):(15 * n)) # onluk blok
  block3 = c((15 * n + 1):(20 * n)) # ikinci beşlik blok
  block1_3 = c(block1, block3)

  seat_cap = 20 * n
  users = users

  # başlamak için tümüyle boş salon
  seats = c(block1, block2, block3)
  seat_status = rep(TRUE, times = seat_cap)

  # Mod 5'e göre 1, 3 ve 0 olan beşli blok koltuk numaraları
  seats_mod5 = block1_3[(block1_3 %% 5) %in% c(0, 1, 3)]
  # Mod 5'e göre 2, 4, 6, 8 ve 0 olan onlu blok koltuk numaraları
  seats_mod10 = block2[(block2 %% 10) %in% c(0, 2, 4, 6, 8)]

  seats_mod5_10 = c(seats_mod5, seats_mod10)
  max_empty = length(seats_mod5_10)

  # Koltuk seçme fonksiyonları
  pick_seat = function(type = "mod5_10", mod5_10_seat_status) {

    if (type == "mod5_10") {
      
      empty_seats_mod5_10 = seats_mod5_10[mod5_10_seat_status]

      if (length(empty_seats_mod5_10) > 1) {
        seat_pick = sample(empty_seats_mod5_10, 1)
      } else {
        seat_pick = empty_seats_mod5_10[1]
      }

      return(seat_pick)
    } else {

      empty_seats = seats[seat_status]

      if (length(empty_seats) > 1) {
        return(sample(empty_seats, 1))
      } else {
        return(empty_seats[1])
      }
    }
  }

  seat_picker = function() {
    if (sum(seat_status) > 0) {

      mod5_10_seat_status = seat_status[seats_mod5_10]

      if (sum(mod5_10_seat_status) > 0) {
        seat_pick = pick_seat("mod5", mod5_10_seat_status)
      } else {
        seat_pick = pick_seat("any")
      }

      return(seat_pick)

    } else {
      return(-1)
    }
  }

  cat_stars = function() {
    cat("****************************************\n")
  }

  cat_stars()
  cat("Toplam başlangıç boş koltuk sayısı: ", seat_cap, "\n")
  cat("Bir boşluklu maximum koltuk sayısı: ", max_empty, "\n")
  cat_stars()

  # Koltuk seçme
  seat_order = rep(NA, seat_cap)

  for (i in 1:(users)) {
    seat_pick = seat_picker()

    if (seat_pick > 0) {
      seat_status[seat_pick] = FALSE
      cat(i, ": ", "Koltuk numaranız: ", seat_pick, "\n", sep = "")

      if (i <= seat_cap) {seat_order[i] = seat_pick}

      if (i == max_empty) {
        cat_stars()
        cat("1. ve 3. bloklarda mod 5'e göre 0, 1 ve 3 olan ve", 
            "2. blokta mod 10'a göre 0, 2, 4, 6 ve 8 olan",
            "koltukların tamamı dolu!", 
            "Diğer numaralara geçiliyor...", sep = "\n")
        cat_stars()
      }
    } else {
      cat_stars()
      cat(i, ": ", "Üzgünüz, şu anda hiç boş koltuğumuz yok.", "\n", sep = "")
    }
  }

  cat("Kalan boş koltuk sayısı:", sum(seat_status))

  # Grid ve oturma verisi oluşturma fonksiyonu
  situate = function(seats){

    seat_x = rep(0, length(seats))

    for (i in 1:length(seats)) {
      if (!is.na(seats[i])) {
        if (seats[i] %in% block1){
          seat_x[i] = seats[i] %% 5
          if (seat_x[i] == 0) {seat_x[i] = 5}
        } else if (seats[i] %in% block2) {
          seat_x[i] = seats[i] %% 10
          if (seat_x[i] == 0) {seat_x[i] = 10}
          seat_x[i] = seat_x[i] + 6
        } else {
          seat_x[i] = seats[i] %% 5
          if (seat_x[i] == 0) {seat_x[i] = 5}
          seat_x[i]  = seat_x[i] + 17
        }
      } else {
        seat_x[i] = NA
      }
    }

    seat_y = rep(0, length(seats))
  
    for (i in 1:length(seats)) {
      if (!is.na(seats[i])) {
        if (seats[i] %in% block1) {
          for (j in 1:n) {
            in_interval = (seats[i] > (j - 1) * 5) & (seats[i] <= j * 5)
            if (in_interval) {
              seat_y[i] = j
            }
          }
        } else if (seats[i] %in% block2) {
          for (j in 1:n) {
            in_interval = (seats[i] > 5 * n + (j - 1) * 10) & (seats[i] <= 5 * n + j * 10)
            if (in_interval) {
              seat_y[i] = j
            }
          }
        } else {
          for (j in 1:n) {
            in_interval = (seats[i] > 15 * n + (j - 1) * 5) & (seats[i] <= 15 * n + j * 5)
            if (in_interval) {
              seat_y[i] = j
            }
          }
        }
      } else {
        seat_y[i] = NA
      }
    }
    return(tibble(seat_x, seat_y))
  }

  grid = situate(seats) |> 
    rename(seat_x_grid = seat_x, seat_y_grid = seat_y)

  seating_data = situate(seat_order)
  seating_data = bind_cols(seating_data, grid)

  return(seating_data)
}

seating_data = seapart(n = 10, users = 130)

seating_data |>
  ggplot(aes(seat_x_grid, seat_y_grid, label = seats)) +
  geom_point(shape = 0, size = 8) +
  geom_point(aes(seat_x, seat_y), shape = 15, color = "rosybrown", size = 8) +
  geom_text() +
  scale_x_discrete(expand = c(2, 2)) +
  #scale_y_discrete(expand = c(1, 0)) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.ticks = element_blank(), 
    axis.text = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

