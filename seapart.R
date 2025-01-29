library(ggplot2) # görselleştirme için
library(dplyr) # tibble oluşturmak için

# Bu fonksiyon ilk önce iki kullanıcı arasında bir boşluk kalacak şekilde tesadüfi olarak koltuk numarası belirler. Bu tip koltukların tamamı dolduktan sonra kalan koltuklar arasından yine tesadüfi olarak koltuk numarası belirler.

seapart = function(seat_cap = 50, users = seat_cap * 0.5, seed = 321) {
  set.seed(seed)
  seat_cap = seat_cap
  users = users

  # başlamak için tümüyle boş salon
  seat_status = rep(TRUE, times = seat_cap)
  seats = c(1:seat_cap)

  # Mod 5'e göre 1, 3, ve 0 olan koltuk numaraları
  seats_mod5 = seats[(seats %% 5) %in% c(0, 1, 3)]
  max_empty = length(seats_mod5)

  # Koltuk seçme fonksiyonları
  pick_seat = function(type = "mod5", mod5_seat_status) {

    if (type == "mod5") {
      
      empty_seats_mod5 = seats_mod5[mod5_seat_status]

      if (length(empty_seats_mod5) > 1) {
        seat_pick = sample(empty_seats_mod5, 1)
      } else {
        seat_pick = empty_seats_mod5[1]
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

      mod5_seat_status = seat_status[seats_mod5]

      if (sum(mod5_seat_status) > 0) {
        seat_pick = pick_seat("mod5", mod5_seat_status)
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
        cat("Mod 5'e göre 0, 1, ve 3 olan koltukların tamamı dolu!", "\n")
        cat("Diğer numaralara geçiliyor...", "\n")
        cat_stars()
      }
    } else {
      cat_stars()
      cat(i, ": ", "Üzgünüz, şu anda hiç boş koltuğumuz yok.", "\n", sep = "")
    }
  }

  cat(paste("Kalan boş koltuk sayısı: ", sum(seat_status)))

  # Oturma verisi oluşturma
  seat_y_grid = rep(0, seat_cap)
  seat_y = rep(NA, length(seat_order))

  seat_x_grid = c(1:seat_cap) %% 5
  seat_x_grid[which(seat_x_grid == 0)] = 5

  seat_x = seat_order %% 5
  seat_x[which(seat_x == 0)] = 5

  for (i in 1:length(seat_order)) {
    if (!is.na(seat_order[i])) {
      for (j in 1:(seat_cap/5)) {
        in_interval = seat_order[i] > (j - 1) * 5 & seat_order[i] <= j * 5
        if (in_interval) {
          seat_y[i] = j
        }
      }
    }
  }

  # grid
  for (i in 1:length(seats)) {
    for (j in 1:(seat_cap/5)) {
      in_interval = seats[i] > (j - 1) * 5 & seats[i] <= j * 5
      if (in_interval) {
        seat_y_grid[i] = j
      }
    }
  }

  seating_data = tibble(
    seats,
    seat_x_grid,
    seat_y_grid,
    seat_order,
    seat_x,
    seat_y
  )

  return(seating_data)
}

seating_data = seapart(seat_cap = 50, users = 15)

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

