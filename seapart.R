library(ggplot2) # görselleştirme için
library(dplyr) # tibble oluşturmak için

# Bu fonksiyon ilk önce iki kullanıcı arasında bir boşluk kalacak şekilde, bu tip koltukların tamamı dolduktan sonra da kalan koltuklar arasından rastlantısal olarak koltuk numarası belirler.

seapart = function(sira_sayisi = 10, kullanici_sayisi = 20 * sira_sayisi * 0.5, seed = 321, verbose = TRUE) {
  set.seed(seed)

  n = sira_sayisi
  users = kullanici_sayisi
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
  # Ya da çift sayılar
  seats_mod10 = block2[(block2 %% 2) == 0]

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
        seat_pick = pick_seat("mod5_10", mod5_10_seat_status)
      } else {
        seat_pick = pick_seat("any")
      }

      return(seat_pick)

    } else {
      return(-1)
    }
  }

  if (verbose) {
    cat_stars = function() {
      cat("****************************************\n")
    }
  
    cat_stars()
    cat("Toplam başlangıç boş koltuk sayısı: ", seat_cap, "\n")
    cat("Bir boşluklu maximum koltuk sayısı: ", max_empty, "\n")
    cat_stars()
  }

  # Koltuk seçme
  seat_order = rep(NA, seat_cap)

  for (i in 1:(users)) {
    seat_pick = seat_picker()

    if (seat_pick > 0) {
      seat_status[seat_pick] = FALSE

      if (verbose) {
        cat(i, ": ", "Koltuk numaranız: ", seat_pick, "\n", sep = "")
      }

      if (i <= seat_cap) {seat_order[i] = seat_pick}

      if (verbose) {
        if (i == max_empty) {
          cat_stars()
          cat("1. ve 3. bloklarda mod 5'e göre 0, 1 ve 3 olan ve", 
              "2. blokta mod 10'a göre 0, 2, 4, 6 ve 8 olan",
              "koltukların tamamı dolu!", 
              "Diğer numaralara geçiliyor...", sep = "\n")
          cat_stars()
        }
      }
    } else {
      if (verbose) {
        cat_stars()
        cat(i, ": ", "Üzgünüz, şu anda hiç boş koltuğumuz yok.", "\n", sep = "")
      }
    }
  }

  if (verbose) {
    cat("Kalan boş koltuk sayısı:", sum(seat_status))
  }
  
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
    return(tibble(seat_x, seat_y, seats))
  }

  grid = situate(seats) |> 
    rename(seat_x_grid = seat_x, seat_y_grid = seat_y)

  seating_data = situate(seat_order) |> 
    rename(seat_order = seats)

  seating_data = bind_cols(seating_data, grid)

  return(seating_data)
}

seaplot = function(seating_data) {
  seating_data |>
  ggplot(aes(seat_x_grid, seat_y_grid, label = seats)) +
  geom_point(shape = 0, size = 9.5) +
  geom_point(
    aes(seat_x, seat_y), shape = 15, color = "rosybrown", 
    size = 9.5, na.rm = TRUE
  ) +
  geom_text() +
  labs(
    x = NULL, y = NULL,
    title = "Raslantısal oturma deseni",
    subtitle = paste0("Kullanıcı sayısı: ", sum(!is.na(seating_data$seat_order))) 
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.ticks = element_blank(), 
    axis.text = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
}

seapart(sira_sayisi =  10, kullanici_sayisi = 20) |>
  seaplot()
ggsave("users_20.jpeg", width = 8, height = 5, units = "in")

seapart(sira_sayisi =  10, kullanici_sayisi = 70) |>
  seaplot()
ggsave("users_70.jpeg", width = 8, height = 5, units = "in")

seapart(sira_sayisi =  10, kullanici_sayisi = 110) |>
  seaplot()
ggsave("users_110.jpeg", width = 8, height = 5, units = "in")

seapart(sira_sayisi =  10, kullanici_sayisi = 200) |>
  seaplot()
ggsave("users_200.jpeg", width = 8, height = 5, units = "in")

