set.seed(345)

seat_cap = 15

# başlamak için tümüyle boş salon
seat_status = rep(TRUE, times = seat_cap)
seats = c(1:seat_cap)

# Mod 5'e göre 1, 3, ve 0 olan koltuk numaraları
seats_mod5 = seats[(seats %% 5) %in% c(0, 1, 3)]
max_empty = length(seats_mod5)

# Koltuk seçme fonksiyonu
pick_seat = function(type = "odd", mod5_seat_status) {

  if (type == "odd") {
    
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
      seat_pick = pick_seat("odd", mod5_seat_status)
    } else {
      seat_pick = pick_seat("any")
    }

    return(seat_pick)

  } else {
    return(-1)
  }
}

cat("****************************************\n")
cat("Toplam koltuk sayısı: ", seat_cap, "\n")
cat("Bir boşluklu maximum koltuk sayısı: ", max_empty, "\n")
cat("****************************************\n")

for (i in 1:(seat_cap + 1)) {
  seat_pick = seat_picker()

  if (seat_pick > 0) {
    seat_status[seat_pick] = FALSE
    cat(i, ": ", "Koltuk numaranız: ", seat_pick, "\n", sep = "")

    if (i == max_empty) {
      cat("****************************************\n")
    }
  } else {
    cat("****************************************\n")
    cat(i, ": ", "Üzgünüz, şu anda hiç boş koltuğumuz yok.", "\n", sep = "")
  }
}

cat(paste("Kalan koltuk sayısı: ", sum(seat_status)))